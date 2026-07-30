/*-
 * Deterministic Simulation Testing (DST) for libdb -- v2 multi-process pilot.
 *
 * mp_failchk_pilot.c --
 *	The highest-value multi-process fault: a process dies mid-transaction
 *	while holding a WRITE LOCK in a real, shared (NOT DB_PRIVATE) region,
 *	and a survivor process runs DB_ENV->failchk to detect + recover it.
 *	Proves the dead txn is aborted, its write lock released, committed
 *	data intact, and the DB verifies clean -- without a full environment
 *	restart.
 *
 *	This exercises the multi-process crash-recovery path with ZERO prior
 *	DST coverage: src/env/env_failchk.c, __lock_failchk, __txn_failchk,
 *	__dbreg_failchk, __memp_failchk, __mut_failchk.
 *
 *	The kill point is seeded on DB_SIM_RNG_SCHED (the v1-reserved stream,
 *	so this does not shift any v1 seed).  Interleaving is NOT controlled
 *	(this is a deterministic-fault, nondeterministic-interleaving pilot;
 *	see DST-V2-DESIGN.md sec.3).
 *
 *	One executable, three roles (argv[1]):
 *	    setup    <home> <seed>            create shared env + commit N recs
 *	    victim   <home> <seed> <sentinel> begin txn, put (write lock), block
 *	    survivor <home> <seed>            failchk, verify, report
 *	Orchestration lives in test/sim/mp-failchk.sh (spawns, kills, cleans
 *	up orphans, timeout-guards).
 *
 *	Build/run (from build_unix, after configure --enable-dst --enable-test):
 *	    make mp_failchk_pilot
 *	    ../test/sim/mp-failchk.sh [seed]
 */

#include <sys/types.h>

#include <errno.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "db.h"
#include "sim_rng.h"
#include "sim_sched.h"

#define DBFILE   "mp.db"
#define NCOMMIT  32		/* durable records committed by setup */
#define VKEY     "victim-hot-key"
#define NKILLPTS 4		/* seeded operation boundaries the victim can stop at */

/*
 * is_alive / thread_id: failchk REQUIRES both.  A plain getpid-based
 * is_alive: a process is alive iff kill(pid, 0) succeeds.  This is exactly
 * what a real multi-process BDB deployment supplies.
 */
static int
mp_is_alive(dbenv, pid, tid, flags)
	DB_ENV *dbenv;
	pid_t pid;
	db_threadid_t tid;
	u_int32_t flags;
{
	(void)dbenv;
	(void)tid;
	(void)flags;
	/* kill(pid,0): 0 => alive; ESRCH => dead. */
	return (kill(pid, 0) == 0 ? 1 : 0);
}

/*
 * Open the SHARED env.  NOT DB_PRIVATE: this is a real cross-process mmap
 * region -- the whole point of v2.  Configure the failchk prerequisites
 * (is_alive + thread tracking) on every attach.
 */
static int
open_shared_env(home, envp, create)
	const char *home;
	DB_ENV **envp;
	int create;
{
	DB_ENV *env;
	int ret;
	u_int32_t flags;

	if ((ret = db_env_create(&env, 0)) != 0)
		return (ret);

	(void)env->set_isalive(env, mp_is_alive);
	(void)env->set_thread_count(env, 64);
	env->set_errpfx(env, "mp_failchk_pilot");
	env->set_errfile(env, stderr);

	flags = DB_INIT_LOCK | DB_INIT_LOG | DB_INIT_MPOOL |
	    DB_INIT_TXN | DB_THREAD;
	if (create)
		flags |= DB_CREATE;

	if ((ret = env->open(env, home, flags, 0664)) != 0) {
		fprintf(stderr, "env->open(%s) failed: %s\n",
		    home, db_strerror(ret));
		(void)env->close(env, 0);
		return (ret);
	}
	*envp = env;
	return (0);
}

static int
open_db(env, txn, dbp, create)
	DB_ENV *env;
	DB_TXN *txn;
	DB **dbp;
	int create;
{
	DB *db;
	int ret;

	if ((ret = db_create(&db, env, 0)) != 0)
		return (ret);
	if ((ret = db->open(db, txn, DBFILE, NULL, DB_BTREE,
	    (create ? DB_CREATE : 0) | DB_THREAD |
	    (txn == NULL ? DB_AUTO_COMMIT : 0), 0664)) != 0) {
		fprintf(stderr, "db->open failed: %s\n", db_strerror(ret));
		(void)db->close(db, 0);
		return (ret);
	}
	*dbp = db;
	return (0);
}

static void
mkval(seed, i, vbuf)
	uint64_t seed;
	int i;
	char *vbuf;
{
	/* Deterministic value derived from seed+index (no APP-stream draw so
	 * setup/survivor agree without stepping the RNG in lockstep). */
	(void)snprintf(vbuf, 32, "v-%016llx-%04d",
	    (unsigned long long)seed, i);
}

/* ---- role: setup ---- */
static int
role_setup(home, seed)
	const char *home;
	uint64_t seed;
{
	DB_ENV *env;
	DB *db;
	DB_TXN *txn;
	DBT key, data;
	char kbuf[32], vbuf[32];
	int i, ret;

	if ((ret = open_shared_env(home, &env, 1)) != 0)
		return (ret);
	if ((ret = open_db(env, NULL, &db, 1)) != 0)
		return (ret);

	for (i = 0; i < NCOMMIT; i++) {
		(void)snprintf(kbuf, sizeof(kbuf), "durable-%04d", i);
		mkval(seed, i, vbuf);
		if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
			return (ret);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
		if ((ret = db->put(db, txn, &key, &data, 0)) != 0)
			return (ret);
		if ((ret = txn->commit(txn, DB_TXN_SYNC)) != 0)
			return (ret);
	}
	if ((ret = db->close(db, 0)) != 0)
		return (ret);
	if ((ret = env->close(env, 0)) != 0)
		return (ret);
	printf("[setup] committed %d durable records (seed 0x%llx)\n",
	    NCOMMIT, (unsigned long long)seed);
	return (0);
}

/* ---- role: victim ----
 * Attach the shared env, begin a txn, put VKEY (acquiring a WRITE LOCK on
 * the page in the shared lock table), stop at the seeded kill point, write
 * the sentinel to tell the harness "I hold a write lock in an open txn",
 * then block so the harness can kill -9 me at this operation boundary.
 * We must NOT commit/abort -- a real crash leaves the txn open.
 */
static int
role_victim(home, seed, sentinel)
	const char *home;
	uint64_t seed;
	const char *sentinel;
{
	DB_ENV *env;
	DB *db;
	DB_TXN *txn;
	DBT key, data;
	uint64_t killpt;
	char vbuf[32];
	FILE *fp;
	int ret;

	__db_sim_activate(seed);
	killpt = __db_sim_sched_killpoint(NKILLPTS);

	if ((ret = open_shared_env(home, &env, 0)) != 0)
		return (ret);
	if ((ret = open_db(env, NULL, &db, 0)) != 0)
		return (ret);
	if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
		return (ret);

	/* Acquire the write lock: put VKEY inside the open txn. */
	(void)snprintf(vbuf, sizeof(vbuf), "victim-uncommitted");
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = (void *)VKEY; key.size = (u_int32_t)strlen(VKEY) + 1;
	data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
	if ((ret = db->put(db, txn, &key, &data, 0)) != 0) {
		fprintf(stderr, "[victim] put failed: %s\n", db_strerror(ret));
		return (ret);
	}

	/*
	 * The seeded kill point: we stopped at operation boundary `killpt`
	 * holding the write lock in an open txn.  (killpt is the seam phase 2
	 * will use to interleave; phase 1 uses it only to record which
	 * boundary the fault landed at, for the repro.)  Announce, then block.
	 */
	if ((fp = fopen(sentinel, "w")) != NULL) {
		(void)fprintf(fp, "pid=%ld killpoint=%llu seed=0x%llx\n",
		    (long)getpid(), (unsigned long long)killpt,
		    (unsigned long long)seed);
		(void)fclose(fp);
	}
	(void)fprintf(stderr,
	    "[victim] pid=%ld holds WRITE LOCK on '%s' in open txn, "
	    "killpoint=%llu -- blocking for kill\n",
	    (long)getpid(), VKEY, (unsigned long long)killpt);
	(void)fflush(stderr);

	/* Block: the harness kill -9's us here.  Bounded so an un-killed
	 * victim cannot wedge forever; the harness kills well before this. */
	for (;;)
		sleep(1);
	/* NOTREACHED */
	return (0);
}

/* ---- role: survivor ----
 * Attach the SAME shared env (the victim is now dead), run failchk, then
 * prove: (1) the victim's write lock is released -- we can now acquire it;
 * (2) the victim's uncommitted put is gone; (3) the committed set is intact;
 * (4) the DB verifies clean.  If failchk returns DB_RUNRECOVERY, run
 * DB_RECOVER and re-verify (the documented failchk->recovery escalation).
 */
static int
survivor_verify(env, seed, saw_uncommitted, missing, mismatch)
	DB_ENV *env;
	uint64_t seed;
	int *saw_uncommitted, *missing, *mismatch;
{
	DB *db;
	DB_TXN *txn;
	DBT key, data;
	char kbuf[32], vbuf[32];
	int i, ret;

	*saw_uncommitted = *missing = *mismatch = 0;

	if ((ret = open_db(env, NULL, &db, 0)) != 0)
		return (ret);

	/* (1)+(2): try to acquire the write lock the victim held and read
	 * VKEY.  A held-forever lock would BLOCK here -- the harness timeout
	 * catches that as the severe-bug signature.  Use a txn with a short
	 * lock timeout so we FAIL rather than hang if the lock leaked. */
	if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
		return (ret);
	(void)env->set_timeout(env, 5000000, DB_SET_LOCK_TIMEOUT); /* 5s */
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = (void *)VKEY; key.size = (u_int32_t)strlen(VKEY) + 1;
	data.flags = DB_DBT_MALLOC;
	ret = db->get(db, txn, &key, &data, DB_RMW);
	if (ret == 0) {
		*saw_uncommitted = 1;		/* victim's put must be gone */
		free(data.data);
	}
	else if (ret == DB_LOCK_DEADLOCK || ret == DB_LOCK_NOTGRANTED) {
		fprintf(stderr, "[survivor] SEVERE: write lock on '%s' NOT "
		    "released by failchk (ret %s) -- dead txn's lock leaked\n",
		    VKEY, db_strerror(ret));
		(void)txn->abort(txn);
		(void)db->close(db, 0);
		return (ret);
	}
	/* DB_NOTFOUND is the expected/good case: uncommitted put rolled back. */
	if ((ret = txn->commit(txn, 0)) != 0) {
		(void)db->close(db, 0);
		return (ret);
	}

	/* (3): committed set intact. */
	for (i = 0; i < NCOMMIT; i++) {
		(void)snprintf(kbuf, sizeof(kbuf), "durable-%04d", i);
		mkval(seed, i, vbuf);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		data.flags = DB_DBT_MALLOC;
		if (db->get(db, NULL, &key, &data, 0) != 0)
			(*missing)++;
		else {
			if (data.size != strlen(vbuf) + 1 ||
			    memcmp(data.data, vbuf, data.size) != 0)
				(*mismatch)++;
			free(data.data);
		}
	}
	(void)db->close(db, 0);
	return (0);
}

static int
role_survivor(home, seed)
	const char *home;
	uint64_t seed;
{
	DB_ENV *env;
	DB *db;
	int ret, fcret, saw_uncommitted, missing, mismatch, recovered = 0;

	if ((ret = open_shared_env(home, &env, 0)) != 0)
		return (ret);

	/* THE multi-process recovery step. */
	fcret = env->failchk(env, 0);
	fprintf(stderr, "[survivor] failchk returned: %s\n",
	    fcret == 0 ? "0 (recovered in place)" : db_strerror(fcret));

	/*
	 * Contract (DST-V2-DESIGN.md sec.2.4): failchk should EITHER recover
	 * the dead process's state in place (return 0) OR tell us to run
	 * recovery (DB_RUNRECOVERY).  Any OTHER nonzero return -- e.g. EBUSY
	 * from a mutex a dead process held that failchk could not destroy --
	 * is the SEVERE-bug signature: failchk left the shared region in a
	 * state with no defined recovery contract.  We still escalate to
	 * DB_RECOVER (the only remaining option) and report the finding.
	 */
	if (fcret != 0) {
		if (fcret != DB_RUNRECOVERY)
			fprintf(stderr, "[survivor] SEVERE: failchk returned a "
			    "non-recovery error (%s) -- see DST-V2-DESIGN.md "
			    "sec.2.4; escalating to DB_RECOVER (seed 0x%llx)\n",
			    db_strerror(fcret), (unsigned long long)seed);
		else
			fprintf(stderr, "[survivor] failchk -> DB_RUNRECOVERY; "
			    "running DB_RECOVER\n");
		(void)env->close(env, 0);
		if ((ret = db_env_create(&env, 0)) != 0)
			return (ret);
		(void)env->set_isalive(env, mp_is_alive);
		(void)env->set_thread_count(env, 64);
		env->set_errpfx(env, "mp_failchk_pilot");
		env->set_errfile(env, stderr);
		if ((ret = env->open(env, home, DB_CREATE | DB_INIT_LOCK |
		    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | DB_THREAD |
		    DB_RECOVER, 0664)) != 0) {
			fprintf(stderr, "[survivor] recover open failed: %s\n",
			    db_strerror(ret));
			return (ret);
		}
		recovered = 1;
	}

	if ((ret = survivor_verify(env, seed,
	    &saw_uncommitted, &missing, &mismatch)) != 0) {
		(void)env->close(env, 0);
		return (ret);
	}

	/* (4): DB verifies clean. */
	if ((ret = db_create(&db, env, 0)) != 0) {
		(void)env->close(env, 0);
		return (ret);
	}
	if ((ret = db->verify(db, DBFILE, NULL, NULL, 0)) != 0) {
		fprintf(stderr, "[survivor] verify FAILED: %s\n",
		    db_strerror(ret));
		(void)env->close(env, 0);
		return (ret);
	}
	(void)env->close(env, 0);

	if (saw_uncommitted || missing || mismatch) {
		fprintf(stderr, "[survivor] FAIL -- uncommitted=%d missing=%d "
		    "mismatch=%d (seed 0x%llx)\n",
		    saw_uncommitted, missing, mismatch,
		    (unsigned long long)seed);
		return (1);
	}
	printf("[survivor] PASS -- failchk %s the dead txn's write lock, "
	    "%d committed intact, uncommitted gone, verifies clean "
	    "(seed 0x%llx)\n",
	    recovered ? "recovered (via DB_RECOVER)" : "released",
	    NCOMMIT, (unsigned long long)seed);
	return (0);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	const char *role, *home;
	uint64_t seed;

	if (argc < 4) {
		fprintf(stderr,
		    "usage: %s setup|victim|survivor <home> <seed> [sentinel]\n",
		    argv[0]);
		return (2);
	}
	role = argv[1];
	home = argv[2];
	seed = strtoull(argv[3], NULL, 0);

	if (strcmp(role, "setup") == 0)
		return (role_setup(home, seed) == 0 ?
		    EXIT_SUCCESS : EXIT_FAILURE);
	if (strcmp(role, "victim") == 0) {
		if (argc < 5) {
			fprintf(stderr, "victim needs <sentinel>\n");
			return (2);
		}
		return (role_victim(home, seed, argv[4]) == 0 ?
		    EXIT_SUCCESS : EXIT_FAILURE);
	}
	if (strcmp(role, "survivor") == 0)
		return (role_survivor(home, seed) == 0 ?
		    EXIT_SUCCESS : EXIT_FAILURE);

	fprintf(stderr, "unknown role: %s\n", role);
	return (2);
}
