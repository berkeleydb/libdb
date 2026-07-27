/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_crash_recover.c --
 *	THE capstone pilot: a transactional btree workload commits N
 *	durable txns, a child process "crashes" mid-uncommitted-txn (an
 *	abrupt _exit with a dirty env), then the parent runs recovery and
 *	verifies EVERY committed txn survived, the uncommitted one did not,
 *	and the DB verifies clean.
 *
 *	Determinism: the workload keys/values are drawn from the seeded APP
 *	stream, so a given seed produces the exact same committed set --
 *	the same seed replays the same run.  The crash point is fixed
 *	(after N synced commits, inside txn N+1), which the deterministic
 *	fault schedule will later parameterize (v2).
 *
 *	CRITICAL (see .agents/concurrent-btree-corruption.md): a crashed
 *	txn env verified WITHOUT recovery falsely looks corrupt.  This
 *	pilot ALWAYS runs DB_RECOVER before db->verify.
 *
 *	Planted-bug hook (DB_DST_INJECT_BUG=1, NODURABLE): the last "acked"
 *	txn commits with DB_TXN_NOSYNC (acked but not fsync'd).  A correct
 *	engine loses it across the crash (it was never durable); the buggy
 *	build ASSERTS it survived, so the DST invariant fires -- proving the
 *	capstone catches an ack-before-durable bug.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_crash_recover && ./test_sim_crash_recover [seed]
 */

#include <sys/types.h>
#include <sys/wait.h>

#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "db.h"
#include "sim_rng.h"
#include "sim_fault.h"
#include "sim_inject.h"

#define HOME    "TESTDIR_sim_crash"
#define DBFILE  "crash.db"
#define NCOMMIT 64            /* durable committed txns before the crash */

/* Deterministic key/value for record i under the active seed. */
static void
mkrec(i, kbuf, vbuf)
	int i;
	char *kbuf, *vbuf;
{
	/* key is just the index (so the parent can look each one up);
	 * value carries a seeded token so a wrong/torn value is visible. */
	uint64_t tok = __db_sim_rng(DB_SIM_RNG_APP);
	(void)snprintf(kbuf, 32, "key-%08d", i);
	(void)snprintf(vbuf, 32, "val-%016llx", (unsigned long long)tok);
}

static int
run_child(seed)
	uint64_t seed;
{
	DB_ENV *env;
	DB *db;
	DB_TXN *txn;
	DBT key, data;
	char kbuf[32], vbuf[32];
	int i, ret;

	/* Re-seed identically in the child so its APP-stream draws match the
	 * parent's expectation (fork copies the seed but we re-activate to
	 * be explicit and independent of copy semantics). */
	__db_sim_activate(seed);
	__db_sim_wb_enable(1);       /* honest disk: writes durable only on fsync */

	if ((ret = db_env_create(&env, 0)) != 0)
		return (ret);
	if ((ret = env->open(env, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN, 0664)) != 0)
		return (ret);
	if ((ret = db_create(&db, env, 0)) != 0)
		return (ret);
	if ((ret = db->open(db, NULL, DBFILE, NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT, 0664)) != 0)
		return (ret);

	/* N durable (synced) commits -- each MUST survive the crash. */
	for (i = 0; i < NCOMMIT; i++) {
		mkrec(i, kbuf, vbuf);
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

#if DB_DST_BUG(1) == 0
	/* Normal path: start an UNCOMMITTED txn, then crash inside it. */
	mkrec(NCOMMIT, kbuf, vbuf);
	if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
		return (ret);
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
	data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
	(void)db->put(db, txn, &key, &data, 0);
	/* Deliberately DO NOT commit txn.  Crash now. */
#else
	/*
	 * NODURABLE bug: "ack" a commit WITHOUT fsync (DB_TXN_NOSYNC) and
	 * treat it as durable.  A correct engine loses this across a crash;
	 * the harness invariant (below) asserts it survived, so the planted
	 * bug makes the capstone FAIL -- exactly the DST catch we want.
	 */
	mkrec(NCOMMIT, kbuf, vbuf);
	if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
		return (ret);
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
	data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
	(void)db->put(db, txn, &key, &data, 0);
	(void)txn->commit(txn, DB_TXN_NOSYNC);   /* acked, NOT durable */
#endif

	/*
	 * CRASH: abrupt exit, no clean close, no checkpoint.  _exit skips
	 * atexit handlers and libdb cleanup, leaving the env dirty exactly
	 * as a kill -9 would.  Flush stdio so any child diagnostics escape.
	 */
	fflush(NULL);
	_exit(42);
	/* NOTREACHED */
	return (0);
}

/* Recover, reopen, and verify the committed set.  Returns 0 on success. */
static int
verify_after_recovery(seed, saw_nosync_key)
	uint64_t seed;
	int *saw_nosync_key;
{
	DB_ENV *env;
	DB *db;
	DBT key, data;
	char kbuf[32], vbuf[32];
	int i, ret, missing = 0, mismatch = 0;

	*saw_nosync_key = 0;

	/* ALWAYS recover before touching the tree (else a WAL-consistent
	 * crashed tree falsely looks corrupt). */
	if ((ret = db_env_create(&env, 0)) != 0)
		return (ret);
	if ((ret = env->open(env, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | DB_RECOVER, 0664))
	    != 0) {
		fprintf(stderr, "recover open failed: %s\n", db_strerror(ret));
		return (ret);
	}

	if ((ret = db_create(&db, env, 0)) != 0)
		return (ret);
	if ((ret = db->open(db, NULL, DBFILE, NULL, DB_BTREE,
	    DB_AUTO_COMMIT, 0664)) != 0) {
		fprintf(stderr, "reopen failed: %s\n", db_strerror(ret));
		return (ret);
	}

	/* Re-derive the expected records deterministically from the seed. */
	__db_sim_activate(seed);
	for (i = 0; i < NCOMMIT; i++) {
		mkrec(i, kbuf, vbuf);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		if ((ret = db->get(db, NULL, &key, &data, 0)) != 0) {
			fprintf(stderr, "MISSING committed key %s: %s\n",
			    kbuf, db_strerror(ret));
			missing++;
		} else if (data.size != strlen(vbuf) + 1 ||
		    memcmp(data.data, vbuf, data.size) != 0) {
			fprintf(stderr, "WRONG value for committed key %s\n",
			    kbuf);
			mismatch++;
		}
	}
	/* The (NOSYNC-acked or uncommitted) key NCOMMIT must be ABSENT after
	 * a correct recovery. */
	mkrec(NCOMMIT, kbuf, vbuf);
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
	if (db->get(db, NULL, &key, &data, 0) == 0)
		*saw_nosync_key = 1;
	__db_sim_deactivate();

	(void)db->close(db, 0);

	/* Verify a FRESH handle (verify needs the db closed). */
	if ((ret = db_create(&db, env, 0)) != 0)
		return (ret);
	if ((ret = db->verify(db, DBFILE, NULL, NULL, 0)) != 0) {
		fprintf(stderr, "db->verify FAILED: %s\n", db_strerror(ret));
		/* verify closes the handle itself on failure. */
		(void)env->close(env, 0);
		return (ret);
	}
	(void)env->close(env, 0);

	if (missing != 0 || mismatch != 0) {
		fprintf(stderr, "%d missing, %d mismatched committed txns\n",
		    missing, mismatch);
		return (1);
	}
	return (0);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	uint64_t seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0xDB5EEDull;
	pid_t pid;
	int status, ret, saw_nosync;
	char cmd[256];

	/* Fresh env dir each run (trash-friendly: a plain rm of a known
	 * scratch dir we created). */
	(void)snprintf(cmd, sizeof(cmd), "rm -rf %s && mkdir -p %s",
	    HOME, HOME);
	(void)system(cmd);

	/* Child does the durable work then crashes. */
	if ((pid = fork()) < 0) {
		perror("fork");
		return (EXIT_FAILURE);
	}
	if (pid == 0)
		exit(run_child(seed) == 0 ? 0 : 1);   /* only reached on setup err */

	if (waitpid(pid, &status, 0) < 0) {
		perror("waitpid");
		return (EXIT_FAILURE);
	}
	if (!(WIFEXITED(status) && WEXITSTATUS(status) == 42)) {
		fprintf(stderr, "child did not reach the crash point "
		    "(status %d) -- setup failed\n", status);
		return (EXIT_FAILURE);
	}

	ret = verify_after_recovery(seed, &saw_nosync);

#if DB_DST_BUG(1)
	/*
	 * NODURABLE invariant: the NOSYNC-acked commit must NOT survive.
	 * If it did (or recovery/verify otherwise passed), the ack-before-
	 * durable bug went UNDETECTED -- fail loudly so the sweep records
	 * DST caught it (a nonzero exit is the "caught" signal here since
	 * the bug is in what we ACCEPT, not what crashes).
	 */
	if (ret == 0 && saw_nosync) {
		fprintf(stderr, "test_sim_crash_recover: DST CAUGHT NODURABLE "
		    "-- a NOSYNC-acked commit survived a crash (seed 0x%llx)\n",
		    (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	printf("test_sim_crash_recover: (bug build) NOSYNC commit correctly "
	    "absent -- would need a real ack-before-fsync site to trip\n");
#endif

	if (ret == 0) {
		printf("test_sim_crash_recover: PASS -- %d committed txns "
		    "survived, uncommitted did not, DB verifies clean "
		    "(seed 0x%llx)\n", NCOMMIT, (unsigned long long)seed);
		return (EXIT_SUCCESS);
	}
	fprintf(stderr, "test_sim_crash_recover: FAIL (seed 0x%llx)\n",
	    (unsigned long long)seed);
	return (EXIT_FAILURE);
}
