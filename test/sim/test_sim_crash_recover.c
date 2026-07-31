/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_crash_recover.c --
 *	THE capstone pilot: a transactional btree workload commits N
 *	durable txns, then a "crash" drops every byte the write-back model
 *	says was written but never fsync'd (a real power loss), the parent
 *	runs recovery, and asserts EVERY committed txn survived, the
 *	uncommitted one did not, and the DB verifies clean.
 *
 *	The write-back durable-frontier model is what makes this HONEST:
 *	the sim writes to a real file, so bytes reach the file on pwrite
 *	regardless of fsync -- a naive crash test therefore cannot catch a
 *	writer that ACKs a commit it never fsync'd.  Here the child, at the
 *	crash boundary, calls __db_sim_wb_crash(), which truncates the real
 *	log file back to its durable frontier (last fsync).  So a commit
 *	whose log was written but not synced is genuinely lost -- exactly
 *	how a disk loses it on power loss.
 *
 *	Determinism: the workload keys/values are drawn from the seeded APP
 *	stream, so a given seed produces the exact same committed set --
 *	the same seed replays the same run.
 *
 *	CRITICAL: a crashed
 *	txn env verified WITHOUT recovery falsely looks corrupt.  This
 *	pilot ALWAYS runs DB_RECOVER before db->verify.
 *
 *	PLANTED BUG (DB_DST_INJECT_BUG=1, NODURABLE): __log_flush_int skips
 *	the log fsync but still acks the commit.  The write-back durable
 *	frontier then never advances past the last DB_TXN_SYNC commit's
 *	log, so __db_sim_wb_crash() truncates that record away and the
 *	"committed" txn is LOST after recovery -- the capstone invariant
 *	fires.  This is the FoundationDB-grade "DST finds a real durability
 *	bug, here is the seed" proof.
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
	 * parent's expectation.  Arm the write-back model so the log's
	 * durable frontier is tracked and un-fsync'd bytes get dropped at
	 * the crash boundary below. */
	__db_sim_activate(seed);
	__db_sim_wb_enable(1);

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

	/* An UNCOMMITTED txn, then crash inside it: its data must NOT
	 * survive.  (With the NODURABLE planted bug, the DB_TXN_SYNC commits
	 * above were never fsync'd, so the write-back crash below drops their
	 * log too -- that is the bug the capstone catches.) */
	mkrec(NCOMMIT, kbuf, vbuf);
	if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
		return (ret);
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
	data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
	(void)db->put(db, txn, &key, &data, 0);
	/* Deliberately DO NOT commit txn. */

	/*
	 * CRASH (power loss): drop every byte written but not fsync'd.  The
	 * write-back model truncates each tracked real file (the log) back
	 * to its durable frontier.  A correct engine fsync'd each
	 * DB_TXN_SYNC commit, so its durable frontier already covers all
	 * NCOMMIT commits and only the uncommitted tail is dropped.  Then an
	 * abrupt _exit (no clean close, no checkpoint) leaves the env dirty
	 * exactly as kill -9 would.
	 */
	__db_sim_wb_crash();
	fflush(NULL);
	_exit(42);
	/* NOTREACHED */
	return (0);
}

/* Recover, reopen, and verify the committed set.  Returns 0 on success. */
static int
verify_after_recovery(seed, saw_uncommitted, missing_out)
	uint64_t seed;
	int *saw_uncommitted, *missing_out;
{
	DB_ENV *env;
	DB *db;
	DBT key, data;
	char kbuf[32], vbuf[32];
	int i, ret, missing = 0, mismatch = 0;

	*saw_uncommitted = 0;
	*missing_out = 0;

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
	/* The uncommitted key NCOMMIT must be ABSENT after recovery. */
	mkrec(NCOMMIT, kbuf, vbuf);
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
	if (db->get(db, NULL, &key, &data, 0) == 0)
		*saw_uncommitted = 1;
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

	*missing_out = missing;
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
	int status, ret, saw_uncommitted, missing;
	char cmd[256];

	/* Fresh env dir each run. */
	(void)snprintf(cmd, sizeof(cmd), "rm -rf %s && mkdir -p %s",
	    HOME, HOME);
	(void)system(cmd);

	/* Child does the durable work then crashes (dropping un-fsync'd
	 * bytes via the write-back model). */
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

	ret = verify_after_recovery(seed, &saw_uncommitted, &missing);

#if DB_DST_BUG(1)
	/*
	 * NODURABLE invariant: with the fsync-skip bug, at least one
	 * DB_TXN_SYNC-committed txn must be LOST after the crash (its log
	 * was never made durable).  If everything survived, the bug went
	 * UNDETECTED -- fail so the sweep records a coverage hole.  When the
	 * bug IS caught (a committed txn missing => verify_after_recovery
	 * returned nonzero), exit 0: DST caught the planted bug for this
	 * seed, which is the success condition for the injected build.
	 */
	if (ret == 0 && missing == 0) {
		fprintf(stderr, "test_sim_crash_recover: DST DID NOT CATCH "
		    "NODURABLE -- every committed txn survived despite the "
		    "skipped fsync (seed 0x%llx)\n",
		    (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	printf("test_sim_crash_recover: DST CAUGHT NODURABLE -- %d "
	    "\"committed\" txn(s) lost after crash because the log fsync was "
	    "skipped (seed 0x%llx)\n", missing, (unsigned long long)seed);
	return (EXIT_SUCCESS);
#else
	if (saw_uncommitted) {
		fprintf(stderr, "test_sim_crash_recover: FAIL -- an "
		    "uncommitted txn survived the crash (seed 0x%llx)\n",
		    (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	if (ret == 0) {
		printf("test_sim_crash_recover: PASS -- %d committed txns "
		    "survived, uncommitted did not, DB verifies clean "
		    "(seed 0x%llx)\n", NCOMMIT, (unsigned long long)seed);
		return (EXIT_SUCCESS);
	}
	fprintf(stderr, "test_sim_crash_recover: FAIL (seed 0x%llx)\n",
	    (unsigned long long)seed);
	return (EXIT_FAILURE);
#endif
}
