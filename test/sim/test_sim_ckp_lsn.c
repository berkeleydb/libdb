/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_ckp_lsn.c --
 *	Checkpoint-LSN correctness across a crash.  A transactional workload
 *	commits a PRE-checkpoint batch, forces a checkpoint (records the LSN
 *	recovery will start from), commits a POST-checkpoint batch (durable
 *	in the fsync'd log, but their data pages are NOT flushed), then
 *	crashes.  Recovery must replay the log FROM THE CHECKPOINT forward
 *	and bring back every post-checkpoint committed txn.
 *
 *	Invariant (DESIGN.md catalog #18, checkpoint-LSN angle): the
 *	recorded checkpoint LSN is the correct recovery start point -- a
 *	checkpoint that records the WRONG (too-recent) LSN makes recovery
 *	skip committed records written after the real checkpoint, silently
 *	losing them.  Every committed txn (pre AND post checkpoint) must be
 *	present after recovery.
 *
 *	PLANTED BUG (DB_DST_INJECT_BUG=5, CKPBADLSN): __txn_updateckp records
 *	a checkpoint LSN advanced far past the true one, so recovery starts
 *	too late and the post-checkpoint committed txns are lost.  This
 *	test's invariant then fires -- the FoundationDB-grade "DST finds a
 *	real recovery bug, here is the seed" proof.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_ckp_lsn && ./test_sim_ckp_lsn [seed]
 */

#include "sim_scenario.h"

#define HOME    "TESTDIR_sim_ckp_lsn"
#define DBFILE  "ckplsn.db"
#define NPRE    48            /* committed BEFORE the checkpoint */
#define NPOST   48            /* committed AFTER the checkpoint */

static void
mkrec(post, i, kbuf, vbuf)
	int post, i;
	char *kbuf, *vbuf;
{
	uint64_t tok = __db_sim_rng(DB_SIM_RNG_APP);
	(void)snprintf(kbuf, 32, "%s-%08d", post ? "po" : "pr", i);
	(void)snprintf(vbuf, 32, "v-%016llx", (unsigned long long)tok);
}

static int
open_db(env, dbp, create)
	DB_ENV *env;
	DB **dbp;
	int create;
{
	DB *db;
	int ret;

	if ((ret = db_create(&db, env, 0)) != 0)
		return (ret);
	if ((ret = db->open(db, NULL, DBFILE, NULL, DB_BTREE,
	    (create ? DB_CREATE : 0) | DB_AUTO_COMMIT, 0664)) != 0) {
		fprintf(stderr, "open: %s\n", db_strerror(ret));
		return (ret);
	}
	*dbp = db;
	return (0);
}

static int
commit_batch(env, db, post, n)
	DB_ENV *env;
	DB *db;
	int post, n;
{
	DB_TXN *txn;
	DBT key, data;
	char kbuf[32], vbuf[32];
	int i, ret;

	for (i = 0; i < n; i++) {
		mkrec(post, i, kbuf, vbuf);
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
	return (0);
}

static int
populate(seed)
	uint64_t seed;
{
	DB_ENV *env;
	DB *db;
	int ret;

	__db_sim_activate(seed);
	__db_sim_wb_enable(1);

	if ((ret = db_env_create(&env, 0)) != 0)
		return (ret);
	if ((ret = env->open(env, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN, 0664)) != 0)
		return (ret);
	if ((ret = open_db(env, &db, 1)) != 0)
		return (ret);

	/* Pre-checkpoint commits. */
	if ((ret = commit_batch(env, db, 0, NPRE)) != 0)
		return (ret);

	/* Checkpoint: records the LSN recovery will start from.  DB_FORCE
	 * flushes dirty pages to disk too. */
	if ((ret = env->txn_checkpoint(env, 0, 0, DB_FORCE)) != 0)
		return (ret);

	/* Post-checkpoint commits: durable in the fsync'd LOG, but their
	 * data pages are NOT flushed (no further checkpoint).  Recovery must
	 * replay them from the checkpoint LSN forward. */
	if ((ret = commit_batch(env, db, 1, NPOST)) != 0)
		return (ret);

	/* Crash: drop un-fsync'd bytes (the log's synced prefix -- covering
	 * every DB_TXN_SYNC commit -- survives; unflushed data pages are
	 * rebuilt by recovery from the log). */
	SIM_CRASH_EXIT();
	return (0);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	uint64_t seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0xC4914;
	DB_ENV *env;
	DB *db;
	DBT key, data;
	char kbuf[32], vbuf[32];
	int i, ret, missing_pre = 0, missing_post = 0;

	if (sim_fresh_home(HOME) != 0)
		return (EXIT_FAILURE);
	if (sim_run_crash_child(seed, populate) != 0)
		return (EXIT_FAILURE);

	if (sim_env_recover(HOME, &env) != 0) {
#if DB_DST_BUG(5)
		/* Recovery from a bogus (too-far-forward) checkpoint LSN may
		 * fail outright -- that is also a detection of the bug. */
		printf("test_sim_ckp_lsn: DST CAUGHT CKPBADLSN -- recovery "
		    "from the wrong checkpoint LSN failed (seed 0x%llx)\n",
		    (unsigned long long)seed);
		return (EXIT_SUCCESS);
#else
		return (EXIT_FAILURE);
#endif
	}
	if (open_db(env, &db, 0) != 0)
		return (EXIT_FAILURE);

	__db_sim_activate(seed);
	for (i = 0; i < NPRE; i++) {
		mkrec(0, i, kbuf, vbuf);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		if (db->get(db, NULL, &key, &data, 0) != 0)
			missing_pre++;
	}
	for (i = 0; i < NPOST; i++) {
		mkrec(1, i, kbuf, vbuf);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		if (db->get(db, NULL, &key, &data, 0) != 0)
			missing_post++;
	}
	__db_sim_deactivate();
	(void)db->close(db, 0);
	(void)env->close(env, 0);

#if DB_DST_BUG(5)
	/*
	 * CKPBADLSN invariant: with the wrong checkpoint LSN, recovery must
	 * have LOST at least one post-checkpoint committed txn.  If all
	 * survived, the bug went undetected -- fail so the sweep records the
	 * hole.  When caught (a post-ckp txn missing), exit 0.
	 */
	if (missing_post == 0 && missing_pre == 0) {
		fprintf(stderr, "test_sim_ckp_lsn: DID NOT CATCH CKPBADLSN -- "
		    "all committed txns survived despite the wrong checkpoint "
		    "LSN (seed 0x%llx)\n", (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	printf("test_sim_ckp_lsn: DST CAUGHT CKPBADLSN -- %d pre + %d post "
	    "checkpoint committed txn(s) lost because recovery started from "
	    "the wrong LSN (seed 0x%llx)\n", missing_pre, missing_post,
	    (unsigned long long)seed);
	return (EXIT_SUCCESS);
#else
	if (missing_pre != 0 || missing_post != 0) {
		fprintf(stderr, "test_sim_ckp_lsn: FAIL -- %d pre + %d post "
		    "checkpoint committed txns lost after recovery "
		    "(seed 0x%llx)\n", missing_pre, missing_post,
		    (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	printf("test_sim_ckp_lsn: PASS -- all %d pre + %d post checkpoint "
	    "committed txns replayed from the correct checkpoint LSN "
	    "(seed 0x%llx)\n", NPRE, NPOST, (unsigned long long)seed);
	return (EXIT_SUCCESS);
#endif
}
