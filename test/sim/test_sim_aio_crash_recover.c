/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_aio_crash_recover.c --
 *	Async-writeback (DB_MPOOL_AIO) crash + recovery equivalence.  The
 *	async data-page writeback path must give EXACTLY the synchronous
 *	path's crash-recovery guarantee: after a power-loss crash with
 *	checkpoints (whose page flush runs through os_aio), DB_RECOVER
 *	brings back every DB_TXN_SYNC-committed txn and the tree verifies
 *	clean.
 *
 *	This is the no-fault regression companion to
 *	test_sim_aio_ckp_enospc: it proves the async path does not LOSE or
 *	CORRUPT data in normal operation, and that a crash with async writes
 *	outstanding recovers correctly (the log is ahead of the data, so
 *	redo re-applies -- WAL invariant).  Run with the same seed as a
 *	synchronous env and the surviving set must be identical.
 *
 *	Build/run (from a build dir, after configure --enable-dst):
 *	    gmake test_sim_aio_crash_recover && ./test_sim_aio_crash_recover [seed]
 */

#include "sim_scenario.h"

#define HOME    "TESTDIR_sim_aio_crash_recover"
#define DBFILE  "aiocr.db"
#define NCOMMIT 300

static void
mkrec(i, kbuf, vbuf)
	int i;
	char *kbuf, *vbuf;
{
	uint64_t tok = __db_sim_rng(DB_SIM_RNG_APP);
	(void)snprintf(kbuf, 32, "cr-%08d", i);
	(void)snprintf(vbuf, 32, "cv-%016llx", (unsigned long long)tok);
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
		fprintf(stderr, "open failed: %s\n", db_strerror(ret));
		return (ret);
	}
	*dbp = db;
	return (0);
}

static int
populate(seed)
	uint64_t seed;
{
	DB_ENV *env;
	DB *db;
	DB_TXN *txn;
	DBT key, data;
	char kbuf[32], vbuf[32];
	int i, ret;

	__db_sim_activate(seed);
	__db_sim_wb_enable(1);

	if ((ret = db_env_create(&env, 0)) != 0)
		return (ret);
	/* Async buffer-pool writeback ON. */
	if ((ret = env->set_flags(env, DB_MPOOL_AIO, 1)) != 0)
		return (ret);
	/* Small cache so many dirty pages get flushed by the checkpoints
	 * through the async window (real submit/drain traffic). */
	(void)env->set_cachesize(env, 0, 512 * 1024, 1);
	if ((ret = env->open(env, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN, 0664)) != 0)
		return (ret);
	if ((ret = open_db(env, &db, 1)) != 0)
		return (ret);

	/* Durable commits (log-backed) + periodic checkpoints whose page
	 * flush runs through os_aio, so at the crash there are async writes
	 * both completed-and-fsync'd and possibly in flight. */
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
		if (i % 50 == 49 &&
		    (ret = env->txn_checkpoint(env, 0, 0, DB_FORCE)) != 0)
			return (ret);
	}

	/* Crash: drop un-fsync'd bytes, abrupt exit (async writes may be in
	 * flight; the log has every committed txn). */
	SIM_CRASH_EXIT();
	return (0);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	uint64_t seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0xA1C4;
	DB_ENV *env;
	DB *db;
	DBT key, data;
	char kbuf[32], vbuf[32];
	int i, ret, missing = 0;

	if (sim_fresh_home(HOME) != 0)
		return (EXIT_FAILURE);
	if (sim_run_crash_child(seed, populate) != 0)
		return (EXIT_FAILURE);

	if (sim_env_recover(HOME, &env) != 0)
		return (EXIT_FAILURE);
	if (open_db(env, &db, 0) != 0)
		return (EXIT_FAILURE);

	__db_sim_activate(seed);
	for (i = 0; i < NCOMMIT; i++) {
		mkrec(i, kbuf, vbuf);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		if (db->get(db, NULL, &key, &data, 0) != 0)
			missing++;
		else if (data.size != strlen(vbuf) + 1 ||
		    memcmp(data.data, vbuf, data.size) != 0)
			missing++;
	}
	__db_sim_deactivate();
	(void)db->close(db, 0);

	if ((ret = db_create(&db, env, 0)) != 0)
		return (EXIT_FAILURE);
	if ((ret = db->verify(db, DBFILE, NULL, NULL, 0)) != 0) {
		fprintf(stderr, "test_sim_aio_crash_recover: verify FAILED: "
		    "%s (seed 0x%llx)\n", db_strerror(ret),
		    (unsigned long long)seed);
		(void)env->close(env, 0);
		return (EXIT_FAILURE);
	}
	(void)env->close(env, 0);

	if (missing != 0) {
		fprintf(stderr, "test_sim_aio_crash_recover: FAIL -- %d of %d "
		    "committed txns lost after async-writeback crash+recover "
		    "(seed 0x%llx)\n", missing, NCOMMIT,
		    (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	printf("test_sim_aio_crash_recover: PASS -- all %d committed txns "
	    "durable across an async-writeback crash+recover, tree clean "
	    "(seed 0x%llx)\n", NCOMMIT, (unsigned long long)seed);
	return (EXIT_SUCCESS);
}
