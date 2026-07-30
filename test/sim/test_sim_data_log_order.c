/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_data_log_order.c --
 *	The classic durability window: a checkpoint must make the log
 *	DURABLE before it lets that log's effects be trusted.  N txns commit
 *	with DB_TXN_NOSYNC (fast: the log record is buffered, NOT fsync'd at
 *	commit), so their ONLY path to durability is a later checkpoint,
 *	which flushes the log (DB_FLUSH on the checkpoint record) and the
 *	dirty data pages.  We drive an explicit env->txn_checkpoint(), then
 *	crash (the write-back model drops every byte not fsync'd).  After
 *	recovery EVERY checkpointed commit must survive.
 *
 *	Why this catches an fsync-of-data-without-fsync-of-log bug: with
 *	NOSYNC commits the log records live only in the volatile log buffer
 *	until the checkpoint flushes them.  If the checkpoint syncs the data
 *	pages (and advances the checkpoint) but fails to fsync the log, the
 *	log records the checkpoint depends on are never durable -- the
 *	write-back crash truncates them away and recovery cannot reconstruct
 *	the committed state.  This is exactly the ordering hazard "fsync of
 *	data before fsync of log".
 *
 *	Invariant (DESIGN.md catalog, checkpoint/WAL ordering): a completed
 *	checkpoint's committed effects are durable across a crash -- the
 *	checkpoint must have made the underlying log durable, not just the
 *	data pages.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_data_log_order && ./test_sim_data_log_order [seed]
 */

#include "sim_scenario.h"

#define HOME    "TESTDIR_sim_dlorder"
#define DBFILE  "dlorder.db"
#define NCOMMIT 96            /* NOSYNC commits made durable by the ckp */

static void
mkrec(i, kbuf, vbuf)
	int i;
	char *kbuf, *vbuf;
{
	uint64_t tok = __db_sim_rng(DB_SIM_RNG_APP);
	(void)snprintf(kbuf, 32, "dl-%08d", i);
	(void)snprintf(vbuf, 32, "dv-%016llx", (unsigned long long)tok);
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
	if ((ret = env->open(env, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN, 0664)) != 0)
		return (ret);
	if ((ret = open_db(env, &db, 1)) != 0)
		return (ret);

	/* NOSYNC commits: fast, log buffered, NOT fsync'd at commit time.
	 * Durability for these depends entirely on the checkpoint below. */
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
		if ((ret = txn->commit(txn, DB_TXN_NOSYNC)) != 0)
			return (ret);
	}

	/* The checkpoint: flush dirty data pages AND make the log durable.
	 * A correct checkpoint fsyncs the log (DB_FLUSH) so the NOSYNC
	 * commits above are now genuinely on stable storage. */
	if ((ret = env->txn_checkpoint(env, 0, 0, DB_FORCE)) != 0)
		return (ret);

	/* Close the DB handle so its pages are flushed to the data file
	 * (belt-and-braces: the committed data lives in the data file after
	 * the checkpoint; recovery + the log are still the source of truth).
	 * We do NOT clean-close the env -- the crash is abrupt. */
	(void)db->close(db, 0);

	SIM_CRASH_EXIT();
	return (0);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	uint64_t seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0xD106DE1;
	DB_ENV *env;
	DB *db;
	DBT key, data;
	char kbuf[32], vbuf[32];
	int i, ret, missing = 0, mismatch = 0;

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
		if ((ret = db->get(db, NULL, &key, &data, 0)) != 0) {
			fprintf(stderr, "MISSING checkpointed key %s: %s\n",
			    kbuf, db_strerror(ret));
			missing++;
		} else if (data.size != strlen(vbuf) + 1 ||
		    memcmp(data.data, vbuf, data.size) != 0)
			mismatch++;
	}
	__db_sim_deactivate();
	(void)db->close(db, 0);
	(void)env->close(env, 0);

	printf("test_sim_data_log_order: %d missing, %d mismatch of %d "
	    "checkpointed commits (seed 0x%llx)\n", missing, mismatch,
	    NCOMMIT, (unsigned long long)seed);

	if (missing != 0 || mismatch != 0) {
		fprintf(stderr, "test_sim_data_log_order: FAIL -- %d "
		    "checkpointed commit(s) lost after crash (the checkpoint's "
		    "log was not durable before its data was trusted, seed "
		    "0x%llx)\n", missing + mismatch, (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	printf("test_sim_data_log_order: PASS -- all %d NOSYNC commits made "
	    "durable by the checkpoint survived the crash (log durable before "
	    "data trusted, seed 0x%llx)\n", NCOMMIT,
	    (unsigned long long)seed);
	return (EXIT_SUCCESS);
}
