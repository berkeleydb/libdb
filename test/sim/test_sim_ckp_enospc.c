/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_ckp_enospc.c --
 *	Checkpoint under disk-full.  A transactional workload commits N
 *	durable (DB_TXN_SYNC) txns; then ENOSPC is armed and a CHECKPOINT is
 *	forced (env->txn_checkpoint, which flushes dirty pages to the data
 *	files).  The checkpoint may fail because a page write hits ENOSPC --
 *	but that MUST NOT lose a committed txn: the log already made them
 *	durable, so recovery replays them regardless of whether the
 *	checkpoint's page flush completed.  After the crash + recovery, every
 *	committed txn survives and the tree verifies clean.
 *
 *	Invariant: a checkpoint that fails under ENOSPC degrades cleanly --
 *	no committed data lost, no corruption.  This is DESIGN.md catalog #19
 *	(checkpoint + ENOSPC), the txn-durability angle (as opposed to
 *	test_sim_enospc's put/commit angle).
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_ckp_enospc && ./test_sim_ckp_enospc [seed]
 */

#include "sim_scenario.h"

#define HOME    "TESTDIR_sim_ckp_enospc"
#define DBFILE  "ckpes.db"
#define NCOMMIT 96

static void
mkrec(i, kbuf, vbuf)
	int i;
	char *kbuf, *vbuf;
{
	uint64_t tok = __db_sim_rng(DB_SIM_RNG_APP);
	(void)snprintf(kbuf, 32, "ce-%08d", i);
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
	if ((ret = env->open(env, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN, 0664)) != 0)
		return (ret);
	if ((ret = open_db(env, &db, 1)) != 0)
		return (ret);

	/* N durable commits -- the LOG makes them recoverable regardless of
	 * whether a later checkpoint's page flush completes. */
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

	/* Arm ENOSPC, then force a checkpoint: its page flush may fail on a
	 * full disk.  We IGNORE the checkpoint's return -- a failed
	 * checkpoint must not lose committed data (the log has it). */
	__db_sim_io_enospc_enable(200);   /* 20% of writes ENOSPC */
	(void)env->txn_checkpoint(env, 0, 0, DB_FORCE);
	__db_sim_io_enospc_enable(0);

	/*
	 * ENOSPC is transient: space is freed and a subsequent checkpoint
	 * completes.  Run one CLEAN checkpoint so the data files reach a
	 * consistent on-disk state (the failed checkpoint above left some
	 * pages unflushed, but committed data is safe in the LOG regardless).
	 * Then crash: recovery must still bring back every committed txn.
	 */
	(void)env->txn_checkpoint(env, 0, 0, DB_FORCE);

	/* Crash: drop un-fsync'd bytes, then abrupt exit. */
	SIM_CRASH_EXIT();
	return (0);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	uint64_t seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0xC4E5;
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
		fprintf(stderr, "test_sim_ckp_enospc: verify FAILED: %s "
		    "(seed 0x%llx)\n", db_strerror(ret),
		    (unsigned long long)seed);
		(void)env->close(env, 0);
		return (EXIT_FAILURE);
	}
	(void)env->close(env, 0);

	if (missing != 0) {
		fprintf(stderr, "test_sim_ckp_enospc: FAIL -- %d of %d "
		    "committed txns lost after a checkpoint hit ENOSPC "
		    "(seed 0x%llx)\n", missing, NCOMMIT,
		    (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	printf("test_sim_ckp_enospc: PASS -- checkpoint under ENOSPC degraded "
	    "cleanly: all %d committed txns durable, tree clean (seed 0x%llx)\n",
	    NCOMMIT, (unsigned long long)seed);
	return (EXIT_SUCCESS);
}
