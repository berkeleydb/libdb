/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_abort_atomic.c --
 *	Transaction atomicity-under-crash scenario.  A seeded mix of
 *	committed and explicitly-aborted txns runs, then the process
 *	crashes.  After recovery: every COMMITTED txn's record is present,
 *	every ABORTED txn left NO trace, and the tree verifies clean.  This
 *	is the all-or-nothing invariant: an abort must be as complete as a
 *	crash-rollback, and a crash must not resurrect an aborted txn.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_abort_atomic && ./test_sim_abort_atomic [seed]
 */

#include "sim_scenario.h"

#define HOME    "TESTDIR_sim_abort"
#define DBFILE  "abort.db"
#define NTXN    96

static void
mkrec(i, kbuf, vbuf)
	int i;
	char *kbuf, *vbuf;
{
	uint64_t tok = __db_sim_rng(DB_SIM_RNG_APP);
	(void)snprintf(kbuf, 32, "ab-%08d", i);
	(void)snprintf(vbuf, 32, "av-%016llx", (unsigned long long)tok);
}

/* Deterministic commit/abort decision for txn i (seeded APP coin). */
static int
should_commit(void)
{
	return ((int)__db_sim_rng_range(DB_SIM_RNG_APP, 2));
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

	for (i = 0; i < NTXN; i++) {
		int commit = should_commit();   /* draws the APP coin */
		mkrec(i, kbuf, vbuf);            /* draws the APP value */
		if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
			return (ret);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
		if ((ret = db->put(db, txn, &key, &data, 0)) != 0) {
			(void)txn->abort(txn);
			return (ret);
		}
		if (commit)
			ret = txn->commit(txn, DB_TXN_SYNC);
		else
			ret = txn->abort(txn);
		if (ret != 0)
			return (ret);
	}

	SIM_CRASH_EXIT();
	return (0);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	uint64_t seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0xAB07;
	DB_ENV *env;
	DB *db;
	DBT key, data;
	char kbuf[32], vbuf[32];
	int i, ret, missing = 0, resurrected = 0, ncommit = 0, nabort = 0;

	if (sim_fresh_home(HOME) != 0)
		return (EXIT_FAILURE);
	if (sim_run_crash_child(seed, populate) != 0)
		return (EXIT_FAILURE);

	if (sim_env_recover(HOME, &env) != 0)
		return (EXIT_FAILURE);
	if (open_db(env, &db, 0) != 0)
		return (EXIT_FAILURE);

	/* Replay the SAME seed draws in the SAME order to reconstruct each
	 * txn's commit/abort decision and value. */
	__db_sim_activate(seed);
	for (i = 0; i < NTXN; i++) {
		int commit = should_commit();
		mkrec(i, kbuf, vbuf);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		ret = db->get(db, NULL, &key, &data, 0);
		if (commit) {
			ncommit++;
			if (ret != 0)
				missing++;
		} else {
			nabort++;
			if (ret == 0)
				resurrected++;
		}
	}
	__db_sim_deactivate();
	(void)db->close(db, 0);

	if ((ret = db_create(&db, env, 0)) != 0)
		return (EXIT_FAILURE);
	if ((ret = db->verify(db, DBFILE, NULL, NULL, 0)) != 0) {
		fprintf(stderr, "test_sim_abort_atomic: verify FAILED: %s\n",
		    db_strerror(ret));
		(void)env->close(env, 0);
		return (EXIT_FAILURE);
	}
	(void)env->close(env, 0);

	if (missing || resurrected) {
		fprintf(stderr, "test_sim_abort_atomic: FAIL -- %d committed "
		    "missing, %d aborted resurrected (seed 0x%llx)\n",
		    missing, resurrected, (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	printf("test_sim_abort_atomic: PASS -- %d committed present, %d "
	    "aborted left no trace, tree clean (seed 0x%llx)\n",
	    ncommit, nabort, (unsigned long long)seed);
	return (EXIT_SUCCESS);
}
