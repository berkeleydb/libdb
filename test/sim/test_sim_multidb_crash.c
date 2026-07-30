/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_multidb_crash.c --
 *	Multi-file / sub-database crash + recovery.  A single transactional
 *	env holds THREE named sub-databases in ONE physical file (BDB's
 *	multiple-databases-per-file feature).  Each sub-db gets its own
 *	durable committed workload, interleaved so the log is a mix of all
 *	three; the process crashes; recovery runs; then EVERY committed
 *	record of EVERY sub-db must be present with the right value, and no
 *	uncommitted record of any sub-db survives.
 *
 *	Invariant (DESIGN.md catalog #5, multi-file angle): recovery
 *	reconstructs multiple sub-databases sharing one file+log
 *	consistently -- a crash must not lose one sub-db's commits while
 *	keeping another's, nor cross records between sub-dbs.  This
 *	exercises the DBREG open-file logging + recovery's per-file redo
 *	dispatch over interleaved sub-db records.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_multidb_crash && ./test_sim_multidb_crash [seed]
 */

#include "sim_scenario.h"

#define HOME    "TESTDIR_sim_multidb"
#define DBFILE  "multi.db"
#define NSUB    3
#define NCOMMIT 48            /* durable committed txns per sub-db */

static const char *subname[NSUB] = { "alpha", "beta", "gamma" };

/* Record for sub-db s, index i: key encodes both so no cross-contamination
 * is possible; value carries a seeded token. */
static void
mkrec(s, i, kbuf, vbuf)
	int s, i;
	char *kbuf, *vbuf;
{
	uint64_t tok = __db_sim_rng(DB_SIM_RNG_APP);
	(void)snprintf(kbuf, 32, "%s-%08d", subname[s], i);
	(void)snprintf(vbuf, 32, "v-%016llx", (unsigned long long)tok);
}

static int
open_sub(env, dbp, s, create)
	DB_ENV *env;
	DB **dbp;
	int s, create;
{
	DB *db;
	int ret;

	if ((ret = db_create(&db, env, 0)) != 0)
		return (ret);
	if ((ret = db->open(db, NULL, DBFILE, subname[s], DB_BTREE,
	    (create ? DB_CREATE : 0) | DB_AUTO_COMMIT, 0664)) != 0) {
		fprintf(stderr, "sub %s open: %s\n", subname[s],
		    db_strerror(ret));
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
	DB *db[NSUB];
	DB_TXN *txn;
	DBT key, data;
	char kbuf[32], vbuf[32];
	int s, i, ret;

	__db_sim_activate(seed);
	__db_sim_wb_enable(1);

	if ((ret = db_env_create(&env, 0)) != 0)
		return (ret);
	if ((ret = env->open(env, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN, 0664)) != 0)
		return (ret);
	for (s = 0; s < NSUB; s++)
		if ((ret = open_sub(env, &db[s], s, 1)) != 0)
			return (ret);

	/* Interleave commits across the three sub-dbs so the single log is
	 * a genuine mix -- recovery must sort each record back to its file. */
	for (i = 0; i < NCOMMIT; i++) {
		for (s = 0; s < NSUB; s++) {
			mkrec(s, i, kbuf, vbuf);
			if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
				return (ret);
			memset(&key, 0, sizeof(key));
			memset(&data, 0, sizeof(data));
			key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
			data.data = vbuf;
			data.size = (u_int32_t)strlen(vbuf) + 1;
			if ((ret = db[s]->put(db[s], txn, &key, &data, 0)) != 0)
				return (ret);
			if ((ret = txn->commit(txn, DB_TXN_SYNC)) != 0)
				return (ret);
		}
	}

	/* One uncommitted put per sub-db (index NCOMMIT), then crash. */
	for (s = 0; s < NSUB; s++) {
		mkrec(s, NCOMMIT, kbuf, vbuf);
		if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
			return (ret);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
		(void)db[s]->put(db[s], txn, &key, &data, 0);
		/* deliberately DO NOT commit (each txn left open) */
	}

	SIM_CRASH_EXIT();
	return (0);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	uint64_t seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0x30DB;
	DB_ENV *env;
	DB *db[NSUB];
	DBT key, data;
	char kbuf[32], vbuf[32];
	int s, i, ret, missing = 0, mismatch = 0, ghost = 0;

	if (sim_fresh_home(HOME) != 0)
		return (EXIT_FAILURE);
	if (sim_run_crash_child(seed, populate) != 0)
		return (EXIT_FAILURE);

	if (sim_env_recover(HOME, &env) != 0)
		return (EXIT_FAILURE);
	for (s = 0; s < NSUB; s++)
		if (open_sub(env, &db[s], s, 0) != 0)
			return (EXIT_FAILURE);

	__db_sim_activate(seed);
	for (i = 0; i < NCOMMIT; i++) {
		for (s = 0; s < NSUB; s++) {
			mkrec(s, i, kbuf, vbuf);
			memset(&key, 0, sizeof(key));
			memset(&data, 0, sizeof(data));
			key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
			if (db[s]->get(db[s], NULL, &key, &data, 0) != 0)
				missing++;
			else if (data.size != strlen(vbuf) + 1 ||
			    memcmp(data.data, vbuf, data.size) != 0)
				mismatch++;
		}
	}
	/* No sub-db's uncommitted tail may survive. */
	for (s = 0; s < NSUB; s++) {
		mkrec(s, NCOMMIT, kbuf, vbuf);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		if (db[s]->get(db[s], NULL, &key, &data, 0) == 0)
			ghost++;
	}
	__db_sim_deactivate();

	for (s = 0; s < NSUB; s++)
		(void)db[s]->close(db[s], 0);

	/* Verify each sub-db's tree is clean. */
	for (s = 0; s < NSUB; s++) {
		if ((ret = db_create(&db[s], env, 0)) != 0)
			return (EXIT_FAILURE);
		if ((ret = db[s]->verify(db[s], DBFILE, subname[s], NULL, 0))
		    != 0) {
			fprintf(stderr, "test_sim_multidb_crash: verify(%s) "
			    "FAILED: %s\n", subname[s], db_strerror(ret));
			(void)env->close(env, 0);
			return (EXIT_FAILURE);
		}
	}
	(void)env->close(env, 0);

	printf("test_sim_multidb_crash: %d sub-dbs, %d missing, %d mismatch, "
	    "%d ghost (seed 0x%llx)\n", NSUB, missing, mismatch, ghost,
	    (unsigned long long)seed);
	if (missing != 0 || mismatch != 0 || ghost != 0) {
		fprintf(stderr, "test_sim_multidb_crash: FAIL -- multi-sub-db "
		    "recovery inconsistent (seed 0x%llx)\n",
		    (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	printf("test_sim_multidb_crash: PASS -- all %d committed records "
	    "across %d sub-dbs survived, uncommitted did not, every sub-tree "
	    "verifies clean (seed 0x%llx)\n", NSUB * NCOMMIT, NSUB,
	    (unsigned long long)seed);
	return (EXIT_SUCCESS);
}
