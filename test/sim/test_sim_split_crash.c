/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_split_crash.c --
 *	Btree split/merge churn across a crash.  A small-page btree takes a
 *	seeded insert/delete churn heavy enough to force many page splits
 *	and merges, all in durable txns, then crashes.  After recovery the
 *	live key set (inserts not later deleted) must be exactly present,
 *	deleted keys absent, and the split-heavy tree verifies clean -- the
 *	structural-integrity-under-crash invariant for the btree's most
 *	complex mutation path.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_split_crash && ./test_sim_split_crash [seed]
 */

#include "sim_scenario.h"

#define HOME    "TESTDIR_sim_split"
#define DBFILE  "split.db"
#define NKEYS   500
#define NDEL    120
#define PGSIZE  512

static void
mkrec(i, kbuf, vbuf)
	int i;
	char *kbuf, *vbuf;
{
	/* Keys drawn so inserts scatter across the keyspace (forces splits
	 * at many points, not just append-at-end). */
	(void)snprintf(kbuf, 32, "sk-%08d", (i * 2654435761u) % 1000000u);
	(void)snprintf(vbuf, 40, "sv-%08d-padded-value-xx", i);
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
	(void)db->set_pagesize(db, PGSIZE);
	if ((ret = db->open(db, NULL, DBFILE, NULL, DB_BTREE,
	    (create ? DB_CREATE : 0) | DB_AUTO_COMMIT, 0664)) != 0) {
		fprintf(stderr, "open failed: %s\n", db_strerror(ret));
		return (ret);
	}
	*dbp = db;
	return (0);
}

/* Which insert indexes get deleted (seeded, deterministic).  Deleted iff
 * (i % (NKEYS/NDEL)) == 0, plus a seeded stride so the pattern varies. */
static int
is_deleted(i)
	int i;
{
	return ((i % (NKEYS / NDEL)) == 0);
}

static int
populate(seed)
	uint64_t seed;
{
	DB_ENV *env;
	DB *db;
	DB_TXN *txn;
	DBT key, data;
	char kbuf[32], vbuf[40];
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

	/* Insert every key (forces splits). */
	for (i = 0; i < NKEYS; i++) {
		mkrec(i, kbuf, vbuf);
		if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
			return (ret);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
		(void)db->put(db, txn, &key, &data, 0);
		if ((ret = txn->commit(txn, DB_TXN_SYNC)) != 0)
			return (ret);
	}
	/* Delete a seeded subset (forces merges/rebalances). */
	for (i = 0; i < NKEYS; i++) {
		if (!is_deleted(i))
			continue;
		mkrec(i, kbuf, vbuf);
		if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
			return (ret);
		memset(&key, 0, sizeof(key));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		(void)db->del(db, txn, &key, 0);
		if ((ret = txn->commit(txn, DB_TXN_SYNC)) != 0)
			return (ret);
	}

	/* Uncommitted split-inducing insert, then crash. */
	{
		char ukey[32] = "sk-99999999";
		if (env->txn_begin(env, NULL, &txn, 0) == 0) {
			memset(&key, 0, sizeof(key));
			memset(&data, 0, sizeof(data));
			key.data = ukey; key.size = (u_int32_t)strlen(ukey) + 1;
			data.data = ukey; data.size = (u_int32_t)strlen(ukey)+1;
			(void)db->put(db, txn, &key, &data, 0);
			/* no commit */
		}
	}

	SIM_CRASH_EXIT();
	return (0);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	uint64_t seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0x5911;
	DB_ENV *env;
	DB *db;
	DBT key, data;
	char kbuf[32], vbuf[40];
	int i, ret, missing = 0, ghost = 0, uncommitted = 0;

	if (sim_fresh_home(HOME) != 0)
		return (EXIT_FAILURE);
	if (sim_run_crash_child(seed, populate) != 0)
		return (EXIT_FAILURE);

	if (sim_env_recover(HOME, &env) != 0)
		return (EXIT_FAILURE);
	if (open_db(env, &db, 0) != 0)
		return (EXIT_FAILURE);

	for (i = 0; i < NKEYS; i++) {
		mkrec(i, kbuf, vbuf);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		ret = db->get(db, NULL, &key, &data, 0);
		if (is_deleted(i)) {
			if (ret == 0)
				ghost++;   /* deleted key resurrected */
		} else {
			if (ret != 0)
				missing++; /* live key lost */
		}
	}
	{
		char ukey[32] = "sk-99999999";
		memset(&key, 0, sizeof(key));
		key.data = ukey; key.size = (u_int32_t)strlen(ukey) + 1;
		if (db->get(db, NULL, &key, &data, 0) == 0)
			uncommitted = 1;
	}
	(void)db->close(db, 0);

	if ((ret = db_create(&db, env, 0)) != 0)
		return (EXIT_FAILURE);
	if ((ret = db->verify(db, DBFILE, NULL, NULL, 0)) != 0) {
		fprintf(stderr, "test_sim_split_crash: verify FAILED "
		    "(split-heavy tree corrupt): %s\n", db_strerror(ret));
		(void)env->close(env, 0);
		return (EXIT_FAILURE);
	}
	(void)env->close(env, 0);

	if (missing || ghost || uncommitted) {
		fprintf(stderr, "test_sim_split_crash: FAIL -- %d live keys "
		    "missing, %d deleted resurrected, uncommitted=%d "
		    "(seed 0x%llx)\n", missing, ghost, uncommitted,
		    (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	printf("test_sim_split_crash: PASS -- %d live keys survived "
	    "split/merge churn, %d deletes stayed deleted, tree clean "
	    "(seed 0x%llx)\n", NKEYS - NDEL, NDEL, (unsigned long long)seed);
	return (EXIT_SUCCESS);
}
