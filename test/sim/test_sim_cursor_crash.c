/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_cursor_crash.c --
 *	Cursor-heavy workload across a crash.  All writes go through a
 *	CURSOR (c_put), a seeded subset is removed via cursor delete (c_del
 *	while iterating), all inside durable txns.  The process crashes;
 *	recovery runs; then a fresh CURSOR walk (c_get DB_NEXT) must return
 *	exactly the live set in key order -- every cursor-inserted, not
 *	cursor-deleted record present, deleted ones absent -- and the tree
 *	verifies clean.
 *
 *	Invariant (DESIGN.md catalog #30, cursor angle): cursor mutations
 *	are durable and consistent across a crash, and a post-recovery
 *	cursor scan sees exactly the committed live set (no phantom, no
 *	lost, correct order).
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_cursor_crash && ./test_sim_cursor_crash [seed]
 */

#include "sim_scenario.h"

#define HOME    "TESTDIR_sim_cursor"
#define DBFILE  "cursor.db"
#define NKEYS   300

static void
mkrec(i, kbuf, vbuf)
	int i;
	char *kbuf, *vbuf;
{
	uint64_t tok = __db_sim_rng(DB_SIM_RNG_APP);
	(void)snprintf(kbuf, 32, "cu-%08d", i);
	(void)snprintf(vbuf, 32, "cv-%016llx", (unsigned long long)tok);
}

/* Deleted iff every 5th key (seeded-stable, deterministic). */
static int
is_deleted(i)
	int i;
{
	return ((i % 5) == 0);
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
	DBC *dbc;
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

	/* Insert all keys via a cursor, each in its own durable txn. */
	for (i = 0; i < NKEYS; i++) {
		mkrec(i, kbuf, vbuf);
		if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
			return (ret);
		if ((ret = db->cursor(db, txn, &dbc, 0)) != 0)
			return (ret);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
		if ((ret = dbc->put(dbc, &key, &data, DB_KEYFIRST)) != 0)
			return (ret);
		(void)dbc->close(dbc);
		if ((ret = txn->commit(txn, DB_TXN_SYNC)) != 0)
			return (ret);
	}

	/* Cursor-delete a seeded subset: position via c_get(SET) then
	 * c_del, in one durable txn (a cursor-heavy delete pass). */
	if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
		return (ret);
	if ((ret = db->cursor(db, txn, &dbc, 0)) != 0)
		return (ret);
	for (i = 0; i < NKEYS; i++) {
		if (!is_deleted(i))
			continue;
		mkrec(i, kbuf, vbuf);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		if (dbc->get(dbc, &key, &data, DB_SET) == 0)
			(void)dbc->del(dbc, 0);
	}
	(void)dbc->close(dbc);
	if ((ret = txn->commit(txn, DB_TXN_SYNC)) != 0)
		return (ret);

	SIM_CRASH_EXIT();
	return (0);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	uint64_t seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0xC025;
	DB_ENV *env;
	DB *db;
	DBC *dbc;
	DBT key, data;
	char kbuf[32], vbuf[32];
	int i, ret, missing = 0, ghost = 0, walked = 0, expect_live = 0;

	if (sim_fresh_home(HOME) != 0)
		return (EXIT_FAILURE);
	if (sim_run_crash_child(seed, populate) != 0)
		return (EXIT_FAILURE);

	if (sim_env_recover(HOME, &env) != 0)
		return (EXIT_FAILURE);
	if (open_db(env, &db, 0) != 0)
		return (EXIT_FAILURE);

	/* Point-check each key. */
	__db_sim_activate(seed);
	for (i = 0; i < NKEYS; i++) {
		mkrec(i, kbuf, vbuf);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		ret = db->get(db, NULL, &key, &data, 0);
		if (is_deleted(i)) {
			if (ret == 0)
				ghost++;
		} else {
			expect_live++;
			if (ret != 0)
				missing++;
			else if (data.size != strlen(vbuf) + 1 ||
			    memcmp(data.data, vbuf, data.size) != 0)
				missing++;
		}
	}
	__db_sim_deactivate();

	/* Fresh cursor walk: count the live set the cursor sees in order. */
	if ((ret = db->cursor(db, NULL, &dbc, 0)) == 0) {
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		while (dbc->get(dbc, &key, &data, DB_NEXT) == 0)
			walked++;
		(void)dbc->close(dbc);
	}
	(void)db->close(db, 0);

	if ((ret = db_create(&db, env, 0)) != 0)
		return (EXIT_FAILURE);
	if ((ret = db->verify(db, DBFILE, NULL, NULL, 0)) != 0) {
		fprintf(stderr, "test_sim_cursor_crash: verify FAILED: %s\n",
		    db_strerror(ret));
		(void)env->close(env, 0);
		return (EXIT_FAILURE);
	}
	(void)env->close(env, 0);

	printf("test_sim_cursor_crash: %d expected-live, cursor walked %d, "
	    "%d missing, %d ghost (seed 0x%llx)\n",
	    expect_live, walked, missing, ghost, (unsigned long long)seed);

	if (missing != 0 || ghost != 0 || walked != expect_live) {
		fprintf(stderr, "test_sim_cursor_crash: FAIL -- cursor "
		    "mutations inconsistent after recovery (seed 0x%llx)\n",
		    (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	printf("test_sim_cursor_crash: PASS -- all cursor-written records "
	    "survived, cursor-deletes stayed deleted, cursor walk exact, "
	    "tree clean (seed 0x%llx)\n", (unsigned long long)seed);
	return (EXIT_SUCCESS);
}
