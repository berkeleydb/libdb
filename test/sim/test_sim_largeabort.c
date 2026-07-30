/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_largeabort.c --
 *	Large-transaction EXPLICIT abort + crash + recovery.  Unlike
 *	test_sim_largetxn_crash (which leaves a big txn uncommitted at the
 *	crash), here a large txn does NOPS puts and is then explicitly
 *	ABORTED via txn->abort() -- exercising the in-line undo rollback
 *	pass over a long single-txn record chain -- BEFORE a durable
 *	committed txn and then a crash.  After recovery: every record of
 *	the committed txn survives, and NOT ONE record of the aborted large
 *	txn survives (the undo must have rolled back all NOPS ops, and the
 *	crash+recovery must not resurrect them).
 *
 *	Invariant (DESIGN.md catalog #14/#27, large-abort angle): a large
 *	transaction that is explicitly aborted leaves no trace, even across
 *	a subsequent crash.  A partial survivor of the aborted txn -- or a
 *	committed record lost because the abort's undo clobbered a shared
 *	page -- is an atomicity/isolation violation.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_largeabort && ./test_sim_largeabort [seed]
 */

#include "sim_scenario.h"

#define HOME    "TESTDIR_sim_largeabort"
#define DBFILE  "largeabort.db"
#define NOPS    1500          /* ops in the aborted large txn */
#define NCOMMIT 300           /* records in the durable committed txn */

/* which == 'a' aborted-txn record, 'c' committed record.  The value is a
 * pure function of (seed, which, i) -- NOT a stream draw -- so it does not
 * depend on the ORDER records are generated in (the child writes 'a' then
 * 'c'; the parent verifies 'c' then 'a'). */
static void
mkrec(which, i, kbuf, vbuf)
	int which, i;
	char *kbuf, *vbuf;
{
	uint64_t tok = __db_sim_seed() ^
	    (((uint64_t)which << 40) | (uint64_t)i);
	tok *= 0x9E3779B97F4A7C15ull;
	tok ^= tok >> 29;
	(void)snprintf(kbuf, 32, "%c-%08d", which, i);
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
	(void)env->set_cachesize(env, 0, 4 * 1024 * 1024, 1);
	if ((ret = env->open(env, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN, 0664)) != 0)
		return (ret);
	if ((ret = open_db(env, &db, 1)) != 0)
		return (ret);

	/* A large txn: NOPS puts, then EXPLICIT abort (in-line undo). */
	if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
		return (ret);
	for (i = 0; i < NOPS; i++) {
		mkrec('a', i, kbuf, vbuf);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
		if ((ret = db->put(db, txn, &key, &data, 0)) != 0)
			return (ret);
	}
	if ((ret = txn->abort(txn)) != 0)
		return (ret);

	/* A durable committed txn AFTER the abort: must survive the crash. */
	if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
		return (ret);
	for (i = 0; i < NCOMMIT; i++) {
		mkrec('c', i, kbuf, vbuf);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
		if ((ret = db->put(db, txn, &key, &data, 0)) != 0)
			return (ret);
	}
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
	uint64_t seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0x1A4B0;
	DB_ENV *env;
	DB *db;
	DBT key, data;
	char kbuf[32], vbuf[32];
	int i, ret, missing = 0, mismatch = 0, ghost = 0;

	if (sim_fresh_home(HOME) != 0)
		return (EXIT_FAILURE);
	if (sim_run_crash_child(seed, populate) != 0)
		return (EXIT_FAILURE);

	if (sim_env_recover(HOME, &env) != 0)
		return (EXIT_FAILURE);
	if (open_db(env, &db, 0) != 0)
		return (EXIT_FAILURE);

	__db_sim_activate(seed);
	/* Committed txn: every record present. */
	for (i = 0; i < NCOMMIT; i++) {
		mkrec('c', i, kbuf, vbuf);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		if (db->get(db, NULL, &key, &data, 0) != 0)
			missing++;
		else if (data.size != strlen(vbuf) + 1 ||
		    memcmp(data.data, vbuf, data.size) != 0)
			mismatch++;
	}
	/* Aborted large txn: NOT ONE record survives. */
	for (i = 0; i < NOPS; i++) {
		mkrec('a', i, kbuf, vbuf);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		if (db->get(db, NULL, &key, &data, 0) == 0)
			ghost++;
	}
	__db_sim_deactivate();
	(void)db->close(db, 0);

	if ((ret = db_create(&db, env, 0)) != 0)
		return (EXIT_FAILURE);
	if ((ret = db->verify(db, DBFILE, NULL, NULL, 0)) != 0) {
		fprintf(stderr, "test_sim_largeabort: verify FAILED: %s\n",
		    db_strerror(ret));
		(void)env->close(env, 0);
		return (EXIT_FAILURE);
	}
	(void)env->close(env, 0);

	printf("test_sim_largeabort: %d missing, %d mismatch (of %d committed),"
	    " %d ghost (of %d aborted) (seed 0x%llx)\n", missing, mismatch,
	    NCOMMIT, ghost, NOPS, (unsigned long long)seed);
	if (missing != 0 || mismatch != 0 || ghost != 0) {
		fprintf(stderr, "test_sim_largeabort: FAIL -- large-abort "
		    "atomicity broken (seed 0x%llx)\n",
		    (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	printf("test_sim_largeabort: PASS -- large aborted txn (%d ops) left "
	    "no trace, committed txn (%d ops) survived, tree clean "
	    "(seed 0x%llx)\n", NOPS, NCOMMIT, (unsigned long long)seed);
	return (EXIT_SUCCESS);
}
