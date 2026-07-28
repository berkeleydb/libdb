/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_largetxn_crash.c --
 *	Large-transaction (many ops in ONE txn) atomicity across a crash.
 *	One BIG txn writes NOPS records and commits durably; a SECOND big
 *	txn writes NOPS more records and is left UNCOMMITTED when the crash
 *	hits.  After recovery: every record of the committed big txn is
 *	present (all-or-nothing: the txn was atomic), and NOT ONE record of
 *	the uncommitted big txn survives (a partial large-txn survivor would
 *	be an atomicity violation).
 *
 *	Invariant (DESIGN.md catalog #8, large-txn angle): a large multi-op
 *	transaction is atomic across a crash -- all of a committed one
 *	survives, none of an uncommitted one does.  This stresses the log's
 *	handling of a long single-txn record chain and the recovery
 *	undo/redo over many ops in one commit boundary.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_largetxn_crash && ./test_sim_largetxn_crash [seed]
 */

#include "sim_scenario.h"

#define HOME    "TESTDIR_sim_largetxn"
#define DBFILE  "largetxn.db"
#define NOPS    2000          /* ops in ONE txn (large) */

static void
mkrec(committed, i, kbuf, vbuf)
	int committed, i;
	char *kbuf, *vbuf;
{
	uint64_t tok = __db_sim_rng(DB_SIM_RNG_APP);
	(void)snprintf(kbuf, 32, "%s-%08d", committed ? "cm" : "un", i);
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
	/* Bigger cache + log so a 2000-op txn fits without a trickle sync. */
	(void)env->set_cachesize(env, 0, 4 * 1024 * 1024, 1);
	if ((ret = env->open(env, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN, 0664)) != 0)
		return (ret);
	if ((ret = open_db(env, &db, 1)) != 0)
		return (ret);

	/* ONE large committed txn: NOPS puts, then a single durable commit. */
	if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
		return (ret);
	for (i = 0; i < NOPS; i++) {
		mkrec(1, i, kbuf, vbuf);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
		if ((ret = db->put(db, txn, &key, &data, 0)) != 0)
			return (ret);
	}
	if ((ret = txn->commit(txn, DB_TXN_SYNC)) != 0)
		return (ret);

	/* ONE large UNCOMMITTED txn: NOPS puts, then crash mid-txn.  None
	 * of these may survive. */
	if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
		return (ret);
	for (i = 0; i < NOPS; i++) {
		mkrec(0, i, kbuf, vbuf);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
		(void)db->put(db, txn, &key, &data, 0);
	}
	/* deliberately DO NOT commit */

	SIM_CRASH_EXIT();
	return (0);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	uint64_t seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0x1A46E;
	DB_ENV *env;
	DB *db;
	DBT key, data;
	char kbuf[32], vbuf[32];
	int i, ret, missing = 0, ghost = 0;

	if (sim_fresh_home(HOME) != 0)
		return (EXIT_FAILURE);
	if (sim_run_crash_child(seed, populate) != 0)
		return (EXIT_FAILURE);

	if (sim_env_recover(HOME, &env) != 0)
		return (EXIT_FAILURE);
	if (open_db(env, &db, 0) != 0)
		return (EXIT_FAILURE);

	__db_sim_activate(seed);
	/* Committed big txn: every record present (atomic all). */
	for (i = 0; i < NOPS; i++) {
		mkrec(1, i, kbuf, vbuf);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		if (db->get(db, NULL, &key, &data, 0) != 0)
			missing++;
		else if (data.size != strlen(vbuf) + 1 ||
		    memcmp(data.data, vbuf, data.size) != 0)
			missing++;
	}
	/* Uncommitted big txn: NOT ONE record survives (atomic none). */
	for (i = 0; i < NOPS; i++) {
		mkrec(0, i, kbuf, vbuf);
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
		fprintf(stderr, "test_sim_largetxn_crash: verify FAILED: %s\n",
		    db_strerror(ret));
		(void)env->close(env, 0);
		return (EXIT_FAILURE);
	}
	(void)env->close(env, 0);

	if (missing != 0 || ghost != 0) {
		fprintf(stderr, "test_sim_largetxn_crash: FAIL -- %d of %d "
		    "committed missing, %d uncommitted survived (large-txn "
		    "atomicity broken, seed 0x%llx)\n", missing, NOPS, ghost,
		    (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	printf("test_sim_largetxn_crash: PASS -- large committed txn (%d ops) "
	    "atomic-all survived, large uncommitted txn atomic-none, tree "
	    "clean (seed 0x%llx)\n", NOPS, (unsigned long long)seed);
	return (EXIT_SUCCESS);
}
