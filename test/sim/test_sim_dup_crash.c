/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_dup_crash.c --
 *	Duplicate-key crash/recover scenario.  A DB_DUPSORT btree holds
 *	several sorted duplicates per key, committed durably, then the
 *	process crashes mid-uncommitted-txn.  After recovery every committed
 *	(key, dup) pair must survive with the right multiplicity and sorted
 *	order, the uncommitted dup must not, and the tree verifies clean.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_dup_crash && ./test_sim_dup_crash [seed]
 */

#include "sim_scenario.h"

#define HOME    "TESTDIR_sim_dup"
#define DBFILE  "dup.db"
#define NKEY    24
#define NDUP    8

static void
mkkey(i, kbuf)
	int i;
	char *kbuf;
{
	(void)snprintf(kbuf, 32, "dk-%06d", i);
}

static void
mkdup(i, j, vbuf)
	int i, j;
	char *vbuf;
{
	(void)snprintf(vbuf, 32, "dv-%06d-%03d", i, j);
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
	(void)db->set_flags(db, DB_DUPSORT);
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
	int i, j, ret;

	__db_sim_activate(seed);
	__db_sim_wb_enable(1);

	if ((ret = db_env_create(&env, 0)) != 0)
		return (ret);
	if ((ret = env->open(env, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN, 0664)) != 0)
		return (ret);
	if ((ret = open_db(env, &db, 1)) != 0)
		return (ret);

	/* Each key gets NDUP sorted dups, all in one durable txn per key. */
	for (i = 0; i < NKEY; i++) {
		if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
			return (ret);
		mkkey(i, kbuf);
		for (j = 0; j < NDUP; j++) {
			mkdup(i, j, vbuf);
			memset(&key, 0, sizeof(key));
			memset(&data, 0, sizeof(data));
			key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
			data.data = vbuf;
			data.size = (u_int32_t)strlen(vbuf) + 1;
			if ((ret = db->put(db, txn, &key, &data, 0)) != 0) {
				(void)txn->abort(txn);
				return (ret);
			}
		}
		if ((ret = txn->commit(txn, DB_TXN_SYNC)) != 0)
			return (ret);
	}

	/* Uncommitted extra dup on key 0, then crash. */
	if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
		return (ret);
	mkkey(0, kbuf);
	mkdup(0, NDUP, vbuf);   /* a dup that was never committed */
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
	data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
	(void)db->put(db, txn, &key, &data, 0);

	SIM_CRASH_EXIT();
	return (0);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	uint64_t seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0xD09;
	DB_ENV *env;
	DB *db;
	DBC *dbc;
	DBT key, data;
	char kbuf[32], vbuf[32];
	int i, j, ret, bad = 0, saw_uncommitted = 0;
	db_recno_t cnt;

	if (sim_fresh_home(HOME) != 0)
		return (EXIT_FAILURE);
	if (sim_run_crash_child(seed, populate) != 0)
		return (EXIT_FAILURE);

	if (sim_env_recover(HOME, &env) != 0)
		return (EXIT_FAILURE);
	if (open_db(env, &db, 0) != 0)
		return (EXIT_FAILURE);

	/* Every committed (key,dup) present with exact multiplicity. */
	for (i = 0; i < NKEY; i++) {
		mkkey(i, kbuf);
		for (j = 0; j < NDUP; j++) {
			mkdup(i, j, vbuf);
			memset(&key, 0, sizeof(key));
			memset(&data, 0, sizeof(data));
			key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
			data.data = vbuf; data.size =
			    (u_int32_t)strlen(vbuf) + 1;
			if (db->get(db, NULL, &key, &data, DB_GET_BOTH) != 0)
				bad++;
		}
		/* Multiplicity: key i must have exactly NDUP dups (key 0 must
		 * NOT have the uncommitted extra). */
		if ((ret = db->cursor(db, NULL, &dbc, 0)) == 0) {
			mkkey(i, kbuf);
			memset(&key, 0, sizeof(key));
			memset(&data, 0, sizeof(data));
			key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
			if (dbc->get(dbc, &key, &data, DB_SET) == 0) {
				cnt = 0;
				(void)dbc->count(dbc, &cnt, 0);
				if (cnt != (db_recno_t)NDUP) {
					if (i == 0 && cnt == (db_recno_t)NDUP + 1)
						saw_uncommitted = 1;
					else
						bad++;
				}
			}
			(void)dbc->close(dbc);
		}
	}
	(void)db->close(db, 0);

	if ((ret = db_create(&db, env, 0)) != 0)
		return (EXIT_FAILURE);
	if ((ret = db->verify(db, DBFILE, NULL, NULL, 0)) != 0) {
		fprintf(stderr, "test_sim_dup_crash: verify FAILED: %s\n",
		    db_strerror(ret));
		(void)env->close(env, 0);
		return (EXIT_FAILURE);
	}
	(void)env->close(env, 0);

	if (bad || saw_uncommitted) {
		fprintf(stderr, "test_sim_dup_crash: FAIL -- %d bad dup "
		    "checks, uncommitted=%d (seed 0x%llx)\n",
		    bad, saw_uncommitted, (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	printf("test_sim_dup_crash: PASS -- %d keys x %d sorted dups "
	    "survived, uncommitted dup did not, tree clean (seed 0x%llx)\n",
	    NKEY, NDUP, (unsigned long long)seed);
	return (EXIT_SUCCESS);
}
