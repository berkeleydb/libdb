/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_torn_log.c --
 *	Torn LOG write scenario.  A torn write persists only a seeded prefix
 *	of a buffer but reports full success (a real partial page/sector
 *	write at a power loss).  Armed on the log, the last flushed log
 *	block can be torn.  Recovery MUST stop cleanly at the last intact
 *	record -- it must not misparse the torn tail into a bogus record,
 *	panic, or silently corrupt the tree.
 *
 *	Invariant: after a crash with a possibly-torn log, DB_RECOVER
 *	succeeds (or fails with a clean error), the DB verifies clean, and
 *	every txn that committed BEFORE the torn point is present.  We do
 *	not assert anything about the record straddling the tear -- that is
 *	the legitimate crash gray zone -- only that recovery is SAFE.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_torn_log && ./test_sim_torn_log [seed]
 */

#include "sim_scenario.h"

#define HOME    "TESTDIR_sim_tornlog"
#define DBFILE  "tornlog.db"
#define NSYNC   32     /* fsync'd (durable) commits -- MUST survive */
#define NNOSYNC 32     /* NOSYNC commits after -- may be torn away */

static void
mkrec(i, kbuf, vbuf)
	int i;
	char *kbuf, *vbuf;
{
	uint64_t tok = __db_sim_rng(DB_SIM_RNG_APP);
	(void)snprintf(kbuf, 32, "tl-%08d", i);
	(void)snprintf(vbuf, 32, "tv-%016llx", (unsigned long long)tok);
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
one(env, db, i, sync)
	DB_ENV *env;
	DB *db;
	int i, sync;
{
	DB_TXN *txn;
	DBT key, data;
	char kbuf[32], vbuf[32];
	int ret;

	mkrec(i, kbuf, vbuf);
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
	return (txn->commit(txn, sync ? DB_TXN_SYNC : DB_TXN_NOSYNC));
}

static int
populate(seed)
	uint64_t seed;
{
	DB_ENV *env;
	DB *db;
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

	/* Durable prefix -- these fsync'd commits must all survive. */
	for (i = 0; i < NSYNC; i++)
		if ((ret = one(env, db, i, 1)) != 0)
			return (ret);

	/* Now arm torn writes and do NOSYNC commits: their log blocks may be
	 * torn.  The write-back crash then drops un-fsync'd bytes AND a torn
	 * write already left a partial tail on the last flushed block. */
	__db_sim_io_corrupt_enable(200);   /* 20% of writes torn */
	for (i = NSYNC; i < NSYNC + NNOSYNC; i++)
		(void)one(env, db, i, 0);

	SIM_CRASH_EXIT();
	return (0);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	uint64_t seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0x70A;
	DB_ENV *env;
	DB *db;
	DBT key, data;
	char kbuf[32], vbuf[32];
	int i, ret, missing = 0;

	if (sim_fresh_home(HOME) != 0)
		return (EXIT_FAILURE);
	if (sim_run_crash_child(seed, populate) != 0)
		return (EXIT_FAILURE);

	/* Recovery must be SAFE even with a torn log tail. */
	if ((ret = sim_env_recover(HOME, &env)) != 0) {
		fprintf(stderr, "test_sim_torn_log: recovery did not complete "
		    "cleanly (%s) -- a torn log tail was misparsed "
		    "(seed 0x%llx)\n", db_strerror(ret),
		    (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	if (open_db(env, &db, 0) != 0)
		return (EXIT_FAILURE);

	__db_sim_activate(seed);
	for (i = 0; i < NSYNC; i++) {
		mkrec(i, kbuf, vbuf);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		if (db->get(db, NULL, &key, &data, 0) != 0)
			missing++;
	}
	__db_sim_deactivate();
	(void)db->close(db, 0);

	/* The tree must be structurally clean after recovering a torn log. */
	if ((ret = db_create(&db, env, 0)) != 0)
		return (EXIT_FAILURE);
	if ((ret = db->verify(db, DBFILE, NULL, NULL, 0)) != 0) {
		fprintf(stderr, "test_sim_torn_log: verify FAILED after "
		    "torn-log recovery: %s (seed 0x%llx)\n",
		    db_strerror(ret), (unsigned long long)seed);
		(void)env->close(env, 0);
		return (EXIT_FAILURE);
	}
	(void)env->close(env, 0);

	if (missing != 0) {
		fprintf(stderr, "test_sim_torn_log: FAIL -- %d durable "
		    "(fsync'd) commits lost to a torn log (seed 0x%llx)\n",
		    missing, (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	printf("test_sim_torn_log: PASS -- recovery safe past a torn log "
	    "tail; all %d durable commits present, tree clean (seed 0x%llx)\n",
	    NSYNC, (unsigned long long)seed);
	return (EXIT_SUCCESS);
}
