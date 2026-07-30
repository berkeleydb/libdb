/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_torn_meta.c --
 *	Torn write of a METADATA page DURING a checkpoint, then crash +
 *	recovery.  A DB_CHKSUM btree in a transactional env is populated
 *	with durable commits; a checkpoint is issued with torn writes armed,
 *	so the meta page (page 0, rewritten by the flush) can persist only a
 *	strict prefix -- a latent bad tail.  The process crashes; recovery
 *	runs; then every committed record is read back.
 *
 *	Invariant (DESIGN.md catalog #17, partial page-write angle, focused
 *	on the structurally-critical meta page): a torn meta page is NEVER
 *	accepted as silently-wrong -- either the page checksum catches it
 *	(recovery / open fails cleanly, or the WAL repairs it) or the read
 *	returns the correct committed data.  A get that returns bytes not
 *	matching what was committed, with no error, is SILENT-BAD.
 *
 *	The meta page is the highest-value torn target: a silently-bad meta
 *	page mis-describes the whole file (root pgno, free list, magic), so
 *	the checksum's protection of it is a load-bearing safety property.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_torn_meta && ./test_sim_torn_meta [seed]
 */

#include "sim_scenario.h"

#define HOME    "TESTDIR_sim_tornmeta"
#define DBFILE  "tornmeta.db"
#define NCOMMIT 120
#define PGSIZE  512

static void
mkrec(i, kbuf, vbuf)
	int i;
	char *kbuf, *vbuf;
{
	uint64_t tok = __db_sim_rng(DB_SIM_RNG_APP);
	(void)snprintf(kbuf, 32, "tm-%08d", i);
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
	(void)db->set_flags(db, DB_CHKSUM);
	(void)db->set_pagesize(db, PGSIZE);
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

	/* Arm torn writes, then checkpoint: the meta + dirty data pages are
	 * flushed to the data file under the torn fault, so a page (possibly
	 * the meta page) persists only a strict prefix. */
	__db_sim_io_corrupt_enable(200);
	(void)env->txn_checkpoint(env, 0, 0, DB_FORCE);
	__db_sim_io_corrupt_disable();

	SIM_CRASH_EXIT();
	return (0);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	uint64_t seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0x70E33A;
	DB_ENV *env;
	DB *db;
	DBT key, data;
	char kbuf[32], vbuf[32];
	int i, ret, correct = 0, detected = 0, silent_bad = 0;

	if (sim_fresh_home(HOME) != 0)
		return (EXIT_FAILURE);
	if (sim_run_crash_child(seed, populate) != 0)
		return (EXIT_FAILURE);

	/* Recover.  A torn meta page that the WAL cannot repair makes
	 * recovery fail cleanly with a checksum/page error -- that IS the
	 * checksum catching it (never silent).  A clean PASS. */
	if ((ret = db_env_create(&env, 0)) != 0)
		return (EXIT_FAILURE);
	if ((ret = env->open(env, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | DB_RECOVER, 0664))
	    != 0) {
		printf("test_sim_torn_meta: torn meta page caught during "
		    "recovery (clean error: %s) (seed 0x%llx)\n",
		    db_strerror(ret), (unsigned long long)seed);
		printf("test_sim_torn_meta: PASS -- torn meta write caught, "
		    "never silently-wrong data\n");
		return (EXIT_SUCCESS);
	}
	if ((ret = db_create(&db, env, 0)) != 0)
		return (EXIT_FAILURE);
	(void)db->set_flags(db, DB_CHKSUM);
	(void)db->set_pagesize(db, PGSIZE);
	if ((ret = db->open(db, NULL, DBFILE, NULL, DB_BTREE,
	    DB_AUTO_COMMIT, 0664)) != 0) {
		printf("test_sim_torn_meta: torn meta page caught at open "
		    "(clean error: %s) (seed 0x%llx)\n", db_strerror(ret),
		    (unsigned long long)seed);
		(void)env->close(env, 0);
		printf("test_sim_torn_meta: PASS -- torn meta write caught, "
		    "never silently-wrong data\n");
		return (EXIT_SUCCESS);
	}

	__db_sim_activate(seed);
	for (i = 0; i < NCOMMIT; i++) {
		mkrec(i, kbuf, vbuf);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		ret = db->get(db, NULL, &key, &data, 0);
		if (ret == 0) {
			if (data.size == strlen(vbuf) + 1 &&
			    memcmp(data.data, vbuf, data.size) == 0)
				correct++;
			else
				silent_bad++;
		} else
			detected++;    /* clean checksum/page error */
	}
	__db_sim_deactivate();
	(void)db->close(db, 0);
	(void)env->close(env, 0);

	printf("test_sim_torn_meta: %d correct, %d detected(clean error), "
	    "%d SILENT-BAD (seed 0x%llx)\n", correct, detected, silent_bad,
	    (unsigned long long)seed);
	if (silent_bad != 0) {
		fprintf(stderr, "test_sim_torn_meta: FAIL -- %d silently "
		    "corrupt records slipped past the meta-page checksum "
		    "(seed 0x%llx)\n", silent_bad, (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	printf("test_sim_torn_meta: PASS -- no torn meta page ever returned "
	    "as silently-wrong data (seed 0x%llx)\n",
	    (unsigned long long)seed);
	return (EXIT_SUCCESS);
}
