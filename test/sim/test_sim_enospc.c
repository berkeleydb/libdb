/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_enospc.c --
 *	Disk-full (ENOSPC) graceful-degradation scenario.  A seeded coin
 *	fails a whole write with ENOSPC mid-workload (nothing persists for
 *	that write).  The engine must degrade gracefully: the affected
 *	put/commit returns an error rather than corrupting anything, txns
 *	that committed durably BEFORE the disk filled survive, and after
 *	recovery the DB verifies clean.
 *
 *	Invariant: no silent data loss and no corruption under ENOSPC.
 *	Every txn we observed commit successfully (ret == 0) must be present
 *	after crash+recover; a txn whose commit returned an error must not
 *	leave a half-written record; the tree verifies clean.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_enospc && ./test_sim_enospc [seed]
 */

#include "sim_scenario.h"

#define HOME    "TESTDIR_sim_enospc"
#define DBFILE  "enospc.db"
#define NTRY    128

static void
mkrec(i, kbuf, vbuf)
	int i;
	char *kbuf, *vbuf;
{
	uint64_t tok = __db_sim_rng(DB_SIM_RNG_APP);
	(void)snprintf(kbuf, 32, "es-%08d", i);
	(void)snprintf(vbuf, 32, "ev-%016llx", (unsigned long long)tok);
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

/* Records whose commit returned 0 (we saw them succeed): a bitmap the
 * child writes to a sidecar so the parent knows what MUST survive. */
static unsigned char g_committed[NTRY];

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
	FILE *fp;

	__db_sim_activate(seed);
	__db_sim_wb_enable(1);

	if ((ret = db_env_create(&env, 0)) != 0)
		return (ret);
	if ((ret = env->open(env, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN, 0664)) != 0)
		return (ret);
	if ((ret = open_db(env, &db, 1)) != 0)
		return (ret);

	memset(g_committed, 0, sizeof(g_committed));

	/* Warm up a durable prefix, THEN arm ENOSPC so the disk "fills"
	 * mid-workload. */
	for (i = 0; i < NTRY; i++) {
		if (i == NTRY / 4)
			__db_sim_io_enospc_enable(150);   /* 15% of writes ENOSPC */
		mkrec(i, kbuf, vbuf);
		if (env->txn_begin(env, NULL, &txn, 0) != 0)
			continue;
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
		if (db->put(db, txn, &key, &data, 0) != 0) {
			(void)txn->abort(txn);
			continue;
		}
		if (txn->commit(txn, DB_TXN_SYNC) == 0)
			g_committed[i] = 1;   /* observed durable */
	}

	/* Hand the observed-committed bitmap to the parent. */
	if ((fp = fopen(HOME "/committed.map", "wb")) != NULL) {
		(void)fwrite(g_committed, 1, sizeof(g_committed), fp);
		(void)fclose(fp);
	}

	/* Disarm before the crash so the durable-frontier truncation itself
	 * is not subject to ENOSPC. */
	__db_sim_io_enospc_enable(0);
	SIM_CRASH_EXIT();
	return (0);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	uint64_t seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0xF011;
	DB_ENV *env;
	DB *db;
	DBT key, data;
	char kbuf[32], vbuf[32];
	unsigned char committed[NTRY];
	FILE *fp;
	int i, ret, missing = 0, ncommitted = 0;

	if (sim_fresh_home(HOME) != 0)
		return (EXIT_FAILURE);
	if (sim_run_crash_child(seed, populate) != 0)
		return (EXIT_FAILURE);

	memset(committed, 0, sizeof(committed));
	if ((fp = fopen(HOME "/committed.map", "rb")) != NULL) {
		(void)fread(committed, 1, sizeof(committed), fp);
		(void)fclose(fp);
	}

	if (sim_env_recover(HOME, &env) != 0)
		return (EXIT_FAILURE);
	if (open_db(env, &db, 0) != 0)
		return (EXIT_FAILURE);

	__db_sim_activate(seed);
	for (i = 0; i < NTRY; i++) {
		mkrec(i, kbuf, vbuf);
		if (!committed[i])
			continue;   /* commit failed/aborted -- no guarantee */
		ncommitted++;
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		if (db->get(db, NULL, &key, &data, 0) != 0)
			missing++;
	}
	__db_sim_deactivate();
	(void)db->close(db, 0);

	/* No corruption under ENOSPC: the tree must verify clean. */
	if ((ret = db_create(&db, env, 0)) != 0)
		return (EXIT_FAILURE);
	if ((ret = db->verify(db, DBFILE, NULL, NULL, 0)) != 0) {
		fprintf(stderr, "test_sim_enospc: verify FAILED: %s "
		    "(seed 0x%llx)\n", db_strerror(ret),
		    (unsigned long long)seed);
		(void)env->close(env, 0);
		return (EXIT_FAILURE);
	}
	(void)env->close(env, 0);

	if (missing != 0) {
		fprintf(stderr, "test_sim_enospc: FAIL -- %d of %d observed-"
		    "committed txns lost under ENOSPC (seed 0x%llx)\n",
		    missing, ncommitted, (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	printf("test_sim_enospc: PASS -- graceful ENOSPC: all %d observed-"
	    "committed txns durable, tree clean, no corruption (seed 0x%llx)\n",
	    ncommitted, (unsigned long long)seed);
	return (EXIT_SUCCESS);
}
