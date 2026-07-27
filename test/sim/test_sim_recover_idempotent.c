/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_recover_idempotent.c --
 *	Recovery idempotency scenario.  Recovery must be idempotent: running
 *	it a second time on an already-recovered environment must reach the
 *	exact same state.  A crash leaves a dirty env; we recover, snapshot
 *	the full key/value state as a hash, recover AGAIN, snapshot again,
 *	and assert the two hashes are identical.  A recovery that
 *	double-applies (or fails to re-converge) a redo/undo would diverge.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_recover_idempotent && ./test_sim_recover_idempotent [seed]
 */

#include "sim_scenario.h"

#define HOME    "TESTDIR_sim_idem"
#define DBFILE  "idem.db"
#define NCOMMIT 80

static void
mkrec(i, kbuf, vbuf)
	int i;
	char *kbuf, *vbuf;
{
	uint64_t tok = __db_sim_rng(DB_SIM_RNG_APP);
	(void)snprintf(kbuf, 32, "id-%08d", i);
	(void)snprintf(vbuf, 32, "iv-%016llx", (unsigned long long)tok);
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
	/* Uncommitted tail, then crash. */
	mkrec(NCOMMIT, kbuf, vbuf);
	if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
		return (ret);
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
	data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
	(void)db->put(db, txn, &key, &data, 0);

	SIM_CRASH_EXIT();
	return (0);
}

/* Recover, then walk the whole tree in key order into an FNV-1a hash of
 * every (key,value) pair -- a full-state fingerprint. */
static int
recover_and_hash(hashp)
	uint64_t *hashp;
{
	DB_ENV *env;
	DB *db;
	DBC *dbc;
	DBT key, data;
	uint64_t h = 1469598103934665603ull;
	int ret;
	const unsigned char *p, *e;

	if (sim_env_recover(HOME, &env) != 0)
		return (-1);
	if (open_db(env, &db, 0) != 0)
		return (-1);
	if ((ret = db->cursor(db, NULL, &dbc, 0)) != 0) {
		(void)env->close(env, 0);
		return (-1);
	}
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	while ((ret = dbc->get(dbc, &key, &data, DB_NEXT)) == 0) {
		for (p = key.data, e = p + key.size; p < e; p++) {
			h ^= *p; h *= 1099511628211ull;
		}
		for (p = data.data, e = p + data.size; p < e; p++) {
			h ^= *p; h *= 1099511628211ull;
		}
	}
	(void)dbc->close(dbc);
	(void)db->close(db, 0);
	(void)env->close(env, 0);
	if (ret != DB_NOTFOUND)
		return (-1);
	*hashp = h;
	return (0);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	uint64_t seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0x1DE33;
	uint64_t h1 = 0, h2 = 0;

	if (sim_fresh_home(HOME) != 0)
		return (EXIT_FAILURE);
	if (sim_run_crash_child(seed, populate) != 0)
		return (EXIT_FAILURE);

	/* First recovery + full-state fingerprint. */
	if (recover_and_hash(&h1) != 0) {
		fprintf(stderr, "test_sim_recover_idempotent: first recovery "
		    "failed (seed 0x%llx)\n", (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	/* Second recovery on the already-recovered env + fingerprint. */
	if (recover_and_hash(&h2) != 0) {
		fprintf(stderr, "test_sim_recover_idempotent: second recovery "
		    "failed (seed 0x%llx)\n", (unsigned long long)seed);
		return (EXIT_FAILURE);
	}

	if (h1 != h2) {
		fprintf(stderr, "test_sim_recover_idempotent: FAIL -- state "
		    "diverged across a second recovery: %016llx != %016llx "
		    "(seed 0x%llx)\n", (unsigned long long)h1,
		    (unsigned long long)h2, (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	printf("test_sim_recover_idempotent: PASS -- recovery idempotent; "
	    "identical state hash %016llx across two recoveries (seed 0x%llx)\n",
	    (unsigned long long)h1, (unsigned long long)seed);
	return (EXIT_SUCCESS);
}
