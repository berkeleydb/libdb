/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_queue_crash.c --
 *	Access-method crash/recover scenario for DB_QUEUE (fixed-length
 *	records): enqueue N records in durable txns, crash mid-uncommitted
 *	enqueue (dropping every un-fsync'd byte), recover, and assert every
 *	committed record survived, the uncommitted one did not, and the DB
 *	verifies clean.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_queue_crash && ./test_sim_queue_crash [seed]
 */

#include "sim_scenario.h"

#define HOME    "TESTDIR_sim_queue"
#define DBFILE  "queue.db"
#define NCOMMIT 64
#define RECLEN  24

static void
mkval(i, vbuf)
	int i;
	char *vbuf;
{
	uint64_t tok = __db_sim_rng(DB_SIM_RNG_APP);
	(void)snprintf(vbuf, RECLEN, "q%06d-%08llx", i,
	    (unsigned long long)(tok & 0xffffffffu));
	/* Pad to a fixed RECLEN (queue records are fixed-length). */
	memset(vbuf + strlen(vbuf), '.', RECLEN - 1 - strlen(vbuf));
	vbuf[RECLEN - 1] = '\0';
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
	(void)db->set_re_len(db, RECLEN);
	if ((ret = db->open(db, NULL, DBFILE, NULL, DB_QUEUE,
	    (create ? DB_CREATE : 0) | DB_AUTO_COMMIT, 0664)) != 0) {
		fprintf(stderr, "queue open failed: %s\n", db_strerror(ret));
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
	db_recno_t recno;
	char vbuf[RECLEN];
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
		mkval(i, vbuf);
		if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
			return (ret);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = &recno; key.size = sizeof(recno);
		data.data = vbuf; data.size = RECLEN;
		if ((ret = db->put(db, txn, &key, &data, DB_APPEND)) != 0)
			return (ret);
		if ((ret = txn->commit(txn, DB_TXN_SYNC)) != 0)
			return (ret);
	}

	mkval(NCOMMIT, vbuf);
	if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
		return (ret);
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = &recno; key.size = sizeof(recno);
	data.data = vbuf; data.size = RECLEN;
	(void)db->put(db, txn, &key, &data, DB_APPEND);

	SIM_CRASH_EXIT();
	return (0);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	uint64_t seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0x9EE;
	DB_ENV *env;
	DB *db;
	DBT key, data;
	db_recno_t recno;
	char vbuf[RECLEN];
	int i, ret, missing = 0, mismatch = 0, extra = 0;

	if (sim_fresh_home(HOME) != 0)
		return (EXIT_FAILURE);
	if (sim_run_crash_child(seed, populate) != 0)
		return (EXIT_FAILURE);

	if (sim_env_recover(HOME, &env) != 0)
		return (EXIT_FAILURE);
	if (open_db(env, &db, 0) != 0)
		return (EXIT_FAILURE);

	__db_sim_activate(seed);
	for (i = 0; i < NCOMMIT; i++) {
		mkval(i, vbuf);
		recno = (db_recno_t)(i + 1);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = &recno; key.size = sizeof(recno);
		if (db->get(db, NULL, &key, &data, 0) != 0)
			missing++;
		else if (data.size != RECLEN ||
		    memcmp(data.data, vbuf, RECLEN) != 0)
			mismatch++;
	}
	recno = (db_recno_t)(NCOMMIT + 1);
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = &recno; key.size = sizeof(recno);
	if (db->get(db, NULL, &key, &data, 0) == 0)
		extra = 1;
	__db_sim_deactivate();
	(void)db->close(db, 0);

	if ((ret = db_create(&db, env, 0)) != 0)
		return (EXIT_FAILURE);
	if ((ret = db->verify(db, DBFILE, NULL, NULL, 0)) != 0) {
		fprintf(stderr, "verify FAILED: %s\n", db_strerror(ret));
		(void)env->close(env, 0);
		return (EXIT_FAILURE);
	}
	(void)env->close(env, 0);

	if (missing || mismatch || extra) {
		fprintf(stderr, "test_sim_queue_crash: FAIL -- %d missing, %d "
		    "mismatched, extra=%d (seed 0x%llx)\n",
		    missing, mismatch, extra, (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	printf("test_sim_queue_crash: PASS -- %d enqueued records survived, "
	    "uncommitted did not, verifies clean (seed 0x%llx)\n",
	    NCOMMIT, (unsigned long long)seed);
	return (EXIT_SUCCESS);
}
