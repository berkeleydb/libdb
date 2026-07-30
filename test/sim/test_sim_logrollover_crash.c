/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_logrollover_crash.c --
 *	Log-file ROLLOVER crash + recovery.  A small log-file size (set via
 *	DB_ENV->set_lg_max) forces the WAL to roll over to a new log file
 *	several times during the workload, so committed txns straddle
 *	multiple physical log files.  The process crashes; recovery must
 *	walk BACKWARD across every log-file boundary and forward-roll
 *	correctly.  After recovery every durable commit must survive,
 *	regardless of which log file its record landed in.
 *
 *	Invariant (DESIGN.md catalog #12): a crash with the WAL spread over
 *	multiple log files recovers cleanly -- recovery correctly chains the
 *	log files (no commit lost at a rollover boundary, no misparse of the
 *	seam between files) and the tree verifies clean.  The write-back
 *	crash model drops only the un-fsync'd tail of the CURRENT log file;
 *	every fsync'd commit in every prior (full) log file survives.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_logrollover_crash && ./test_sim_logrollover_crash [seed]
 */

#include "sim_scenario.h"

#define HOME    "TESTDIR_sim_logroll"
#define DBFILE  "logroll.db"
#define NCOMMIT 400           /* enough to roll several small log files */
#define LGMAX   (64 * 1024)   /* 64KB log files: forces many rollovers */

static void
mkrec(i, kbuf, vbuf)
	int i;
	char *kbuf, *vbuf;
{
	uint64_t tok = __db_sim_rng(DB_SIM_RNG_APP);
	int j;
	(void)snprintf(kbuf, 32, "lr-%08d", i);
	/* A chunky value so the log fills and rolls faster. */
	(void)snprintf(vbuf, 64, "lv-%016llx-", (unsigned long long)tok);
	for (j = (int)strlen(vbuf); j < 60; j++)
		vbuf[j] = (char)('0' + ((i + j) % 10));
	vbuf[60] = '\0';
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
open_env(env, home, recover)
	DB_ENV **env;
	const char *home;
	int recover;
{
	DB_ENV *e;
	int ret;

	if ((ret = db_env_create(&e, 0)) != 0)
		return (ret);
	/* Small log files force rollovers -- set BEFORE open. */
	(void)e->set_lg_max(e, LGMAX);
	if ((ret = e->open(e, home, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN |
	    (recover ? DB_RECOVER : 0), 0664)) != 0) {
		fprintf(stderr, "env open: %s\n", db_strerror(ret));
		return (ret);
	}
	*env = e;
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
	char kbuf[32], vbuf[64];
	int i, ret;

	__db_sim_activate(seed);
	__db_sim_wb_enable(1);

	if ((ret = open_env(&env, HOME, 0)) != 0)
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

	/* Uncommitted tail, then crash mid-txn (in the current log file). */
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

/* Count the log files present so the test can confirm a rollover happened. */
static int
count_logfiles(home)
	const char *home;
{
	char cmd[512];
	FILE *fp;
	int n = 0;
	char line[64];

	(void)snprintf(cmd, sizeof(cmd),
	    "ls %s/log.* 2>/dev/null | wc -l", home);
	if ((fp = popen(cmd, "r")) == NULL)
		return (-1);
	if (fgets(line, sizeof(line), fp) != NULL)
		n = atoi(line);
	(void)pclose(fp);
	return (n);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	uint64_t seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0x106401;
	DB_ENV *env;
	DB *db;
	DBT key, data;
	char kbuf[32], vbuf[64];
	int i, ret, missing = 0, mismatch = 0, saw_uncommitted = 0, nlogs;

	if (sim_fresh_home(HOME) != 0)
		return (EXIT_FAILURE);
	if (sim_run_crash_child(seed, populate) != 0)
		return (EXIT_FAILURE);

	nlogs = count_logfiles(HOME);

	if (open_env(&env, HOME, 1) != 0)
		return (EXIT_FAILURE);
	if (open_db(env, &db, 0) != 0)
		return (EXIT_FAILURE);

	__db_sim_activate(seed);
	for (i = 0; i < NCOMMIT; i++) {
		mkrec(i, kbuf, vbuf);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		if (db->get(db, NULL, &key, &data, 0) != 0)
			missing++;
		else if (data.size != strlen(vbuf) + 1 ||
		    memcmp(data.data, vbuf, data.size) != 0)
			mismatch++;
	}
	mkrec(NCOMMIT, kbuf, vbuf);
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
	if (db->get(db, NULL, &key, &data, 0) == 0)
		saw_uncommitted = 1;
	__db_sim_deactivate();
	(void)db->close(db, 0);

	if ((ret = db_create(&db, env, 0)) != 0)
		return (EXIT_FAILURE);
	if ((ret = db->verify(db, DBFILE, NULL, NULL, 0)) != 0) {
		fprintf(stderr, "test_sim_logrollover_crash: verify FAILED: %s "
		    "(seed 0x%llx)\n", db_strerror(ret),
		    (unsigned long long)seed);
		(void)env->close(env, 0);
		return (EXIT_FAILURE);
	}
	(void)env->close(env, 0);

	printf("test_sim_logrollover_crash: %d log files, %d missing, %d "
	    "mismatch, uncommitted=%d (seed 0x%llx)\n", nlogs, missing,
	    mismatch, saw_uncommitted, (unsigned long long)seed);

	if (nlogs < 2) {
		fprintf(stderr, "test_sim_logrollover_crash: FAIL -- only %d "
		    "log file(s); no rollover occurred, the scenario is not "
		    "exercising the boundary (seed 0x%llx)\n", nlogs,
		    (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	if (missing != 0 || mismatch != 0 || saw_uncommitted != 0) {
		fprintf(stderr, "test_sim_logrollover_crash: FAIL -- %d "
		    "missing, %d mismatch, uncommitted=%d across a log "
		    "rollover (seed 0x%llx)\n", missing, mismatch,
		    saw_uncommitted, (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	printf("test_sim_logrollover_crash: PASS -- all %d committed txns "
	    "survived a crash across %d log files, uncommitted gone, tree "
	    "clean (seed 0x%llx)\n", NCOMMIT, nlogs, (unsigned long long)seed);
	return (EXIT_SUCCESS);
}
