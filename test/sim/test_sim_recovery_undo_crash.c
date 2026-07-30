/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_recovery_undo_crash.c --
 *	Crash EARLY in a recovery pass -- during / just after the UNDO
 *	phase, before redo completes -- then recover.  Recovery runs the
 *	log backward first (undo uncommitted / open-txn changes) then
 *	forward (redo committed).  If the process dies after the undo pass
 *	has written some pages but before redo has re-applied the committed
 *	tail, the NEXT recovery must still converge: committed txns present,
 *	the aborted / uncommitted work gone, tree clean.
 *
 *	A workload commits a first batch of txns, EXPLICITLY aborts a middle
 *	batch (exercising the undo path), commits a final batch, then leaves
 *	an uncommitted tail and crashes.  Recovery is then crashed at the
 *	EARLIEST recovery I/O ops (op 1, 2, ...), and after finishing we
 *	assert:
 *	  - every COMMITTED txn present + exact,
 *	  - every ABORTED txn absent,
 *	  - the uncommitted tail absent,
 *	  - the tree verifies clean.
 *
 *	Deterministic: same seed => same commit/abort split => same recovery
 *	=> same early crash points => same outcome.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_recovery_undo_crash && ./test_sim_recovery_undo_crash [seed]
 */

#include "sim_scenario.h"

#define HOME    "TESTDIR_sim_rec_undo"
#define DBFILE  "recundo.db"
#define NBATCH  40      /* per batch */

static void
mkrec(tag, i, kbuf, vbuf)
	char tag;
	int i;
	char *kbuf, *vbuf;
{
	int j;

	(void)snprintf(kbuf, 32, "ru-%c-%08d", tag, i);
	for (j = 0; j < 18; j++)
		vbuf[j] = (char)('a' + ((i * 5 + j + tag) % 26));
	vbuf[18] = '\0';
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

/* Put one batch under one txn, then commit or abort it. */
static int
batch(env, db, tag, commit)
	DB_ENV *env;
	DB *db;
	char tag;
	int commit;
{
	DB_TXN *txn;
	DBT key, data;
	char kbuf[32], vbuf[32];
	int i, ret;

	if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
		return (ret);
	for (i = 0; i < NBATCH; i++) {
		mkrec(tag, i, kbuf, vbuf);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
		if ((ret = db->put(db, txn, &key, &data, 0)) != 0) {
			(void)txn->abort(txn);
			return (ret);
		}
	}
	return (commit ? txn->commit(txn, DB_TXN_SYNC) : txn->abort(txn));
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
	int ret;

	__db_sim_activate(seed);
	__db_sim_wb_enable(1);

	if ((ret = db_env_create(&env, 0)) != 0)
		return (ret);
	if ((ret = env->open(env, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN, 0664)) != 0)
		return (ret);
	if ((ret = open_db(env, &db, 1)) != 0)
		return (ret);

	if ((ret = batch(env, db, 'C', 1)) != 0)   /* committed batch 1 */
		return (ret);
	if ((ret = batch(env, db, 'X', 0)) != 0)   /* aborted batch (undo) */
		return (ret);
	if ((ret = batch(env, db, 'D', 1)) != 0)   /* committed batch 2 */
		return (ret);

	/* Uncommitted tail (open txn at crash -> undone by recovery). */
	if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
		return (ret);
	mkrec('U', 0, kbuf, vbuf);
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
	data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
	(void)db->put(db, txn, &key, &data, 0);

	SIM_CRASH_EXIT();
	return (0);
}

static int
snap(save) const char *save;
{
	char cmd[512];
	(void)snprintf(cmd, sizeof(cmd), "rm -rf %s && cp -a %s %s",
	    save, HOME, save);
	return (system(cmd));
}
static int
rest(save) const char *save;
{
	char cmd[512];
	(void)snprintf(cmd, sizeof(cmd), "rm -rf %s && cp -a %s %s",
	    HOME, save, HOME);
	return (system(cmd));
}

/* Recover to completion; assert committed present, aborted+uncommitted
 * absent, tree clean.  Returns 0 = correct, 1 = invariant violation. */
static int
recover_and_check(seed)
	uint64_t seed;
{
	DB_ENV *env;
	DB *db;
	DBT key, data;
	char kbuf[32], vbuf[32];
	int i, ret, bad = 0, cpresent = 0, dpresent = 0, aleft = 0;

	if (sim_env_recover(HOME, &env) != 0)
		return (-1);
	if (open_db(env, &db, 0) != 0) {
		(void)env->close(env, 0);
		return (-1);
	}
	for (i = 0; i < NBATCH; i++) {
		/* committed 'C' + 'D' present + exact */
		mkrec('C', i, kbuf, vbuf);
		memset(&key, 0, sizeof(key)); memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		if (db->get(db, NULL, &key, &data, 0) == 0) {
			cpresent++;
			if (data.size != strlen(vbuf) + 1 ||
			    memcmp(data.data, vbuf, data.size) != 0)
				bad++;
		}
		mkrec('D', i, kbuf, vbuf);
		memset(&key, 0, sizeof(key)); memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		if (db->get(db, NULL, &key, &data, 0) == 0) {
			dpresent++;
			if (data.size != strlen(vbuf) + 1 ||
			    memcmp(data.data, vbuf, data.size) != 0)
				bad++;
		}
		/* aborted 'X' must be absent */
		mkrec('X', i, kbuf, vbuf);
		memset(&key, 0, sizeof(key)); memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		if (db->get(db, NULL, &key, &data, 0) == 0)
			aleft++;
	}
	/* uncommitted 'U' must be absent */
	mkrec('U', 0, kbuf, vbuf);
	memset(&key, 0, sizeof(key)); memset(&data, 0, sizeof(data));
	key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
	if (db->get(db, NULL, &key, &data, 0) == 0)
		aleft++;
	(void)db->close(db, 0);

	{
		DB *vdb;
		if (db_create(&vdb, env, 0) == 0 &&
		    vdb->verify(vdb, DBFILE, NULL, NULL, 0) != 0)
			bad++;
	}
	(void)env->close(env, 0);

	if (bad != 0 || cpresent != NBATCH || dpresent != NBATCH || aleft != 0)
		return (1);
	return (0);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	uint64_t seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0x00ED0;
	const char *save = HOME ".saved";
	unsigned long full_ticks = 0, c;
	int rc, trials = 0, crashes = 0, npoints, k;

	if (sim_fresh_home(HOME) != 0)
		return (EXIT_FAILURE);
	if (sim_run_crash_child(seed, populate) != 0)
		return (EXIT_FAILURE);
	if (snap(save) != 0)
		return (EXIT_FAILURE);

	rc = sim_recover_child(seed, HOME, 0, &full_ticks);
	if (rc != 0) {
		fprintf(stderr, "test_sim_recovery_undo_crash: probe recovery "
		    "failed (rc=%d, seed 0x%llx)\n", rc,
		    (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	if (rest(save) != 0)
		return (EXIT_FAILURE);

	printf("test_sim_recovery_undo_crash: full recovery %lu I/O ops "
	    "(seed 0x%llx)\n", full_ticks, (unsigned long long)seed);

	/* Crash at the EARLIEST recovery ops (undo region): 1..min(4,full). */
	npoints = (int)(full_ticks < 4 ? full_ticks : 4);
	if (npoints == 0) {
		if (recover_and_check(seed) != 0) {
			fprintf(stderr, "test_sim_recovery_undo_crash: FAIL -- "
			    "plain recovery incorrect (seed 0x%llx)\n",
			    (unsigned long long)seed);
			return (EXIT_FAILURE);
		}
		(void)sim_fresh_home(save);
		printf("test_sim_recovery_undo_crash: PASS -- recovery correct "
		    "(no interruptible I/O this seed)\n");
		return (EXIT_SUCCESS);
	}
	for (k = 1; k <= npoints; k++) {
		c = (unsigned long)k;
		if (rest(save) != 0)
			return (EXIT_FAILURE);
		rc = sim_recover_child(seed, HOME, c, NULL);
		if (rc < 0)
			return (EXIT_FAILURE);
		if (rc == 1)
			crashes++;
		if (recover_and_check(seed) != 0) {
			fprintf(stderr, "test_sim_recovery_undo_crash: FAIL -- "
			    "after an early recovery crash at op %lu the "
			    "recovered DB is WRONG (seed 0x%llx) -- committed "
			    "lost, aborted survived, or tree dirty\n", c,
			    (unsigned long long)seed);
			return (EXIT_FAILURE);
		}
		trials++;
	}

	(void)sim_fresh_home(save);
	printf("test_sim_recovery_undo_crash: PASS -- %d early-recovery "
	    "crashes (%d hit), committed present, aborted+uncommitted gone, "
	    "tree clean after re-recovery (seed 0x%llx)\n", trials, crashes,
	    (unsigned long long)seed);
	return (EXIT_SUCCESS);
}
