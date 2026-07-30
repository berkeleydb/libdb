/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_recovery_redo_crash.c --
 *	Crash mid-REDO of a recovery pass, then recover: the re-apply must
 *	be IDEMPOTENT.  Recovery rolls the log forward re-applying committed
 *	changes to pages; if the process dies after SOME redo pages are
 *	applied but the recovery checkpoint is not yet durable, the NEXT
 *	recovery re-applies the same redo records.  A redo handler that is
 *	not idempotent (double-applies, or advances a counter twice) would
 *	corrupt; a correct one converges.
 *
 *	This scenario crashes recovery at a MID-pass I/O op (roughly the
 *	middle of the recovery I/O sequence -- the redo region), then runs
 *	recovery to completion and asserts:
 *	  - every committed txn is present with its exact value;
 *	  - the uncommitted tail (undone) is absent;
 *	  - the DB verifies clean.
 *	Repeated for a few mid-pass points; the committed set must match a
 *	seed-derived reference regardless of where the redo was interrupted.
 *
 *	Deterministic: same seed => same recovery I/O sequence => same
 *	mid-point => same outcome.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_recovery_redo_crash && ./test_sim_recovery_redo_crash [seed]
 */

/* Tiny recovery cache: force redo to evict + write dirty pages mid-pass. */
#define SIM_RECOVER_CACHE (64 * 1024)

#include "sim_scenario.h"

#define HOME    "TESTDIR_sim_rec_redo"
#define DBFILE  "recredo.db"
#define NCOMMIT 100
#define PGSIZE  512

static void
mkrec(i, kbuf, vbuf)
	int i;
	char *kbuf, *vbuf;
{
	int j;

	(void)snprintf(kbuf, 32, "rr-%08d", i);
	for (j = 0; j < 20; j++)
		vbuf[j] = (char)('A' + ((i * 3 + j) % 26));
	vbuf[20] = '\0';
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
	(void)db->set_pagesize(db, PGSIZE);
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
	/* Uncommitted tail: must be undone by recovery. */
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

/* Recover to completion, then check every committed txn present + exact,
 * the uncommitted tail absent, and the tree verifies clean. */
static int
recover_and_check(seed, seenp)
	uint64_t seed;
	int *seenp;
{
	DB_ENV *env;
	DB *db;
	DBT key, data;
	char kbuf[32], vbuf[32];
	int i, ret, present = 0, bad = 0;

	if (sim_env_recover(HOME, &env) != 0)
		return (-1);
	if (open_db(env, &db, 0) != 0) {
		(void)env->close(env, 0);
		return (-1);
	}
	for (i = 0; i < NCOMMIT; i++) {
		mkrec(i, kbuf, vbuf);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		ret = db->get(db, NULL, &key, &data, 0);
		if (ret == 0) {
			present++;
			if (data.size != strlen(vbuf) + 1 ||
			    memcmp(data.data, vbuf, data.size) != 0)
				bad++;
		}
	}
	/* Uncommitted tail must be absent. */
	mkrec(NCOMMIT, kbuf, vbuf);
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
	ret = db->get(db, NULL, &key, &data, 0);
	(void)db->close(db, 0);

	/* Verify the tree structure. */
	{
		DB *vdb;
		int vret;
		if (db_create(&vdb, env, 0) == 0) {
			vret = vdb->verify(vdb, DBFILE, NULL, NULL, 0);
			if (vret != 0)
				bad++;
			/* verify closes vdb */
		}
	}
	(void)env->close(env, 0);

	if (seenp != NULL)
		*seenp = present;
	if (bad != 0 || present != NCOMMIT || ret != DB_NOTFOUND)
		return (1);   /* invariant violation */
	return (0);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	uint64_t seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0x2ED0;
	const char *save = HOME ".saved";
	unsigned long full_ticks = 0, points[3], c;
	int rc, i, seen, trials = 0, crashes = 0;

	if (sim_fresh_home(HOME) != 0)
		return (EXIT_FAILURE);
	if (sim_run_crash_child(seed, populate) != 0)
		return (EXIT_FAILURE);
	if (snap(save) != 0)
		return (EXIT_FAILURE);

	/* Learn full recovery I/O count. */
	rc = sim_recover_child(seed, HOME, 0, &full_ticks);
	if (rc != 0) {
		fprintf(stderr, "test_sim_recovery_redo_crash: probe recovery "
		    "failed (rc=%d, seed 0x%llx)\n", rc,
		    (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	if (rest(save) != 0)
		return (EXIT_FAILURE);

	printf("test_sim_recovery_redo_crash: full recovery %lu I/O ops "
	    "(seed 0x%llx)\n", full_ticks, (unsigned long long)seed);

	if (full_ticks == 0) {
		/* No interruptible recovery I/O this seed: still assert a
		 * plain recovery is correct. */
		if (recover_and_check(seed, &seen) != 0) {
			fprintf(stderr, "test_sim_recovery_redo_crash: FAIL "
			    "-- plain recovery incorrect (seed 0x%llx)\n",
			    (unsigned long long)seed);
			return (EXIT_FAILURE);
		}
		(void)sim_fresh_home(save);
		printf("test_sim_recovery_redo_crash: PASS -- recovery correct "
		    "(no interruptible I/O this seed)\n");
		return (EXIT_SUCCESS);
	}

	/* Mid-redo crash points: 1/2, 2/3, and the last op (checkpoint). */
	points[0] = 1 + full_ticks / 2;
	points[1] = 1 + (2 * full_ticks) / 3;
	points[2] = full_ticks;

	for (i = 0; i < 3; i++) {
		c = points[i];
		if (c < 1)
			c = 1;
		if (c > full_ticks)
			c = full_ticks;
		if (rest(save) != 0)
			return (EXIT_FAILURE);
		rc = sim_recover_child(seed, HOME, c, NULL);
		if (rc < 0)
			return (EXIT_FAILURE);
		if (rc == 1)
			crashes++;
		/* Finish recovery + check invariants. */
		if (recover_and_check(seed, &seen) != 0) {
			fprintf(stderr, "test_sim_recovery_redo_crash: FAIL -- "
			    "after a crash mid-recovery at op %lu the recovered "
			    "DB is WRONG (present=%d/%d, seed 0x%llx) -- redo "
			    "not idempotent OR committed txn lost\n", c, seen,
			    NCOMMIT, (unsigned long long)seed);
			return (EXIT_FAILURE);
		}
		trials++;
	}

	(void)sim_fresh_home(save);
	printf("test_sim_recovery_redo_crash: PASS -- %d mid-recovery crashes "
	    "(%d hit the crash point), every committed txn present + exact, "
	    "uncommitted undone, tree clean after re-recovery (seed 0x%llx)\n",
	    trials, crashes, (unsigned long long)seed);
	return (EXIT_SUCCESS);
}
