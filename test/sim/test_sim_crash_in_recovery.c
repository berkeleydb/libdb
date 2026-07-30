/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_crash_in_recovery.c --
 *	The RECOVERY-LOOP capstone: recovery must itself be crash-safe and
 *	idempotent.  FoundationDB reboots repeatedly; a real BDB deployment
 *	can crash again WHILE recovering from the first crash.  So recovery
 *	(__db_apprec: run the log backward to undo, forward to redo, then
 *	write a recovery checkpoint) must be re-runnable and interruptible:
 *	crash -> recover-partial -> crash -> recover-partial -> ... -> full
 *	recovery must converge to the SAME correct, durable state regardless
 *	of how many partial-recovery crashes happened.
 *
 *	Mechanism (DESIGN.md catalog #14, the crash-during-recovery axis):
 *	  1. a seeded transactional workload commits N durable txns, then
 *	     crashes (write-back drop of the un-fsync'd tail);
 *	  2. we run one FULL recovery to learn how many recovery-phase I/O
 *	     ops it does (T ticks -- page writes + fsyncs through the __os_*
 *	     seam), then recover once uncrashed and fingerprint the state:
 *	     that is the reference "correct recovered state";
 *	  3. then, for a sweep of crash points c in 1..T, we RESET the env
 *	     to its post-workload-crash state, run recovery crashing at the
 *	     c-th recovery I/O op (dropping recovery's own un-fsync'd work),
 *	     then run recovery to completion, and assert the final state
 *	     fingerprint EQUALS the reference.  Recovery interrupted at ANY
 *	     point and re-run must reach the identical state -- idempotent +
 *	     convergent.
 *	  4. a stacked "reboot loop": crash the recovery at c, then crash
 *	     the NEXT recovery too, then finish -- proves convergence holds
 *	     across multiple interruptions, not just one.
 *
 *	Because the crash truncates every tracked file to its durable
 *	frontier, the crashed-recovery process loses exactly the pages +
 *	checkpoint it wrote but had not fsync'd -- precisely what a power
 *	loss mid-recovery drops.  A non-idempotent redo (double-apply) or an
 *	ack-before-durable checkpoint would make a re-run DIVERGE from the
 *	reference, which fails here.
 *
 *	Deterministic: same seed => same workload => same recovery I/O
 *	sequence => same crash point for a given c => same final state.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_crash_in_recovery && ./test_sim_crash_in_recovery [seed]
 */

/* Tiny recovery cache: force recovery to evict + write dirty pages mid-
 * pass, so the crash-point sweep covers genuine redo page writes, not
 * only the recovery checkpoint. */
#define SIM_RECOVER_CACHE (64 * 1024)

#include "sim_scenario.h"

#define HOME    "TESTDIR_sim_crash_in_recovery"
#define DBFILE  "cir.db"
#define NCOMMIT 120

static void
mkrec(i, kbuf, vbuf)
	int i;
	char *kbuf, *vbuf;
{
	int j;

	(void)snprintf(kbuf, 32, "cir-%08d", i);
	for (j = 0; j < 24; j++)
		vbuf[j] = (char)('a' + ((i * 7 + j) % 26));
	vbuf[24] = '\0';
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
	(void)db->set_pagesize(db, 512);
	if ((ret = db->open(db, NULL, DBFILE, NULL, DB_BTREE,
	    (create ? DB_CREATE : 0) | DB_AUTO_COMMIT, 0664)) != 0) {
		fprintf(stderr, "open failed: %s\n", db_strerror(ret));
		return (ret);
	}
	*dbp = db;
	return (0);
}

/*
 * The workload: commit N durable txns, then leave an uncommitted tail and
 * crash (drop the un-fsync'd bytes).  This is the "dirty env awaiting
 * recovery" the whole scenario starts from.
 */
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
	/* Uncommitted tail (must be undone by recovery), then crash. */
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

/*
 * Snapshot the post-crash on-disk state of HOME to a saved copy, and
 * restore it back.  Each crash-in-recovery trial must start from the SAME
 * dirty env (the post-workload-crash files); a partial recovery mutates
 * them, so we checkpoint the dirty state once and restore before each
 * trial.  (Plain directory copy -- these are small scratch dirs we own.)
 */
static int
snapshot_home(save)
	const char *save;
{
	char cmd[512];
	(void)snprintf(cmd, sizeof(cmd),
	    "rm -rf %s && cp -a %s %s", save, HOME, save);
	return (system(cmd));
}
static int
restore_home(save)
	const char *save;
{
	char cmd[512];
	(void)snprintf(cmd, sizeof(cmd),
	    "rm -rf %s && cp -a %s %s", HOME, save, HOME);
	return (system(cmd));
}

/* Recover to completion, walk the whole tree into an FNV-1a fingerprint. */
static int
recover_and_hash(seed, hashp)
	uint64_t seed;
	uint64_t *hashp;
{
	DB_ENV *env;
	DB *db;
	DBC *dbc;
	DBT key, data;
	uint64_t h = 1469598103934665603ull;
	int ret;
	const unsigned char *p, *e;

	/* A completion recovery: no crash armed. */
	if ((ret = db_env_create(&env, 0)) != 0)
		return (-1);
	__db_sim_activate(seed);
	__db_sim_wb_enable(1);
	ret = env->open(env, HOME, DB_CREATE | DB_INIT_LOCK | DB_INIT_LOG |
	    DB_INIT_MPOOL | DB_INIT_TXN | DB_RECOVER, 0664);
	if (ret != 0) {
		__db_sim_deactivate();
		fprintf(stderr, "completion recovery failed: %s\n",
		    db_strerror(ret));
		return (-1);
	}
	if (open_db(env, &db, 0) != 0) {
		(void)env->close(env, 0);
		__db_sim_deactivate();
		return (-1);
	}
	if ((ret = db->cursor(db, NULL, &dbc, 0)) != 0) {
		(void)db->close(db, 0);
		(void)env->close(env, 0);
		__db_sim_deactivate();
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
	__db_sim_deactivate();
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
	uint64_t seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0xC1AC5;
	const char *save = HOME ".saved";
	uint64_t ref = 0, trial = 0;
	unsigned long full_ticks = 0, c;
	int rc, crashes_seen = 0, trials = 0, converged = 0;

	if (sim_fresh_home(HOME) != 0)
		return (EXIT_FAILURE);
	if (sim_run_crash_child(seed, populate) != 0)
		return (EXIT_FAILURE);

	/* Preserve the dirty post-crash env so every trial starts identical. */
	if (snapshot_home(save) != 0) {
		fprintf(stderr, "snapshot failed\n");
		return (EXIT_FAILURE);
	}

	/*
	 * Learn how many recovery I/O ops a FULL recovery does (uncrashed),
	 * so we can sweep crash points across the whole pass.  This mutates
	 * HOME, so restore afterward.
	 */
	rc = sim_recover_child(seed, HOME, 0, &full_ticks);
	if (rc != 0) {
		fprintf(stderr, "test_sim_crash_in_recovery: probe recovery "
		    "did not complete (rc=%d, seed 0x%llx)\n", rc,
		    (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	if (restore_home(save) != 0)
		return (EXIT_FAILURE);

	/* Reference: a clean full recovery + full-state fingerprint. */
	if (recover_and_hash(seed, &ref) != 0) {
		fprintf(stderr, "test_sim_crash_in_recovery: reference "
		    "recovery failed (seed 0x%llx)\n",
		    (unsigned long long)seed);
		return (EXIT_FAILURE);
	}

	printf("test_sim_crash_in_recovery: full recovery does %lu I/O ops; "
	    "reference state %016llx (seed 0x%llx)\n", full_ticks,
	    (unsigned long long)ref, (unsigned long long)seed);

	if (full_ticks == 0) {
		/* Nothing to interrupt -- still a pass (recovery was trivial),
		 * but note it so a regression that stops doing recovery I/O
		 * during a real workload is visible. */
		printf("test_sim_crash_in_recovery: PASS -- recovery did no "
		    "interruptible I/O this seed (nothing to crash)\n");
		(void)sim_fresh_home(save);
		return (EXIT_SUCCESS);
	}

	/*
	 * Sweep crash points across the recovery pass.  For each, restore
	 * the dirty env, crash recovery at op c, then finish recovery, then
	 * fingerprint -- must equal the reference.
	 */
	for (c = 1; c <= full_ticks; c++) {
		if (restore_home(save) != 0)
			return (EXIT_FAILURE);
		rc = sim_recover_child(seed, HOME, c, NULL);
		if (rc < 0) {
			fprintf(stderr, "test_sim_crash_in_recovery: recover "
			    "child error at crash point %lu\n", c);
			return (EXIT_FAILURE);
		}
		if (rc == 1)
			crashes_seen++;
		/* Now finish recovery (to completion) and fingerprint. */
		if (recover_and_hash(seed, &trial) != 0) {
			fprintf(stderr, "test_sim_crash_in_recovery: FAIL -- "
			    "recovery after a crash at op %lu did NOT complete "
			    "(seed 0x%llx) -- recovery is not re-runnable\n",
			    c, (unsigned long long)seed);
			return (EXIT_FAILURE);
		}
		trials++;
		if (trial != ref) {
			fprintf(stderr, "test_sim_crash_in_recovery: FAIL -- "
			    "state DIVERGED after crashing recovery at op %lu: "
			    "%016llx != reference %016llx (seed 0x%llx) -- "
			    "recovery is NOT idempotent/convergent\n", c,
			    (unsigned long long)trial,
			    (unsigned long long)ref,
			    (unsigned long long)seed);
			return (EXIT_FAILURE);
		}
		converged++;
	}

	/*
	 * Stacked reboot loop: crash recovery twice in a row (at two
	 * different points), THEN finish -- convergence must survive
	 * multiple interruptions, not just one.
	 */
	if (full_ticks >= 2) {
		unsigned long c1 = 1 + (full_ticks / 3);
		unsigned long c2 = 1 + (full_ticks / 2);

		if (restore_home(save) != 0)
			return (EXIT_FAILURE);
		rc = sim_recover_child(seed, HOME, c1, NULL);
		if (rc < 0)
			return (EXIT_FAILURE);
		if (rc == 1)
			crashes_seen++;
		rc = sim_recover_child(seed, HOME, c2, NULL);
		if (rc < 0)
			return (EXIT_FAILURE);
		if (rc == 1)
			crashes_seen++;
		if (recover_and_hash(seed, &trial) != 0 || trial != ref) {
			fprintf(stderr, "test_sim_crash_in_recovery: FAIL -- "
			    "double-crash reboot loop did not converge: "
			    "%016llx vs %016llx (seed 0x%llx)\n",
			    (unsigned long long)trial,
			    (unsigned long long)ref,
			    (unsigned long long)seed);
			return (EXIT_FAILURE);
		}
		converged++;
		trials++;
	}

	(void)sim_fresh_home(save);   /* clean the saved copy */

	printf("test_sim_crash_in_recovery: PASS -- %d trials, %d recovery "
	    "crashes injected, ALL converged to the reference state %016llx "
	    "(recovery idempotent + interruptible; seed 0x%llx)\n",
	    trials, crashes_seen, (unsigned long long)ref,
	    (unsigned long long)seed);
	(void)converged;
	return (EXIT_SUCCESS);
}
