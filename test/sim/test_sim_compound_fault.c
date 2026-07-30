/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_compound_fault.c --
 *	COMPOUND fault (the swarm ethos): seeded I/O latency AND ENOSPC AND
 *	torn writes all active at once across a transactional crash+recover.
 *	A DB_CHKSUM btree writes a DURABLE (fsync'd, fault-free) prefix that
 *	MUST survive, then -- with all three faults armed -- a tail of
 *	NOSYNC commits whose log/data blocks may be torn, whose writes may
 *	hit ENOSPC, all on a slow disk.  The process crashes; recovery runs.
 *
 *	Invariant (DESIGN.md catalog #14, compound-fault angle): under three
 *	simultaneous fault classes, the engine produces NO failure mode none
 *	of them produces alone -- recovery completes (or errors cleanly), the
 *	durable prefix survives intact, the tree verifies clean, and NO read
 *	ever returns silently-wrong data (a torn page is caught by the
 *	checksum, an ENOSPC write fails cleanly, latency only slows things).
 *	The torn/ENOSPC tail is the legitimate crash gray zone; we assert
 *	only that recovery is SAFE and never silently corrupt.
 *
 *	This is the highest-stress single-process scenario: it is the swarm's
 *	brutal-mix corner turned into a crash+recover durability test with a
 *	checkable durable prefix.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_compound_fault && ./test_sim_compound_fault [seed]
 */

#include "sim_scenario.h"

#define HOME    "TESTDIR_sim_compound"
#define DBFILE  "compound.db"
#define NDURABLE 40           /* fsync'd, fault-free -- MUST survive */
#define NTAIL    60           /* NOSYNC under all faults -- gray zone */
#define PGSIZE   512

static void
mkrec(i, kbuf, vbuf)
	int i;
	char *kbuf, *vbuf;
{
	uint64_t tok = __db_sim_rng(DB_SIM_RNG_APP);
	(void)snprintf(kbuf, 32, "cf-%08d", i);
	(void)snprintf(vbuf, 32, "cv-%016llx", (unsigned long long)tok);
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
	/* Latency on from the start (a slow disk, always graceful). */
	__db_sim_io_faults_enable(1000, 30000, 0);   /* 1-30us latency */

	if ((ret = db_env_create(&env, 0)) != 0)
		return (ret);
	if ((ret = env->open(env, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN, 0664)) != 0)
		return (ret);
	if ((ret = open_db(env, &db, 1)) != 0)
		return (ret);

	/* Durable prefix: fsync'd, NO torn/ENOSPC -- these must all survive
	 * (only latency is active, which cannot cost durability). */
	for (i = 0; i < NDURABLE; i++)
		if ((ret = one(env, db, i, 1)) != 0)
			return (ret);

	/* Now arm the OTHER two faults on top of latency: torn writes AND
	 * ENOSPC.  The tail commits (NOSYNC) run under all three at once. */
	__db_sim_io_corrupt_enable(150);      /* torn writes */
	__db_sim_io_enospc_enable(100);       /* disk-full coin */
	for (i = NDURABLE; i < NDURABLE + NTAIL; i++)
		(void)one(env, db, i, 0);

	/* Disarm the destructive faults so the crash truncation is clean. */
	__db_sim_io_enospc_enable(0);
	__db_sim_io_corrupt_disable();
	__db_sim_io_faults_disable();
	SIM_CRASH_EXIT();
	return (0);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	uint64_t seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0xC0FA017;
	DB_ENV *env;
	DB *db;
	DBT key, data;
	char kbuf[32], vbuf[32];
	int i, ret, missing = 0, silent_bad = 0;
	unsigned long torn, enospc, latency;

	if (sim_fresh_home(HOME) != 0)
		return (EXIT_FAILURE);
	if (sim_run_crash_child(seed, populate) != 0)
		return (EXIT_FAILURE);

	/* Recovery must be SAFE even after a compound-fault tail. */
	if ((ret = sim_env_recover(HOME, &env)) != 0) {
		fprintf(stderr, "test_sim_compound_fault: recovery did not "
		    "complete cleanly (%s) under latency+ENOSPC+torn "
		    "(seed 0x%llx)\n", db_strerror(ret),
		    (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	if (open_db(env, &db, 0) != 0)
		return (EXIT_FAILURE);

	__db_sim_activate(seed);
	/* Durable prefix: every fsync'd, fault-free commit must be present
	 * and correct (never silently wrong). */
	for (i = 0; i < NDURABLE; i++) {
		mkrec(i, kbuf, vbuf);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		ret = db->get(db, NULL, &key, &data, 0);
		if (ret != 0)
			missing++;
		else if (data.size != strlen(vbuf) + 1 ||
		    memcmp(data.data, vbuf, data.size) != 0)
			silent_bad++;
	}
	/* Tail (gray zone): whatever is present must be CORRECT (a torn page
	 * must be a clean error, never silently wrong).  Absence is fine. */
	for (i = NDURABLE; i < NDURABLE + NTAIL; i++) {
		mkrec(i, kbuf, vbuf);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		if (db->get(db, NULL, &key, &data, 0) == 0 &&
		    (data.size != strlen(vbuf) + 1 ||
		    memcmp(data.data, vbuf, data.size) != 0))
			silent_bad++;
	}
	torn = __db_sim_fault_count(DB_SIM_FC_TORN);
	enospc = __db_sim_fault_count(DB_SIM_FC_ENOSPC);
	latency = __db_sim_fault_count(DB_SIM_FC_LATENCY);
	__db_sim_deactivate();
	(void)db->close(db, 0);

	/* Tree must be structurally clean after a compound-fault recovery. */
	if ((ret = db_create(&db, env, 0)) != 0)
		return (EXIT_FAILURE);
	if ((ret = db->verify(db, DBFILE, NULL, NULL, 0)) != 0) {
		fprintf(stderr, "test_sim_compound_fault: verify FAILED after "
		    "compound-fault recovery: %s (seed 0x%llx)\n",
		    db_strerror(ret), (unsigned long long)seed);
		(void)env->close(env, 0);
		return (EXIT_FAILURE);
	}
	(void)env->close(env, 0);

	printf("test_sim_compound_fault: %d durable missing, %d silent-bad; "
	    "torn=%lu enospc=%lu latency=%lu (seed 0x%llx)\n", missing,
	    silent_bad, torn, enospc, latency, (unsigned long long)seed);
	if (missing != 0 || silent_bad != 0) {
		fprintf(stderr, "test_sim_compound_fault: FAIL -- durable "
		    "prefix lost (%d) or silently-wrong data (%d) under "
		    "latency+ENOSPC+torn (seed 0x%llx)\n", missing, silent_bad,
		    (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	printf("test_sim_compound_fault: PASS -- durable prefix (%d) intact, "
	    "no silent corruption, tree clean under 3 simultaneous faults "
	    "(seed 0x%llx)\n", NDURABLE, (unsigned long long)seed);
	return (EXIT_SUCCESS);
}
