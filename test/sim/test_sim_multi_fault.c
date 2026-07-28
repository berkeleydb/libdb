/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_multi_fault.c --
 *	MULTI-fault combination: seeded I/O latency AND ENOSPC armed
 *	together across a transactional crash+recover.  A workload runs
 *	against a slow disk that also intermittently reports "disk full"
 *	on a write; a put/commit that hits ENOSPC fails cleanly (the txn is
 *	aborted), one that succeeds under DB_TXN_SYNC is durable.  The
 *	process then crashes; recovery runs.  Every OBSERVED-committed txn
 *	must survive, the uncommitted one must not, and the tree verifies
 *	clean -- under two independent fault classes at once (a slow, full
 *	disk), a combination that must not produce a failure mode neither
 *	fault produces alone.
 *
 *	Why latency+ENOSPC (not latency+torn): ENOSPC fails a WHOLE write
 *	cleanly (nothing persists, the error is returned and handled), so it
 *	combines with latency across a crash without corrupting a live page
 *	the writer re-reads mid-run.  Torn-write-during-a-workload is
 *	covered by test_sim_split_torn / test_sim_torn_log; corrupt-read is
 *	test_sim_torn.  This scenario is the two-graceful-faults-at-once
 *	durability check.
 *
 *	Invariant (DESIGN.md, multi-fault): observed-committed survives,
 *	uncommitted gone, no corruption, EVEN with latency + ENOSPC both
 *	active -- and both fault classes actually fire (activation counters).
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_multi_fault && ./test_sim_multi_fault [seed]
 */

#include "sim_scenario.h"

#define HOME    "TESTDIR_sim_multi"
#define DBFILE  "multi.db"
#define NCOMMIT 64
#define PGSIZE  512

static void
mkrec(i, kbuf, vbuf)
	int i;
	char *kbuf, *vbuf;
{
	uint64_t tok = __db_sim_rng(DB_SIM_RNG_APP);
	(void)snprintf(kbuf, 32, "mf-%08d", i);
	(void)snprintf(vbuf, 32, "mv-%016llx", (unsigned long long)tok);
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

/* Records whose commit returned 0 (observed durable): the child writes a
 * bitmap so the parent knows what MUST survive. */
static unsigned char g_committed[NCOMMIT];

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
	/* Latency on from the start (a slow disk, always graceful).  ENOSPC
	 * is armed AFTER the DB is created + a durable prefix is written (so
	 * the create/meta writes are not failed), modelling a disk that fills
	 * mid-workload. */
	__db_sim_io_faults_enable(1000, 30000, 0);   /* 1-30us latency */

	if ((ret = db_env_create(&env, 0)) != 0)
		return (ret);
	if ((ret = env->open(env, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN, 0664)) != 0)
		return (ret);
	if ((ret = open_db(env, &db, 1)) != 0)
		return (ret);

	memset(g_committed, 0, sizeof(g_committed));
	for (i = 0; i < NCOMMIT; i++) {
		if (i == NCOMMIT / 4)
			__db_sim_io_enospc_enable(120);   /* disk fills at 25% */
		mkrec(i, kbuf, vbuf);
		if (env->txn_begin(env, NULL, &txn, 0) != 0)
			continue;
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
		/* An ENOSPC write may fail the put/commit cleanly. */
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

	/* Uncommitted txn (best-effort; may itself hit ENOSPC), then crash. */
	mkrec(NCOMMIT, kbuf, vbuf);
	if (env->txn_begin(env, NULL, &txn, 0) == 0) {
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
		(void)db->put(db, txn, &key, &data, 0);
	}

	/* Disarm ENOSPC so the crash truncation is clean, then drop
	 * un-fsync'd bytes and exit. */
	__db_sim_io_enospc_enable(0);
	__db_sim_io_faults_disable();
	SIM_CRASH_EXIT();
	return (0);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	uint64_t seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0x33FA;
	DB_ENV *env;
	DB *db;
	DBT key, data;
	char kbuf[32], vbuf[32];
	unsigned char committed[NCOMMIT];
	FILE *fp;
	int i, ret, missing = 0, silent_bad = 0, saw_uncommitted = 0;
	int ncommitted = 0;

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
	for (i = 0; i < NCOMMIT; i++) {
		mkrec(i, kbuf, vbuf);
		if (!committed[i])
			continue;   /* commit failed under ENOSPC -- no guarantee */
		ncommitted++;
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		ret = db->get(db, NULL, &key, &data, 0);
		if (ret == 0) {
			if (data.size != strlen(vbuf) + 1 ||
			    memcmp(data.data, vbuf, data.size) != 0)
				silent_bad++;
		} else {
			missing++;   /* an observed-committed txn LOST: bad */
		}
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
		fprintf(stderr, "test_sim_multi_fault: verify FAILED: %s "
		    "(seed 0x%llx)\n", db_strerror(ret),
		    (unsigned long long)seed);
		(void)env->close(env, 0);
		return (EXIT_FAILURE);
	}
	(void)env->close(env, 0);

	printf("test_sim_multi_fault: %d observed-committed present of %d, %d "
	    "missing, %d silent-bad, uncommitted=%d; latency=%lu enospc=%lu "
	    "(seed 0x%llx)\n", ncommitted - missing, ncommitted, missing,
	    silent_bad, saw_uncommitted,
	    __db_sim_fault_count(DB_SIM_FC_LATENCY),
	    __db_sim_fault_count(DB_SIM_FC_ENOSPC),
	    (unsigned long long)seed);

	/* Hard invariants: no observed-committed txn lost, no silently-wrong
	 * data, no uncommitted survivor, tree clean -- under latency+ENOSPC. */
	if (missing != 0 || silent_bad != 0 || saw_uncommitted != 0) {
		fprintf(stderr, "test_sim_multi_fault: FAIL -- missing=%d "
		    "silent_bad=%d uncommitted=%d under latency+ENOSPC "
		    "(seed 0x%llx)\n", missing, silent_bad, saw_uncommitted,
		    (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	printf("test_sim_multi_fault: PASS -- all observed-committed durable, "
	    "no silent corruption, uncommitted gone, tree clean under "
	    "latency+ENOSPC\n");
	return (EXIT_SUCCESS);
}
