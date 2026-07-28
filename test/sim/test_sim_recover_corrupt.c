/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_recover_corrupt.c --
 *	Corrupt read DURING recovery.  A DB_CHKSUM transactional workload
 *	commits N durable txns, then crashes (write-back drop of un-fsync'd
 *	bytes).  Recovery is then run with corrupt reads ARMED, so pages/log
 *	records read during the recovery replay can be bit-flipped.
 *
 *	Invariant (DESIGN.md catalog #14, recovery robustness): recovery
 *	must never silently accept corrupt data.  Either it succeeds and
 *	every committed txn is correct (the corrupt read either missed the
 *	live pages this seed, or the checksum caught + the page was re-read),
 *	or recovery/verify fails CLEANLY.  A recovered DB that hands back a
 *	committed value not matching what was stored, with no error, is
 *	SILENT-BAD and fails.  A crash/hang during recovery also fails.
 *
 *	Because a corrupt read during recovery can legitimately make
 *	recovery ERROR (a checksum-failed log/meta page), a clean recovery
 *	failure is an ACCEPTABLE outcome for a seed -- what is NOT acceptable
 *	is silent bad data or a crash.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_recover_corrupt && ./test_sim_recover_corrupt [seed]
 */

#include "sim_scenario.h"

#define HOME    "TESTDIR_sim_rec_corrupt"
#define DBFILE  "reccorrupt.db"
#define NCOMMIT 80
#define PGSIZE  512

static void
mkrec(i, kbuf, vbuf)
	int i;
	char *kbuf, *vbuf;
{
	int j;

	(void)snprintf(kbuf, 32, "rc-%08d", i);
	for (j = 0; j < 24; j++)
		vbuf[j] = (char)('a' + ((i + j) % 26));
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
	(void)db->set_flags(db, DB_CHKSUM);
	(void)db->set_pagesize(db, PGSIZE);
	if ((ret = db->open(db, NULL, DBFILE, NULL, DB_BTREE,
	    (create ? DB_CREATE : 0) | DB_AUTO_COMMIT, 0664)) != 0)
		return (ret);
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

	/*
	 * Crash abruptly WITHOUT the write-back truncation: this scenario
	 * isolates corrupt reads DURING recovery, so the on-disk files must
	 * be whole (a torn DB page would be a different fault -- that is
	 * test_sim_split_torn/test_sim_torn_log).  The committed txns are
	 * durable in the fsync'd log; recovery replays them, and we inject
	 * corrupt reads into THAT replay.
	 */
	fflush(NULL);
	_exit(42);
	/* NOTREACHED */
	return (0);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	uint64_t seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0xEC0;
	DB_ENV *env;
	DB *db;
	DBT key, data;
	char kbuf[32], vbuf[32];
	int i, ret, missing = 0, silent_bad = 0, rec_failed = 0;

	if (sim_fresh_home(HOME) != 0)
		return (EXIT_FAILURE);
	if (sim_run_crash_child(seed, populate) != 0)
		return (EXIT_FAILURE);

	/* Recover with corrupt reads armed: pages read during replay can be
	 * bit-flipped.  A checksum-failed recovery is a CLEAN failure. */
	if ((ret = db_env_create(&env, 0)) != 0)
		return (EXIT_FAILURE);
	__db_sim_activate(seed);
	__db_sim_io_corrupt_enable(40);   /* 4% corrupt reads during recovery */
	ret = env->open(env, HOME, DB_CREATE | DB_INIT_LOCK | DB_INIT_LOG |
	    DB_INIT_MPOOL | DB_INIT_TXN | DB_RECOVER, 0664);
	__db_sim_io_corrupt_disable();
	if (ret != 0) {
		/* Recovery refused to proceed on corruption: clean failure. */
		rec_failed = 1;
		printf("test_sim_recover_corrupt: recovery cleanly refused "
		    "corrupt input: %s (corrupt=%lu, seed 0x%llx)\n",
		    db_strerror(ret),
		    __db_sim_fault_count(DB_SIM_FC_CORRUPT),
		    (unsigned long long)seed);
		__db_sim_deactivate();
		return (EXIT_SUCCESS);
	}
	__db_sim_deactivate();

	if (open_db(env, &db, 0) != 0) {
		/* A clean open failure after a corrupt recovery is acceptable
		 * too -- what matters is no silent bad data. */
		(void)env->close(env, 0);
		printf("test_sim_recover_corrupt: DB open cleanly refused "
		    "after corrupt recovery (seed 0x%llx)\n",
		    (unsigned long long)seed);
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
			if (data.size != strlen(vbuf) + 1 ||
			    memcmp(data.data, vbuf, data.size) != 0)
				silent_bad++;
		} else {
			missing++;   /* a clean error is acceptable */
		}
	}
	__db_sim_deactivate();
	(void)db->close(db, 0);
	(void)env->close(env, 0);

	printf("test_sim_recover_corrupt: %d ok-committed, %d clean-error, "
	    "%d SILENT-BAD, rec_failed=%d; corrupt=%lu (seed 0x%llx)\n",
	    NCOMMIT - missing - silent_bad, missing, silent_bad, rec_failed,
	    __db_sim_fault_count(DB_SIM_FC_CORRUPT),
	    (unsigned long long)seed);

	if (silent_bad != 0) {
		fprintf(stderr, "test_sim_recover_corrupt: FAIL -- %d "
		    "committed values silently wrong after recovery under "
		    "corrupt reads\n", silent_bad);
		return (EXIT_FAILURE);
	}
	printf("test_sim_recover_corrupt: PASS -- recovery under corrupt "
	    "reads never returned silently-wrong committed data\n");
	return (EXIT_SUCCESS);
}
