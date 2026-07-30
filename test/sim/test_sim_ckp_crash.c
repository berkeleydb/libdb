/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_ckp_crash.c --
 *	Checkpoint / page-flush durability scenario.  A non-transactional
 *	btree (data reaches disk ONLY via a buffer-pool flush, not a
 *	redoable log) is populated, flushed to disk with db->sync (the
 *	checkpoint analogue: __memp_pgwrite lands every dirty page), and the
 *	process crashes abruptly.  After reopening, every synced record MUST
 *	be present -- the flush claimed they were durable.
 *
 *	This is the scenario the LOSTUPDATE planted bug (DB_DST_INJECT_BUG=3)
 *	targets: __memp_pgwrite skips the page write but reports success, so
 *	the flush believes the page is durable when it never reached disk.
 *	With no log to redo it, the record is simply gone after the crash --
 *	and this test's "every synced record survives" invariant fires.
 *
 *	A crash here is an abrupt _exit after the flush with NO clean close
 *	(no second flush of anything the bug dropped), modelling kill -9.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_ckp_crash && ./test_sim_ckp_crash [seed]
 */

#include "sim_scenario.h"

#define HOME    "TESTDIR_sim_ckp"
#define DBFILE  "ckp.db"
#define NREC    200
#define PGSIZE  512

static void
mkrec(i, kbuf, vbuf)
	int i;
	char *kbuf, *vbuf;
{
	uint64_t tok = __db_sim_rng(DB_SIM_RNG_APP);
	(void)snprintf(kbuf, 32, "ck-%08d", i);
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
	(void)db->set_pagesize(db, PGSIZE);
	if ((ret = db->open(db, NULL, DBFILE, NULL, DB_BTREE,
	    create ? DB_CREATE : 0, 0664)) != 0) {
		fprintf(stderr, "ckp db open failed: %s\n", db_strerror(ret));
		return (ret);
	}
	*dbp = db;
	return (0);
}

/* Populate a NON-txn btree, flush every dirty page to disk, then crash
 * abruptly (no clean close).  A correct flush made every record durable
 * in the data file. */
static int
populate(seed)
	uint64_t seed;
{
	DB_ENV *env;
	DB *db;
	DBT key, data;
	char kbuf[32], vbuf[32];
	int i, ret;

	__db_sim_activate(seed);
	/* Arm the write-back model so a WRITTEN-but-not-fsync'd page (the
	 * SYNCSKIP bug, #7) is dropped at the crash boundary -- otherwise the
	 * page reached the file via pwrite and a skipped fsync is invisible.
	 * In a correct build db->sync fsyncs, the frontier advances, and
	 * wb_crash truncates nothing. */
	__db_sim_wb_enable(1);

	if ((ret = db_env_create(&env, 0)) != 0)
		return (ret);
	/* No txn/log subsystem: durability is ONLY via the page flush.
	 * DB_PRIVATE: a process-local cache that dies with the crash, so
	 * the parent must read pages from the DATA FILE, not a surviving
	 * shared mpool region -- that is what makes a skipped page write
	 * (LOSTUPDATE) observable. */
	if ((ret = env->open(env, HOME,
	    DB_CREATE | DB_INIT_MPOOL | DB_PRIVATE, 0664)) != 0)
		return (ret);
	if ((ret = open_db(env, &db, 1)) != 0)
		return (ret);

	for (i = 0; i < NREC; i++) {
		mkrec(i, kbuf, vbuf);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
		if ((ret = db->put(db, NULL, &key, &data, 0)) != 0)
			return (ret);
	}

	/* The checkpoint analogue: flush every dirty page to the data file.
	 * A correct __memp_pgwrite lands them all AND a correct sync fsyncs. */
	if ((ret = db->sync(db, 0)) != 0)
		return (ret);

	/* CRASH abruptly -- drop every byte written but not fsync'd (a power
	 * loss), then _exit with no clean close (which would flush again). */
	__db_sim_wb_crash();
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
	uint64_t seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0xC4E;
	DB_ENV *env;
	DB *db;
	DBT key, data;
	char kbuf[32], vbuf[32];
	int i, ret, missing = 0, mismatch = 0;

	if (sim_fresh_home(HOME) != 0)
		return (EXIT_FAILURE);
	if (sim_run_crash_child(seed, populate) != 0)
		return (EXIT_FAILURE);

	/* Reopen the (non-txn) env and DB; the flushed data must be there.
	 * No recovery: there is no log -- the data file is the truth. */
	if ((ret = db_env_create(&env, 0)) != 0)
		return (EXIT_FAILURE);
	if ((ret = env->open(env, HOME,
	    DB_CREATE | DB_INIT_MPOOL | DB_PRIVATE, 0664)) != 0) {
		fprintf(stderr, "reopen env failed: %s\n", db_strerror(ret));
		return (EXIT_FAILURE);
	}
	if (open_db(env, &db, 0) != 0)
		return (EXIT_FAILURE);

	__db_sim_activate(seed);
	for (i = 0; i < NREC; i++) {
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
	__db_sim_deactivate();
	(void)db->close(db, 0);
	(void)env->close(env, 0);

	if (missing || mismatch) {
		fprintf(stderr, "test_sim_ckp_crash: FAIL -- %d missing, %d "
		    "mismatched synced records after crash (seed 0x%llx)\n",
		    missing, mismatch, (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	printf("test_sim_ckp_crash: PASS -- all %d flushed records durable "
	    "across the crash (seed 0x%llx)\n", NREC,
	    (unsigned long long)seed);
	return (EXIT_SUCCESS);
}
