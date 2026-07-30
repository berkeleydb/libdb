/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_log_enospc.c --
 *	ENOSPC during the LOG WRITE (not just a data-page write) + crash +
 *	recovery.  A transactional btree commits records DB_TXN_SYNC while a
 *	seeded ENOSPC coin can fail an __os_io write on the WAL file.  The
 *	child records, per record, whether its DB_TXN_SYNC commit RETURNED
 *	SUCCESS (a durable ack) into a side file; then it crashes.  After
 *	recovery the parent asserts the WAL-durability contract:
 *
 *	  EVERY commit the engine ACKED (commit returned 0) must survive the
 *	  crash; a commit that FAILED (ENOSPC bubbled up) may legitimately be
 *	  absent.  A record the engine acked but that vanished is a silent
 *	  durability loss -- the exact failure bug 8 (LOGWRITEIGNORE) plants
 *	  by swallowing a log-write error and acking anyway.
 *
 *	Invariant (DESIGN.md catalog, WAL/ENOSPC): the WAL must never lose an
 *	acked commit even when the log device is full -- either the commit
 *	fails cleanly (caller sees the error, treats it as not durable) or it
 *	is genuinely durable.  Never "acked yet lost".
 *
 *	PLANTED BUG (DB_DST_INJECT_BUG=8, LOGWRITEIGNORE): __log_write
 *	ignores the __os_io ENOSPC error and advances w_off, so the commit
 *	is acked though its log bytes never persisted; the write-back crash
 *	drops them and this scenario's "acked => durable" invariant fires.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_log_enospc && ./test_sim_log_enospc [seed]
 */

#include "sim_scenario.h"

#define HOME    "TESTDIR_sim_logenospc"
#define DBFILE  "logenospc.db"
#define ACKFILE "TESTDIR_sim_logenospc/acked.bin"
#define NREC    200

static void
mkrec(i, kbuf, vbuf)
	int i;
	char *kbuf, *vbuf;
{
	uint64_t tok = __db_sim_rng(DB_SIM_RNG_APP);
	(void)snprintf(kbuf, 32, "le-%08d", i);
	(void)snprintf(vbuf, 32, "lv-%016llx", (unsigned long long)tok);
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
populate(seed)
	uint64_t seed;
{
	DB_ENV *env;
	DB *db;
	DB_TXN *txn;
	DBT key, data;
	char kbuf[32], vbuf[32];
	unsigned char acked[NREC];
	FILE *af;
	int i, ret;

	__db_sim_activate(seed);
	__db_sim_wb_enable(1);

	memset(acked, 0, sizeof(acked));

	if ((ret = db_env_create(&env, 0)) != 0)
		return (ret);
	if ((ret = env->open(env, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN, 0664)) != 0)
		return (ret);
	if ((ret = open_db(env, &db, 1)) != 0)
		return (ret);

	for (i = 0; i < NREC; i++) {
		/* Arm ENOSPC on the WRITE fast path only AFTER the env/db are
		 * created and a durable prefix is written, modelling a log
		 * device that fills mid-workload (so the create/meta writes
		 * are never failed and setup always reaches the crash). */
		if (i == NREC / 4)
			__db_sim_io_enospc_enable(120);
		mkrec(i, kbuf, vbuf);
		if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
			return (ret);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
		if (db->put(db, txn, &key, &data, 0) != 0) {
			/* put failed (ENOSPC on a data write): abort, not
			 * acked; the record is legitimately allowed absent. */
			(void)txn->abort(txn);
			continue;
		}
		/* DB_TXN_SYNC: a return of 0 is a DURABLE ACK -- the engine
		 * promises this record is on stable storage. */
		if (txn->commit(txn, DB_TXN_SYNC) == 0)
			acked[i] = 1;
		/* commit != 0 => not acked; record may be absent, fine. */
	}

	/* Persist the acked bitmap so the parent knows exactly which records
	 * the engine promised were durable.  Write + fsync it OUTSIDE the
	 * write-back tracking window is unnecessary: the ack file is not a
	 * tracked WAL/db file, so wb_crash never truncates it. */
	__db_sim_io_enospc_enable(0);       /* don't fail the ack-file write */
	if ((af = fopen(ACKFILE, "wb")) != NULL) {
		(void)fwrite(acked, 1, sizeof(acked), af);
		(void)fflush(af);
		(void)fsync(fileno(af));
		(void)fclose(af);
	}

	SIM_CRASH_EXIT();
	return (0);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	uint64_t seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0x105E5;
	DB_ENV *env;
	DB *db;
	DBT key, data;
	char kbuf[32], vbuf[32];
	unsigned char acked[NREC];
	FILE *af;
	int i, ret, nacked = 0, lost = 0, mismatch = 0;

	if (sim_fresh_home(HOME) != 0)
		return (EXIT_FAILURE);
	if (sim_run_crash_child(seed, populate) != 0)
		return (EXIT_FAILURE);

	/* Load the acked bitmap the child recorded. */
	memset(acked, 0, sizeof(acked));
	if ((af = fopen(ACKFILE, "rb")) == NULL) {
		fprintf(stderr, "test_sim_log_enospc: no ack file (seed "
		    "0x%llx)\n", (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	(void)fread(acked, 1, sizeof(acked), af);
	(void)fclose(af);

	if (sim_env_recover(HOME, &env) != 0)
		return (EXIT_FAILURE);
	if (open_db(env, &db, 0) != 0)
		return (EXIT_FAILURE);

	__db_sim_activate(seed);
	for (i = 0; i < NREC; i++) {
		mkrec(i, kbuf, vbuf);       /* keep the APP stream in lockstep */
		if (!acked[i])
			continue;           /* not acked: may be absent */
		nacked++;
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		if ((ret = db->get(db, NULL, &key, &data, 0)) != 0) {
			fprintf(stderr, "LOST acked record %s: %s\n", kbuf,
			    db_strerror(ret));
			lost++;
		} else if (data.size != strlen(vbuf) + 1 ||
		    memcmp(data.data, vbuf, data.size) != 0)
			mismatch++;
	}
	__db_sim_deactivate();
	(void)db->close(db, 0);
	(void)env->close(env, 0);

	printf("test_sim_log_enospc: %d acked commits, %d lost, %d mismatch "
	    "(seed 0x%llx)\n", nacked, lost, mismatch,
	    (unsigned long long)seed);

#if DB_DST_BUG(8)
	/* With LOGWRITEIGNORE the engine acks commits whose log write hit
	 * ENOSPC, so at least one acked record must be LOST after the crash.
	 * If none was lost, the bug went undetected -- fail so the sweep
	 * records a coverage hole. */
	if (lost == 0 && mismatch == 0) {
		fprintf(stderr, "test_sim_log_enospc: DST DID NOT CATCH "
		    "LOGWRITEIGNORE -- every acked commit survived despite the "
		    "swallowed log-write ENOSPC (seed 0x%llx)\n",
		    (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	printf("test_sim_log_enospc: DST CAUGHT LOGWRITEIGNORE -- %d acked "
	    "commit(s) lost after crash because a failed log write was "
	    "ignored (seed 0x%llx)\n", lost + mismatch,
	    (unsigned long long)seed);
	return (EXIT_SUCCESS);
#else
	if (lost != 0 || mismatch != 0) {
		fprintf(stderr, "test_sim_log_enospc: FAIL -- %d acked "
		    "commit(s) lost / %d mismatched after a log-ENOSPC crash "
		    "(acked-but-not-durable, seed 0x%llx)\n", lost, mismatch,
		    (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	printf("test_sim_log_enospc: PASS -- all %d acked commits durable "
	    "across a log-ENOSPC crash; failed commits cleanly absent "
	    "(seed 0x%llx)\n", nacked, (unsigned long long)seed);
	return (EXIT_SUCCESS);
#endif
}
