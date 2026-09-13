/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_aio_ckp_enospc.c --
 *	Async-writeback (DB_MPOOL_AIO) checkpoint under disk-full.  This is
 *	the AIO analogue of test_sim_ckp_enospc, and its purpose is narrow
 *	and diagnostic: prove that when a data-page write FAILS (ENOSPC)
 *	during a checkpoint, the async writeback path reports the failure to
 *	the caller EXACTLY as the synchronous path does.
 *
 *	The synchronous path (test_sim_ckp_enospc) makes txn_checkpoint
 *	return an error when a page write fails; the checkpoint record is
 *	then NOT written, so recovery never trusts a false durable frontier.
 *
 *	The async path routes the same write through os_aio; the DST ENOSPC
 *	fault fires inside __os_io, which the threadpool and synchronous-
 *	fallback backends both call (io_uring uses a raw io_uring_prep_write
 *	and so bypasses the __os_io fault hook -- see the audit report).  A
 *	backend that reports the error to its completion callback is only
 *	half the story: __memp_aio_drain must then surface it to
 *	__memp_sync_int -> txn_checkpoint.
 *
 *	Invariant (the bug this test pins): with DB_MPOOL_AIO enabled and a
 *	checkpoint page write failing under ENOSPC, txn_checkpoint MUST
 *	return non-zero, matching the synchronous path.  If it returns 0 the
 *	failed write was silently swallowed and a false checkpoint would be
 *	logged -- "a fast liar."
 *
 *	Build/run (from a build dir, after configure --enable-dst):
 *	    gmake test_sim_aio_ckp_enospc && ./test_sim_aio_ckp_enospc [seed]
 *
 *	This program does NOT crash/recover; it checks the return-code
 *	contract directly, so it is a pure single-process determinism test.
 */

#include "sim_scenario.h"

#define HOME    "TESTDIR_sim_aio_ckp_enospc"
#define DBFILE  "aioce.db"
#define NCOMMIT 400		/* enough dirty pages that a 20% ENOSPC hits */

static void
mkrec(i, kbuf, vbuf)
	int i;
	char *kbuf, *vbuf;
{
	uint64_t tok = __db_sim_rng(DB_SIM_RNG_APP);
	(void)snprintf(kbuf, 32, "ae-%08d", i);
	(void)snprintf(vbuf, 32, "av-%016llx", (unsigned long long)tok);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	uint64_t seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0xA10E;
	DB_ENV *env;
	DB *db;
	DB_TXN *txn;
	DBT key, data;
	char kbuf[32], vbuf[32];
	int i, ret, ckp_ret;
	unsigned long fired;

	if (sim_fresh_home(HOME) != 0)
		return (EXIT_FAILURE);

	__db_sim_activate(seed);
	__db_sim_wb_enable(1);

	if ((ret = db_env_create(&env, 0)) != 0) {
		fprintf(stderr, "db_env_create: %s\n", db_strerror(ret));
		return (EXIT_FAILURE);
	}
	/* Turn ON async buffer-pool writeback (the whole point). */
	if ((ret = env->set_flags(env, DB_MPOOL_AIO, 1)) != 0) {
		fprintf(stderr, "set_flags(DB_MPOOL_AIO): %s\n",
		    db_strerror(ret));
		return (EXIT_FAILURE);
	}
	/* Small cache so the workload dirties many pages that must be
	 * flushed by the checkpoint through the async window. */
	(void)env->set_cachesize(env, 0, 512 * 1024, 1);
	if ((ret = env->open(env, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN, 0664)) != 0) {
		fprintf(stderr, "env->open: %s\n", db_strerror(ret));
		return (EXIT_FAILURE);
	}
	if ((ret = db_create(&db, env, 0)) != 0 ||
	    (ret = db->open(db, NULL, DBFILE, NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT, 0664)) != 0) {
		fprintf(stderr, "db open: %s\n", db_strerror(ret));
		return (EXIT_FAILURE);
	}

	/* Dirty a lot of pages (durability of the data itself is not the
	 * point here; the return-code contract is). */
	for (i = 0; i < NCOMMIT; i++) {
		mkrec(i, kbuf, vbuf);
		if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
			return (EXIT_FAILURE);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
		if ((ret = db->put(db, txn, &key, &data, 0)) != 0)
			return (EXIT_FAILURE);
		if ((ret = txn->commit(txn, 0)) != 0)
			return (EXIT_FAILURE);
	}

	/* Arm ENOSPC (20% of writes fail), then force a checkpoint whose
	 * page flush goes through the async window and hits ENOSPC. */
	__db_sim_fault_count_reset();
	__db_sim_io_enospc_enable(200);
	ckp_ret = env->txn_checkpoint(env, 0, 0, DB_FORCE);
	__db_sim_io_enospc_enable(0);
	fired = __db_sim_fault_count(DB_SIM_FC_ENOSPC);

	__db_sim_deactivate();
	(void)db->close(db, 0);
	(void)env->close(env, 0);

	/*
	 * The DST ENOSPC fault fires inside __os_io.  The threadpool and
	 * synchronous-fallback backends write via __os_io, so the fault
	 * reaches the async write; the io_uring backend issues a raw
	 * io_uring_prep_write on the fd and BYPASSES __os_io, so no fault
	 * fires and this scenario cannot exercise its error path (the uring
	 * error path is covered separately by the real-full-filesystem probe
	 * in the audit).  If no fault fired, report inconclusive rather than
	 * a false failure.
	 */
	if (fired == 0) {
		printf("test_sim_aio_ckp_enospc: SKIP -- the active backend "
		    "bypasses the __os_io fault hook (e.g. io_uring); no write "
		    "fault was injected, cannot exercise the swallow path here "
		    "(seed 0x%llx)\n", (unsigned long long)seed);
		return (EXIT_SUCCESS);
	}

	/*
	 * The contract: a checkpoint whose data-page write failed under
	 * ENOSPC must NOT report success.  The synchronous path returns the
	 * error; the async path must match it.
	 */
	if (ckp_ret == 0) {
		fprintf(stderr, "test_sim_aio_ckp_enospc: FAIL -- "
		    "txn_checkpoint returned SUCCESS while an async data-page "
		    "write failed under ENOSPC (%lu faults fired; error "
		    "swallowed; a false checkpoint would be logged) "
		    "(seed 0x%llx)\n", fired, (unsigned long long)seed);
		return (EXIT_FAILURE);
	}
	printf("test_sim_aio_ckp_enospc: PASS -- async checkpoint surfaced "
	    "the ENOSPC page-write failure (%lu faults fired, ret %d: %s) "
	    "(seed 0x%llx)\n", fired, ckp_ret, db_strerror(ckp_ret),
	    (unsigned long long)seed);
	return (EXIT_SUCCESS);
}
