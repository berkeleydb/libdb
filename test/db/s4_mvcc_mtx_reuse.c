/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * s4_mvcc_mtx_reuse.c -- RFC 0015 S4: retain td->mvcc_mtx across MVCC writer
 * transactions instead of __mutex_alloc/_free per writer.
 *
 * Each MVCC-writer txn's first dirty fget allocates an MTX_TXN_MVCC mutex
 * (mp_fget.c:268) and __txn_end frees it (txn.c:1966), each taking the single
 * global MUTEX_SYSTEM_LOCK.  S4 retains the freed mutex in a bounded region
 * cache for the next writer -- the P13 shape for the MVCC mutex.
 *
 * The observable is MUTEX_SYSTEM_LOCK acquisitions: that latch is reported as
 * the mutex region's st_region_wait + st_region_nowait (db_stat -x).  It is
 * taken on every __mutex_alloc and __mutex_free.  Running N serial MVCC
 * writers:
 *   - reuse OFF (DB_NO_MVCC_MTX_REUSE): ~2N acquisitions for mvcc_mtx churn.
 *   - reuse ON  (default): the first writer allocates, the rest reuse the
 *     cached mutex and push it back, so mvcc_mtx contributes ~O(1).
 * So reuse-on must take materially FEWER mutex-region acquisitions than
 * reuse-off for the same workload, and the region cache (free_mvcc_n) must be
 * populated with reuse on and empty with reuse off.
 *
 * The binary is run twice by the runner, once with DB_NO_MVCC_MTX_REUSE set.
 * It prints a REUSE/NOREUSE metric line and a VERDICT; the runner compares.
 */
#include "db_config.h"

#include "db_int.h"
#include "dbinc/txn.h"

#define	HOME	"TESTDIR_s4"
#define	NWRITER	400
#define	PURGE_EVERY 16		/* checkpoint cadence to reclaim versions */

int
main(int argc, char *argv[])
{
#ifndef HAVE_STATISTICS
	COMPQUIET(argc, 0);
	COMPQUIET(argv, NULL);
	printf("VERDICT s4_mvcc_mtx_reuse SKIP built without statistics\n");
	return (0);
#else
	DB *dbp;
	DB_ENV *dbenv;
	DB_MUTEX_STAT *msp;
	DB_TXN *txn;
	DB_TXNREGION *region;
	DBT key, data;
	ENV *env;
	uintmax_t before, after, acq;
	u_int32_t i, cache_n;
	int reuse_off, ret;
	char kbuf[32], vbuf[64];
	const char *tag;

	COMPQUIET(argc, 0);
	COMPQUIET(argv, NULL);
	reuse_off = getenv("DB_NO_MVCC_MTX_REUSE") != NULL;
	tag = reuse_off ? "NOREUSE" : "REUSE";

	if ((ret = db_env_create(&dbenv, 0)) != 0 ||
	    (ret = dbenv->set_cachesize(dbenv, 0, 4 * 1024 * 1024, 1)) != 0 ||
	    (ret = dbenv->open(dbenv, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | DB_PRIVATE |
	    DB_THREAD, 0600)) != 0) {
		printf("VERDICT s4_mvcc_mtx_reuse FAIL env open: %s\n",
		    db_strerror(ret));
		return (1);
	}
	env = dbenv->env;
	region = env->tx_handle->reginfo.primary;

	if ((ret = db_create(&dbp, dbenv, 0)) != 0 ||
	    (ret = dbp->set_flags(dbp, DB_MULTIVERSION)) != 0) {
		printf("VERDICT s4_mvcc_mtx_reuse FAIL db create: %s\n",
		    db_strerror(ret));
		return (1);
	}
	/* DB_MULTIVERSION requires the open to run inside a transaction. */
	if ((ret = dbenv->txn_begin(dbenv, NULL, &txn, 0)) != 0 ||
	    (ret = dbp->open(dbp, txn, "s4.db", NULL, DB_BTREE,
	    DB_CREATE | DB_MULTIVERSION, 0600)) != 0 ||
	    (ret = txn->commit(txn, 0)) != 0) {
		printf("VERDICT s4_mvcc_mtx_reuse FAIL db open: %s\n",
		    db_strerror(ret));
		return (1);
	}

	/* Warm one writer so the pool/free-list growth is out of the window. */
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	if ((ret = dbenv->txn_begin(dbenv, NULL, &txn, 0)) != 0) {
		printf("VERDICT s4_mvcc_mtx_reuse FAIL warm begin: %s\n",
		    db_strerror(ret));
		return (1);
	}
	key.data = (char *)"warm"; key.size = 4;
	data.data = (char *)"warm"; data.size = 4;
	if ((ret = dbp->put(dbp, txn, &key, &data, 0)) != 0 ||
	    (ret = txn->commit(txn, 0)) != 0) {
		printf("VERDICT s4_mvcc_mtx_reuse FAIL warm put/commit: %s\n",
		    db_strerror(ret));
		return (1);
	}

	if ((ret = dbenv->mutex_stat(dbenv, &msp, 0)) != 0) {
		printf("VERDICT s4_mvcc_mtx_reuse FAIL mutex_stat: %s\n",
		    db_strerror(ret));
		return (1);
	}
	before = (uintmax_t)msp->st_region_wait + msp->st_region_nowait;
	free(msp);

	/* N serial MVCC writer transactions, each a dirty put. */
	for (i = 0; i < NWRITER; i++) {
		if ((ret = dbenv->txn_begin(dbenv, NULL, &txn, 0)) != 0) {
			printf("VERDICT s4_mvcc_mtx_reuse FAIL begin %u: %s\n",
			    i, db_strerror(ret));
			return (1);
		}
		snprintf(kbuf, sizeof(kbuf), "k%u", i % 50);
		snprintf(vbuf, sizeof(vbuf), "v%u", i);
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
		if ((ret = dbp->put(dbp, txn, &key, &data, 0)) != 0 ||
		    (ret = txn->commit(txn, 0)) != 0) {
			printf("VERDICT s4_mvcc_mtx_reuse FAIL put/commit %u: "
			    "%s\n", i, db_strerror(ret));
			return (1);
		}
		/*
		 * Reclaim obsolete versions periodically so the committed
		 * writers' details leave mvcc_txn and their mutexes are
		 * released (__txn_remove_buffer / purge) -- which is the churn
		 * S4 removes.  A checkpoint flushes dirty MVCC buffers and lets
		 * obsolete versions be purged; without it the details pile up
		 * on mvcc_txn and no mutex is freed until env close.
		 */
		if ((i % PURGE_EVERY) == PURGE_EVERY - 1 &&
		    (ret = dbenv->txn_checkpoint(dbenv, 0, 0, DB_FORCE)) != 0) {
			printf("VERDICT s4_mvcc_mtx_reuse FAIL checkpoint %u: "
			    "%s\n", i, db_strerror(ret));
			return (1);
		}
	}

	if ((ret = dbenv->mutex_stat(dbenv, &msp, 0)) != 0) {
		printf("VERDICT s4_mvcc_mtx_reuse FAIL mutex_stat2: %s\n",
		    db_strerror(ret));
		return (1);
	}
	after = (uintmax_t)msp->st_region_wait + msp->st_region_nowait;
	free(msp);
	acq = after - before;
	cache_n = region->free_mvcc_n;

	{
		DB_TXN_STAT *tsp;
		if (dbenv->txn_stat(dbenv, &tsp, 0) == 0) {
			printf("  %s: nsnapshot=%lu maxnsnapshot=%lu\n", tag,
			    (u_long)tsp->st_nsnapshot,
			    (u_long)tsp->st_maxnsnapshot);
			free(tsp);
		}
	}

	printf("  %s: mutex-region acquisitions over %d MVCC writers = %ju  "
	    "free_mvcc_n = %u\n", tag, NWRITER, acq, cache_n);

	if ((ret = dbp->close(dbp, 0)) != 0 ||
	    (ret = dbenv->close(dbenv, 0)) != 0) {
		printf("VERDICT s4_mvcc_mtx_reuse FAIL close: %s\n",
		    db_strerror(ret));
		return (1);
	}

	/*
	 * Single-run self-checks.  The cross-run comparison (REUSE acq <
	 * NOREUSE acq) is the runner's job; here each run asserts its own
	 * invariant so a sabotage shows up on the matching run.
	 */
	if (reuse_off) {
		if (cache_n != 0) {
			printf("VERDICT s4_mvcc_mtx_reuse FAIL (NOREUSE): cache "
			    "populated (%u) with reuse disabled\n", cache_n);
			return (1);
		}
	} else {
		if (cache_n == 0) {
			printf("VERDICT s4_mvcc_mtx_reuse FAIL (REUSE): cache "
			    "empty after %d MVCC writers -- mutex not "
			    "retained\n", NWRITER);
			return (1);
		}
	}
	printf("VERDICT s4_mvcc_mtx_reuse PASS %s acq=%ju cache=%u\n",
	    tag, acq, cache_n);
	return (0);
#endif
}
