/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * m5_fname_cache.c -- RFC 0015 M5: take dbp->mutex off the per-cursor path.
 *
 * __db_cursor_int records a logged handle in the transaction via
 * __txn_record_fname, under dbp->mutex, for EVERY cursor the transaction
 * opens on that handle.  After the first cursor the call is a no-op (the
 * transaction already recorded the handle), yet the 2nd and later cursors
 * still took dbp->mutex -- the per-handle latch every thread sharing the
 * handle contends on (T-4).  M5 does the dedup scan lock-free first and
 * skips the mutex when it hits.
 *
 * Observable: dbp->mutex acquisitions (st_region_wait + st_region_nowait on
 * that mutex) while one transaction opens NCURSOR cursors on one logged
 * handle.
 *   CACHE   (default):          ~1 acquisition (only the first cursor locks).
 *   NOCACHE (DB_NO_FNAME_CACHE): ~NCURSOR acquisitions (every cursor locks).
 * PASS requires CACHE acquisitions strictly fewer than NOCACHE's.
 *
 * The binary runs the workload once; DB_NO_FNAME_CACHE selects the arm.
 */
#include "db_config.h"

#include "db_int.h"
#include "dbinc/txn.h"

#define	HOME	"TESTDIR_m5"
#define	NCURSOR	200

int
main(int argc, char *argv[])
{
#ifndef HAVE_STATISTICS
	COMPQUIET(argc, 0);
	COMPQUIET(argv, NULL);
	printf("VERDICT m5_fname_cache SKIP built without statistics\n");
	return (0);
#else
	DB *dbp;
	DBC *dbc[NCURSOR];
	DB_ENV *dbenv;
	DB_TXN *txn;
	DBT key, data;
	ENV *env;
	uintmax_t w, nw, before, acq;
	int i, nocache, ret;
	const char *tag;

	COMPQUIET(argc, 0);
	COMPQUIET(argv, NULL);
	nocache = getenv("DB_NO_FNAME_CACHE") != NULL;
	tag = nocache ? "NOCACHE" : "CACHE";

	if ((ret = db_env_create(&dbenv, 0)) != 0 ||
	    (ret = dbenv->open(dbenv, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | DB_THREAD,
	    0600)) != 0) {
		printf("VERDICT m5_fname_cache FAIL env open: %s\n",
		    db_strerror(ret));
		return (1);
	}
	env = dbenv->env;

	if ((ret = db_create(&dbp, dbenv, 0)) != 0 ||
	    (ret = dbp->open(dbp, NULL, "m5.db", NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT, 0600)) != 0) {
		printf("VERDICT m5_fname_cache FAIL db open: %s\n",
		    db_strerror(ret));
		return (1);
	}

	/* Seed a row so cursors have something to position on. */
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = (char *)"k"; key.size = 2;
	data.data = (char *)"v"; data.size = 2;
	if ((ret = dbp->put(dbp, NULL, &key, &data, 0)) != 0) {
		printf("VERDICT m5_fname_cache FAIL put: %s\n",
		    db_strerror(ret));
		return (1);
	}

	/* dbp->mutex must be a real mutex for the measurement to mean anything. */
	if (dbp->mutex == MUTEX_INVALID) {
		printf("VERDICT m5_fname_cache SKIP dbp->mutex is INVALID "
		    "(no free-threading mutex on this handle)\n");
		return (0);
	}

	if ((ret = dbenv->txn_begin(dbenv, NULL, &txn, 0)) != 0) {
		printf("VERDICT m5_fname_cache FAIL txn_begin: %s\n",
		    db_strerror(ret));
		return (1);
	}

	__mutex_set_wait_info(env, dbp->mutex, &w, &nw);
	before = w + nw;

	/* Open NCURSOR cursors on the one logged handle in the one txn. */
	for (i = 0; i < NCURSOR; i++)
		if ((ret = dbp->cursor(dbp, txn, &dbc[i], 0)) != 0) {
			printf("VERDICT m5_fname_cache FAIL cursor %d: %s\n",
			    i, db_strerror(ret));
			return (1);
		}

	__mutex_set_wait_info(env, dbp->mutex, &w, &nw);
	acq = (w + nw) - before;

	printf("  %s: dbp->mutex acquisitions over %d cursors = %ju\n",
	    tag, NCURSOR, acq);

	for (i = 0; i < NCURSOR; i++)
		(void)dbc[i]->close(dbc[i]);
	if ((ret = txn->commit(txn, 0)) != 0 ||
	    (ret = dbp->close(dbp, 0)) != 0 ||
	    (ret = dbenv->close(dbenv, 0)) != 0) {
		printf("VERDICT m5_fname_cache FAIL close: %s\n",
		    db_strerror(ret));
		return (1);
	}

	/*
	 * Per-run self-check: NOCACHE must take about one lock per cursor
	 * (so the comparison is meaningful); CACHE must take far fewer.
	 */
	if (nocache) {
		if (acq < (uintmax_t)(NCURSOR / 2)) {
			printf("VERDICT m5_fname_cache FAIL (NOCACHE): only %ju "
			    "locks for %d cursors -- expected ~one each\n",
			    acq, NCURSOR);
			return (1);
		}
	} else {
		if (acq > (uintmax_t)(NCURSOR / 4)) {
			printf("VERDICT m5_fname_cache FAIL (CACHE): %ju locks "
			    "for %d cursors -- the mutex was not skipped\n",
			    acq, NCURSOR);
			return (1);
		}
	}
	printf("VERDICT m5_fname_cache PASS %s acq=%ju\n", tag, acq);
	return (0);
#endif
}
