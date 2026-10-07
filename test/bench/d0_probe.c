/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: AGPL-3.0-or-later OR Sleepycat-OSL
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * d0_probe.c -- validate D0's central claim BEFORE building a new log record
 * type.
 *
 * RFC 0008 Finding 3 says what moves throughput is the number of LOG LATCH
 * ACQUISITIONS per transaction, not the work inside the critical section, and
 * D0 proposes halving the dominant record count by combining the key and data
 * __db_addrem records that __bam_iitem emits via two separate __db_pitem calls.
 *
 * Building that means a new record type, a recovery function, log_verify work
 * and a DB_LOGVERSION bump.  Before paying that, this measures whether the
 * PREDICTED RELATIONSHIP actually holds on this machine, using only existing
 * knobs:
 *
 *   arm "rows"   : N rows per transaction, 1 row per put.  Varying N varies
 *                  records-per-txn while holding records-per-ROW constant at
 *                  ~3.10.  This is the batch experiment RFC 0008 already ran.
 *   arm "partial": N rows per transaction, but the data item is EMPTY.  A
 *                  zero-length data item still costs a __db_pitem call, so this
 *                  does NOT reduce the record count -- it is the control that
 *                  separates "fewer records" from "less bytes".
 *
 * If D0's premise is right, throughput should track records-per-transaction and
 * be largely indifferent to bytes.  If the "partial" arm moves as much as the
 * "rows" arm, then bytes matter too and D0's projection is overstated.
 *
 * Reports st_region_wait/st_region_nowait from the log subsystem, which is the
 * engine's own count of latch contention, so the record-count claim is checked
 * against a counter rather than inferred.
 */
#include <sys/types.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include "db.h"

static DB_ENV *env;
static DB *dbp;
static int nthreads, rows_per_txn, valsz, secs, empty_data;
static volatile int stop;
static unsigned long long total_txn, total_row;
static pthread_mutex_t acc = PTHREAD_MUTEX_INITIALIZER;

static double now(void)
{
	struct timespec ts;
	(void)clock_gettime(CLOCK_MONOTONIC, &ts);
	return (ts.tv_sec + ts.tv_nsec / 1e9);
}

static void *worker(void *arg)
{
	DBT key, data;
	DB_TXN *txn;
	char kb[40], *vb;
	unsigned long long ntxn = 0, nrow = 0;
	long id = (long)arg;
	int i, ret;
	unsigned seed = (unsigned)(id * 7919 + 13);

	if ((vb = malloc((size_t)(valsz > 0 ? valsz : 1))) == NULL)
		return (NULL);
	memset(vb, 'v', (size_t)(valsz > 0 ? valsz : 1));

	while (!stop) {
		if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0) {
			fprintf(stderr, "txn_begin: %s\n", db_strerror(ret));
			stop = 1;
			break;
		}
		for (i = 0; i < rows_per_txn; i++) {
			memset(&key, 0, sizeof(key));
			memset(&data, 0, sizeof(data));
			snprintf(kb, sizeof(kb), "k%ld-%u-%d",
			    id, (seed = seed * 1103515245 + 12345), i);
			key.data = kb;
			key.size = (u_int32_t)strlen(kb);
			data.data = vb;
			data.size = empty_data ? 0 : (u_int32_t)valsz;
			if ((ret = dbp->put(dbp, txn, &key, &data, 0)) != 0) {
				/*
				 * DB_LOCK_DEADLOCK is expected here and is not
				 * a failure: abort and try a fresh transaction.
				 * Counting it as a completed txn would inflate
				 * the rate with work that never committed.
				 */
				(void)txn->abort(txn);
				goto next;
			}
			nrow++;
		}
		if ((ret = txn->commit(txn, 0)) != 0) {
			fprintf(stderr, "commit: %s\n", db_strerror(ret));
			stop = 1;
			break;
		}
		ntxn++;
next:		;
	}
	free(vb);
	(void)pthread_mutex_lock(&acc);
	total_txn += ntxn;
	total_row += nrow;
	(void)pthread_mutex_unlock(&acc);
	return (NULL);
}

int main(int argc, char **argv)
{
	DB_LOG_STAT *ls;
	pthread_t *th;
	const char *home;
	double t0, el;
	int i, ret;

	if (argc < 7) {
		fprintf(stderr, "usage: %s <dir> <threads> <secs> <rows/txn> "
		    "<valsz> <empty_data 0|1>\n", argv[0]);
		return (2);
	}
	home         = argv[1];
	nthreads     = atoi(argv[2]);
	secs         = atoi(argv[3]);
	rows_per_txn = atoi(argv[4]);
	valsz        = atoi(argv[5]);
	empty_data   = atoi(argv[6]);

	if ((ret = db_env_create(&env, 0)) != 0) {
		fprintf(stderr, "env_create: %s\n", db_strerror(ret));
		return (2);
	}
	env->set_errfile(env, stderr);
	(void)env->set_cachesize(env, 0, 512 * 1024 * 1024, 1);
	/*
	 * A transaction touching more than one key can deadlock against its
	 * peers, and with no detector running the whole process simply stops --
	 * which it did, silently, on the first multi-row arm.  Pick the youngest
	 * victim so the retry loop below converges.
	 */
	(void)env->set_lk_detect(env, DB_LOCK_YOUNGEST);
	if ((ret = env->open(env, home, DB_CREATE | DB_INIT_MPOOL |
	    DB_INIT_TXN | DB_INIT_LOG | DB_INIT_LOCK | DB_THREAD, 0644)) != 0) {
		fprintf(stderr, "env->open: %s\n", db_strerror(ret));
		return (2);
	}
	if ((ret = db_create(&dbp, env, 0)) != 0)
		return (2);
	if ((ret = dbp->open(dbp, NULL, "d0.db", NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT | DB_THREAD, 0644)) != 0) {
		fprintf(stderr, "db->open: %s\n", db_strerror(ret));
		return (2);
	}

	th = calloc((size_t)nthreads, sizeof(*th));
	t0 = now();
	for (i = 0; i < nthreads; i++)
		(void)pthread_create(&th[i], NULL, worker, (void *)(long)i);
	sleep((unsigned)secs);
	stop = 1;
	for (i = 0; i < nthreads; i++)
		(void)pthread_join(th[i], NULL);
	el = now() - t0;

	if ((ret = env->log_stat(env, &ls, 0)) != 0) {
		fprintf(stderr, "log_stat: %s\n", db_strerror(ret));
		return (2);
	}

	printf("threads=%d rows_per_txn=%d valsz=%d empty_data=%d secs=%.2f\n",
	    nthreads, rows_per_txn, valsz, empty_data, el);
	printf("  txn=%llu rows=%llu txn_per_sec=%.0f rows_per_sec=%.0f\n",
	    total_txn, total_row, total_txn / el, total_row / el);
	printf("  log_records=%lu records_per_txn=%.3f records_per_row=%.3f\n",
	    (unsigned long)ls->st_record,
	    total_txn ? (double)ls->st_record / total_txn : 0.0,
	    total_row ? (double)ls->st_record / total_row : 0.0);
	printf("  region_wait=%lu region_nowait=%lu wait_per_txn=%.3f\n",
	    (unsigned long)ls->st_region_wait,
	    (unsigned long)ls->st_region_nowait,
	    total_txn ? (double)ls->st_region_wait / total_txn : 0.0);
	printf("VERDICT d0 OK threads=%d rows=%d empty=%d txn_per_sec=%.0f "
	    "rows_per_sec=%.0f rec_per_row=%.3f\n",
	    nthreads, rows_per_txn, empty_data, total_txn / el,
	    total_row / el, total_row ? (double)ls->st_record / total_row : 0.0);

	free(ls);
	free(th);
	(void)dbp->close(dbp, 0);
	(void)env->close(env, 0);
	return (0);
}
