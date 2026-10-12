/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * typed_putcost -- measure UNTYPED Btree put throughput, so an A/B of a
 * default build against an --enable-typed build can price the one branch the
 * typed layer adds to the hot put path (RFC 0014 D2's revisit condition).
 *
 * The typed build adds exactly one test to __dbc_put for every put:
 *
 *	if (F2_ISSET(dbp, DB2_AM_TYPED) && flags != DB_UPDATE_SECONDARY && ...
 *
 * On an UNTYPED handle DB2_AM_TYPED is clear, so the && short-circuits on the
 * first term -- a single bit test on dbp->flags2, a field already hot in cache
 * -- and __typed_put_check is never called.  The default build does not even
 * have this test.  This driver does nothing but put into an untyped Btree, so
 * the only compiled-in difference between the two builds on this path is that
 * one branch.  If the A/B shows a difference above the noise floor, D2's
 * "revisit if it shows on the hot path" condition has triggered.
 *
 * Steady state, not a load ramp: the key range is prepopulated first (the
 * commit_bench lesson), then timed puts overwrite existing keys so no run is
 * dominated by page splits.  Single thread by default: the branch cost is a
 * per-call CPU cost, best seen without lock-contention noise on top; a thread
 * count can be given to confirm it does not interact with contention.
 *
 * usage: typed_putcost <dir> <secs> [nthreads] [nkeys]
 * Reports one RESULT line:
 *   RESULT puts=<n> secs=<s> putspersec=<r> threads=<t> nkeys=<k> typed=<0|1>
 */
#include <sys/types.h>
#include <errno.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "db.h"

static DB_ENV *dbenv;
static DB *dbp;
static volatile int stop;
static unsigned long nkeys = 1000000;

struct arg { unsigned long puts; unsigned seed; };

static double
now(void)
{
	struct timespec ts;
	clock_gettime(CLOCK_MONOTONIC, &ts);
	return (ts.tv_sec + ts.tv_nsec / 1e9);
}

static void *
worker(void *vp)
{
	struct arg *a = vp;
	DBT k, d;
	unsigned long key;
	unsigned seed = a->seed;
	unsigned char kb[8], vb[64];

	memset(vb, 'v', sizeof(vb));
	memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
	k.data = kb; k.size = 8;
	d.data = vb; d.size = sizeof(vb);
	while (!stop) {
		/* overwrite an existing key (steady state, no split). */
		key = (unsigned long)rand_r(&seed) % nkeys;
		kb[0]=key>>56; kb[1]=key>>48; kb[2]=key>>40; kb[3]=key>>32;
		kb[4]=key>>24; kb[5]=key>>16; kb[6]=key>>8; kb[7]=key;
		if (dbp->put(dbp, NULL, &k, &d, 0) != 0)
			break;
		a->puts++;
	}
	return (NULL);
}

int
main(int argc, char *argv[])
{
	const char *dir = (argc > 1) ? argv[1] : ".";
	int secs = (argc > 2) ? atoi(argv[2]) : 10;
	int nthreads = (argc > 3) ? atoi(argv[3]) : 1;
	DBT k, d;
	unsigned char kb[8], vb[64];
	unsigned long i, total;
	double t0, elapsed;
	pthread_t *th;
	struct arg *args;
	int j, typed = 0;

	if (argc > 4) nkeys = strtoul(argv[4], NULL, 10);

#ifdef HAVE_TYPED
	typed = 1;
#endif

	if (db_env_create(&dbenv, 0) != 0 ||
	    dbenv->open(dbenv, dir, DB_CREATE | DB_INIT_MPOOL | DB_PRIVATE,
	    0600) != 0) {
		printf("RESULT FAIL env\n"); return (1); }
	(void)dbenv->set_cachesize(dbenv, 0, 256 * 1024 * 1024, 1);
	if (db_create(&dbp, dbenv, 0) != 0 ||
	    dbp->open(dbp, NULL, "pc.db", NULL, DB_BTREE, DB_CREATE, 0600) != 0) {
		printf("RESULT FAIL open\n"); return (1); }

	/* Prepopulate the key range so timed puts are overwrites, not inserts. */
	memset(vb, 'v', sizeof(vb));
	memset(&k, 0, sizeof(k)); memset(&d, 0, sizeof(d));
	k.data = kb; k.size = 8; d.data = vb; d.size = sizeof(vb);
	for (i = 0; i < nkeys; i++) {
		kb[0]=i>>56; kb[1]=i>>48; kb[2]=i>>40; kb[3]=i>>32;
		kb[4]=i>>24; kb[5]=i>>16; kb[6]=i>>8; kb[7]=i;
		if (dbp->put(dbp, NULL, &k, &d, 0) != 0) {
			printf("RESULT FAIL prepop at %lu\n", i); return (1); }
	}

	th = calloc(nthreads, sizeof(*th));
	args = calloc(nthreads, sizeof(*args));
	stop = 0;
	t0 = now();
	for (j = 0; j < nthreads; j++) {
		args[j].seed = (unsigned)(j + 1);
		pthread_create(&th[j], NULL, worker, &args[j]);
	}
	/* run for secs seconds */
	while (now() - t0 < secs)
		usleep(50000);
	stop = 1;
	total = 0;
	for (j = 0; j < nthreads; j++) {
		pthread_join(th[j], NULL);
		total += args[j].puts;
	}
	elapsed = now() - t0;

	(void)dbp->close(dbp, 0);
	(void)dbenv->close(dbenv, 0);

	printf("RESULT puts=%lu secs=%.3f putspersec=%.0f threads=%d nkeys=%lu typed=%d\n",
	    total, elapsed, total / elapsed, nthreads, nkeys, typed);
	return (0);
}
