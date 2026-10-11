/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 *
 * spin_lat -- throughput of ONE libdb mutex under contention, through the
 * public DB_ENV->mutex_* API, with a configurable critical section and
 * think time.  The question it answers is the shape of the spin policy:
 * what tas_spins does to acquisitions/s and to how many acquires fall
 * through to the pthread (suspend) path, at a given thread count.
 *
 *   spin_lat HOME THREADS SECONDS SPINS CS_NS THINK_NS
 *
 * SPINS=0 keeps the library default (50 x ncpu).  Prints one line:
 *   SPINLAT threads=.. spins=.. cs_ns=.. think_ns=.. acq_per_s=..
 * The spin policy itself is selected by the library's runtime switches
 * (DB_MUTEX_SPIN_LEGACY, DB_MUTEX_BACKOFF), read from the environment.
 */
#include <sys/types.h>

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include <db.h>

static DB_ENV *dbenv;
static db_mutex_t mtx;
static volatile int stop;
static unsigned long cs_ns, think_ns;
static unsigned long count[256 * 16];
static volatile unsigned long shared_word[16];

static unsigned long
now_ns(void)
{
	struct timespec t;

	clock_gettime(CLOCK_MONOTONIC, &t);
	return ((unsigned long)t.tv_sec * 1000000000UL + t.tv_nsec);
}

static void
busy(unsigned long ns)
{
	unsigned long end;

	if (ns == 0)
		return;
	end = now_ns() + ns;
	while (now_ns() < end)
		;
}

static void *
worker(void *arg)
{
	long id = (long)arg;
	unsigned long n = 0;

	while (!stop) {
		(void)dbenv->mutex_lock(dbenv, mtx);
		shared_word[0]++;	/* the critical section touches a shared line */
		busy(cs_ns);
		(void)dbenv->mutex_unlock(dbenv, mtx);
		n++;
		busy(think_ns);
	}
	count[id * 16] = n;
	return (NULL);
}

int
main(int argc, char *argv[])
{
	DB_MUTEX_STAT *sp;
	pthread_t tid[256];
	unsigned long total;
	int i, nthreads, secs, spins, ret;

	if (argc != 7) {
		fprintf(stderr,
		    "usage: %s HOME THREADS SECONDS SPINS CS_NS THINK_NS\n",
		    argv[0]);
		return (2);
	}
	nthreads = atoi(argv[2]);
	secs = atoi(argv[3]);
	spins = atoi(argv[4]);
	cs_ns = strtoul(argv[5], NULL, 10);
	think_ns = strtoul(argv[6], NULL, 10);
	if (nthreads < 1 || nthreads > 256)
		return (2);

	if ((ret = db_env_create(&dbenv, 0)) != 0 ||
	    (spins > 0 && (ret = dbenv->mutex_set_tas_spins(dbenv,
	    (u_int32_t)spins)) != 0) ||
	    (ret = dbenv->open(dbenv, argv[1],
	    DB_CREATE | DB_INIT_MPOOL | DB_THREAD, 0644)) != 0 ||
	    (ret = dbenv->mutex_alloc(dbenv, 0, &mtx)) != 0) {
		fprintf(stderr, "setup: %s\n", db_strerror(ret));
		return (1);
	}

	for (i = 0; i < nthreads; i++)
		pthread_create(&tid[i], NULL, worker, (void *)(long)i);
	sleep((unsigned)secs);
	stop = 1;
	for (i = 0; i < nthreads; i++)
		pthread_join(tid[i], NULL);
	for (total = 0, i = 0; i < nthreads; i++)
		total += count[i * 16];

	(void)dbenv->mutex_stat(dbenv, &sp, 0);
	printf("SPINLAT threads=%d spins=%u cs_ns=%lu think_ns=%lu "
	    "acq_per_s=%.0f\n", nthreads, (unsigned)sp->st_mutex_tas_spins,
	    cs_ns, think_ns, (double)total / secs);
	free(sp);
	(void)dbenv->mutex_free(dbenv, mtx);
	(void)dbenv->close(dbenv, 0);
	return (0);
}
