/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 *
 * test_mutex_stress -- mutual exclusion and lost-wakeup check for the
 * TAS / hybrid mutex under real contention, through DB_ENV->mutex_*.
 *
 *   test_mutex_stress HOME NTHREADS SECONDS SPINS [handoff]
 *
 * Every thread repeatedly takes ONE mutex and, inside it, does a
 * non-atomic read-modify-write of a counter that lives in a separate cache
 * line, plus a check that no other thread is inside (an "occupant" word
 * written with plain stores).  If MUTEX_SET does not acquire, or
 * MUTEX_UNSET does not release, either the occupant check fires or the
 * final counter differs from the number of acquisitions.  On a weakly
 * ordered CPU a missing release barrier shows up as a lost update.
 *
 * SPINS selects the hybrid regime: a low value (1) drives almost every
 * contended acquire into __db_hybrid_mutex_suspend, which is where a missing
 * store->load fence in the sleep/wake handshake becomes a LOST WAKEUP -- a
 * thread asleep in pthread_cond_wait with the mutex free.  A watchdog
 * declares a hang if total progress stops for 10 s.
 *
 * With many threads a lost wakeup is HEALED by the next unlocker that sees
 * wait != 0, so the counter mode cannot see one (measured: removing
 * MUTEX_MEMBAR passed it 6/6).  "handoff" mode removes the rescuers: exactly
 * two threads, strictly alternating.  The owner holds the mutex for a short,
 * varying time (so the waiter is caught at every point of its spin -> wait++
 * -> re-check -> condwait sequence), unlocks, and then does NOT touch the
 * mutex again until the waiter has acquired it.  If that one unlock misses
 * the waiter, nobody else will ever wake it, and the watchdog fires.
 *
 * Prints one line:  VERDICT mutex_stress PASS|FAIL ...
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
static volatile int occupant;		/* plain stores, inside the mutex */
static char pad1[128];
static volatile unsigned long counter;	/* plain RMW, inside the mutex */
static char pad2[128];
static unsigned long acquired[256];
static volatile unsigned long progress[256 * 16];
static volatile int violation;
static volatile unsigned long round_a, round_b;	/* handoff mode */

static void
spin_ns(unsigned long ns)
{
	struct timespec t0, t;

	clock_gettime(CLOCK_MONOTONIC, &t0);
	do
		clock_gettime(CLOCK_MONOTONIC, &t);
	while ((unsigned long)((t.tv_sec - t0.tv_sec) * 1000000000L +
	    (t.tv_nsec - t0.tv_nsec)) < ns && !stop);
}

/* Handoff owner: hold, release, then wait for the waiter to get it. */
static void *
handoff_a(void *arg)
{
	unsigned long r, x = 88172645463325252UL;

	(void)arg;
	for (r = 1; !stop; r++) {
		if (dbenv->mutex_lock(dbenv, mtx) != 0) {
			violation = 2;
			break;
		}
		round_a = r;			/* waiter may now try */
		x ^= x << 13; x ^= x >> 7; x ^= x << 17;
		spin_ns(x % 20000);		/* 0..20 us */
		(void)dbenv->mutex_unlock(dbenv, mtx);
		while (round_b != r && !stop)	/* nobody else will unlock */
			;
		progress[0] = r;
	}
	acquired[0] = r;
	return (NULL);
}

static void *
handoff_b(void *arg)
{
	unsigned long r;

	(void)arg;
	for (r = 1; !stop; r++) {
		while (round_a != r && !stop)
			;
		if (stop)
			break;
		if (dbenv->mutex_lock(dbenv, mtx) != 0) {
			violation = 2;
			break;
		}
		round_b = r;
		(void)dbenv->mutex_unlock(dbenv, mtx);
		progress[16] = r;
	}
	acquired[1] = r;
	return (NULL);
}

static void *
worker(void *arg)
{
	long id = (long)arg;
	unsigned long n = 0, c;

	while (!stop) {
		if (dbenv->mutex_lock(dbenv, mtx) != 0) {
			violation = 2;
			break;
		}
		if (occupant != 0)
			violation = 1;
		occupant = (int)id + 1;
		c = counter;
		counter = c + 1;
		if (occupant != (int)id + 1)
			violation = 1;
		occupant = 0;
		if (dbenv->mutex_unlock(dbenv, mtx) != 0) {
			violation = 3;
			break;
		}
		n++;
		progress[id * 16] = n;
	}
	acquired[id] = n;
	return (NULL);
}

int
main(int argc, char *argv[])
{
	pthread_t tid[256];
	unsigned long total, last, now;
	int i, nthreads, secs, spins, ret, stalled, hung, handoff;
	time_t end;

	(void)pad1; (void)pad2;
	if (argc != 5 && argc != 6) {
		fprintf(stderr,
		    "usage: %s HOME NTHREADS SECONDS SPINS [handoff]\n", argv[0]);
		return (2);
	}
	handoff = argc == 6 && strcmp(argv[5], "handoff") == 0;
	nthreads = atoi(argv[2]);
	secs = atoi(argv[3]);
	spins = atoi(argv[4]);
	if (nthreads < 1 || nthreads > 256)
		return (2);

	if ((ret = db_env_create(&dbenv, 0)) != 0 ||
	    (spins > 0 && (ret = dbenv->mutex_set_tas_spins(dbenv,
	    (u_int32_t)spins)) != 0) ||
	    (ret = dbenv->open(dbenv, argv[1],
	    DB_CREATE | DB_INIT_MPOOL | DB_THREAD, 0644)) != 0 ||
	    (ret = dbenv->mutex_alloc(dbenv, 0, &mtx)) != 0) {
		printf("VERDICT mutex_stress FAIL setup %s\n", db_strerror(ret));
		return (1);
	}

	if (handoff) {
		nthreads = 2;
		pthread_create(&tid[0], NULL, handoff_a, NULL);
		pthread_create(&tid[1], NULL, handoff_b, NULL);
	} else
		for (i = 0; i < nthreads; i++)
			pthread_create(&tid[i], NULL, worker, (void *)(long)i);

	end = time(NULL) + secs;
	last = 0;
	stalled = hung = 0;
	while (time(NULL) < end) {
		sleep(1);
		for (now = 0, i = 0; i < nthreads; i++)
			now += progress[i * 16];
		if (now == last) {
			if (++stalled >= 10) {
				hung = 1;
				break;
			}
		} else
			stalled = 0;
		last = now;
	}
	if (hung) {
		printf("VERDICT mutex_stress FAIL hang: no progress for 10s "
		    "(mode=%s threads=%d spins=%d, total=%lu round_a=%lu "
		    "round_b=%lu) -- lost wakeup?\n", handoff ? "handoff" :
		    "counter", nthreads, spins, last, round_a, round_b);
		fflush(stdout);
		_exit(1);
	}
	stop = 1;
	for (i = 0; i < nthreads; i++)
		pthread_join(tid[i], NULL);
	if (handoff) {
		if (violation != 0) {
			printf("VERDICT mutex_stress FAIL handoff violation=%d\n",
			    violation);
			return (1);
		}
		printf("VERDICT mutex_stress PASS mode=handoff rounds=%lu "
		    "spins=%d secs=%d\n", round_b, spins, secs);
		return (0);
	}
	for (total = 0, i = 0; i < nthreads; i++)
		total += acquired[i];

	if (violation != 0 || total != counter) {
		printf("VERDICT mutex_stress FAIL violation=%d acquisitions=%lu "
		    "counter=%lu lost=%ld threads=%d spins=%d\n", violation,
		    total, counter, (long)total - (long)counter,
		    nthreads, spins);
		return (1);
	}
	printf("VERDICT mutex_stress PASS acquisitions=%lu counter=%lu "
	    "threads=%d spins=%d secs=%d\n", total, counter, nthreads, spins,
	    secs);
	(void)dbenv->mutex_free(dbenv, mtx);
	(void)dbenv->close(dbenv, 0);
	return (0);
}
