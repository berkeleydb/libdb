/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * FreeBSD process-shared mutex cost probe.
 *
 * Hypothesis: libdb sets PTHREAD_PROCESS_SHARED on every mutex
 * (mut_pthread.c:111/134/156). On Linux that is a userspace futex; on FreeBSD
 * it routes through _umtx_op(UMTX_OP_SHM), one syscall per operation plus an
 * shm mapping. If so, a process-shared lock/unlock pair is orders of magnitude
 * more expensive than a process-private one on this platform.
 *
 * This isolates pthreads alone -- no libdb -- so the answer cannot be confused
 * with anything in the engine.
 */
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define	N	200000

static double
bench(int shared)
{
	pthread_mutex_t m;
	pthread_mutexattr_t a;
	struct timespec t0, t1;
	int i;

	pthread_mutexattr_init(&a);
	if (shared)
		pthread_mutexattr_setpshared(&a, PTHREAD_PROCESS_SHARED);
	if (pthread_mutex_init(&m, &a) != 0) {
		fprintf(stderr, "init failed\n");
		exit(1);
	}
	clock_gettime(CLOCK_MONOTONIC, &t0);
	for (i = 0; i < N; i++) {
		pthread_mutex_lock(&m);
		pthread_mutex_unlock(&m);
	}
	clock_gettime(CLOCK_MONOTONIC, &t1);
	pthread_mutex_destroy(&m);
	pthread_mutexattr_destroy(&a);
	return ((t1.tv_sec - t0.tv_sec) * 1e9 + (t1.tv_nsec - t0.tv_nsec)) / N;
}

int
main(void)
{
	double priv, shar;

	/* Warm both paths before timing either. */
	(void)bench(0);
	(void)bench(1);

	priv = bench(0);
	shar = bench(1);

	printf("  PROCESS_PRIVATE lock+unlock: %8.1f ns\n", priv);
	printf("  PROCESS_SHARED  lock+unlock: %8.1f ns\n", shar);
	printf("  ratio shared/private       : %8.1fx\n",
	    priv > 0 ? shar / priv : 0.0);
	return (0);
}
