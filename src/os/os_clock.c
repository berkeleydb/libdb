/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2001, 2013 Oracle and/or its affiliates.  All rights reserved.
 *
 * $Id$
 */

#include "db_config.h"

#include "db_int.h"

#ifdef HAVE_DST
#include "sim_os.h"			/* DST clock-skew hook (--enable-dst only). */
#endif

/*
 * __os_gettime --
 *	Return the current time-of-day clock in seconds and nanoseconds.
 *
 * PUBLIC: void __os_gettime __P((ENV *, db_timespec *, int));
 */
void
__os_gettime(env, tp, monotonic)
	ENV *env;
	db_timespec *tp;
	int monotonic;
{
	const char *sc;
	int ret;

#if defined(HAVE_CLOCK_GETTIME)
#if defined(HAVE_CLOCK_MONOTONIC)
	if (monotonic)
		RETRY_CHK((clock_gettime(
		    CLOCK_MONOTONIC, (struct timespec *)tp)), ret);
	else
#endif
		RETRY_CHK((clock_gettime(
		    CLOCK_REALTIME, (struct timespec *)tp)), ret);

	RETRY_CHK((clock_gettime(CLOCK_REALTIME, (struct timespec *)tp)), ret);
	if (ret != 0) {
		sc = "clock_gettime";
		goto err;
	}
#elif defined(HAVE_GETTIMEOFDAY)
	struct timeval v;

	RETRY_CHK((gettimeofday(&v, NULL)), ret);
	if (ret != 0) {
		sc = "gettimeofday";
		goto err;
	}

	tp->tv_sec = v.tv_sec;
	tp->tv_nsec = v.tv_usec * NS_PER_US;
#elif defined(HAVE_TIME)
	time_t now;

	RETRY_CHK((time(&now) == (time_t)-1 ? 1 : 0), ret);
	if (ret != 0) {
		sc = "time";
		goto err;
	}

	tp->tv_sec = now;
	tp->tv_nsec = 0;
#else
	NO AVAILABLE CLOCK IMPLEMENTATION
#endif
#ifdef HAVE_DST
	/*
	 * DST clock-skew / time-jump fault: when a sim armed the knob, apply
	 * the seeded skew (fixed offset + jitter + occasional forward/BACKWARD
	 * jump) to the reading BDB is about to return.  This is the single
	 * seam every time-dependent decision (lock/txn timeout deadlines, the
	 * deadlock detector's expiry scan, checkpoint scheduling, replication
	 * lease/election timers) reads "now" through.  A no-op when no sim is
	 * running; compiled out entirely without --enable-dst (zero overhead).
	 */
	__db_sim_clock_hook(tp, monotonic);
#endif
	COMPQUIET(monotonic, 0);
	return;

err:	__db_syserr(env, ret, "%s", sc);
	(void)__env_panic(env, __os_posix_err(ret));
}
