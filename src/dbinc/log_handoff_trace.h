/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2026 Greg Burd.  All rights reserved.
 *
 * $Id$
 */

/*
 * log_handoff_trace -- measure the group-commit handoff in __log_flush_int.
 *
 * WHY THIS EXISTS.  test/bench/WRITE-PATH-2026-09.md establishes that libdb's
 * durable-commit path batches correctly (up to 31 commits per fsync, 380
 * waiters against one __os_fsync) yet still shows p99 commit latency ~70x p50.
 * A latency spread that large with correct batching is a statement about the
 * HANDOFF -- who is woken, in what order, and how many flush rounds a given
 * committer sits through -- not about the batching.  None of those quantities
 * is observable from outside: DB_LOG_STAT counts flushes and commits, so it
 * can prove coalescing happens, but it cannot distinguish "every waiter waits
 * one round" from "most wait one round and a few wait ninety".  Those two have
 * identical flush/commit ratios and radically different tails.
 *
 * So this records, per flush round and per waiter:
 *
 *   - rounds waited: how many flush rounds completed between a waiter
 *     enqueueing and that waiter being released.  THIS IS THE FAIRNESS
 *     METRIC.  If p99 waiters cross one round like p50 waiters do, the tail
 *     is device time or queueing and starvation is disproved; if p99 waiters
 *     cross many rounds, starvation is measured rather than inferred.
 *   - waiter wall time, split into the blocked interval and the interval
 *     spent re-acquiring the region lock after being woken.  The split
 *     matters: the first is the protocol's fault, the second is lock
 *     convoying behind the same latch the leader needs.
 *   - leader hold: how long the thread that performs the fsync keeps other
 *     committers out, and how much of that is the fsync itself.  Software
 *     handoff cost is (hold - fsync), measured in the same thread on the same
 *     clock, rather than subtracted from a separate benchmark's device number
 *     -- fsync_probe appends to a growing file and libdb often syncs an
 *     already-sized one, so those two numbers are not comparable and the
 *     difference between them is not a software cost.
 *
 * DESIGN CONSTRAINTS.
 *
 * Compiled out entirely unless --enable-handoff-trace.  In a normal build
 * every macro here is an empty statement, no field is added to any shared
 * struct, and src/env/env_sig.c's region signature is unchanged -- the
 * instrumented library is deliberately NOT ABI-compatible with a stock one
 * and must not be mixed with it, which the differing signature enforces.
 *
 * Nothing here allocates, takes a lock, or does I/O on the measured path: a
 * sample is a few stores into a per-process array.  Samples live in process
 * memory, not the region, so a multi-process environment reports per-process
 * histograms; that is sufficient because commit_bench is multi-THREADED, and
 * putting them in the region would change its layout and its signature.
 *
 * Round numbering is a plain u_int64_t bumped under the region lock by the
 * thread that completes a flush, so "rounds waited" is a difference of two
 * values both read under that lock.  It is not a timestamp and cannot be
 * skewed by clock behaviour.
 */

#ifndef	_DB_LOG_HANDOFF_TRACE_H_
#define	_DB_LOG_HANDOFF_TRACE_H_

#if defined(__cplusplus)
extern "C" {
#endif

#ifdef HAVE_HANDOFF_TRACE

/*
 * Per-process sample rings.  Fixed size, no allocation; if a run overflows we
 * keep the first N and record the overflow count so a truncated histogram is
 * never mistaken for a complete one.
 */
#define	DB_HOFF_MAX	(4 * 1024 * 1024)

struct __db_hoff_stats {
	/* Per-waiter samples. */
	u_int32_t *rounds;	/* flush rounds crossed while waiting */
	u_int32_t *wait_us;	/* enqueue -> woken+region reacquired */
	u_int32_t *block_us;	/* enqueue -> woken (blocked only) */
	u_int32_t *reacq_us;	/* woken -> region lock reacquired */
	u_int32_t nwait;
	u_int32_t wait_over;

	/* Per-round samples, recorded by the leader. */
	u_int32_t *hold_us;	/* in_flush++ -> waiters woken */
	u_int32_t *fsync_us;	/* the __os_fsync call itself */
	u_int32_t *woke;	/* waiters released by this round */
	u_int32_t *baton;	/* 1 if this round handed the FLUSH baton on */
	u_int32_t nround;
	u_int32_t round_over;
};

/*
 * Declared here rather than via dist/s_include's PUBLIC: extraction: these are
 * conditional on HAVE_HANDOFF_TRACE, and the generated dbinc_auto/log_ext.h is
 * shared by every build, so an unconditional prototype there would appear in
 * stock builds for functions that do not exist in them.
 */
void __db_hoff_init __P((void));
void __db_hoff_wait __P((u_int32_t, u_int32_t, u_int32_t, u_int32_t));
void __db_hoff_round __P((u_int32_t, u_int32_t, u_int32_t, u_int32_t));
void __db_hoff_dump __P((const char *));

#define	DB_HOFF_DECL_WAIT						\
	db_timespec __hoff_t0, __hoff_t1, __hoff_t2;			\
	u_int64_t __hoff_round0 = 0;					\
	int __hoff_waited = 0

#define	DB_HOFF_DECL_ROUND						\
	db_timespec __hoff_h0, __hoff_h1, __hoff_f0, __hoff_f1;		\
	u_int32_t __hoff_woke = 0, __hoff_baton = 0;			\
	int __hoff_leader = 0

#define	DB_HOFF_US(a, b)						\
	((u_int32_t)(((b).tv_sec - (a).tv_sec) * 1000000 +		\
	    ((b).tv_nsec - (a).tv_nsec) / 1000))

/* Waiter: about to enqueue and drop the region lock. */
#define	DB_HOFF_WAIT_ENQUEUE(env, lp)					\
	do {								\
		__hoff_waited = 1;					\
		__hoff_round0 = (lp)->hoff_round;			\
		__os_gettime(env, &__hoff_t0, 1);			\
	} while (0)

/* Waiter: just returned from MUTEX_LOCK on its own mtx_txnwait. */
#define	DB_HOFF_WAIT_WOKEN(env)						\
	__os_gettime(env, &__hoff_t1, 1)

/* Waiter: has the region lock back; lp->hoff_round is stable to read. */
#define	DB_HOFF_WAIT_DONE(env, lp)					\
	do {								\
		if (__hoff_waited) {					\
			__os_gettime(env, &__hoff_t2, 1);		\
			__db_hoff_wait(				 	\
			    (u_int32_t)((lp)->hoff_round - __hoff_round0), \
			    DB_HOFF_US(__hoff_t0, __hoff_t2),		\
			    DB_HOFF_US(__hoff_t0, __hoff_t1),		\
			    DB_HOFF_US(__hoff_t1, __hoff_t2));		\
		}							\
	} while (0)

/* Leader: took in_flush, about to (maybe) release the region and fsync. */
#define	DB_HOFF_LEAD_BEGIN(env)						\
	do {								\
		__hoff_leader = 1;					\
		__os_gettime(env, &__hoff_h0, 1);			\
		__hoff_f0 = __hoff_h0;					\
		__hoff_f1 = __hoff_h0;					\
	} while (0)

#define	DB_HOFF_FSYNC_BEGIN(env)					\
	do { if (__hoff_leader) __os_gettime(env, &__hoff_f0, 1); } while (0)

#define	DB_HOFF_FSYNC_END(env)						\
	do { if (__hoff_leader) __os_gettime(env, &__hoff_f1, 1); } while (0)

/* Wake pass: one waiter released / the FLUSH baton handed on. */
#define	DB_HOFF_WOKE_ONE	(__hoff_woke++)
#define	DB_HOFF_BATON_ONE	(__hoff_baton = 1)

/*
 * Leader: finished the wake pass.  Bumping lp->hoff_round here, under the
 * region lock and AFTER the wake pass, is what makes a waiter's
 * (hoff_round - round0) equal the number of rounds that completed while it
 * was queued.
 */
#define	DB_HOFF_LEAD_END(env, lp)					\
	do {								\
		if (__hoff_leader) {					\
			__os_gettime(env, &__hoff_h1, 1);		\
			__db_hoff_round(DB_HOFF_US(__hoff_h0, __hoff_h1), \
			    DB_HOFF_US(__hoff_f0, __hoff_f1),		\
			    __hoff_woke, __hoff_baton);			\
			(lp)->hoff_round++;				\
		}							\
	} while (0)

#else /* !HAVE_HANDOFF_TRACE */

#define	DB_HOFF_DECL_WAIT		NOP_STATEMENT
#define	DB_HOFF_DECL_ROUND		NOP_STATEMENT
#define	DB_HOFF_WAIT_ENQUEUE(env, lp)	NOP_STATEMENT
#define	DB_HOFF_WAIT_WOKEN(env)		NOP_STATEMENT
#define	DB_HOFF_WAIT_DONE(env, lp)	NOP_STATEMENT
#define	DB_HOFF_LEAD_BEGIN(env)		NOP_STATEMENT
#define	DB_HOFF_FSYNC_BEGIN(env)	NOP_STATEMENT
#define	DB_HOFF_FSYNC_END(env)		NOP_STATEMENT
#define	DB_HOFF_WOKE_ONE		NOP_STATEMENT
#define	DB_HOFF_BATON_ONE		NOP_STATEMENT
#define	DB_HOFF_LEAD_END(env, lp)	NOP_STATEMENT

#endif /* HAVE_HANDOFF_TRACE */

#if defined(__cplusplus)
}
#endif
#endif /* !_DB_LOG_HANDOFF_TRACE_H_ */
