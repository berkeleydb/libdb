/*
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 2013 Oracle and/or its affiliates.  All rights reserved.
 *
 * $Id$
 */

#ifndef _DB_MUTEX_H_
#define	_DB_MUTEX_H_

#ifdef HAVE_MUTEX_SUPPORT
/* The inlined trylock calls need access to the details of mutexes. */
#define	LOAD_ACTUAL_MUTEX_CODE
#include "dbinc/mutex_int.h"

#ifndef HAVE_SHARED_LATCHES
 #error "Shared latches are required in DB 4.8 and above"
#endif
#endif

#if defined(__cplusplus)
extern "C" {
#endif

/*
 * By default, spin 50 times per processor if fail to acquire a test-and-set
 * mutex, we have anecdotal evidence it's a reasonable value.
 */
#define	MUTEX_SPINS_PER_PROCESSOR	50

/*
 * Mutexes are represented by unsigned, 32-bit integral values.  As the
 * OOB value is 0, mutexes can be initialized by zero-ing out the memory
 * in which they reside.
 */
#define	MUTEX_INVALID	0

/*
 * We track mutex allocations by ID.
 */
#define	MTX_APPLICATION		 1
#define	MTX_ATOMIC_EMULATION	 2
#define	MTX_DB_HANDLE		 3
#define	MTX_ENV_DBLIST		 4
#define	MTX_ENV_EXCLDBLIST	 5
#define	MTX_ENV_HANDLE		 6
#define	MTX_ENV_REGION		 7
#define	MTX_LOCK_REGION		 8
#define	MTX_LOGICAL_LOCK	 9
#define	MTX_LOG_FILENAME	10
#define	MTX_LOG_FLUSH		11
#define	MTX_LOG_HANDLE		12
#define	MTX_LOG_REGION		13
#define	MTX_MPOOLFILE_HANDLE	14
#define	MTX_MPOOL_BH		15
#define	MTX_MPOOL_FH		16
#define	MTX_MPOOL_FILE_BUCKET	17
#define	MTX_MPOOL_HANDLE	18
#define	MTX_MPOOL_HASH_BUCKET	19
#define	MTX_MPOOL_REGION	20
#define	MTX_MUTEX_REGION	21
#define	MTX_MUTEX_TEST		22
#define	MTX_REP_CHKPT		23
#define	MTX_REP_DATABASE	24
#define	MTX_REP_DIAG		25
#define	MTX_REP_EVENT		26
#define	MTX_REP_REGION		27
#define	MTX_REP_START		28
#define	MTX_REP_WAITER		29
#define	MTX_REPMGR		30
#define	MTX_SEQUENCE		31
#define	MTX_TWISTER		32
#define	MTX_TCL_EVENTS		33
#define	MTX_TXN_ACTIVE		34
#define	MTX_TXN_CHKPT		35
#define	MTX_TXN_COMMIT		36
#define	MTX_TXN_MVCC		37
#define	MTX_TXN_REGION		38
/*
 * Appended, not inserted in alphabetical order: these IDs are the index
 * space of the mutex-region allocation statistics, so renumbering the
 * existing entries would silently relabel every historical stat dump.
 */
#define	MTX_MPOOL_AIO		39

#define	MTX_MAX_ENTRY		39

/* The following macros are defined on some platforms, e.g. QNX. */
#undef __mutex_init
#undef __mutex_lock
#undef __mutex_timedlock
#undef __mutex_unlock
#undef __mutex_destroy
#undef __mutex_trylock

/* Redirect mutex calls to the correct functions. */
#if !defined(HAVE_MUTEX_HYBRID) && (					\
    defined(HAVE_MUTEX_PTHREADS) ||					\
    defined(HAVE_MUTEX_SOLARIS_LWP) ||					\
    defined(HAVE_MUTEX_UI_THREADS))
#define	__mutex_init(a, b, c)		__db_pthread_mutex_init(a, b, c)
#define	__mutex_lock(a, b)		__db_pthread_mutex_lock(a, b, 0)
#define	__mutex_timedlock(a, b, c)	__db_pthread_mutex_lock(a, b, c)
#define	__mutex_unlock(a, b)		__db_pthread_mutex_unlock(a, b)
#define	__mutex_destroy(a, b)		__db_pthread_mutex_destroy(a, b)
#define	__mutex_trylock(a, b)		__db_pthread_mutex_trylock(a, b)
/*
 * These trylock versions do not support DB_ENV_FAILCHK. Callers which loop
 * checking mutexes which are held by dead processes or threads might spin.
 * These have ANSI-style definitions because this file can be included by
 * C++ files, and extern "C" affects linkage only, not argument typing.
 */
static inline int __db_pthread_mutex_trylock(ENV *env, db_mutex_t mutex)
{
	int ret;
	DB_MUTEX *mutexp;
	if (!MUTEX_ON(env) || F_ISSET(env->dbenv, DB_ENV_NOLOCKING))
		return (0);
	mutexp = MUTEXP_SET(env, mutex);
#ifdef HAVE_SHARED_LATCHES
	if (F_ISSET(mutexp, DB_MUTEX_SHARED))
		ret = pthread_rwlock_trywrlock(&mutexp->u.rwlock);
	else
#endif
		ret = pthread_mutex_trylock(&mutexp->u.m.mutex);
	if (ret == EBUSY)
		ret = DB_LOCK_NOTGRANTED;
	else if (ret == 0) {
		F_SET(mutexp, DB_MUTEX_LOCKED);
		env->dbenv->thread_id(env->dbenv, &mutexp->pid, &mutexp->tid);
		STAT_INC(env,
		    mutex, set_nowait, mutexp->mutex_set_nowait, mutex);
	}
	return (ret);
}
#ifdef HAVE_SHARED_LATCHES
#define	__mutex_rdlock(a, b)		__db_pthread_mutex_readlock(a, b)
#define	__mutex_tryrdlock(a, b)		__db_pthread_mutex_tryreadlock(a, b)
static inline int __db_pthread_mutex_tryreadlock(ENV *env, db_mutex_t mutex)
{
	int ret;
	DB_MUTEX *mutexp;
	if (!MUTEX_ON(env) || F_ISSET(env->dbenv, DB_ENV_NOLOCKING))
		return (0);
	mutexp = MUTEXP_SET(env, mutex);
	if (F_ISSET(mutexp, DB_MUTEX_SHARED))
		ret = pthread_rwlock_tryrdlock(&mutexp->u.rwlock);
	else
		return (EINVAL);
	if (ret == EBUSY || ret == EDEADLK)
		ret = DB_LOCK_NOTGRANTED;
#ifdef HAVE_STATISTICS
	if (ret == 0)
		STAT_INC(env,
		    mutex, set_rd_nowait, mutexp->mutex_set_nowait, mutex);
#endif
	return (ret);
}
#endif
#elif defined(HAVE_MUTEX_WIN32) || defined(HAVE_MUTEX_WIN32_GCC)
#define	__mutex_init(a, b, c)		__db_win32_mutex_init(a, b, c)
#define	__mutex_lock(a, b)		__db_win32_mutex_lock(a, b, 0)
#define	__mutex_timedlock(a, b, c)	__db_win32_mutex_lock(a, b, c)
#define	__mutex_trylock(a, b)		__db_win32_mutex_trylock(a, b)
#define	__mutex_unlock(a, b)		__db_win32_mutex_unlock(a, b)
#define	__mutex_destroy(a, b)		__db_win32_mutex_destroy(a, b)
#ifdef HAVE_SHARED_LATCHES
#define	__mutex_rdlock(a, b)		__db_win32_mutex_readlock(a, b)
#define	__mutex_tryrdlock(a, b)		__db_win32_mutex_tryreadlock(a, b)
#endif
#elif defined(HAVE_MUTEX_FCNTL)
#define	__mutex_init(a, b, c)		__db_fcntl_mutex_init(a, b, c)
#define	__mutex_lock(a, b)		__db_fcntl_mutex_lock(a, b, 0)
#define	__mutex_timedlock(a, b, c)	__db_fcntl_lock(a, b, c)
#define	__mutex_trylock(a, b)		__db_fcntl_mutex_trylock(a, b)
#define	__mutex_unlock(a, b)		__db_fcntl_mutex_unlock(a, b)
#define	__mutex_destroy(a, b)		__db_fcntl_mutex_destroy(a, b)
#else
#define	__mutex_init(a, b, c)		__db_tas_mutex_init(a, b, c)
#define	__mutex_lock(a, b)		__db_tas_mutex_lock(a, b, 0)
#define	__mutex_timedlock(a, b, c)	__db_tas_mutex_lock(a, b, c)
#define	__mutex_trylock(a, b)		__db_tas_mutex_trylock(a, b)
#define	__mutex_unlock(a, b)		__db_tas_mutex_unlock(a, b)
#define	__mutex_destroy(a, b)		__db_tas_mutex_destroy(a, b)
#if defined(HAVE_SHARED_LATCHES)
#define	__mutex_rdlock(a, b)		__db_tas_mutex_readlock(a, b)
#define	__mutex_tryrdlock(a,b)		__db_tas_mutex_tryreadlock(a, b)
#endif
#endif

/*
 * When there is no method to get a shared latch, fall back to
 * implementing __mutex_rdlock() as getting an exclusive one.
 * This occurs either when !HAVE_SHARED_LATCHES or HAVE_MUTEX_FCNTL.
 */
#ifndef __mutex_rdlock
#define	__mutex_rdlock(a, b)		__mutex_lock(a, b)
#endif
#ifndef __mutex_tryrdlock
#define	__mutex_tryrdlock(a, b)		__mutex_trylock(a, b)
#endif

/*
 * DIAGNOSTIC-only lock-order checking.  Hooked here, at the __mutex_*
 * redirection layer, rather than at the MUTEX_* macros below, because this is
 * the single narrowest waist that every acquisition passes through: all five
 * MUTEX_* macros expand to these, and so do the direct __mutex_* callers in
 * mut_region.c (the region self-test) and mut_method.c (the public
 * DB_ENV->mutex_lock).  Hooking the macros instead would have missed those and
 * would have needed five hook sites instead of two.
 *
 * The order is VALIDATED BEFORE the acquisition and RECORDED AFTER it succeeds.
 * Validating first is not a detail -- it is the whole reason the checker has
 * teeth.  A self-deadlock (the same non-recursive latch twice on one thread)
 * never returns from the acquire call, so a checker that only looked after a
 * successful acquisition would hang in exactly the case it exists to diagnose.
 * Recording only on success keeps the held-set honest when a trylock fails.
 *
 * See src/dbinc/lock_order.h for the order itself.
 */
#include "dbinc/lock_order.h"

#ifdef DIAGNOSTIC
#define	DB_LO_LOCK(fn, env, mutex)					\
	(__db_lo_check(env, mutex, __FILE__, __LINE__),			\
	 __db_lo_wrap_acq(env, mutex, fn(env, mutex), __FILE__, __LINE__))
#define	DB_LO_UNLOCK(fn, env, mutex)					\
	(__db_lo_release(env, mutex), fn(env, mutex))

/*
 * An inline helper rather than a statement macro: this sits inside expressions
 * (MUTEX_TRYLOCK is used as a value), so it must be an expression itself.
 */
static inline int
__db_lo_wrap_acq(ENV *env, db_mutex_t mutex, int ret,
    const char *file, int line)
{
	if (ret == 0)
		__db_lo_acquire(env, mutex, file, line);
	return (ret);
}

#define	__mutex_lock_ck(env, m)		DB_LO_LOCK(__mutex_lock, env, m)
#define	__mutex_rdlock_ck(env, m)	DB_LO_LOCK(__mutex_rdlock, env, m)
#define	__mutex_trylock_ck(env, m)	DB_LO_LOCK(__mutex_trylock, env, m)
#define	__mutex_tryrdlock_ck(env, m)	DB_LO_LOCK(__mutex_tryrdlock, env, m)
#define	__mutex_unlock_ck(env, m)	DB_LO_UNLOCK(__mutex_unlock, env, m)
#else
#define	__mutex_lock_ck(env, m)		__mutex_lock(env, m)
#define	__mutex_rdlock_ck(env, m)	__mutex_rdlock(env, m)
#define	__mutex_trylock_ck(env, m)	__mutex_trylock(env, m)
#define	__mutex_tryrdlock_ck(env, m)	__mutex_tryrdlock(env, m)
#define	__mutex_unlock_ck(env, m)	__mutex_unlock(env, m)
#endif

/*
 * Lock/unlock a mutex.  If the mutex was never required, the thread of
 * control can proceed without it.
 *
 * We never fail to acquire or release a mutex without panicing.  Simplify
 * the macros to always return a panic value rather than saving the actual
 * return value of the mutex routine.
 */
#ifdef HAVE_MUTEX_SUPPORT
#define	MUTEX_LOCK(env, mutex) do {					\
	if ((mutex) != MUTEX_INVALID &&					\
	    __mutex_lock_ck(env, mutex) != 0)				\
		return (DB_RUNRECOVERY);				\
} while (0)

/*
 * Always check the return value of MUTEX_TRYLOCK()!  Expect 0 on success,
 * or DB_LOCK_NOTGRANTED, or possibly DB_RUNRECOVERY for failchk.
 */
#define	MUTEX_TRYLOCK(env, mutex)					\
	(((mutex) == MUTEX_INVALID) ? 0 : __mutex_trylock_ck(env, mutex))

/*
 * Acquire a DB_MUTEX_SHARED "mutex" in shared mode.
 */
#define	MUTEX_READLOCK(env, mutex) do {					\
	if ((mutex) != MUTEX_INVALID &&					\
	    __mutex_rdlock_ck(env, mutex) != 0)				\
		return (DB_RUNRECOVERY);				\
} while (0)
#define	MUTEX_TRY_READLOCK(env, mutex)					\
	((mutex) != MUTEX_INVALID ? __mutex_tryrdlock_ck(env, mutex) : 0)

#define	MUTEX_UNLOCK(env, mutex) do {					\
	if ((mutex) != MUTEX_INVALID &&					\
	    __mutex_unlock_ck(env, mutex) != 0)				\
		return (DB_RUNRECOVERY);				\
} while (0)

#define	MUTEX_WAIT(env, mutex, duration) do {			      \
	int __ret;						      \
	if ((mutex) != MUTEX_INVALID &&				      \
	    (__ret = __mutex_timedlock(env, mutex, duration)) != 0 && \
	    __ret != DB_TIMEOUT)				      \
		return (DB_RUNRECOVERY);			      \
} while (0)
#else
/*
 * There are calls to lock/unlock mutexes outside of #ifdef's -- replace
 * the call with something the compiler can discard, but which will make
 * if-then-else blocks work correctly.
 */
/*
 * Each stub must be usable both as a STATEMENT and as a VALUE.  The original
 * form was `(mutex) = (mutex)`, which the comment above explains was chosen so
 * if-then-else blocks still parse -- but it is an assignment expression, so a
 * caller that tests the result, as the os_aio cross-reap latch does with
 *
 *	MUTEX_TRYLOCK(env, dbmp->aio_ctx->mtx_aio) == 0
 *
 * fails with "lvalue required as left operand of assignment" whenever the
 * argument is not a bare lvalue.  Casting to void and yielding 0 keeps the
 * statement use working, makes the value use compile, and returns the success
 * code the trylock/readlock contract specifies (0 == acquired), which is the
 * right answer when there is no mutex to contend for.
 */
#define	MUTEX_LOCK(env, mutex)		((void)(mutex), 0)
#define	MUTEX_TRYLOCK(env, mutex)	((void)(mutex), 0)
#define	MUTEX_READLOCK(env, mutex)	((void)(mutex), 0)
#define	MUTEX_TRY_READLOCK(env, mutex)	((void)(mutex), 0)
#define	MUTEX_UNLOCK(env, mutex)	((void)(mutex), 0)
#define	MUTEX_REQUIRED(env, mutex)	(mutex) = (mutex)
#define	MUTEX_REQUIRED_READ(env, mutex)	(mutex) = (mutex)
#define	MUTEX_WAIT(env, mutex, duration) (mutex) = (mutex)
#endif

/*
 * Berkeley DB ports may require single-threading at places in the code.
 */

/*
 * Single-threading defaults to a no-op.
 */
#ifndef DB_BEGIN_SINGLE_THREAD
#define	DB_BEGIN_SINGLE_THREAD
#endif
#ifndef DB_END_SINGLE_THREAD
#define	DB_END_SINGLE_THREAD
#endif

#if defined(__cplusplus)
}
#endif

#include "dbinc_auto/mutex_ext.h"
#endif /* !_DB_MUTEX_H_ */
