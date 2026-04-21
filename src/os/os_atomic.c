/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2009, 2013 Oracle and/or its affiliates.  All rights reserved.
 *
 * $Id$
 */

#include "db_config.h"

#include "db_int.h"
#include "dbinc/atomic.h"

/*
 * Atomic operations implementation for Berkeley DB.
 *
 * Uses a 5-tier fallback strategy to provide atomic primitives on all
 * supported platforms:
 *
 *   Tier 1: GCC/Clang __atomic_* builtins (GCC 5.1+, Clang 3.6+)
 *   Tier 2: Legacy GCC __sync_* builtins (GCC 4.1-4.9)
 *   Tier 3: Architecture-specific inline assembly
 *   Tier 4: OS-specific APIs (Windows Interlocked, Solaris atomic.h)
 *   Tier 5: Mutex-based emulation fallback
 *
 * The tier selection is done at compile time via preprocessor macros
 * defined in db_config.h and dbinc/atomic.h.
 *
 * The API is split into two families based on ENV requirements:
 *
 *   Query operations (no ENV needed):
 *     __os_atomic_init(p, val)     - initialize (single-threaded context)
 *     __os_atomic_read(p)          - atomic load
 *     __os_atomic_store(p, val)    - atomic store
 *     __os_atomic_thread_fence()   - full memory barrier
 *
 *   Modification operations (ENV required for mutex fallback):
 *     __os_atomic_inc(env, p)      - increment by 1
 *     __os_atomic_dec(env, p)      - decrement by 1
 *     __os_atomic_cas(env, p, o, n)- compare and swap
 *     __os_atomic_add(env, p, v)   - add arbitrary value
 *     __os_atomic_fetch_add(env, p, v) - fetch then add
 *     __os_atomic_exchange(env, p, v)  - atomic exchange
 *     __os_atomic_cas_64(env, p, o, n) - 64-bit CAS
 *
 * Query operations never need ENV because hardware provides atomic
 * reads/writes for aligned 32-bit values on all supported platforms.
 * Modification operations need ENV because the mutex fallback path
 * requires access to the environment's mutex region.
 */

/*
 * Tier detection macros.
 *
 * These determine which implementation to use.  The header atomic.h
 * defines HAVE_ATOMIC_SUPPORT for native operations; this file provides
 * the function bodies for all tiers including the mutex fallback.
 */

/*
 * Detect GCC/Clang __atomic_* builtins (Tier 1).
 * GCC 5.1+ and Clang 3.6+ provide __atomic_* with explicit memory orders.
 */
#if !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
    !defined(HAVE_ATOMIC_X86_GCC_ASSEMBLY) && \
    !defined(HAVE_ATOMIC_SOLARIS) && !defined(DB_WIN32)
#if defined(__GNUC__) && \
    ((__GNUC__ > 4) || (__GNUC__ == 4 && __GNUC_MINOR__ >= 7))
#define	HAVE_ATOMIC_GCC_BUILTIN	1
#elif defined(__clang__)
#if __has_builtin(__atomic_fetch_add)
#define	HAVE_ATOMIC_GCC_BUILTIN	1
#endif
#endif
#endif

/*
 * Detect legacy GCC __sync_* builtins (Tier 2).
 * GCC 4.1+ provides __sync_* with full barrier semantics.
 */
#if !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
    !defined(HAVE_ATOMIC_SYNC_BUILTIN) && \
    !defined(HAVE_ATOMIC_X86_GCC_ASSEMBLY) && \
    !defined(HAVE_ATOMIC_SOLARIS) && !defined(DB_WIN32)
#if defined(__GNUC__) && \
    ((__GNUC__ > 4) || (__GNUC__ == 4 && __GNUC_MINOR__ >= 1))
#define	HAVE_ATOMIC_SYNC_BUILTIN	1
#endif
#endif

/*
 * =====================================================================
 * Tier 1: GCC/Clang __atomic_* builtins.
 *
 * These provide the most portable and correct atomic operations with
 * explicit memory ordering.  They cover ARM64, ARM32, PowerPC, MIPS,
 * x86, x86_64, RISC-V, s390x, and any other GCC-supported architecture.
 * =====================================================================
 */
#if defined(HAVE_ATOMIC_GCC_BUILTIN) && defined(HAVE_ATOMIC_SUPPORT)

/*
 * __os_atomic_init --
 *	Initialize an atomic variable.  Not itself atomic; the caller
 *	must guarantee single-threaded access during initialization.
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_GCC_BUILTIN) && defined(HAVE_ATOMIC_SUPPORT)
 * PUBLIC: void __os_atomic_init
 * PUBLIC:     __P((db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
void
__os_atomic_init(p, val)
	db_atomic_t *p;
	atomic_value_t val;
{
	__atomic_store_n(&p->value, val, __ATOMIC_RELAXED);
}

/*
 * __os_atomic_read --
 *	Atomically load and return the current value.
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_GCC_BUILTIN) && defined(HAVE_ATOMIC_SUPPORT)
 * PUBLIC: atomic_value_t __os_atomic_read
 * PUBLIC:     __P((const db_atomic_t *));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_read(p)
	const db_atomic_t *p;
{
	return (__atomic_load_n(
	    (volatile atomic_value_t *)&p->value, __ATOMIC_SEQ_CST));
}

/*
 * __os_atomic_store --
 *	Atomically store a value.
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_GCC_BUILTIN) && defined(HAVE_ATOMIC_SUPPORT)
 * PUBLIC: void __os_atomic_store
 * PUBLIC:     __P((db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
void
__os_atomic_store(p, val)
	db_atomic_t *p;
	atomic_value_t val;
{
	__atomic_store_n(&p->value, val, __ATOMIC_SEQ_CST);
}

/*
 * __os_atomic_inc --
 *	Atomically increment by 1, return the new value.
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_GCC_BUILTIN) && defined(HAVE_ATOMIC_SUPPORT)
 * PUBLIC: atomic_value_t __os_atomic_inc
 * PUBLIC:     __P((ENV *, db_atomic_t *));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_inc(env, p)
	ENV *env;
	db_atomic_t *p;
{
	COMPQUIET(env, NULL);
	return (__atomic_add_fetch(&p->value, 1, __ATOMIC_SEQ_CST));
}

/*
 * __os_atomic_dec --
 *	Atomically decrement by 1, return the new value.
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_GCC_BUILTIN) && defined(HAVE_ATOMIC_SUPPORT)
 * PUBLIC: atomic_value_t __os_atomic_dec
 * PUBLIC:     __P((ENV *, db_atomic_t *));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_dec(env, p)
	ENV *env;
	db_atomic_t *p;
{
	COMPQUIET(env, NULL);
	return (__atomic_sub_fetch(&p->value, 1, __ATOMIC_SEQ_CST));
}

/*
 * __os_atomic_cas --
 *	Atomic compare-and-swap.  If *p equals oldval, set to newval.
 *	Returns 1 on success (swap performed), 0 on failure.
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_GCC_BUILTIN) && defined(HAVE_ATOMIC_SUPPORT)
 * PUBLIC: int __os_atomic_cas __P((ENV *,
 * PUBLIC:     db_atomic_t *, atomic_value_t, atomic_value_t));
 * PUBLIC: #endif
 */
int
__os_atomic_cas(env, p, oldval, newval)
	ENV *env;
	db_atomic_t *p;
	atomic_value_t oldval;
	atomic_value_t newval;
{
	atomic_value_t expected;

	COMPQUIET(env, NULL);
	expected = oldval;
	return (__atomic_compare_exchange_n(
	    &p->value, &expected, newval,
	    0, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST));
}

/*
 * __os_atomic_add --
 *	Atomically add a value, return the new value.
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_GCC_BUILTIN) && defined(HAVE_ATOMIC_SUPPORT)
 * PUBLIC: atomic_value_t __os_atomic_add
 * PUBLIC:     __P((ENV *, db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_add(env, p, val)
	ENV *env;
	db_atomic_t *p;
	atomic_value_t val;
{
	COMPQUIET(env, NULL);
	return (__atomic_add_fetch(&p->value, val, __ATOMIC_SEQ_CST));
}

/*
 * __os_atomic_fetch_add --
 *	Atomically add a value, return the old value (before addition).
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_GCC_BUILTIN) && defined(HAVE_ATOMIC_SUPPORT)
 * PUBLIC: atomic_value_t __os_atomic_fetch_add
 * PUBLIC:     __P((ENV *, db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_fetch_add(env, p, val)
	ENV *env;
	db_atomic_t *p;
	atomic_value_t val;
{
	COMPQUIET(env, NULL);
	return (__atomic_fetch_add(&p->value, val, __ATOMIC_SEQ_CST));
}

/*
 * __os_atomic_exchange --
 *	Atomically set a new value, return the old value.
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_GCC_BUILTIN) && defined(HAVE_ATOMIC_SUPPORT)
 * PUBLIC: atomic_value_t __os_atomic_exchange
 * PUBLIC:     __P((ENV *, db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_exchange(env, p, val)
	ENV *env;
	db_atomic_t *p;
	atomic_value_t val;
{
	COMPQUIET(env, NULL);
	return (__atomic_exchange_n(&p->value, val, __ATOMIC_SEQ_CST));
}

/*
 * __os_atomic_thread_fence --
 *	Full memory barrier (sequentially consistent fence).
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_GCC_BUILTIN) && defined(HAVE_ATOMIC_SUPPORT)
 * PUBLIC: void __os_atomic_thread_fence __P((void));
 * PUBLIC: #endif
 */
void
__os_atomic_thread_fence()
{
	__atomic_thread_fence(__ATOMIC_SEQ_CST);
}

#ifdef HAVE_64BIT_TYPES
/*
 * __os_atomic_cas_64 --
 *	64-bit atomic compare-and-swap.  Returns 1 on success, 0 on failure.
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && defined(HAVE_64BIT_TYPES)
 * PUBLIC: int __os_atomic_cas_64
 * PUBLIC:     __P((ENV *, volatile int64_t *, int64_t, int64_t));
 * PUBLIC: #endif
 */
int
__os_atomic_cas_64(env, p, oldval, newval)
	ENV *env;
	volatile int64_t *p;
	int64_t oldval;
	int64_t newval;
{
	int64_t expected;

	COMPQUIET(env, NULL);
	expected = oldval;
	return (__atomic_compare_exchange_n(
	    p, &expected, newval,
	    0, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST));
}
#endif /* HAVE_64BIT_TYPES */

#endif /* HAVE_ATOMIC_GCC_BUILTIN && HAVE_ATOMIC_SUPPORT */

/*
 * =====================================================================
 * Tier 2: Legacy GCC __sync_* builtins.
 *
 * Available since GCC 4.1.  These always provide full-barrier
 * (sequentially consistent) semantics.  Less flexible than __atomic_*
 * but covers older GCC versions on ARM, PowerPC, MIPS, x86, etc.
 * =====================================================================
 */
#if defined(HAVE_ATOMIC_SYNC_BUILTIN) && defined(HAVE_ATOMIC_SUPPORT)

/*
 * __os_atomic_init --
 *	Initialize an atomic variable.
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_SYNC_BUILTIN) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT)
 * PUBLIC: void __os_atomic_init
 * PUBLIC:     __P((db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
void
__os_atomic_init(p, val)
	db_atomic_t *p;
	atomic_value_t val;
{
	p->value = val;
	__sync_synchronize();
}

/*
 * __os_atomic_read --
 *	Atomically load and return the current value.
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_SYNC_BUILTIN) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT)
 * PUBLIC: atomic_value_t __os_atomic_read
 * PUBLIC:     __P((const db_atomic_t *));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_read(p)
	const db_atomic_t *p;
{
	/*
	 * __sync_* doesn't have a dedicated load; use add-0 for an
	 * atomic read with barrier.
	 */
	return (__sync_add_and_fetch(
	    (volatile atomic_value_t *)&p->value, 0));
}

/*
 * __os_atomic_store --
 *	Atomically store a value.
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_SYNC_BUILTIN) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT)
 * PUBLIC: void __os_atomic_store
 * PUBLIC:     __P((db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
void
__os_atomic_store(p, val)
	db_atomic_t *p;
	atomic_value_t val;
{
	/*
	 * Use a CAS loop to atomically store.  The __sync_* API does
	 * not provide a direct atomic store primitive.
	 */
	for (;;) {
		atomic_value_t cur;
		cur = p->value;
		if (__sync_bool_compare_and_swap(&p->value, cur, val))
			break;
	}
}

/*
 * __os_atomic_inc --
 *	Atomically increment by 1, return the new value.
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_SYNC_BUILTIN) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT)
 * PUBLIC: atomic_value_t __os_atomic_inc
 * PUBLIC:     __P((ENV *, db_atomic_t *));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_inc(env, p)
	ENV *env;
	db_atomic_t *p;
{
	COMPQUIET(env, NULL);
	return (__sync_add_and_fetch(&p->value, 1));
}

/*
 * __os_atomic_dec --
 *	Atomically decrement by 1, return the new value.
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_SYNC_BUILTIN) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT)
 * PUBLIC: atomic_value_t __os_atomic_dec
 * PUBLIC:     __P((ENV *, db_atomic_t *));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_dec(env, p)
	ENV *env;
	db_atomic_t *p;
{
	COMPQUIET(env, NULL);
	return (__sync_sub_and_fetch(&p->value, 1));
}

/*
 * __os_atomic_cas --
 *	Atomic compare-and-swap.  Returns 1 on success, 0 on failure.
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_SYNC_BUILTIN) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT)
 * PUBLIC: int __os_atomic_cas __P((ENV *,
 * PUBLIC:     db_atomic_t *, atomic_value_t, atomic_value_t));
 * PUBLIC: #endif
 */
int
__os_atomic_cas(env, p, oldval, newval)
	ENV *env;
	db_atomic_t *p;
	atomic_value_t oldval;
	atomic_value_t newval;
{
	COMPQUIET(env, NULL);
	return (__sync_bool_compare_and_swap(&p->value, oldval, newval));
}

/*
 * __os_atomic_add --
 *	Atomically add a value, return the new value.
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_SYNC_BUILTIN) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT)
 * PUBLIC: atomic_value_t __os_atomic_add
 * PUBLIC:     __P((ENV *, db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_add(env, p, val)
	ENV *env;
	db_atomic_t *p;
	atomic_value_t val;
{
	COMPQUIET(env, NULL);
	return (__sync_add_and_fetch(&p->value, val));
}

/*
 * __os_atomic_fetch_add --
 *	Atomically add a value, return the old value (before addition).
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_SYNC_BUILTIN) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT)
 * PUBLIC: atomic_value_t __os_atomic_fetch_add
 * PUBLIC:     __P((ENV *, db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_fetch_add(env, p, val)
	ENV *env;
	db_atomic_t *p;
	atomic_value_t val;
{
	COMPQUIET(env, NULL);
	return (__sync_fetch_and_add(&p->value, val));
}

/*
 * __os_atomic_exchange --
 *	Atomically set a new value, return the old value.
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_SYNC_BUILTIN) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT)
 * PUBLIC: atomic_value_t __os_atomic_exchange
 * PUBLIC:     __P((ENV *, db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_exchange(env, p, val)
	ENV *env;
	db_atomic_t *p;
	atomic_value_t val;
{
	atomic_value_t old;

	COMPQUIET(env, NULL);
	for (;;) {
		old = p->value;
		if (__sync_bool_compare_and_swap(&p->value, old, val))
			return (old);
	}
}

/*
 * __os_atomic_thread_fence --
 *	Full memory barrier.
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_SYNC_BUILTIN) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT)
 * PUBLIC: void __os_atomic_thread_fence __P((void));
 * PUBLIC: #endif
 */
void
__os_atomic_thread_fence()
{
	__sync_synchronize();
}

#ifdef HAVE_64BIT_TYPES
/*
 * __os_atomic_cas_64 --
 *	64-bit atomic compare-and-swap.  Returns 1 on success, 0 on failure.
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_SYNC_BUILTIN) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && defined(HAVE_64BIT_TYPES)
 * PUBLIC: int __os_atomic_cas_64
 * PUBLIC:     __P((ENV *, volatile int64_t *, int64_t, int64_t));
 * PUBLIC: #endif
 */
int
__os_atomic_cas_64(env, p, oldval, newval)
	ENV *env;
	volatile int64_t *p;
	int64_t oldval;
	int64_t newval;
{
	COMPQUIET(env, NULL);
	return (__sync_bool_compare_and_swap(p, oldval, newval));
}
#endif /* HAVE_64BIT_TYPES */

#endif /* HAVE_ATOMIC_SYNC_BUILTIN && HAVE_ATOMIC_SUPPORT */

/*
 * =====================================================================
 * Tier 3: Architecture-specific inline assembly.
 *
 * These are used when compiler builtins are not available.  Each
 * architecture section provides inline assembly with correct
 * register constraints, clobber lists, and memory barriers.
 * =====================================================================
 */

#if defined(HAVE_ATOMIC_X86_GCC_ASSEMBLY) && defined(HAVE_ATOMIC_SUPPORT) && \
    !defined(HAVE_ATOMIC_GCC_BUILTIN) && !defined(HAVE_ATOMIC_SYNC_BUILTIN)

/*
 * __os_atomic_init --
 *	Initialize an atomic variable (x86 assembly path).
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_X86_GCC_ASSEMBLY) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: void __os_atomic_init
 * PUBLIC:     __P((db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
void
__os_atomic_init(p, val)
	db_atomic_t *p;
	atomic_value_t val;
{
	p->value = val;
}

/*
 * __os_atomic_read --
 *	Atomically load and return the current value (x86).
 *	On x86, aligned 32-bit reads are naturally atomic.
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_X86_GCC_ASSEMBLY) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: atomic_value_t __os_atomic_read
 * PUBLIC:     __P((const db_atomic_t *));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_read(p)
	const db_atomic_t *p;
{
	atomic_value_t val;

	/*
	 * On x86/x86_64, aligned 32-bit loads are atomic.  Use a
	 * compiler barrier to prevent reordering.
	 */
	val = p->value;
	__asm__ __volatile__("" ::: "memory");
	return (val);
}

/*
 * __os_atomic_store --
 *	Atomically store a value (x86).
 *	On x86, aligned 32-bit writes are naturally atomic.
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_X86_GCC_ASSEMBLY) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: void __os_atomic_store
 * PUBLIC:     __P((db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
void
__os_atomic_store(p, val)
	db_atomic_t *p;
	atomic_value_t val;
{
	/*
	 * On x86/x86_64, aligned 32-bit stores are atomic.  Use a
	 * compiler barrier to ensure ordering with surrounding code.
	 */
	__asm__ __volatile__("" ::: "memory");
	p->value = val;
	__asm__ __volatile__("" ::: "memory");
}

/*
 * __os_atomic_inc --
 *	Atomically increment by 1, return the new value (x86).
 *	Uses lock; xaddl with correct memory operand constraints.
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_X86_GCC_ASSEMBLY) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: atomic_value_t __os_atomic_inc
 * PUBLIC:     __P((ENV *, db_atomic_t *));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_inc(env, p)
	ENV *env;
	db_atomic_t *p;
{
	atomic_value_t temp;

	COMPQUIET(env, NULL);
	temp = 1;
	/*
	 * Use "m" constraint for the memory operand to let the compiler
	 * generate correct addressing, and explicit "l" size suffix.
	 */
	__asm__ __volatile__("lock; xaddl %0, %1"
	    : "+r"(temp), "+m"(p->value)
	    :
	    : "memory", "cc");
	return (temp + 1);
}

/*
 * __os_atomic_dec --
 *	Atomically decrement by 1, return the new value (x86).
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_X86_GCC_ASSEMBLY) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: atomic_value_t __os_atomic_dec
 * PUBLIC:     __P((ENV *, db_atomic_t *));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_dec(env, p)
	ENV *env;
	db_atomic_t *p;
{
	atomic_value_t temp;

	COMPQUIET(env, NULL);
	temp = -1;
	__asm__ __volatile__("lock; xaddl %0, %1"
	    : "+r"(temp), "+m"(p->value)
	    :
	    : "memory", "cc");
	return (temp - 1);
}

/*
 * __os_atomic_cas --
 *	Atomic compare-and-swap (x86).
 *	Uses lock; cmpxchgl with correct constraints and clobbers.
 *	Returns 1 on success, 0 on failure.
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_X86_GCC_ASSEMBLY) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: int __os_atomic_cas __P((ENV *,
 * PUBLIC:     db_atomic_t *, atomic_value_t, atomic_value_t));
 * PUBLIC: #endif
 */
int
__os_atomic_cas(env, p, oldval, newval)
	ENV *env;
	db_atomic_t *p;
	atomic_value_t oldval;
	atomic_value_t newval;
{
	atomic_value_t was;

	COMPQUIET(env, NULL);
	/*
	 * Fast check without locking the cache line.  This is safe
	 * because a false positive just falls through to the real CAS.
	 */
	if (p->value != oldval)
		return (0);
	/*
	 * Use "m" constraint for the memory operand, and mark it as
	 * both input and output ("+m") so GCC knows the memory is
	 * modified.  The "0" constraint ties 'was' to the same register
	 * as the "a" input.
	 */
	__asm__ __volatile__("lock; cmpxchgl %2, %1"
	    : "=a"(was), "+m"(p->value)
	    : "r"(newval), "0"(oldval)
	    : "memory", "cc");
	return (was == oldval);
}

/*
 * __os_atomic_add --
 *	Atomically add a value, return the new value (x86).
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_X86_GCC_ASSEMBLY) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: atomic_value_t __os_atomic_add
 * PUBLIC:     __P((ENV *, db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_add(env, p, val)
	ENV *env;
	db_atomic_t *p;
	atomic_value_t val;
{
	atomic_value_t temp;

	COMPQUIET(env, NULL);
	temp = val;
	__asm__ __volatile__("lock; xaddl %0, %1"
	    : "+r"(temp), "+m"(p->value)
	    :
	    : "memory", "cc");
	return (temp + val);
}

/*
 * __os_atomic_fetch_add --
 *	Atomically add a value, return the old value (x86).
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_X86_GCC_ASSEMBLY) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: atomic_value_t __os_atomic_fetch_add
 * PUBLIC:     __P((ENV *, db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_fetch_add(env, p, val)
	ENV *env;
	db_atomic_t *p;
	atomic_value_t val;
{
	atomic_value_t temp;

	COMPQUIET(env, NULL);
	temp = val;
	/* xadd returns the old value in the register operand. */
	__asm__ __volatile__("lock; xaddl %0, %1"
	    : "+r"(temp), "+m"(p->value)
	    :
	    : "memory", "cc");
	return (temp);
}

/*
 * __os_atomic_exchange --
 *	Atomically set a new value, return the old value (x86).
 *	Uses xchg which has an implicit lock prefix on x86.
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_X86_GCC_ASSEMBLY) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: atomic_value_t __os_atomic_exchange
 * PUBLIC:     __P((ENV *, db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_exchange(env, p, val)
	ENV *env;
	db_atomic_t *p;
	atomic_value_t val;
{
	atomic_value_t ret;

	COMPQUIET(env, NULL);
	ret = val;
	/* xchg has an implicit lock prefix; always atomic on x86. */
	__asm__ __volatile__("xchgl %0, %1"
	    : "+r"(ret), "+m"(p->value)
	    :
	    : "memory");
	return (ret);
}

/*
 * __os_atomic_thread_fence --
 *	Full memory barrier (x86).
 *	Uses mfence on x86_64, lock; addl $0 on x86 for compatibility.
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_X86_GCC_ASSEMBLY) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: void __os_atomic_thread_fence __P((void));
 * PUBLIC: #endif
 */
void
__os_atomic_thread_fence()
{
#if defined(__x86_64__) || defined(x86_64)
	__asm__ __volatile__("mfence" ::: "memory");
#else
	/*
	 * On 32-bit x86, mfence may not be available (requires SSE2).
	 * Use a locked add to the stack as a full barrier.
	 */
	__asm__ __volatile__("lock; addl $0, (%%esp)" ::: "memory", "cc");
#endif
}

#endif /* HAVE_ATOMIC_X86_GCC_ASSEMBLY && !builtins */

/*
 * =====================================================================
 * ARM64/AArch64 inline assembly.
 *
 * Uses ldxr/stxr (load-exclusive/store-conditional) instructions for
 * atomic operations.  Available on ARMv8+ (64-bit ARM).
 * =====================================================================
 */

#if defined(HAVE_ATOMIC_ARM64_ASSEMBLY) && defined(HAVE_ATOMIC_SUPPORT) && \
    !defined(HAVE_ATOMIC_GCC_BUILTIN) && !defined(HAVE_ATOMIC_SYNC_BUILTIN)

/*
 * __os_atomic_init --
 *	Initialize an atomic variable (ARM64 assembly path).
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_ARM64_ASSEMBLY) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: void __os_atomic_init
 * PUBLIC:     __P((db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
void
__os_atomic_init(p, val)
	db_atomic_t *p;
	atomic_value_t val;
{
	p->value = val;
}

/*
 * __os_atomic_read --
 *	Atomically load and return the current value (ARM64).
 *	On ARM64, aligned 32-bit reads are naturally atomic.
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_ARM64_ASSEMBLY) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: atomic_value_t __os_atomic_read
 * PUBLIC:     __P((const db_atomic_t *));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_read(p)
	const db_atomic_t *p;
{
	atomic_value_t val;

	/*
	 * Use ldar (load-acquire) for acquire semantics.
	 * For full sequential consistency, use dmb after load.
	 */
	__asm__ __volatile__(
	    "ldr %w0, %1\n\t"
	    "dmb ish"
	    : "=r"(val)
	    : "Q"(p->value)
	    : "memory");
	return (val);
}

/*
 * __os_atomic_store --
 *	Atomically store a value (ARM64).
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_ARM64_ASSEMBLY) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: void __os_atomic_store
 * PUBLIC:     __P((db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
void
__os_atomic_store(p, val)
	db_atomic_t *p;
	atomic_value_t val;
{
	/*
	 * Use dmb before store, then stlr (store-release) for
	 * sequential consistency.
	 */
	__asm__ __volatile__(
	    "dmb ish\n\t"
	    "str %w1, %0"
	    : "=Q"(p->value)
	    : "r"(val)
	    : "memory");
}

/*
 * __os_atomic_inc --
 *	Atomically increment by 1, return the new value (ARM64).
 *	Uses ldxr/stxr with retry loop.
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_ARM64_ASSEMBLY) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: atomic_value_t __os_atomic_inc
 * PUBLIC:     __P((ENV *, db_atomic_t *));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_inc(env, p)
	ENV *env;
	db_atomic_t *p;
{
	atomic_value_t result, temp;
	int failed;

	COMPQUIET(env, NULL);
	__asm__ __volatile__(
	    "1:\n\t"
	    "ldxr %w0, %2\n\t"          /* Load exclusive */
	    "add %w0, %w0, #1\n\t"      /* Increment */
	    "stxr %w1, %w0, %2\n\t"     /* Store conditional */
	    "cbnz %w1, 1b\n\t"          /* Retry if failed */
	    "dmb ish"                   /* Memory barrier */
	    : "=&r"(result), "=&r"(failed), "+Q"(p->value)
	    :
	    : "memory");
	return (result);
}

/*
 * __os_atomic_dec --
 *	Atomically decrement by 1, return the new value (ARM64).
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_ARM64_ASSEMBLY) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: atomic_value_t __os_atomic_dec
 * PUBLIC:     __P((ENV *, db_atomic_t *));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_dec(env, p)
	ENV *env;
	db_atomic_t *p;
{
	atomic_value_t result, temp;
	int failed;

	COMPQUIET(env, NULL);
	__asm__ __volatile__(
	    "1:\n\t"
	    "ldxr %w0, %2\n\t"
	    "sub %w0, %w0, #1\n\t"
	    "stxr %w1, %w0, %2\n\t"
	    "cbnz %w1, 1b\n\t"
	    "dmb ish"
	    : "=&r"(result), "=&r"(failed), "+Q"(p->value)
	    :
	    : "memory");
	return (result);
}

/*
 * __os_atomic_cas --
 *	Atomic compare-and-swap (ARM64).
 *	Uses ldxr/stxr with comparison.
 *	Returns 1 on success, 0 on failure.
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_ARM64_ASSEMBLY) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: int __os_atomic_cas __P((ENV *,
 * PUBLIC:     db_atomic_t *, atomic_value_t, atomic_value_t));
 * PUBLIC: #endif
 */
int
__os_atomic_cas(env, p, oldval, newval)
	ENV *env;
	db_atomic_t *p;
	atomic_value_t oldval;
	atomic_value_t newval;
{
	atomic_value_t current;
	int result, failed;

	COMPQUIET(env, NULL);
	/* Fast check without exclusive access. */
	if (p->value != oldval)
		return (0);

	__asm__ __volatile__(
	    "1:\n\t"
	    "ldxr %w0, %3\n\t"          /* Load exclusive */
	    "cmp %w0, %w4\n\t"          /* Compare with expected */
	    "b.ne 2f\n\t"               /* Exit if not equal */
	    "stxr %w1, %w5, %3\n\t"     /* Store conditional */
	    "cbnz %w1, 1b\n\t"          /* Retry if failed */
	    "2:\n\t"
	    "cset %w2, eq\n\t"          /* Set result based on comparison */
	    "dmb ish"
	    : "=&r"(current), "=&r"(failed), "=&r"(result), "+Q"(p->value)
	    : "r"(oldval), "r"(newval)
	    : "memory", "cc");
	return (result);
}

/*
 * __os_atomic_add --
 *	Atomically add a value, return the new value (ARM64).
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_ARM64_ASSEMBLY) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: atomic_value_t __os_atomic_add
 * PUBLIC:     __P((ENV *, db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_add(env, p, val)
	ENV *env;
	db_atomic_t *p;
	atomic_value_t val;
{
	atomic_value_t result;
	int failed;

	COMPQUIET(env, NULL);
	__asm__ __volatile__(
	    "1:\n\t"
	    "ldxr %w0, %2\n\t"
	    "add %w0, %w0, %w3\n\t"
	    "stxr %w1, %w0, %2\n\t"
	    "cbnz %w1, 1b\n\t"
	    "dmb ish"
	    : "=&r"(result), "=&r"(failed), "+Q"(p->value)
	    : "r"(val)
	    : "memory");
	return (result);
}

/*
 * __os_atomic_fetch_add --
 *	Atomically add a value, return the old value (ARM64).
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_ARM64_ASSEMBLY) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: atomic_value_t __os_atomic_fetch_add
 * PUBLIC:     __P((ENV *, db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_fetch_add(env, p, val)
	ENV *env;
	db_atomic_t *p;
	atomic_value_t val;
{
	atomic_value_t old, temp;
	int failed;

	COMPQUIET(env, NULL);
	__asm__ __volatile__(
	    "1:\n\t"
	    "ldxr %w0, %3\n\t"
	    "add %w2, %w0, %w4\n\t"
	    "stxr %w1, %w2, %3\n\t"
	    "cbnz %w1, 1b\n\t"
	    "dmb ish"
	    : "=&r"(old), "=&r"(failed), "=&r"(temp), "+Q"(p->value)
	    : "r"(val)
	    : "memory");
	return (old);
}

/*
 * __os_atomic_exchange --
 *	Atomically set a new value, return the old value (ARM64).
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_ARM64_ASSEMBLY) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: atomic_value_t __os_atomic_exchange
 * PUBLIC:     __P((ENV *, db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_exchange(env, p, val)
	ENV *env;
	db_atomic_t *p;
	atomic_value_t val;
{
	atomic_value_t old;
	int failed;

	COMPQUIET(env, NULL);
	__asm__ __volatile__(
	    "1:\n\t"
	    "ldxr %w0, %2\n\t"
	    "stxr %w1, %w3, %2\n\t"
	    "cbnz %w1, 1b\n\t"
	    "dmb ish"
	    : "=&r"(old), "=&r"(failed), "+Q"(p->value)
	    : "r"(val)
	    : "memory");
	return (old);
}

/*
 * __os_atomic_thread_fence --
 *	Full memory barrier (ARM64).
 *	Uses dmb ish (data memory barrier, inner shareable).
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_ARM64_ASSEMBLY) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: void __os_atomic_thread_fence __P((void));
 * PUBLIC: #endif
 */
void
__os_atomic_thread_fence()
{
	__asm__ __volatile__("dmb ish" ::: "memory");
}

#endif /* HAVE_ATOMIC_ARM64_ASSEMBLY && !builtins */

/*
 * =====================================================================
 * ARM32 inline assembly.
 *
 * Uses ldrex/strex (load-exclusive/store-exclusive) instructions for
 * atomic operations.  Available on ARMv6+ (32-bit ARM).
 * For older ARM without ldrex, falls back to kernel helper functions.
 * =====================================================================
 */

#if defined(HAVE_ATOMIC_ARM32_ASSEMBLY) && defined(HAVE_ATOMIC_SUPPORT) && \
    !defined(HAVE_ATOMIC_GCC_BUILTIN) && !defined(HAVE_ATOMIC_SYNC_BUILTIN)

/*
 * __os_atomic_init --
 *	Initialize an atomic variable (ARM32 assembly path).
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_ARM32_ASSEMBLY) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: void __os_atomic_init
 * PUBLIC:     __P((db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
void
__os_atomic_init(p, val)
	db_atomic_t *p;
	atomic_value_t val;
{
	p->value = val;
}

/*
 * __os_atomic_read --
 *	Atomically load and return the current value (ARM32).
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_ARM32_ASSEMBLY) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: atomic_value_t __os_atomic_read
 * PUBLIC:     __P((const db_atomic_t *));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_read(p)
	const db_atomic_t *p;
{
	atomic_value_t val;

	/*
	 * Use dmb for full barrier on ARM32.
	 */
	__asm__ __volatile__(
	    "ldr %0, %1\n\t"
	    "dmb"
	    : "=r"(val)
	    : "m"(p->value)
	    : "memory");
	return (val);
}

/*
 * __os_atomic_store --
 *	Atomically store a value (ARM32).
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_ARM32_ASSEMBLY) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: void __os_atomic_store
 * PUBLIC:     __P((db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
void
__os_atomic_store(p, val)
	db_atomic_t *p;
	atomic_value_t val;
{
	__asm__ __volatile__(
	    "dmb\n\t"
	    "str %1, %0"
	    : "=m"(p->value)
	    : "r"(val)
	    : "memory");
}

/*
 * __os_atomic_inc --
 *	Atomically increment by 1, return the new value (ARM32).
 *	Uses ldrex/strex with retry loop.
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_ARM32_ASSEMBLY) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: atomic_value_t __os_atomic_inc
 * PUBLIC:     __P((ENV *, db_atomic_t *));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_inc(env, p)
	ENV *env;
	db_atomic_t *p;
{
	atomic_value_t result;
	int failed;

	COMPQUIET(env, NULL);
	__asm__ __volatile__(
	    "1:\n\t"
	    "ldrex %0, [%2]\n\t"        /* Load exclusive */
	    "add %0, %0, #1\n\t"        /* Increment */
	    "strex %1, %0, [%2]\n\t"    /* Store conditional */
	    "teq %1, #0\n\t"            /* Test result */
	    "bne 1b\n\t"                /* Retry if failed */
	    "dmb"                       /* Memory barrier */
	    : "=&r"(result), "=&r"(failed)
	    : "r"(&p->value)
	    : "memory", "cc");
	return (result);
}

/*
 * __os_atomic_dec --
 *	Atomically decrement by 1, return the new value (ARM32).
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_ARM32_ASSEMBLY) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: atomic_value_t __os_atomic_dec
 * PUBLIC:     __P((ENV *, db_atomic_t *));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_dec(env, p)
	ENV *env;
	db_atomic_t *p;
{
	atomic_value_t result;
	int failed;

	COMPQUIET(env, NULL);
	__asm__ __volatile__(
	    "1:\n\t"
	    "ldrex %0, [%2]\n\t"
	    "sub %0, %0, #1\n\t"
	    "strex %1, %0, [%2]\n\t"
	    "teq %1, #0\n\t"
	    "bne 1b\n\t"
	    "dmb"
	    : "=&r"(result), "=&r"(failed)
	    : "r"(&p->value)
	    : "memory", "cc");
	return (result);
}

/*
 * __os_atomic_cas --
 *	Atomic compare-and-swap (ARM32).
 *	Returns 1 on success, 0 on failure.
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_ARM32_ASSEMBLY) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: int __os_atomic_cas __P((ENV *,
 * PUBLIC:     db_atomic_t *, atomic_value_t, atomic_value_t));
 * PUBLIC: #endif
 */
int
__os_atomic_cas(env, p, oldval, newval)
	ENV *env;
	db_atomic_t *p;
	atomic_value_t oldval;
	atomic_value_t newval;
{
	atomic_value_t current;
	int result, failed;

	COMPQUIET(env, NULL);
	/* Fast check without exclusive access. */
	if (p->value != oldval)
		return (0);

	__asm__ __volatile__(
	    "1:\n\t"
	    "ldrex %0, [%4]\n\t"        /* Load exclusive */
	    "cmp %0, %5\n\t"            /* Compare with expected */
	    "bne 2f\n\t"                /* Exit if not equal */
	    "strex %1, %6, [%4]\n\t"    /* Store conditional */
	    "teq %1, #0\n\t"
	    "bne 1b\n\t"                /* Retry if failed */
	    "2:\n\t"
	    "moveq %2, #1\n\t"          /* Success: set result = 1 */
	    "movne %2, #0\n\t"          /* Failure: set result = 0 */
	    "dmb"
	    : "=&r"(current), "=&r"(failed), "=&r"(result), "+m"(p->value)
	    : "r"(&p->value), "r"(oldval), "r"(newval)
	    : "memory", "cc");
	return (result);
}

/*
 * __os_atomic_add --
 *	Atomically add a value, return the new value (ARM32).
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_ARM32_ASSEMBLY) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: atomic_value_t __os_atomic_add
 * PUBLIC:     __P((ENV *, db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_add(env, p, val)
	ENV *env;
	db_atomic_t *p;
	atomic_value_t val;
{
	atomic_value_t result;
	int failed;

	COMPQUIET(env, NULL);
	__asm__ __volatile__(
	    "1:\n\t"
	    "ldrex %0, [%2]\n\t"
	    "add %0, %0, %3\n\t"
	    "strex %1, %0, [%2]\n\t"
	    "teq %1, #0\n\t"
	    "bne 1b\n\t"
	    "dmb"
	    : "=&r"(result), "=&r"(failed)
	    : "r"(&p->value), "r"(val)
	    : "memory", "cc");
	return (result);
}

/*
 * __os_atomic_fetch_add --
 *	Atomically add a value, return the old value (ARM32).
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_ARM32_ASSEMBLY) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: atomic_value_t __os_atomic_fetch_add
 * PUBLIC:     __P((ENV *, db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_fetch_add(env, p, val)
	ENV *env;
	db_atomic_t *p;
	atomic_value_t val;
{
	atomic_value_t old, temp;
	int failed;

	COMPQUIET(env, NULL);
	__asm__ __volatile__(
	    "1:\n\t"
	    "ldrex %0, [%3]\n\t"
	    "add %2, %0, %4\n\t"
	    "strex %1, %2, [%3]\n\t"
	    "teq %1, #0\n\t"
	    "bne 1b\n\t"
	    "dmb"
	    : "=&r"(old), "=&r"(failed), "=&r"(temp)
	    : "r"(&p->value), "r"(val)
	    : "memory", "cc");
	return (old);
}

/*
 * __os_atomic_exchange --
 *	Atomically set a new value, return the old value (ARM32).
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_ARM32_ASSEMBLY) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: atomic_value_t __os_atomic_exchange
 * PUBLIC:     __P((ENV *, db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_exchange(env, p, val)
	ENV *env;
	db_atomic_t *p;
	atomic_value_t val;
{
	atomic_value_t old;
	int failed;

	COMPQUIET(env, NULL);
	__asm__ __volatile__(
	    "1:\n\t"
	    "ldrex %0, [%2]\n\t"
	    "strex %1, %3, [%2]\n\t"
	    "teq %1, #0\n\t"
	    "bne 1b\n\t"
	    "dmb"
	    : "=&r"(old), "=&r"(failed)
	    : "r"(&p->value), "r"(val)
	    : "memory", "cc");
	return (old);
}

/*
 * __os_atomic_thread_fence --
 *	Full memory barrier (ARM32).
 *	Uses dmb (data memory barrier).
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_ARM32_ASSEMBLY) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: void __os_atomic_thread_fence __P((void));
 * PUBLIC: #endif
 */
void
__os_atomic_thread_fence()
{
	__asm__ __volatile__("dmb" ::: "memory");
}

#endif /* HAVE_ATOMIC_ARM32_ASSEMBLY && !builtins */

/*
 * =====================================================================
 * Tier 4: OS-specific APIs.
 *
 * Solaris atomic.h functions and Windows Interlocked APIs.
 * =====================================================================
 */

#if defined(HAVE_ATOMIC_SOLARIS) && defined(HAVE_ATOMIC_SUPPORT) && \
    !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
    !defined(HAVE_ATOMIC_SYNC_BUILTIN)
#include <atomic.h>

/*
 * __os_atomic_init --
 *	Initialize an atomic variable (Solaris).
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_SOLARIS) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: void __os_atomic_init
 * PUBLIC:     __P((db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
void
__os_atomic_init(p, val)
	db_atomic_t *p;
	atomic_value_t val;
{
	p->value = val;
}

/*
 * __os_atomic_read --
 *	Atomically load (Solaris).
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_SOLARIS) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: atomic_value_t __os_atomic_read
 * PUBLIC:     __P((const db_atomic_t *));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_read(p)
	const db_atomic_t *p;
{
	membar_consumer();
	return ((atomic_value_t)p->value);
}

/*
 * __os_atomic_store --
 *	Atomically store (Solaris).
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_SOLARIS) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: void __os_atomic_store
 * PUBLIC:     __P((db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
void
__os_atomic_store(p, val)
	db_atomic_t *p;
	atomic_value_t val;
{
	p->value = val;
	membar_producer();
}

/*
 * __os_atomic_inc --
 *	Atomically increment by 1, return the new value (Solaris).
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_SOLARIS) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: atomic_value_t __os_atomic_inc
 * PUBLIC:     __P((ENV *, db_atomic_t *));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_inc(env, p)
	ENV *env;
	db_atomic_t *p;
{
	COMPQUIET(env, NULL);
	return ((atomic_value_t)atomic_inc_uint_nv(
	    (volatile unsigned int *)&p->value));
}

/*
 * __os_atomic_dec --
 *	Atomically decrement by 1, return the new value (Solaris).
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_SOLARIS) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: atomic_value_t __os_atomic_dec
 * PUBLIC:     __P((ENV *, db_atomic_t *));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_dec(env, p)
	ENV *env;
	db_atomic_t *p;
{
	COMPQUIET(env, NULL);
	return ((atomic_value_t)atomic_dec_uint_nv(
	    (volatile unsigned int *)&p->value));
}

/*
 * __os_atomic_cas --
 *	Atomic compare-and-swap (Solaris).
 *	Returns 1 on success, 0 on failure.
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_SOLARIS) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: int __os_atomic_cas __P((ENV *,
 * PUBLIC:     db_atomic_t *, atomic_value_t, atomic_value_t));
 * PUBLIC: #endif
 */
int
__os_atomic_cas(env, p, oldval, newval)
	ENV *env;
	db_atomic_t *p;
	atomic_value_t oldval;
	atomic_value_t newval;
{
	COMPQUIET(env, NULL);
	return (atomic_cas_32(
	    (volatile unsigned int *)&p->value,
	    (unsigned int)oldval,
	    (unsigned int)newval) == (unsigned int)oldval);
}

/*
 * __os_atomic_add --
 *	Atomically add a value, return the new value (Solaris).
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_SOLARIS) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: atomic_value_t __os_atomic_add
 * PUBLIC:     __P((ENV *, db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_add(env, p, val)
	ENV *env;
	db_atomic_t *p;
	atomic_value_t val;
{
	COMPQUIET(env, NULL);
	return ((atomic_value_t)atomic_add_int_nv(
	    (volatile unsigned int *)&p->value, val));
}

/*
 * __os_atomic_fetch_add --
 *	Atomically add, return the old value (Solaris).
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_SOLARIS) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: atomic_value_t __os_atomic_fetch_add
 * PUBLIC:     __P((ENV *, db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_fetch_add(env, p, val)
	ENV *env;
	db_atomic_t *p;
	atomic_value_t val;
{
	atomic_value_t old;

	COMPQUIET(env, NULL);
	/*
	 * Solaris has no fetch-and-add; use the add_nv variant and
	 * subtract to recover the old value.
	 */
	old = (atomic_value_t)atomic_add_int_nv(
	    (volatile unsigned int *)&p->value, val);
	return (old - val);
}

/*
 * __os_atomic_exchange --
 *	Atomically set a new value, return the old value (Solaris).
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_SOLARIS) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: atomic_value_t __os_atomic_exchange
 * PUBLIC:     __P((ENV *, db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_exchange(env, p, val)
	ENV *env;
	db_atomic_t *p;
	atomic_value_t val;
{
	COMPQUIET(env, NULL);
	return ((atomic_value_t)atomic_swap_32(
	    (volatile unsigned int *)&p->value, (unsigned int)val));
}

/*
 * __os_atomic_thread_fence --
 *	Full memory barrier (Solaris).
 *
 * PUBLIC: #if defined(HAVE_ATOMIC_SOLARIS) && \
 * PUBLIC:     defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: void __os_atomic_thread_fence __P((void));
 * PUBLIC: #endif
 */
void
__os_atomic_thread_fence()
{
	membar_enter();
}

#endif /* HAVE_ATOMIC_SOLARIS && !builtins */

/*
 * Tier 4 (continued): Windows Interlocked APIs.
 */

#if defined(DB_WIN32) && defined(HAVE_ATOMIC_SUPPORT) && \
    !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
    !defined(HAVE_ATOMIC_SYNC_BUILTIN)

/*
 * __os_atomic_init --
 *	Initialize an atomic variable (Windows).
 *
 * PUBLIC: #if defined(DB_WIN32) && defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: void __os_atomic_init
 * PUBLIC:     __P((db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
void
__os_atomic_init(p, val)
	db_atomic_t *p;
	atomic_value_t val;
{
	p->value = val;
#if defined(DB_WINCE)
	p->dummy = 0;
#endif
}

/*
 * __os_atomic_read --
 *	Atomically load (Windows).
 *
 * PUBLIC: #if defined(DB_WIN32) && defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: atomic_value_t __os_atomic_read
 * PUBLIC:     __P((const db_atomic_t *));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_read(p)
	const db_atomic_t *p;
{
	/*
	 * On Windows x86/x64, aligned 32-bit reads are atomic.
	 * Use a compiler barrier to prevent reordering.
	 */
	MemoryBarrier();
	return (p->value);
}

/*
 * __os_atomic_store --
 *	Atomically store (Windows).
 *
 * PUBLIC: #if defined(DB_WIN32) && defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: void __os_atomic_store
 * PUBLIC:     __P((db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
void
__os_atomic_store(p, val)
	db_atomic_t *p;
	atomic_value_t val;
{
	WINCE_ATOMIC_MAGIC(p);
	InterlockedExchange(
	    (interlocked_val)(&p->value), (LONG)val);
}

/*
 * __os_atomic_inc --
 *	Atomically increment by 1, return the new value (Windows).
 *
 * PUBLIC: #if defined(DB_WIN32) && defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: atomic_value_t __os_atomic_inc
 * PUBLIC:     __P((ENV *, db_atomic_t *));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_inc(env, p)
	ENV *env;
	db_atomic_t *p;
{
	COMPQUIET(env, NULL);
	WINCE_ATOMIC_MAGIC(p);
	return ((atomic_value_t)InterlockedIncrement(
	    (interlocked_val)(&p->value)));
}

/*
 * __os_atomic_dec --
 *	Atomically decrement by 1, return the new value (Windows).
 *
 * PUBLIC: #if defined(DB_WIN32) && defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: atomic_value_t __os_atomic_dec
 * PUBLIC:     __P((ENV *, db_atomic_t *));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_dec(env, p)
	ENV *env;
	db_atomic_t *p;
{
	COMPQUIET(env, NULL);
	WINCE_ATOMIC_MAGIC(p);
	return ((atomic_value_t)InterlockedDecrement(
	    (interlocked_val)(&p->value)));
}

/*
 * __os_atomic_cas --
 *	Atomic compare-and-swap (Windows).
 *	Returns 1 on success, 0 on failure.
 *
 * PUBLIC: #if defined(DB_WIN32) && defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: int __os_atomic_cas __P((ENV *,
 * PUBLIC:     db_atomic_t *, atomic_value_t, atomic_value_t));
 * PUBLIC: #endif
 */
int
__os_atomic_cas(env, p, oldval, newval)
	ENV *env;
	db_atomic_t *p;
	atomic_value_t oldval;
	atomic_value_t newval;
{
	COMPQUIET(env, NULL);
	WINCE_ATOMIC_MAGIC(p);
	return (InterlockedCompareExchange(
	    (interlocked_val)(&p->value),
	    (LONG)newval, (LONG)oldval) == (LONG)oldval);
}

/*
 * __os_atomic_add --
 *	Atomically add a value, return the new value (Windows).
 *
 * PUBLIC: #if defined(DB_WIN32) && defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: atomic_value_t __os_atomic_add
 * PUBLIC:     __P((ENV *, db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_add(env, p, val)
	ENV *env;
	db_atomic_t *p;
	atomic_value_t val;
{
	COMPQUIET(env, NULL);
	WINCE_ATOMIC_MAGIC(p);
	/* InterlockedExchangeAdd returns the OLD value. */
	return ((atomic_value_t)InterlockedExchangeAdd(
	    (interlocked_val)(&p->value), (LONG)val) + val);
}

/*
 * __os_atomic_fetch_add --
 *	Atomically add, return the old value (Windows).
 *
 * PUBLIC: #if defined(DB_WIN32) && defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: atomic_value_t __os_atomic_fetch_add
 * PUBLIC:     __P((ENV *, db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_fetch_add(env, p, val)
	ENV *env;
	db_atomic_t *p;
	atomic_value_t val;
{
	COMPQUIET(env, NULL);
	WINCE_ATOMIC_MAGIC(p);
	return ((atomic_value_t)InterlockedExchangeAdd(
	    (interlocked_val)(&p->value), (LONG)val));
}

/*
 * __os_atomic_exchange --
 *	Atomically set a new value, return the old value (Windows).
 *
 * PUBLIC: #if defined(DB_WIN32) && defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: atomic_value_t __os_atomic_exchange
 * PUBLIC:     __P((ENV *, db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_exchange(env, p, val)
	ENV *env;
	db_atomic_t *p;
	atomic_value_t val;
{
	COMPQUIET(env, NULL);
	WINCE_ATOMIC_MAGIC(p);
	return ((atomic_value_t)InterlockedExchange(
	    (interlocked_val)(&p->value), (LONG)val));
}

/*
 * __os_atomic_thread_fence --
 *	Full memory barrier (Windows).
 *
 * PUBLIC: #if defined(DB_WIN32) && defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN)
 * PUBLIC: void __os_atomic_thread_fence __P((void));
 * PUBLIC: #endif
 */
void
__os_atomic_thread_fence()
{
	MemoryBarrier();
}

#ifdef HAVE_64BIT_TYPES
/*
 * __os_atomic_cas_64 --
 *	64-bit atomic compare-and-swap (Windows).
 *	Returns 1 on success, 0 on failure.
 *
 * PUBLIC: #if defined(DB_WIN32) && defined(HAVE_ATOMIC_SUPPORT) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_GCC_BUILTIN) && \
 * PUBLIC:     !defined(HAVE_ATOMIC_SYNC_BUILTIN) && \
 * PUBLIC:     defined(HAVE_64BIT_TYPES)
 * PUBLIC: int __os_atomic_cas_64
 * PUBLIC:     __P((ENV *, volatile int64_t *, int64_t, int64_t));
 * PUBLIC: #endif
 */
int
__os_atomic_cas_64(env, p, oldval, newval)
	ENV *env;
	volatile int64_t *p;
	int64_t oldval;
	int64_t newval;
{
	COMPQUIET(env, NULL);
	return (InterlockedCompareExchange64(
	    p, newval, oldval) == oldval);
}
#endif /* HAVE_64BIT_TYPES */

#endif /* DB_WIN32 && HAVE_ATOMIC_SUPPORT && !builtins */

/*
 * =====================================================================
 * Tier 5: Mutex-based emulation fallback.
 *
 * When no native atomic support is available but mutexes are, emulate
 * all atomic operations using a hash-based pool of mutexes.
 *
 * The address of the atomic variable is hashed to select one of
 * MAX_ATOMIC_MUTEXES protecting mutexes, reducing contention across
 * independent variables while keeping the mutex pool small.
 *
 * Query operations (init, read, store, fence) do NOT need ENV because:
 *   - init: single-threaded context, no contention
 *   - read: volatile provides visibility on all platforms
 *   - store: volatile provides visibility on all platforms
 *   - fence: platform-level operation, no ENV state needed
 *
 * Modification operations (inc, dec, cas, add, etc.) need ENV to
 * look up the mutex pool in the environment's mutex region.
 * =====================================================================
 */

#if !defined(HAVE_ATOMIC_SUPPORT) && defined(HAVE_MUTEX_SUPPORT)

/*
 * __os_atomic_get_mutex --
 *	Map an atomic variable address to a protecting mutex.
 *	Uses bit-shift and modulo to distribute across the mutex pool.
 */
static inline db_mutex_t
__os_atomic_get_mutex(env, v)
	ENV *env;
	db_atomic_t *v;
{
	u_int index;
	DB_MUTEXREGION *mtxreg;

	if (!MUTEX_ON(env))
		return (MUTEX_INVALID);
	index = (u_int)(((uintptr_t)(v)) >> 6) % MAX_ATOMIC_MUTEXES;
	mtxreg = (DB_MUTEXREGION *)env->mutex_handle->reginfo.primary;
	return (mtxreg->mtx_atomic[index]);
}

/*
 * __os_atomic_init --
 *	Initialize an atomic variable (mutex fallback).
 *	Single-threaded context; no mutex protection needed.
 *
 * PUBLIC: #if !defined(HAVE_ATOMIC_SUPPORT) && defined(HAVE_MUTEX_SUPPORT)
 * PUBLIC: void __os_atomic_init
 * PUBLIC:     __P((db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
void
__os_atomic_init(p, val)
	db_atomic_t *p;
	atomic_value_t val;
{
	p->value = val;
}

/*
 * __os_atomic_read --
 *	Atomically load the current value (mutex fallback).
 *	Volatile qualifier on db_atomic_t.value provides visibility.
 *
 * PUBLIC: #if !defined(HAVE_ATOMIC_SUPPORT) && defined(HAVE_MUTEX_SUPPORT)
 * PUBLIC: atomic_value_t __os_atomic_read
 * PUBLIC:     __P((const db_atomic_t *));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_read(p)
	const db_atomic_t *p;
{
	return (p->value);
}

/*
 * __os_atomic_store --
 *	Atomically store a value (mutex fallback).
 *	Volatile qualifier on db_atomic_t.value provides visibility.
 *
 * PUBLIC: #if !defined(HAVE_ATOMIC_SUPPORT) && defined(HAVE_MUTEX_SUPPORT)
 * PUBLIC: void __os_atomic_store
 * PUBLIC:     __P((db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
void
__os_atomic_store(p, val)
	db_atomic_t *p;
	atomic_value_t val;
{
	p->value = val;
}

/*
 * __os_atomic_inc --
 *	Atomically increment by 1, return the new value (mutex fallback).
 *
 * PUBLIC: #if !defined(HAVE_ATOMIC_SUPPORT) && defined(HAVE_MUTEX_SUPPORT)
 * PUBLIC: atomic_value_t __os_atomic_inc
 * PUBLIC:     __P((ENV *, db_atomic_t *));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_inc(env, p)
	ENV *env;
	db_atomic_t *p;
{
	db_mutex_t mtx;
	atomic_value_t ret;

	mtx = __os_atomic_get_mutex(env, p);
	MUTEX_LOCK(env, mtx);
	ret = ++p->value;
	MUTEX_UNLOCK(env, mtx);
	return (ret);
}

/*
 * __os_atomic_dec --
 *	Atomically decrement by 1, return the new value (mutex fallback).
 *
 * PUBLIC: #if !defined(HAVE_ATOMIC_SUPPORT) && defined(HAVE_MUTEX_SUPPORT)
 * PUBLIC: atomic_value_t __os_atomic_dec
 * PUBLIC:     __P((ENV *, db_atomic_t *));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_dec(env, p)
	ENV *env;
	db_atomic_t *p;
{
	db_mutex_t mtx;
	atomic_value_t ret;

	mtx = __os_atomic_get_mutex(env, p);
	MUTEX_LOCK(env, mtx);
	ret = --p->value;
	MUTEX_UNLOCK(env, mtx);
	return (ret);
}

/*
 * __os_atomic_cas --
 *	Atomic compare-and-swap (mutex fallback).
 *	Returns 1 on success, 0 on failure.
 *
 * PUBLIC: #if !defined(HAVE_ATOMIC_SUPPORT) && defined(HAVE_MUTEX_SUPPORT)
 * PUBLIC: int __os_atomic_cas __P((ENV *,
 * PUBLIC:     db_atomic_t *, atomic_value_t, atomic_value_t));
 * PUBLIC: #endif
 */
int
__os_atomic_cas(env, p, oldval, newval)
	ENV *env;
	db_atomic_t *p;
	atomic_value_t oldval;
	atomic_value_t newval;
{
	db_mutex_t mtx;
	int ret;

	/* Fast check without lock. */
	if (p->value != oldval)
		return (0);

	mtx = __os_atomic_get_mutex(env, p);
	MUTEX_LOCK(env, mtx);
	ret = (p->value == oldval);
	if (ret)
		p->value = newval;
	MUTEX_UNLOCK(env, mtx);
	return (ret);
}

/*
 * __os_atomic_add --
 *	Atomically add a value, return the new value (mutex fallback).
 *
 * PUBLIC: #if !defined(HAVE_ATOMIC_SUPPORT) && defined(HAVE_MUTEX_SUPPORT)
 * PUBLIC: atomic_value_t __os_atomic_add
 * PUBLIC:     __P((ENV *, db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_add(env, p, val)
	ENV *env;
	db_atomic_t *p;
	atomic_value_t val;
{
	db_mutex_t mtx;
	atomic_value_t ret;

	mtx = __os_atomic_get_mutex(env, p);
	MUTEX_LOCK(env, mtx);
	p->value += val;
	ret = p->value;
	MUTEX_UNLOCK(env, mtx);
	return (ret);
}

/*
 * __os_atomic_fetch_add --
 *	Atomically add, return the old value (mutex fallback).
 *
 * PUBLIC: #if !defined(HAVE_ATOMIC_SUPPORT) && defined(HAVE_MUTEX_SUPPORT)
 * PUBLIC: atomic_value_t __os_atomic_fetch_add
 * PUBLIC:     __P((ENV *, db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_fetch_add(env, p, val)
	ENV *env;
	db_atomic_t *p;
	atomic_value_t val;
{
	db_mutex_t mtx;
	atomic_value_t ret;

	mtx = __os_atomic_get_mutex(env, p);
	MUTEX_LOCK(env, mtx);
	ret = p->value;
	p->value += val;
	MUTEX_UNLOCK(env, mtx);
	return (ret);
}

/*
 * __os_atomic_exchange --
 *	Atomically set a new value, return the old (mutex fallback).
 *
 * PUBLIC: #if !defined(HAVE_ATOMIC_SUPPORT) && defined(HAVE_MUTEX_SUPPORT)
 * PUBLIC: atomic_value_t __os_atomic_exchange
 * PUBLIC:     __P((ENV *, db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_exchange(env, p, val)
	ENV *env;
	db_atomic_t *p;
	atomic_value_t val;
{
	db_mutex_t mtx;
	atomic_value_t ret;

	mtx = __os_atomic_get_mutex(env, p);
	MUTEX_LOCK(env, mtx);
	ret = p->value;
	p->value = val;
	MUTEX_UNLOCK(env, mtx);
	return (ret);
}

/*
 * __os_atomic_thread_fence --
 *	Memory barrier (mutex fallback -- no-op since mutexes
 *	provide their own barrier semantics).
 *
 * PUBLIC: #if !defined(HAVE_ATOMIC_SUPPORT) && defined(HAVE_MUTEX_SUPPORT)
 * PUBLIC: void __os_atomic_thread_fence __P((void));
 * PUBLIC: #endif
 */
void
__os_atomic_thread_fence()
{
	/* Mutex lock/unlock provides an implicit full barrier. */
}

#endif /* !HAVE_ATOMIC_SUPPORT && HAVE_MUTEX_SUPPORT */

/*
 * =====================================================================
 * Single-threaded, no-mutex fallback.
 *
 * When neither native atomics nor mutexes are available, provide
 * plain non-atomic operations.  Only safe for single-threaded,
 * single-process use.
 * =====================================================================
 */

#if !defined(HAVE_ATOMIC_SUPPORT) && !defined(HAVE_MUTEX_SUPPORT)

/*
 * __os_atomic_init --
 *	Initialize an atomic variable (single-threaded fallback).
 *
 * PUBLIC: #if !defined(HAVE_ATOMIC_SUPPORT) && !defined(HAVE_MUTEX_SUPPORT)
 * PUBLIC: void __os_atomic_init
 * PUBLIC:     __P((db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
void
__os_atomic_init(p, val)
	db_atomic_t *p;
	atomic_value_t val;
{
	p->value = val;
}

/*
 * __os_atomic_read --
 *	Load the current value (single-threaded fallback).
 *
 * PUBLIC: #if !defined(HAVE_ATOMIC_SUPPORT) && !defined(HAVE_MUTEX_SUPPORT)
 * PUBLIC: atomic_value_t __os_atomic_read
 * PUBLIC:     __P((const db_atomic_t *));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_read(p)
	const db_atomic_t *p;
{
	return (p->value);
}

/*
 * __os_atomic_store --
 *	Store a value (single-threaded fallback).
 *
 * PUBLIC: #if !defined(HAVE_ATOMIC_SUPPORT) && !defined(HAVE_MUTEX_SUPPORT)
 * PUBLIC: void __os_atomic_store
 * PUBLIC:     __P((db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
void
__os_atomic_store(p, val)
	db_atomic_t *p;
	atomic_value_t val;
{
	p->value = val;
}

/*
 * __os_atomic_inc --
 *	Increment by 1, return the new value (single-threaded fallback).
 *
 * PUBLIC: #if !defined(HAVE_ATOMIC_SUPPORT) && !defined(HAVE_MUTEX_SUPPORT)
 * PUBLIC: atomic_value_t __os_atomic_inc
 * PUBLIC:     __P((ENV *, db_atomic_t *));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_inc(env, p)
	ENV *env;
	db_atomic_t *p;
{
	COMPQUIET(env, NULL);
	return (++p->value);
}

/*
 * __os_atomic_dec --
 *	Decrement by 1, return the new value (single-threaded fallback).
 *
 * PUBLIC: #if !defined(HAVE_ATOMIC_SUPPORT) && !defined(HAVE_MUTEX_SUPPORT)
 * PUBLIC: atomic_value_t __os_atomic_dec
 * PUBLIC:     __P((ENV *, db_atomic_t *));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_dec(env, p)
	ENV *env;
	db_atomic_t *p;
{
	COMPQUIET(env, NULL);
	return (--p->value);
}

/*
 * __os_atomic_cas --
 *	Compare-and-swap (single-threaded fallback).
 *	Returns 1 on success, 0 on failure.
 *
 * PUBLIC: #if !defined(HAVE_ATOMIC_SUPPORT) && !defined(HAVE_MUTEX_SUPPORT)
 * PUBLIC: int __os_atomic_cas __P((ENV *,
 * PUBLIC:     db_atomic_t *, atomic_value_t, atomic_value_t));
 * PUBLIC: #endif
 */
int
__os_atomic_cas(env, p, oldval, newval)
	ENV *env;
	db_atomic_t *p;
	atomic_value_t oldval;
	atomic_value_t newval;
{
	COMPQUIET(env, NULL);
	if (p->value != oldval)
		return (0);
	p->value = newval;
	return (1);
}

/*
 * __os_atomic_add --
 *	Add a value, return the new value (single-threaded fallback).
 *
 * PUBLIC: #if !defined(HAVE_ATOMIC_SUPPORT) && !defined(HAVE_MUTEX_SUPPORT)
 * PUBLIC: atomic_value_t __os_atomic_add
 * PUBLIC:     __P((ENV *, db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_add(env, p, val)
	ENV *env;
	db_atomic_t *p;
	atomic_value_t val;
{
	COMPQUIET(env, NULL);
	p->value += val;
	return (p->value);
}

/*
 * __os_atomic_fetch_add --
 *	Add a value, return the old value (single-threaded fallback).
 *
 * PUBLIC: #if !defined(HAVE_ATOMIC_SUPPORT) && !defined(HAVE_MUTEX_SUPPORT)
 * PUBLIC: atomic_value_t __os_atomic_fetch_add
 * PUBLIC:     __P((ENV *, db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_fetch_add(env, p, val)
	ENV *env;
	db_atomic_t *p;
	atomic_value_t val;
{
	atomic_value_t old;

	COMPQUIET(env, NULL);
	old = p->value;
	p->value += val;
	return (old);
}

/*
 * __os_atomic_exchange --
 *	Set a new value, return the old (single-threaded fallback).
 *
 * PUBLIC: #if !defined(HAVE_ATOMIC_SUPPORT) && !defined(HAVE_MUTEX_SUPPORT)
 * PUBLIC: atomic_value_t __os_atomic_exchange
 * PUBLIC:     __P((ENV *, db_atomic_t *, atomic_value_t));
 * PUBLIC: #endif
 */
atomic_value_t
__os_atomic_exchange(env, p, val)
	ENV *env;
	db_atomic_t *p;
	atomic_value_t val;
{
	atomic_value_t old;

	COMPQUIET(env, NULL);
	old = p->value;
	p->value = val;
	return (old);
}

/*
 * __os_atomic_thread_fence --
 *	Memory barrier (single-threaded fallback -- no-op).
 *
 * PUBLIC: #if !defined(HAVE_ATOMIC_SUPPORT) && !defined(HAVE_MUTEX_SUPPORT)
 * PUBLIC: void __os_atomic_thread_fence __P((void));
 * PUBLIC: #endif
 */
void
__os_atomic_thread_fence()
{
}

#endif /* !HAVE_ATOMIC_SUPPORT && !HAVE_MUTEX_SUPPORT */
