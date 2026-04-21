/*
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2009, 2013 Oracle and/or its affiliates.  All rights reserved.
 *
 * $Id$
 */

#ifndef _DB_ATOMIC_H_
#define	_DB_ATOMIC_H_

#if defined(__cplusplus)
extern "C" {
#endif

/*
 *	Atomic operation support for Oracle Berkeley DB
 *
 * HAVE_ATOMIC_SUPPORT configures whether to use hardware atomic
 * instructions or OS-provided atomic APIs.  When defined, the
 * __os_atomic_*() functions in os_atomic.c use the best available
 * backend for the target platform:
 *
 *   Tier 1: GCC/Clang __atomic_* builtins (C11-compatible)
 *   Tier 2: Legacy GCC __sync_* builtins (GCC 4.1+)
 *   Tier 3: OS-specific APIs (Windows Interlocked*, Solaris atomic.h)
 *   Tier 4: Mutex-based emulation (when HAVE_MUTEX_SUPPORT defined)
 *   Tier 5: Non-atomic fallback (single-threaded only)
 *
 * All operations are declared in dbinc_auto/os_ext.h, auto-generated
 * by dist/s_include from PUBLIC comments in src/os/os_atomic.c.
 *
 * The API is split into two groups based on ENV requirements:
 *
 * Query/init operations (no ENV parameter):
 *   These are safe without mutex fallback because aligned 32-bit
 *   reads are hardware-atomic, and init occurs in single-threaded
 *   context before the value is shared.
 *
 *   __os_atomic_read(p)
 *	Atomically reads and returns the current value.
 *
 *   __os_atomic_init(p, val)
 *	Sets the initial value (single-threaded context only).
 *
 *   __os_atomic_store(p, val)
 *	Atomically sets the value (volatile write).
 *
 * Modification operations (ENV * parameter required):
 *   These may need mutex-based fallback on platforms without native
 *   atomic instructions, which requires access to the ENV handle
 *   for mutex region lookup.
 *
 *   __os_atomic_inc(env, p)
 *	Adds 1 to the db_atomic_t value, returning the new value.
 *
 *   __os_atomic_dec(env, p)
 *	Subtracts 1 from the db_atomic_t value, returning the new value.
 *
 *   __os_atomic_cas(env, p, oldval, newval)
 *	If the db_atomic_t's value is still oldval, set it to newval.
 *	Returns 1 for success or 0 for failure.
 *
 *   __os_atomic_add(env, p, val)
 *	Adds val to the db_atomic_t value, returning the new value.
 */

/*
 * Atomic value type: 32-bit signed integer on all platforms.
 * Windows historically used DWORD but we normalize for portability
 * and to avoid signed/unsigned mismatches with the Interlocked APIs.
 * Use Berkeley DB's standard signed 32-bit type.
 */
#if defined(DB_WIN32)
typedef DWORD	atomic_value_t;
#else
typedef signed int atomic_value_t;
#endif

/*
 * Windows CE has strange issues using the Interlocked APIs with variables
 * stored in shared memory.  It seems like the page needs to have been written
 * prior to the API working as expected.  Work around this by allocating an
 * additional 32-bit value that can be harmlessly written for each value
 * used in Interlocked instructions.
 */
#if defined(DB_WINCE)
typedef struct {
	volatile atomic_value_t value;
	volatile atomic_value_t dummy;
} db_atomic_t;
#else
typedef struct {
	volatile atomic_value_t value;
} db_atomic_t;
#endif

/*
 * Memory ordering constants for future use with relaxed/acquire/release
 * semantics.  Currently all operations use sequential consistency
 * (DB_ATOMIC_SEQ_CST) which is the safest default.
 */
#define	DB_ATOMIC_RELAXED	0
#define	DB_ATOMIC_ACQUIRE	1
#define	DB_ATOMIC_RELEASE	2
#define	DB_ATOMIC_SEQ_CST	3

/*
 * Compatibility macros: map the legacy atomic_xxx() call sites to the
 * new __os_atomic_*() functions.  These avoid the C11 <stdatomic.h>
 * naming conflict (atomic_init, atomic_load, etc.) by providing
 * BDB-prefixed names.
 *
 * Query/init operations delegate to ENV-free functions.
 * Modification operations delegate to ENV-aware functions.
 */
#define	atomic_read(p)		__os_atomic_read(p)
#define	atomic_init(p, val)	__os_atomic_init((p), (val))

#ifdef HAVE_ATOMIC_SUPPORT

#define	atomic_inc(env, p)	\
	__os_atomic_inc((env), (p))
#define	atomic_dec(env, p)	\
	__os_atomic_dec((env), (p))
#define	atomic_compare_exchange(env, p, oldval, newval)	\
	__os_atomic_cas((env), (p), (oldval), (newval))

#else
/*
 * No native hardware support for atomic increment, decrement, and
 * compare-exchange.  Emulate them when mutexes are supported;
 * do them without concern for atomicity when no mutexes.
 */
#ifndef HAVE_MUTEX_SUPPORT
/*
 * These minimal versions are correct to use only for single-threaded,
 * single-process environments.
 */
#define	atomic_inc(env, p)	(++(p)->value)
#define	atomic_dec(env, p)	(--(p)->value)
#define	atomic_compare_exchange(env, p, oldval, newval)		\
	(DB_ASSERT(env, atomic_read(p) == (oldval)),		\
	atomic_init(p, (newval)), 1)
#else
#define atomic_inc(env, p)	__atomic_inc(env, p)
#define atomic_dec(env, p)	__atomic_dec(env, p)
#endif
#endif

#if defined(__cplusplus)
}
#endif

#endif /* !_DB_ATOMIC_H_ */
