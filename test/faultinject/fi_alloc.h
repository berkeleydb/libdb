/*-
 * SQLite-style malloc-failure injection for libdb.
 *
 * fi_alloc.h --
 *	The allocation fault-injection hook the __os_malloc/__os_calloc/
 *	__os_realloc/__os_umalloc/__os_urealloc seam calls.  Declares one
 *	predicate, __db_fi_fail(), plus the runtime knobs a test uses to
 *	sweep the failing allocation over a workload.
 *
 *	SQLite's most powerful correctness technique: fail the Nth memory
 *	allocation, sweep N across a workload, and assert at EVERY failure
 *	point that the library returns a clean error, leaks nothing, holds
 *	no lock and leaves no corruption.  BDB routes essentially all
 *	allocation through the __os_* seam, so a single injection point
 *	covers the library.
 *
 *	Compilation model (mirrors the DST hooks in test/sim/):
 *	  - HAVE_FAULT_INJECT OFF (production / default): this header is
 *	    never included and os_alloc.c's hook calls vanish, so a stock
 *	    build is bit-for-bit unchanged and zero-overhead.
 *	  - HAVE_FAULT_INJECT ON (--enable-faultinject): fi_alloc.c is
 *	    linked into the library.  Every allocation first consults
 *	    __db_fi_fail(); when injection is inactive (the default even in
 *	    a faultinject build) that is a single relaxed load returning 0,
 *	    so an --enable-faultinject library not running a sweep behaves
 *	    like stock.
 *
 *	Runtime control (so a test can sweep K without rebuilding):
 *	  - Environment: DB_FI_FAIL_AT=<K> arms "fail the Kth allocation"
 *	    at process start (read lazily on first allocation).  K<=0 means
 *	    inactive.  DB_FI_VERBOSE=1 traces each fired failure to stderr.
 *	  - API: __db_fi_arm(K)/__db_fi_disarm()/__db_fi_count()/
 *	    __db_fi_fired() let a driver sweep in-process.
 */

#ifndef _DB_FI_ALLOC_H_
#define _DB_FI_ALLOC_H_

#if defined(__cplusplus)
extern "C" {
#endif

/*
 * __db_fi_fail --
 *	Consulted once per allocation.  Returns non-zero (inject an OOM)
 *	when injection is armed and this is the allocation that should
 *	fail; else 0.  Also bumps the total-allocation counter used to
 *	measure the workload's baseline M.
 */
int __db_fi_fail __P((void));

/* Arm "fail the Kth allocation from now" (K>=1).  Resets counters. */
void __db_fi_arm __P((long));

/* Disarm injection; allocations always succeed.  Counter keeps counting. */
void __db_fi_disarm __P((void));

/* Reset the allocation counter to 0 (call before a measured workload). */
void __db_fi_reset __P((void));

/* Total successful+injected allocations seen since the last reset/arm. */
long __db_fi_count __P((void));

/* Non-zero once the armed failure has actually fired this run. */
int __db_fi_fired __P((void));

#if defined(__cplusplus)
}
#endif

#endif /* !_DB_FI_ALLOC_H_ */
