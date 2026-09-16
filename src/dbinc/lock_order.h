/*
 * See the file LICENSE for redistribution information.
 *
 * DIAGNOSTIC-only mechanical lock-order checker.  See docs/design/
 * global-invariants.md section 4 (A3, "The global lock order") -- this file is
 * the machine-readable form of that partial order, and gap G9 is what it
 * closes.
 *
 * $Id$
 */

#ifndef _DB_LOCK_ORDER_H_
#define	_DB_LOCK_ORDER_H_

#if defined(__cplusplus)
extern "C" {
#endif

#ifdef DIAGNOSTIC

/*
 * WHY THIS EXISTS
 *
 * A3 is a partial order over latch classes.  Nothing enforced it.  A violation
 * on a cold path does not fail a test; it produces an intermittent
 * multi-process hang, which is the worst failure mode this code base has.  The
 * shipped os_aio deadlock (v2026.09.5 release notes) is exactly this class and
 * was found by hand with `bt full` after it manifested.
 *
 * WHAT NODE IDENTITY IS, AND WHY IT IS NOT JUST THE MTX_* ID
 *
 * The obvious model -- one rank per MTX_* alloc id -- CANNOT express A3, and
 * discovering that is the main correction this checker makes to the note.
 * The lock, txn and log "regions" are not three latches.  All three share the
 * env region latch, explicitly and by design:
 *
 *	lock_region.c:179  region->mtx_region = renv->mtx_regenv;
 *	txn_region.c:118   region->mtx_region = renv->mtx_regenv;
 *	log.c:224          lp->mtx_region     = renv->mtx_regenv;
 *
 * each with the comment "We share the region so we need the same mutex", and
 * mtx_regenv is allocated once as MTX_ENV_REGION (env_region.c:713).  Measured
 * on a live environment, LOCK_SYSTEM_LOCK, TXN_SYSTEM_LOCK and LOG_SYSTEM_LOCK
 * are all mutex index 2; mpool's region latch is a genuinely different one.
 *
 * So a single alloc id would have to occupy several ranks at once, and A3's
 * drawn edges between "lock region", "txn region" and "log region" are edges
 * from a node to itself.  This checker therefore does two independent things:
 *
 *   1. RANK CHECK -- rank per alloc id, for the edges that really are between
 *      distinct latches (partition -> buffer, bucket -> buffer, anything ->
 *      mutex region, ...).
 *   2. SAME-LATCH CHECK -- the same db_mutex_t index acquired twice by one
 *      thread, non-recursively.  This is rank-independent, and it is the rule
 *      that catches the aliased-region bug: with lk_partitions == 1,
 *      LOCK_SYSTEM_LOCK is live (dbinc/lock.h:340) and __lock_get_internal's
 *      SSI branch then takes TXN_SYSTEM_LOCK -- the same latch -- at
 *      lock.c:1119.  One thread, blocked on a latch it already holds.
 *
 * SCOPE -- WHAT IS AND IS NOT COVERED
 *
 * Covered: every acquisition that goes through __mutex_lock / __mutex_rdlock /
 * __mutex_trylock / __mutex_tryrdlock, which is every acquisition in the
 * library, because MUTEX_LOCK and friends are the only way to take a
 * db_mutex_t.  The hook is at that layer (not at the MUTEX_* macros) so that
 * one pair of hooks catches all five macros plus the direct __mutex_*
 * call sites in mut_region.c and mut_method.c.
 *
 * NOT covered: latches whose whole purpose is to block (DB_MUTEX_SELF_BLOCK --
 * MTX_TXN_COMMIT, MTX_LOGICAL_LOCK, MTX_REP_WAITER), which are handoff
 * signals rather than ordered latches and are deliberately held across waits
 * by a different thread than the one that releases them.  Also not covered:
 * MTX_APPLICATION (the application's own order is not ours to police) and
 * MTX_MUTEX_TEST (mut_region.c's self-test takes a latch recursively on
 * purpose).  Those are RANK_EXEMPT below.
 *
 * COST
 *
 * Outside --enable-diagnostic this whole file compiles to nothing: the hook
 * macros become do { } while (0), verified with cc -E.  No struct in
 * env_sig.c's hash list grows, so __env_struct_sig() is byte-identical to
 * master and existing environments still attach.  State is file-scope
 * thread-local, never region or handle state, for exactly that reason.
 */

/*
 * Ranks.  Lower acquires before higher.  Equal ranks are unordered with
 * respect to each other and are only checked for self-deadlock; that is
 * deliberate, because A3 is a partial order, not a total one, and inventing
 * an order between siblings would manufacture false positives.
 *
 * The ranks follow A3's diagram top to bottom.  Gaps are intentional: they
 * leave room to insert a class without renumbering (renumbering is how a
 * checker like this silently changes meaning).
 */
#define	DB_LO_RANK_EXEMPT	0	/* Not tracked or checked at all. */
/*
 * Tracked for the same-latch (self-deadlock) rule, but deliberately NOT
 * rank-compared.  This exists for one latch: the aliased env region latch.
 *
 * mtx_regenv wears three hats at three different depths, and no single rank can
 * be right for all of them:
 *   - as infop->mtx_alloc it is an ALLOCATOR latch, taken innermost
 *     (env_open.c:1122, and __env_alloc asserts it at env_alloc.c:219);
 *   - as LOCK_SYSTEM_LOCK it is outer, wrapping __lock_get_internal;
 *   - as TXN_SYSTEM_LOCK / LOG_SYSTEM_LOCK it sits in the middle, e.g. taken
 *     while mtx_filelist is held (dbreg.c:283 -> log_put.c:174) and while a
 *     lock partition is held (lock.c:1119).
 * Ranking it produced a false positive for every one of those legal paths.
 *
 * This is not a loss of teeth: the bug this checker was written to catch --
 * the same aliased latch taken twice on one thread -- is caught by the
 * same-latch rule, which needs no rank.  What is given up is only the ability
 * to police ordering BETWEEN the env region and other classes, which A3 states
 * but which the aliasing makes unstateable as a rank.
 */
#define	DB_LO_RANK_UNRANKED	1
#define	DB_LO_RANK_HANDLE	20	/* shared-region latches above lock/mpool. */
#define	DB_LO_RANK_LOCK_PART	30	/* lock partition, locker stripes. */
#define	DB_LO_RANK_MPOOL_FILE	30	/* mpool file bucket -- sibling. */
#define	DB_LO_RANK_MPOOL_HASH	40	/* mpool bucket mtx_hash. */
/*
 * NOTE: there is deliberately no rank for a buffer's mtx_buf.  It is a page
 * PIN, held across arbitrary caller work, not an ordered latch -- see the
 * MTX_MPOOL_BH case in mut_order.c for the full argument and for what this
 * costs in coverage.
 */
#define	DB_LO_RANK_TXN_MVCC	60	/* td->mvcc_mtx. */
#define	DB_LO_RANK_MPOOL_FILECNT 65	/* mfp->mutex: leaf counter latch. */
/*
 * Process-local latches (DB_MUTEX_PROCESS_ONLY) are tracked but NOT ranked --
 * they are taken as leaves on some paths and as outer latches on others, and
 * being process-local they cannot cause a multi-process hang.  See the
 * MTX_ENV_DBLIST case in mut_order.c for the measured evidence.
 */
/*
 * Allocator latches, innermost.  A3 says "anything -> mutex region" for
 * MUTEX_SYSTEM_LOCK; the mpool region latch belongs in the same tier for the
 * same reason, and A3 does not say so -- a correction this checker forced.
 * MPOOL_REGION_LOCK guards __env_alloc/__memp_free for the mpool region (it IS
 * infop->mtx_alloc, mp_region.c:160, :361), and every caller takes it innermost
 * and drops it immediately: while holding mtx_hash (mp_alloc.c:725), while
 * holding a file bucket (mp_method.c:800), and after dropping mtx_buf
 * (mp_fget.c:437, mp_alloc.c:629, mp_bh.c:933).  Ranking it as a peer of the
 * env region made every one of those a false positive.
 */
#define	DB_LO_RANK_ALLOC	70	/* mpool region: allocation only. */
#define	DB_LO_RANK_MUTEX_REGION	80	/* MUTEX_SYSTEM_LOCK, innermost. */

/*
 * The per-thread held-latch stack.  Fixed size: if a path nests deeper than
 * this the checker says so rather than growing, because an unbounded
 * diagnostic allocation on a latch path would itself change the timing it is
 * trying to observe.
 *
 * Sized above LOCK_LOCKER_STRIPES (64, dbinc/lock.h:44), because LOCK_LOCKERS
 * legitimately holds ALL 64 locker stripes at once (dbinc/lock.h:400-404).
 * A smaller stack overflows on the first __lock_getlocker and the checker then
 * reports nonsense -- that is not hypothetical, it was the first false positive
 * this checker produced.
 */
#define	DB_LO_STACK_MAX	128

typedef struct __db_lo_held {
	db_mutex_t	 mutex;		/* Which latch (region-relative). */
	int		 alloc_id;	/* MTX_* class. */
	int		 rank;		/* Cached rank of alloc_id. */
	const char	*file;		/* Acquisition site. */
	int		 line;
} DB_LO_HELD;

/*
 * The hooks.  __db_lo_acquire is called only AFTER the latch is actually held,
 * so a violation report names a state the thread really reached; calling it
 * before would report orders that never happened when a trylock fails.
 *
 * dbinc_auto/mutex_ext.h also declares these (dist/s_include generates it from
 * the PUBLIC: comments in mut_order.c), but it is included at the very BOTTOM
 * of dbinc/mutex.h -- after the __db_lo_wrap_acq() inline helper that CALLS
 * __db_lo_acquire().  An implicit declaration there is a hard error on clang
 * (-Wimplicit-function-declaration is an error since clang 16 / C99), and the
 * later real prototype then also collides: that is exactly what broke every
 * `--enable-diagnostic` clang build (and macOS, where cc is clang under both
 * compiler names) while gcc merely warned.  So declare them here, where they
 * are first used.  Keep these signatures identical to mutex_ext.h.
 */
int __db_lo_rank __P((int));
void __db_lo_check __P((ENV *, db_mutex_t, const char *, int));
void __db_lo_acquire __P((ENV *, db_mutex_t, const char *, int));
void __db_lo_release __P((ENV *, db_mutex_t));

#define	DB_LO_ACQUIRED(env, mutex, file, line)				\
	__db_lo_acquire(env, mutex, file, line)
#define	DB_LO_RELEASED(env, mutex)					\
	__db_lo_release(env, mutex)

#else /* !DIAGNOSTIC */

/*
 * Production: nothing.  NOP_STATEMENT keeps if-then-else blocks correct, the
 * same precedent the SSI crash hook in txn.c uses.
 */
#define	DB_LO_ACQUIRED(env, mutex, file, line)	NOP_STATEMENT
#define	DB_LO_RELEASED(env, mutex)		NOP_STATEMENT

#endif /* DIAGNOSTIC */

#if defined(__cplusplus)
}
#endif
#endif /* !_DB_LOCK_ORDER_H_ */
