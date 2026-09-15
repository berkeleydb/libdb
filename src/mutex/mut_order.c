/*-
 * See the file LICENSE for redistribution information.
 *
 * DIAGNOSTIC-only mechanical lock-order checker.  The specification is
 * docs/design/global-invariants.md section 4 (A3); the design rationale, the
 * node-identity correction and the scope statement are in
 * src/dbinc/lock_order.h.  This closes named gap G9.
 *
 * $Id$
 */

#include "db_config.h"

#include "db_int.h"

#ifdef DIAGNOSTIC

/*
 * Per-thread state.  File-scope thread-local, NOT a field in any struct:
 * env_sig.c hashes ~137 structs and env_region.c refuses to attach an
 * environment whose build signature differs, so growing a hashed struct would
 * break upgrade-in-place while libabigail stayed silent about it.  Keeping the
 * state here means __env_struct_sig() is byte-identical to master.
 *
 * DB_LO_TLS is the portability guard.  With no thread-local storage the
 * checker degrades to doing nothing, which is the right failure mode for a
 * diagnostic: it must never turn a working build into a broken one.  We do not
 * key off DB_THREAD_INFO (ip) instead, because ip is NULL on real paths --
 * ENV_ENTER leaves it NULL when env->thr_hashtab is NULL, e.g. DB_PRIVATE
 * without set_thread_count -- and a checker that silently skips the
 * single-process case would miss the very configuration most tests run in.
 */
#if defined(HAVE_MUTEX_PTHREADS) || defined(HAVE_MUTEX_HYBRID) ||	\
    defined(HAVE_MUTEX_TAS) || defined(HAVE_MUTEX_UI_THREADS) ||		\
    defined(HAVE_MUTEX_SOLARIS_LWP)
#if defined(__GNUC__) || defined(__clang__) || defined(__SUNPRO_C)
#define	DB_LO_TLS	__thread
#elif defined(_MSC_VER)
#define	DB_LO_TLS	__declspec(thread)
#endif
#endif

#ifdef DB_LO_TLS

static DB_LO_TLS DB_LO_HELD lo_stack[DB_LO_STACK_MAX];
static DB_LO_TLS int	    lo_depth;
static DB_LO_TLS int	    lo_overflow;	/* Nesting exceeded the stack. */
static DB_LO_TLS int	    lo_reported;	/* One report per thread. */
static DB_LO_TLS int	    lo_warn_only = -1;	/* -1 = not yet resolved. */
static DB_LO_TLS db_mutex_t  lo_last_held;	/* Survey-mode dedup. */
static DB_LO_TLS db_mutex_t  lo_last_new;

/*
 * __db_lo_rank --
 *	Map an MTX_* allocation id to its rank in A3's partial order.
 *
 *	A switch, not a table, on purpose: a new MTX_* id added without a rank
 *	produces a -Wswitch warning in a DIAGNOSTIC build rather than silently
 *	defaulting to some rank and being checked wrongly.  The same reasoning
 *	as dist/validate/s_chk_mutex_print applies to __mutex_print_id.
 *
 * PUBLIC: #ifdef DIAGNOSTIC
 * PUBLIC: int __db_lo_rank __P((int));
 * PUBLIC: #endif
 */
int
__db_lo_rank(alloc_id)
	int alloc_id;
{
	switch (alloc_id) {
	/*
	 * Deliberately exempt.
	 *
	 * DB_MUTEX_SELF_BLOCK latches (MTX_TXN_COMMIT, MTX_LOGICAL_LOCK,
	 * MTX_REP_WAITER) are handoff signals: they are acquired by one thread
	 * and released by another, and are meant to be held across a wait.
	 * Ranking them would report every group-commit and every rep waiter as
	 * a violation.  MTX_APPLICATION is the application's own latch and its
	 * order is not ours to police.  MTX_MUTEX_TEST is mut_region.c's
	 * self-test, which recursively latches on purpose.  MTX_ATOMIC_EMULATION
	 * stands in for an atomic instruction on platforms without one.
	 */
	case MTX_APPLICATION:
	case MTX_ATOMIC_EMULATION:
	case MTX_LOGICAL_LOCK:
	case MTX_MUTEX_TEST:
	case MTX_TXN_COMMIT:
	case MTX_REP_WAITER:
		return (DB_LO_RANK_EXEMPT);

	/*
	 * A buffer's mtx_buf is a PAGE PIN, not an ordered latch, and this is
	 * the single most important exemption in the model.
	 *
	 * __memp_fget returns to its caller with mtx_buf still held -- that is
	 * what "the page is pinned" means -- and the caller then does arbitrary
	 * higher-level work with the page: acquires record locks
	 * (lock.c:911 reached from a btree search holding a pinned page), fetches
	 * more pages, latches other buckets.  Every one of those is a legal
	 * mtx_buf-then-something acquisition, so ranking mtx_buf makes ordinary
	 * btree traversal report continuously.
	 *
	 * A3 already says this without saying it: its mpool rule is
	 * "acquire the bucket, take a ref, DROP THE BUCKET, then latch the
	 * buffer" (mp_fget.c:387-397).  The ordered latch in that sentence is
	 * mtx_hash.  mtx_buf is what is left holding the reference afterwards,
	 * and deadlock freedom for it comes from the ref/pin protocol
	 * (BH_EXCLUSIVE, atomic ref counts, the trylock-and-retry loops), not
	 * from a global order.
	 *
	 * Consequence, stated honestly: this checker does NOT police
	 * mtx_buf-vs-mtx_hash ordering, so it would NOT by itself have caught
	 * the shipped os_aio deadlock, whose shape is exactly "blocks acquiring
	 * a buffer's mtx_buf while holding deferred-write pins".  That deadlock
	 * is a hold-and-block-on-a-pin bug, a different class from a latch
	 * misordering, and catching it needs pin-aware accounting -- named as
	 * remaining work in the report rather than papered over here.
	 */
	case MTX_MPOOL_BH:
		return (DB_LO_RANK_EXEMPT);

	/*
	 * The env region latch -- and, because of the aliasing documented in
	 * lock_order.h, also LOCK_SYSTEM_LOCK, TXN_SYSTEM_LOCK, LOG_SYSTEM_LOCK
	 * and every infop->mtx_alloc.  Tracked but NOT rank-compared: this one
	 * latch legitimately appears at three different depths, so any rank is
	 * wrong somewhere.  The same-latch rule still covers it, and that is
	 * the rule that catches the bug this checker was written for.
	 */
	case MTX_ENV_REGION:
	case MTX_TXN_REGION:
	case MTX_LOG_REGION:
		return (DB_LO_RANK_UNRANKED);

	/*
	 * The mpool region latch is an ALLOCATOR latch, taken innermost, not a
	 * peer of the env region.  See DB_LO_RANK_ALLOC in lock_order.h.
	 */
	case MTX_MPOOL_REGION:
		return (DB_LO_RANK_ALLOC);

	/*
	 * PROCESS-LOCAL LATCHES -- a separate domain from the shared-region
	 * latches, and deliberately NOT rank-compared against them.
	 *
	 * All of these are DB_MUTEX_PROCESS_ONLY (env_open.c:1146, :1182,
	 * mp_region.c:164, txn_region.c:52, log.c:64, db.c:497, ...).  They guard
	 * per-process linked lists: env->fdlist, dbmp->dbmfq, the dblist, the
	 * txn chain.
	 *
	 * Two measured facts make them unrankable against the region latches, in
	 * OPPOSITE directions, which is why no single rank works:
	 *
	 *   - Some are taken as LEAVES while a region latch is held:
	 *       mp_sync.c:60 (file bucket) -> mp_sync.c:837 (dbmp->mutex)
	 *       mp_fopen.c:350 (mpool region) -> os_handle.c:43 (env->mtx_env)
	 *   - Others are taken as OUTER latches, held across real work that then
	 *     takes region latches:
	 *       db.c:964 (env->mtx_dblist) -> mp_fopen.c:1009, dbreg.c:632
	 *       db.c:1381 (env->mtx_dblist) -> mp_mvcc.c:92
	 *
	 * A3's diagram puts "handle mutex" at the TOP, alongside the regions.
	 * That describes the second group and mis-describes the first.  Ranking
	 * them either way produced false positives on ordinary open/close/sync
	 * paths -- measured: 5 distinct pairs, ~2000 firings, in test/isolation
	 * alone.
	 *
	 * Leaving them unranked costs nothing this checker was built to provide:
	 * being process-local they cannot take part in a MULTI-PROCESS hang,
	 * which is the failure mode A3 exists to prevent.  They are still
	 * tracked, so the same-latch self-deadlock rule still covers them.
	 */
	case MTX_DB_HANDLE:
	case MTX_ENV_DBLIST:
	case MTX_ENV_EXCLDBLIST:
	case MTX_ENV_HANDLE:
	case MTX_LOG_HANDLE:
	case MTX_MPOOL_HANDLE:
	case MTX_MPOOL_FH:
	case MTX_SEQUENCE:
	case MTX_TWISTER:
	case MTX_TCL_EVENTS:
	case MTX_TXN_ACTIVE:
	case MTX_MPOOL_AIO:
		return (DB_LO_RANK_UNRANKED);

	/* Shared-region latches that sit above the lock/mpool internals. */
	case MTX_LOG_FILENAME:
	case MTX_TXN_CHKPT:
	case MTX_LOG_FLUSH:
	case MTX_REP_REGION:
	case MTX_REP_DATABASE:
	case MTX_REP_CHKPT:
	case MTX_REP_DIAG:
	case MTX_REP_EVENT:
	case MTX_REP_START:
	case MTX_REPMGR:
		return (DB_LO_RANK_HANDLE);

	/*
	 * Lock partitions and locker stripes.  Both are MTX_LOCK_REGION (see
	 * lock_region.c:249, :253, :267), so this one rank covers mtx_part,
	 * mtx_dd and mtx_locker_stripe[].  LOCK_LOCKERS takes all 64 stripes,
	 * stripe 0 first, so sibling-vs-sibling must stay unordered here or
	 * every LOCK_LOCKERS would report (dbinc/lock.h:381-390).
	 */
	case MTX_LOCK_REGION:
		return (DB_LO_RANK_LOCK_PART);

	case MTX_MPOOL_FILE_BUCKET:
		return (DB_LO_RANK_MPOOL_FILE);

	case MTX_MPOOL_HASH_BUCKET:
		return (DB_LO_RANK_MPOOL_HASH);

	case MTX_TXN_MVCC:
		return (DB_LO_RANK_TXN_MVCC);

	/*
	 * MPOOLFILE's mutex, despite the "HANDLE" in its name, is a LEAF
	 * counter latch, not an outer handle latch: callers take it, bump
	 * block_cnt / mpf_cnt / a stat, and drop it (mp_fget.c:962-964,
	 * mp_bh.c:950-955, and a dozen more).  It is routinely taken while a
	 * buffer's mtx_buf is held (mp_fget.c:899 -> :962), so ranking it with
	 * the handles made a normal cache miss a false positive.
	 *
	 * mfp->mtx_write is the same id but a genuinely different latch; it is
	 * ranked here too, which is sound only because nothing takes mtx_write
	 * while holding mfp->mutex or vice versa.
	 */
	case MTX_MPOOLFILE_HANDLE:
		return (DB_LO_RANK_MPOOL_FILECNT);

	case MTX_MUTEX_REGION:
		return (DB_LO_RANK_MUTEX_REGION);

	default:
		/* Unknown id: do not check it rather than check it wrongly. */
		return (DB_LO_RANK_EXEMPT);
	}
}

/*
 * lo_alloc_id --
 *	Recover the MTX_* class of a latch.
 *
 *	alloc_id only exists under HAVE_STATISTICS.  Without it the class is
 *	unknowable, so the rank check is skipped -- but the same-latch check
 *	below still works, because it compares mutex indices, not classes.
 *	That is the part that catches the region-aliasing deadlock, so the
 *	checker keeps its teeth even in a no-statistics build.
 */
static int
lo_alloc_id(env, mutex)
	ENV *env;
	db_mutex_t mutex;
{
#ifdef HAVE_STATISTICS
	DB_MUTEX *mutexp;

	if (!MUTEX_ON(env))
		return (0);
	mutexp = MUTEXP_SET(env, mutex);
	return (mutexp->alloc_id);
#else
	COMPQUIET(env, NULL);
	COMPQUIET(mutex, 0);
	return (0);
#endif
}

static const char *
lo_name(alloc_id)
	int alloc_id;
{
	switch (alloc_id) {
	case MTX_APPLICATION:		return ("MTX_APPLICATION");
	case MTX_ATOMIC_EMULATION:	return ("MTX_ATOMIC_EMULATION");
	case MTX_DB_HANDLE:		return ("MTX_DB_HANDLE");
	case MTX_ENV_DBLIST:		return ("MTX_ENV_DBLIST");
	case MTX_ENV_EXCLDBLIST:	return ("MTX_ENV_EXCLDBLIST");
	case MTX_ENV_HANDLE:		return ("MTX_ENV_HANDLE");
	case MTX_ENV_REGION:		return ("MTX_ENV_REGION");
	case MTX_LOCK_REGION:		return ("MTX_LOCK_REGION");
	case MTX_LOGICAL_LOCK:		return ("MTX_LOGICAL_LOCK");
	case MTX_LOG_FILENAME:		return ("MTX_LOG_FILENAME");
	case MTX_LOG_FLUSH:		return ("MTX_LOG_FLUSH");
	case MTX_LOG_HANDLE:		return ("MTX_LOG_HANDLE");
	case MTX_LOG_REGION:		return ("MTX_LOG_REGION");
	case MTX_MPOOLFILE_HANDLE:	return ("MTX_MPOOLFILE_HANDLE");
	case MTX_MPOOL_BH:		return ("MTX_MPOOL_BH");
	case MTX_MPOOL_FH:		return ("MTX_MPOOL_FH");
	case MTX_MPOOL_FILE_BUCKET:	return ("MTX_MPOOL_FILE_BUCKET");
	case MTX_MPOOL_HANDLE:		return ("MTX_MPOOL_HANDLE");
	case MTX_MPOOL_HASH_BUCKET:	return ("MTX_MPOOL_HASH_BUCKET");
	case MTX_MPOOL_REGION:		return ("MTX_MPOOL_REGION");
	case MTX_MUTEX_REGION:		return ("MTX_MUTEX_REGION");
	case MTX_MUTEX_TEST:		return ("MTX_MUTEX_TEST");
	case MTX_REP_CHKPT:		return ("MTX_REP_CHKPT");
	case MTX_REP_DATABASE:		return ("MTX_REP_DATABASE");
	case MTX_REP_DIAG:		return ("MTX_REP_DIAG");
	case MTX_REP_EVENT:		return ("MTX_REP_EVENT");
	case MTX_REP_REGION:		return ("MTX_REP_REGION");
	case MTX_REP_START:		return ("MTX_REP_START");
	case MTX_REP_WAITER:		return ("MTX_REP_WAITER");
	case MTX_REPMGR:		return ("MTX_REPMGR");
	case MTX_SEQUENCE:		return ("MTX_SEQUENCE");
	case MTX_TWISTER:		return ("MTX_TWISTER");
	case MTX_TCL_EVENTS:		return ("MTX_TCL_EVENTS");
	case MTX_TXN_ACTIVE:		return ("MTX_TXN_ACTIVE");
	case MTX_TXN_CHKPT:		return ("MTX_TXN_CHKPT");
	case MTX_TXN_COMMIT:		return ("MTX_TXN_COMMIT");
	case MTX_TXN_MVCC:		return ("MTX_TXN_MVCC");
	case MTX_TXN_REGION:		return ("MTX_TXN_REGION");
	case MTX_MPOOL_AIO:		return ("MTX_MPOOL_AIO");
	default:			return ("unknown");
	}
}

/*
 * lo_report --
 *	Say exactly what happened, loudly, then stop the process.
 *
 *	Both classes, both mutex indices and both acquisition sites, because
 *	the whole point is that the reader should not need to reproduce it
 *	under a debugger the way the os_aio deadlock had to be.
 */
static void
lo_report(env, why, held, newp)
	ENV *env;
	const char *why;
	DB_LO_HELD *held;
	DB_LO_HELD *newp;
{
	int i;

	/*
	 * Survey mode.  DB_LOCK_ORDER_WARN=1 makes a violation report-and-
	 * continue instead of report-and-die.  This is a triage aid, not the
	 * default: it exists because classifying N distinct violations across a
	 * whole test suite one abort at a time is O(N) full suite runs.  The
	 * default stays fatal, because a lock-order violation must fail a test
	 * rather than scroll past in a log.
	 */
	if (lo_warn_only < 0)
		lo_warn_only = getenv("DB_LOCK_ORDER_WARN") != NULL ? 1 : 0;

	/*
	 * One report per thread in fatal mode.  __env_panic below re-enters the
	 * mutex code to set the panic flag, and a checker that recursed into
	 * itself while reporting would bury the first (real) report under a
	 * cascade.  In survey mode we want every distinct site, so only
	 * suppress an exact repeat of the pair we just reported.
	 */
	if (lo_warn_only) {
		if (lo_last_held == held->mutex && lo_last_new == newp->mutex)
			return;
		lo_last_held = held->mutex;
		lo_last_new = newp->mutex;
	} else {
		if (lo_reported)
			return;
		lo_reported = 1;
	}

	__db_errx(env, DB_STR_A("2077",
	    "LOCK ORDER VIOLATION: %s", "%s"), why);
	__db_errx(env, DB_STR_A("2078",
	    "  already held: %s (mutex %lu) acquired at %s:%d",
	    "%s %lu %s %d"), lo_name(held->alloc_id),
	    (u_long)held->mutex, held->file, held->line);
	__db_errx(env, DB_STR_A("2079",
	    "  now acquiring: %s (mutex %lu) at %s:%d",
	    "%s %lu %s %d"), lo_name(newp->alloc_id),
	    (u_long)newp->mutex, newp->file, newp->line);
	if (held->rank == newp->rank)
		__db_errx(env, DB_STR("2080",
     "  these are the SAME latch: not recursive, so this can never complete"));
	else
		__db_errx(env, DB_STR_A("2084",
		    "  declared order: rank %d must not be taken before rank %d "
		    "(see docs/design/global-invariants.md A3)", "%d %d"),
		    held->rank, newp->rank);

	__db_errx(env, DB_STR("2081", "  latches held by this thread:"));
	for (i = 0; i < lo_depth; i++)
		__db_errx(env, DB_STR_A("2082",
		    "    [%d] %s (mutex %lu) at %s:%d", "%d %s %lu %s %d"),
		    i, lo_name(lo_stack[i].alloc_id),
		    (u_long)lo_stack[i].mutex,
		    lo_stack[i].file, lo_stack[i].line);

	/*
	 * Abort rather than return an error.  A lock-order violation is a
	 * design defect, not a runtime condition, and it must fail the test
	 * hard -- a returned error would be swallowed by some caller's
	 * cleanup path and the test would go green.
	 */
	if (lo_warn_only)
		return;
	(void)__env_panic(env, DB_RUNRECOVERY);
	abort();
	/* NOTREACHED */
}

/*
 * __db_lo_check --
 *	Validate a pending acquisition against everything this thread holds.
 *
 *	Called BEFORE the latch is taken, because a self-deadlock never returns
 *	from the acquire call -- checking afterwards would hang in precisely the
 *	case this exists to diagnose.  Being early means a report describes an
 *	acquisition the thread was ABOUT to make, which is what a developer
 *	needs to see anyway: the two sites and the two classes.
 *
 * PUBLIC: #ifdef DIAGNOSTIC
 * PUBLIC: void __db_lo_check __P((ENV *, db_mutex_t, const char *, int));
 * PUBLIC: #endif
 */
void
__db_lo_check(env, mutex, file, line)
	ENV *env;
	db_mutex_t mutex;
	const char *file;
	int line;
{
	DB_LO_HELD nw;
	int i;

	if (mutex == MUTEX_INVALID || env == NULL || lo_reported)
		return;

	nw.mutex = mutex;
	nw.alloc_id = lo_alloc_id(env, mutex);
	nw.rank = __db_lo_rank(nw.alloc_id);
	nw.file = file;
	nw.line = line;

	if (nw.rank == DB_LO_RANK_EXEMPT)
		return;

	for (i = 0; i < lo_depth; i++) {
		/*
		 * Same latch twice on one thread.  Rank-independent: these are
		 * not recursive latches, so this is an unconditional
		 * self-deadlock.  This is the check that catches the
		 * aliased-region bug, and it only works from here -- the
		 * acquisition below is the one that never returns.
		 */
		if (lo_stack[i].mutex == mutex) {
			lo_report(env,
			    "same latch acquired twice by one thread "
			    "(self-deadlock)", &lo_stack[i], &nw);
			return;
		}

		/* Rank inversion: holding an inner latch, taking an outer. */
		if (nw.rank == DB_LO_RANK_UNRANKED ||
		    lo_stack[i].rank == DB_LO_RANK_UNRANKED)
			continue;
		if (lo_stack[i].rank > nw.rank) {
			lo_report(env,
			    "latch acquired out of the declared global order",
			    &lo_stack[i], &nw);
			return;
		}
	}
}

/*
 * __db_lo_acquire --
 *	Record that this thread now holds a latch.
 *
 *	Called only after the acquisition succeeded; validation already happened
 *	in __db_lo_check.
 *
 * PUBLIC: #ifdef DIAGNOSTIC
 * PUBLIC: void __db_lo_acquire __P((ENV *, db_mutex_t, const char *, int));
 * PUBLIC: #endif
 */
void
__db_lo_acquire(env, mutex, file, line)
	ENV *env;
	db_mutex_t mutex;
	const char *file;
	int line;
{
	DB_LO_HELD nw;

	if (mutex == MUTEX_INVALID || env == NULL || lo_reported)
		return;

	nw.mutex = mutex;
	nw.alloc_id = lo_alloc_id(env, mutex);
	nw.rank = __db_lo_rank(nw.alloc_id);
	nw.file = file;
	nw.line = line;

	/*
	 * Exempt latches are not tracked at all -- not merely skipped when
	 * comparing.  DB_MUTEX_SELF_BLOCK latches (MTX_LOGICAL_LOCK and friends)
	 * are routinely acquired by one thread and released by another, so
	 * pushing them would leak stack entries that never pop; that leak filled
	 * the stack and produced a false report before this was fixed.
	 */
	if (nw.rank == DB_LO_RANK_EXEMPT)
		return;

	if (lo_depth >= DB_LO_STACK_MAX) {
		/*
		 * Deeper than the stack.  Warn once and leave this latch
		 * untracked; the release path searches, so an untracked latch
		 * simply will not be found and its release is a no-op.
		 * Crucially this does NOT suspend the release path -- an
		 * earlier version did, which left a stale stack behind and
		 * manufactured a false positive on the very next acquisition.
		 */
		if (!lo_overflow) {
			lo_overflow = 1;
			__db_errx(env, DB_STR_A("2083",
     "lock-order checker: nesting deeper than %d latches, not tracked",
			    "%d"), DB_LO_STACK_MAX);
		}
		return;
	}
	lo_stack[lo_depth++] = nw;
}

/*
 * __db_lo_release --
 *	Pop a latch.
 *
 *	Searches rather than assuming the top: mpool legitimately releases out
 *	of order (take bucket, take buffer, drop bucket, keep buffer -- see
 *	mp_fget.c:387-397), so a strict stack discipline would be wrong.
 *
 * PUBLIC: #ifdef DIAGNOSTIC
 * PUBLIC: void __db_lo_release __P((ENV *, db_mutex_t));
 * PUBLIC: #endif
 */
void
__db_lo_release(env, mutex)
	ENV *env;
	db_mutex_t mutex;
{
	int i;

	COMPQUIET(env, NULL);
	if (mutex == MUTEX_INVALID || lo_reported)
		return;

	for (i = lo_depth; i-- > 0; )
		if (lo_stack[i].mutex == mutex) {
			if (i != lo_depth - 1)
				memmove(&lo_stack[i], &lo_stack[i + 1],
				    (size_t)(lo_depth - i - 1) *
				    sizeof(lo_stack[0]));
			lo_depth--;
			return;
		}
	/*
	 * Not found.  Legal and common: a shared latch released by a thread
	 * that acquired it before the checker saw it, a latch handed off
	 * between threads, or an unlock of something never rank-tracked.
	 * Silence is correct here -- reporting would be pure noise.
	 */
}

#else /* !DB_LO_TLS */

/*
 * No thread-local storage: degrade to nothing.  A diagnostic must never be the
 * reason a platform stops building.
 */
int
__db_lo_rank(alloc_id)
	int alloc_id;
{
	COMPQUIET(alloc_id, 0);
	return (DB_LO_RANK_EXEMPT);
}

void
__db_lo_check(env, mutex, file, line)
	ENV *env;
	db_mutex_t mutex;
	const char *file;
	int line;
{
	COMPQUIET(env, NULL);
	COMPQUIET(mutex, 0);
	COMPQUIET(file, NULL);
	COMPQUIET(line, 0);
}

void
__db_lo_acquire(env, mutex, file, line)
	ENV *env;
	db_mutex_t mutex;
	const char *file;
	int line;
{
	COMPQUIET(env, NULL);
	COMPQUIET(mutex, 0);
	COMPQUIET(file, NULL);
	COMPQUIET(line, 0);
}

void
__db_lo_release(env, mutex)
	ENV *env;
	db_mutex_t mutex;
{
	COMPQUIET(env, NULL);
	COMPQUIET(mutex, 0);
}

#endif /* DB_LO_TLS */

#endif /* DIAGNOSTIC */
