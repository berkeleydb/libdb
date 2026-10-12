/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 2013 Oracle and/or its affiliates.  All rights reserved.
 *
 * $Id$
 */

#include "db_config.h"

#include "db_int.h"
#include "dbinc/log.h"
#include "dbinc/txn.h"

static int __txn_init __P((ENV *, DB_TXNMGR *));

/*
 * __txn_open --
 *	Open a transaction region.
 *
 * PUBLIC: int __txn_open __P((ENV *));
 */
int
__txn_open(env)
	ENV *env;
{
	DB_TXNMGR *mgr;
	DB_TXNREGION *region;
	int ret;

	/* Create/initialize the transaction manager structure. */
	if ((ret = __os_calloc(env, 1, sizeof(DB_TXNMGR), &mgr)) != 0)
		return (ret);
	TAILQ_INIT(&mgr->txn_chain);
	mgr->env = env;

	/* Join/create the txn region. */
	if ((ret = __env_region_share(env, &mgr->reginfo)) != 0)
		goto err;

	/* If we created the region, initialize it. */
	if (F_ISSET(&mgr->reginfo, REGION_CREATE))
		if ((ret = __txn_init(env, mgr)) != 0)
			goto err;

	/* Set the local addresses. */
	region = mgr->reginfo.primary =
	    R_ADDR(&mgr->reginfo,
	    ((REGENV *)env->reginfo->primary)->tx_primary);

	/* If threaded, acquire a mutex to protect the active TXN list. */
	if ((ret = __mutex_alloc(
	    env, MTX_TXN_ACTIVE, DB_MUTEX_PROCESS_ONLY, &mgr->mutex)) != 0)
		goto err;

	mgr->reginfo.mtx_alloc = region->mtx_region;
	env->tx_handle = mgr;
	return (0);

err:	env->tx_handle = NULL;
	if (mgr->reginfo.addr != NULL)
		(void)__env_region_detach(env, &mgr->reginfo, 0);

	(void)__mutex_free(env, &mgr->mutex);
	__os_free(env, mgr);
	return (ret);
}

/*
 * __txn_init --
 *	Initialize a transaction region in shared memory.
 */
static int
__txn_init(env, mgr)
	ENV *env;
	DB_TXNMGR *mgr;
{
	DB_ENV *dbenv;
	DB_LSN last_ckp;
	DB_TXNREGION *region;
	int ret;

	dbenv = env->dbenv;

	/*
	 * Find the last checkpoint in the log.
	 */
	ZERO_LSN(last_ckp);
	if (LOGGING_ON(env)) {
		/*
		 * The log system has already walked through the last
		 * file.  Get the LSN of a checkpoint it may have found.
		 */
		if ((ret = __log_get_cached_ckp_lsn(env, &last_ckp)) != 0)
			return (ret);

		/*
		 * If that didn't work, look backwards from the beginning of
		 * the last log file until we find the last checkpoint.
		 */
		if (IS_ZERO_LSN(last_ckp) &&
		    (ret = __txn_findlastckp(env, &last_ckp, NULL)) != 0)
			return (ret);
	}

	if ((ret = __env_alloc(&mgr->reginfo,
	    sizeof(DB_TXNREGION), &mgr->reginfo.primary)) != 0) {
		__db_errx(env, DB_STR("4508",
		    "Unable to allocate memory for the transaction region"));
		return (ret);
	}
	((REGENV *)env->reginfo->primary)->tx_primary =
	     R_OFFSET(&mgr->reginfo, mgr->reginfo.primary);
	region = mgr->reginfo.primary;
	memset(region, 0, sizeof(*region));

	/*
	 * RFC 0015 S4: allocate the retained-MVCC-mutex cache while the region
	 * allocator latch (mgr->reginfo.mtx_alloc) is still MUTEX_INVALID --
	 * i.e. before we point it at mtx_regenv just below -- so __env_alloc's
	 * MUTEX_REQUIRED check is satisfied exactly as it is for the primary
	 * region allocation above.  Size it to the same transaction bound
	 * __txn_region_size uses (tx_max, or DEF_MAX_TXNS when unset), which
	 * bounds the live details and hence the live mvcc mutexes.
	 * Best-effort: on failure the cache stays empty (free_mvcc_max 0) and
	 * every writer allocs/frees its own mutex as before.
	 */
	region->free_mvcc_n = 0;
	region->free_mvcc_max = 0;
	region->free_mvcc_mtx = INVALID_ROFF;
	{
		db_mutex_t *mv;
		u_int32_t cap;

		cap = dbenv->tx_max != 0 ? dbenv->tx_max : DEF_MAX_TXNS;
		if (cap != 0 && __env_alloc(&mgr->reginfo,
		    cap * sizeof(db_mutex_t), &mv) == 0) {
			region->free_mvcc_mtx = R_OFFSET(&mgr->reginfo, mv);
			region->free_mvcc_max = cap;
		}
	}

	/*
	 * mtx_region is normally aliased to the single env latch (mtx_regenv):
	 * TXN_SYSTEM_LOCK, LOG_SYSTEM_LOCK and LOCK_SYSTEM_LOCK are then one
	 * physical mutex (RFC 0015 §2).  M1 (DB_TXN_LATCH_SPLIT) was an
	 * experiment to give the txn region its own mutex so txn_begin/txn_end
	 * no longer serialise against __log_put.
	 *
	 * DISABLED: a metal A/B (test/bench/METAL-AB-2026-10.md) proved the
	 * split both (a) LOSES ~31% throughput at high thread counts -- the
	 * extra handoff costs more than the saved serialisation, the P12
	 * admission-control lesson -- and worse (b) SIGSEGVs under concurrency
	 * at t>=32.  The split moves txn_begin off mtx_regenv, but the hot path
	 * __txn_begin_pp -> __env_set_state -> __env_alloc still mutates the
	 * shared region free list (env_alloc.c:354 SH_TAILQ_REMOVE) that
	 * mtx_regenv protected; a split-latch holder and an mtx_regenv holder
	 * then corrupt it.  __env_alloc on the txn path is NOT rare (every
	 * txn_begin reaches it via THREAD_ACTIVE tracking), so the nested-under-
	 * mtx_region allocator-latch argument above does not hold.  The switch
	 * is kept inert so a future, correct split (one that keeps __env_alloc
	 * serialised, or moves THREAD_ACTIVE off the region heap) can reuse the
	 * plumbing; turning it on now would enable a known crash.
	 */
	if (getenv("DB_TXN_LATCH_SPLIT") != NULL) {
		__db_errx(env, DB_STR("5317",
	    "DB_TXN_LATCH_SPLIT is disabled: the latch split corrupts the "
	    "region allocator under concurrency (see METAL-AB-2026-10.md). "
	    "Ignoring."));
	}
	region->mtx_region =
	    ((REGENV *)env->reginfo->primary)->mtx_regenv;
	mgr->reginfo.mtx_alloc = ((REGENV *)env->reginfo->primary)->mtx_regenv;

	/* S5: recycle TXN_DETAILs through a region free list, off __env_alloc. */
	if (getenv("DB_TXN_FREELIST") != NULL)
		F_SET(region, TXN_REGION_FREELIST);

	region->maxtxns = dbenv->tx_max;
	region->inittxns = dbenv->tx_init;
	region->last_txnid = TXN_MINIMUM;
	region->cur_maxid = TXN_MAXIMUM;

	if ((ret = __mutex_alloc(
	    env, MTX_TXN_CHKPT, 0, &region->mtx_ckp)) != 0)
		return (ret);
	region->last_ckp = last_ckp;
	region->time_ckp = time(NULL);

	memset(&region->stat, 0, sizeof(region->stat));
#ifdef HAVE_STATISTICS
	region->stat.st_maxtxns = region->maxtxns;
	region->stat.st_inittxns = region->inittxns;
#endif

	SH_TAILQ_INIT(&region->active_txn);
	SH_TAILQ_INIT(&region->mvcc_txn);
	SH_TAILQ_INIT(&region->free_txn);
	return (ret);
}

/*
 * __txn_findlastckp --
 *	Find the last checkpoint in the log, walking backwards from the
 *	max_lsn given or the beginning of the last log file.  (The
 *	log system looked through the last log file when it started up.)
 *
 * PUBLIC: int __txn_findlastckp __P((ENV *, DB_LSN *, DB_LSN *));
 */
int
__txn_findlastckp(env, lsnp, max_lsn)
	ENV *env;
	DB_LSN *lsnp;
	DB_LSN *max_lsn;
{
	DBT dbt;
	DB_LOGC *logc;
	DB_LSN lsn;
	int ret, t_ret;
	u_int32_t rectype;

	ZERO_LSN(*lsnp);

	if ((ret = __log_cursor(env, &logc)) != 0)
		return (ret);

	/* Get the last LSN. */
	memset(&dbt, 0, sizeof(dbt));
	if (max_lsn != NULL) {
		lsn = *max_lsn;
		if ((ret = __logc_get(logc, &lsn, &dbt, DB_SET)) != 0)
			goto err;
	} else {
		if ((ret = __logc_get(logc, &lsn, &dbt, DB_LAST)) != 0)
			goto err;
		/*
		 * Twiddle the last LSN so it points to the beginning of the
		 * last file; we know there's no checkpoint after that, since
		 * the log system already looked there.
		 */
		lsn.offset = 0;
	}

	/* Read backwards, looking for checkpoints. */
	while ((ret = __logc_get(logc, &lsn, &dbt, DB_PREV)) == 0) {
		if (dbt.size < sizeof(u_int32_t))
			continue;
		LOGCOPY_32(env, &rectype, dbt.data);
		if (rectype == DB___txn_ckp) {
			*lsnp = lsn;
			break;
		}
	}

err:	if ((t_ret = __logc_close(logc)) != 0 && ret == 0)
		ret = t_ret;

	/*
	 * Not finding a checkpoint is not an error;  there may not exist
	 * one in the log.
	 */
	return ((ret == 0 || ret == DB_NOTFOUND) ? 0 : ret);
}

/*
 * __txn_env_refresh --
 *	Clean up after the transaction system on a close or failed open.
 *
 * PUBLIC: int __txn_env_refresh __P((ENV *));
 */
int
__txn_env_refresh(env)
	ENV *env;
{
	DB_TXN *txn;
	DB_TXNMGR *mgr;
	REGINFO *reginfo;
	u_int32_t txnid;
	int aborted, ret, t_ret;

	ret = 0;
	mgr = env->tx_handle;
	reginfo = &mgr->reginfo;

	/*
	 * This function can only be called once per process (i.e., not
	 * once per thread), so no synchronization is required.
	 *
	 * The caller is probably doing something wrong if close is called with
	 * active transactions.  Try and abort any active transactions that are
	 * not prepared, but it's quite likely the aborts will fail because
	 * recovery won't find open files.  If we can't abort any of the
	 * unprepared transaction, panic, we have to run recovery to get back
	 * to a known state.
	 */
	aborted = 0;
	if (TAILQ_FIRST(&mgr->txn_chain) != NULL) {
		while ((txn = TAILQ_FIRST(&mgr->txn_chain)) != NULL) {
			/* Prepared transactions are OK. */
			txnid = txn->txnid;
			if (((TXN_DETAIL *)txn->td)->status == TXN_PREPARED) {
				if ((ret = __txn_discard_int(txn, 0)) != 0) {
					__db_err(env, ret, DB_STR_A("4509",
					    "unable to discard txn %#lx",
					    "%#lx"), (u_long)txnid);
					break;
				}
				continue;
			}
			aborted = 1;
			if ((t_ret = __txn_abort(txn)) != 0) {
				__db_err(env, t_ret, DB_STR_A("4510",
				    "unable to abort transaction %#lx", "%#lx"),
				    (u_long)txnid);
				ret = __env_panic(env, t_ret);
				break;
			}
		}
		if (aborted) {
			__db_errx(env, DB_STR("4511",
	"Error: closing the transaction region with active transactions"));
			if (ret == 0)
				ret = EINVAL;
		}
	}

	/* Discard the per-thread lock. */
	if ((t_ret = __mutex_free(env, &mgr->mutex)) != 0 && ret == 0)
		ret = t_ret;

	/* Detach from the region. */
	if (F_ISSET(env, ENV_PRIVATE)) {
		reginfo->mtx_alloc = MUTEX_INVALID;
		/*
		 * RFC 0015 S4: MVCC-writer mutexes retained in the region cache
		 * (see __txn_mvcc_mtx_free) are not attached to any detail, so
		 * release them before the region goes away -- otherwise each one
		 * orphans a slot in the mutex region, a BOUNDED pool
		 * (mut_region.c:78 enforces dbenv->mutex_max).  Only the
		 * ENV_PRIVATE arm does this, for the same reason
		 * __lock_env_refresh gives: a shared region outlives this
		 * process, so its cached mutexes stay for the next attacher and
		 * freeing them here would corrupt a live region.  Done after
		 * clearing mtx_alloc, as __lock_env_refresh does, so
		 * __env_alloc_free's MUTEX_REQUIRED check is a no-op.
		 */
		{
			DB_TXNREGION *fregion;
			db_mutex_t *mv;
			u_int32_t i;

			fregion = reginfo->primary;
			if (fregion->free_mvcc_mtx != INVALID_ROFF) {
				mv = R_ADDR(reginfo, fregion->free_mvcc_mtx);
				for (i = 0; i < fregion->free_mvcc_n; i++)
					(void)__mutex_free(env, &mv[i]);
				fregion->free_mvcc_n = 0;
				__env_alloc_free(reginfo, mv);
				fregion->free_mvcc_mtx = INVALID_ROFF;
			}
		}
	}
	if ((t_ret = __env_region_detach(env, reginfo, 0)) != 0 && ret == 0)
		ret = t_ret;

	__os_free(env, mgr);

	env->tx_handle = NULL;
	return (ret);
}

/*
 * __txn_region_mutex_count --
 *	Return the number of mutexes the txn region will need.
 *
 * PUBLIC: u_int32_t __txn_region_mutex_count __P((ENV *));
 */
u_int32_t
__txn_region_mutex_count(env)
	ENV *env;
{
	COMPQUIET(env, NULL);
	/*
	 * We need  a mutex for DB_TXNMGR structure, two mutexes for
	 * the DB_TXNREGION structure.
	 */
	return (1 + 2);
}
/*
 * __txn_region_mutex_max --
 *	Return the number of additional mutexes the txn region will need.
 *
 * PUBLIC: u_int32_t __txn_region_mutex_max __P((ENV *));
 */
u_int32_t
__txn_region_mutex_max(env)
	ENV *env;
{
	DB_ENV *dbenv;
	u_int32_t count;

	dbenv = env->dbenv;

	if ((count = dbenv->tx_max) == 0)
		count = DEF_MAX_TXNS;
	/* We may need a mutex for each MVCC txn. */
	return (count > dbenv->tx_init ? count - dbenv->tx_init : 0);
}

/*
 * __txn_region_size --
 *	 Return the amount of space needed for the txn region.
 * PUBLIC:  size_t __txn_region_size __P((ENV *));
 */
size_t
__txn_region_size(env)
	ENV *env;
{
	DB_ENV *dbenv;
	size_t s;

	dbenv = env->dbenv;

	/*
	 * Make the region large enough to hold the primary transaction region
	 * structure, txn_init transaction detail structures, txn_init chunks of
	 * overhead required by the underlying shared region allocator for each
	 * chunk of memory, txn_max transaction names, at an average of 20
	 * bytes each, and 10KB for safety.
	 */
	s = sizeof(DB_TXNREGION) + dbenv->tx_init *
	    (sizeof(TXN_DETAIL) + __env_alloc_overhead() + 20) + 10 * 1024;
	return (s);
}

/*
 * __txn_region_max --
 *	 Return the additional amount of space needed for the txn region.
 * PUBLIC:  size_t __txn_region_max __P((ENV *));
 */
size_t
__txn_region_max(env)
	ENV *env;
{
	DB_ENV *dbenv;
	size_t s;
	u_int32_t count;

	dbenv = env->dbenv;

	if ((count = dbenv->tx_max) == 0)
		count = DEF_MAX_TXNS;
	if (count <= dbenv->tx_init)
		return (0);
	s = (count - dbenv->tx_init) *
	    (sizeof(TXN_DETAIL) + __env_alloc_overhead() + 20);
	return (s);
}

/*
 * __txn_id_set --
 *	Set the current transaction ID and current maximum unused ID (for
 *	testing purposes only).
 *
 * PUBLIC: int __txn_id_set __P((ENV *, u_int32_t, u_int32_t));
 */
int
__txn_id_set(env, cur_txnid, max_txnid)
	ENV *env;
	u_int32_t cur_txnid, max_txnid;
{
	DB_TXNMGR *mgr;
	DB_TXNREGION *region;
	int ret;

	ENV_REQUIRES_CONFIG(env, env->tx_handle, "txn_id_set", DB_INIT_TXN);

	mgr = env->tx_handle;
	region = mgr->reginfo.primary;
	region->last_txnid = cur_txnid;
	region->cur_maxid = max_txnid;

	ret = 0;
	if (cur_txnid < TXN_MINIMUM) {
		__db_errx(env, DB_STR_A("4512",
		    "Current ID value %lu below minimum", "%lu"),
		    (u_long)cur_txnid);
		ret = EINVAL;
	}
	if (max_txnid < TXN_MINIMUM) {
		__db_errx(env, DB_STR_A("4513",
		    "Maximum ID value %lu below minimum", "%lu"),
		    (u_long)max_txnid);
		ret = EINVAL;
	}
	return (ret);
}

/*
 * __txn_oldest_reader --
 *	 Find the oldest "read LSN" of any active transaction'
 *	 MVCC changes older than this can safely be discarded from the cache.
 *
 * PUBLIC: int __txn_oldest_reader __P((ENV *, DB_LSN *));
 */
int
__txn_oldest_reader(env, lsnp)
	ENV *env;
	DB_LSN *lsnp;
{
	DB_LSN old_lsn;
	DB_TXNMGR *mgr;
	DB_TXNREGION *region;
	TXN_DETAIL *td;
	int ret;

	if ((mgr = env->tx_handle) == NULL)
		return (0);
	region = mgr->reginfo.primary;

	if ((ret = __log_current_lsn_int(env, &old_lsn, NULL, NULL)) != 0)
		return (ret);

	TXN_SYSTEM_LOCK(env);
	SH_TAILQ_FOREACH(td, &region->active_txn, links, __txn_detail)
		if (LOG_COMPARE(&td->read_lsn, &old_lsn) < 0)
			old_lsn = td->read_lsn;

	*lsnp = old_lsn;
	TXN_SYSTEM_UNLOCK(env);

	return (0);
}

/*
 * __txn_reap_si_details --
 *	Free committed snapshot-safe reader details that __txn_end parked on
 *	the mvcc_txn list (TXN_DTL_SNAPSHOT) because their SIREAD markers still
 *	referenced them, and whose markers have since been garbage-collected
 *	(si_ref back to 0).  Only reaps details with no MVCC pages either
 *	(mvcc_ref == 0); genuine MVCC-version holders are freed by
 *	__txn_remove_buffer when their last page is evicted.  Best effort.
 *
 *	TXN_DTL_SNAPSHOT is the free claim, and it is taken under td->mvcc_mtx
 *	-- the latch every writer of that flag holds (__txn_end takes it nested
 *	inside TXN_SYSTEM_LOCK; __txn_remove_buffer takes it alone).  This
 *	routine and __txn_remove_buffer test the same predicate, so without a
 *	shared claim both could free the same detail.  Ordering here is
 *	TXN_SYSTEM_LOCK -> mvcc_mtx, matching __txn_end.
 *
 * PUBLIC: int __txn_reap_si_details __P((ENV *));
 */
int
__txn_reap_si_details(env)
	ENV *env;
{
	DB_TXNMGR *mgr;
	DB_TXNREGION *region;
	TXN_DETAIL *td, *next_td;
	db_mutex_t mvcc_mtx;
	int free_it, ret, t_ret;

	if ((mgr = env->tx_handle) == NULL)
		return (0);
	region = mgr->reginfo.primary;
	ret = 0;

	TXN_SYSTEM_LOCK(env);
	for (td = SH_TAILQ_FIRST(&region->mvcc_txn, __txn_detail);
	    td != NULL; td = next_td) {
		next_td = SH_TAILQ_NEXT(td, links, __txn_detail);
		if (!F_ISSET(td, TXN_DTL_SNAPSHOT))
			continue;
		/*
		 * Re-test and claim under mvcc_mtx: mvcc_ref is only stable
		 * under it, and the claim has to exclude __txn_remove_buffer,
		 * which holds only that mutex.  A read-only committed reader
		 * never allocated one (MUTEX_INVALID); then MUTEX_LOCK is a
		 * no-op and TXN_SYSTEM_LOCK alone is the serializer, which is
		 * all __txn_end holds for that case too.
		 */
		mvcc_mtx = td->mvcc_mtx;
		MUTEX_LOCK(env, mvcc_mtx);
		free_it = F_ISSET(td, TXN_DTL_SNAPSHOT) &&
		    td->mvcc_ref == 0 && atomic_read(&td->si_ref) == 0;
		if (free_it)
			F_CLR(td, TXN_DTL_SNAPSHOT);
		MUTEX_UNLOCK(env, mvcc_mtx);
		if (!free_it)
			continue;

		SH_TAILQ_REMOVE(&region->mvcc_txn, td, links, __txn_detail);
		STAT_DEC(env,
		    txn, nsnapshot, region->stat.st_nsnapshot, td->txnid);
		/*
		 * Release the detail's MVCC mutex before freeing the detail --
		 * otherwise the slot is leaked for the life of the environment
		 * and repetition exhausts the mutex region (a later valid
		 * operation then gets ENOMEM).  RFC 0015 S4 retains it in the
		 * region cache (__txn_mvcc_mtx_free) instead of freeing, under
		 * the TXN_SYSTEM_LOCK we already hold -- matching __txn_end.
		 * Safe to release now: we hold the claim, mvcc_ref is 0, and
		 * the detail is off every list.
		 */
		if ((t_ret =
		    __txn_mvcc_mtx_free(env, region, &td->mvcc_mtx)) != 0 &&
		    ret == 0)
			ret = t_ret;
		__env_alloc_free(&mgr->reginfo, td);
	}
	TXN_SYSTEM_UNLOCK(env);

	return (ret);
}

/*
 * __txn_mvcc_mtx_free --
 *	Release an MVCC-writer detail's mvcc_mtx: RFC 0015 S4 retains it in the
 *	bounded region cache for the next writer instead of __mutex_free'ing it
 *	(and __mutex_alloc'ing a fresh one on the next first dirty fget), each
 *	of which takes the single global MUTEX_SYSTEM_LOCK.  This is the one
 *	release routine for all three detail-free paths (__txn_end,
 *	__txn_remove_buffer, __txn_reap_si_details); every caller already holds
 *	TXN_SYSTEM_LOCK, which is also what serialises __txn_begin_int's pop, so
 *	the cache needs no mutex of its own.
 *
 *	The cache is a pure best-effort optimisation: when it is full, absent,
 *	reuse is disabled (DB_NO_MVCC_MTX_REUSE) or failchk is on (F6: a
 *	retained, never-locked mutex has no owner failchk can key on), it falls
 *	back to __mutex_free, so it never fails and never bounds concurrency.
 *	The mutex is unlocked and unreferenced on entry -- exactly the
 *	postcondition __mutex_free would leave for the mutex free list -- so a
 *	cached mutex and a freshly-allocated one are indistinguishable to the
 *	next writer (the same property P13 relies on for locker mutexes).
 *
 * PUBLIC: int __txn_mvcc_mtx_free __P((ENV *, DB_TXNREGION *, db_mutex_t *));
 */
int
__txn_mvcc_mtx_free(env, region, mtxp)
	ENV *env;
	DB_TXNREGION *region;
	db_mutex_t *mtxp;
{
	DB_TXNMGR *mgr;
	db_mutex_t *mv;
	static int reuse = -1;

	if (*mtxp == MUTEX_INVALID)
		return (0);

	if (reuse == -1)
		reuse = getenv("DB_NO_MVCC_MTX_REUSE") != NULL ? 0 : 1;

	mgr = env->tx_handle;
	if (reuse &&
	    !F_ISSET(env->dbenv, DB_ENV_FAILCHK) &&
	    region->free_mvcc_mtx != INVALID_ROFF &&
	    region->free_mvcc_n < region->free_mvcc_max) {
		mv = R_ADDR(&mgr->reginfo, region->free_mvcc_mtx);
		mv[region->free_mvcc_n++] = *mtxp;
		*mtxp = MUTEX_INVALID;
		return (0);
	}
	return (__mutex_free(env, mtxp));
}

/*
 * __txn_add_buffer --
 *	Add to the count of buffers created by the given transaction.
 *
 * PUBLIC: int __txn_add_buffer __P((ENV *, TXN_DETAIL *));
 */
int
__txn_add_buffer(env, td)
	ENV *env;
	TXN_DETAIL *td;
{
	DB_ASSERT(env, td != NULL);

	MUTEX_LOCK(env, td->mvcc_mtx);
	DB_ASSERT(env, td->mvcc_ref < UINT32_MAX);
	++td->mvcc_ref;
	MUTEX_UNLOCK(env, td->mvcc_mtx);

	COMPQUIET(env, NULL);
	return (0);
}

/*
 * __txn_remove_buffer --
 *	Remove a buffer from a transaction -- free the transaction if necessary.
 *
 * PUBLIC: int __txn_remove_buffer __P((ENV *, TXN_DETAIL *, db_mutex_t));
 */
int
__txn_remove_buffer(env, td, hash_mtx)
	ENV *env;
	TXN_DETAIL *td;
	db_mutex_t hash_mtx;
{
	DB_TXNMGR *mgr;
	DB_TXNREGION *region;
	int need_free, ret;

	DB_ASSERT(env, td != NULL);
	ret = 0;
	mgr = env->tx_handle;
	region = mgr->reginfo.primary;

	MUTEX_LOCK(env, td->mvcc_mtx);
	DB_ASSERT(env, td->mvcc_ref > 0);

	/*
	 * We free the transaction detail here only if this is the last
	 * reference and td is on the list of committed snapshot transactions
	 * with active pages.
	 *
	 * Claim the free by clearing TXN_DTL_SNAPSHOT here, while mvcc_mtx is
	 * still held: __txn_reap_si_details tests the same predicate and takes
	 * the same claim under this mutex, so exactly one of us frees the
	 * detail.  Claiming later (under TXN_SYSTEM_LOCK, which we cannot hold
	 * yet -- hash_mtx must be dropped first) would leave a window in which
	 * the reaper frees td and we then touch freed region memory.
	 */
	need_free = (--td->mvcc_ref == 0) && F_ISSET(td, TXN_DTL_SNAPSHOT) &&
	    atomic_read(&td->si_ref) == 0;
	if (need_free)
		F_CLR(td, TXN_DTL_SNAPSHOT);
	MUTEX_UNLOCK(env, td->mvcc_mtx);

	if (need_free) {
		MUTEX_UNLOCK(env, hash_mtx);

		TXN_SYSTEM_LOCK(env);
		SH_TAILQ_REMOVE(&region->mvcc_txn, td, links, __txn_detail);
		STAT_DEC(env,
		    txn, nsnapshot, region->stat.st_nsnapshot, td->txnid);
		/* RFC 0015 S4: retain the mutex in the region cache, not free. */
		ret = __txn_mvcc_mtx_free(env, region, &td->mvcc_mtx);
		__env_alloc_free(&mgr->reginfo, td);
		TXN_SYSTEM_UNLOCK(env);

		MUTEX_READLOCK(env, hash_mtx);
	}

	return (ret);
}
