/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1999, 2013 Oracle and/or its affiliates.  All rights reserved.
 *
 * $Id$
 */

#include "db_config.h"

#include "db_int.h"
#include "dbinc/db_page.h"
#include "dbinc/hash.h"
#include "dbinc/lock.h"
#include "dbinc/mp.h"

/*
 * __ham_use_cache --
 *	Whether the RFC 0017 cache-and-validate fast path applies to this
 *	cursor.  Gated on DBC_LOGGING(dbc), not merely LOGGING_ON(env): a split
 *	on a non-logging cursor (DB_TXN_NOT_DURABLE, or a recovery cursor) takes
 *	the LSN_NOT_LOGGED branch in __ham_expand_table / __ham_contract_table,
 *	which writes the same not-logged LSN every time, so the LSN stamp would
 *	not advance and a stale cache would go undetected.  DBC_LOGGING is the
 *	same per-cursor predicate those split paths use to decide whether to
 *	log, so gating on it guarantees any mapping-changing split advances the
 *	stamp.  It also excludes recovery (DBC_RECOVER).  When this is false the
 *	cursor uses the historical lock-held metapage path verbatim (including
 *	the !STD_LOCKING nelem++ and CDB behaviour).  Finally it is confined to
 *	keyed point access (hcp->cache_ok, set by __hamc_get for DB_SET and
 *	friends and by __hamc_put): a cursor scan (DB_NEXT/DB_PREV/...) walks
 *	buckets 0..max_bucket and, without the metapage held, a concurrent
 *	committed split could relocate a record from an already-scanned bucket
 *	into a not-yet-scanned higher bucket and return it twice.  Scans keep
 *	master's locked metapath; only point lookups and inserts use the cache.
 */
static int
__ham_use_cache(dbc)
	DBC *dbc;
{
	HASH_CURSOR *hcp;

	hcp = (HASH_CURSOR *)dbc->internal;
	return (hcp->cache_ok && STD_LOCKING(dbc) && DBC_LOGGING(dbc));
}

/*
 * __ham_meta_cache_fill --
 *	Refresh the per-cursor copy of the metapage mapping fields under a
 *	transient READ lock that is released immediately (not held to
 *	transaction end).  On return hcp->hdr points at hcp->hdr_cache and no
 *	metapage lock or pin is held.  Preserves the subdb revision /
 *	__db_reopen path verbatim.
 */
static int
__ham_meta_cache_fill(dbc)
	DBC *dbc;
{
	DB *dbp;
	DB_MPOOLFILE *mpf;
	HASH *hashp;
	HASH_CURSOR *hcp;
	HMETA *real;
	u_int32_t revision;
	int ret, t_ret;

	dbp = dbc->dbp;
	mpf = dbp->mpf;
	hashp = dbp->h_internal;
	hcp = (HASH_CURSOR *)dbc->internal;

again:
	revision = hashp->revision;
	if ((ret = __db_lget(dbc, 0,
	     hashp->meta_pgno, DB_LOCK_READ, 0, &hcp->hlock)) != 0)
		return (ret);
	if ((ret = __memp_fget(mpf, &hashp->meta_pgno,
	    dbc->thread_info, dbc->txn, DB_MPOOL_CREATE, &real)) != 0) {
		(void)__LPUT(dbc, hcp->hlock);
		return (ret);
	}

	if (F_ISSET(dbp, DB_AM_SUBDB) &&
	    (revision != dbp->mpf->mfp->revision ||
	    (TYPE(real) != P_HASHMETA &&
	    !IS_RECOVERING(dbp->env) && !F_ISSET(dbp, DB_AM_RECOVER)))) {
		ret = __LPUT(dbc, hcp->hlock);
		t_ret = __memp_fput(mpf, dbc->thread_info, real, dbc->priority);
		if (ret != 0)
			return (ret);
		if (t_ret != 0)
			return (t_ret);
		if ((ret = __db_reopen(dbc)) != 0)
			return (ret);
		goto again;
	}

	/*
	 * Copy the whole metapage under the READ lock -- this is the only
	 * place the mapping fields (max_bucket, masks, spares[]) and the LSN
	 * stamp are read together, so the copy cannot tear against a splitter
	 * holding the metapage WRITE lock.  Then release the lock and the pin
	 * immediately; the cursor maps from the copy with no metapage lock.
	 */
	memcpy(&hcp->hdr_cache, real, sizeof(HMETA));
	hcp->hdr = &hcp->hdr_cache;

	ret = __memp_fput(mpf, dbc->thread_info, real, dbc->priority);
	if ((t_ret = __LPUT(dbc, hcp->hlock)) != 0 && ret == 0)
		ret = t_ret;
	return (ret);
}

/*
 * __ham_meta_validate --
 *	After the bucket has been locked, confirm the cached mapping is still
 *	current by comparing the real metapage LSN with the cached one.  The
 *	LSN is 8 bytes, aligned at offset 0 of the page, so an unlocked read
 *	is atomic on the target platforms.  A logged split (or abort, which is
 *	also logged) advances it.  Returns 0 and leaves *stalep set when the
 *	mapping moved; the caller then releases the bucket, refreshes and
 *	retries.
 *
 * PUBLIC: int __ham_meta_validate __P((DBC *, int *));
 */
int
__ham_meta_validate(dbc, stalep)
	DBC *dbc;
	int *stalep;
{
	DB *dbp;
	DB_MPOOLFILE *mpf;
	HASH *hashp;
	HASH_CURSOR *hcp;
	HMETA *real;
	DB_LSN cached;
	int ret, t_ret;

	*stalep = 0;
	hcp = (HASH_CURSOR *)dbc->internal;
	if (!HCP_META_CACHED(hcp))
		return (0);

	dbp = dbc->dbp;
	mpf = dbp->mpf;
	hashp = dbp->h_internal;
	cached = LSN(&hcp->hdr_cache);

	/*
	 * DB_MPOOL_TRY, not a blocking fget (hazard H7).  A splitter holds the
	 * metapage buffer latch (the page is dirtied) AND then waits for a
	 * bucket WRITE lock-manager lock.  We hold a bucket lock here, so a
	 * blocking read of the metapage latch would be a bucket-lock-held,
	 * wait-on-metapage-latch edge that closes a cycle the lock-manager
	 * deadlock detector cannot see (it is a latch, not a lock object).
	 * TRY turns that wait into DB_LOCK_NOTGRANTED, which we treat as
	 * "stale": the caller releases the bucket lock and retries, breaking
	 * the cycle.  A split in progress is exactly a case where the mapping
	 * may be changing, so declaring it stale is also semantically right.
	 */
	if ((ret = __memp_fget(mpf, &hashp->meta_pgno,
	    dbc->thread_info, dbc->txn, DB_MPOOL_TRY, &real)) != 0) {
		if (ret == DB_LOCK_NOTGRANTED) {
			*stalep = 1;
			return (0);
		}
		return (ret);
	}
	if (LOG_COMPARE(&LSN(real), &cached) != 0)
		*stalep = 1;
	if ((t_ret = __memp_fput(mpf,
	    dbc->thread_info, real, dbc->priority)) != 0 && ret == 0)
		ret = t_ret;
	return (ret);
}

/*
 * Acquire the meta-data page.
 *
 * PUBLIC: int __ham_get_meta __P((DBC *));
 */
int
__ham_get_meta(dbc)
	DBC *dbc;
{
	DB *dbp;
	DB_MPOOLFILE *mpf;
	HASH *hashp;
	HASH_CURSOR *hcp;
	u_int32_t revision;
	int ret, t_ret;

	dbp = dbc->dbp;
	mpf = dbp->mpf;
	hashp = dbp->h_internal;
	hcp = (HASH_CURSOR *)dbc->internal;

	if (__ham_use_cache(dbc))
		return (__ham_meta_cache_fill(dbc));

again:
	revision = hashp->revision;
	if ((ret = __db_lget(dbc, 0,
	     hashp->meta_pgno, DB_LOCK_READ, 0, &hcp->hlock)) != 0)
		return (ret);

	if ((ret = __memp_fget(mpf, &hashp->meta_pgno,
	    dbc->thread_info, dbc->txn, DB_MPOOL_CREATE, &hcp->hdr)) != 0) {
		(void)__LPUT(dbc, hcp->hlock);
		return (ret);
	}

	if (F_ISSET(dbp, DB_AM_SUBDB) &&
	    (revision != dbp->mpf->mfp->revision ||
	    (TYPE(hcp->hdr) != P_HASHMETA &&
	    !IS_RECOVERING(dbp->env) && !F_ISSET(dbp, DB_AM_RECOVER)))) {
		ret = __LPUT(dbc, hcp->hlock);
		t_ret =
		    __memp_fput(mpf, dbc->thread_info, hcp->hdr, dbc->priority);
		hcp->hdr = NULL;
		if (ret != 0)
			return (ret);
		if (t_ret != 0)
			return (t_ret);
		if ((ret = __db_reopen(dbc)) != 0)
			return (ret);
		goto again;
	}

	return (ret);
}

/*
 * Release the meta-data page.
 *
 * PUBLIC: int __ham_release_meta __P((DBC *));
 */
int
__ham_release_meta(dbc)
	DBC *dbc;
{
	DB_MPOOLFILE *mpf;
	HASH_CURSOR *hcp;
	int ret;

	mpf = dbc->dbp->mpf;
	hcp = (HASH_CURSOR *)dbc->internal;

	/*
	 * RFC 0017 change 1: in the cached fast path there is no pinned
	 * metapage and no lock-manager lock to release -- just drop the cache
	 * view.  __db_lput/__TLPUT were already avoided when the cache was
	 * filled, so this removes the transaction-duration metapage read lock
	 * from every hash operation that stayed on the fast path.
	 */
	if (HCP_META_CACHED(hcp)) {
		hcp->hdr = NULL;
		return (0);
	}

	if (hcp->hdr != NULL) {
		if ((ret = __memp_fput(mpf,
		    dbc->thread_info, hcp->hdr, dbc->priority)) != 0)
			return (ret);
		hcp->hdr = NULL;
	}

	ret = __TLPUT(dbc, hcp->hlock);
	hcp->hlock.mode = DB_LOCK_NG;
	return (ret);
}

/*
 * Mark the meta-data page dirty.
 *
 * PUBLIC: int __ham_dirty_meta __P((DBC *, u_int32_t));
 */
int
__ham_dirty_meta(dbc, flags)
	DBC *dbc;
	u_int32_t flags;
{
	DB_MPOOLFILE *mpf;
	HASH *hashp;
	HASH_CURSOR *hcp;
	int ret;

	if (F_ISSET(dbc, DBC_OPD))
		dbc = dbc->internal->pdbc;
	hashp = dbc->dbp->h_internal;
	hcp = (HASH_CURSOR *)dbc->internal;
	if (hcp->hlock.mode == DB_LOCK_WRITE)
		return (0);

	mpf = dbc->dbp->mpf;

	/*
	 * RFC 0017 change 1: if we are on the cached fast path we hold no
	 * metapage pin or lock.  A metapage write (split/contract/nelem) needs
	 * the real page, so promote: drop the cache view and acquire the real
	 * metapage under a WRITE lock.  From here the historical code runs
	 * unchanged, so __ham_dirty_meta's callers (__ham_expand_table,
	 * __ham_contract_table, the !STD_LOCKING nelem++ path) see a pinned,
	 * write-locked metapage exactly as before.
	 */
	if (HCP_META_CACHED(hcp)) {
		hcp->hdr = NULL;
		if ((ret = __db_lget(dbc, 0, hashp->meta_pgno,
		    DB_LOCK_WRITE, 0, &hcp->hlock)) != 0)
			return (ret);
		return (__memp_fget(mpf, &hashp->meta_pgno, dbc->thread_info,
		    dbc->txn, DB_MPOOL_DIRTY, &hcp->hdr));
	}

	if ((ret = __db_lget(dbc, LCK_COUPLE, hashp->meta_pgno,
	     DB_LOCK_WRITE, DB_LOCK_NOWAIT, &hcp->hlock)) != 0) {
		if (ret != DB_LOCK_NOTGRANTED && ret != DB_LOCK_DEADLOCK)
			return (ret);
		if ((ret = __memp_fput(mpf,
		    dbc->thread_info, hcp->hdr, dbc->priority)) != 0)
			return (ret);
		hcp->hdr = NULL;
		if ((ret = __db_lget(dbc, LCK_COUPLE, hashp->meta_pgno,
		     DB_LOCK_WRITE, 0, &hcp->hlock)) != 0)
			return (ret);
		ret = __memp_fget(mpf, &hashp->meta_pgno,
		    dbc->thread_info, dbc->txn, DB_MPOOL_DIRTY, &hcp->hdr);
		return (ret);
	}

	return (__memp_dirty(mpf,
	    &hcp->hdr, dbc->thread_info, dbc->txn, dbc->priority, flags));
}

/*
 * Return the meta data page if it is saved in the cursor.
 *
 * PUBLIC: int __ham_return_meta __P((DBC *, u_int32_t, DBMETA **));
 */
 int
 __ham_return_meta(dbc, flags, metap)
	DBC *dbc;
	u_int32_t flags;
	DBMETA **metap;
{
	HASH_CURSOR *hcp;
	int ret;

	*metap = NULL;
	if (F_ISSET(dbc, DBC_OPD))
		dbc = dbc->internal->pdbc;

	hcp = (HASH_CURSOR *)dbc->internal;
	if (hcp->hdr == NULL || PGNO(hcp->hdr) != PGNO_BASE_MD)
		return (0);

	/*
	 * RFC 0017 change 1: never hand back the per-cursor cache copy here.
	 * Callers (__db_new / __db_free) read and write the shared free list
	 * and last_pgno through this pointer, which the snapshot must not
	 * shadow.  __ham_dirty_meta promotes the cache view to the real,
	 * write-locked metapage; do that even for a read-only request so the
	 * returned page is the authoritative shared one.
	 */
	if (HCP_META_CACHED(hcp) &&
	    (ret = __ham_dirty_meta(dbc, flags)) != 0)
		return (ret);

	if (LF_ISSET(DB_MPOOL_DIRTY) &&
	    (ret = __ham_dirty_meta(dbc, flags)) != 0)
		return (ret);

	*metap = (DBMETA *)hcp->hdr;
	return (0);
}
