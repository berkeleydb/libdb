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
 * RFC 0017 change 1 -- keyed operations without the hash metapage lock.
 *
 * Every keyed get or put used to take the metapage READ lock through the lock
 * manager, and a transaction kept it until commit.  At 32 threads that one
 * lock object is the bottleneck: its acquire, and its release at commit, are
 * most of the profile.
 *
 * The fast path maps the key with a per-cursor hint of the mapping fields
 * (max_bucket, the masks, spares[]) and takes no metapage lock:
 *
 *	1. map key -> bucket -> page from the hint;
 *	2. lock that bucket page through the lock manager (a probe: nothing is
 *	   read under it yet);
 *	3. pin the metapage with DB_MPOOL_TRY (a latch, never a lock-manager
 *	   lock) and map the key again from the live fields;
 *	4. same page: the mapping is current, so proceed.  The bucket lock fences
 *	   any split or contraction of this bucket for as long as we hold it.
 *	   Different page: release the probe with __LPUT, refresh the hint and
 *	   retry.  Nothing was read under the probe, so dropping it inside a
 *	   transaction is safe, and the lock manager's reference count keeps an
 *	   earlier hold by the same transaction.
 *
 * The validation is a recomputation, not a stamp comparison: a split of some
 * other bucket changes the metapage but not this key's page, so the operation
 * does not retry.  Bucket page numbers never move while the bucket lock is
 * held: a split write-locks both buckets, contraction locks both, and
 * compaction write-locks a page before exchanging it.
 *
 * Fallback: if the metapage is latched (a split is updating it) the TRY fails.
 * Waiting on that latch while holding a bucket lock could deadlock against the
 * splitter, which holds the latch and wants a bucket lock, and the deadlock
 * detector cannot see latches.  So the probe is released and the operation
 * falls back to the historical order, metapage lock first, for this one call.
 *
 * Used only where the lock manager governs visibility: standard locking, no
 * MVCC on the file, no dirty reads, not under recovery.  Scans keep the locked
 * metapage, because a split during a scan could return a record twice.
 */
static int
__ham_use_cache(dbc)
	DBC *dbc;
{
	HASH_CURSOR *hcp;

	hcp = (HASH_CURSOR *)dbc->internal;
	return (hcp->cache_ok && STD_LOCKING(dbc) &&
	    !F_ISSET(dbc, DBC_RECOVER | DBC_READ_UNCOMMITTED) &&
	    !F_ISSET(dbc->dbp, DB_AM_READ_UNCOMMITTED | DB_AM_RECOVER) &&
	    !IS_RECOVERING(dbc->env) && !MULTIVERSION(dbc->dbp));
}

/*
 * __ham_hint_map --
 *	Map a key to its bucket page through a set of mapping fields.
 */
static db_pgno_t
__ham_hint_map(dbc, meta, key, bucketp)
	DBC *dbc;
	HMETA *meta;
	const DBT *key;
	u_int32_t *bucketp;
{
	DB *dbp;
	u_int32_t bucket, n;

	dbp = dbc->dbp;
	n = (u_int32_t)(((HASH *)dbp->h_internal)->h_hash(dbp,
	    key->data, key->size));
	bucket = n & meta->high_mask;
	if (bucket > meta->max_bucket)
		bucket = bucket & meta->low_mask;
	*bucketp = bucket;
	return (BS_TO_PAGE(bucket, meta->spares));
}

/*
 * __ham_hint_fill --
 *	Copy the mapping fields from a pinned metapage into the cursor's hint.
 */
static void
__ham_hint_fill(hcp, real)
	HASH_CURSOR *hcp;
	HMETA *real;
{
	memcpy(&hcp->hdr_cache, real, sizeof(HMETA));
	hcp->hint_valid = 1;
}

/*
 * __ham_get_meta_fast --
 *	Keyed fast path: lock the key's bucket without the metapage lock.
 *	Returns 0 with the bucket locked in mode, hcp->bucket and hcp->pgno set,
 *	and hcp->hdr pointing at the cursor's hint; or DB_MPOOL_RETRY if the
 *	caller must use the locked metapage path, with nothing held.
 *
 * PUBLIC: int __ham_get_meta_fast __P((DBC *, const DBT *, db_lockmode_t));
 */
int
__ham_get_meta_fast(dbc, key, mode)
	DBC *dbc;
	const DBT *key;
	db_lockmode_t mode;
{
	DB *dbp;
	DB_MPOOLFILE *mpf;
	HASH *hashp;
	HASH_CURSOR *hcp;
	HMETA *real;
	db_pgno_t pgno, live_pgno;
	u_int32_t bucket, live_bucket;
	int ret, t_ret, tries;

	dbp = dbc->dbp;
	mpf = dbp->mpf;
	hashp = dbp->h_internal;
	hcp = (HASH_CURSOR *)dbc->internal;

	if (!__ham_use_cache(dbc))
		return (DB_MPOOL_RETRY);
	/* A subdatabase whose metapage may have moved re-opens the slow way. */
	if (F_ISSET(dbp, DB_AM_SUBDB) &&
	    hashp->revision != dbp->mpf->mfp->revision)
		return (DB_MPOOL_RETRY);

	for (tries = 0; tries < 4; tries++) {
		if (!hcp->hint_valid) {
			/* No hint yet: read the live fields under a latch only. */
			if ((ret = __memp_fget(mpf, &hashp->meta_pgno,
			    dbc->thread_info, dbc->txn, DB_MPOOL_TRY, &real)) != 0)
				return (ret == DB_LOCK_NOTGRANTED ?
				    DB_MPOOL_RETRY : ret);
			if (TYPE(real) != P_HASHMETA) {
				(void)__memp_fput(mpf,
				    dbc->thread_info, real, dbc->priority);
				return (DB_MPOOL_RETRY);
			}
			__ham_hint_fill(hcp, real);
			if ((ret = __memp_fput(mpf,
			    dbc->thread_info, real, dbc->priority)) != 0)
				return (ret);
		}

		/* 1-2. Map from the hint and lock the bucket as a probe. */
		pgno = __ham_hint_map(dbc, &hcp->hdr_cache, key, &bucket);
		if ((ret = __db_lget(dbc,
		    0, pgno, mode, 0, &hcp->lock)) != 0)
			return (ret);

		/* 3. Re-map from the live metapage, latched, never locked. */
		if ((ret = __memp_fget(mpf, &hashp->meta_pgno,
		    dbc->thread_info, dbc->txn, DB_MPOOL_TRY, &real)) != 0) {
			if ((t_ret = __LPUT(dbc, hcp->lock)) != 0)
				return (t_ret);
			return (ret == DB_LOCK_NOTGRANTED ? DB_MPOOL_RETRY : ret);
		}
		live_pgno = __ham_hint_map(dbc, real, key, &live_bucket);
#ifdef RFC17_SABOTAGE_NO_VALIDATE
		/* Test teeth only: trust a stale hint.  Never in a shipped build. */
		live_pgno = pgno;
		live_bucket = bucket;
#endif
		if (live_pgno == pgno && live_bucket == bucket) {
			/* 4. Current.  Keep the hint, which carries ffactor too. */
			ret = __memp_fput(mpf, dbc->thread_info, real, dbc->priority);
			if (ret != 0) {
				(void)__LPUT(dbc, hcp->lock);
				return (ret);
			}
			hcp->hdr = &hcp->hdr_cache;
			hcp->bucket = hcp->lbucket = bucket;
			hcp->lock_mode = mode;
			hcp->pgno = pgno;
			return (0);
		}

		/* Stale: drop the probe, refresh the hint from the live page. */
		__ham_hint_fill(hcp, real);
		ret = __memp_fput(mpf, dbc->thread_info, real, dbc->priority);
		if ((t_ret = __LPUT(dbc, hcp->lock)) != 0 && ret == 0)
			ret = t_ret;
		if (ret != 0)
			return (ret);
	}
	return (DB_MPOOL_RETRY);
}

/*
 * __ham_fast_lock --
 *	Entry point for a keyed get or put.  Resets the cursor position, then
 *	tries __ham_get_meta_fast.  Returns 0 with the bucket locked and
 *	H_FASTLOCKED set, so __ham_lookup continues from that lock; or non-zero
 *	(DB_MPOOL_RETRY, or a real error the caller will meet again on the slow
 *	path) with the cursor reset and nothing held.
 *
 * PUBLIC: int __ham_fast_lock __P((DBC *, const DBT *, db_lockmode_t));
 */
int
__ham_fast_lock(dbc, key, mode)
	DBC *dbc;
	const DBT *key;
	db_lockmode_t mode;
{
	HASH_CURSOR *hcp;
	int ret;

	hcp = (HASH_CURSOR *)dbc->internal;
	if ((ret = __ham_item_reset(dbc)) != 0)
		return (ret);
	/* A failure leaves the cursor reset with no lock and cache_ok off. */
	if ((ret = __ham_get_meta_fast(dbc, key, mode)) != 0) {
		hcp->cache_ok = 0;
		return (ret);
	}
	F_SET(hcp, H_FASTLOCKED);
	return (0);
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

	/* RFC 0017: the fast path holds no metapage pin or lock. */
	hcp->cache_ok = 0;
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
	 * RFC 0017: on the fast path the cursor holds no metapage pin or lock.
	 * A split or contraction needs the real page, so take it with a WRITE
	 * lock now; the callers then run unchanged.  The cursor already holds
	 * its bucket lock, so this is bucket-then-metapage, the order a split
	 * has always used (__hamc_put holds the bucket when it expands).
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
	 * RFC 0017: never hand back the cursor's hint.  __db_new and __db_free
	 * use this pointer for the shared free list and last_pgno, so promote
	 * to the real, write-locked metapage first.
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
