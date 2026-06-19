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
#include "dbinc/mp.h"


/*
 * __memp_fput_pp --
 *	DB_MPOOLFILE->put pre/post processing.
 *
 * PUBLIC: int __memp_fput_pp
 * PUBLIC:     __P((DB_MPOOLFILE *, void *, DB_CACHE_PRIORITY, u_int32_t));
 */
int
__memp_fput_pp(dbmfp, pgaddr, priority, flags)
	DB_MPOOLFILE *dbmfp;
	void *pgaddr;
	DB_CACHE_PRIORITY priority;
	u_int32_t flags;
{
	DB_THREAD_INFO *ip;
	ENV *env;
	int ret, t_ret;

	env = dbmfp->env;

	if (flags != 0)
		return (__db_ferr(env, "DB_MPOOLFILE->put", 0));

	MPF_ILLEGAL_BEFORE_OPEN(dbmfp, "DB_MPOOLFILE->put");

	ENV_ENTER(env, ip);

	ret = __memp_fput(dbmfp, ip, pgaddr, priority);
	if (IS_ENV_REPLICATED(env) &&
	    (t_ret = __op_rep_exit(env)) != 0 && ret == 0)
		ret = t_ret;

	ENV_LEAVE(env, ip);
	return (ret);
}

/*
 * __memp_fput --
 *	DB_MPOOLFILE->put.
 *
 * PUBLIC: int __memp_fput __P((DB_MPOOLFILE *,
 * PUBLIC:      DB_THREAD_INFO *, void *, DB_CACHE_PRIORITY));
 */
int
__memp_fput(dbmfp, ip, pgaddr, priority)
	DB_MPOOLFILE *dbmfp;
	DB_THREAD_INFO *ip;
	void *pgaddr;
	DB_CACHE_PRIORITY priority;
{
	BH *bhp;
	DB_ENV *dbenv;
	DB_MPOOL *dbmp;
	DB_MPOOL_HASH *hp;
	ENV *env;
	MPOOL *c_mp;
	MPOOLFILE *mfp;
	PIN_LIST *list, *lp;
	REGINFO *infop, *reginfo;
	roff_t b_ref;
	int region;
	int ret;
	u_int32_t warmth;
	char buf[DB_THREADID_STRLEN];

	env = dbmfp->env;
	dbenv = env->dbenv;
	dbmp = env->mp_handle;
	mfp = dbmfp->mfp;
	bhp = (BH *)((u_int8_t *)pgaddr - SSZA(BH, buf));
	ret = 0;

	/*
	 * If this is marked dummy, we are using it to unpin a buffer for
	 * another thread.
	 */
	if (F_ISSET(dbmfp, MP_DUMMY))
		goto unpin;

	/*
	 * If we're mapping the file, there's nothing to do.  Because we can
	 * stop mapping the file at any time, we have to check on each buffer
	 * to see if the address we gave the application was part of the map
	 * region.
	 */
	if (dbmfp->addr != NULL && pgaddr >= dbmfp->addr &&
	    (u_int8_t *)pgaddr <= (u_int8_t *)dbmfp->addr + dbmfp->len)
		return (0);

	DB_ASSERT(env, IS_RECOVERING(env) || bhp->pgno <= mfp->last_pgno ||
	    F_ISSET(bhp, BH_FREED) || !SH_CHAIN_SINGLETON(bhp, vc));
#ifdef DIAGNOSTIC
	/*
	 * Decrement the per-file pinned buffer count (mapped pages aren't
	 * counted).
	 */
	MPOOL_SYSTEM_LOCK(env);
	if (dbmfp->pinref == 0) {
		MPOOL_SYSTEM_UNLOCK(env);
		__db_errx(env, DB_STR_A("3011",
		    "%s: more pages returned than retrieved", "%s"),
		    __memp_fn(dbmfp));
		return (__env_panic(env, EACCES));
	}
	--dbmfp->pinref;
	MPOOL_SYSTEM_UNLOCK(env);
#endif

unpin:
	infop = &dbmp->reginfo[bhp->region];
	c_mp = infop->primary;
	hp = R_ADDR(infop, c_mp->htab);
	hp = &hp[bhp->bucket];

	/*
	 * Check for a reference count going to zero.  This can happen if the
	 * application returns a page twice.
	 */
	if (atomic_read(&bhp->ref) == 0) {
		__db_errx(env, DB_STR_A("3012",
		    "%s: page %lu: unpinned page returned", "%s %lu"),
		    __memp_fn(dbmfp), (u_long)bhp->pgno);
		DB_ASSERT(env, atomic_read(&bhp->ref) != 0);
		return (__env_panic(env, EACCES));
	}

	/* Note the activity so allocation won't decide to quit. */
	++c_mp->put_counter;

	if (ip != NULL) {
		reginfo = env->reginfo;
		list = R_ADDR(reginfo, ip->dbth_pinlist);
		region = (int)(infop - dbmp->reginfo);
		b_ref = R_OFFSET(infop, bhp);
		for (lp = list; lp < &list[ip->dbth_pinmax]; lp++)
			if (lp->b_ref == b_ref && lp->region == region)
				break;

		if (lp == &list[ip->dbth_pinmax]) {
			__db_errx(env, DB_STR_A("3013",
		    "__memp_fput: pinned buffer not found for thread %s",
			    "%s"), dbenv->thread_id_string(dbenv,
			    ip->dbth_pid, ip->dbth_tid, buf));
			return (__env_panic(env, EINVAL));
		}

		lp->b_ref = INVALID_ROFF;
		ip->dbth_pincount--;
	}

	/*
	 * Mark the file dirty.
	 */
	if (F_ISSET(bhp, BH_EXCLUSIVE) && F_ISSET(bhp, BH_DIRTY)) {
		DB_ASSERT(env, atomic_read(&hp->hash_page_dirty) > 0);
		mfp->file_written = 1;
	}

	/*
	 * If more than one reference to the page we're done.  Ignore the
	 * discard flags (for now) and leave the buffer's priority alone.
	 * We are doing this a little early as the remaining ref may or
	 * may not be a write behind.  If it is we set the priority
	 * here, if not it will get set again later.  We might race
	 * and miss setting the priority which would leave it wrong
	 * for a while.
	 */
	DB_ASSERT(env, atomic_read(&bhp->ref) != 0);
	if (atomic_dec(env, &bhp->ref) > 1 || (atomic_read(&bhp->ref) == 1 &&
	    !F_ISSET(bhp, BH_DIRTY))) {
		/*
		 * __memp_pgwrite only has a shared lock while it clears
		 * the BH_DIRTY bit. If we only have a shared latch then
		 * we can't touch the flags bits.
		 */
		if (F_ISSET(bhp, BH_EXCLUSIVE))
			F_CLR(bhp, BH_EXCLUSIVE);
		MUTEX_UNLOCK(env, bhp->mtx_buf);
		return (0);
	}

	/* The buffer should not be accessed again. */
#ifdef DIAG_MVCC
	MUTEX_LOCK(env, hp->mtx_hash);
	if (BH_REFCOUNT(bhp) == 0)
		MVCC_MPROTECT(bhp->buf, mfp->pagesize, 0);
	MUTEX_UNLOCK(env, hp->mtx_hash);
#endif

	/*
	 * CLOCK warmth update.  Warmth encodes access frequency: each access
	 * climbs the buffer one step toward a cap derived from the access
	 * priority hint, and the eviction hand decrements warmth as it sweeps.
	 * A frequently-accessed (hot) buffer climbs to its cap and then stays
	 * there with no further store (the climb is read-first), so the hot
	 * read path is free of shared writes; a buffer touched once by a scan
	 * only reaches warmth 1 and is aged out well before the hot set.  We
	 * don't lock the warmth; a benign race only mis-warms a buffer briefly.
	 */
	if (priority == DB_PRIORITY_VERY_LOW ||
	    mfp->priority == MPOOL_PRI_VERY_LOW)
		bhp->priority = MPOOL_CLOCK_VERY_LOW;
	else if (priority == DB_PRIORITY_HIGH ||
	    priority == DB_PRIORITY_VERY_HIGH) {
		/* Explicit high-priority hint pins the buffer at the ceiling. */
		if (bhp->priority < MPOOL_CLOCK_MAX)
			bhp->priority = MPOOL_CLOCK_MAX;
	} else {
		u_int32_t cap;

		cap = (priority == DB_PRIORITY_LOW ||
		    mfp->priority == MPOOL_PRI_LOW) ?
		    MPOOL_CLOCK_LOW : MPOOL_CLOCK_MAX;
		if (F_ISSET(bhp, BH_DIRTY) &&
		    cap <= MPOOL_CLOCK_MAX - MPOOL_CLOCK_DIRTY_BOOST)
			cap += MPOOL_CLOCK_DIRTY_BOOST;
		warmth = bhp->priority;
		if (warmth < cap)
			bhp->priority = warmth + 1;
	}

	/*
	 * __memp_pgwrite only has a shared lock while it clears the
	 * BH_DIRTY bit. If we only have a shared latch then we can't
	 * touch the flags bits.
	 */
	if (F_ISSET(bhp, BH_EXCLUSIVE))
		F_CLR(bhp, BH_EXCLUSIVE);
	MUTEX_UNLOCK(env, bhp->mtx_buf);

	return (ret);
}


/*
 * __memp_unpin_buffers --
 *	Unpin buffers pinned by a thread.
 *
 * PUBLIC: int __memp_unpin_buffers __P((ENV *, DB_THREAD_INFO *));
 */
int
__memp_unpin_buffers(env, ip)
	ENV *env;
	DB_THREAD_INFO *ip;
{
	BH *bhp;
	DB_MPOOL *dbmp;
	DB_MPOOLFILE dbmf;
	PIN_LIST *list, *lp;
	REGINFO *rinfop, *reginfo;
	int ret;

	memset(&dbmf, 0, sizeof(dbmf));
	dbmf.env = env;
	dbmf.flags = MP_DUMMY;
	dbmp = env->mp_handle;
	reginfo = env->reginfo;

	list = R_ADDR(reginfo, ip->dbth_pinlist);
	for (lp = list; lp < &list[ip->dbth_pinmax]; lp++) {
		if (lp->b_ref == INVALID_ROFF)
			continue;
		rinfop = &dbmp->reginfo[lp->region];
		bhp = R_ADDR(rinfop, lp->b_ref);
		dbmf.mfp = R_ADDR(dbmp->reginfo, bhp->mf_offset);
		if ((ret = __memp_fput(&dbmf, ip,
		    (u_int8_t *)bhp + SSZA(BH, buf),
		    DB_PRIORITY_UNCHANGED)) != 0)
			return (ret);
	}
	return (0);
}

/*
 * __memp_wire --
 *	Mark a resident buffer as non-evictable ("wired").  Used for B-tree
 *	internal/root pages so the Stage 1 optimistic descent can read them
 *	without the frame being reclaimed under it.  The set is a plain
 *	monotonic store to a dedicated byte (not the flags word), safe to do
 *	while the caller holds only a shared buffer latch; the byte is reset to
 *	0 wherever a buffer header is (re)initialized.
 *
 *	If wiredp is non-NULL it is set to 1 iff the frame is wired (and so
 *	guaranteed resident) on return -- either newly wired here or already
 *	wired -- and 0 otherwise (an mmap'd page, or the per-region cap was
 *	reached).  Callers that cache the frame address for later lock-free
 *	reads MUST only do so when *wiredp is 1; a non-wired frame is evictable
 *	and its address may dangle.
 *
 * PUBLIC: int __memp_wire __P((DB_MPOOLFILE *, void *, int *));
 */
int
__memp_wire(dbmfp, pgaddr, wiredp)
	DB_MPOOLFILE *dbmfp;
	void *pgaddr;
	int *wiredp;
{
	BH *bhp;
	DB_MPOOL *dbmp;
	ENV *env;
	MPOOL *c_mp;

	if (wiredp != NULL)
		*wiredp = 0;

	/*
	 * A memory-mapped (read-only) file hands back a pointer into the mmap
	 * region, not a buffer frame, so the BH back-computation below would be
	 * a wild pointer.  Such pages are never in the buffer pool and never
	 * evicted, so there is nothing to wire (and nothing that can dangle).
	 */
	if (dbmfp->addr != NULL && pgaddr >= dbmfp->addr &&
	    (u_int8_t *)pgaddr <=
	    (u_int8_t *)dbmfp->addr + dbmfp->len)
		return (0);

	bhp = (BH *)((u_int8_t *)pgaddr - SSZA(BH, buf));
	if (bhp->wired != 0) {
		if (wiredp != NULL)
			*wiredp = 1;
		return (0);
	}

	/*
	 * Cap wiring at MPOOL_WIRED_MAX_PCT of the region's buffers so wiring
	 * can never starve the cache.  Over the cap this is a no-op (the frame
	 * stays evictable and the descent uses a normal pin).  The count is
	 * approximate under races, which is fine for a cap.  Compute the limit
	 * as (pages * PCT) / 100 so it does not round down to zero for caches
	 * smaller than 100 buffers.
	 */
	env = dbmfp->env;
	dbmp = env->mp_handle;
	c_mp = dbmp->reginfo[bhp->region].primary;
	if (atomic_read(&c_mp->wired_pages) >=
	    c_mp->pages * MPOOL_WIRED_MAX_PCT / 100)
		return (0);

	bhp->wired = 1;
	(void)atomic_inc(env, &c_mp->wired_pages);
	if (wiredp != NULL)
		*wiredp = 1;
	return (0);
}

/*
 * __memp_unwire --
 *	Clear the wired mark on a buffer (e.g. when its page is freed) so the
 *	frame becomes evictable again.  Safe on mmap'd pages (no-op).
 *
 * PUBLIC: int __memp_unwire __P((DB_MPOOLFILE *, void *));
 */
int
__memp_unwire(dbmfp, pgaddr)
	DB_MPOOLFILE *dbmfp;
	void *pgaddr;
{
	BH *bhp;
	DB_MPOOL *dbmp;
	ENV *env;
	MPOOL *c_mp;

	if (dbmfp->addr != NULL && pgaddr >= dbmfp->addr &&
	    (u_int8_t *)pgaddr <=
	    (u_int8_t *)dbmfp->addr + dbmfp->len)
		return (0);

	bhp = (BH *)((u_int8_t *)pgaddr - SSZA(BH, buf));
	if (bhp->wired == 0)
		return (0);

	bhp->wired = 0;
	env = dbmfp->env;
	dbmp = env->mp_handle;
	c_mp = dbmp->reginfo[bhp->region].primary;
	(void)atomic_dec(env, &c_mp->wired_pages);
	return (0);
}
