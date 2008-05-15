/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2006,2008 Oracle.  All rights reserved.
 *
 * $Id: mp_mvcc.c,v 12.42 2008/01/31 18:40:45 bostic Exp $
 */

#include "db_config.h"

#include "db_int.h"
#include "dbinc/log.h"
#include "dbinc/mp.h"
#include "dbinc/txn.h"

static int __pgno_cmp __P((const void *, const void *));

/*
 * __memp_bh_priority --
 *	Get the the aggregate priority of a chain of buffer headers.
 *
 * PUBLIC: u_int32_t __memp_bh_priority __P((BH *));
 */
u_int32_t
__memp_bh_priority(bhp)
	BH *bhp;
{
	u_int32_t priority;

	while (SH_CHAIN_HASNEXT(bhp, vc))
		bhp = SH_CHAIN_NEXT(bhp, vc, __bh);

	priority = bhp->priority;

	while ((bhp = SH_CHAIN_PREV(bhp, vc, __bh)) != NULL)
		if (bhp->priority < priority)
			priority = bhp->priority;

	return (priority);
}

/*
 * __memp_bh_settxn --
 *	Set the transaction that owns the given buffer.
 *
 * PUBLIC: int __memp_bh_settxn __P((DB_MPOOL *, MPOOLFILE *mfp, BH *, void *));
 */
int
__memp_bh_settxn(dbmp, mfp, bhp, vtd)
	DB_MPOOL *dbmp;
	MPOOLFILE *mfp;
	BH *bhp;
	void *vtd;
{
	ENV *env;
	TXN_DETAIL *td;

	env = dbmp->env;
	td = (TXN_DETAIL *)vtd;

	if (td == NULL) {
		__db_errx(env,
		      "%s: non-transactional update to a multiversion file",
		    __memp_fns(dbmp, mfp));
		return (EINVAL);
	}

	if (bhp->td_off != INVALID_ROFF) {
		DB_ASSERT(env, BH_OWNER(env, bhp) == td);
		return (0);
	}

	bhp->td_off = R_OFFSET(&env->tx_handle->reginfo, td);
	return (__txn_add_buffer(env, td));
}

/*
 * __memp_skip_curadj --
 *	Indicate whether a cursor adjustment can be skipped for a snapshot
 *	cursor.
 *
 * PUBLIC: int __memp_skip_curadj __P((DBC *, db_pgno_t));
 */
int
__memp_skip_curadj(dbc, pgno)
	DBC * dbc;
	db_pgno_t pgno;
{
	BH *bhp;
	DB_MPOOL *dbmp;
	DB_MPOOLFILE *dbmfp;
	DB_MPOOL_HASH *hp;
	DB_TXN *txn;
	ENV *env;
	MPOOLFILE *mfp;
	REGINFO *infop;
	roff_t mf_offset;
	int ret, skip;

	env = dbc->env;
	dbmp = env->mp_handle;
	dbmfp = dbc->dbp->mpf;
	mfp = dbmfp->mfp;
	mf_offset = R_OFFSET(dbmp->reginfo, mfp);
	skip = 0;

	for (txn = dbc->txn; txn->parent != NULL; txn = txn->parent)
		;

	/*
	 * Determine the cache and hash bucket where this page lives and get
	 * local pointers to them.  Reset on each pass through this code, the
	 * page number can change.
	 */
	MP_GET_BUCKET(env, mfp, pgno, &infop, hp, ret);
	if (ret != 0) {
		/* Panic: there is no way to return the error. */
		(void)__env_panic(env, ret);
		return (0);
	}

	SH_TAILQ_FOREACH(bhp, &hp->hash_bucket, hq, __bh) {
		if (bhp->pgno != pgno || bhp->mf_offset != mf_offset)
			continue;

		if (!BH_OWNED_BY(env, bhp, txn))
			skip = 1;
		break;
	}
	MUTEX_UNLOCK(env, hp->mtx_hash);

	return (skip);
}

#define	DB_FREEZER_MAGIC 0x06102002

/*
 * __memp_bh_freeze --
 *	Save a buffer header to temporary storage in case it is needed later by
 *	a snapshot transaction.  This function should be called with the hash
 *	bucket locked and will exit with it locked, as it inserts a frozen
 *	buffer after writing the data.
 *
 * PUBLIC: int __memp_bh_freeze __P((DB_MPOOL *, REGINFO *, DB_MPOOL_HASH *,
 * PUBLIC:     BH *, int *));
 */
int
__memp_bh_freeze(dbmp, infop, hp, bhp, need_frozenp)
	DB_MPOOL *dbmp;
	REGINFO *infop;
	DB_MPOOL_HASH *hp;
	BH *bhp;
	int *need_frozenp;
{
	BH *frozen_bhp;
	BH_FROZEN_ALLOC *frozen_alloc;
	DB_FH *fhp;
	ENV *env;
	MPOOL *c_mp;
	MPOOLFILE *bh_mfp;
	db_pgno_t maxpgno, newpgno, nextfree;
	size_t nio;
	int ret, t_ret;
	u_int32_t magic, nbucket, ncache, pagesize;
	char filename[100], *real_name;

	env = dbmp->env;
	c_mp = infop->primary;
	ret = 0;
	/* Find the associated MPOOLFILE. */
	bh_mfp = R_ADDR(dbmp->reginfo, bhp->mf_offset);
	pagesize = bh_mfp->stat.st_pagesize;
	real_name = NULL;
	fhp = NULL;

	DB_ASSERT(env, bhp->ref == 0);
	DB_ASSERT(env, !F_ISSET(bhp, BH_DIRTY | BH_FROZEN | BH_LOCKED));

	++bhp->ref;
	F_SET(bhp, BH_LOCKED);
	MVCC_MPROTECT(bhp->buf, pagesize, PROT_READ | PROT_WRITE);

	MUTEX_UNLOCK(env, hp->mtx_hash);

	MPOOL_REGION_LOCK(env, infop);
	frozen_bhp = SH_TAILQ_FIRST(&c_mp->free_frozen, __bh);
	if (frozen_bhp != NULL) {
		SH_TAILQ_REMOVE(&c_mp->free_frozen, frozen_bhp, hq, __bh);
		*need_frozenp = SH_TAILQ_EMPTY(&c_mp->free_frozen);
	} else {
		*need_frozenp = 1;

		/* There might be a small amount of unallocated space. */
		if (__env_alloc(infop,
		    sizeof(BH_FROZEN_ALLOC) + sizeof(BH_FROZEN_PAGE),
		    &frozen_alloc) == 0) {
			frozen_bhp = (BH *)(frozen_alloc + 1);
			SH_TAILQ_INSERT_TAIL(&c_mp->alloc_frozen,
			    frozen_alloc, links);
		}
	}
	MPOOL_REGION_UNLOCK(env, infop);
	MUTEX_LOCK(env, hp->mtx_hash);

	/*
	 * If we can't get a frozen buffer header, return ENOMEM immediately:
	 * we don't want to call __memp_alloc recursively.  __memp_alloc will
	 * turn the next free page it finds into frozen buffer headers.
	 */
	if (frozen_bhp == NULL) {
		ret = ENOMEM;
		goto err;
	}

	/*
	 * For now, keep things simple and have one file per page size per
	 * hash bucket.  This improves concurrency but can mean lots of files
	 * if there is lots of freezing.
	 */
	ncache = (u_int32_t)(infop - dbmp->reginfo);
	nbucket = (u_int32_t)(hp - (DB_MPOOL_HASH *)R_ADDR(infop, c_mp->htab));
	snprintf(filename, sizeof(filename), "__db.freezer.%lu.%lu.%luK",
	    (u_long)ncache, (u_long)nbucket, (u_long)pagesize / 1024);

	if ((ret = __db_appname(env, DB_APP_NONE, filename,
	    0, NULL, &real_name)) != 0)
		goto err;
	if ((ret = __os_open(env, real_name, pagesize,
	    DB_OSO_CREATE | DB_OSO_EXCL, env->db_mode, &fhp)) == 0) {
		/* We're creating the file -- initialize the metadata page. */
		magic = DB_FREEZER_MAGIC;
		maxpgno = newpgno = 0;
		if ((ret = __os_write(env, fhp, &magic, sizeof(u_int32_t),
		    &nio)) < 0 || nio == 0 ||
		    (ret = __os_write(env, fhp, &newpgno, sizeof(db_pgno_t),
		    &nio)) < 0 || nio == 0 ||
		    (ret = __os_write(env, fhp, &maxpgno, sizeof(db_pgno_t),
		    &nio)) < 0 || nio == 0 ||
		    (ret = __os_seek(env, fhp, 0, 0, 0)) != 0)
			goto err;
	} else if (ret == EEXIST)
		ret = __os_open(
		    env, real_name, pagesize, 0, env->db_mode, &fhp);
	if (ret != 0)
		goto err;
	if ((ret = __os_read(env, fhp, &magic, sizeof(u_int32_t),
	    &nio)) < 0 || nio == 0 ||
	    (ret = __os_read(env, fhp, &newpgno, sizeof(db_pgno_t),
	    &nio)) < 0 || nio == 0 ||
	    (ret = __os_read(env, fhp, &maxpgno, sizeof(db_pgno_t),
	    &nio)) < 0 || nio == 0)
		goto err;
	if (magic != DB_FREEZER_MAGIC) {
		ret = EINVAL;
		goto err;
	}
	if (newpgno == 0) {
		newpgno = ++maxpgno;
		if ((ret = __os_seek(env,
		    fhp, 0, 0, sizeof(u_int32_t) + sizeof(db_pgno_t))) != 0 ||
		    (ret = __os_write(env, fhp, &maxpgno, sizeof(db_pgno_t),
		    &nio)) < 0 || nio == 0)
			goto err;
	} else {
		if ((ret = __os_seek(env, fhp, newpgno, pagesize, 0)) != 0 ||
		    (ret = __os_read(env, fhp, &nextfree, sizeof(db_pgno_t),
		    &nio)) < 0 || nio == 0)
			goto err;
		if ((ret =
		    __os_seek(env, fhp, 0, 0, sizeof(u_int32_t))) != 0 ||
		    (ret = __os_write(env, fhp, &nextfree, sizeof(db_pgno_t),
		    &nio)) < 0 || nio == 0)
			goto err;
	}

	/* Write the buffer to the allocated page. */
	if ((ret = __os_io(env, DB_IO_WRITE, fhp, newpgno, pagesize, 0,
	    pagesize, bhp->buf, &nio)) != 0 || nio == 0)
		goto err;

	/*
	 * Set up the frozen_bhp with the freezer page number.  The original
	 * buffer header is about to be freed, so transfer resources to the
	 * frozen header here.
	 */
#ifdef DIAG_MVCC
	memcpy(frozen_bhp, bhp, SSZ(BH, align_off));
#else
	memcpy(frozen_bhp, bhp, SSZA(BH, buf));
#endif
	frozen_bhp->ref = frozen_bhp->ref_sync = 0;
	F_SET(frozen_bhp, BH_FROZEN);
	F_CLR(frozen_bhp, BH_LOCKED);
	frozen_bhp->priority = UINT32_MAX;
	((BH_FROZEN_PAGE *)frozen_bhp)->spgno = newpgno;

	/*
	 * We're about to add the frozen buffer header to the version chain, so
	 * we have temporarily created another buffer for the owning
	 * transaction.
	 */
	if (frozen_bhp->td_off != INVALID_ROFF &&
	    (ret = __txn_add_buffer(env, BH_OWNER(env, frozen_bhp))) != 0) {
		(void)__env_panic(env, ret);
		goto err;
	}

	/*
	 * Add the frozen buffer to the version chain and update the hash
	 * bucket if this is the head revision.  The original buffer will be
	 * freed by __memp_alloc calling __memp_bhfree (assuming no other
	 * thread has blocked waiting for it while we were freezing).
	 */
	SH_CHAIN_INSERT_AFTER(bhp, frozen_bhp, vc, __bh);
	if (!SH_CHAIN_HASNEXT(frozen_bhp, vc)) {
		SH_TAILQ_INSERT_BEFORE(&hp->hash_bucket,
		    bhp, frozen_bhp, hq, __bh);
		SH_TAILQ_REMOVE(&hp->hash_bucket, bhp, hq, __bh);
	}

	/*
	 * Increment the file's block count -- freeing the original buffer will
	 * decrement it.
	 */
	MUTEX_LOCK(env, bh_mfp->mutex);
	++bh_mfp->block_cnt;
	MUTEX_UNLOCK(env, bh_mfp->mutex);

	STAT(++hp->hash_frozen);

	if (0) {
err:		if (ret == 0)
			ret = EIO;
		if (frozen_bhp != NULL) {
			MUTEX_UNLOCK(env, hp->mtx_hash);
			MPOOL_REGION_LOCK(env, infop);
			SH_TAILQ_INSERT_TAIL(&c_mp->free_frozen,
			    frozen_bhp, hq);
			MPOOL_REGION_UNLOCK(env, infop);
			MUTEX_LOCK(env, hp->mtx_hash);
		}
	}
	if (real_name != NULL)
		__os_free(env, real_name);
	if (fhp != NULL &&
	    (t_ret = __os_closehandle(env, fhp)) != 0 && ret == 0)
		ret = t_ret;
	if (ret != 0 && ret != ENOMEM)
		__db_err(env, ret, "__memp_bh_freeze");
	F_CLR(bhp, BH_LOCKED);
	--bhp->ref;

	/*
	 * If a thread of control is waiting on this buffer, wake it up.
	 */
	if (F_ISSET(hp, IO_WAITER)) {
		F_CLR(hp, IO_WAITER);
		MUTEX_UNLOCK(env, hp->mtx_io);
	}
	return (ret);
}

static int
__pgno_cmp(a, b)
	const void *a, *b;
{
	db_pgno_t *ap, *bp;

	ap = (db_pgno_t *)a;
	bp = (db_pgno_t *)b;

	return (int)(*ap - *bp);
}

/*
 * __memp_bh_thaw --
 *	Free a buffer header in temporary storage.  optionally restore the
 *	buffer (if alloc_bhp != NULL).  This function should be
 *	called with the hash bucket locked and will return with it locked.
 *
 * PUBLIC: int __memp_bh_thaw __P((DB_MPOOL *, REGINFO *,
 * PUBLIC:	DB_MPOOL_HASH *, BH *, BH *));
 */
int
__memp_bh_thaw(dbmp, infop, hp, frozen_bhp, alloc_bhp)
	DB_MPOOL *dbmp;
	REGINFO *infop;
	DB_MPOOL_HASH *hp;
	BH *frozen_bhp, *alloc_bhp;
{
	BH *next_bhp;
	DB_FH *fhp;
	ENV *env;
#ifdef DIAGNOSTIC
	DB_LSN vlsn;
#endif
	MPOOL *c_mp;
	MPOOLFILE *bh_mfp;
	db_pgno_t *freelist, *ppgno, freepgno, maxpgno, spgno;
	size_t nio;
	u_int32_t listsize, magic, nbucket, ncache, ntrunc, nfree, pagesize;
#ifdef HAVE_FTRUNCATE
	int i;
#endif
	int needfree, ret, t_ret;
	char filename[100], *real_name;

	env = dbmp->env;
	fhp = NULL;
	c_mp = infop->primary;
	bh_mfp = R_ADDR(dbmp->reginfo, frozen_bhp->mf_offset);
	freelist = NULL;
	pagesize = bh_mfp->stat.st_pagesize;
	ret = 0;
	real_name = NULL;

	DB_ASSERT(env, F_ISSET(frozen_bhp, BH_FROZEN));
	DB_ASSERT(env, !F_ISSET(frozen_bhp, BH_LOCKED));
	DB_ASSERT(env, alloc_bhp != NULL ||
	    BH_OBSOLETE(frozen_bhp, hp->old_reader, vlsn));

	spgno = ((BH_FROZEN_PAGE *)frozen_bhp)->spgno;

	if (alloc_bhp != NULL) {
#ifdef DIAG_MVCC
		memcpy(alloc_bhp, frozen_bhp, SSZ(BH, align_off));
#else
		memcpy(alloc_bhp, frozen_bhp, SSZA(BH, buf));
#endif
		alloc_bhp->ref = 1;
		alloc_bhp->ref_sync = 0;
		F_CLR(alloc_bhp, BH_FROZEN);
	}

	F_SET(frozen_bhp, BH_LOCKED);

	/*
	 * For now, keep things simple and have one file per page size per
	 * hash bucket.  This improves concurrency but can mean lots of files
	 * if there is lots of freezing.
	 */
	ncache = (u_int32_t)(infop - dbmp->reginfo);
	nbucket = (u_int32_t)(hp - (DB_MPOOL_HASH *)R_ADDR(infop, c_mp->htab));
	snprintf(filename, sizeof(filename), "__db.freezer.%lu.%lu.%luK",
	    (u_long)ncache, (u_long)nbucket, (u_long)pagesize / 1024);

	if ((ret = __db_appname(
	    env, DB_APP_NONE, filename, 0, NULL, &real_name)) != 0)
		goto err;

	if ((ret = __os_open(
	    env, real_name, pagesize, 0, env->db_mode, &fhp)) != 0)
		goto err;

	/*
	 * Read the first free page number -- we're about to free the page
	 * after we we read it.
	 */
	if ((ret = __os_read(env, fhp, &magic, sizeof(u_int32_t),
	    &nio)) < 0 || nio == 0 ||
	    (ret = __os_read(env, fhp, &freepgno, sizeof(db_pgno_t),
	    &nio)) < 0 || nio == 0 ||
	    (ret = __os_read(env, fhp, &maxpgno, sizeof(db_pgno_t),
	    &nio)) < 0 || nio == 0)
		goto err;

	if (magic != DB_FREEZER_MAGIC) {
		ret = EINVAL;
		goto err;
	}

	/* Read the buffer from the frozen page. */
	if (alloc_bhp != NULL &&
	    ((ret = __os_io(env, DB_IO_READ, fhp, spgno, pagesize,
	    0, pagesize, alloc_bhp->buf, &nio)) != 0 || nio == 0))
		goto err;

	/*
	 * Free the page from the file.  If it's the last page, truncate.
	 * Otherwise, update free page linked list.
	 */
	needfree = 1;
	if (spgno == maxpgno) {
		listsize = 100;
		if ((ret = __os_malloc(env,
		    listsize * sizeof(db_pgno_t), &freelist)) != 0)
			goto err;
		nfree = 0;
		while (freepgno != 0) {
			if (nfree == listsize - 1) {
				listsize *= 2;
				if ((ret = __os_realloc(env,
				    listsize * sizeof(db_pgno_t),
				    &freelist)) != 0)
					goto err;
			}
			freelist[nfree++] = freepgno;
			if ((ret = __os_seek(
			    env, fhp, freepgno, pagesize, 0)) != 0 ||
			    (ret = __os_read(env, fhp, &freepgno,
			    sizeof(db_pgno_t), &nio)) < 0 || nio == 0)
				goto err;
		}
		freelist[nfree++] = spgno;
		qsort(freelist, nfree, sizeof(db_pgno_t), __pgno_cmp);
		for (ppgno = &freelist[nfree - 1]; ppgno > freelist; ppgno--)
			if (*(ppgno - 1) != *ppgno - 1)
				break;
		ntrunc = (u_int32_t)(&freelist[nfree] - ppgno);
		if (ntrunc == (u_int32_t)maxpgno) {
			needfree = 0;
			ret = __os_closehandle(env, fhp);
			fhp = NULL;
			if (ret != 0 ||
			    (ret = __os_unlink(env, real_name, 0)) != 0)
				goto err;
		}
#ifdef HAVE_FTRUNCATE
		else {
			maxpgno -= (db_pgno_t)ntrunc;
			if ((ret = __os_truncate(env, fhp,
			    maxpgno + 1, pagesize)) != 0)
				goto err;

			/* Fix up the linked list */
			freelist[nfree - ntrunc] = 0;
			if ((ret = __os_seek(
			    env, fhp, 0, 0, sizeof(u_int32_t))) != 0 ||
			    (ret = __os_write(env, fhp, &freelist[0],
			    sizeof(db_pgno_t), &nio)) < 0 || nio == 0 ||
			    (ret = __os_write(env, fhp, &maxpgno,
			    sizeof(db_pgno_t), &nio)) < 0 || nio == 0)
				goto err;

			for (i = 0; i < (int)(nfree - ntrunc); i++)
				if ((ret = __os_seek(env,
				    fhp, freelist[i], pagesize, 0)) != 0 ||
				    (ret = __os_write(env, fhp,
				    &freelist[i + 1], sizeof(db_pgno_t),
				    &nio)) < 0 || nio == 0)
					goto err;
			needfree = 0;
		}
#endif
	}
	if (needfree &&
	    ((ret = __os_seek(env, fhp, spgno, pagesize, 0)) != 0 ||
	    (ret = __os_write(env, fhp, &freepgno, sizeof(db_pgno_t),
	    &nio)) < 0 || nio == 0 ||
	    (ret = __os_seek(env, fhp, 0, 0, sizeof(u_int32_t))) != 0 ||
	    (ret = __os_write(env, fhp, &spgno, sizeof(db_pgno_t),
	    &nio)) < 0 || nio == 0))
		goto err;

	/*
	 * Add the thawed buffer (if any) to the version chain.  We can't
	 * do this any earlier, because we can't guarantee that another thread
	 * won't be waiting for it, which means we can't clean up if there are
	 * errors reading from the freezer.  We can't do it any later, because
	 * we're about to free frozen_bhp, and without it we would need to do
	 * another cache lookup to find out where the new page should live.
	 */
	if (alloc_bhp != NULL) {
		alloc_bhp->priority = c_mp->lru_count;

		SH_CHAIN_INSERT_AFTER(frozen_bhp, alloc_bhp, vc, __bh);
		if (!SH_CHAIN_HASNEXT(alloc_bhp, vc)) {
			SH_TAILQ_INSERT_BEFORE(&hp->hash_bucket,
			    frozen_bhp, alloc_bhp, hq, __bh);
			SH_TAILQ_REMOVE(&hp->hash_bucket, frozen_bhp, hq, __bh);
		}
	}

	if ((next_bhp = SH_CHAIN_NEXT(frozen_bhp, vc, __bh)) == NULL) {
		if ((next_bhp = SH_CHAIN_PREV(frozen_bhp, vc, __bh)) != NULL)
			SH_TAILQ_INSERT_BEFORE(&hp->hash_bucket, frozen_bhp,
			    next_bhp, hq, __bh);
		SH_TAILQ_REMOVE(&hp->hash_bucket, frozen_bhp, hq, __bh);
	}
	SH_CHAIN_REMOVE(frozen_bhp, vc, __bh);

	/*
	 * If other threads are waiting for this buffer as well, they will have
	 * incremented the reference count and will be waiting on the I/O mutex.
	 * For that reason, we can't unconditionally free the memory here.
	 */
	if (--frozen_bhp->ref == 0) {
		MUTEX_UNLOCK(env, hp->mtx_hash);

		if (alloc_bhp == NULL && frozen_bhp->td_off != INVALID_ROFF &&
		    (ret = __txn_remove_buffer(env,
		    BH_OWNER(env, frozen_bhp), MUTEX_INVALID)) != 0) {
			(void)__env_panic(env, ret);
			goto err;
		}

		/*
		 * We need to be careful in the error case, because our caller
		 * will attempt to free frozen_bhp.
		 */
		MPOOL_REGION_LOCK(env, infop);
		SH_TAILQ_INSERT_TAIL(&c_mp->free_frozen, frozen_bhp, hq);
		MPOOL_REGION_UNLOCK(env, infop);
		MUTEX_LOCK(env, hp->mtx_hash);
	} else {
		F_SET(frozen_bhp, BH_THAWED);
		F_CLR(frozen_bhp, BH_LOCKED);
	}

#ifdef HAVE_STATISTICS
	if (alloc_bhp != NULL)
		++hp->hash_thawed;
	else
		++hp->hash_frozen_freed;
#endif

	if (0) {
err:		if (ret == 0)
			ret = EIO;
	}
	if (real_name != NULL)
		__os_free(env, real_name);
	if (freelist != NULL)
		__os_free(env, freelist);
	if (fhp != NULL &&
	    (t_ret = __os_closehandle(env, fhp)) != 0 && ret == 0)
		ret = t_ret;
	if (ret != 0)
		__db_err(env, ret, "__memp_bh_thaw");

	/*
	 * If a thread of control is waiting on this buffer, wake it up.
	 */
	if (F_ISSET(hp, IO_WAITER)) {
		F_CLR(hp, IO_WAITER);
		MUTEX_UNLOCK(env, hp->mtx_io);
	}

	return (ret);
}
