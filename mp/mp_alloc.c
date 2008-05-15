/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996,2008 Oracle.  All rights reserved.
 *
 * $Id: mp_alloc.c,v 12.43 2008/04/21 14:39:57 carol Exp $
 */

#include "db_config.h"

#include "db_int.h"
#include "dbinc/mp.h"
#include "dbinc/txn.h"

static void __memp_bad_buffer __P((DB_MPOOL_HASH *));

/*
 * __memp_alloc --
 *	Allocate some space from a cache region.
 *
 * PUBLIC: int __memp_alloc __P((DB_MPOOL *,
 * PUBLIC:     REGINFO *, MPOOLFILE *, size_t, roff_t *, void *));
 */
int
__memp_alloc(dbmp, infop, mfp, len, offsetp, retp)
	DB_MPOOL *dbmp;
	REGINFO *infop;
	MPOOLFILE *mfp;
	size_t len;
	roff_t *offsetp;
	void *retp;
{
	BH *bhp, *mvcc_bhp, *t1bhp, *t2bhp, *t3bhp;
	BH_FROZEN_PAGE *frozen_bhp;
	DB_LSN vlsn;
	DB_MPOOL_HASH *dbht, *hp, *hp_end, *hp_saved, *hp_tmp;
	ENV *env;
	MPOOL *c_mp;
	MPOOLFILE *bh_mfp;
	size_t freed_space;
	u_int32_t buckets, buffers, high_priority, priority, priority_saved;
	u_int32_t put_counter, total_buckets;
	int aggressive, alloc_freeze, giveup, got_oldest, ret;
	u_int8_t *endp;
	void *p;

	env = dbmp->env;
	c_mp = infop->primary;
	dbht = R_ADDR(infop, c_mp->htab);
	hp_end = &dbht[c_mp->htab_buckets];
	hp_saved = NULL;
	priority_saved = 0;

	buckets = buffers = put_counter = total_buckets = 0;
	aggressive = alloc_freeze = giveup = got_oldest = 0;

	STAT(c_mp->stat.st_alloc++);

	/*
	 * If we're allocating a buffer, and the one we're discarding is the
	 * same size, we don't want to waste the time to re-integrate it into
	 * the shared memory free list.  If the DB_MPOOLFILE argument isn't
	 * NULL, we'll compare the underlying page sizes of the two buffers
	 * before free-ing and re-allocating buffers.
	 */
	if (mfp != NULL) {
		len = SSZA(BH, buf) + mfp->stat.st_pagesize;
		/* Add space for alignment padding for MVCC diagnostics. */
		MVCC_BHSIZE(mfp, len);
	}

	MPOOL_REGION_LOCK(env, infop);

	/*
	 * Anything newer than 1/10th of the buffer pool is ignored during
	 * allocation (unless allocation starts failing).
	 */
	high_priority = c_mp->lru_count - c_mp->stat.st_pages / 10;

	/*
	 * First we try to allocate from free memory.  If that fails, scan the
	 * buffer pool to find buffers with low priorities.  We consider small
	 * sets of hash buckets each time to limit the amount of work needing
	 * to be done.  This approximates LRU, but not very well.  We either
	 * find a buffer of the same size to use, or we will free 3 times what
	 * we need in the hopes it will coalesce into a contiguous chunk of the
	 * right size.  In the latter case we branch back here and try again.
	 */
alloc:	if ((ret = __env_alloc(infop, len, &p)) == 0) {
		if (mfp != NULL)
			c_mp->stat.st_pages++;
		MPOOL_REGION_UNLOCK(env, infop);
		/*
		 * For MVCC diagnostics, align the pointer so that the buffer
		 * starts on a page boundary.
		 */
		MVCC_BHALIGN(mfp, p);

found:		if (offsetp != NULL)
			*offsetp = R_OFFSET(infop, p);
		*(void **)retp = p;

		/*
		 * Update the search statistics.
		 *
		 * We're not holding the region locked here, these statistics
		 * can't be trusted.
		 */
#ifdef HAVE_STATISTICS
		total_buckets += buckets;
		if (total_buckets != 0) {
			if (total_buckets > c_mp->stat.st_alloc_max_buckets)
				c_mp->stat.st_alloc_max_buckets = total_buckets;
			c_mp->stat.st_alloc_buckets += total_buckets;
		}
		if (buffers != 0) {
			if (buffers > c_mp->stat.st_alloc_max_pages)
				c_mp->stat.st_alloc_max_pages = buffers;
			c_mp->stat.st_alloc_pages += buffers;
		}
#endif
		return (0);
	} else if (giveup || c_mp->stat.st_pages == 0) {
		MPOOL_REGION_UNLOCK(env, infop);

		__db_errx(env,
		    "unable to allocate space from the buffer cache");
		return (ret);
	}
	ret = 0;

	/*
	 * We re-attempt the allocation every time we've freed 3 times what
	 * we need.  Reset our free-space counter.
	 */
	freed_space = 0;
	total_buckets += buckets;
	buckets = 0;

	/*
	 * Walk the hash buckets and find the next two with potentially useful
	 * buffers.  Free the buffer with the lowest priority from the buckets'
	 * chains.
	 */
	for (;;) {
		/* All pages have been freed, make one last try */
		if (c_mp->stat.st_pages == 0)
			goto alloc;

		/* Check for wrap around. */
		hp = &dbht[c_mp->last_checked++];
		if (hp >= hp_end) {
			c_mp->last_checked = 0;
			hp = &dbht[c_mp->last_checked++];
		}

		/*
		 * The failure mode is when there are too many buffers we can't
		 * write or there's not enough memory in the system to support
		 * the number of pinned buffers.
		 *
		 * Get aggressive if we've reviewed the entire cache without
		 * freeing the needed space.  (The code resets "aggressive"
		 * when we free any space.)  Aggressive means:
		 *
		 * a: set a flag to attempt to flush high priority buffers as
		 *    well as other buffers.
		 * b: sync the mpool to force out queue extent pages.  While we
		 *    might not have enough space for what we want and flushing
		 *    is expensive, why not?
		 * c: look at a buffer in every hash bucket rather than choose
		 *    the more preferable of two.
		 * d: start to think about giving up.
		 *
		 * If we get here twice, sleep for a second, hopefully someone
		 * else will run and free up some memory.
		 *
		 * Always try to allocate memory too, in case some other thread
		 * returns its memory to the region.
		 *
		 * We don't have any way to know an allocation has no way to
		 * succeed.  Fail if no pages are returned to the cache after
		 * we've been trying for a relatively long time.
		 *
		 * !!!
		 * This test ignores pathological cases like no buffers in the
		 * system -- we check for that early on, so it isn't possible.
		 */
		if (buckets++ == c_mp->htab_buckets) {
			if (freed_space > 0)
				goto alloc;
			MPOOL_REGION_UNLOCK(env, infop);

			switch (++aggressive) {
			case 1:
				break;
			case 2:
				put_counter = c_mp->put_counter;
				/* FALLTHROUGH */
			case 3:
			case 4:
			case 5:
			case 6:
				(void)__memp_sync_int(
				    env, NULL, 0, DB_SYNC_ALLOC, NULL, NULL);

				__os_yield(env, 1, 0);
				break;
			default:
				aggressive = 1;
				if (put_counter == c_mp->put_counter)
					giveup = 1;
				break;
			}

			MPOOL_REGION_LOCK(env, infop);
			goto alloc;
		}

		/*
		 * Skip empty buckets.
		 *
		 * We can check for empty buckets before locking the hash
		 * bucket as we only care if the pointer is zero or non-zero.
		 */
		if (SH_TAILQ_FIRST(&hp->hash_bucket, __bh) == NULL)
			continue;

		/* Unlock the region and lock the hash bucket. */
		MPOOL_REGION_UNLOCK(env, infop);
		MUTEX_LOCK(env, hp->mtx_hash);

		/*
		 * Find a buffer we can use.
		 *
		 * We don't want to free a buffer out of the middle of an MVCC
		 * chain (that requires I/O).  So, walk the buffers, looking
		 * for, in order of preference:
		 *	an obsolete buffer at the end of an MVCC chain,
		 *	the lowest-LRU singleton buffer, and
		 *	the lowest LRU-buffer of all.
		 * We use an obsolete buffer at the end of a chain as soon as
		 * we find one.  We use the lowest-LRU singleton buffer if we
		 * find one and it's better than the result of another hash
		 * bucket we've reviewed.  We use the lowest-LRU buffer we find
		 * if it's lower than another hash bucket we've reviewed and
		 * we're being aggressive.
		 *
		 * Ignore referenced buffers, we can't get rid of them.
		 */
retry_search:	bhp = mvcc_bhp = NULL;
		SH_TAILQ_FOREACH(t1bhp, &hp->hash_bucket, hq, __bh) {
			/*
			 * It's a single buffer (not an MVCC chain).
			 *
			 * If the buffer is not in use, its LRU is one we'll
			 * consider at this point in our search, and it's a
			 * better LRU than we've found so far, remember it.
			 */
			if (SH_CHAIN_SINGLETON(t1bhp, vc)) {
				if (t1bhp->ref == 0 &&
				    (aggressive ||
				    t1bhp->priority < high_priority) &&
				    (bhp == NULL ||
				    bhp->priority > t1bhp->priority))
					bhp = t1bhp;
				continue;
			}

			/*
			 * It's an MVCC chain.
			 */
			t2bhp = t1bhp;
			do {
				t3bhp = t2bhp;

				/*
				 * If the buffer is not in use, its LRU is one
				 * we'll consider at this point, and it's a
				 * better LRU than we've found so far, remember
				 * it.  The "LRU is OK" check is simpler here
				 * because we'll only consider a MVCC buffer if
				 * we're being aggressive.
				 */
				if (t2bhp->ref == 0 &&
				    aggressive &&
				    (mvcc_bhp == NULL ||
				    mvcc_bhp->priority > t2bhp->priority))
					mvcc_bhp = t2bhp;
			} while
			    ((t2bhp = SH_CHAIN_PREV(t2bhp, vc, __bh)) != NULL);

			/*
			 * t3bhp is the last buffer on the MVCC chain, and
			 * an obsolete buffer at the end of the MVCC chain
			 * gets used without further search.
			 *
			 * If the buffer isn't obsolete with respect to the
			 * cached old reader LSN, recalculate the oldest
			 * reader LSN and check again.
			 */
retry_obsolete:		if (BH_OBSOLETE(t3bhp, hp->old_reader, vlsn)) {
				bhp = t3bhp;
				goto this_buffer;
			}
			if (!got_oldest) {
				if ((ret = __txn_oldest_reader(
				    env, &hp->old_reader)) != 0)
					return (ret);
				got_oldest = 1;
				goto retry_obsolete;
			}
		}

		/*
		 * bhp is either NULL or the lowest-LRU singleton buffer.
		 * mvcc_bhp is either NULL or the lowest-LRU MVCC buffer.
		 * In both cases, we'll use the chosen buffer only if we
		 * have compared its LRU against the chosen LRU of another
		 * hash bucket.
		 */
		if (bhp == NULL) {
			if (mvcc_bhp == NULL)
				goto next_hb;
			bhp = mvcc_bhp;
		}

		/* Adjust the priority if the bucket has not been reset. */
		priority = bhp->priority;
		if (c_mp->lru_reset != 0 && c_mp->lru_reset <= hp - dbht)
			priority -= MPOOL_BASE_DECREMENT;

		/*
		 * Compare two hash buckets and select the one with the lowest
		 * priority.  Performance testing shows looking at two improves
		 * the LRU-ness and looking at more only does a little better.
		 */
		if (hp_saved == NULL) {
			hp_saved = hp;
			priority_saved = priority;
			goto next_hb;
		}

		/*
		 * If the buffer we just found is a better choice than our
		 * previous choice, use it.
		 *
		 * If the previous choice was better, pretend we're moving
		 * from this hash bucket to the previous one and re-do the
		 * search.
		 *
		 * We don't worry about simply swapping between two buckets
		 * because that could only happen if a buffer was removed
		 * from the chain, or its priority updated.   If a buffer
		 * is removed from the chain, some other thread has managed
		 * to discard a buffer, so we're moving forward.  Updating
		 * a buffer's priority will make it a high-priority buffer,
		 * so we'll ignore it when we search again, and so we will
		 * eventually zero in on a buffer to use, or we'll decide
		 * there are no buffers we can use.
		 *
		 * If there's only a single hash bucket with buffers, we'll
		 * search the bucket once, choose a buffer, walk the entire
		 * list of buckets and search it again.   In the case of a
		 * system that's busy, it's possible to imagine a case where
		 * we'd loop for a long while.  For that reason, and because
		 * the test is easy, we special case and test for it.
		 */
		if (priority > priority_saved && hp != hp_saved) {
			MUTEX_UNLOCK(env, hp->mtx_hash);
			hp_tmp = hp_saved;
			hp_saved = hp;
			hp = hp_tmp;
			priority_saved = priority;
			MUTEX_LOCK(env, hp->mtx_hash);
			goto retry_search;
		}

this_buffer:	buffers++;

		/*
		 * Discard any previously remembered hash bucket, we've got
		 * a winner.
		 */
		hp_saved = NULL;

		/* Find the associated MPOOLFILE. */
		bh_mfp = R_ADDR(dbmp->reginfo, bhp->mf_offset);

		/* If the page is dirty, pin it and write it. */
		ret = 0;
		if (F_ISSET(bhp, BH_DIRTY)) {
			++bhp->ref;
			ret = __memp_bhwrite(dbmp, hp, bh_mfp, bhp, 0);
			--bhp->ref;
#ifdef HAVE_STATISTICS
			if (ret == 0)
				++c_mp->stat.st_rw_evict;
#endif
		}
#ifdef HAVE_STATISTICS
		else
			++c_mp->stat.st_ro_evict;
#endif

		/*
		 * Freeze this buffer, if necessary.  That is, if the buffer
		 * itself or the next version created could be read by the
		 * oldest reader in the system.
		 */
		if (ret == 0 && bh_mfp->multiversion) {
			if (!got_oldest && !SH_CHAIN_HASPREV(bhp, vc) &&
			    !BH_OBSOLETE(bhp, hp->old_reader, vlsn)) {
				(void)__txn_oldest_reader(env,
				    &hp->old_reader);
				got_oldest = 1;
			}
			if (SH_CHAIN_HASPREV(bhp, vc) ||
			    !BH_OBSOLETE(bhp, hp->old_reader, vlsn)) {
				/*
				 * Before freezing, double-check that we have
				 * an up-to-date old_reader LSN.
				 */
				if (!aggressive ||
				    F_ISSET(bhp, BH_FROZEN) || bhp->ref != 0)
					goto next_hb;
				ret = __memp_bh_freeze(dbmp,
				    infop, hp, bhp, &alloc_freeze);
			}
		}

		/*
		 * If a write fails for any reason, we can't proceed.
		 *
		 * We released the hash bucket lock while doing I/O, so another
		 * thread may have acquired this buffer and incremented the ref
		 * count after we wrote it, in which case we can't have it.
		 *
		 * If there's a write error and we're having problems finding
		 * something to allocate, avoid selecting this buffer again
		 * by making it the bucket's least-desirable buffer.
		 */
		if (ret != 0 || bhp->ref != 0) {
			if (ret != 0 && aggressive)
				__memp_bad_buffer(hp);
			goto next_hb;
		}

		/*
		 * If the buffer is frozen, thaw it and look for another one
		 * we can use.
		 */
		if (F_ISSET(bhp, BH_FROZEN)) {
			++bhp->ref;
			if ((ret = __memp_bh_thaw(dbmp, infop, hp,
			    bhp, NULL)) != 0) {
				MUTEX_UNLOCK(env, hp->mtx_hash);
				return (ret);
			}
			alloc_freeze = 0;
			goto retry_search;
		}

		/*
		 * If we need some empty buffer headers for freezing, turn the
		 * buffer we've found into frozen headers and put them on the
		 * free list.  Only reset alloc_freeze if we've actually
		 * allocated some frozen buffer headers.
		 */
		if (alloc_freeze) {
			if ((ret = __memp_bhfree(dbmp, infop, hp, bhp, 0)) != 0)
				return (ret);
			MVCC_MPROTECT(bhp->buf, bh_mfp->stat.st_pagesize,
			    PROT_READ | PROT_WRITE | PROT_EXEC);

			MPOOL_REGION_LOCK(env, infop);
			SH_TAILQ_INSERT_TAIL(&c_mp->alloc_frozen,
			    (BH_FROZEN_ALLOC *)bhp, links);
			frozen_bhp = (BH_FROZEN_PAGE *)
			    ((BH_FROZEN_ALLOC *)bhp + 1);
			endp = (u_int8_t *)bhp->buf + bh_mfp->stat.st_pagesize;
			while ((u_int8_t *)(frozen_bhp + 1) < endp) {
				SH_TAILQ_INSERT_TAIL(&c_mp->free_frozen,
				    (BH *)frozen_bhp, hq);
				frozen_bhp++;
			}
			alloc_freeze = 0;
			continue;
		}

		/*
		 * Check to see if the buffer is the size we're looking for.
		 * If so, we can simply reuse it.  Otherwise, free the buffer
		 * and its space and keep looking.
		 */
		if (mfp != NULL &&
		    mfp->stat.st_pagesize == bh_mfp->stat.st_pagesize) {
			if ((ret = __memp_bhfree(dbmp, infop, hp, bhp, 0)) != 0)
				return (ret);
			p = bhp;
			goto found;
		}

		freed_space += sizeof(*bhp) + bh_mfp->stat.st_pagesize;
		if ((ret =
		    __memp_bhfree(dbmp, infop, hp, bhp, BH_FREE_FREEMEM)) != 0)
			return (ret);

		/* Reset "aggressive" if we free any space. */
		if (aggressive > 1)
			aggressive = 1;

		/*
		 * Unlock this hash bucket and re-acquire the region lock. If
		 * we're reaching here as a result of calling memp_bhfree, the
		 * hash bucket lock has already been discarded.
		 */
		if (0) {
next_hb:		MUTEX_UNLOCK(env, hp->mtx_hash);
		}
		MPOOL_REGION_LOCK(env, infop);

		/*
		 * Retry the allocation as soon as we've freed up sufficient
		 * space.  We're likely to have to coalesce of memory to
		 * satisfy the request, don't try until it's likely (possible?)
		 * we'll succeed.
		 */
		if (freed_space >= 3 * len)
			goto alloc;
	}
	/* NOTREACHED */
}

/*
 * __memp_free --
 *	Free some space from a cache region.
 *
 * PUBLIC: void __memp_free __P((REGINFO *, MPOOLFILE *, void *));
 */
void
__memp_free(infop, mfp, buf)
	REGINFO *infop;
	MPOOLFILE *mfp;
	void *buf;
{
	MVCC_BHUNALIGN(mfp, buf);
	COMPQUIET(mfp, NULL);
	__env_alloc_free(infop, buf);
}

/*
 * __memp_bad_buffer --
 *	Make the first buffer in a hash bucket the least desirable buffer.
 */
static void
__memp_bad_buffer(hp)
	DB_MPOOL_HASH *hp;
{
	BH *bhp, *last_bhp;
	u_int32_t priority;

	/*
	 * Get the first buffer from the bucket.  If it is also the last buffer
	 * (in other words, it is the only buffer in the bucket), we're done.
	 */
	bhp = SH_TAILQ_FIRST(&hp->hash_bucket, __bh);
	last_bhp = SH_TAILQ_LASTP(&hp->hash_bucket, hq, __bh);
	if (bhp == last_bhp)
		return;

	/* There are multiple buffers in the bucket, remove the first one. */
	SH_TAILQ_REMOVE(&hp->hash_bucket, bhp, hq, __bh);

	/*
	 * Find the highest priority buffer in the bucket.  Buffers are
	 * sorted by priority, so it's the last one in the bucket.
	 */
	priority = BH_PRIORITY(last_bhp);

	/*
	 * Append our buffer to the bucket and set its priority to be just as
	 * bad.
	 */
	SH_TAILQ_INSERT_TAIL(&hp->hash_bucket, bhp, hq);
	for (; bhp != NULL ; bhp = SH_CHAIN_PREV(bhp, vc, __bh))
		bhp->priority = priority;
}
