/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 2013 Oracle and/or its affiliates.  All rights reserved.
 *
 * $Id$
 */

#include "db_config.h"

#include "db_int.h"
#include "dbinc/lock.h"
#include "dbinc/log.h"
#include "dbinc/txn.h"

static int __lock_freelocker_int
    __P((DB_LOCKTAB *, DB_LOCKREGION *, DB_LOCKER *, int));

/*
 * __lock_locker_mutex_reuse --
 *	P13: is a freed locker allowed to retain its mtx_locker for the next
 *	locker that recycles the slot?
 *
 *	On by default.  DB_NO_LOCKER_MUTEX_REUSE restores the pre-P13 behaviour
 *	(one __mutex_alloc + one __mutex_free per locker, each taking the single
 *	global MUTEX_SYSTEM_LOCK) so the A/B can be run on ONE binary with only a
 *	runtime switch varying -- the form test/bench/run_bench.sh's DB_PRIVATE
 *	warning requires.  Cached in a process-local static, like the
 *	DB_NO_OPTREAD and DB_NO_GROUP_COMMIT switches; this is a pure
 *	performance choice with no region-visible consequence, so unlike P12's
 *	locker_shard it need not be stored in the region for attachers to
 *	inherit: two processes disagreeing about it simply allocate mutexes at
 *	different rates.
 */
static int
__lock_locker_mutex_reuse()
{
	static int cached = -1;

	if (cached == -1)
		cached = getenv("DB_NO_LOCKER_MUTEX_REUSE") != NULL ? 0 : 1;
	return (cached);
}

/*
 * __lock_id_pp --
 *	ENV->lock_id pre/post processing.
 *
 * PUBLIC: int __lock_id_pp __P((DB_ENV *, u_int32_t *));
 */
int
__lock_id_pp(dbenv, idp)
	DB_ENV *dbenv;
	u_int32_t *idp;
{
	DB_THREAD_INFO *ip;
	ENV *env;
	int ret;

	env = dbenv->env;

	ENV_REQUIRES_CONFIG(env,
	    env->lk_handle, "DB_ENV->lock_id", DB_INIT_LOCK);

	ENV_ENTER(env, ip);
	REPLICATION_WRAP(env, (__lock_id(env, idp, NULL)), 0, ret);
	ENV_LEAVE(env, ip);
	return (ret);
}

/*
 * __lock_id --
 *	ENV->lock_id.
 *
 * PUBLIC: int  __lock_id __P((ENV *, u_int32_t *, DB_LOCKER **));
 */
int
__lock_id(env, idp, lkp)
	ENV *env;
	u_int32_t *idp;
	DB_LOCKER **lkp;
{
	DB_LOCKER *lk;
	DB_LOCKREGION *region;
	DB_LOCKTAB *lt;
	u_int32_t id, *ids, s;
	int nids, ret;

	lk = NULL;
	lt = env->lk_handle;
	region = lt->reginfo.primary;
	id = DB_LOCK_INVALIDID;
	ret = 0;

	id = DB_LOCK_INVALIDID;
	lk = NULL;

	/*
	 * The id counter and the wraparound rebuild live under stripe 0
	 * (LOCK_LOCKER_ID_ALLOC): an id's own stripe cannot guard them because
	 * the stripe is not known until the id is chosen.  The rebuild walks
	 * every stripe's ulinks list, so it escalates to all stripes; the
	 * common path takes stripe 0 plus the one bucket the new locker hashes
	 * to.  Race-freedom for lock_id/cur_maxid: every read and every write
	 * of either counter in this function happens with stripe 0 held, and
	 * stripe 0 is held continuously from before the wraparound test to
	 * after the increment, so no concurrent bump can slip between the test
	 * and the ++.  __lock_id_set is the only other writer and is
	 * documented single-threaded test-only; lock_stat reads them under
	 * LOCK_REGION_LOCK for reporting, where a torn read is harmless.
	 */
	LOCK_LOCKER_ID_ALLOC(env, region);

	/*
	 * Allocate a new lock id.  If we wrap around then we find the minimum
	 * currently in use and make sure we can stay below that.  This code is
	 * similar to code in __txn_begin_int for recovering txn ids.
	 *
	 * Our current valid range can span the maximum valid value, so check
	 * for it and wrap manually.
	 */
	if (region->lock_id == DB_LOCK_MAXID &&
	    region->cur_maxid != DB_LOCK_MAXID)
		region->lock_id = DB_LOCK_INVALIDID;
	if (region->lock_id == region->cur_maxid) {
		/*
		 * Wraparound: walk every locker.  The in-use ulinks lists are
		 * per stripe, so take the remaining stripes and hold the whole
		 * table for the rebuild.  Rare by construction (once per 2^31
		 * ids).
		 */
		LOCK_LOCKERS_REST(env, region);
		if ((ret = __os_malloc(env, sizeof(u_int32_t) *
		    __lock_nlockers(region), &ids)) != 0) {
			UNLOCK_LOCKERS_REST(env, region);
			goto err;
		}
		nids = 0;
		for (s = 0; s < LOCK_LOCKER_STRIPES; s++)
			SH_TAILQ_FOREACH(lk,
			    &region->locker_stripe[s].lockers,
			    ulinks, __db_locker)
				ids[nids++] = lk->id;
		region->lock_id = DB_LOCK_INVALIDID;
		region->cur_maxid = DB_LOCK_MAXID;
		if (nids != 0)
			__db_idspace(ids, nids,
			    &region->lock_id, &region->cur_maxid);
		__os_free(env, ids);
		UNLOCK_LOCKERS_REST(env, region);
	}
	id = ++region->lock_id;

	/*
	 * Allocate a locker for this id.  __lock_getlocker_int inserts into
	 * locker_tab[id's bucket] and pops that same stripe's free list, so the
	 * one bucket stripe is exactly the coverage it needs -- UNLESS that
	 * stripe's free list is empty, in which case it refills, which drops
	 * and retakes ALL the locker latches and touches region state no single
	 * stripe covers.
	 *
	 * As in __lock_getlocker, the emptiness test is made with the BUCKET
	 * STRIPE held, not merely with stripe 0 held: for any id whose bucket
	 * does not map onto stripe 0, stripe 0 says nothing about that stripe's
	 * free list, and a caller that entered the refill branch holding less
	 * than every stripe would release latches it does not hold.  Stripe 0
	 * stays held throughout because it guards the id counters.
	 */
	LOCK_LOCKER_BUCKET(env, region, id);
	if (SH_TAILQ_FIRST(&LOCK_LOCKER_ID_LS(region, id)->free_lockers,
	    __db_locker) == NULL) {
		UNLOCK_LOCKER_BUCKET(env, region, id);
		LOCK_LOCKERS_REST(env, region);
		ret = __lock_getlocker_int(lt, id, 1, &lk);
		UNLOCK_LOCKERS_REST(env, region);
	} else {
		ret = __lock_getlocker_int(lt, id, 1, &lk);
		UNLOCK_LOCKER_BUCKET(env, region, id);
	}

err:	UNLOCK_LOCKER_ID_ALLOC(env, region);

	if (idp != NULL)
		*idp = id;
	if (lkp != NULL)
		*lkp = lk;

	return (ret);
}

/*
 * __lock_set_thread_id --
 *	Set the thread_id in an existing locker.
 * PUBLIC: void __lock_set_thread_id __P((void *, pid_t, db_threadid_t));
 */
void
__lock_set_thread_id(lref_arg, pid, tid)
	void *lref_arg;
	pid_t pid;
	db_threadid_t tid;
{
	DB_LOCKER *lref;

	lref = lref_arg;
	lref->pid = pid;
	lref->tid = tid;
}

/*
 * __lock_id_free_pp --
 *	ENV->lock_id_free pre/post processing.
 *
 * PUBLIC: int __lock_id_free_pp __P((DB_ENV *, u_int32_t));
 */
int
__lock_id_free_pp(dbenv, id)
	DB_ENV *dbenv;
	u_int32_t id;
{
	DB_LOCKER *sh_locker;
	DB_LOCKREGION *region;
	DB_LOCKTAB *lt;
	DB_THREAD_INFO *ip;
	ENV *env;
	int handle_check, ret, t_ret;

	env = dbenv->env;

	ENV_REQUIRES_CONFIG(env,
	    env->lk_handle, "DB_ENV->lock_id_free", DB_INIT_LOCK);

	ENV_ENTER(env, ip);

	/* Check for replication block. */
	handle_check = IS_ENV_REPLICATED(env);
	if (handle_check && (ret = __env_rep_enter(env, 0)) != 0) {
		handle_check = 0;
		goto err;
	}

	lt = env->lk_handle;
	region = lt->reginfo.primary;

	LOCK_LOCKERS(env, region);
	if ((ret =
	     __lock_getlocker_int(env->lk_handle, id, 0, &sh_locker)) == 0) {
		if (sh_locker != NULL)
			ret = __lock_freelocker_int(lt, region, sh_locker, 1);
		else {
			__db_errx(env, DB_STR_A("2045",
			    "Unknown locker id: %lx", "%lx"), (u_long)id);
			ret = EINVAL;
		}
	}
	UNLOCK_LOCKERS(env, region);

	if (handle_check && (t_ret = __env_db_rep_exit(env)) != 0 && ret == 0)
		ret = t_ret;

err:	ENV_LEAVE(env, ip);
	return (ret);
}

/*
 * __lock_id_free --
 *	Free a locker id.
 *
 * PUBLIC: int  __lock_id_free __P((ENV *, DB_LOCKER *));
 */
int
__lock_id_free(env, sh_locker)
	ENV *env;
	DB_LOCKER *sh_locker;
{
	DB_LOCKREGION *region;
	DB_LOCKTAB *lt;
	int ret;

	lt = env->lk_handle;
	region = lt->reginfo.primary;
	ret = 0;

	if (sh_locker->nlocks != 0) {
		__db_errx(env, DB_STR("2046",
		    "Locker still has locks"));
		ret = EINVAL;
		goto err;
	}

	LOCK_LOCKERS(env, region);
	ret = __lock_freelocker_int(lt, region, sh_locker, 1);
	UNLOCK_LOCKERS(env, region);

err:	return (ret);
}

/*
 * __lock_id_set --
 *	Set the current locker ID and current maximum unused ID (for
 *	testing purposes only).
 *
 * PUBLIC: int __lock_id_set __P((ENV *, u_int32_t, u_int32_t));
 */
int
__lock_id_set(env, cur_id, max_id)
	ENV *env;
	u_int32_t cur_id, max_id;
{
	DB_LOCKREGION *region;
	DB_LOCKTAB *lt;

	ENV_REQUIRES_CONFIG(env,
	    env->lk_handle, "lock_id_set", DB_INIT_LOCK);

	lt = env->lk_handle;
	region = lt->reginfo.primary;
	region->lock_id = cur_id;
	region->cur_maxid = max_id;

	return (0);
}

/*
 * __lock_getlocker --
 *	Get a locker in the locker hash table.  The create parameter
 * indicates if the locker should be created if it doesn't exist in
 * the table.
 *
 * This must be called with the locker mutex lock if create == 1.
 *
 * PUBLIC: int __lock_getlocker __P((DB_LOCKTAB *,
 * PUBLIC:     u_int32_t, int, DB_LOCKER **));
 * PUBLIC: int __lock_getlocker_int __P((DB_LOCKTAB *,
 * PUBLIC:     u_int32_t, int, DB_LOCKER **));
 */
int
__lock_getlocker(lt, locker, create, retp)
	DB_LOCKTAB *lt;
	u_int32_t locker;
	int create;
	DB_LOCKER **retp;
{
	DB_LOCKREGION *region;
	ENV *env;
	int ret;

	COMPQUIET(region, NULL);
	env = lt->env;
	region = lt->reginfo.primary;

	/*
	 * A create may have to refill this stripe's locker free list, which
	 * drops and retakes ALL the locker latches and touches region-wide
	 * state; a lookup never does.  So test the stripe's free list and
	 * escalate to all stripes only when it is empty.
	 *
	 * THE TEST MUST HAPPEN UNDER THE STRIPE LATCH, which is why the latch
	 * is taken first and the branch is inside it.  An unlatched test here
	 * is not merely racy, it is unsound: the refill branch inside
	 * __lock_getlocker_int begins with UNLOCK_LOCKERS, so a caller that
	 * reached it holding ONE stripe would release 64 latches it does not
	 * hold.  Reading the list before the acquire leaves exactly that
	 * window -- another holder of this stripe can pop the last entry in
	 * it -- so the observation has to be made with the stripe held, after
	 * which no other thread can change the list (its only consumers are
	 * holders of that stripe: create pops, free pushes, refill fills).
	 *
	 * Dropping the stripe to escalate is safe: __lock_getlocker_int looks
	 * the locker up again before creating it, so another thread creating
	 * the same id in the gap is found rather than duplicated.  That is the
	 * same tolerance the refill path has always had, since it drops every
	 * stripe across __env_alloc regardless.
	 *
	 * This is the P12 fix.  Before it this path took stripe 0 -- a single
	 * global latch -- for the whole call, because the free list, the ulinks
	 * list and nlockers were region-wide.  All three are now per stripe, so
	 * this is the hot txn_begin path (txn.c:588) at ONE uncontended latch
	 * per call.
	 */
	LOCK_LOCKER_WRITE(env, region, locker);
	if (create && SH_TAILQ_FIRST(&LOCK_LOCKER_ID_LS(region,
	    locker)->free_lockers, __db_locker) == NULL) {
		UNLOCK_LOCKER_WRITE(env, region, locker);
		LOCK_LOCKERS(env, region);
		ret = __lock_getlocker_int(lt, locker, create, retp);
		UNLOCK_LOCKERS(env, region);
	} else {
		ret = __lock_getlocker_int(lt, locker, create, retp);
		UNLOCK_LOCKER_WRITE(env, region, locker);
	}

	return (ret);
}

/*
 * __lock_nlockers --
 *	Sum the per-stripe in-use locker counts.
 *
 * nlockers was a single region-wide counter bumped under stripe 0 on every
 * locker create and free -- one of the three per-transaction mutations of
 * shared state that made stripe 0 a global serialization point (P12).  It is
 * read only for statistics (st_nlockers), for the st_maxnlockers watermark,
 * and to size the deadlock detector's arrays, so summing N small per-stripe
 * counters on those rare paths is strictly cheaper than keeping one hot one.
 *
 * Callers must hold enough of the locker latches for the answer to mean what
 * they need it to.  The deadlock detector holds LOCK_LOCKERS, so its sum is
 * exact and stays exact for the walk that follows -- which matters, because it
 * sizes a bitmap the walk then indexes.  lock_stat reads it under
 * LOCK_REGION_LOCK only, so its value may be skewed across stripes; that is
 * the same latitude the old single counter had (it was read without stripe 0)
 * and is harmless for a reported statistic.
 *
 * PUBLIC: u_int32_t __lock_nlockers __P((DB_LOCKREGION *));
 */
u_int32_t
__lock_nlockers(region)
	DB_LOCKREGION *region;
{
	u_int32_t n, s;

	for (n = 0, s = 0; s < LOCK_LOCKER_STRIPES; s++)
		n += region->locker_stripe[s].nlockers;
	return (n);
}

/*
 * __lock_maxnlockers --
 *	Sum the per-stripe in-use locker watermarks.
 *
 * The sum of the per-stripe peaks, which is an upper bound on (not necessarily
 * equal to) the peak of the sum, since the stripes need not peak together.
 * st_maxnlockers is an operator-facing "how close did I come to the ceiling"
 * figure, so erring high is the safe direction; a single exact region-wide
 * watermark is what P12 removed from the per-transaction path.
 *
 * PUBLIC: u_int32_t __lock_maxnlockers __P((DB_LOCKREGION *));
 */
u_int32_t
__lock_maxnlockers(region)
	DB_LOCKREGION *region;
{
	u_int32_t n, s;

	for (n = 0, s = 0; s < LOCK_LOCKER_STRIPES; s++)
		n += region->locker_stripe[s].maxnlockers;
	return (n);
}

int
__lock_getlocker_int(lt, locker, create, retp)
	DB_LOCKTAB *lt;
	u_int32_t locker;
	int create;
	DB_LOCKER **retp;
{
	DB_LOCKER *sh_locker;
	DB_LOCKERSTRIPE *ls;
	DB_LOCKREGION *region;
	DB_THREAD_INFO *ip;
	ENV *env;
	db_mutex_t mutex;
	u_int32_t i, indx, nlockers, s;
	int ret;

	env = lt->env;
	region = lt->reginfo.primary;

	LOCKER_HASH(lt, region, locker, indx);
	/*
	 * The stripe that owns locker_tab[indx] also owns the free list, the
	 * in-use ulinks list and the counters this function mutates (P12), so
	 * one latch covers everything below.  Derived from the BUCKET, which is
	 * what LOCK_LOCKER_ID_STRIPE does, so the caller's latch and this
	 * pointer always agree.
	 */
	ls = &region->locker_stripe[LOCK_LOCKER_ALLOC_STRIPE(region,
	    LOCK_LOCKER_STRIPE(indx))];

	/*
	 * If we find the locker, then we can just return it.  If we don't find
	 * the locker, then we need to create it.
	 */
	SH_TAILQ_FOREACH(sh_locker, &lt->locker_tab[indx], links, __db_locker)
		if (sh_locker->id == locker)
			break;
	if (sh_locker == NULL && create) {
		nlockers = 0;
		/*
		 * Create new locker and then insert it into hash table.
		 *
		 * P13: the mutex is acquired AFTER the locker is popped, not
		 * before, because a recycled locker already carries the
		 * mtx_locker it was allocated with on a previous use --
		 * __lock_freelocker_int deliberately keeps it rather than
		 * calling __mutex_free.  Reusing it means the hot path reaches
		 * neither __mutex_alloc nor __mutex_free, and so never takes
		 * MUTEX_SYSTEM_LOCK, a single global latch that was otherwise
		 * paid twice per transaction.  Only a locker that has never
		 * been used -- off the region's initial free list, or a fresh
		 * refill batch, both of which set MUTEX_INVALID explicitly --
		 * allocates one.  See __lock_freelocker_int for why retaining
		 * a logical-lock mutex across recycling is sound.
		 */
		if ((sh_locker = SH_TAILQ_FIRST(
		    &ls->free_lockers, __db_locker)) == NULL) {
			nlockers = region->stat.st_lockers >> 2;
			/* Just in case. */
			if (nlockers == 0)
				nlockers = 1;
			if (region->stat.st_maxlockers != 0 &&
			    region->stat.st_maxlockers <
			    region->stat.st_lockers + nlockers)
				nlockers = region->stat.st_maxlockers -
				region->stat.st_lockers;
			/*
			 * Don't hold lockers when getting the region,
			 * we could deadlock.  When creating a locker
			 * there is no race since the id allocation
			 * is synchronized.
			 *
			 * This branch releases and retakes ALL stripes, so
			 * every caller that can reach it must hold all of
			 * them.  The single-bucket callers guarantee that by
			 * testing their stripe's free list first (see
			 * __lock_id, __lock_getlocker) and escalating when it
			 * is empty, which is the only way to get here.
			 */
			UNLOCK_LOCKERS(env, region);
			LOCK_REGION_LOCK(env);
			/*
			 * If the max memory is not sized for max objects,
			 * allocate as much as possible.
			 */
			F_SET(&lt->reginfo, REGION_TRACKED);
			/*
			 * Try to allocate nlockers entries; on failure halve the
			 * request and retry, down to zero.  The halving assignment
			 * (nlockers >>= 1) is essential: without it the loop either
			 * spins on the same failing size or breaks with sh_locker
			 * still NULL while nlockers is non-zero -- the insert loop
			 * below would then dereference a NULL sh_locker.  When the
			 * request cannot be satisfied at all, nlockers reaches 0 and
			 * we take the __lock_nomem path before touching sh_locker.
			 */
			while (__env_alloc(&lt->reginfo, nlockers *
			    sizeof(struct __db_locker), &sh_locker) != 0)
				if ((nlockers >>= 1) == 0)
					break;
			F_CLR(&lt->reginfo, REGION_TRACKED);
			LOCK_REGION_UNLOCK(lt->env);
			LOCK_LOCKERS(env, region);
			if (nlockers == 0)
				return (__lock_nomem(env, "locker entries"));
			/*
			 * Spread the new lockers across ALL stripes' free lists,
			 * round-robin starting at this one.
			 *
			 * Putting them all on this stripe would be wrong in a way
			 * that only shows up under load: every other stripe would
			 * stay empty, so every create on one of them would take the
			 * all-stripes refill path -- turning the escalation that is
			 * supposed to be rare into the common case and reproducing,
			 * with a wider hold, exactly the global serialization P12
			 * removes.  This stripe gets the first one so the caller
			 * that paid for the refill is guaranteed to find one.
			 *
			 * Deliberately NOT nlockers/STRIPES-per-stripe chunks: with
			 * a small nlockers (it starts at st_lockers >> 2, and the
			 * halving loop above can shrink it to 1) integer division
			 * gives zero and some stripes would get nothing.
			 */
			s = LOCK_LOCKER_ALLOC_STRIPE(region,
			    LOCK_LOCKER_STRIPE(indx));
			for (i = 0; i < nlockers; i++) {
				/*
				 * P13: __env_alloc memory is not zeroed, and
				 * the reuse test below reads mtx_locker on a
				 * locker that has never been used.  Mark it
				 * explicitly, exactly as lock_region.c does for
				 * the initial free lists.
				 */
				sh_locker->mtx_locker = MUTEX_INVALID;
				SH_TAILQ_INSERT_HEAD(
				    &region->locker_stripe[s].free_lockers,
				    sh_locker, links, __db_locker);
				s = LOCK_LOCKER_ALLOC_STRIPE(region,
				    LOCK_LOCKER_STRIPE(s + 1));
				sh_locker++;
			}
			region->stat.st_lockers += nlockers;
			sh_locker = SH_TAILQ_FIRST(
			    &ls->free_lockers, __db_locker);
		}
		SH_TAILQ_REMOVE(
		    &ls->free_lockers, sh_locker, links, __db_locker);
		/*
		 * P13: reuse this locker's retained mutex if it has one, else
		 * allocate.  Unlike the pre-P13 code this runs with the locker
		 * already popped, so on the allocation-failure path the locker
		 * must go back on its stripe's free list rather than leak.
		 */
		if ((mutex = sh_locker->mtx_locker) == MUTEX_INVALID) {
			if ((ret = __mutex_alloc(env, MTX_LOGICAL_LOCK,
			    DB_MUTEX_LOGICAL_LOCK | DB_MUTEX_SELF_BLOCK,
			    &mutex)) != 0) {
				SH_TAILQ_INSERT_HEAD(&ls->free_lockers,
				    sh_locker, links, __db_locker);
				return (ret);
			}
		}
		MUTEX_LOCK(env, mutex);
		++ls->nlockers;
#ifdef HAVE_STATISTICS
		STAT_PERFMON2(env, lock, nlockers, ls->nlockers, locker);
		if (ls->nlockers > ls->maxnlockers)
			ls->maxnlockers = ls->nlockers;
#endif
		sh_locker->id = locker;
		env->dbenv->thread_id(
		    env->dbenv, &sh_locker->pid, &sh_locker->tid);
		sh_locker->mtx_locker = mutex;
		sh_locker->dd_id = 0;
		sh_locker->td_off = INVALID_ROFF;	/* SSI: set by txn layer. */
		sh_locker->master_locker = INVALID_ROFF;
		sh_locker->parent_locker = INVALID_ROFF;
		SH_LIST_INIT(&sh_locker->child_locker);
		sh_locker->flags = 0;
		SH_LIST_INIT(&sh_locker->heldby);
		sh_locker->nlocks = 0;
		sh_locker->nwrites = 0;
		sh_locker->priority = DB_LOCK_DEFPRIORITY;
		sh_locker->lk_timeout = 0;
		timespecclear(&sh_locker->tx_expire);
		timespecclear(&sh_locker->lk_expire);

		SH_TAILQ_INSERT_HEAD(
		    &lt->locker_tab[indx], sh_locker, links, __db_locker);
		SH_TAILQ_INSERT_HEAD(&ls->lockers,
		    sh_locker, ulinks, __db_locker);
		ENV_GET_THREAD_INFO(env, ip);
#ifdef DIAGNOSTIC
		if (ip != NULL)
			ip->dbth_locker = R_OFFSET(&lt->reginfo, sh_locker);
#endif
	}

	*retp = sh_locker;
	return (0);
}

/*
 * __lock_addfamilylocker
 *	Put a locker entry in for a child transaction.
 *
 * PUBLIC: int __lock_addfamilylocker __P((ENV *,
 * PUBLIC:     u_int32_t, u_int32_t, u_int32_t));
 */
int
__lock_addfamilylocker(env, pid, id, is_family)
	ENV *env;
	u_int32_t pid, id, is_family;
{
	DB_LOCKER *lockerp, *mlockerp;
	DB_LOCKREGION *region;
	DB_LOCKTAB *lt;
	int ret;

	COMPQUIET(region, NULL);
	lt = env->lk_handle;
	region = lt->reginfo.primary;
	LOCK_LOCKERS(env, region);

	/* get/create the  parent locker info */
	if ((ret = __lock_getlocker_int(lt, pid, 1, &mlockerp)) != 0)
		goto err;

	/*
	 * We assume that only one thread can manipulate
	 * a single transaction family.
	 * Therefore the master locker cannot go away while
	 * we manipulate it, nor can another child in the
	 * family be created at the same time.
	 */
	if ((ret = __lock_getlocker_int(lt, id, 1, &lockerp)) != 0)
		goto err;

	/* Point to our parent. */
	lockerp->parent_locker = R_OFFSET(&lt->reginfo, mlockerp);

	/* See if this locker is the family master. */
	if (mlockerp->master_locker == INVALID_ROFF)
		lockerp->master_locker = R_OFFSET(&lt->reginfo, mlockerp);
	else {
		lockerp->master_locker = mlockerp->master_locker;
		mlockerp = R_ADDR(&lt->reginfo, mlockerp->master_locker);
	}

	/*
	 * Set the family locker flag, so it is possible to distinguish
	 * between locks held by subtransactions and those with compatible
	 * lockers.
	 */
	if (is_family)
		F_SET(mlockerp, DB_LOCKER_FAMILY_LOCKER);

	/*
	 * Link the child at the head of the master's list.
	 * The guess is when looking for deadlock that
	 * the most recent child is the one that's blocked.
	 */
	SH_LIST_INSERT_HEAD(
	    &mlockerp->child_locker, lockerp, child_link, __db_locker);

err:	UNLOCK_LOCKERS(env, region);

	return (ret);
}

/*
 * __lock_freelocker_int
 *      Common code for deleting a locker; must be called with the
 *	locker bucket locked.
 */
static int
__lock_freelocker_int(lt, region, sh_locker, reallyfree)
	DB_LOCKTAB *lt;
	DB_LOCKREGION *region;
	DB_LOCKER *sh_locker;
	int reallyfree;
{
	DB_LOCKERSTRIPE *ls;
	ENV *env;
	u_int32_t indx;
	int ret;

	env = lt->env;

	if (SH_LIST_FIRST(&sh_locker->heldby, __db_lock) != NULL) {
		__db_errx(env, DB_STR("2047",
		    "Freeing locker with locks"));
		return (EINVAL);
	}

	/*
	 * SSI: a committed snapshot-safe reader's persisted SIREAD markers stay
	 * on their objects' sireaders lists (detached from heldby) and still
	 * reference this locker via lp->holder.  Freeing the locker while any
	 * such marker exists is a use-after-free when the GC (or a WRITE
	 * acquirer) later dereferences LOCK_HOLDER(marker).  The per-detail
	 * si_ref counts exactly those markers and is atomic, so it is the
	 * race-free guard (nlocks is decremented under the object partition
	 * mutex but read here under LOCK_LOCKERS -- a cross-domain race).
	 * Defer while markers remain; flag DB_LOCKER_FREED so __lock_sicleanup
	 * reclaims the locker once the last marker is gone.
	 */
	if (sh_locker->td_off != INVALID_ROFF &&
	    atomic_read(&LOCKER_TD(env, sh_locker)->si_ref) != 0) {
		F_SET(sh_locker, DB_LOCKER_FREED);
		return (0);
	}

	/* If this is part of a family, we must fix up its links. */
	if (sh_locker->master_locker != INVALID_ROFF) {
		SH_LIST_REMOVE(sh_locker, child_link, __db_locker);
		sh_locker->master_locker = INVALID_ROFF;
	}

	if (reallyfree) {
		LOCKER_HASH(lt, region, sh_locker->id, indx);
		/*
		 * The stripe is derived from the BUCKET, exactly as in
		 * __lock_getlocker_int -- so this locker goes back onto the
		 * free list it was taken from, and comes off the ulinks list
		 * it was put on.  Deriving it any other way (from the id
		 * directly, say) would let lockers migrate between stripes and
		 * the lists would leak across them.
		 */
		ls = &region->locker_stripe[LOCK_LOCKER_ALLOC_STRIPE(region,
		    LOCK_LOCKER_STRIPE(indx))];
		SH_TAILQ_REMOVE(&lt->locker_tab[indx], sh_locker,
		    links, __db_locker);
		/*
		 * P13: KEEP sh_locker->mtx_locker across recycling instead of
		 * calling __mutex_free here.  That call, paired with the
		 * __mutex_alloc on the create path, took MUTEX_SYSTEM_LOCK --
		 * ONE global latch, twice per transaction, since a locker is
		 * created and freed by every txn_begin/txn_end pair.  P12
		 * measured that latch becoming ~11x more contended once the
		 * locker-allocation latch stopped incidentally serialising
		 * arrivals at it; see rfc/0012 and P12-NEGATIVE-RESULT.
		 *
		 * Soundness.  The mutex is a pure blocking primitive with no
		 * persistent identity: it carries no locker state, it is only
		 * ever copied into a DB_LOCK's mtx_lock so a waiter can block
		 * on it (lock.c:1591, :1613), and it is held LOCKED for the
		 * whole life of the locker (taken at create, dropped here).
		 * The locker is unreachable at this point -- off its bucket
		 * chain, heldby empty, so no DB_LOCK references it -- so no
		 * other thread can observe the mutex between this unlock and
		 * the next create's lock.  A recycled mutex and a freshly
		 * allocated one are therefore indistinguishable to every
		 * caller, which is exactly the property __mutex_refresh relies
		 * on for lock.c's DB_LOCK mutexes.
		 *
		 * The mutex lives in the MUTEX region, not in this locker and
		 * not in any process's heap: mtx_locker is a db_mutex_t, which
		 * is an INDEX into the shared mutex array (MUTEXP_SET,
		 * mutex_int.h), and it is allocated with neither
		 * DB_MUTEX_PROCESS_ONLY nor ENV_PRIVATE semantics on a shared
		 * env.  So a mutex retained by process A and inherited by a
		 * locker that process B later recycles resolves, in B, to the
		 * same shared DB_MUTEX -- which is precisely the property that
		 * makes mtx_locker usable for cross-process blocking in the
		 * first place.  Retaining it adds no new cross-process
		 * assumption.
		 *
		 * Unlock it so the next user's MUTEX_LOCK is the uncontended
		 * acquisition that __mutex_alloc + MUTEX_LOCK used to produce.
		 * MUTEX_UNLOCK can only fail by panicking the environment, so
		 * there is no status to propagate here.
		 *
		 * WHY NOT ON THE FAILCHK PATH.  lock_failchk.c:170 frees the
		 * locker of a process that has DIED, and that locker's mutex is
		 * still LOCKED by the corpse.  Unlocking a mutex this thread
		 * does not own is undefined for pthreads, and a retained mutex
		 * that stayed locked would self-deadlock the next locker to
		 * recycle the slot at the MUTEX_LOCK below.  __mutex_free is the
		 * path that copes: it calls __mutex_destroy, which has explicit
		 * failchk handling (mut_pthread.c:723 skips the destroy for the
		 * failchk thread rather than trusting the state).  So failchk
		 * keeps the pre-P13 behaviour.  This costs nothing: failchk is
		 * not a hot path, and the optimisation only needs the __txn_end
		 * path.
		 *
		 * Mutexes retained on the free list are released by
		 * __lock_env_refresh, which walks free_lockers at env close for
		 * exactly this reason.  Without that the mutex region -- a
		 * BOUNDED pool, mut_region.c:78 enforces dbenv->mutex_max --
		 * would leak one slot per retained locker across an env
		 * lifetime.
		 *
		 * DB_NO_LOCKER_MUTEX_REUSE restores the pre-P13 behaviour so
		 * the A/B runs on one binary.
		 */
		if (sh_locker->mtx_locker != MUTEX_INVALID) {
			if (__lock_locker_mutex_reuse() &&
			    !F_ISSET(env->dbenv, DB_ENV_FAILCHK)) {
				MUTEX_UNLOCK(env, sh_locker->mtx_locker);
			} else if ((ret = __mutex_free(env,
			    &sh_locker->mtx_locker)) != 0)
				return (ret);
		}
		SH_TAILQ_INSERT_HEAD(&ls->free_lockers, sh_locker,
		    links, __db_locker);
		SH_TAILQ_REMOVE(&ls->lockers, sh_locker,
		    ulinks, __db_locker);
		ls->nlockers--;
		STAT_PERFMON2(env,
		    lock, nlockers, ls->nlockers, sh_locker->id);
	}

	return (0);
}

/*
 * __lock_sireap_lockers --
 *	Free committed-reader (SSI) lockers whose last SIREAD marker has been
 *	reclaimed.  __lock_siclean_obj marks such a locker while holding the
 *	object partition mutex, by clearing its td_off once the marker count
 *	reaches zero; here, with no partition mutex held, we take LOCK_LOCKERS
 *	and release the locker and its logical mutex.  Without this the
 *	DB_LOCKER_FREED locker stayed allocated for the life of the environment,
 *	so sequential read-only snapshot transactions eventually exhausted the
 *	mutex region (DB_ENV->txn_begin returning ENOMEM).
 *
 *	This deliberately dereferences no TXN_DETAIL: mpool may free the detail
 *	as soon as si_ref reaches zero, so the marker-count observation has to
 *	happen (and does) in __lock_siclean_obj, not here.
 *
 * PUBLIC: int __lock_sireap_lockers __P((ENV *));
 */
int
__lock_sireap_lockers(env)
	ENV *env;
{
	DB_LOCKER *sh_locker, *next_locker;
	DB_LOCKREGION *region;
	DB_LOCKTAB *lt;
	u_int32_t s;
	int ret;

	if (!LOCKING_ON(env))
		return (0);
	lt = env->lk_handle;
	region = lt->reginfo.primary;
	ret = 0;

	/*
	 * The in-use lists are per stripe (P12), so sweep every one of them.
	 * LOCK_LOCKERS already covers all stripes, so no extra latching.
	 */
	LOCK_LOCKERS(env, region);
	for (s = 0; s < LOCK_LOCKER_STRIPES && ret == 0; s++)
	    for (sh_locker = SH_TAILQ_FIRST(
		&region->locker_stripe[s].lockers, __db_locker);
		sh_locker != NULL; sh_locker = next_locker) {
		next_locker = SH_TAILQ_NEXT(sh_locker, ulinks, __db_locker);
		/*
		 * (DB_LOCKER_FREED && td_off == INVALID_ROFF) is set only by
		 * __lock_siclean_obj: a locker whose reclamation was deferred
		 * for SIREAD markers that are now all gone.  A live locker never
		 * carries DB_LOCKER_FREED, and a still-deferred one still has
		 * its td_off.  heldby must be empty (__lock_sicommit detached
		 * the markers, DB_LOCK_PUT_ALL released everything else) --
		 * __lock_freelocker_int would return EINVAL rather than free a
		 * locker with locks, so skip it instead of failing the sweep.
		 */
		if (!F_ISSET(sh_locker, DB_LOCKER_FREED) ||
		    sh_locker->td_off != INVALID_ROFF ||
		    !SH_LIST_EMPTY(&sh_locker->heldby))
			continue;
		if ((ret =
		    __lock_freelocker_int(lt, region, sh_locker, 1)) != 0)
			break;
	}
	UNLOCK_LOCKERS(env, region);

	return (ret);
}

/*
 * __lock_freelocker
 *	Remove a locker its family from the hash table.
 *
 * This must be called without the locker bucket locked.
 *
 * PUBLIC: int __lock_freelocker  __P((DB_LOCKTAB *, DB_LOCKER *));
 */
int
__lock_freelocker(lt, sh_locker)
	DB_LOCKTAB *lt;
	DB_LOCKER *sh_locker;
{
	DB_LOCKREGION *region;
	ENV *env;
	int ret;

	region = lt->reginfo.primary;
	env = lt->env;

	if (sh_locker == NULL)
		return (0);

	/*
	 * Hot path (txn.c:1916, every txn_end).  __lock_freelocker_int touches
	 * this locker's own bucket chain plus its STRIPE's free list, ulinks
	 * list and counter -- and since P12 made those three per-stripe, that
	 * is all one latch: the stripe that owns the bucket.  EXCEPT for a
	 * locker in a transaction family, where it also unlinks from the
	 * master's child_locker list, and the master may live in some other
	 * bucket.  Escalate in that case.
	 *
	 * Reading master_locker/child_locker to make that decision WITHOUT a
	 * latch is safe: both are written only by __lock_addfamilylocker and
	 * __lock_freelocker_int, and both of those hold LOCK_LOCKERS -- i.e.
	 * every stripe, including this locker's.  So after taking this
	 * locker's stripe below, no concurrent writer of those fields can be
	 * running; the all-stripes arm re-reads nothing because it is a
	 * superset of the latch it would need.  A stale read can therefore
	 * only send us to the wider arm, never the narrower one... with one
	 * exception worth naming: a stale read could send a family locker to
	 * the narrow arm if the family link were added concurrently.  It
	 * cannot be: the comment on __lock_addfamilylocker records that only
	 * one thread manipulates a transaction family, and that thread is this
	 * one.
	 */
	if (sh_locker->master_locker != INVALID_ROFF ||
	    !SH_LIST_EMPTY(&sh_locker->child_locker)) {
		LOCK_LOCKERS(env, region);
		ret = __lock_freelocker_int(lt, region, sh_locker, 1);
		UNLOCK_LOCKERS(env, region);
	} else {
		LOCK_LOCKER_WRITE(env, region, sh_locker->id);
		ret = __lock_freelocker_int(lt, region, sh_locker, 1);
		UNLOCK_LOCKER_WRITE(env, region, sh_locker->id);
	}

	return (ret);
}


/*
 * __lock_familyremove
 *	Remove a locker from its family.
 *
 * This must be called without the locker bucket locked.
 *
 * PUBLIC: int __lock_familyremove  __P((DB_LOCKTAB *, DB_LOCKER *));
 */
int
__lock_familyremove(lt, sh_locker)
	DB_LOCKTAB *lt;
	DB_LOCKER *sh_locker;
{
	DB_LOCKREGION *region;
	ENV *env;
	int ret;

	region = lt->reginfo.primary;
	env = lt->env;

	LOCK_LOCKERS(env, region);
	ret = __lock_freelocker_int(lt, region, sh_locker, 0);
	UNLOCK_LOCKERS(env, region);

	return (ret);
}
