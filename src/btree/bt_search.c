/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 2013 Oracle and/or its affiliates.  All rights reserved.
 */
/*
 * Copyright (c) 1990, 1993, 1994, 1995, 1996
 *	Keith Bostic.  All rights reserved.
 */
/*
 * Copyright (c) 1990, 1993, 1994, 1995
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Mike Olson.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * $Id$
 */

#include "db_config.h"

#include "db_int.h"
#include "dbinc/db_page.h"
#include "dbinc/btree.h"
#include "dbinc/lock.h"
#include "dbinc/mp.h"

static int __bam_rsnap_refresh __P((DBC *));
static int __bam_rsnap_child __P((DBC *, const DBT *, db_pgno_t *, DB_LSN *));
static int __bam_snap_child_of __P((DBC *, const DBT *, PAGE *, db_pgno_t *));
static int __bam_snap_descend __P((DBC *, const DBT *, db_pgno_t *, DB_LSN *));
static int __bam_isnap_store __P((DBC *, PAGE *));

/*
 * __bam_rsnap_enabled --
 *	The lock-free root-snapshot read descent is on by default; setting
 *	DB_NO_RSNAP in the environment disables it (A/B benchmarking and
 *	bisecting a suspected fast-path bug).  Read once, cached process-wide.
 */
static int
__bam_rsnap_enabled()
{
	static int cached = -1;

	if (cached == -1)
		cached = getenv("DB_NO_RSNAP") != NULL ? 0 : 1;
	return (cached);
}

/*
 * __bam_isnap_enabled --
 *	The multi-level snapshot descent (ROADMAP #2) caches upper NON-root
 *	internal pages so the descent can start deeper than one level below the
 *	root.  On by default; DB_NO_ISNAP disables ONLY the multi-level levels
 *	(the single-level root snapshot still runs), so the two can be A/B'd
 *	independently.  DB_NO_RSNAP disables both (it gates the whole path).
 *	Read once, cached process-wide.
 */
static int
__bam_isnap_enabled()
{
	static int cached = -1;

	if (cached == -1)
		cached = getenv("DB_NO_ISNAP") != NULL ? 0 : 1;
	return (cached);
}

/*
 * __bam_rsnap_refresh --
 *	Refresh this handle's private copy of the B-tree root.  Fetches the
 *	(wired) root once under its shared latch, takes a consistent copy and
 *	its LSN, and publishes it.  The previously-current copy is retired to
 *	a free list (freed at handle close) so a concurrent reader still
 *	holding it is never freed underneath -- root changes are rare, so few
 *	copies accumulate.  Serialized by the handle mutex.
 */
static int
__bam_rsnap_refresh(dbc)
	DBC *dbc;
{
	BTREE *t;
	BAM_RSNAP *snap;
	DB *dbp;
	DB_MPOOLFILE *mpf;
	DB_LSN lsn;
	ENV *env;
	PAGE *h;
	db_pgno_t root_pgno;
	u_int32_t psize;
	int ret, t_ret, wired;

	dbp = dbc->dbp;
	env = dbp->env;
	mpf = dbp->mpf;
	t = dbp->bt_internal;
	root_pgno = t->bt_root;
	if (root_pgno == PGNO_INVALID)
		return (0);

	if ((ret = __memp_fget(mpf, &root_pgno,
	    dbc->thread_info, dbc->txn, 0, &h)) != 0)
		return (ret);
	/*
	 * Wire the root so the cached buffer address stays resident: only then
	 * may we keep a pointer to the frame for later lock-free LSN reads.  If
	 * wiring did not take (mmap'd page, or the per-region wired cap was
	 * reached), we must not cache the frame -- it is evictable and the
	 * pointer could dangle -- so we disarm the fast path for this handle
	 * (bt_rootpage/bt_rsnap NULL) and fall back to the normal descent.
	 */
	wired = 0;
	(void)__memp_wire(mpf, h, &wired);
	lsn = LSN(h);
	psize = dbp->pgsize;
	snap = NULL;
	if (wired && TYPE(h) == P_IBTREE && psize != 0 &&
	    (ret = __os_malloc(env, sizeof(BAM_RSNAP) + psize, &snap)) == 0) {
		snap->next = NULL;
		snap->frame = NULL;	/* Root's wired frame is bt_rootpage. */
		snap->pgno = root_pgno;
		snap->lsn = lsn;
		snap->size = psize;
		memcpy(BAM_RSNAP_PAGE(snap), h, psize);
	}

	MUTEX_LOCK(env, dbp->mutex);
	/* Cache the wired live-root buffer; NULL if it could not be wired. */
	t->bt_rootpage = wired ? h : NULL;
	/* Retire the previously-current copy to the free list. */
	if (t->bt_rsnap != NULL) {
		((BAM_RSNAP *)t->bt_rsnap)->next = t->bt_rsnap_free;
		t->bt_rsnap_free = t->bt_rsnap;
	}
	t->bt_rsnap = snap;		/* NULL if not wired or the root is a leaf */
	t->bt_rsnap_lsn = lsn;
	MUTEX_UNLOCK(env, dbp->mutex);

	if ((t_ret = __memp_fput(mpf,
	    dbc->thread_info, h, dbc->priority)) != 0 && ret == 0)
		ret = t_ret;
	return (ret);
}

/*
 * __bam_snap_child_of --
 *	Search a P_IBTREE page image for the child that a normal descent for
 *	"key" would take, returning that child page number.  This mirrors the
 *	internal-page child selection in __bam_search EXACTLY (same binary
 *	search, same __bam_cmp, same base->index rule) so the child chosen is
 *	byte-for-byte what the live descent would choose.  The caller is
 *	responsible for having validated that this image is a current copy of
 *	its live page (by pgno and LSN) before trusting the result.
 */
static int
__bam_snap_child_of(dbc, key, cp, childp)
	DBC *dbc;
	const DBT *key;
	PAGE *cp;
	db_pgno_t *childp;
{
	DB *dbp;
	BTREE *t;
	db_indx_t base, indx, lim;
	int (*func) __P((DB *, const DBT *, const DBT *));
	int cmp, ret;

	dbp = dbc->dbp;
	t = dbp->bt_internal;
	if (TYPE(cp) != P_IBTREE || NUM_ENT(cp) == 0)
		return (DB_NOTFOUND);

	func = t->bt_compare;
	indx = 0;
	cmp = 1;
	DB_BINARY_SEARCH_FOR(base, lim, NUM_ENT(cp), O_INDX) {
		DB_BINARY_SEARCH_INCR(indx, base, lim, O_INDX);
		if ((ret = __bam_cmp(dbc, key, cp, indx, func, &cmp)) != 0)
			return (DB_NOTFOUND);
		if (cmp == 0)
			break;
		if (cmp > 0)
			DB_BINARY_SEARCH_SHIFT_BASE(indx, base, lim, O_INDX);
	}
	if (cmp != 0)
		indx = base > 0 ? base - O_INDX : base;
	*childp = GET_BINTERNAL(dbp, cp, indx)->pgno;
	return (0);
}

/*
 * __bam_rsnap_child --
 *	If this handle holds a current snapshot of the root (its LSN still
 *	matches the live root), search the snapshot copy for the child that
 *	the descent for "key" would take, returning that child page number and
 *	the snapshot LSN.  Returns DB_NOTFOUND if there is no current snapshot
 *	(the caller falls back to the normal descent and refreshes).
 */
static int
__bam_rsnap_child(dbc, key, childp, snap_lsnp)
	DBC *dbc;
	const DBT *key;
	db_pgno_t *childp;
	DB_LSN *snap_lsnp;
{
	BTREE *t;
	BAM_RSNAP *snap;
	DB *dbp;
	DB_LSN live;
	PAGE *cp;
	int ret;

	dbp = dbc->dbp;
	t = dbp->bt_internal;
	snap = t->bt_rsnap;
	if (snap == NULL || t->bt_rootpage == NULL)
		return (DB_NOTFOUND);

	/* Racy read of the live root LSN; a torn read just forces a refresh. */
	live = LSN((PAGE *)t->bt_rootpage);
	if (live.file != snap->lsn.file || live.offset != snap->lsn.offset)
		return (DB_NOTFOUND);

	cp = BAM_RSNAP_PAGE(snap);
	if ((ret = __bam_snap_child_of(dbc, key, cp, childp)) != 0)
		return (ret);
	*snap_lsnp = snap->lsn;
	return (0);
}

/*
 * __bam_snap_descend --
 *	Multi-level snapshot descent (ROADMAP #2, wired variant).  Starting
 *	from the current root snapshot (validated against the live wired root
 *	LSN), walk down through this handle's cached NON-root internal-page
 *	copies as far as they reach, returning the deepest child page number to
 *	begin the real (pinning) descent at, and the LSN of that start page's
 *	parent (the deepest confirmed level) for the post-fetch re-check.
 *
 *	Each cached internal copy carries the WIRED live buffer it was taken
 *	from (BAM_RSNAP.frame), so this walk reads the live page's current LSN
 *	with a plain load and NEVER fetches/pins the internal -- exactly as the
 *	root path reads bt_rootpage.  A wired frame is never evicted, so the
 *	pointer cannot dangle; when the page is freed the frame is unwired and
 *	the slot is invalidated (see __bam_isnap_invalidate), so a freed page's
 *	copy is dropped, not trusted.
 *
 *	Correctness: at every level the child is read from the cached COPY only
 *	after its live wired-frame LSN is confirmed to still equal the copy's
 *	LSN (identical LSN => identical bytes => byte-identical child choice to
 *	a normal descent).  A copy with no wired frame, a changed LSN, a wrong
 *	type, or no cached slot stops the walk at the last confirmed page --
 *	a strictly higher (safe) start page.  Thus a stale cache can only make
 *	the descent start higher or restart -- never pick a wrong child.
 *
 *	Returns 0 with *childp set to the deepest start page and *snap_lsnp
 *	set to the LSN the real descent must confirm for *childp's parent;
 *	DB_NOTFOUND if there is no current root snapshot at all.
 */
static int
__bam_snap_descend(dbc, key, childp, snap_lsnp)
	DBC *dbc;
	const DBT *key;
	db_pgno_t *childp;
	DB_LSN *snap_lsnp;
{
	BTREE *t;
	BAM_RSNAP *snap;
	DB *dbp;
	DB_LSN start_lsn;
	PAGE *cp;
	db_pgno_t child;
	int i, j, ret;

	dbp = dbc->dbp;
	t = dbp->bt_internal;

	/* Level 0: the root copy (validated against the live wired root). */
	if ((ret = __bam_rsnap_child(dbc, key, &child, &start_lsn)) != 0)
		return (ret);

	/*
	 * Levels 1..N: walk cached NON-root internal copies fetch-free.  Each
	 * step finds a cached copy of "child", reads the LIVE page's LSN through
	 * the copy's wired frame with a plain load, and -- only if that LSN
	 * still matches -- reads the next child from the copy and continues.
	 * Cap the steps at BAM_ISNAP_MAX (the cache holds no more levels).
	 */
	if (__bam_isnap_enabled())
		for (i = 0; i < BAM_ISNAP_MAX; i++) {
			db_pgno_t next;
			DB_LSN live;
			void *frame;

			/*
			 * Find a cached copy of "child" and snapshot its wired
			 * frame pointer in one load.  A concurrent free unwires
			 * the page and NULLs snap->frame (see
			 * __bam_isnap_invalidate); reading the pointer ONCE means
			 * we never deref a NULL that a race stored after our check.
			 */
			snap = NULL;
			frame = NULL;
			for (j = 0; j < BAM_ISNAP_MAX; j++) {
				snap = t->bt_isnap[j];
				if (snap != NULL && snap->pgno == child) {
					frame = snap->frame;
					if (frame != NULL)
						break;
				}
				snap = NULL;
			}
			if (snap == NULL || frame == NULL)
				break;

			/*
			 * Plain-load the live LSN from the wired frame.  The frame
			 * lives in the (never-unmapped) mpool region, so even if it
			 * was just unwired and reused the read is to valid mapped
			 * memory -- a stale/torn value simply fails the compare and
			 * stops the walk; it can never fault or pick a wrong child.
			 */
			live = LSN((PAGE *)frame);
			if (live.file != snap->lsn.file ||
			    live.offset != snap->lsn.offset)
				break;

			/*
			 * Confirmed current.  The copy's bytes equal the live
			 * page's, so the child chosen from the copy is exactly
			 * what a normal descent would choose.  Read it fetch-free.
			 */
			cp = BAM_RSNAP_PAGE(snap);
			if (__bam_snap_child_of(dbc, key, cp, &next) != 0)
				break;
			start_lsn = snap->lsn;
			child = next;
		}

	*childp = child;
	*snap_lsnp = start_lsn;
	return (0);
}

/*
 * __bam_isnap_invalidate --
 *	Drop any cached internal-page copy for "pgno" (called when the page is
 *	about to be freed/unwired, so its wired frame must no longer be read).
 *	Retires the copy to bt_isnap_free (freed at handle close) so a
 *	concurrent reader still holding it is never freed underneath, and NULLs
 *	the slot so the next descent falls back to a real fetch.  Serialized by
 *	the handle mutex; process-local.  A no-op for non-Btree handles.
 *
 * PUBLIC: int __bam_isnap_invalidate __P((DB *, db_pgno_t));
 */
int
__bam_isnap_invalidate(dbp, pgno)
	DB *dbp;
	db_pgno_t pgno;
{
	BTREE *t;
	BAM_RSNAP *snap;
	int i;

	if (dbp->type != DB_BTREE || (t = dbp->bt_internal) == NULL)
		return (0);
	/* Cheap racy pre-check: nothing cached for this pgno, nothing to do. */
	for (i = 0; i < BAM_ISNAP_MAX; i++) {
		snap = t->bt_isnap[i];
		if (snap != NULL && snap->pgno == pgno)
			break;
	}
	if (i == BAM_ISNAP_MAX)
		return (0);

	MUTEX_LOCK(dbp->env, dbp->mutex);
	for (i = 0; i < BAM_ISNAP_MAX; i++) {
		snap = t->bt_isnap[i];
		if (snap != NULL && snap->pgno == pgno) {
			snap->frame = NULL;
			snap->next = t->bt_isnap_free;
			t->bt_isnap_free = snap;
			t->bt_isnap[i] = NULL;
		}
	}
	MUTEX_UNLOCK(dbp->env, dbp->mutex);
	return (0);
}

/*
 * __bam_isnap_store --
 *	Populate the multi-level internal-page cache from a live internal page
 *	the real descent has in hand (P_IBTREE, non-root).  WIRES the page so
 *	its frame stays resident (via __memp_wire, which self-caps at
 *	MPOOL_WIRED_MAX_PCT and is a no-op over the cap); ONLY if wiring took
 *	does it cache a copy keyed by (pgno, LSN) together with the wired frame
 *	pointer -- an unwired frame is evictable and its pointer could dangle,
 *	so it is never cached (mirrors the root path's wired-only invariant).
 *	If the pgno is already cached at the same LSN it is left alone;
 *	otherwise a slot is (re)filled, retiring any superseded copy to
 *	bt_isnap_free (freed at handle close).  Serialized by the handle mutex;
 *	process-local.
 */
static int
__bam_isnap_store(dbc, h)
	DBC *dbc;
	PAGE *h;
{
	DB *dbp;
	BTREE *t;
	ENV *env;
	DB_MPOOLFILE *mpf;
	BAM_RSNAP *snap;
	db_pgno_t pgno;
	u_int32_t psize;
	int i, slot, wired;

	dbp = dbc->dbp;
	env = dbp->env;
	mpf = dbp->mpf;
	t = dbp->bt_internal;
	psize = dbp->pgsize;
	pgno = PGNO(h);

	if (psize == 0 || TYPE(h) != P_IBTREE)
		return (0);
	/* Never cache the root here; the root copy lives in bt_rsnap. */
	if (pgno == t->bt_root)
		return (0);

	/* Already cached at this same LSN?  Nothing to do. */
	for (i = 0; i < BAM_ISNAP_MAX; i++) {
		snap = t->bt_isnap[i];
		if (snap != NULL && snap->pgno == pgno &&
		    snap->lsn.file == LSN(h).file &&
		    snap->lsn.offset == LSN(h).offset)
			return (0);
	}

	/*
	 * Wire the frame so the cached pointer stays resident.  If wiring did
	 * not take (mmap'd page, or the per-region wired cap was reached), do
	 * not cache it -- an evictable frame's pointer could dangle.  This is
	 * the same invariant the root path enforces in __bam_rsnap_refresh.
	 */
	wired = 0;
	(void)__memp_wire(mpf, h, &wired);
	if (!wired)
		return (0);

	if (__os_malloc(env, sizeof(BAM_RSNAP) + psize, &snap) != 0)
		return (0);
	snap->next = NULL;
	snap->frame = h;
	snap->pgno = pgno;
	snap->lsn = LSN(h);
	snap->size = psize;
	memcpy(BAM_RSNAP_PAGE(snap), h, psize);

	MUTEX_LOCK(env, dbp->mutex);
	/*
	 * Prefer an empty slot, else the slot already holding this pgno
	 * (refresh in place), else evict round-robin keyed by pgno so the
	 * choice is stable and spreads distinct pages across slots.
	 */
	slot = -1;
	for (i = 0; i < BAM_ISNAP_MAX; i++)
		if (t->bt_isnap[i] == NULL) {
			slot = i;
			break;
		}
	if (slot == -1)
		for (i = 0; i < BAM_ISNAP_MAX; i++)
			if (((BAM_RSNAP *)t->bt_isnap[i])->pgno == pgno) {
				slot = i;
				break;
			}
	if (slot == -1)
		slot = (int)(pgno % BAM_ISNAP_MAX);
	if (t->bt_isnap[slot] != NULL) {
		BAM_RSNAP *old = t->bt_isnap[slot];

		/*
		 * Retire the superseded copy.  If it is a DIFFERENT page, this
		 * handle no longer caches it, so unwire its frame now (promptly
		 * returning it to the cache; __memp_unwire is a safe no-op if it
		 * was already unwired).  If it is the SAME page (LSN refresh),
		 * the new copy inherits the wire, so just detach the old copy's
		 * frame pointer -- never unwire a frame two copies still name.
		 */
		if (old->frame != NULL) {
			if (old->pgno != pgno)
				(void)__memp_unwire(mpf, old->frame);
			old->frame = NULL;
		}
		old->next = t->bt_isnap_free;
		t->bt_isnap_free = old;
	}
	t->bt_isnap[slot] = snap;
	MUTEX_UNLOCK(env, dbp->mutex);
	return (0);
}

/*
 * __bam_get_root --
 *	Fetch the root of a tree and see if we want to keep
 * it in the stack.
 *
 * PUBLIC: int __bam_get_root __P((DBC *, db_pgno_t, int, u_int32_t, int *));
 */
int
__bam_get_root(dbc, root_pgno, slevel, flags, stack)
	DBC *dbc;
	db_pgno_t root_pgno;
	int slevel;
	u_int32_t flags;
	int *stack;
{
	BTREE_CURSOR *cp;
	DB *dbp;
	DB_LOCK lock;
	DB_MPOOLFILE *mpf;
	PAGE *h;
	db_lockmode_t lock_mode;
	u_int32_t get_mode;
	int ret, t_ret;

	COMPQUIET(h, NULL);
	LOCK_INIT(lock);
	dbp = dbc->dbp;
	mpf = dbp->mpf;
	cp = (BTREE_CURSOR *)dbc->internal;
	/*
	 * If write-locking pages, we need to know whether or not to acquire a
	 * write lock on a page before getting it.  This depends on how deep it
	 * is in tree, which we don't know until we acquire the root page.  So,
	 * if we need to lock the root page we may have to upgrade it later,
	 * because we won't get the correct lock initially.
	 *
	 * Retrieve the root page.
	 */
try_again:
	*stack = LF_ISSET(SR_STACK) &&
	      (dbc->dbtype == DB_RECNO || F_ISSET(cp, C_RECNUM));
	lock_mode = DB_LOCK_READ;
	if (*stack ||
	    LF_ISSET(SR_DEL) || (LF_ISSET(SR_NEXT) && LF_ISSET(SR_WRITE)))
		lock_mode = DB_LOCK_WRITE;

	/*
	 * Get the root.  If the root happens to be a leaf page then
	 * we are supposed to get a read lock on it before latching
	 * it.  So if we have not locked it do a try get first.
	 * If we can't get the root shared, then get a lock on it and
	 * then wait for the latch.
	 */
retry:	if (lock_mode == DB_LOCK_WRITE)
		get_mode = DB_MPOOL_DIRTY;
	else if (LOCK_ISSET(lock) || !STD_LOCKING(dbc) ||
	    F_ISSET(dbc, DBC_DOWNREV) ||
	    dbc->dbtype == DB_RECNO || F_ISSET(cp, C_RECNUM))
		get_mode = 0;
	else
		get_mode = DB_MPOOL_TRY;

	BAM_GET_ROOT(dbc, root_pgno, h, get_mode, lock_mode, lock, ret);
	if (ret == DB_LOCK_NOTGRANTED && get_mode == DB_MPOOL_TRY) {
		DB_ASSERT(dbp->env, !LOCK_ISSET(lock));
		if ((ret = __db_lget(dbc, 0,
		    root_pgno == PGNO_INVALID ? BAM_ROOT_PGNO(dbc) : root_pgno,
		    lock_mode, 0, &lock)) != 0)
			return (ret);
		goto retry;
	}
	if (ret != 0) {
		/* Did not read it, so we can release the lock */
		(void)__LPUT(dbc, lock);
		return (ret);
	}
	/*
	 * When the descent started at a root-snapshot child (SR_SNAPSHOT), the
	 * start page number came from a private snapshot of the root and may
	 * since have been freed and reused as a non-btree page.  A bad page type
	 * is therefore not corruption but a stale snapshot: release the page and
	 * the lock and tell the caller to restart from the real root.  For a
	 * normal (non-snapshot) descent the root type is invariant, so keep the
	 * assertion.
	 */
	if (LF_ISSET(SR_SNAPSHOT) &&
	    TYPE(h) != P_IBTREE && TYPE(h) != P_IRECNO &&
	    TYPE(h) != P_LBTREE && TYPE(h) != P_LRECNO && TYPE(h) != P_LDUP) {
		(void)__memp_fput(mpf, dbc->thread_info, h, dbc->priority);
		(void)__LPUT(dbc, lock);
		return (DB_NOTFOUND);
	}
	DB_ASSERT(dbp->env, TYPE(h) == P_IBTREE || TYPE(h) == P_IRECNO ||
	    TYPE(h) == P_LBTREE || TYPE(h) == P_LRECNO || TYPE(h) == P_LDUP);

	/*
	 * Wire the one common tree root so it stays resident: it is fetched by
	 * every operation, so keeping it non-evictable removes the read-in/
	 * eviction churn on the hottest page and lets the root snapshot refresh
	 * cheaply.  Only the main tree root (BAM_ROOT_PGNO) is wired -- subtree
	 * (off-page duplicate) roots and all internal pages stay evictable.
	 * Unwired when the page is freed (__db_free) or the file closes.
	 */
	if (h->pgno == BAM_ROOT_PGNO(dbc))
		(void)__memp_wire(mpf, h, NULL);

	/*
	 * Decide if we need to dirty and/or lock this page.
	 * We must not hold the latch while we get the lock.
	 */
	if (!*stack &&
	    ((LF_ISSET(SR_PARENT) && (u_int8_t)(slevel + 1) >= LEVEL(h)) ||
	    LEVEL(h) == LEAFLEVEL ||
	    (LF_ISSET(SR_START) && slevel == LEVEL(h)))) {
		*stack = 1;
		/* If we already have the write lock, we are done. */
		if (dbc->dbtype == DB_RECNO || F_ISSET(cp, C_RECNUM)) {
			if (lock_mode == DB_LOCK_WRITE)
				goto done;
			if ((ret = __LPUT(dbc, lock)) != 0)
				return (ret);
		}

		/*
		 * Now that we know what level the root is at, do we need a
		 * write lock?  If not or we got the lock before latching
		 * we are done.
		 */
		if (LEVEL(h) != LEAFLEVEL || LF_ISSET(SR_WRITE)) {
			lock_mode = DB_LOCK_WRITE;
			/* Drop the read lock if we got it above. */
			if ((ret = __LPUT(dbc, lock)) != 0)
				return (ret);
		} else if (LOCK_ISSET(lock))
			goto done;
		if (!STD_LOCKING(dbc)) {
			if (lock_mode != DB_LOCK_WRITE)
				goto done;
			if ((ret = __memp_dirty(mpf, &h, dbc->thread_info,
			    dbc->txn, dbc->priority, 0)) != 0) {
				if (h != NULL)
					(void)__memp_fput(mpf,
					    dbc->thread_info, h, dbc->priority);
				return (ret);
			}
		} else {
			/* Try to lock the page without waiting first. */
			if ((ret = __db_lget(dbc, 0, root_pgno,
			    lock_mode, DB_LOCK_NOWAIT, &lock)) == 0) {
				if (lock_mode == DB_LOCK_WRITE && (ret =
				    __memp_dirty(mpf, &h, dbc->thread_info,
				    dbc->txn, dbc->priority, 0)) != 0) {
					if (h != NULL)
						(void)__memp_fput(mpf,
						    dbc->thread_info, h,
						    dbc->priority);
					return (ret);
				}
				goto done;
			}

			t_ret = __memp_fput(mpf,
			    dbc->thread_info, h, dbc->priority);
			h = NULL;

			if (ret == DB_LOCK_DEADLOCK ||
			    ret == DB_LOCK_NOTGRANTED)
				ret = 0;
			if (ret == 0)
				ret = t_ret;

			if (ret != 0)
				return (ret);
			get_mode = 0;
			if (lock_mode == DB_LOCK_WRITE)
				get_mode = DB_MPOOL_DIRTY;

			if ((ret = __db_lget(dbc,
			     0, root_pgno, lock_mode, 0, &lock)) != 0)
				return (ret);
			if ((ret = __memp_fget(mpf,
			     &root_pgno, dbc->thread_info, dbc->txn,
			     (atomic_read(&mpf->mfp->multiversion) == 0 &&
			     lock_mode == DB_LOCK_WRITE) ? DB_MPOOL_DIRTY : 0,
			     &h)) != 0) {
				/* Did not read it, release the lock */
				(void)__LPUT(dbc, lock);
				return (ret);
			}
		}
		/*
		 * While getting dirty or locked we need to drop the mutex
		 * so someone else could get in and split the root.
		 */
		if (!((LF_ISSET(SR_PARENT) &&
		    (u_int8_t)(slevel + 1) >= LEVEL(h)) ||
		    LEVEL(h) == LEAFLEVEL ||
		    (LF_ISSET(SR_START) && slevel == LEVEL(h)))) {
			/* Someone else split the root, start over. */
			ret = __memp_fput(mpf,
			    dbc->thread_info, h, dbc->priority);
			h = NULL;
			if ((t_ret = __LPUT(dbc, lock)) != 0 && ret == 0)
				ret = t_ret;
			if (ret != 0)
				return (ret);
			goto try_again;
		} else if (atomic_read(&mpf->mfp->multiversion) != 0 &&
		    lock_mode == DB_LOCK_WRITE && (ret = __memp_dirty(mpf, &h,
		    dbc->thread_info, dbc->txn, dbc->priority, 0)) != 0) {
			/*
			 * __memp_dirty releases the read-only page and re-fetches
			 * it dirty; on failure (e.g. DB_LOCK_DEADLOCK re-fetching
			 * under MVCC write contention) it sets h to NULL.  Release
			 * whatever we still hold and return the error -- do not
			 * fall through to done: with a NULL page, which the caller
			 * would dereference.
			 */
			if (h != NULL)
				(void)__memp_fput(mpf,
				    dbc->thread_info, h, dbc->priority);
			(void)__LPUT(dbc, lock);
			return (ret);
		}
	}

done:	BT_STK_ENTER(dbp->env, cp, h, 0, lock, lock_mode, ret);

	return (ret);
}

/*
 * __bam_search --
 *	Search a btree for a key.
 *
 * PUBLIC: int __bam_search __P((DBC *, db_pgno_t,
 * PUBLIC:     const DBT *, u_int32_t, int, db_recno_t *, int *));
 */
int
__bam_search(dbc, root_pgno, key, flags, slevel, recnop, exactp)
	DBC *dbc;
	db_pgno_t root_pgno;
	const DBT *key;
	u_int32_t flags;
	int slevel, *exactp;
	db_recno_t *recnop;
{
	BTREE *t;
	BTREE_CURSOR *cp;
	DB *dbp;
	DB_LOCK lock, saved_lock;
	DB_MPOOLFILE *mpf;
	ENV *env;
	PAGE *h, *parent_h;
	db_indx_t base, i, indx, *inp, lim;
	db_lockmode_t lock_mode;
	db_pgno_t pg, saved_pg, start_pgno;
	db_recno_t recno;
	int adjust, cmp, deloffset, ret, set_stack, stack, t_ret;
	int getlock, was_next;
	int (*func) __P((DB *, const DBT *, const DBT *));
	u_int32_t get_mode, wait;
	u_int8_t level, saved_level;
	int from_snap;
	db_pgno_t snap_child;
	DB_LSN snap_lsn, root_snap_lsn;
	int snap_ok;

	if (F_ISSET(dbc, DBC_OPD))
		LOCK_CHECK_OFF(dbc->thread_info);

	dbp = dbc->dbp;
	env = dbp->env;
	mpf = dbp->mpf;
	cp = (BTREE_CURSOR *)dbc->internal;
	h = NULL;
	parent_h = NULL;
	t = dbp->bt_internal;
	recno = 0;
	t_ret = 0;

	BT_STK_CLR(cp);
	LOCK_INIT(saved_lock);
	LOCK_INIT(lock);
	was_next = LF_ISSET(SR_NEXT);
	wait = DB_LOCK_NOWAIT;

	/*
	 * There are several ways we search a btree tree.  The flags argument
	 * specifies if we're acquiring read or write latches, if we position
	 * to the first or last item in a set of duplicates, if we return
	 * deleted items, and if we are latching pairs of pages.  In addition,
	 * if we're modifying record numbers, we have to latch the entire tree
	 * regardless.  See btree.h for more details.
	 */

	start_pgno = saved_pg = root_pgno;

	/*
	 * Snapshot fast path (option B, ROADMAP #2 multi-level): for a plain
	 * read lookup of the main tree (not write/stack/parent/next/del/min/max,
	 * not OPD, not recno/recnum, not multiversion), descend this handle's
	 * private page copies -- the root copy and, when the tree is tall, the
	 * cached upper NON-root internal copies -- to find the deepest child to
	 * begin the real descent at, never fetching (pinning/latching) the
	 * contended live root and never lock-coupling the skipped upper levels.
	 * The root copy's validity is confirmed against the live root LSN by
	 * __bam_snap_descend, and re-checked after the start page is fetched
	 * (below) to close the window where a concurrent root change could make
	 * the start page stale.  Each skipped NON-root internal level is
	 * confirmed live (by pgno+LSN) inside __bam_snap_descend at the moment
	 * its child is read, so a stale cache can only start the descent higher
	 * (safe) or trigger a restart, never pick a wrong child.
	 */
	from_snap = 0;
	snap_ok = root_pgno == PGNO_INVALID && key != NULL &&
	    slevel == LEAFLEVEL &&
	    LF_ISSET(SR_READ) && !LF_ISSET(SR_WRITE | SR_PARENT | SR_STACK |
	    SR_NEXT | SR_DEL | SR_START | SR_BOTH | SR_MIN | SR_MAX |
	    SR_STK_ONLY) && !F_ISSET(dbc, DBC_OPD) &&
	    dbc->dbtype == DB_BTREE && !F_ISSET(cp, C_RECNUM) &&
	    atomic_read(&mpf->mfp->multiversion) == 0 &&
	    LOGGING_ON(env) && !F_ISSET(dbp, DB_AM_NOT_DURABLE) &&
	    __bam_rsnap_enabled();
	if (snap_ok) {
		if (__bam_snap_descend(dbc, key, &snap_child, &snap_lsn) == 0) {
			start_pgno = snap_child;
			from_snap = 1;
			/*
			 * Remember the live root LSN as it stood when the
			 * snapshot descent picked the start page, so the
			 * post-fetch re-check can detect a concurrent root
			 * change (e.g. a split adding a tree level).
			 */
			root_snap_lsn = t->bt_rootpage != NULL ?
			    LSN((PAGE *)t->bt_rootpage) : snap_lsn;
		} else
			(void)__bam_rsnap_refresh(dbc);
	}
	saved_level = MAXBTREELEVEL;
retry:	if ((ret = __bam_get_root(dbc, start_pgno, slevel,
	    from_snap ? (flags | SR_SNAPSHOT) : flags, &stack)) != 0) {
		if (from_snap && ret == DB_NOTFOUND) {
			/*
			 * The snapshot child was a freed/reused non-btree page:
			 * a stale snapshot.  Refresh and restart from the real
			 * root (the page and lock were already released).
			 */
			from_snap = 0;
			start_pgno = PGNO_INVALID;
			(void)__bam_rsnap_refresh(dbc);
			goto retry;
		}
		goto err;
	}
	lock_mode = cp->csp->lock_mode;
	get_mode = lock_mode == DB_LOCK_WRITE ? DB_MPOOL_DIRTY : 0;
	h = cp->csp->page;
	root_pgno = pg = PGNO(h);
	lock = cp->csp->lock;
	set_stack = stack;
	/*
	 * Determine if we need to lock interior nodes.
	 * If we have record numbers we always lock.  Otherwise we only
	 * need to do this if we are write locking and we are returning
	 * a stack of nodes.  SR_NEXT will eventually get a stack and
	 * release the locks above that level.
	 */
	if (F_ISSET(dbc, DBC_DOWNREV)) {
		getlock = 1;
		wait = 0;
	} else
		getlock = F_ISSET(cp, C_RECNUM) ||
		   (lock_mode == DB_LOCK_WRITE &&
		   (stack || LF_ISSET(SR_NEXT | SR_DEL)));

	/*
	 * If we are asked a level that is above the root,
	 * just return the root.  This can happen if the tree
	 * collapses while we are trying to lock the root.
	 */
	if (!LF_ISSET(SR_START) && LEVEL(h) < slevel)
		goto done;

	BT_STK_CLR(cp);

	/*
	 * Snapshot re-check: we began the descent at a page taken (directly or
	 * transitively) from this handle's private page copies.  If the live
	 * root LSN no longer matches what it was when the snapshot descent
	 * picked the start page, the tree changed (e.g. a split added a level,
	 * or a merge freed the start page's subtree) while we were fetching the
	 * start page, so it may be stale or reused.  Release it and restart the
	 * descent from the real root.  (A freed/reused start page of a bad type
	 * is already caught by SR_SNAPSHOT in __bam_get_root and the per-level
	 * LEVEL guard below.)
	 */
	if (from_snap) {
		DB_LSN now;

		now = t->bt_rootpage != NULL ?
		    LSN((PAGE *)t->bt_rootpage) : root_snap_lsn;
		if (t->bt_rootpage == NULL ||
		    now.file != root_snap_lsn.file ||
		    now.offset != root_snap_lsn.offset) {
			if ((ret = __memp_fput(mpf,
			    dbc->thread_info, h, dbc->priority)) != 0)
				goto err;
			h = NULL;
			(void)__LPUT(dbc, lock);
			LOCK_INIT(lock);
			from_snap = 0;
			start_pgno = PGNO_INVALID;
			(void)__bam_rsnap_refresh(dbc);
			goto retry;
		}
	}

	/* Choose a comparison function. */
	func = F_ISSET(dbc, DBC_OPD) ?
	    (dbp->dup_compare == NULL ? __bam_defcmp : dbp->dup_compare) :
	    t->bt_compare;

	for (;;) {
		if (TYPE(h) == P_LBTREE)
			adjust = P_INDX;
		else {
			/*
			 * It is possible to catch an internal page as a change
			 * is being backed out.  Its leaf pages will be locked
			 * but we must be sure we get to one.  If the page
			 * is not populated enough lock it.
			 */
			if (TYPE(h) != P_LDUP && NUM_ENT(h) == 0) {
				getlock = 1;
				level = LEVEL(h) + 1;
				if ((ret = __memp_fput(mpf, dbc->thread_info,
				     h, dbc->priority)) != 0)
					goto err;
				goto lock_next;
			}
			adjust = O_INDX;
			/*
			 * Multi-level snapshot cache (ROADMAP #2, wired): this is a
			 * populated internal page in hand during a plain read
			 * descent.  Cache a copy of every NON-root internal page
			 * (P_IBTREE), WIRING the frame so a later descent can read
			 * its live LSN with a plain load and skip the fetch/pin
			 * entirely.  __bam_isnap_store ignores the root, same-LSN
			 * duplicates, and pages it cannot wire; a page that splits
			 * often (a leaf parent) simply fails the live-LSN confirm on
			 * the next descent and falls back -- never a wrong child.
			 */
			if (snap_ok && TYPE(h) == P_IBTREE)
				(void)__bam_isnap_store(dbc, h);
		}
		inp = P_INP(dbp, h);
		if (LF_ISSET(SR_MIN | SR_MAX)) {
			if (LF_ISSET(SR_MIN) || NUM_ENT(h) == 0)
				indx = 0;
			else if (TYPE(h) == P_LBTREE)
				indx = NUM_ENT(h) - 2;
			else
				indx = NUM_ENT(h) - 1;

			if (LEVEL(h) == LEAFLEVEL ||
			     (!LF_ISSET(SR_START) && LEVEL(h) == slevel)) {
				if (LF_ISSET(SR_NEXT))
					goto get_next;
				goto found;
			}
			goto next;
		}
		/*
		 * Do a binary search on the current page.  If we're searching
		 * a Btree leaf page, we have to walk the indices in groups of
		 * two.  If we're searching an internal page or a off-page dup
		 * page, they're an index per page item.  If we find an exact
		 * match on a leaf page, we're done.
		 */
		DB_BINARY_SEARCH_FOR(base, lim, NUM_ENT(h), adjust) {
			DB_BINARY_SEARCH_INCR(indx, base, lim, adjust);
			if ((ret = __bam_cmp(dbc, key, h, indx,
			    func, &cmp)) != 0)
				goto err;
			if (cmp == 0) {
				if (LEVEL(h) == LEAFLEVEL ||
				    (!LF_ISSET(SR_START) &&
				    LEVEL(h) == slevel)) {
					if (LF_ISSET(SR_NEXT))
						goto get_next;
					goto found;
				}
				goto next;
			}
			if (cmp > 0)
				DB_BINARY_SEARCH_SHIFT_BASE(indx, base,
				    lim, adjust);
		}

		/*
		 * No match found.  Base is the smallest index greater than
		 * key and may be zero or a last + O_INDX index.
		 *
		 * If it's a leaf page or the stopping point,
		 * return base as the "found" value.
		 * Delete only deletes exact matches.
		 */
		if (LEVEL(h) == LEAFLEVEL ||
		    (!LF_ISSET(SR_START) && LEVEL(h) == slevel)) {
			*exactp = 0;

			if (LF_ISSET(SR_EXACT)) {
				ret = DB_NOTFOUND;
				goto err;
			}

			if (LF_ISSET(SR_STK_ONLY)) {
				BT_STK_NUM(env, cp, h, base, ret);
				if ((t_ret =
				    __LPUT(dbc, lock)) != 0 && ret == 0)
					ret = t_ret;
				if ((t_ret = __memp_fput(mpf, dbc->thread_info,
				     h, dbc->priority)) != 0 && ret == 0)
					ret = t_ret;
				h = NULL;
				if (ret != 0)
					goto err;
				goto done;
			}
			if (LF_ISSET(SR_NEXT)) {
get_next:			/*
				 * The caller could have asked for a NEXT
				 * at the root if the tree recently collapsed.
				 */
				if (PGNO(h) == root_pgno) {
					ret = DB_NOTFOUND;
					goto err;
				}

				indx = cp->sp->indx + 1;
				if (indx == NUM_ENT(cp->sp->page)) {
					ret = DB_NOTFOUND;
					cp->csp++;
					goto err;
				}
				/*
				 * If we want both the key page and the next
				 * page, push the key page on the stack
				 * otherwise save the root of the subtree
				 * and drop the rest of the subtree.
				 * Search down again starting at the
				 * next child of the root of this subtree.
				 */
				LF_SET(SR_MIN);
				LF_CLR(SR_NEXT);
				set_stack = stack = 1;
				if (LF_ISSET(SR_BOTH)) {
					cp->csp++;
					BT_STK_PUSH(env,
					    cp, h, indx, lock, lock_mode, ret);
					if (ret != 0)
						goto err;
					LOCK_INIT(lock);
					h = cp->sp->page;
					pg = GET_BINTERNAL(dbp, h, indx)->pgno;
					level = LEVEL(h);
					h = NULL;
					goto lock_next;
				} else {
					if ((ret = __LPUT(dbc, lock)) != 0)
						goto err;
					if ((ret = __memp_fput(mpf,
					    dbc->thread_info,
					    h, dbc->priority)) != 0)
						goto err;
					h = cp->sp->page;
					cp->sp->page = NULL;
					lock = cp->sp->lock;
					LOCK_INIT(cp->sp->lock);
					if ((ret = __bam_stkrel(dbc,
					    STK_NOLOCK)) != 0)
						goto err;
					goto next;
				}
			}

			/*
			 * !!!
			 * Possibly returning a deleted record -- DB_SET_RANGE,
			 * DB_KEYFIRST and DB_KEYLAST don't require an exact
			 * match, and we don't want to walk multiple pages here
			 * to find an undeleted record.  This is handled by the
			 * calling routine.
			 */
			if (LF_ISSET(SR_DEL) && cp->csp == cp->sp)
				cp->csp++;
			BT_STK_ENTER(env, cp, h, base, lock, lock_mode, ret);
			if (ret != 0)
				goto err;
			goto done;
		}

		/*
		 * If it's not a leaf page, record the internal page (which is
		 * a parent page for the key).  Decrement the base by 1 if it's
		 * non-zero so that if a split later occurs, the inserted page
		 * will be to the right of the saved page.
		 */
		indx = base > 0 ? base - O_INDX : base;

		/*
		 * If we're trying to calculate the record number, sum up
		 * all the record numbers on this page up to the indx point.
		 */
next:		if (recnop != NULL)
			for (i = 0; i < indx; ++i)
				recno += GET_BINTERNAL(dbp, h, i)->nrecs;

		pg = GET_BINTERNAL(dbp, h, indx)->pgno;
		level = LEVEL(h);

		/* See if we are at the level to start stacking. */
		if (LF_ISSET(SR_START) && slevel == level)
			set_stack = stack = 1;

		if (LF_ISSET(SR_STK_ONLY)) {
			if (slevel == LEVEL(h)) {
				BT_STK_NUM(env, cp, h, indx, ret);
				if ((t_ret = __memp_fput(mpf, dbc->thread_info,
				    h, dbc->priority)) != 0 && ret == 0)
					ret = t_ret;
				h = NULL;
				if (ret != 0)
					goto err;
				goto done;
			}
			BT_STK_NUMPUSH(env, cp, h, indx, ret);
			(void)__memp_fput(mpf,
			    dbc->thread_info, h, dbc->priority);
			h = NULL;
		} else if (stack) {
			/* Return if this is the lowest page wanted. */
			if (LF_ISSET(SR_PARENT) && slevel == level) {
				BT_STK_ENTER(env,
				    cp, h, indx, lock, lock_mode, ret);
				if (ret != 0)
					goto err;
				goto done;
			}
			if (LF_ISSET(SR_DEL) && NUM_ENT(h) > 1) {
				/*
				 * There was a page with a singleton pointer
				 * to a non-empty subtree.
				 */
				cp->csp--;
				if ((ret = __bam_stkrel(dbc, STK_NOLOCK)) != 0)
					goto err;
				set_stack = stack = 0;
				goto do_del;
			}
			BT_STK_PUSH(env,
			    cp, h, indx, lock, lock_mode, ret);
			if (ret != 0)
				goto err;

			LOCK_INIT(lock);
			get_mode = DB_MPOOL_DIRTY;
			lock_mode = DB_LOCK_WRITE;
			getlock = 1;
			goto lock_next;
		} else {
			/*
			 * Decide if we want to return a reference to the next
			 * page in the return stack.  If so, latch it and don't
			 * unlatch it.  We will want to stack things on the
			 * next iteration.  The stack variable cannot be
			 * set until we leave this clause. If we are locking
			 * then we must lock this level before getting the page.
			 */
			if ((LF_ISSET(SR_PARENT) &&
			    (u_int8_t)(slevel + 1) >= (level - 1)) ||
			    (level - 1) == LEAFLEVEL)
				set_stack = 1;

			/*
			 * Check for a normal search.  If so, we need to
			 * latch couple the parent/chid buffers.
			 */
			if (!LF_ISSET(SR_DEL | SR_NEXT)) {
				parent_h = h;
				goto lock_next;
			}

			/*
			 * Returning a subtree.  See if we have hit the start
			 * point if so save the parent and set stack.
			 * Otherwise free the parent and temporarily
			 * save this one.
			 * For SR_DEL we need to find a page with 1 entry.
			 * For SR_NEXT we want find the minimal subtree
			 * that contains the key and the next page.
			 * We save pages as long as we are at the right
			 * edge of the subtree.  When we leave the right
			 * edge, then drop the subtree.
			 */

			if ((LF_ISSET(SR_DEL) && NUM_ENT(h) == 1)) {
				/*
				 * We are pushing the things on the stack,
				 * set the stack variable now to indicate this
				 * has happened.
				 */
				stack = set_stack = 1;
				LF_SET(SR_WRITE);
				/* Push the parent. */
				cp->csp++;
				/* Push this node. */
				BT_STK_PUSH(env, cp, h,
				     indx, lock, DB_LOCK_NG, ret);
				if (ret != 0)
					goto err;
				LOCK_INIT(lock);
			} else {
			/*
			 * See if we want to save the tree so far.
			 * If we are looking for the next key,
			 * then we must save this node if we are
			 * at the end of the page.  If not then
			 * discard anything we have saved so far.
			 * For delete only keep one node until
			 * we find a singleton.
			 */
do_del:				if (cp->csp->page != NULL) {
					if (LF_ISSET(SR_NEXT) &&
					     indx == NUM_ENT(h) - 1)
						cp->csp++;
					else if ((ret =
					    __bam_stkrel(dbc, STK_NOLOCK)) != 0)
						goto err;
				}
				/* Save this node. */
				BT_STK_ENTER(env, cp,
				    h, indx, lock, lock_mode, ret);
				if (ret != 0)
					goto err;
				LOCK_INIT(lock);
			}

lock_next:		h = NULL;

			if (set_stack && LF_ISSET(SR_WRITE)) {
				lock_mode = DB_LOCK_WRITE;
				get_mode = DB_MPOOL_DIRTY;
				getlock = 1;
			}
			/*
			 * If we are retrying and we are back at the same
			 * page then we already have it locked.  If we are
			 * at a different page we want to lock couple and
			 * release that lock.
			 */
			if (level - 1 == saved_level) {
				if ((ret = __LPUT(dbc, lock)) != 0)
					goto err;
				lock = saved_lock;
				LOCK_INIT(saved_lock);
				saved_level = MAXBTREELEVEL;
				if (pg == saved_pg)
					goto skip_lock;
			}
			if ((getlock || level - 1 == LEAFLEVEL) &&
			    (ret = __db_lget(dbc, LCK_COUPLE_ALWAYS,
			    pg, lock_mode, wait, &lock)) != 0) {
				/*
				 * If we are doing DEL or NEXT then we
				 * have an extra level saved in the stack,
				 * push it so it will get freed.
				 */
				if (LF_ISSET(SR_DEL | SR_NEXT) && !stack)
					cp->csp++;
				PERFMON6(env, race, bam_search, dbp->fname,
				    dbp->dname, ret, h, parent_h, flags);
				/*
				 * If we fail, discard the lock we held.
				 * This is ok because we will either search
				 * again or exit without actually looking
				 * at the data.
				 */
				if ((t_ret = __LPUT(dbc, lock)) != 0)
					ret = t_ret;
				/*
				 * If we blocked at a different level release
				 * the previous saved lock.
				 */
				if ((t_ret = __LPUT(dbc, saved_lock)) != 0 &&
				    ret == 0)
					ret = t_ret;
				if (wait == 0 || (ret != DB_LOCK_NOTGRANTED &&
				     ret != DB_LOCK_DEADLOCK))
					goto err;

				/* Release the parent if we are holding it. */
				if (parent_h != NULL &&
				    (ret = __memp_fput(mpf, dbc->thread_info,
				    parent_h, dbc->priority)) != 0)
					goto err;
				parent_h = NULL;

				BT_STK_POP(cp);
				if ((ret = __bam_stkrel(dbc, STK_NOLOCK)) != 0)
					goto err;
				if ((ret = __db_lget(dbc,
				    0, pg, lock_mode, 0, &saved_lock)) != 0)
					goto err;
				/*
				 * A very strange case: if this page was
				 * freed while we wait then we cannot hold
				 * the lock on it while we reget the root
				 * latch because allocation is one place
				 * we lock while holding a latch.
				 * We want to hold the lock but must ensure
				 * that the page is not free or cannot become
				 * free.  If we are at the LEAF level we can
				 * hold on to the lock if the page is still
				 * of the right type.  Otherwise we need to
				 * be sure this page cannot move to an off page
				 * duplicate tree (which are not locked) and
				 * masquerade as the page we want.
				 */

				/*
				 * If the page is not at leaf level
				 * then see if OPD trees are around.
				 * If the page could appear as an
				 * interior offpage duplicate node
				 * at the right level the it will
				 * not be locked and subsequently be
				 * freed. If there are multiple
				 * databases in the file then they
				 * could have OPDs.
				 */
				if (level - 1 > LEAFLEVEL &&
				    (F_ISSET(dbp, DB_AM_SUBDB) ||
				    (dbp->type == DB_BTREE &&
				    F_ISSET(dbp, DB_AM_DUPSORT))))
					goto drop_lock;

				/*
				 * Take a look at the page.  If it got
				 * freed it could be very gone.
				 */
				if ((ret = __memp_fget(mpf, &pg,
				     dbc->thread_info, dbc->txn, 0, &h)) != 0 &&
				     ret != DB_PAGE_NOTFOUND)
					goto err;

				/*
				 * Check for right level and page type.
				 */
				if (ret != 0 || LEVEL(h) != level - 1 ||
				    (LEVEL(h) == LEAFLEVEL ?
				    TYPE(h) != (dbc->dbtype == DB_BTREE ?
				    P_LBTREE : P_LRECNO) :
				    TYPE(h) != (dbc->dbtype == DB_BTREE ?
				    P_IBTREE : P_IRECNO))) {
drop_lock:				ret = __LPUT(dbc, saved_lock);
					if (ret != 0)
						goto err;
					pg = root_pgno;
					saved_level = MAXBTREELEVEL;
				}
				if (h != NULL && (ret = __memp_fput(mpf,
				    dbc->thread_info, h, dbc->priority)) != 0)
					goto err;
				h = NULL;

				if (was_next) {
					LF_CLR(SR_MIN);
					LF_SET(SR_NEXT);
				}
				/*
				 * We have the lock but we dropped the
				 * latch so we need to search again. If
				 * we get back to the same page then all
				 * is good, otherwise we need to try to
				 * lock the new page.
				 */
				saved_pg = pg;
				saved_level = level - 1;
				goto retry;
			}
skip_lock:		stack = set_stack;
		}
		/* Get the child page. */
		if ((ret = __memp_fget(mpf, &pg,
		     dbc->thread_info, dbc->txn, get_mode, &h)) != 0)
			goto err;
		/*
		 * On an untrusted/corrupt file a BINTERNAL child pointer can
		 * point back up the tree (to itself, a sibling, or an ancestor)
		 * at the same or a higher level.  The descent then never reaches
		 * LEAFLEVEL and this loop spins forever (a denial of service).
		 * A valid Btree always has strictly decreasing levels from root
		 * to leaf, so a child whose level is not below its parent's is
		 * corruption -- reject it as a clean page error rather than loop.
		 * (The lock-retry path above already enforces LEVEL(h)==level-1;
		 * this guards the common latch-coupling fast path.)
		 */
		if (LEVEL(h) >= level) {
			(void)__memp_fput(mpf,
			    dbc->thread_info, h, dbc->priority);
			h = NULL;
			ret = DB_PAGE_NOTFOUND;
			goto err;
		}
		/* Release the parent. */
		if (parent_h != NULL && (ret = __memp_fput(mpf,
		    dbc->thread_info, parent_h, dbc->priority)) != 0)
			goto err;
		parent_h = NULL;
	}
	/* NOTREACHED */

found:	*exactp = 1;

	/*
	 * If we got here, we know that we have a Btree leaf or off-page
	 * duplicates page.  If it's a Btree leaf page, we have to handle
	 * on-page duplicates.
	 *
	 * If there are duplicates, go to the first/last one.  This is
	 * safe because we know that we're not going to leave the page,
	 * all duplicate sets that are not on overflow pages exist on a
	 * single leaf page.
	 */
	if (TYPE(h) == P_LBTREE && NUM_ENT(h) > P_INDX) {
		if (LF_ISSET(SR_DUPLAST))
			while (indx < (db_indx_t)(NUM_ENT(h) - P_INDX) &&
			    inp[indx] == inp[indx + P_INDX])
				indx += P_INDX;
		else if (LF_ISSET(SR_DUPFIRST))
			while (indx > 0 &&
			    inp[indx] == inp[indx - P_INDX])
				indx -= P_INDX;
	}

	/*
	 * Now check if we are allowed to return deleted items; if not, then
	 * find the next (or previous) non-deleted duplicate entry.  (We do
	 * not move from the original found key on the basis of the SR_DELNO
	 * flag.)
	 */
	DB_ASSERT(env, recnop == NULL || LF_ISSET(SR_DELNO));
	if (LF_ISSET(SR_DELNO)) {
		deloffset = TYPE(h) == P_LBTREE ? O_INDX : 0;
		if (LF_ISSET(SR_DUPLAST))
			while (B_DISSET(GET_BKEYDATA(dbp,
			    h, indx + deloffset)->type) && indx > 0 &&
			    inp[indx] == inp[indx - adjust])
				indx -= adjust;
		else
			while (B_DISSET(GET_BKEYDATA(dbp,
			    h, indx + deloffset)->type) &&
			    indx < (db_indx_t)(NUM_ENT(h) - adjust) &&
			    inp[indx] == inp[indx + adjust])
				indx += adjust;

		/*
		 * If we weren't able to find a non-deleted duplicate, return
		 * DB_NOTFOUND.
		 */
		if (B_DISSET(GET_BKEYDATA(dbp, h, indx + deloffset)->type)) {
			ret = DB_NOTFOUND;
			goto err;
		}

		/*
		 * Increment the record counter to point to the found element.
		 * Ignore any deleted key/data pairs.  There doesn't need to
		 * be any correction for duplicates, as Btree doesn't support
		 * duplicates and record numbers in the same tree.
		 */
		if (recnop != NULL) {
			DB_ASSERT(env, TYPE(h) == P_LBTREE);

			for (i = 0; i < indx; i += P_INDX)
				if (!B_DISSET(
				    GET_BKEYDATA(dbp, h, i + O_INDX)->type))
					++recno;

			/* Correct the number for a 0-base. */
			*recnop = recno + 1;
		}
	}

	if (LF_ISSET(SR_STK_ONLY)) {
		BT_STK_NUM(env, cp, h, indx, ret);
		if ((t_ret = __memp_fput(mpf,
		     dbc->thread_info, h, dbc->priority)) != 0 && ret == 0)
			ret = t_ret;
		h = NULL;
	} else {
		if (LF_ISSET(SR_DEL) && cp->csp == cp->sp)
			cp->csp++;
		BT_STK_ENTER(env, cp, h, indx, lock, lock_mode, ret);
	}
	if (ret != 0)
		goto err;

	cp->csp->lock = lock;
	DB_ASSERT(env, parent_h == NULL);

done:
	if (F_ISSET(dbc, DBC_OPD))
		LOCK_CHECK_ON(dbc->thread_info);

	if ((ret = __LPUT(dbc, saved_lock)) != 0)
		return (ret);

	return (0);

err:	if (ret == 0)
		ret = t_ret;
	if (h != NULL && (t_ret = __memp_fput(mpf,
	    dbc->thread_info, h, dbc->priority)) != 0 && ret == 0)
		ret = t_ret;
	if (parent_h != NULL && (t_ret = __memp_fput(mpf,
	    dbc->thread_info, parent_h, dbc->priority)) != 0 && ret == 0)
		ret = t_ret;

	/* Keep any not-found page locked for serializability. */
	if ((t_ret = __TLPUT(dbc, lock)) != 0 && ret == 0)
		ret = t_ret;

	(void)__LPUT(dbc, saved_lock);

	BT_STK_POP(cp);
	(void)__bam_stkrel(dbc, 0);

	if (F_ISSET(dbc, DBC_OPD))
		LOCK_CHECK_ON(dbc->thread_info);

	return (ret);
}

/*
 * __bam_stkrel --
 *	Release all pages currently held in the stack.
 *
 * PUBLIC: int __bam_stkrel __P((DBC *, u_int32_t));
 */
int
__bam_stkrel(dbc, flags)
	DBC *dbc;
	u_int32_t flags;
{
	BTREE_CURSOR *cp;
	DB *dbp;
	DB_MPOOLFILE *mpf;
	EPG *epg;
	int ret, t_ret;

	DB_ASSERT(NULL, dbc != NULL);
	dbp = dbc->dbp;
	mpf = dbp->mpf;
	cp = (BTREE_CURSOR *)dbc->internal;

	/*
	 * Release inner pages first.
	 *
	 * The caller must be sure that setting STK_NOLOCK will not effect
	 * either serializability or recoverability.
	 */
	for (ret = 0, epg = cp->sp; epg <= cp->csp; ++epg) {
		if (epg->page != NULL) {
			if (LF_ISSET(STK_CLRDBC) && cp->page == epg->page) {
				cp->page = NULL;
				LOCK_INIT(cp->lock);
			}
			if ((t_ret = __memp_fput(mpf, dbc->thread_info,
			     epg->page, dbc->priority)) != 0 && ret == 0)
				ret = t_ret;
			epg->page = NULL;
		}
		/*
		 * We set this if we need to release our pins,
		 * but are not logically ready to have the pages
		 * visible.
		 */
		if (LF_ISSET(STK_PGONLY))
			continue;
		if (LF_ISSET(STK_NOLOCK) &&
		    (epg->lock.mode == DB_LOCK_READ ||
		    atomic_read(&mpf->mfp->multiversion) == 0)) {
			if ((t_ret = __LPUT(dbc, epg->lock)) != 0 && ret == 0)
				ret = t_ret;
		} else
			if ((t_ret = __TLPUT(dbc, epg->lock)) != 0 && ret == 0)
				ret = t_ret;
	}

	/* Clear the stack, all pages have been released. */
	if (!LF_ISSET(STK_PGONLY))
		BT_STK_CLR(cp);

	return (ret);
}

/*
 * __bam_stkgrow --
 *	Grow the stack.
 *
 * PUBLIC: int __bam_stkgrow __P((ENV *, BTREE_CURSOR *));
 */
int
__bam_stkgrow(env, cp)
	ENV *env;
	BTREE_CURSOR *cp;
{
	EPG *p;
	size_t entries;
	int ret;

	entries = cp->esp - cp->sp;

	if ((ret = __os_calloc(env, entries * 2, sizeof(EPG), &p)) != 0)
		return (ret);
	memcpy(p, cp->sp, entries * sizeof(EPG));
	if (cp->sp != cp->stack)
		__os_free(env, cp->sp);
	cp->sp = p;
	cp->csp = p + entries;
	cp->esp = p + entries * 2;
	return (0);
}
