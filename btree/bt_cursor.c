/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)bt_cursor.c	11.21 (Sleepycat) 11/10/99";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <errno.h>
#include <stdlib.h>
#include <string.h>
#endif

#include "db_int.h"
#include "db_page.h"
#include "db_shash.h"
#include "btree.h"
#include "lock.h"
#include "qam.h"

static int __bam_c_close __P((DBC *));
static int __bam_c_del __P((DBC *, u_int32_t));
static int __bam_c_destroy __P((DBC *));
static int __bam_c_first __P((DBC *));
static int __bam_c_get __P((DBC *, DBT *, DBT *, u_int32_t));
static int __bam_c_getstack __P((DBC *));
static int __bam_c_last __P((DBC *));
static int __bam_c_next __P((DBC *, int));
static int __bam_c_physdel __P((DBC *));
static int __bam_c_prev __P((DBC *));
static int __bam_c_put __P((DBC *, DBT *, DBT *, u_int32_t));
static void __bam_c_reset __P((BTREE_CURSOR *));
static int __bam_c_rget __P((DBC *, DBT *, u_int32_t));
static int __bam_c_search __P((DBC *, const DBT *, u_int32_t, int *));
static int __bam_dsearch __P((DBC *, DBT *, u_int32_t *));
static int __bam_dup __P((DBC *, u_int32_t, int));

/*
 * Acquire a new page/lock for the cursor.  If we hold a page/lock, discard
 * the page, and lock-couple the lock.
 *
 * !!!
 * We have to handle both where we have a lock to lock-couple and where we
 * don't -- we don't duplicate locks when we duplicate cursors if we are
 * running in a transaction environment as there's no point if locks are
 * never discarded.  This means that the cursor may or may no hold a lock.
 */
#undef	ACQUIRE
#define	ACQUIRE(dbc, pgno, mode, ret) {					\
	BTREE_CURSOR *__cp = (dbc)->internal;				\
	if (__cp->page != NULL) {					\
		ret = memp_fput((dbc)->dbp->mpf, __cp->page, 0);	\
		__cp->page = NULL;					\
	} else								\
		ret = 0;						\
	if (ret == 0 && mode != DB_LOCK_NG &&				\
	    (ret = __db_lget(dbc,					\
	    __cp->lock.off == LOCK_INVALID ? 0 : 1,			\
	    pgno, mode, 0, &__cp->lock)) != 0)				\
		__cp->lock_mode = mode;					\
	if (ret == 0)							\
		ret = memp_fget(					\
		    (dbc)->dbp->mpf, &(pgno), 0, &__cp->page);		\
}

/*
 * Acquire a write lock if we don't already have one.
 *
 * !!!
 * See ACQUIRE macro on why we handle cursors that don't have locks.
 */
#undef	ACQUIRE_WRITE_LOCK
#define	ACQUIRE_WRITE_LOCK(dbc, ret) {					\
	BTREE_CURSOR *__cp = (dbc)->internal;				\
	if (F_ISSET((dbc)->dbp->dbenv, DB_ENV_LOCKING) &&		\
	    __cp->lock_mode != DB_LOCK_WRITE &&				\
	    ((ret) = __db_lget(dbc,					\
	    __cp->lock.off == LOCK_INVALID ? 0 : 1,			\
	    __cp->pgno, DB_LOCK_WRITE, 0, &__cp->lock)) == 0)		\
		__cp->lock_mode = DB_LOCK_WRITE;			\
}

/* Discard the current page/lock held by a cursor. */
#undef	DISCARD
#define	DISCARD(dbc, ret) {						\
	BTREE_CURSOR *__cp = (dbc)->internal;				\
	int __t_ret;							\
	if (__cp->page != NULL) {					\
		ret = memp_fput((dbc)->dbp->mpf, __cp->page, 0);	\
		__cp->page = NULL;					\
	} else								\
		ret = 0;						\
	if (__cp->lock.off != LOCK_INVALID) {				\
		if ((__t_ret =						\
		    __TLPUT((dbc), __cp->lock)) != 0 &&	(ret) == 0)	\
			ret = __t_ret;					\
		__cp->lock.off = LOCK_INVALID;				\
		__cp->lock_mode = DB_LOCK_NG;				\
	}								\
}

/* If the cursor references a deleted record. */
#undef	IS_CUR_DELETED
#define	IS_CUR_DELETED(cp)						\
	(((cp)->dpgno == PGNO_INVALID &&				\
	B_DISSET(GET_BKEYDATA((cp)->page,				\
	(cp)->indx + O_INDX)->type)) ||					\
	((cp)->dpgno != PGNO_INVALID &&					\
	B_DISSET(GET_BKEYDATA((cp)->page, (cp)->dindx)->type)))

/* If the cursor and index combination references a deleted record. */
#undef	IS_DELETED
#define	IS_DELETED(cp, indx)						\
	(((cp)->dpgno == PGNO_INVALID &&				\
	B_DISSET(GET_BKEYDATA((cp)->page, (indx) + O_INDX)->type)) ||	\
	((cp)->dpgno != PGNO_INVALID &&					\
	B_DISSET(GET_BKEYDATA((cp)->page, (indx))->type)))

/*
 * Test to see if two cursors could point to duplicates of the same key,
 * whether on-page or off-page.  The leaf page numbers must be the same
 * in both cases.  In the case of off-page duplicates, the key indices
 * on the leaf page will be the same.  In the case of on-page duplicates,
 * the duplicate page number must not be set, and the key index offsets
 * must be the same.  For the last test, as the saved copy of the cursor
 * will not have a valid page pointer, we use the cursor's.
 */
#undef	POSSIBLE_DUPLICATE
#define	POSSIBLE_DUPLICATE(cursor, copy)				\
	((cursor)->pgno == (copy)->pgno &&				\
	((cursor)->indx == (copy)->indx ||				\
	((cursor)->dpgno == PGNO_INVALID &&				\
	    (copy)->dpgno == PGNO_INVALID &&				\
	    (cursor)->page->inp[(cursor)->indx] ==			\
	    (cursor)->page->inp[(copy)->indx])))

/*
 * __bam_c_reset --
 *	Initialize internal cursor structure.
 */
static void
__bam_c_reset(cp)
	BTREE_CURSOR *cp;
{
	cp->sp = cp->csp = cp->stack;
	cp->esp = cp->stack + sizeof(cp->stack) / sizeof(cp->stack[0]);
	cp->page = NULL;
	cp->pgno = PGNO_INVALID;
	cp->indx = 0;
	cp->dpgno = PGNO_INVALID;
	cp->dindx = 0;
	cp->lock.off = LOCK_INVALID;
	cp->lock_mode = DB_LOCK_NG;
	cp->recno = RECNO_OOB;
	cp->flags = 0;
}

/*
 * __bam_c_init --
 *	Initialize the access private portion of a cursor
 *
 * PUBLIC: int __bam_c_init __P((DBC *));
 */
int
__bam_c_init(dbc)
	DBC *dbc;
{
	BTREE_CURSOR *cp;
	DB *dbp;
	int ret;

	dbp = dbc->dbp;

	/* Allocate the internal structure. */
	if ((ret = __os_calloc(1, sizeof(BTREE_CURSOR), &cp)) != 0)
		return (ret);

	/*
	 * Logical record numbers are always the same size, and we don't want
	 * to have to check for space every time we return one.  Allocate it
	 * in advance.
	 */
	if (dbp->type == DB_RECNO || F_ISSET(dbp, DB_BT_RECNUM)) {
		if ((ret = __os_malloc(sizeof(db_recno_t),
		    NULL, &dbc->rkey.data)) != 0) {
			__os_free(cp, sizeof(BTREE_CURSOR));
			return (ret);
		}
		dbc->rkey.ulen = sizeof(db_recno_t);
	}

	/* Initialize methods. */
	dbc->internal = cp;
	if (dbp->type == DB_BTREE) {
		dbc->c_am_close = __bam_c_close;
		dbc->c_am_destroy = __bam_c_destroy;
		dbc->c_del = __bam_c_del;
		dbc->c_get = __bam_c_get;
		dbc->c_put = __bam_c_put;
	} else {
		dbc->c_am_close = __bam_c_close;
		dbc->c_am_destroy = __bam_c_destroy;
		dbc->c_del = __ram_c_del;
		dbc->c_get = __ram_c_get;
		dbc->c_put = __ram_c_put;
	}

	/* Initialize dynamic information. */
	__bam_c_reset(cp);

	return (0);
}

/*
 * __bam_c_dup --
 *	Duplicate a btree cursor, such that the new one holds appropriate
 *	locks for the position of the original.
 *
 * PUBLIC: int __bam_c_dup __P((DBC *, DBC *));
 */
int
__bam_c_dup(orig_dbc, new_dbc)
	DBC *orig_dbc, *new_dbc;
{
	BTREE_CURSOR *orig, *new;

	orig = orig_dbc->internal;
	new = new_dbc->internal;

	__bam_c_reset(new);

	new->pgno = orig->pgno;
	new->indx = orig->indx;
	new->dpgno = orig->dpgno;
	new->dindx = orig->dindx;

	new->recno = orig->recno;

	new->lock_mode = orig->lock_mode;

	if (orig->lock.off == LOCK_INVALID)
		return (0);

	/*
	 * If we are in a transaction, then we do not need to reacquire
	 * a lock, because we automatically hold all locks until transaction
	 * completion.
	 */
	if (orig_dbc->txn == NULL)
		return (__db_lget(new_dbc,
		    0, new->pgno, new->lock_mode, 0, &new->lock));

	return (0);
}

/*
 * __bam_c_close --
 *	Close down the cursor from a single use.
 */
static int
__bam_c_close(dbc)
	DBC *dbc;
{
	BTREE_CURSOR *cp;
	DB *dbp;
	int ret, t_ret;

	dbp = dbc->dbp;
	cp = dbc->internal;
	ret = 0;

	/*
	 * If a cursor deleted a btree key, perform the actual deletion.
	 * (Recno keys are either deleted immediately or never deleted.)
	 */
	if (dbp->type == DB_BTREE && F_ISSET(cp, C_DELETED))
		ret = __bam_c_physdel(dbc);

	/* Discard any locks not acquired inside of a transaction. */
	if (cp->lock.off != LOCK_INVALID) {
		if ((t_ret = __TLPUT(dbc, cp->lock)) != 0 && ret == 0)
			ret = t_ret;
		cp->lock.off = LOCK_INVALID;
	}

	/* Confirm that the stack has been emptied. */
	DB_ASSERT(cp->csp == cp->stack);

	/* Initialize dynamic information. */
	__bam_c_reset(cp);

	return (ret);
}

/*
 * __bam_c_destroy --
 *	Close a single cursor -- internal version.
 */
static int
__bam_c_destroy(dbc)
	DBC *dbc;
{
	/* Discard the structures. */
	__os_free(dbc->internal, sizeof(BTREE_CURSOR));

	return (0);
}

/*
 * __bam_c_del --
 *	Delete using a cursor.
 */
static int
__bam_c_del(dbc, flags)
	DBC *dbc;
	u_int32_t flags;
{
	BTREE_CURSOR *cp;
	DB *dbp;
	PAGE *h;
	db_pgno_t pgno;
	db_indx_t indx;
	int ret;

	dbp = dbc->dbp;
	cp = dbc->internal;
	h = NULL;

	PANIC_CHECK(dbp->dbenv);

	/* Check for invalid flags. */
	if ((ret = __db_cdelchk(dbp, flags,
	    F_ISSET(dbp, DB_AM_RDONLY), cp->pgno != PGNO_INVALID)) != 0)
		return (ret);

	DEBUG_LWRITE(dbc, dbc->txn, "bam_c_del", NULL, NULL, flags);

	/* If already deleted, return failure. */
	if (F_ISSET(cp, C_DELETED))
		return (DB_KEYEMPTY);

	/*
	 * If we are running CDB, this had better be either a write
	 * cursor or an immediate writer.  If it's a regular writer,
	 * that means we have an IWRITE lock and we need to upgrade
	 * it to a write lock.
	 */
	if (F_ISSET(dbp->dbenv, DB_ENV_CDB)) {
		if (!F_ISSET(dbc, DBC_WRITECURSOR | DBC_WRITER))
			return (EPERM);

		if (F_ISSET(dbc, DBC_WRITECURSOR) &&
		    (ret = lock_get(dbp->dbenv, dbc->locker,
		    DB_LOCK_UPGRADE, &dbc->lock_dbt, DB_LOCK_WRITE,
		    &dbc->mylock)) != 0)
			return (ret);
	}

	/*
	 * We don't physically delete the record until the cursor moves, so
	 * we have to have a long-lived write lock on the page instead of a
	 * a long-lived read lock.
	 *
	 * We have to have a read lock to even get here.  We lock-couple so
	 * that we don't lose our read lock on failure.  That's probably not
	 * necessary, our only failure mode is deadlock and once we deadlock
	 * the cursor shouldn't have to support further operations.
	 */
	ACQUIRE_WRITE_LOCK(dbc, ret);
	if (ret != 0)
		goto err;

	/*
	 * Acquire the underlying page and set the on-page and in-cursor
	 * delete flags.
	 */
	if (cp->dpgno == PGNO_INVALID) {
		pgno = cp->pgno;
		indx = cp->indx;
	} else {
		pgno = cp->dpgno;
		indx = cp->dindx;
	}

	if ((ret = memp_fget(dbp->mpf, &pgno, 0, &h)) != 0)
		goto err;

	/* Log the change. */
	if (DB_LOGGING(dbc) &&
	    (ret = __bam_cdel_log(dbp->dbenv, dbc->txn, &LSN(h),
	    0, dbp->log_fileid, PGNO(h), &LSN(h), indx)) != 0)
		goto err;

	/* Set the intent-to-delete flag on the page and update all cursors. */
	if (cp->dpgno == PGNO_INVALID)
		B_DSET(GET_BKEYDATA(h, indx + O_INDX)->type);
	else
		B_DSET(GET_BKEYDATA(h, indx)->type);
	(void)__bam_ca_delete(dbp, pgno, indx, 1);

	if ((ret = memp_fput(dbp->mpf, h, DB_MPOOL_DIRTY)) != 0)
		goto err;
	h = NULL;

	/*
	 * If the tree has record numbers, we have to adjust the counts.
	 *
	 * !!!
	 * This test is right -- we don't yet support duplicates and record
	 * numbers in the same tree, so ignore duplicates if DB_BT_RECNUM
	 * set.
	 */
	if (F_ISSET(dbp, DB_BT_RECNUM)) {
		if ((ret = __bam_c_getstack(dbc)) != 0)
			goto err;
		if ((ret = __bam_adjust(dbc, -1)) != 0)
			goto err;
		(void)__bam_stkrel(dbc, 0);
	}

err:	if (h != NULL)
		(void)memp_fput(dbp->mpf, h, 0);

	/* Release the upgraded lock. */
	if (F_ISSET(dbc, DBC_WRITECURSOR))
		(void)__lock_downgrade(dbp->dbenv,
		    &dbc->mylock, DB_LOCK_IWRITE, 0);

	return (ret);
}

/*
 * __bam_c_get --
 *	Get using a cursor (btree).
 */
static int
__bam_c_get(dbc_orig, key, data, flags)
	DBC *dbc_orig;
	DBT *key, *data;
	u_int32_t flags;
{
	BTREE_CURSOR *cp, *orig, start;
	DB *dbp;
	DBC *dbc;
	PAGE *h;
	u_int32_t tmp_rmw;
	int exact, ret;

	dbp = dbc_orig->dbp;
	orig = dbc_orig->internal;

	PANIC_CHECK(dbp->dbenv);

	/* Check for invalid flags. */
	if ((ret = __db_cgetchk(dbp,
	    key, data, flags, orig->pgno != PGNO_INVALID)) != 0)
		return (ret);

	/* Clear OR'd in additional bits so we can check for flag equality. */
	tmp_rmw = LF_ISSET(DB_RMW);
	LF_CLR(DB_RMW);

	DEBUG_LREAD(dbc_orig, dbc_orig->txn, "bam_c_get",
	    flags == DB_SET || flags == DB_SET_RANGE ? key : NULL, NULL, flags);

	/*
	 * Return a cursor's record number.  It has nothing to do with the
	 * cursor get code except that it's been rammed into the interface.
	 */
	if (flags == DB_GET_RECNO)
		return (__bam_c_rget(dbc_orig, data, flags | tmp_rmw));

	/* Get a copy of the original cursor, including position. */
	if ((ret = dbc_orig->c_dup(dbc_orig, &dbc, DB_POSITIONI)) != 0)
		return (ret);
	if (tmp_rmw)
		F_SET(dbc, DBC_RMW);
	cp = dbc->internal;

	switch (flags) {
	case DB_CURRENT:
		/* It's not possible to return a deleted record. */
		if (F_ISSET(orig, C_DELETED)) {
			ret = DB_KEYEMPTY;
			goto err;
		}

		/*
		 * Acquire the current page.  We have at least a read-lock
		 * already.  The caller may have set DB_RMW asking for a
		 * write lock, but any upgrade to a write lock has no better
		 * chance of succeeding now instead of later, so we don't try.
		 */
		if ((ret = memp_fget(dbp->mpf,
		    cp->dpgno == PGNO_INVALID ?
		    &cp->pgno : &cp->dpgno, 0, &cp->page)) != 0)
			goto err;
		break;
	case DB_NEXT_DUP:
		if (cp->pgno == PGNO_INVALID) {
			ret = EINVAL;
			goto err;
		}
		if ((ret = __bam_c_next(dbc, 1)) != 0)
			goto err;

		/* Make sure we didn't go past the end of the duplicates. */
		if (!POSSIBLE_DUPLICATE(cp, orig)) {
			ret = DB_NOTFOUND;
			goto err;
		}
		break;
	case DB_NEXT:
		if (cp->pgno != PGNO_INVALID) {
			if ((ret = __bam_c_next(dbc, 1)) != 0)
				goto err;
			break;
		}
		/* FALLTHROUGH */
	case DB_FIRST:
		if ((ret = __bam_c_first(dbc)) != 0)
			goto err;
		break;
	case DB_PREV:
		if (cp->pgno != PGNO_INVALID) {
			if ((ret = __bam_c_prev(dbc)) != 0)
				goto err;
			break;
		}
		/* FALLTHROUGH */
	case DB_LAST:
		if ((ret = __bam_c_last(dbc)) != 0)
			goto err;
		break;
	case DB_SET:
		if ((ret = __bam_c_search(dbc, key, flags, &exact)) != 0)
			goto err;

		/*
		 * We cannot be referencing a non-existent or deleted record
		 * because we specified an exact match.  We may be referencing
		 * off-page duplicates.
		 *
		 * If we're referencing off-page duplicates, move off-page.
		 * If we moved off-page, move to the next non-deleted record.
		 * If we moved to the next non-deleted record, check to make
		 * sure we didn't switch records because our current record
		 * had no non-deleted data items.
		 */
		start = *cp;
		if ((ret = __bam_dup(dbc, cp->indx, 0)) != 0)
			goto err;
		if (cp->dpgno != PGNO_INVALID && IS_CUR_DELETED(cp)) {
			if ((ret = __bam_c_next(dbc, 0)) != 0)
				goto err;
			if (!POSSIBLE_DUPLICATE(cp, &start)) {
				ret = DB_NOTFOUND;
				goto err;
			}
		}
		break;
	case DB_SET_RECNO:
		if ((ret = __bam_c_search(dbc, key, flags, &exact)) != 0)
			goto err;
		break;
	case DB_GET_BOTH:
		if (F_ISSET(dbc, DBC_CONTINUE)) {
			/* Acquire the current page. */
			if ((ret = memp_fget(dbp->mpf,
			    cp->dpgno == PGNO_INVALID ?
			    &cp->pgno : &cp->dpgno, 0, &cp->page)) != 0)
				goto err;

			/* Move to the next item. */
			start = *cp;
			if ((ret = __bam_c_next(dbc, 1)) != 0)
				goto err;
			/* Verify that we haven't moved to a new key. */
			if (!POSSIBLE_DUPLICATE(cp, &start)) {
				ret = DB_NOTFOUND;
				goto err;
			}
		} else {
			if ((ret =
			    __bam_c_search(dbc, key, flags, &exact)) != 0)
				goto err;

			/*
			 * We cannot be referencing a non-existent or deleted
			 * record because we specified an exact match.  We may
			 * be referencing off-page duplicates.
			 */
			if ((ret = __bam_dup(dbc, cp->indx, 0)) != 0)
				goto err;
		}

		/* Search for a matching entry. */
		if ((ret = __bam_dsearch(dbc, data, NULL)) != 0)
			goto err;

		/* Ignore deleted entries. */
		if (IS_CUR_DELETED(cp)) {
			ret = DB_NOTFOUND;
			goto err;
		}
		break;
	case DB_SET_RANGE:
		if ((ret = __bam_c_search(dbc, key, flags, &exact)) != 0)
			goto err;

		/*
		 * As we didn't require an exact match, the search function
		 * may have returned an entry past the end of the page.  Or,
		 * we may be referencing a deleted record.  If so, move to
		 * the next entry.
		 */
		if (cp->indx == NUM_ENT(cp->page) || IS_CUR_DELETED(cp))
			if ((ret = __bam_c_next(dbc, 0)) != 0)
				goto err;

		/*
		 * If we're referencing off-page duplicates, move off-page.
		 * If we moved off-page, move to the next non-deleted record.
		 */
		if ((ret = __bam_dup(dbc, cp->indx, 0)) != 0)
			goto err;
		if (cp->dpgno != PGNO_INVALID && IS_CUR_DELETED(cp))
			if ((ret = __bam_c_next(dbc, 0)) != 0)
				goto err;
		break;
	}

	/*
	 * Return the key if the user didn't give us one.  If we've moved to
	 * a duplicate page, we may no longer have a pointer to the main page,
	 * so we have to go get it.  We know that it's already read-locked,
	 * however, so we don't have to acquire a new lock.
	 */
	if (flags != DB_SET) {
		if (cp->dpgno != PGNO_INVALID) {
			if ((ret = memp_fget(dbp->mpf, &cp->pgno, 0, &h)) != 0)
				goto err;
		} else
			h = cp->page;
		ret = __db_ret(dbp,
		    h, cp->indx, key, &dbc->rkey.data, &dbc->rkey.ulen);
		if (cp->dpgno != PGNO_INVALID)
			(void)memp_fput(dbp->mpf, h, 0);
		if (ret)
			goto err;
	}

	/* Return the data. */
	if ((ret = __db_ret(dbp, cp->page,
	    cp->dpgno == PGNO_INVALID ? cp->indx + O_INDX : cp->dindx,
	    data, &dbc->rdata.data, &dbc->rdata.ulen)) != 0)
		goto err;

	/* Release the current page. */
	if ((ret = memp_fput(dbp->mpf, cp->page, 0)) != 0)
		goto err;
	cp->page = NULL;

	/* Release the temporary lock upgrade. */
	if (tmp_rmw)
		F_CLR(dbc, DBC_RMW);

	/*
	 * Swap the cursors so we are left with the new position inside of
	 * the original DBCs structure, and close the dup'd cursor once it
	 * references the old position.
	 *
	 * The close can fail, but we only expect DB_LOCK_DEADLOCK failures.
	 * This violates our "the cursor is unchanged on error" semantics,
	 * but since all you can do with a DB_LOCK_DEADLOCK failure is close
	 * the cursor, I believe that's OK.
	 */
	orig = dbc_orig->internal;
	dbc_orig->internal = dbc->internal;
	dbc->internal = orig;
	ret = dbc->c_close(dbc);

	if (0) {
err:		/* Discard any page we acquired. */
		if (cp->page != NULL)
			(void)memp_fput(dbp->mpf, cp->page, 0);

		/* Close the newly dup'd cursor. */
		(void)dbc->c_close(dbc);
	}

	return (ret);
}

/*
 * __bam_dsearch --
 *	Search for a matching data item (or the first data item that's
 *	equal to or greater than the one we're searching for).
 */
static int
__bam_dsearch(dbc, data, iflagp)
	DBC *dbc;
	DBT *data;
	u_int32_t *iflagp;	/* Non-NULL if we're doing an insert. */
{
	BTREE_CURSOR *cp, copy, last;
	DB *dbp;
	int cmp, ret;

	dbp = dbc->dbp;
	cp = dbc->internal;

	/* If the duplicates are off-page, use the duplicate search routine. */
	if (cp->dpgno != PGNO_INVALID) {
		if ((ret = __db_dsearch(dbc, iflagp != NULL,
		    data, cp->dpgno, &cp->dindx, &cp->page, &cmp)) != 0)
			return (ret);
		cp->dpgno = cp->page->pgno;

		if (iflagp == NULL) {
			if (cmp != 0)
				return (DB_NOTFOUND);
			return (0);
		}
		*iflagp = DB_BEFORE;
		return (0);
	}

	/* Otherwise, do the search ourselves. */
	copy = *cp;
	for (;;) {
		/* Save the last interesting cursor position. */
		last = *cp;

		/* See if the data item matches the one we're looking for. */
		if ((cmp = __bam_cmp(dbp, data, cp->page, cp->indx + O_INDX,
		    dbp->dup_compare == NULL ?
		    __bam_defcmp : dbp->dup_compare)) == 0) {
			if (iflagp != NULL)
				*iflagp = DB_AFTER;
			return (0);
		}

		/*
		 * If duplicate entries are sorted, we're done if we find a
		 * page entry that sorts greater than the application item.
		 * If doing an insert, return success, otherwise DB_NOTFOUND.
		 */
		if (dbp->dup_compare != NULL && cmp < 0) {
			if (iflagp == NULL)
				return (DB_NOTFOUND);
			*iflagp = DB_BEFORE;
			return (0);
		}

		/*
		 * Move to the next item.  If we reach the end of the page and
		 * we're doing an insert, set the cursor to the last item and
		 * set the referenced memory location so callers know to insert
		 * after the item, instead of before it.  If not inserting, we
		 * return DB_NOTFOUND.
		 */
		if ((cp->indx += P_INDX) >= NUM_ENT(cp->page)) {
			if (iflagp == NULL)
				return (DB_NOTFOUND);
			goto use_last;
		}

		/*
		 * Make sure we didn't go past the end of the duplicates.  The
		 * error conditions are the same as above.
		 */
		if (!POSSIBLE_DUPLICATE(cp, &copy)) {
			if (iflagp == NULL)
				 return (DB_NOTFOUND);
use_last:		*cp = last;
			*iflagp = DB_AFTER;
			return (0);
		}
	}
	/* NOTREACHED */
}

/*
 * __bam_c_rget --
 *	Return the record number for a cursor.
 */
static int
__bam_c_rget(dbc, data, flags)
	DBC *dbc;
	DBT *data;
	u_int32_t flags;
{
	BTREE_CURSOR *cp;
	DB *dbp;
	DBT dbt;
	db_recno_t recno;
	int exact, ret;

	COMPQUIET(flags, 0);
	dbp = dbc->dbp;
	cp = dbc->internal;

	/* Get the page with the current item on it. */
	if ((ret = memp_fget(dbp->mpf, &cp->pgno, 0, &cp->page)) != 0)
		return (ret);

	/* Get a copy of the key. */
	memset(&dbt, 0, sizeof(DBT));
	dbt.flags = DB_DBT_MALLOC | DB_DBT_INTERNAL;
	if ((ret = __db_ret(dbp, cp->page, cp->indx, &dbt, NULL, NULL)) != 0)
		goto err;

	exact = 1;
	if ((ret = __bam_search(dbc, &dbt,
	    F_ISSET(dbc, DBC_RMW) ? S_FIND_WR : S_FIND,
	    1, &recno, &exact)) != 0)
		goto err;

	ret = __db_retcopy(dbp, data,
	    &recno, sizeof(recno), &dbc->rdata.data, &dbc->rdata.ulen);

	/* Release the stack. */
	__bam_stkrel(dbc, 0);

err:	(void)memp_fput(dbp->mpf, cp->page, 0);
	__os_free(dbt.data, dbt.size);
	return (ret);
}

/*
 * __bam_c_put --
 *	Put using a cursor.
 */
static int
__bam_c_put(dbc_orig, key, data, flags)
	DBC *dbc_orig;
	DBT *key, *data;
	u_int32_t flags;
{
	BTREE_CURSOR *cp, *orig;
	DB *dbp;
	DBC *dbc;
	DBT dbt;
	db_indx_t indx;
	db_pgno_t pgno;
	u_int32_t iiop;
	int exact, needkey, ret, ret_ignore, stack;
	void *arg;

	dbp = dbc_orig->dbp;
	orig = dbc_orig->internal;

	PANIC_CHECK(dbp->dbenv);

	/* Check for invalid flags. */
	if ((ret = __db_cputchk(dbp, key, data, flags,
	    F_ISSET(dbp, DB_AM_RDONLY), orig->pgno != PGNO_INVALID)) != 0)
		return (ret);

	DEBUG_LWRITE(dbc_orig, dbc_orig->txn, "bam_c_put",
	    flags == DB_KEYFIRST || flags == DB_KEYLAST ? key : NULL,
	    data, flags);

	/*
	 * If we are running CDB, this had better be either a write
	 * cursor or an immediate writer.  If it's a regular writer,
	 * that means we have an IWRITE lock and we need to upgrade
	 * it to a write lock.
	 */
	if (F_ISSET(dbp->dbenv, DB_ENV_CDB)) {
		if (!F_ISSET(dbc_orig, DBC_WRITECURSOR | DBC_WRITER))
			return (EPERM);

		if (F_ISSET(dbc_orig, DBC_WRITECURSOR) &&
		    (ret = lock_get(dbp->dbenv, dbc_orig->locker,
		    DB_LOCK_UPGRADE, &dbc_orig->lock_dbt, DB_LOCK_WRITE,
		    &dbc_orig->mylock)) != 0)
			return (ret);
	}

	if (0) {
split:		/*
		 * To split, we need a valid key for the page, and since it's
		 * a cursor, we may have to build one.  Get a copy of a key
		 * from the page.
		 */
		if (needkey) {
			memset(&dbt, 0, sizeof(DBT));
			if ((ret = __db_ret(dbp, cp->page, indx,
			    &dbt, &dbc->rkey.data, &dbc->rkey.ulen)) != 0)
				goto err;
			arg = &dbt;
		} else
			arg = key;

		/*
		 * Discard any locks and pinned pages (the locks are discarded
		 * even if we're running with transactions, as they lock pages
		 * that we're sorry we ever acquired).  If stack is set and the
		 * cursor entries are valid, they point to the same entries as
		 * the stack, don't free them twice.
		 */
		if (stack) {
			(void)__bam_stkrel(dbc, 1);
			stack = 0;
		} else {
			DISCARD(dbc, ret);
			if (ret != 0)
				goto err;
		}

		/* Close the newly dup'd cursor. */
		(void)dbc->c_close(dbc);

		/* Split the tree. */
		if ((ret = __bam_split(dbc_orig, arg)) != 0)
			return (ret);
	}

	/* Get a copy of the original cursor, including position. */
	if ((ret = dbc_orig->c_dup(dbc_orig, &dbc, DB_POSITIONI)) != 0)
		return (ret);
	cp = dbc->internal;

	needkey = ret = stack = 0;
	switch (flags) {
	case DB_AFTER:
	case DB_BEFORE:
	case DB_CURRENT:
		needkey = 1;
		if (cp->dpgno == PGNO_INVALID) {
			pgno = cp->pgno;
			indx = cp->indx;
		} else {
			pgno = cp->dpgno;
			indx = cp->dindx;
		}

		/*
		 * !!!
		 * This test is right -- we don't yet support duplicates and
		 * record numbers in the same tree, so ignore duplicates if
		 * DB_BT_RECNUM set.
		 */
		if (F_ISSET(dbp, DB_BT_RECNUM) &&
		    (flags != DB_CURRENT || F_ISSET(orig, C_DELETED))) {
			/* Acquire a complete stack. */
			if ((ret = __bam_c_getstack(dbc)) != 0)
				goto err;
			cp->page = cp->csp->page;

			stack = 1;
		} else {
			/* Acquire the current page with a write lock. */
			ACQUIRE_WRITE_LOCK(dbc, ret);
			if (ret != 0)
				goto err;
			if ((ret =
			    memp_fget(dbp->mpf, &pgno, 0, &cp->page)) != 0)
				goto err;
		}
		iiop = flags;
		break;
	case DB_KEYFIRST:
	case DB_KEYLAST:
		/*
		 * If we have a duplicate comparison function, we position to
		 * the first of any on-page duplicates, and use __bam_dsearch
		 * to search for the right slot.  Otherwise, we position to
		 * the first/last of any on-page duplicates based on the flag
		 * value.
		 */
		if ((ret = __bam_c_search(dbc, key,
		    flags == DB_KEYFIRST || dbp->dup_compare != NULL ?
		    DB_KEYFIRST : DB_KEYLAST, &exact)) != 0)
			goto err;
		stack = 1;

		/*
		 * If an exact match:
		 *	If duplicates aren't supported, replace the current
		 *	item.  (When implementing the DB->put function, our
		 *	caller has already checked the DB_NOOVERWRITE flag.)
		 *
		 *	If there's a duplicate comparison function, find the
		 *	correct slot for this duplicate item.
		 *
		 *	If there's no duplicate comparison function, set the
		 *	insert flag based on the argument flags.
		 *
		 * If there's no match, the search function returned the
		 * smallest slot greater than the key, use it.
		 */
		if (exact) {
			if (F_ISSET(dbp, DB_AM_DUP)) {
				/*
				 * If at off-page duplicate page, move to the
				 * first or last entry -- if a comparison
				 * function was specified, start searching at
				 * the first entry.  Otherwise, move based on
				 * the DB_KEYFIRST/DB_KEYLAST flags.
				 */
				if ((ret = __bam_dup(dbc,
				    cp->indx, dbp->dup_compare == NULL &&
				    flags != DB_KEYFIRST)) != 0)
					goto err;

				/*
				 * If there's a comparison function, search for
				 * the correct slot.  Otherwise, set the insert
				 * flag based on the argment flag.
				 */
				if (dbp->dup_compare == NULL)
					iiop = flags == DB_KEYFIRST ?
					    DB_BEFORE : DB_AFTER;
				else
					if ((ret = __bam_dsearch(
					    dbc, data, &iiop)) != 0)
						goto err;
			} else
				iiop = DB_CURRENT;
		} else
			iiop = DB_KEYFIRST;

		if (cp->dpgno == PGNO_INVALID) {
			pgno = cp->pgno;
			indx = cp->indx;
		} else {
			pgno = cp->dpgno;
			indx = cp->dindx;
		}
		break;
	}

	ret = __bam_iitem(dbc, &cp->page, &indx, key, data, iiop, 0);
	if (ret == DB_NEEDSPLIT)
		goto split;
	if (ret != 0)
		goto err;

	/*
	 * Discard any pages pinned in the tree and their locks, except for
	 * the leaf page, for which we only discard the pin, not the lock.
	 *
	 * Note, the leaf page participated in the stack we acquired, and so
	 * we have to adjust the stack as necessary.  If there was only a
	 * single page on the stack, we don't have to free further stack pages.
	 */
	if (stack && BT_STK_POP(cp) != NULL)
		(void)__bam_stkrel(dbc, 0);

	/* Release the current page. */
	if ((ret = memp_fput(dbp->mpf, cp->page, 0)) != 0)
		goto err;

	/*
	 * Swap the cursors so we are left with the new position inside of
	 * the original DBCs structure, and close the dup'd cursor once it
	 * references the old position.
	 *
	 * The close can fail, but we only expect DB_LOCK_DEADLOCK failures.
	 * This violates our "the cursor is unchanged on error" semantics,
	 * but since all you can do with a DB_LOCK_DEADLOCK failure is close
	 * the cursor, I believe that's OK.
	 */
	orig = dbc_orig->internal;
	dbc_orig->internal = dbc->internal;
	dbc->internal = orig;
	ret = dbc->c_close(dbc);

	if (0) {
err:		/* Discard any page(s) we acquired. */
		if (stack)
			(void)__bam_stkrel(dbc, 0);
		else
			DISCARD(dbc, ret_ignore);

		/* Close the newly dup'd cursor. */
		(void)dbc->c_close(dbc);
	}

	/* Release the upgraded lock. */
	if (F_ISSET(dbc_orig, DBC_WRITECURSOR))
		(void)__lock_downgrade(dbp->dbenv,
		    &dbc_orig->mylock, DB_LOCK_IWRITE, 0);

	return (ret);
}

/*
 * __bam_c_first --
 *	Return the first record.
 */
static int
__bam_c_first(dbc)
	DBC *dbc;
{
	BTREE_CURSOR *cp;
	DB *dbp;
	db_pgno_t pgno;
	int ret;

	dbp = dbc->dbp;
	cp = dbc->internal;
	ret = 0;

	/* Walk down the left-hand side of the tree. */
	for (pgno = ((BTREE *)dbp->bt_internal)->bt_root;;) {
		ACQUIRE(dbc, pgno, DB_LOCK_READ, ret);
		if (ret != 0)
			return (ret);

		/* If we find a leaf page, we're done. */
		if (ISLEAF(cp->page))
			break;

		pgno = GET_BINTERNAL(cp->page, 0)->pgno;
	}

	/* If we want a write lock instead of a read lock, get it now. */
	if (F_ISSET(dbc, DBC_RMW)) {
		ACQUIRE_WRITE_LOCK(dbc, ret);
		if (ret != 0)
			return (ret);
	}

	cp->pgno = cp->page->pgno;
	cp->indx = 0;
	cp->dpgno = PGNO_INVALID;

	/*
	 * If we're referencing off-page duplicates, move off-page.
	 * If on an empty page or a deleted record, move to the next one.
	 */
	if (NUM_ENT(cp->page) > 0)
		if ((ret = __bam_dup(dbc, cp->indx, 0)) != 0)
			return (ret);
	if (NUM_ENT(cp->page) == 0 || IS_CUR_DELETED(cp))
		if ((ret = __bam_c_next(dbc, 0)) != 0)
			return (ret);

	return (0);
}

/*
 * __bam_c_last --
 *	Return the last record.
 */
static int
__bam_c_last(dbc)
	DBC *dbc;
{
	BTREE_CURSOR *cp;
	DB *dbp;
	db_pgno_t pgno;
	int ret;

	dbp = dbc->dbp;
	cp = dbc->internal;
	ret = 0;

	/* Walk down the right-hand side of the tree. */
	for (pgno = ((BTREE *)dbp->bt_internal)->bt_root;;) {
		ACQUIRE(dbc, pgno, DB_LOCK_READ, ret);
		if (ret != 0)
			return (ret);

		/* If we find a leaf page, we're done. */
		if (ISLEAF(cp->page))
			break;

		pgno =
		    GET_BINTERNAL(cp->page, NUM_ENT(cp->page) - O_INDX)->pgno;
	}

	/* If we want a write lock instead of a read lock, get it now. */
	if (F_ISSET(dbc, DBC_RMW)) {
		ACQUIRE_WRITE_LOCK(dbc, ret);
		if (ret != 0)
			return (ret);
	}

	cp->pgno = cp->page->pgno;
	cp->indx = NUM_ENT(cp->page) == 0 ? 0 : NUM_ENT(cp->page) - P_INDX;
	cp->dpgno = PGNO_INVALID;

	/*
	 * If we're referencing off-page duplicates, move off-page.
	 * If on an empty page or a deleted record, move to the previous one.
	 */
	if (NUM_ENT(cp->page) > 0)
		if ((ret = __bam_dup(dbc, cp->indx, 1)) != 0)
			return (ret);
	if (NUM_ENT(cp->page) == 0 || IS_CUR_DELETED(cp))
		if ((ret = __bam_c_prev(dbc)) != 0)
			return (ret);

	return (0);
}

/*
 * __bam_c_next --
 *	Move to the next record.
 */
static int
__bam_c_next(dbc, initial_move)
	DBC *dbc;
	int initial_move;
{
	BTREE_CURSOR *cp;
	DB *dbp;
	db_indx_t adjust, indx;
	db_lockmode_t lock_mode;
	db_pgno_t pgno;
	int ret;

	dbp = dbc->dbp;
	cp = dbc->internal;
	ret = 0;

	/*
	 * We're either moving through a page of duplicates or a btree leaf
	 * page.
	 */
	if (cp->dpgno == PGNO_INVALID) {
		adjust = dbp->type == DB_BTREE ? P_INDX : O_INDX;
		pgno = cp->pgno;
		indx = cp->indx;
		lock_mode =
		    F_ISSET(dbc, DBC_RMW) ? DB_LOCK_WRITE : DB_LOCK_READ;
	} else {
		adjust = O_INDX;
		pgno = cp->dpgno;
		indx = cp->dindx;
		lock_mode = DB_LOCK_NG;
	}
	if (cp->page == NULL) {
		ACQUIRE(dbc, pgno, lock_mode, ret);
		if (ret != 0)
			return (ret);
	}

	/*
	 * If at the end of the page, move to a subsequent page.
	 *
	 * !!!
	 * Check for >= NUM_ENT.  If we're here as the result of a search that
	 * landed us on NUM_ENT, we'll increment indx before we test.
	 *
	 * !!!
	 * This code handles empty pages and pages with only deleted entries.
	 */
	if (initial_move)
		indx += adjust;
	for (;;) {
		if (indx >= NUM_ENT(cp->page)) {
			/*
			 * If we're in a btree leaf page, we've reached the end
			 * of the tree.  If we've reached the end of a page of
			 * duplicates, continue from the btree leaf page where
			 * we found this page of duplicates.
			 */
			pgno = cp->page->next_pgno;
			if (pgno == PGNO_INVALID) {
				/* If in a btree leaf page, it's EOF. */
				if (cp->dpgno == PGNO_INVALID)
					return (DB_NOTFOUND);

				/* Continue from the last btree leaf page. */
				cp->dpgno = PGNO_INVALID;

				adjust = P_INDX;
				pgno = cp->pgno;
				indx = cp->indx + P_INDX;
				lock_mode = F_ISSET(dbc, DBC_RMW) ?
				    DB_LOCK_WRITE : DB_LOCK_READ;
			} else
				indx = 0;

			ACQUIRE(dbc, pgno, lock_mode, ret);
			if (ret != 0)
				return (ret);
			continue;
		}

		/* Ignore deleted records. */
		if (IS_DELETED(cp, indx)) {
			indx += adjust;
			continue;
		}

		/*
		 * If we're not in a duplicates page, check to see if we've
		 * found a page of duplicates, in which case we move to the
		 * first entry.
		 */
		if (cp->dpgno == PGNO_INVALID) {
			cp->pgno = cp->page->pgno;
			cp->indx = indx;

			if ((ret = __bam_dup(dbc, indx, 0)) != 0)
				return (ret);
			if (cp->dpgno != PGNO_INVALID) {
				indx = cp->dindx;
				adjust = O_INDX;
				continue;
			}
		} else {
			cp->dpgno = cp->page->pgno;
			cp->dindx = indx;
		}
		break;
	}
	return (0);
}

/*
 * __bam_c_prev --
 *	Move to the previous record.
 */
static int
__bam_c_prev(dbc)
	DBC *dbc;
{
	BTREE_CURSOR *cp;
	DB *dbp;
	db_indx_t indx, adjust;
	db_lockmode_t lock_mode;
	db_pgno_t pgno;
	int ret, set_indx;

	dbp = dbc->dbp;
	cp = dbc->internal;
	ret = 0;

	/*
	 * We're either moving through a page of duplicates or a btree leaf
	 * page.
	 */
	if (cp->dpgno == PGNO_INVALID) {
		adjust = dbp->type == DB_BTREE ? P_INDX : O_INDX;
		pgno = cp->pgno;
		indx = cp->indx;
		lock_mode =
		    F_ISSET(dbc, DBC_RMW) ? DB_LOCK_WRITE : DB_LOCK_READ;
	} else {
		adjust = O_INDX;
		pgno = cp->dpgno;
		indx = cp->dindx;
		lock_mode = DB_LOCK_NG;
	}
	if (cp->page == NULL) {
		ACQUIRE(dbc, pgno, lock_mode, ret);
		if (ret != 0)
			return (ret);
	}

	/*
	 * If at the beginning of the page, move to any previous one.
	 *
	 * !!!
	 * This code handles empty pages and pages with only deleted entries.
	 */
	for (;;) {
		if (indx == 0) {
			/*
			 * If we're in a btree leaf page, we've reached the
			 * beginning of the tree.  If we've reached the first
			 * of a page of duplicates, continue from the btree
			 * leaf page where we found this page of duplicates.
			 */
			pgno = cp->page->prev_pgno;
			if (pgno == PGNO_INVALID) {
				/* If in a btree leaf page, it's SOF. */
				if (cp->dpgno == PGNO_INVALID)
					return (DB_NOTFOUND);

				/* Continue from the last btree leaf page. */
				cp->dpgno = PGNO_INVALID;

				adjust = P_INDX;
				pgno = cp->pgno;
				indx = cp->indx;
				set_indx = 0;
				lock_mode = F_ISSET(dbc, DBC_RMW) ?
				    DB_LOCK_WRITE : DB_LOCK_READ;
			} else
				set_indx = 1;

			ACQUIRE(dbc, pgno, lock_mode, ret);
			if (ret != 0)
				return (ret);

			if (set_indx)
				indx = NUM_ENT(cp->page);
			if (indx == 0)
				continue;
		}

		/* Ignore deleted records. */
		indx -= adjust;
		if (IS_DELETED(cp, indx))
			continue;

		/*
		 * If we're not in a duplicates page, check to see if we've
		 * found a page of duplicates, in which case we move to the
		 * last entry.
		 */
		if (cp->dpgno == PGNO_INVALID) {
			cp->pgno = cp->page->pgno;
			cp->indx = indx;

			if ((ret = __bam_dup(dbc, indx, 1)) != 0)
				return (ret);
			if (cp->dpgno != PGNO_INVALID) {
				indx = cp->dindx + O_INDX;
				adjust = O_INDX;
				continue;
			}
		} else {
			cp->dpgno = cp->page->pgno;
			cp->dindx = indx;
		}
		break;
	}
	return (0);
}

/*
 * __bam_c_search --
 *	Move to a specified record.
 */
static int
__bam_c_search(dbc, key, flags, exactp)
	DBC *dbc;
	const DBT *key;
	u_int32_t flags;
	int *exactp;
{
	BTREE *t;
	BTREE_CURSOR *cp;
	DB *dbp;
	DB_LOCK lock;
	PAGE *h;
	db_recno_t recno;
	db_indx_t indx;
	u_int32_t sflags;
	int cmp, ret;

	dbp = dbc->dbp;
	cp = dbc->internal;
	t = dbp->bt_internal;
	ret = 0;

	/* Discard any previously held position. */
	DISCARD(dbc, ret);
	if (ret != 0)
		return (ret);

	/* Find an entry in the database. */
	switch (flags) {
	case DB_SET_RECNO:
		if ((ret = __ram_getno(dbc, key, &recno, 0)) != 0)
			return (ret);
		sflags = (F_ISSET(dbc, DBC_RMW) ? S_FIND_WR : S_FIND) | S_EXACT;
		ret = __bam_rsearch(dbc, &recno, sflags, 1, exactp);
		break;
	case DB_SET:
	case DB_GET_BOTH:
		sflags = (F_ISSET(dbc, DBC_RMW) ? S_FIND_WR : S_FIND) | S_EXACT;
		goto search;
	case DB_SET_RANGE:
		sflags =
		    (F_ISSET(dbc, DBC_RMW) ? S_WRITE : S_READ) | S_DUPFIRST;
		goto search;
	case DB_KEYFIRST:
		sflags = S_KEYFIRST;
		goto fast_search;
	case DB_KEYLAST:
		sflags = S_KEYLAST;
fast_search:	/*
		 * If the application has a history of inserting into the first
		 * or last pages of the database, we check those pages first to
		 * avoid doing a full search.
		 *
		 * Record numbers can't be fast-tracked, the entire tree has to
		 * be locked.
		 */
		h = NULL;
		lock.off = LOCK_INVALID;
		if (F_ISSET(dbp, DB_BT_RECNUM))
			goto search;

		/* Check if the application has a history of sorted input. */
		if (t->bt_lpgno == PGNO_INVALID)
			goto search;

		/*
		 * Lock and retrieve the page on which we did the last insert.
		 * It's okay if it doesn't exist, or if it's not the page type
		 * we expected, it just means that the world changed.
		 */
		cp->lock_mode = DB_LOCK_WRITE;
		if (__db_lget(dbc, 0, t->bt_lpgno, cp->lock_mode, 0, &lock))
			goto fast_miss;
		if (memp_fget(dbp->mpf, &t->bt_lpgno, 0, &h))
			goto fast_miss;
		if (TYPE(h) != P_LBTREE)
			goto fast_miss;
		if (NUM_ENT(h) == 0)
			goto fast_miss;

		/*
		 * What we do here is test to see if we're at the beginning or
		 * end of the tree and if the new item sorts before/after the
		 * first/last page entry.  We don't try and catch inserts into
		 * the middle of the tree (although we could, as long as there
		 * were two keys on the page and we saved both the index and
		 * the page number of the last insert).
		 */
		if (h->next_pgno == PGNO_INVALID) {
			indx = NUM_ENT(h) - P_INDX;
			if ((cmp =
			    __bam_cmp(dbp, key, h, indx, t->bt_compare)) < 0)
				goto try_begin;
			if (cmp > 0) {
				indx += P_INDX;
				goto fast_hit;
			}

			/*
			 * Found a duplicate.  If doing DB_KEYLAST, we're at
			 * the correct position, otherwise, move to the first
			 * of the duplicates.
			 */
			if (flags == DB_KEYLAST)
				goto fast_hit;
			for (;
			    indx > 0 && h->inp[indx - P_INDX] == h->inp[indx];
			    indx -= P_INDX)
				;
			goto fast_hit;
		}
try_begin:	if (h->prev_pgno == PGNO_INVALID) {
			indx = 0;
			if ((cmp =
			    __bam_cmp(dbp, key, h, indx, t->bt_compare)) > 0)
				goto fast_miss;
			if (cmp < 0)
				goto fast_hit;
			/*
			 * Found a duplicate.  If doing DB_KEYFIRST, we're at
			 * the correct position, otherwise, move to the last
			 * of the duplicates.
			 */
			if (flags == DB_KEYFIRST)
				goto fast_hit;
			for (;
			    indx < (db_indx_t)(NUM_ENT(h) - P_INDX) &&
			    h->inp[indx] == h->inp[indx + P_INDX];
			    indx += P_INDX)
				;
			goto fast_hit;
		}
		goto fast_miss;

fast_hit:	/* Set the exact match flag, we may have found a duplicate. */
		*exactp = cmp == 0;

		/* Enter the entry in the stack. */
		BT_STK_CLR(cp);
		BT_STK_ENTER(cp, h, indx, lock, cp->lock_mode, ret);
		break;

fast_miss:	if (h != NULL)
			(void)memp_fput(dbp->mpf, h, 0);
		/*
		 * This is not the right page, so logically we do not need to
		 * retain the lock.
		 */
		if (lock.off != LOCK_INVALID)
			(void)__LPUT(dbc, lock);

search:		ret = __bam_search(dbc, key, sflags, 1, NULL, exactp);
		break;
	default:				/* XXX: Impossible. */
		abort();
		/* NOTREACHED */
	}
	if (ret != 0)
		return (ret);

	/* Initialize the cursor to reference the returned page. */
	cp->page = cp->csp->page;
	cp->pgno = cp->csp->page->pgno;
	cp->indx = cp->csp->indx;
	cp->dpgno = PGNO_INVALID;
	cp->lock = cp->csp->lock;
	cp->lock_mode = cp->csp->lock_mode;

	/*
	 * If we inserted a key into the first or last slot of the tree,
	 * remember where it was so we can do it more quickly next time.
	 */
	if (flags == DB_KEYFIRST || flags == DB_KEYLAST)
		t->bt_lpgno =
		    ((cp->page->next_pgno == PGNO_INVALID &&
		    cp->indx >= NUM_ENT(cp->page)) ||
		    (cp->page->prev_pgno == PGNO_INVALID && cp->indx == 0)) ?
		    cp->pgno : PGNO_INVALID;

	return (0);
}

/*
 * __bam_dup --
 *	Check for an off-page duplicates entry, and if found, move to the
 *	first or last entry.
 */
static int
__bam_dup(dbc, indx, last_dup)
	DBC *dbc;
	u_int32_t indx;
	int last_dup;
{
	BOVERFLOW *bo;
	BTREE_CURSOR *cp;
	DB *dbp;
	db_pgno_t pgno;
	int ret;

	dbp = dbc->dbp;
	cp = dbc->internal;

	/* We should be referencing a valid entry on the page. */
	DB_ASSERT(NUM_ENT(cp->page) > 0);

	/*
	 * It's possible that the entry is deleted, in which case it doesn't
	 * have duplicates.
	 */
	if (IS_CUR_DELETED(cp))
		return (0);

	/*
	 * Check for an overflow entry.  If we find one, move to the
	 * duplicates page, and optionally move to the last record on
	 * that page.
	 *
	 * !!!
	 * We don't lock duplicates pages, we've already got the correct
	 * lock on the main page.
	 */
	bo = GET_BOVERFLOW(cp->page, indx + O_INDX);
	if (B_TYPE(bo->type) != B_DUPLICATE)
		return (0);

	pgno = bo->pgno;
	if ((ret = memp_fput(dbp->mpf, cp->page, 0)) != 0)
		return (ret);
	cp->page = NULL;
	if (last_dup) {
		if ((ret = __db_dend(dbc, pgno, &cp->page)) != 0)
			return (ret);
		indx = NUM_ENT(cp->page) - O_INDX;
	} else {
		if ((ret = memp_fget(dbp->mpf, &pgno, 0, &cp->page)) != 0)
			return (ret);
		indx = 0;
	}

	/* Update the cursor's duplicate information. */
	cp->dpgno = cp->page->pgno;
	cp->dindx = indx;

	return (0);
}

/*
 * __bam_c_physdel --
 *	Actually do the cursor deletion.
 */
static int
__bam_c_physdel(dbc)
	DBC *dbc;
{
	enum { DELETE_ITEM, DELETE_PAGE, NOTHING_FURTHER } cmd;
	BOVERFLOW bo;
	BTREE_CURSOR *cp;
	DB *dbp;
	DBT dbt;
	DB_LOCK lock;
	PAGE *h;
	db_indx_t indx;
	db_pgno_t pgno, next_pgno, prev_pgno, root_pgno;
	int delete_page, local_page, ret;

	dbp = dbc->dbp;
	cp = dbc->internal;
	delete_page = ret = 0;

	/* Figure out what we're deleting. */
	if (cp->dpgno == PGNO_INVALID) {
		pgno = cp->pgno;
		indx = cp->indx;
	} else {
		pgno = cp->dpgno;
		indx = cp->dindx;
	}

	/*
	 * If the item is referenced by another cursor, make sure that
	 * cursor's delete flag is set and leave it up to it to do the
	 * delete.
	 *
	 * !!!
	 * This test for > 0 is tricky.  This code is called when we close
	 * a cursor.  In this case, we've already removed the cursor from
	 * the active queue, so we won't see it in __bam_ca_delete.
	 */
	if (__bam_ca_delete(dbp, pgno, indx, 1) > 0)
		return (0);

	/*
	 * If this is concurrent DB, upgrade the lock if necessary.
	 */
	if (F_ISSET(dbc, DBC_WRITECURSOR) &&
	    (ret = lock_get(dbp->dbenv, dbc->locker,
	    DB_LOCK_UPGRADE, &dbc->lock_dbt, DB_LOCK_WRITE, &dbc->mylock)) != 0)
		return (ret);

	/*
	 * Lock and retrieve the current page.  We potentially have to acquire
	 * a new lock here if the cursor that did the original logical deletion
	 * and which already has a write lock is not the cursor that is doing
	 * the physical deletion and which may only have a read lock.
	 */
	if ((ret = __db_lget(dbc, 0, pgno, DB_LOCK_WRITE, 0, &lock)) != 0)
		return (ret);
	if ((ret = memp_fget(dbp->mpf, &pgno, 0, &h)) != 0)
		return (ret);
	local_page = 1;

	/*
	 * If we're deleting a duplicate entry and there are other duplicate
	 * entries remaining, call the common code to do the work and fix up
	 * the parent page as necessary.  Otherwise, do a normal btree delete.
	 *
	 * There are 5 possible cases:
	 *
	 * 1. It's not a duplicate item: do a normal btree delete.
	 * 2. It's a duplicate item:
	 *	2a: We delete an item from a page of duplicates, but there are
	 *	    more items on the page.
	 *      2b: We delete the last item from a page of duplicates, deleting
	 *	    the last duplicate.
	 *      2c: We delete the last item from a page of duplicates, but there
	 *	    is a previous page of duplicates.
	 *      2d: We delete the last item from a page of duplicates, but there
	 *	    is a following page of duplicates.
	 *
	 * In the case of:
	 *
	 *  1: There's nothing further to do.
	 * 2a: There's nothing further to do.
	 * 2b: Do the normal btree delete instead of a duplicate delete, as
	 *     that deletes both the duplicate chain and the parent page's
	 *     entry.
	 * 2c: There's nothing further to do.
	 * 2d: Delete the duplicate, and update the parent page's entry.
	 */
	if (TYPE(h) == P_DUPLICATE) {
		pgno = PGNO(h);
		prev_pgno = PREV_PGNO(h);
		next_pgno = NEXT_PGNO(h);

		if (NUM_ENT(h) == 1 &&
		    prev_pgno == PGNO_INVALID && next_pgno == PGNO_INVALID)
			cmd = DELETE_PAGE;
		else {
			cmd = DELETE_ITEM;

			/* Delete the duplicate. */
			if ((ret = __db_drem(dbc, &h, indx)) != 0)
				goto err;

			/*
			 * Update the cursors.
			 *
			 * !!!
			 * The page referenced by h may have been modified,
			 * don't use its page number.
			 */
			__bam_ca_di(dbp, pgno, indx, -1);

			/*
			 * 2a: h != NULL, h->pgno == pgno
			 * 2b: We don't reach this clause, as the above test
			 *     was true.
			 * 2c: h == NULL, prev_pgno != PGNO_INVALID
			 * 2d: h != NULL, next_pgno != PGNO_INVALID
			 *
			 * Test for 2a and 2c: if we didn't empty the current
			 * page or there was a previous page of duplicates, we
			 * don't need to touch the parent page.
			 */
			if ((h != NULL && pgno == h->pgno) ||
			    prev_pgno != PGNO_INVALID)
				cmd = NOTHING_FURTHER;
		}

		/*
		 * Release any page we're holding and its lock.
		 *
		 * !!!
		 * If there is no subsequent page in the duplicate chain, then
		 * __db_drem will have put page "h" and set it to NULL.
		*/
		if (local_page) {
			if (h != NULL)
				(void)memp_fput(dbp->mpf, h, 0);
			(void)__TLPUT(dbc, lock);
			local_page = 0;
		}

		if (cmd == NOTHING_FURTHER)
			goto done;

		/* Acquire the parent page and switch the index to its entry. */
		if ((ret =
		    __db_lget(dbc, 0, cp->pgno, DB_LOCK_WRITE, 0, &lock)) != 0)
			goto err;
		if ((ret = memp_fget(dbp->mpf, &cp->pgno, 0, &h)) != 0) {
			(void)__TLPUT(dbc, lock);
			goto err;
		}
		local_page = 1;
		indx = cp->indx;

		if (cmd == DELETE_PAGE)
			goto btd;

		/*
		 * Copy, delete, update, add-back the parent page's data entry.
		 *
		 * XXX
		 * This may be a performance/logging problem.  We should add a
		 * log message which simply logs/updates a random set of bytes
		 * on a page, and use it instead of doing a delete/add pair.
		 */
		indx += O_INDX;
		bo = *GET_BOVERFLOW(h, indx);
		if ((ret = __db_ditem(dbc, h, indx, BOVERFLOW_SIZE)) != 0)
			goto err;
		bo.pgno = next_pgno;
		memset(&dbt, 0, sizeof(dbt));
		dbt.data = &bo;
		dbt.size = BOVERFLOW_SIZE;
		if ((ret =
		    __db_pitem(dbc, h, indx, BOVERFLOW_SIZE, &dbt, NULL)) != 0)
			goto err;
		if ((ret = memp_fset(dbp->mpf, h, DB_MPOOL_DIRTY)) != 0)
			goto err;
		goto done;
	}

btd:	/*
	 * If the page is going to be emptied, delete it.  To delete a leaf
	 * page we need a copy of a key from the page.  We use the 0th page
	 * index since it's the last key that the page held.
	 *
	 * We malloc the page information instead of using the return key/data
	 * memory because we've already set them -- the reason we've already
	 * set them is because we're (potentially) about to do a reverse split,
	 * which would make our saved page information useless.
	 *
	 * !!!
	 * The following operations to delete a page might deadlock.  I think
	 * that's OK.  The problem is if we're deleting an item because we're
	 * closing cursors because we've already deadlocked and want to call
	 * txn_abort().  If we fail due to deadlock, we leave a locked empty
	 * page in the tree, which won't be empty long because we're going to
	 * undo the delete.
	 */
	root_pgno = ((BTREE *)dbp->bt_internal)->bt_root;
	if (!F_ISSET(dbp, DB_BT_REVSPLIT) &&
	    NUM_ENT(h) == 2 && h->pgno != root_pgno) {
		memset(&dbt, 0, sizeof(DBT));
		dbt.flags = DB_DBT_MALLOC | DB_DBT_INTERNAL;
		if ((ret = __db_ret(dbp, h, 0, &dbt, NULL, NULL)) != 0)
			goto err;
		delete_page = 1;
	}

	/*
	 * Do a normal btree delete.
	 *
	 * !!!
	 * Delete the key item first, otherwise the duplicate checks in
	 * __bam_ditem() won't work!
	 */
	if ((ret = __bam_ditem(dbc, h, indx)) != 0)
		goto err;
	if ((ret = __bam_ditem(dbc, h, indx)) != 0)
		goto err;

	/* Discard any remaining locks/pages. */
	if (local_page) {
		(void)memp_fput(dbp->mpf, h, 0);
		(void)__TLPUT(dbc, lock);
		local_page = 0;
	}

	/* Delete the page if it was emptied. */
	if (delete_page)
		ret = __bam_dpage(dbc, &dbt);

err:
done:	if (delete_page)
		__os_free(dbt.data, dbt.size);

	if (local_page) {
		/*
		 * It's possible for h to be NULL, as __db_drem may have
		 * been relinking pages by the time that it deadlocked.
		 */
		if (h != NULL)
			(void)memp_fput(dbp->mpf, h, 0);
		(void)__TLPUT(dbc, lock);
	}

	if (F_ISSET(dbc, DBC_WRITECURSOR))
		(void)__lock_downgrade(dbp->dbenv, &dbc->mylock,
		    DB_LOCK_IWRITE, 0);

	return (ret);
}

/*
 * __bam_c_getstack --
 *	Acquire a full stack for a cursor.
 */
static int
__bam_c_getstack(dbc)
	DBC *dbc;
{
	BTREE_CURSOR *cp;
	DB *dbp;
	DBT dbt;
	PAGE *h;
	db_pgno_t pgno;
	int exact, ret;

	dbp = dbc->dbp;
	cp = dbc->internal;
	memset(&dbt, 0, sizeof(DBT));
	h = NULL;
	ret = 0;

	/* Get the page with the current item on it. */
	pgno = cp->pgno;
	if ((ret = memp_fget(dbp->mpf, &pgno, 0, &h)) != 0)
		return (ret);

	/* Get a copy of a key from the page. */
	dbt.flags = DB_DBT_MALLOC | DB_DBT_INTERNAL;
	if ((ret = __db_ret(dbp, h, 0, &dbt, NULL, NULL)) != 0)
		goto err;

	/* Get a write-locked stack for that page. */
	exact = 0;
	ret = __bam_search(dbc, &dbt, S_KEYFIRST, 1, NULL, &exact);

	/* We no longer need the key or the page. */
err:	if (h != NULL)
		(void)memp_fput(dbp->mpf, h, 0);
	if (dbt.data != NULL)
		__os_free(dbt.data, dbt.size);
	return (ret);
}
