/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)bt_recno.c	11.9 (Sleepycat) 10/29/99";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <errno.h>
#include <limits.h>
#include <string.h>
#endif

#include "db_int.h"
#include "db_page.h"
#include "btree.h"
#include "db_ext.h"
#include "db_shash.h"
#include "lock.h"
#include "lock_ext.h"
#include "qam.h"

static int __ram_add __P((DBC *, db_recno_t *, DBT *, u_int32_t, u_int32_t));
static int __ram_delete __P((DB *, DB_TXN *, DBT *, u_int32_t));
static int __ram_fmap __P((DBC *, db_recno_t));
static int __ram_i_delete __P((DBC *));
static int __ram_put __P((DB *, DB_TXN *, DBT *, DBT *, u_int32_t));
static int __ram_source __P((DB *));
static int __ram_update __P((DBC *, db_recno_t, int));
static int __ram_vmap __P((DBC *, db_recno_t));

/*
 * In recno, there are two meanings to the on-page "deleted" flag.  If we're
 * re-numbering records, it means the record was implicitly created.  We skip
 * over implicitly created records if doing a cursor "next" or "prev", and
 * return DB_KEYEMPTY if they're explicitly requested..  If not re-numbering
 * records, it means that the record was implicitly created, or was deleted.
 * We skip over implicitly created or deleted records if doing a cursor "next"
 * or "prev", and return DB_KEYEMPTY if they're explicitly requested.
 *
 * If we're re-numbering records, then we have to detect in the cursor that
 * a record was deleted, and adjust the cursor as necessary on the next get.
 * If we're not re-numbering records, then we can detect that a record has
 * been deleted by looking at the actual on-page record, so we completely
 * ignore the cursor's delete flag.  This is different from the B+tree code.
 * It also maintains whether the cursor references a deleted record in the
 * cursor, and it doesn't always check the on-page value.
 */
#define	CD_SET(dbp, cp) {						\
	if (F_ISSET(dbp, DB_RE_RENUMBER))				\
		F_SET(cp, C_DELETED);					\
}
#define	CD_CLR(dbp, cp) {						\
	if (F_ISSET(dbp, DB_RE_RENUMBER))				\
		F_CLR(cp, C_DELETED);					\
}
#define	CD_ISSET(dbp, cp)						\
	(F_ISSET(dbp, DB_RE_RENUMBER) && F_ISSET(cp, C_DELETED))

/*
 * __ram_open --
 *	Recno open function.
 *
 * PUBLIC: int __ram_open __P((DB *, const char *, db_pgno_t));
 */
int
__ram_open(dbp, name, base_pgno)
	DB *dbp;
	const char *name;
	db_pgno_t base_pgno;
{
	BTREE *t;
	DBC *dbc;
	int ret, t_ret;

	t = dbp->bt_internal;

	/* Initialize the remaining fields/methods of the DB. */
	dbp->del = __ram_delete;
	dbp->put = __ram_put;
	dbp->stat = __bam_stat;

	/* Set the overflow page size. */
	__bam_setovflsize(dbp);

	/* Start up the tree. */
	if ((ret = __bam_read_root(dbp, name, base_pgno)) != 0)
		goto err;

	/*
	 * If the user specified a source tree, open it and map it in.
	 *
	 * !!!
	 * We don't complain if the user specified transactions or threads.
	 * It's possible to make it work, but you'd better know what you're
	 * doing!
	 */
	if (t->re_source == NULL)
		F_SET(t, RECNO_EOF);
	else
		if ((ret = __ram_source(dbp)) != 0)
			goto err;

	/* If we're snapshotting an underlying source file, do it now. */
	if (F_ISSET(dbp, DB_RE_SNAPSHOT)) {
		/* Allocate a cursor. */
		if ((ret = dbp->cursor(dbp, NULL, &dbc, 0)) != 0)
			goto err;

		/* Do the snapshot. */
		if ((ret = __ram_update(dbc,
		    DB_MAX_RECORDS, 0)) != 0 && ret == DB_NOTFOUND)
			ret = 0;

		/* Discard the cursor. */
		if ((t_ret = dbc->c_close(dbc)) != 0 && ret == 0)
			ret = t_ret;

		if (ret != 0)
			goto err;
	}

	return (0);

err:	/* If we mmap'd a source file, discard it. */
	if (t->re_smap != NULL)
		(void)__os_unmapfile(dbp->dbenv, t->re_smap, t->re_msize);

	/* If we opened a source file, discard it. */
	if (F_ISSET(&t->re_fh, DB_FH_VALID))
		(void)__os_closehandle(&t->re_fh);
	if (t->re_source != NULL)
		__os_freestr(t->re_source);

	return (ret);
}

/*
 * __ram_delete --
 *	Recno db->del function.
 */
static int
__ram_delete(dbp, txn, key, flags)
	DB *dbp;
	DB_TXN *txn;
	DBT *key;
	u_int32_t flags;
{
	BTREE_CURSOR *cp;
	DBC *dbc;
	db_recno_t recno;
	int ret, t_ret;

	PANIC_CHECK(dbp->dbenv);

	/* Check for invalid flags. */
	if ((ret = __db_delchk(dbp,
	    key, flags, F_ISSET(dbp, DB_AM_RDONLY))) != 0)
		return (ret);

	/* Acquire a cursor. */
	if ((ret = dbp->cursor(dbp, txn, &dbc, DB_WRITELOCK)) != 0)
		return (ret);

	DEBUG_LWRITE(dbc, txn, "ram_delete", key, NULL, flags);

	/* Check the user's record number and fill in as necessary. */
	if ((ret = __ram_getno(dbc, key, &recno, 0)) != 0)
		goto err;

	/* Do the delete. */
	cp = dbc->internal;
	cp->recno = recno;
	ret = __ram_i_delete(dbc);

	/* Release the cursor. */
err:	if ((t_ret = dbc->c_close(dbc)) != 0 && ret == 0)
		ret = t_ret;

	return (ret);
}

/*
 * __ram_i_delete --
 *	Internal version of recno delete, called by __ram_delete and
 *	__ram_c_del.
 */
static int
__ram_i_delete(dbc)
	DBC *dbc;
{
	BKEYDATA bk;
	BTREE *t;
	BTREE_CURSOR *cp;
	DB *dbp;
	DBT hdr, data;
	PAGE *h;
	db_indx_t indx;
	int exact, ret, stack;

	dbp = dbc->dbp;
	cp = dbc->internal;
	t = dbp->bt_internal;
	stack = 0;

	/*
	 * If this is CDB and this isn't a write cursor, then it's an error.
	 * If it is a write cursor, but we don't yet hold the write lock, then
	 * we need to upgrade to the write lock.
	 */
	if (F_ISSET(dbp->dbenv, DB_ENV_CDB)) {
		/* Make sure it's a valid update cursor. */
		if (!F_ISSET(dbc, DBC_WRITECURSOR | DBC_WRITER))
			return (EINVAL);

		if (F_ISSET(dbc, DBC_WRITECURSOR) &&
		    (ret = lock_get(dbp->dbenv, dbc->locker,
		    DB_LOCK_UPGRADE, &dbc->lock_dbt, DB_LOCK_WRITE,
		    &dbc->mylock)) != 0)
			return (ret);
	}

	/* Search the tree for the key; delete only deletes exact matches. */
	if ((ret = __bam_rsearch(dbc, &cp->recno, S_DELETE, 1, &exact)) != 0)
		goto err;
	if (!exact) {
		ret = DB_NOTFOUND;
		goto err;
	}
	stack = 1;

	h = cp->csp->page;
	indx = cp->csp->indx;

	/*
	 * If re-numbering records, the on-page deleted flag can only mean
	 * that this record was implicitly created.  Applications aren't
	 * permitted to delete records they never created, return an error.
	 *
	 * If not re-numbering records, the on-page deleted flag means that
	 * this record was implicitly created, or, was deleted at some time.
	 * The former is an error because applications aren't permitted to
	 * delete records they never created, the latter is an error because
	 * if the record was "deleted", we could never have found it.
	 */
	if (B_DISSET(GET_BKEYDATA(h, indx)->type)) {
		ret = DB_KEYEMPTY;
		goto err;
	}

	if (F_ISSET(dbp, DB_RE_RENUMBER)) {
		/* Delete the item, adjust the counts, adjust the cursors. */
		if ((ret = __bam_ditem(dbc, h, indx)) != 0)
			goto err;
		__bam_adjust(dbc, -1);
		__ram_ca(dbp, cp->recno, CA_DELETE);

		/*
		 * If the page is empty, delete it.   The whole tree is locked
		 * so there are no preparations to make.
		 */
		if (NUM_ENT(h) == 0 && h->pgno != t->bt_root) {
			stack = 0;
			ret = __bam_dpages(dbc);
		}
	} else {
		/* Use a delete/put pair to replace the record with a marker. */
		if ((ret = __bam_ditem(dbc, h, indx)) != 0)
			goto err;

		B_TSET(bk.type, B_KEYDATA, 1);
		bk.len = 0;
		memset(&hdr, 0, sizeof(hdr));
		hdr.data = &bk;
		hdr.size = SSZA(BKEYDATA, data);
		memset(&data, 0, sizeof(data));
		data.data = (void *)"";
		data.size = 0;
		if ((ret = __db_pitem(dbc,
		    h, indx, BKEYDATA_SIZE(0), &hdr, &data)) != 0)
			goto err;
	}
	F_SET(t, RECNO_MODIFIED);

err:	if (stack)
		__bam_stkrel(dbc, 0);

	/* If we upgraded the CDB lock upon entry; downgrade it now. */
	if (F_ISSET(dbc, DBC_WRITECURSOR))
		(void)__lock_downgrade(dbp->dbenv,
		    &dbc->mylock, DB_LOCK_IWRITE, 0);
	return (ret);
}

/*
 * __ram_put --
 *	Recno db->put function.
 */
static int
__ram_put(dbp, txn, key, data, flags)
	DB *dbp;
	DB_TXN *txn;
	DBT *key, *data;
	u_int32_t flags;
{
	DBC *dbc;
	db_recno_t recno;
	int ret, t_ret;

	PANIC_CHECK(dbp->dbenv);

	/* Check for invalid flags. */
	if ((ret = __db_putchk(dbp,
	    key, data, flags, F_ISSET(dbp, DB_AM_RDONLY), 0)) != 0)
		return (ret);

	/* Allocate a cursor. */
	if ((ret = dbp->cursor(dbp, txn, &dbc, DB_WRITELOCK)) != 0)
		return (ret);

	DEBUG_LWRITE(dbc, txn, "ram_put", key, data, flags);

	/*
	 * If we're appending to the tree, make sure we've read in all of
	 * the backing source file.  Otherwise, check the user's record
	 * number and fill in as necessary.
	 */
	if (flags == DB_APPEND) {
		if ((ret = __ram_update(
		    dbc, DB_MAX_RECORDS, 0)) != 0 && ret == DB_NOTFOUND)
			ret = 0;
	} else
		ret = __ram_getno(dbc, key, &recno, 1);

	/* Add the record. */
	if (ret == 0)
		ret = __ram_add(dbc, &recno, data, flags, 0);

	/* Discard the cursor. */
	if ((t_ret = dbc->c_close(dbc)) != 0 && ret == 0)
		ret = t_ret;

	/* Return the record number if we're appending to the tree. */
	if (ret == 0 && flags == DB_APPEND)
		*(db_recno_t *)key->data = recno;

	return (ret);
}

/*
 * __ram_c_del --
 *	Recno cursor->c_del function.
 *
 * PUBLIC: int __ram_c_del __P((DBC *, u_int32_t));
 */
int
__ram_c_del(dbc, flags)
	DBC *dbc;
	u_int32_t flags;
{
	BTREE_CURSOR *cp;
	DB *dbp;
	int ret;

	dbp = dbc->dbp;
	cp = dbc->internal;

	PANIC_CHECK(dbp->dbenv);

	/* Check for invalid flags. */
	if ((ret = __db_cdelchk(dbp, flags,
	    F_ISSET(dbp, DB_AM_RDONLY), cp->recno != RECNO_OOB)) != 0)
		return (ret);

	DEBUG_LWRITE(dbc, dbc->txn, "ram_c_del", NULL, NULL, flags);

	/*
	 * The semantics of cursors during delete are as follows: if record
	 * numbers are mutable (DB_RE_RENUMBER is set), deleting a record
	 * causes the cursor to automatically point to the record immediately
	 * following.  In this case it is possible to use a single cursor for
	 * repeated delete operations, without intervening operations.
	 *
	 * If record numbers are not mutable, then records are replaced with
	 * a marker containing a delete flag.  If the record referenced by
	 * this cursor has already been deleted, we will detect that as part
	 * of the delete operation, and fail.
	 */
	return (__ram_i_delete(dbc));
}

/*
 * __ram_c_get --
 *	Recno cursor->c_get function.
 *
 * PUBLIC: int __ram_c_get __P((DBC *, DBT *, DBT *, u_int32_t));
 */
int
__ram_c_get(dbc, key, data, flags)
	DBC *dbc;
	DBT *key, *data;
	u_int32_t flags;
{
	BTREE_CURSOR *cp, copy;
	DB *dbp;
	PAGE *h;
	db_indx_t indx;
	int exact, ret, stack, tmp_rmw;

	dbp = dbc->dbp;
	cp = dbc->internal;

	PANIC_CHECK(dbp->dbenv);

	/* Check for invalid flags. */
	if ((ret = __db_cgetchk(dbc->dbp,
	    key, data, flags, cp->recno != RECNO_OOB)) != 0)
		return (ret);

	/* Clear OR'd in additional bits so we can check for flag equality. */
	tmp_rmw = 0;
	if (LF_ISSET(DB_RMW)) {
		tmp_rmw = 1;
		F_SET(dbc, DBC_RMW);
		LF_CLR(DB_RMW);
	}

	DEBUG_LREAD(dbc, dbc->txn, "ram_c_get",
	    flags == DB_SET || flags == DB_SET_RANGE ? key : NULL, NULL, flags);

	/* Initialize the cursor for a new retrieval. */
	copy = *cp;

retry:	/* Update the record number. */
	stack = 0;
	switch (flags) {
	case DB_CURRENT:
		/*
		 * If record numbers are mutable: if we just deleted a record,
		 * there is no action necessary, we return the record following
		 * the deleted item by virtue of renumbering the tree.
		 */
		break;
	case DB_NEXT:
		/*
		 * If record numbers are mutable: if we just deleted a record,
		 * we have to avoid incrementing the record number so that we
		 * return the right record by virtue of renumbering the tree.
		 */
		if (CD_ISSET(dbp, cp))
			break;

		if (cp->recno != RECNO_OOB) {
			++cp->recno;
			break;
		}
		/* FALLTHROUGH */
	case DB_FIRST:
		flags = DB_NEXT;
		cp->recno = 1;
		break;
	case DB_PREV:
		if (cp->recno != RECNO_OOB) {
			if (cp->recno == 1) {
				ret = DB_NOTFOUND;
				goto err;
			}
			--cp->recno;
			break;
		}
		/* FALLTHROUGH */
	case DB_LAST:
		flags = DB_PREV;
		if (((ret = __ram_update(dbc,
		    DB_MAX_RECORDS, 0)) != 0) && ret != DB_NOTFOUND)
			goto err;
		if ((ret = __bam_nrecs(dbc, &cp->recno)) != 0)
			goto err;
		if (cp->recno == 0) {
			ret = DB_NOTFOUND;
			goto err;
		}
		break;
	case DB_SET:
	case DB_SET_RANGE:
		if ((ret = __ram_getno(dbc, key, &cp->recno, 0)) != 0)
			goto err;
		break;
	}

	/*
	 * For DB_PREV, DB_LAST, DB_SET and DB_SET_RANGE, we have already
	 * called __ram_update() to make sure sufficient records have been
	 * read from the backing source file.  Do it now for DB_CURRENT (if
	 * the current record was deleted we may need more records from the
	 * backing file for a DB_CURRENT operation), DB_FIRST and DB_NEXT.
	 */
	if (flags == DB_NEXT && ((ret =
	    __ram_update(dbc, cp->recno, 0)) != 0) && ret != DB_NOTFOUND)
		goto err;

	/* Search the tree for the record. */
	if ((ret = __bam_rsearch(dbc, &cp->recno,
	    F_ISSET(dbc, DBC_RMW) ? S_FIND_WR : S_FIND, 1, &exact)) != 0)
		goto err;
	stack = 1;
	if (!exact) {
		ret = DB_NOTFOUND;
		goto err;
	}
	h = cp->csp->page;
	indx = cp->csp->indx;

	/*
	 * If re-numbering records, the on-page deleted flag means this record
	 * was implicitly created.  If not re-numbering records, the on-page
	 * deleted flag means this record was implicitly created, or, it was
	 * deleted at some time.  Regardless, we skip such records if doing
	 * cursor next/prev operations, and fail if the application requested
	 * them explicitly.
	 */
	if (B_DISSET(GET_BKEYDATA(h, indx)->type)) {
		if (flags == DB_NEXT || flags == DB_PREV) {
			(void)__bam_stkrel(dbc, 0);
			goto retry;
		}
		ret = DB_KEYEMPTY;
		goto err;
	}

	/* Return the key if the user didn't give us one. */
	if (flags != DB_SET && flags != DB_SET_RANGE &&
	    (ret = __db_retcopy(dbp, key, &cp->recno, sizeof(cp->recno),
	    &dbc->rkey.data, &dbc->rkey.ulen)) != 0)
		goto err;

	/* Return the data item. */
	if ((ret = __db_ret(dbp,
	    h, indx, data, &dbc->rdata.data, &dbc->rdata.ulen)) != 0)
		goto err;

	/* The cursor was reset, no further delete adjustment is necessary. */
	CD_CLR(dbp, cp);

err:	if (stack)
		(void)__bam_stkrel(dbc, 0);

	/* Release temporary lock upgrade. */
	if (tmp_rmw)
		F_CLR(dbc, DBC_RMW);

	if (ret != 0)
		*cp = copy;

	return (ret);
}

/*
 * __ram_c_put --
 *	Recno cursor->c_put function.
 *
 * PUBLIC: int __ram_c_put __P((DBC *, DBT *, DBT *, u_int32_t));
 */
int
__ram_c_put(dbc, key, data, flags)
	DBC *dbc;
	DBT *key, *data;
	u_int32_t flags;
{
	BTREE_CURSOR *cp, copy;
	DB *dbp;
	int exact, ret;
	void *arg;

	dbp = dbc->dbp;
	cp = dbc->internal;

	PANIC_CHECK(dbp->dbenv);

	if ((ret = __db_cputchk(dbc->dbp, key, data, flags,
	    F_ISSET(dbc->dbp, DB_AM_RDONLY), cp->recno != RECNO_OOB)) != 0)
		return (ret);

	DEBUG_LWRITE(dbc, dbc->txn, "ram_c_put", NULL, data, flags);

	/*
	 * If we are running CDB, this had better be either a write
	 * cursor or an immediate writer.  If it's a regular writer,
	 * that means we have an IWRITE lock and we need to upgrade
	 * it to a write lock.
	 */
	if (F_ISSET(dbp->dbenv, DB_ENV_CDB)) {
		if (!F_ISSET(dbc, DBC_WRITECURSOR | DBC_WRITER))
			return (EINVAL);

		if (F_ISSET(dbc, DBC_WRITECURSOR) &&
		    (ret = lock_get(dbp->dbenv, dbc->locker, DB_LOCK_UPGRADE,
		    &dbc->lock_dbt, DB_LOCK_WRITE, &dbc->mylock)) != 0)
			return (ret);
	}

	/* Initialize the cursor for a new retrieval. */
	copy = *cp;

	/*
	 * To split, we need a valid key for the page.
	 *
	 * The split code discards all short-term locks and stack pages.
	 */
	if (0) {
split:		arg = &cp->recno;
		if ((ret = __bam_split(dbc, arg)) != 0)
			goto err;
	}

	if ((ret = __bam_rsearch(dbc, &cp->recno, S_INSERT, 1, &exact)) != 0)
		goto err;
	if (!exact) {
		ret = DB_NOTFOUND;
		goto err;
	}
	if ((ret = __bam_iitem(dbc, &cp->csp->page,
	    &cp->csp->indx, key, data, flags, 0)) == DB_NEEDSPLIT) {
		if ((ret = __bam_stkrel(dbc, 0)) != 0)
			goto err;
		goto split;
	}
	if ((ret = __bam_stkrel(dbc, 0)) != 0)
		goto err;

	switch (flags) {
	case DB_AFTER:
		/* Adjust the cursors. */
		__ram_ca(dbp, cp->recno, CA_IAFTER);

		/* Set this cursor to reference the new record. */
		cp->recno = copy.recno + 1;
		break;
	case DB_BEFORE:
		/* Adjust the cursors. */
		__ram_ca(dbp, cp->recno, CA_IBEFORE);

		/* Set this cursor to reference the new record. */
		cp->recno = copy.recno;
		break;
	}

	/* Return the key if we've created a new record. */
	if ((flags == DB_AFTER || flags == DB_BEFORE) &&
	    (ret = __db_retcopy(dbp, key, &cp->recno, sizeof(cp->recno),
	    &dbc->rkey.data, &dbc->rkey.ulen)) != 0)
		goto err;

	/* The cursor was reset, no further delete adjustment is necessary. */
	CD_CLR(dbp, cp);

err:	if (F_ISSET(dbc, DBC_WRITECURSOR))
		(void)__lock_downgrade(dbp->dbenv,
		    &dbc->mylock, DB_LOCK_IWRITE, 0);

	if (ret != 0)
		*cp = copy;

	return (ret);
}

/*
 * __ram_ca --
 *	Adjust cursors.
 *
 * PUBLIC: void __ram_ca __P((DB *, db_recno_t, ca_recno_arg));
 */
void
__ram_ca(dbp, recno, op)
	DB *dbp;
	db_recno_t recno;
	ca_recno_arg op;
{
	BTREE_CURSOR *cp;
	DBC *dbc;
	db_recno_t nrecs;

	/*
	 * Adjust the cursors.  See the comment in __bam_ca_delete().
	 */
	MUTEX_THREAD_LOCK(dbp->mutexp);
	for (dbc = TAILQ_FIRST(&dbp->active_queue);
	    dbc != NULL; dbc = TAILQ_NEXT(dbc, links)) {
		cp = dbc->internal;
		switch (op) {
		case CA_DELETE:
			if (recno < cp->recno)
				--cp->recno;
			if (recno == cp->recno) {
				if (__bam_nrecs(
				    dbc, &nrecs) == 0 && recno > nrecs)
					--cp->recno;
				else
					CD_SET(dbp, cp);
			}
			break;
		case CA_IAFTER:
			if (recno < cp->recno)
				++cp->recno;
			break;
		case CA_IBEFORE:
			if (recno <= cp->recno)
				++cp->recno;
			break;
		}
	}
	MUTEX_THREAD_UNLOCK(dbp->mutexp);
}

/*
 * __ram_getno --
 *	Check the user's record number, and make sure we've seen it.
 *
 * PUBLIC: int __ram_getno __P((DBC *, const DBT *, db_recno_t *, int));
 */
int
__ram_getno(dbc, key, rep, can_create)
	DBC *dbc;
	const DBT *key;
	db_recno_t *rep;
	int can_create;
{
	DB *dbp;
	db_recno_t recno;

	dbp = dbc->dbp;

	/* Check the user's record number. */
	if ((recno = *(db_recno_t *)key->data) == 0) {
		__db_err(dbp->dbenv, "illegal record number of 0");
		return (EINVAL);
	}
	if (rep != NULL)
		*rep = recno;

	/*
	 * Btree can neither create records nor read them in.  Recno can
	 * do both, see if we can find the record.
	 */
	return (dbp->type == DB_RECNO ?
	    __ram_update(dbc, recno, can_create) : 0);
}

/*
 * __ram_update --
 *	Ensure the tree has records up to and including the specified one.
 */
static int
__ram_update(dbc, recno, can_create)
	DBC *dbc;
	db_recno_t recno;
	int can_create;
{
	BTREE *t;
	DB *dbp;
	db_recno_t nrecs;
	int ret;

	dbp = dbc->dbp;
	t = dbp->bt_internal;

	/*
	 * If we can't create records and we've read the entire backing input
	 * file, we're done.
	 */
	if (!can_create && F_ISSET(t, RECNO_EOF))
		return (0);

	/*
	 * If we haven't seen this record yet, try to get it from the original
	 * file.
	 */
	if ((ret = __bam_nrecs(dbc, &nrecs)) != 0)
		return (ret);
	if (!F_ISSET(t, RECNO_EOF) && recno > nrecs) {
		if ((ret = t->re_irec(dbc, recno)) != 0)
			return (ret);
		if ((ret = __bam_nrecs(dbc, &nrecs)) != 0)
			return (ret);
	}

	/*
	 * If we can create records, create empty ones up to the requested
	 * record.
	 */
	if (!can_create || recno <= nrecs + 1)
		return (0);

	dbc->rdata.dlen = 0;
	dbc->rdata.doff = 0;
	dbc->rdata.flags = 0;
	if (F_ISSET(dbp, DB_RE_FIXEDLEN)) {
		if (dbc->rdata.ulen < t->re_len) {
			if ((ret = __os_realloc(t->re_len,
			    NULL, &dbc->rdata.data)) != 0) {
				dbc->rdata.ulen = 0;
				dbc->rdata.data = NULL;
				return (ret);
			}
			dbc->rdata.ulen = t->re_len;
		}
		dbc->rdata.size = t->re_len;
		memset(dbc->rdata.data, t->re_pad, t->re_len);
	} else
		dbc->rdata.size = 0;

	while (recno > ++nrecs)
		if ((ret = __ram_add(dbc,
		    &nrecs, &dbc->rdata, 0, BI_DELETED)) != 0)
			return (ret);
	return (0);
}

/*
 * __ram_source --
 *	Load information about the backing file.
 */
static int
__ram_source(dbp)
	DB *dbp;
{
	BTREE *t;
	size_t size;
	u_int32_t bytes, mbytes;
	int ret;

	t = dbp->bt_internal;

	/*
	 * !!!
	 * The caller has full responsibility for cleaning up on error --
	 * (it has to anyway, in case it fails after this routine succeeds).
	 */
	if ((ret = __db_appname(dbp->dbenv,
	    DB_APP_DATA, NULL, t->re_source, 0, NULL, &t->re_source)) != 0)
		return (ret);

	/*
	 * !!!
	 * It's possible that the backing source file is read-only.  We don't
	 * much care other than we'll complain if there are any modifications
	 * when it comes time to write the database back to the source.
	 */
	ret = __os_open(t->re_source,
	    F_ISSET(dbp, DB_AM_RDONLY) ? DB_OSO_RDONLY : 0, 0, &t->re_fh);
	if (ret != 0 && !F_ISSET(dbp, DB_AM_RDONLY))
		ret = __os_open(t->re_source, DB_OSO_RDONLY, 0, &t->re_fh);
	if (ret != 0) {
		__db_err(dbp->dbenv, "%s: %s", t->re_source, db_strerror(ret));
		return (ret);
	}

	/*
	 * XXX
	 * We'd like to test to see if the file is too big to mmap.  Since we
	 * don't know what size or type off_t's or size_t's are, or the largest
	 * unsigned integral type is, or what random insanity the local C
	 * compiler will perpetrate, doing the comparison in a portable way is
	 * flatly impossible.  Hope that mmap fails if the file is too large.
	 */
	if ((ret = __os_ioinfo(t->re_source,
	    &t->re_fh, &mbytes, &bytes, NULL)) != 0) {
		__db_err(dbp->dbenv, "%s: %s", t->re_source, db_strerror(ret));
		return (ret);
	}
	if (mbytes == 0 && bytes == 0) {
		F_SET(t, RECNO_EOF);
		return (0);
	}

	size = mbytes * MEGABYTE + bytes;
	if ((ret = __os_mapfile(dbp->dbenv, t->re_source,
	    &t->re_fh, (size_t)size, 1, &t->re_smap)) != 0)
		return (ret);
	t->re_cmap = t->re_smap;
	t->re_emap = (u_int8_t *)t->re_smap + (t->re_msize = size);
	t->re_irec = F_ISSET(dbp, DB_RE_FIXEDLEN) ?  __ram_fmap : __ram_vmap;
	return (0);
}

/*
 * __ram_writeback --
 *	Rewrite the backing file.
 *
 * PUBLIC: int __ram_writeback __P((DB *));
 */
int
__ram_writeback(dbp)
	DB *dbp;
{
	BTREE *t;
	DBC *dbc;
	DBT key, data;
	DB_FH fh;
	db_recno_t keyno;
	ssize_t nw;
	int ret, t_ret;
	u_int8_t delim, *pad;

	t = dbp->bt_internal;

	/* If the file wasn't modified, we're done. */
	if (!F_ISSET(t, RECNO_MODIFIED))
		return (0);

	/* If there's no backing source file, we're done. */
	if (t->re_source == NULL) {
		F_CLR(t, RECNO_MODIFIED);
		return (0);
	}

	/* Allocate a cursor. */
	if ((ret = dbp->cursor(dbp, NULL, &dbc, 0)) != 0)
		return (ret);

	/*
	 * Read any remaining records into the tree.
	 *
	 * !!!
	 * This is why we can't support transactions when applications specify
	 * backing (re_source) files.  At this point we have to read in the
	 * rest of the records from the file so that we can write all of the
	 * records back out again, which could modify a page for which we'd
	 * have to log changes and which we don't have locked.  This could be
	 * partially fixed by taking a snapshot of the entire file during the
	 * DB->open as DB->open is transaction protected.  But, if a checkpoint
	 * occurs then, the part of the log holding the copy of the file could
	 * be discarded, and that would make it impossible to recover in the
	 * face of disaster.  This could all probably be fixed, but it would
	 * require transaction protecting the backing source file, i.e. mpool
	 * would have to know about it, and we don't want to go there.
	 */
	if ((ret =
	    __ram_update(dbc, DB_MAX_RECORDS, 0)) != 0 && ret != DB_NOTFOUND)
		return (ret);

	/*
	 * !!!
	 * Close any underlying mmap region.  This is required for Windows NT
	 * (4.0, Service Pack 2) -- if the file is still mapped, the following
	 * open will fail.
	 */
	if (t->re_smap != NULL) {
		(void)__os_unmapfile(dbp->dbenv, t->re_smap, t->re_msize);
		t->re_smap = NULL;
	}

	/* Get rid of any backing file descriptor, just on GP's. */
	if (F_ISSET(&t->re_fh, DB_FH_VALID))
		(void)__os_closehandle(&t->re_fh);

	/* Open the file, truncating it. */
	if ((ret = __os_open(
	    t->re_source, DB_OSO_SEQ | DB_OSO_TRUNC, 0, &fh)) != 0) {
		__db_err(dbp->dbenv, "%s: %s", t->re_source, db_strerror(ret));
		goto err;
	}

	/*
	 * We step through the records, writing each one out.  Use the record
	 * number and the dbp->get() function, instead of a cursor, so we find
	 * and write out "deleted" or non-existent records.
	 */
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.size = sizeof(db_recno_t);
	key.data = &keyno;

	/*
	 * We'll need the delimiter if we're doing variable-length records,
	 * and the pad character if we're doing fixed-length records.
	 */
	delim = t->re_delim;
	if (F_ISSET(dbp, DB_RE_FIXEDLEN)) {
		if ((ret = __os_malloc(t->re_len, NULL, &pad)) != 0)
			goto err;
		memset(pad, t->re_pad, t->re_len);
	} else
		COMPQUIET(pad, NULL);
	for (keyno = 1;; ++keyno) {
		switch (ret = dbp->get(dbp, NULL, &key, &data, 0)) {
		case 0:
			if ((ret =
			    __os_write(&fh, data.data, data.size, &nw)) != 0)
				goto err;
			if (nw != (ssize_t)data.size) {
				ret = EIO;
				goto err;
			}
			break;
		case DB_KEYEMPTY:
			if (F_ISSET(dbp, DB_RE_FIXEDLEN)) {
				if ((ret = __os_write(
				    &fh, pad, t->re_len, &nw)) != 0)
					goto err;
				if (nw != (ssize_t)t->re_len) {
					ret = EIO;
					goto err;
				}
			}
			break;
		case DB_NOTFOUND:
			ret = 0;
			goto done;
		}
		if (!F_ISSET(dbp, DB_RE_FIXEDLEN)) {
			if ((ret = __os_write(&fh, &delim, 1, &nw)) != 0)
				goto err;
			if (nw != 1) {
				ret = EIO;
				goto err;
			}
		}
	}

err:
done:	/* Close the file descriptor. */
	if (F_ISSET(&fh, DB_FH_VALID) &&
	    (t_ret = __os_closehandle(&fh)) != 0 && ret == 0)
		ret = t_ret;

	/* Discard the cursor. */
	if ((t_ret = dbc->c_close(dbc)) != 0 && ret == 0)
		ret = t_ret;

	if (ret == 0)
		F_CLR(t, RECNO_MODIFIED);

	return (ret);
}

/*
 * __ram_fmap --
 *	Get fixed length records from a file.
 */
static int
__ram_fmap(dbc, top)
	DBC *dbc;
	db_recno_t top;
{
	BTREE *t;
	DB *dbp;
	DBT data;
	db_recno_t recno;
	u_int32_t len;
	u_int8_t *sp, *ep, *p;
	int is_modified, ret;

	dbp = dbc->dbp;
	t = dbp->bt_internal;

	if ((ret = __bam_nrecs(dbc, &recno)) != 0)
		return (ret);

	if (dbc->rdata.ulen < t->re_len) {
		if ((ret =
		    __os_realloc(t->re_len, NULL, &dbc->rdata.data)) != 0) {
			dbc->rdata.ulen = 0;
			dbc->rdata.data = NULL;
			return (ret);
		}
		dbc->rdata.ulen = t->re_len;
	}

	is_modified = F_ISSET(t, RECNO_MODIFIED);

	memset(&data, 0, sizeof(data));
	data.data = dbc->rdata.data;
	data.size = t->re_len;

	sp = (u_int8_t *)t->re_cmap;
	ep = (u_int8_t *)t->re_emap;
	while (recno < top) {
		if (sp >= ep) {
			F_SET(t, RECNO_EOF);
			ret = DB_NOTFOUND;
			goto err;
		}
		len = t->re_len;
		for (p = dbc->rdata.data;
		    sp < ep && len > 0; *p++ = *sp++, --len)
			;

		/*
		 * Another process may have read this record from the input
		 * file and stored it into the database already, in which
		 * case we don't need to repeat that operation.  We detect
		 * this by checking if the last record we've read is greater
		 * or equal to the number of records in the database.
		 *
		 * XXX
		 * We should just do a seek, since the records are fixed
		 * length.
		 */
		if (t->re_last >= recno) {
			if (len != 0)
				memset(p, t->re_pad, len);

			++recno;
			if ((ret = __ram_add(dbc, &recno, &data, 0, 0)) != 0)
				goto err;
		}
		++t->re_last;
	}
	t->re_cmap = sp;

err:	if (!is_modified)
		F_CLR(t, RECNO_MODIFIED);

	return (0);
}

/*
 * __ram_vmap --
 *	Get variable length records from a file.
 */
static int
__ram_vmap(dbc, top)
	DBC *dbc;
	db_recno_t top;
{
	BTREE *t;
	DBT data;
	db_recno_t recno;
	u_int8_t *sp, *ep;
	int delim, is_modified, ret;

	t = dbc->dbp->bt_internal;

	if ((ret = __bam_nrecs(dbc, &recno)) != 0)
		return (ret);

	delim = t->re_delim;
	is_modified = F_ISSET(t, RECNO_MODIFIED);

	memset(&data, 0, sizeof(data));

	sp = (u_int8_t *)t->re_cmap;
	ep = (u_int8_t *)t->re_emap;
	while (recno < top) {
		if (sp >= ep) {
			F_SET(t, RECNO_EOF);
			ret = DB_NOTFOUND;
			goto err;
		}
		for (data.data = sp; sp < ep && *sp != delim; ++sp)
			;

		/*
		 * Another process may have read this record from the input
		 * file and stored it into the database already, in which
		 * case we don't need to repeat that operation.  We detect
		 * this by checking if the last record we've read is greater
		 * or equal to the number of records in the database.
		 */
		if (t->re_last >= recno) {
			data.size = sp - (u_int8_t *)data.data;
			++recno;
			if ((ret = __ram_add(dbc, &recno, &data, 0, 0)) != 0)
				goto err;
		}
		++t->re_last;
		++sp;
	}
	t->re_cmap = sp;

err:	if (!is_modified)
		F_CLR(t, RECNO_MODIFIED);

	return (ret);
}

/*
 * __ram_add --
 *	Add records into the tree.
 */
static int
__ram_add(dbc, recnop, data, flags, bi_flags)
	DBC *dbc;
	db_recno_t *recnop;
	DBT *data;
	u_int32_t flags, bi_flags;
{
	BKEYDATA *bk;
	BTREE_CURSOR *cp;
	PAGE *h;
	db_indx_t indx;
	int exact, ret, stack;

	cp = dbc->internal;

retry:	/* Find the slot for insertion. */
	if ((ret = __bam_rsearch(dbc, recnop,
	    S_INSERT | (flags == DB_APPEND ? S_APPEND : 0), 1, &exact)) != 0)
		return (ret);
	h = cp->csp->page;
	indx = cp->csp->indx;
	stack = 1;

	/*
	 * If re-numbering records, the on-page deleted flag means this record
	 * was implicitly created.  If not re-numbering records, the on-page
	 * deleted flag means this record was implicitly created, or, it was
	 * deleted at some time.
	 *
	 * If DB_NOOVERWRITE is set and the item already exists in the tree,
	 * return an error unless the item was either marked for deletion or
	 * only implicitly created.
	 */
	if (exact) {
		bk = GET_BKEYDATA(h, indx);
		if (!B_DISSET(bk->type) && flags == DB_NOOVERWRITE) {
			ret = DB_KEYEXIST;
			goto err;
		}
	}

	/*
	 * Select the arguments for __bam_iitem() and do the insert.  If the
	 * key is an exact match, or we're replacing the data item with a
	 * new data item, replace the current item.  If the key isn't an exact
	 * match, we're inserting a new key/data pair, before the search
	 * location.
	 */
	switch (ret = __bam_iitem(dbc,
	    &h, &indx, NULL, data, exact ? DB_CURRENT : DB_BEFORE, bi_flags)) {
	case 0:
		/*
		 * Don't adjust anything.
		 *
		 * If we inserted a record, no cursors need adjusting because
		 * the only new record it's possible to insert is at the very
		 * end of the tree.  The necessary adjustments to the internal
		 * page counts were made by __bam_iitem().
		 *
		 * If we overwrote a record, no cursors need adjusting because
		 * future DBcursor->get calls will simply return the underlying
		 * record (there's no adjustment made for the DB_CURRENT flag
		 * when a cursor get operation immediately follows a cursor
		 * delete operation, and the normal adjustment for the DB_NEXT
		 * flag is still correct).
		 */
		break;
	case DB_NEEDSPLIT:
		/* Discard the stack of pages and split the page. */
		(void)__bam_stkrel(dbc, 0);
		stack = 0;

		if ((ret = __bam_split(dbc, recnop)) != 0)
			goto err;

		goto retry;
		/* NOTREACHED */
	default:
		goto err;
	}


err:	if (stack)
		__bam_stkrel(dbc, 0);

	return (ret);
}
