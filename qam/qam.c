/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1999
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)qam.c	11.23 (Sleepycat) 10/26/99";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <errno.h>
#include <string.h>
#endif

#include "db_int.h"
#include "db_page.h"
#include "db_shash.h"
#include "db_am.h"
#include "lock.h"
#include "qam.h"
#include "mp.h"

static int __qam_c_close __P((DBC *));
static int __qam_c_del __P((DBC *, u_int32_t));
static int __qam_c_get __P((DBC *, DBT *, DBT *, u_int32_t));
static int __qam_c_put __P((DBC *, DBT *, DBT *, u_int32_t));
static int __qam_getno __P((DB *, const DBT *, db_recno_t *));
static int __qam_i_delete __P((DBC *));
static int __qam_i_put __P((DBC *, DBT *, u_int32_t));
static int __qam_nrecs __P((DBC *, db_recno_t *, db_recno_t *));
static int __qam_position
	       __P((DBC *, db_recno_t *, db_lockmode_t, db_recno_t, int *));
static int __qam_c_destroy __P((DBC *));

/*
 * __qam_nrecs --
 *	Return the record number for the head and tail of the queue.
 */
static int
__qam_nrecs(dbc, rep, start)
	DBC *dbc;
	db_recno_t *rep, *start;
{
	DB *dbp;
	DB_LOCK lock;
	QMETA *meta;
	db_pgno_t pg;
	int ret;

	dbp = dbc->dbp;

	pg = ((QUEUE *)dbp->q_internal)->q_meta;
	if ((ret = __db_lget(dbc, 0, pg,  DB_LOCK_READ, 0, &lock)) != 0)
		return (ret);
	if ((ret = memp_fget(dbp->mpf, &pg, 0, &meta)) != 0) {
		/* We did not fetch it, we can release the lock. */
		(void)__LPUT(dbc, lock);
		return (ret);
	}

	*rep = meta->cur_recno;
	*start = meta->start;

	if ((ret = memp_fput(dbp->mpf, meta, 0)) != 0)
		return (ret);
	/* Don't hold the meta page long term. */
	if ((ret = __LPUT(dbc, lock)) != 0)
		return (ret);

	return (0);
}

/*
 * __qam_position --
 *	Position a queued access method cursor at a record.  This returns
 *	the page locked.  *exactp will be set if the record is valid.
 */
static int
__qam_position(dbc, recnop, lock_mode, start, exactp)
	DBC *dbc;		/* open cursor */
	db_recno_t *recnop;	/* pointer to recno to find */
	db_lockmode_t lock_mode;/* locking: read or write */
	db_recno_t  start;	/* meta.start */
	int *exactp;		/* indicate if it was found */
{
	QUEUE_CURSOR *cp;
	DB *dbp;
	QAMDATA  *qp;
	db_pgno_t pg;
	int ret;

	dbp = dbc->dbp;
	cp = (QUEUE_CURSOR *) dbc->internal;

	/* Fetch the page for this recno. */
	pg = QAM_RECNO_PAGE(dbp, start, *recnop);

	if ((ret = __db_lget(dbc, 0, pg,  lock_mode, 0, &cp->lock)) != 0)
		return (ret);
	if ((ret = memp_fget(dbp->mpf, &pg,
	    lock_mode == DB_LOCK_WRITE ? DB_MPOOL_CREATE : 0,
	    &cp->page)) != 0) {
		/* We did not fetch it, we can release the lock. */
		(void)__LPUT(dbc, cp->lock);
		cp->lock.off = LOCK_INVALID;
		return (ret);
	}
	cp->pgno = pg;
	cp->indx = QAM_RECNO_INDEX(dbp, pg, start, *recnop);

	if (cp->page->pgno == 0) {
		if (F_ISSET(dbp, DB_AM_RDONLY)) {
			*exactp = 0;
			return (0);
		}
		cp->page->pgno = pg;
		cp->page->type = P_QAMDATA;
	}

	qp = QAM_GET_RECORD(dbp, cp->page, cp->indx);
	*exactp = F_ISSET(qp, QAM_VALID);

	return (ret);
}

/*
 * __qam_pitem --
 *	Put an item on a queue page.  Copy the data to the page and set the
 *	VALID and SET bits.  If logging and the record was previously set,
 *	log that data, otherwise just log the new data.
 *
 *   pagep must be write locked
 *
 * PUBLIC: int __qam_pitem
 * PUBLIC:     __P((DBC *,  QPAGE *, u_int32_t, db_recno_t, DBT *));
 */
int
__qam_pitem(dbc, pagep, indx, recno, data)
	DBC *dbc;
	QPAGE *pagep;
	u_int32_t indx;
	db_recno_t recno;
	DBT *data;
{
	DB *dbp;
	DBT olddata, pdata, *datap;
	QAMDATA *qp;
	QUEUE *t;
	u_int32_t size;
	u_int8_t *dest, *p;
	int alloced, ret;

	alloced = ret = 0;

	dbp = dbc->dbp;
	t = (QUEUE *)dbp->q_internal;

	if (data->size > t->re_len)
		return (EINVAL);

	qp = QAM_GET_RECORD(dbp, pagep, indx);

	p = qp->data;
	size = data->size;
	datap = data;
	if (F_ISSET(data, DB_DBT_PARTIAL)) {
		if (data->size != data->dlen)
			return (EINVAL);
		if (data->doff + data->dlen > t->re_len)
			return (EINVAL);
		if (data->size == t->re_len)
			goto no_partial;

		/*
		 * If we are logging, then we have to build the record
		 * first, otherwise, we can simply drop the change
		 * directly on the page.  After this clause, make
		 * sure that datap and p are set up correctly so that
		 * copying datap into p does the right thing.
		 *
		 * Note, I am changing this so that if the existing
		 * record is not valid, we create a complete record
		 * to log so that both this and the recovery code is simpler.
		 */

		if (DB_LOGGING(dbc) || !F_ISSET(qp, QAM_VALID)) {
			datap = &pdata;
			memset(datap, 0, sizeof(*datap));

			if ((ret =
			    __os_malloc(t->re_len, NULL, &datap->data)) != 0)
				return (ret);
			alloced = 1;
			datap->size = t->re_len;

			/*
			 * Construct the record if it's valid, otherwise set it
			 * all to the pad character.
			 */
			dest = datap->data;
			if (F_ISSET(qp, QAM_VALID))
				memcpy(dest, p, t->re_len);
			else
				memset(dest, t->re_pad, t->re_len);

			dest += data->doff;
			memcpy(dest, data->data, data->size);
		} else {
			datap = data;
			p += data->doff;
		}
	}

no_partial:
	if (DB_LOGGING(dbc)) {
		olddata.size = 0;
		if (F_ISSET(qp, QAM_SET)) {
			olddata.data = qp->data;
			olddata.size = t->re_len;
		}
		if ((ret = __qam_add_log(dbp->dbenv, dbc->txn, &LSN(pagep),
		    0, dbp->log_fileid, &LSN(pagep), pagep->pgno,
		    indx, recno, datap, qp->flags,
		    olddata.size == 0 ? NULL : &olddata)) != 0)
			goto err;
	}

	F_SET(qp, QAM_VALID | QAM_SET);
	memcpy(p, datap->data, datap->size);
	if (!F_ISSET(data, DB_DBT_PARTIAL))
		memset(p + datap->size,  t->re_pad, t->re_len - datap->size);

err:	if (alloced)
		__os_free(datap->data, t->re_len);

	return (ret);
}
/*
 * __qam_c_put
 * 	Cursor put for queued access method.
 *	BEFORE and AFTER cannot be specified.
 */
static int
__qam_c_put(dbc, key, data, flags)
	DBC *dbc;
	DBT *key, *data;
	u_int32_t flags;
{
	QUEUE_CURSOR *cp;
	DB_LOCK lock;
	int ret;

	/* Check for invalid flags. */
	if ((ret = __db_cputchk(dbc->dbp, key, data, flags,
	    F_ISSET(dbc->dbp, DB_AM_RDONLY),
	    ((QUEUE_CURSOR *)dbc->internal)->recno != RECNO_OOB)) != 0)
		return (ret);

	DEBUG_LWRITE(dbc, dbc->txn, "qam_c_put", NULL, data, flags);

	cp = (QUEUE_CURSOR *)dbc->internal;
	lock = cp->lock;

	ret = __qam_i_put(dbc, data, flags);

	/* Release any previous lock, if not in a transaction. */
	if (ret == 0 && lock.off != LOCK_INVALID) {
		if ((ret = __TLPUT(dbc, lock)) != 0)
			return (ret);
		cp->lock.off = LOCK_INVALID;
	}
	return (0);
}

/*
 * __qam_i_put
 * 	Internal cursor put for queued access method.
 */
static int
__qam_i_put(dbc, data, flags)
	DBC *dbc;
	DBT *data;
	u_int32_t flags;
{
	QUEUE_CURSOR *cp;
	DB *dbp;
	DB_LOCK lock;
	QMETA *meta;
	db_pgno_t pg;
	db_recno_t new_cur, new_first;
	u_int32_t opcode;
	int exact, ret, t_ret;

	dbp = dbc->dbp;
	PANIC_CHECK(dbp->dbenv);
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

	cp = (QUEUE_CURSOR *)dbc->internal;

	/* Write lock the record. */
	if ((ret = __db_lget(dbc,
	    0, cp->recno, DB_LOCK_WRITE, DB_LOCK_RECORD, &lock)) != 0)
		return (ret);

	if ((ret = __qam_position(dbc,
	    &cp->recno, DB_LOCK_WRITE, cp->start, &exact)) != 0) {
		/* We could not get the page, we can release the record lock. */
		__LPUT(dbc, lock);
		return (ret);
	}

	if (exact && flags == DB_NOOVERWRITE) {
		ret = __TLPUT(dbc, lock);
		/* Doing record locking, release the page lock */
		if ((t_ret = __LPUT(dbc, cp->lock)) == 0)
			cp->lock.off = LOCK_INVALID;
		else
			if (ret == 0)
				ret = t_ret;
		if ((t_ret = memp_fput(dbp->mpf, cp->page, 0)) != 0 && ret == 0)
			ret = t_ret;
		return (ret == 0 ? DB_KEYEXIST : ret);
	}

	/* Put the item on the page. */
	ret = __qam_pitem(dbc, (QPAGE *)cp->page, cp->indx, cp->recno, data);

	/* Doing record locking, release the page lock */
	if ((t_ret = __LPUT(dbc, cp->lock)) != 0 && ret == 0)
		ret = t_ret;
	if ((t_ret = memp_fput(dbp->mpf, cp->page, DB_MPOOL_DIRTY)) && ret == 0)
		ret = t_ret;
	cp->lock = lock;
	cp->lock_mode = DB_LOCK_WRITE;
	if (ret != 0)
		return (ret);

	/* We may need to reset the head or tail of the queue. */
	pg = ((QUEUE *)dbp->q_internal)->q_meta;
	if ((ret = __db_lget(dbc, 0, pg,  DB_LOCK_WRITE, 0, &lock)) != 0)
		return (ret);
	if ((ret = memp_fget(dbp->mpf, &pg, 0, &meta)) != 0) {
		/* We did not fetch it, we can release the lock. */
		(void)__LPUT(dbc, lock);
		return (ret);
	}

	opcode = 0;
	new_cur = new_first = 0;

	if (cp->recno > meta->cur_recno) {
		new_cur = cp->recno;
		opcode |= QAM_SETCUR;
	}
	if (cp->recno < meta->first_recno || meta->first_recno < meta->start) {
		new_first = cp->recno;
		opcode |= QAM_SETFIRST;
	}

	if (opcode != 0 && DB_LOGGING(dbc)) {
		ret = __qam_mvptr_log(dbp->dbenv, dbc->txn, &meta->dbmeta.lsn,
		    0, opcode, dbp->log_fileid, meta->first_recno, new_first,
		    meta->cur_recno, new_cur, &meta->dbmeta.lsn);
	}

	if (opcode & QAM_SETCUR)
		meta->cur_recno = cp->recno;
	if (opcode & QAM_SETFIRST)
		meta->first_recno = cp->recno;

	if ((t_ret =
	    memp_fput(dbp->mpf, meta, opcode != 0 ? DB_MPOOL_DIRTY : 0)) != 0 &&
	    ret == 0)
		ret = t_ret;

	/* Don't hold the meta page long term. */
	if ((t_ret = __LPUT(dbc, lock)) != 0 && ret == 0)
		ret = t_ret;
	return (ret);
}

/*
 * __qam_put --
 * 	Add a record to the queue.
 *	If we are doing anything but appending, just call qam_c_put to do the
 *	work.  Otherwise we fast path things here.
 *
 * PUBLIC: int __qam_put __P((DB *, DB_TXN *, DBT *, DBT *, u_int32_t));
 */
int
__qam_put(dbp, txn, key, data, flags)
	DB *dbp;
	DB_TXN *txn;
	DBT *key, *data;
	u_int32_t flags;
{
	QUEUE_CURSOR *cp;
	DBC *dbc;
	DB_LOCK lock;
	QMETA *meta;
	QPAGE *page;
	db_pgno_t pg;
	db_recno_t recno, start, total;
	int ret, t_ret;

	PANIC_CHECK(dbp->dbenv);

	/* Allocate a cursor. */
	if ((ret = dbp->cursor(dbp, txn, &dbc, DB_WRITELOCK)) != 0)
		return (ret);

	DEBUG_LWRITE(dbc, txn, "qam_put", key, data, flags);

	cp = dbc->internal;

	/* Check for invalid flags. */
	if ((ret = __db_putchk(dbp,
	    key, data, flags, F_ISSET(dbp, DB_AM_RDONLY), 0)) != 0)
		goto done;

	/* If not appending, then just call the cursor routine */
	if (flags != DB_APPEND) {
		if ((ret = __qam_getno(dbp, key, &cp->recno)) != 0)
			goto done;
		__qam_nrecs(dbc, &total, &cp->start);

		ret = __qam_i_put(dbc, data, flags);
		goto done;
	}

	/* Write lock the meta page. */
	pg = ((QUEUE *)dbp->q_internal)->q_meta;
	if ((ret = __db_lget(dbc, 0, pg,  DB_LOCK_WRITE, 0, &lock)) != 0)
		goto done;
	if ((ret = memp_fget(dbp->mpf, &pg, 0, &meta)) != 0) {
		/* We did not fetch it, we can release the lock. */
		(void)__LPUT(dbc, lock);
		goto done;
	}

	/* Record that we are going to allocate a record. */
	if (DB_LOGGING(dbc)) {
		__qam_inc_log(dbp->dbenv,
		    txn, &meta->dbmeta.lsn,
		    0, dbp->log_fileid, &meta->dbmeta.lsn);
	}

	/* Get the next record number. */
	recno = ++meta->cur_recno;
	start = meta->start;

	if (meta->first_recno < meta->start || meta->first_recno > recno)
		meta->first_recno = recno;

	/* Release the meta page. */
	if ((ret = memp_fput(dbp->mpf, meta, DB_MPOOL_DIRTY)) != 0)
		return (ret);

	/* Lock the record and release meta page lock. */
	if ((ret = __db_lget(dbc,
	    1, recno, DB_LOCK_WRITE, DB_LOCK_RECORD, &lock)) != 0)
		goto done;

	cp->lock = lock;
	cp->lock_mode = DB_LOCK_WRITE;

	pg = QAM_RECNO_PAGE(dbp, start, recno);

	/* Fetch and write lock the data page. */
	if ((ret = __db_lget(dbc, 0, pg,  DB_LOCK_WRITE, 0, &lock)) != 0)
		goto done;
	if ((ret = memp_fget(dbp->mpf, &pg, DB_MPOOL_CREATE, &page)) != 0) {
		/* We did not fetch it, we can release the lock. */
		(void)__LPUT(dbc, lock);
		goto done;
	}

	/* See if this is a new page. */
	if (page->pgno == 0) {
		page->pgno = pg;
		page->type = P_QAMDATA;
	}

	/* Put the item on the page and log it. */
	if ((ret = __qam_pitem(dbc, page,
	    QAM_RECNO_INDEX(dbp, pg, start, recno), recno, data)) != 0) {
		/* The put failed.  We did not change the row. */
		(void)__LPUT(dbc, cp->lock);
		(void)memp_fput(dbp->mpf, cp->page, DB_MPOOL_DIRTY);
 		return (ret);
	}

	/* Doing record locking, release the page lock */
	if ((ret = __LPUT(dbc, lock)) != 0)
		return (ret);
	if ((ret = memp_fput(dbp->mpf, page, DB_MPOOL_DIRTY)) != 0)
		return (ret);

	/* Return the record number to the user. */
	*(db_recno_t *)key->data = recno;

done:
	/* Discard the cursor. */
	if ((t_ret = dbc->c_close(dbc)) != 0 && ret == 0)
		ret = t_ret;

	return (ret);
}

/*
 * __qam_i_delete --
 *	Internal version of recno delete, called by __qam_delete and
 *	__qam_c_del.
 */
static int
__qam_i_delete(dbc)
	DBC *dbc;
{
	QUEUE_CURSOR *cp;
	DB *dbp;
	DBT data;
	DB_LOCK lock;
	PAGE *pagep;
	QAMDATA *qp;
	db_recno_t start;
	int exact, ret, t_ret;

	dbp = dbc->dbp;
	cp = dbc->internal;
	start = cp->start;
	ret = 0;

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
			return (EAGAIN);
	}

	if ((ret = __db_lget(dbc,
	    0, cp->recno, DB_LOCK_WRITE, DB_LOCK_RECORD, &lock)) != 0)
		goto err;

	cp->lock_mode = DB_LOCK_WRITE;
	/* Find the record ; delete only deletes exact matches. */
	if ((ret = __qam_position(dbc,
	    &cp->recno, DB_LOCK_WRITE, start, &exact)) != 0) {
		cp->lock = lock;
		goto err;
	}
	if (!exact) {
		ret = DB_NOTFOUND;
		goto err1;
	}

	pagep = cp->page;
	qp = QAM_GET_RECORD(dbp, pagep, cp->indx);

	if (DB_LOGGING(dbc)) {
		data.size = ((QUEUE *)dbp->q_internal)->re_len;
		data.data = qp->data;
		if ((ret = __qam_del_log(dbp->dbenv, dbc->txn,
		    &LSN(pagep), 0, dbp->log_fileid, &LSN(pagep),
		    pagep->pgno, cp->indx, cp->recno)) != 0)
			goto err1;
	}

	F_CLR(qp, QAM_VALID);

err1:
	if ((t_ret = memp_fput(
	    dbp->mpf, cp->page, ret == 0 ? DB_MPOOL_DIRTY : 0)) != 0)
		return (ret ? ret : t_ret);
	/* Doing record locking, release the page lock */
	if ((t_ret = __LPUT(dbc, cp->lock)) != 0) {
		cp->lock = lock;
		return (ret ? ret : t_ret);
	}
	cp->lock = lock;
	return (ret);

err:	if (F_ISSET(dbp->dbenv, DB_ENV_CDB) && F_ISSET(dbc, DBC_WRITECURSOR))
		(void)__lock_downgrade(
		    dbp->dbenv, &dbc->mylock, DB_LOCK_IWRITE, 0);
	return (ret);
}

/*
 * __qam_delete --
 *	Queue db->del function.
 *
 * PUBLIC: int __qam_delete __P((DB *, DB_TXN *, DBT *, u_int32_t));
 */
int
__qam_delete(dbp, txn, key, flags)
	DB *dbp;
	DB_TXN *txn;
	DBT *key;
	u_int32_t flags;
{
	QUEUE_CURSOR *cp;
	DBC *dbc;
	db_recno_t total;
	int ret, t_ret;

	PANIC_CHECK(dbp->dbenv);

	/* Check for invalid flags. */
	if ((ret =
	    __db_delchk(dbp, key, flags, F_ISSET(dbp, DB_AM_RDONLY))) != 0)
		return (ret);

	/* Acquire a cursor. */
	if ((ret = dbp->cursor(dbp, txn, &dbc, DB_WRITELOCK)) != 0)
		return (ret);

	DEBUG_LWRITE(dbc, txn, "qam_delete", key, NULL, flags);

	cp = dbc->internal;
	if ((ret = __qam_getno(dbp, key, &cp->recno)) != 0)
		goto err;
	__qam_nrecs(dbc, &total, &cp->start);

	/* Do the delete. */
	if (cp->recno > total) {
		ret = DB_NOTFOUND;
		goto err;
	}

	ret = __qam_i_delete(dbc);

	/* Release the cursor. */
err:	if ((t_ret = dbc->c_close(dbc)) != 0 && ret == 0)
		ret = t_ret;

	return (ret);
}

/*
 * __qam_c_del --
 *	Qam cursor->c_del function
 */
static int
__qam_c_del(dbc, flags)
	DBC *dbc;
	u_int32_t flags;
{
	QUEUE_CURSOR *cp;
	DB *dbp;
	db_recno_t total;
	int ret;

	dbp = dbc->dbp;
	cp = dbc->internal;

	PANIC_CHECK(dbp->dbenv);

	/* Check for invalid flags. */
	if ((ret = __db_cdelchk(dbp, flags,
	    F_ISSET(dbp, DB_AM_RDONLY), cp->recno != RECNO_OOB)) != 0)
		return (ret);

	DEBUG_LWRITE(dbc, dbc->txn, "qam_c_del", NULL, NULL, flags);

	if ((ret = __qam_nrecs(dbc, &total, &cp->start)) != 0)
		return (ret);
	return (__qam_i_delete(dbc));
}


/*
 * __qam_c_get --
 *	Queue cursor->c_get function.
 */
static int
__qam_c_get(dbc_orig, key, data, flags)
	DBC *dbc_orig;
	DBT *key, *data;
	u_int32_t flags;
{
	QUEUE_CURSOR *cp, *orig;
	DB *dbp;
	DB_LOCK lock, pglock, metalock, save_lock;
	DBC *dbc;
	PAGE *pg;
	QAMDATA *qp;
	QMETA *meta;
	db_indx_t save_indx;
	db_lockmode_t lock_mode;
	db_pgno_t metapno, save_page;
	db_recno_t start, first, skipped, save_recno;
	int exact, is_first, locked, ret, t_ret, with_delete;
	int put_mode, meta_dirty;

	orig = dbc_orig->internal;
	dbp = dbc_orig->dbp;

	PANIC_CHECK(dbp->dbenv);

	with_delete = 0;
	lock_mode = DB_LOCK_READ;
	put_mode = 0;
	t_ret = 0;

	/* Check for invalid flags. */
	if ((ret = __db_cgetchk(dbp,
	    key, data, flags, orig->recno != RECNO_OOB)) != 0)
		return (ret);

	/* Clear OR'd in additional bits so we can check for flag equality. */
	if (LF_ISSET(DB_RMW)) {
		lock_mode = DB_LOCK_WRITE;
		LF_CLR(DB_RMW);
	}

	if (flags == DB_CONSUME) {
		with_delete = 1;
		flags = DB_FIRST;
		lock_mode = DB_LOCK_WRITE;
	}

	DEBUG_LREAD(dbc_orig, dbc_orig->txn, "qam_c_get",
	    flags == DB_SET || flags == DB_SET_RANGE ? key : NULL, NULL, flags);

	/* Get a copy of the original cursor, including position. */
	if ((ret = dbc_orig->c_dup(dbc_orig, &dbc, DB_POSITIONI)) != 0)
		return (ret);
	cp = dbc->internal;

	is_first = 0;

	/* get the meta page */
	metapno = ((QUEUE *)dbp->q_internal)->q_meta;
	if ((ret = __db_lget(dbc, 0, metapno, lock_mode, 0, &metalock)) != 0)
		return (ret);
	locked = 1;
	if ((ret = memp_fget(dbp->mpf, &metapno, 0, &meta)) != 0) {
		/* We did not fetch it, we can release the lock. */
		(void)__LPUT(dbc, metalock);
		return (ret);
	}

	skipped = 0;

	/* Make lint and friends happy. */
	first = 0;
	meta_dirty = 0;

	/* Release any previous lock if not in a transaction. */
	if (cp->lock.off != LOCK_INVALID) {
		(void)__TLPUT(dbc, cp->lock);
		cp->lock.off = LOCK_INVALID;
	}

retry:	/* Update the record number. */
	cp->start = start = meta->start;
	switch (flags) {
	case DB_CURRENT:
		break;
	case DB_NEXT:
		if (cp->recno != RECNO_OOB) {
			++cp->recno;
			break;
		}
		/* FALLTHROUGH */
	case DB_FIRST:
		flags = DB_NEXT;
		is_first = 1;

		/* get the first record number */
		cp->recno = first = meta->first_recno;

		/* if we will delete it, then increment */
		if (with_delete && first < meta->cur_recno) {
			if (DB_LOGGING(dbc))
				__qam_incfirst_log(dbp->dbenv, dbc->txn,
					&LSN(meta), 0,
					dbp->log_fileid, first);
			meta->first_recno++;
			meta_dirty = 1;
		}

		break;
	case DB_PREV:
		if (cp->recno != RECNO_OOB) {
			if (cp->recno <= meta->first_recno) {
				ret = DB_NOTFOUND;
				goto err;
			}
			--cp->recno;
			break;
		}
		/* FALLTHROUGH */
	case DB_LAST:
		cp->recno = meta->cur_recno;
		if (cp->recno == 0) {
			ret = DB_NOTFOUND;
			goto err;
		}
		break;
	case DB_SET:
	case DB_SET_RANGE:
		if ((ret = __qam_getno(dbp, key, &cp->recno)) != 0)
			goto err;
		break;
	}

	if (cp->recno > meta->cur_recno || cp->recno < start) {
		ret = DB_NOTFOUND;
		pg = NULL;
		if (skipped)
			goto undo_meta;
		goto err;
	}

	/* Don't hold the meta page long term. */
	if (locked) {
		if ((ret = __LPUT(dbc, metalock)) != 0)
			goto err;
		locked = 0;
	}

	/* Lock the record. */
	if ((ret = __db_lget(dbc, 0, cp->recno, lock_mode,
	    with_delete ? DB_LOCK_NOWAIT | DB_LOCK_RECORD : DB_LOCK_RECORD,
	    &lock)) == DB_LOCK_NOTGRANTED) {
		/*
		 * In the DB_CONSUME case we skip the locked
		 * record, someone else will pick it up.
		 *
		 */
		is_first = 0;
		if (skipped == 0)
			skipped = cp->recno;
		goto retry;
	}

	if (ret != 0)
		goto err;

	/*
	 * In the DB_FIRST or DB_LAST cases we must wait and then start over
	 * since the first/last may have moved while we slept.
	 * We release our locks and try again.
	 */
	if ((!with_delete && is_first) || flags == DB_LAST) {
		if ((ret =
		    __db_lget(dbc, 0, metapno, lock_mode, 0, &metalock)) != 0)
			goto err;
		if (cp->recno !=
		    (is_first ? meta->first_recno : meta->cur_recno)) {
			__LPUT(dbc, lock);
			if (is_first)
				flags = DB_FIRST;
			locked = 1;
			goto retry;
		}
		/* Don't hold the meta page long term. */
		if ((ret = __LPUT(dbc, metalock)) != 0)
			goto err;
	}

	/* Position the cursor on the record. */
	if ((ret =
	    __qam_position(dbc, &cp->recno, lock_mode, start, &exact)) != 0) {
		/* We cannot get the page, release the record lock. */
		(void)__LPUT(dbc, lock);
		goto err;
	}

	pg = cp->page;
	pglock = cp->lock;
	cp->lock = lock;
	cp->lock_mode = lock_mode;

	if (!exact) {
		if (flags == DB_NEXT || flags == DB_PREV || flags == DB_LAST) {
			/* Release locks and try again. */
			(void)memp_fput(dbp->mpf, cp->page, 0);
			(void)__LPUT(dbc, pglock);
			(void)__LPUT(dbc, cp->lock);
			cp->lock.off = LOCK_INVALID;
			if (flags == DB_LAST)
				flags = DB_PREV;
			if (!with_delete)
				is_first = 0;
			goto retry;
		}
		/* this is for the SET and SET_RANGE cases */
		ret = DB_KEYEMPTY;
		goto err1;
	}

	/* Return the key if the user didn't give us one. */
	if (flags != DB_SET &&
	    (ret = __db_retcopy(dbp, key, &cp->recno, sizeof(cp->recno),
	    &dbc->rkey.data, &dbc->rkey.ulen)) != 0) {
		if (with_delete)
			goto undo_meta;
		else
			goto err1;
	}

	qp = QAM_GET_RECORD(dbp, pg, cp->indx);

	/* Return the data item. */
	if ((ret = __db_retcopy(dbp, data, qp->data,
	    ((QUEUE *)dbp->q_internal)->re_len,
	    &dbc->rdata.data, &dbc->rdata.ulen)) != 0) {
		if (with_delete)
			goto undo_meta;
		else
			goto err1;
	}

	/* Finally, if we are doing DB_CONSUME mark the record. */
	if (with_delete) {
		if (DB_LOGGING(dbc))
			if ((ret = __qam_del_log(dbp->dbenv, dbc->txn,
			    &LSN(pg), 0, dbp->log_fileid, &LSN(pg),
			    pg->pgno, cp->indx, cp->recno)) != 0)
				goto undo_meta;

		F_CLR(qp, QAM_VALID);
		put_mode = DB_MPOOL_DIRTY;

		/*
		 * This code is responsible for correcting metadata.
		 * There are 3 cases.
		 *	1) We moved ahead more than one record.
		 *	2) We did not actually delete cp->recno.
		 *	3) We encountered at least one locked.
		 *		record and skipped them.
		 */
		if (cp->recno != first) {
			if (0) {
undo_meta:			is_first = 0;
			}
			if (locked == 0 && (t_ret = __db_lget(
			    dbc, 0, metapno, lock_mode, 0, &metalock)) != 0)
				goto err1;

			if (is_first) {
			/*
			 * Check to see if we moved past the first record,
			 * if so update meta so others can start past the
			 * deleted records.
			 */
				if (meta->first_recno > first) {
					meta->first_recno = cp->recno;
					meta_dirty = 1;
				}
			}
			else if (skipped == 0) {
				/*
				 * Error case: we did not actually delete the
				 * record, restore meta_first so that it is at
				 * least at or before cp->recno
				 */
				if (meta->first_recno > cp->recno) {
					meta->first_recno = cp->recno;
					meta_dirty = 1;
				}
			}
			else if (meta->first_recno > skipped) {
				/*
				 * We skipped some records because they were
				 * locked. If the meta-data page reflects a
				 * starting pointer after the skipped records
				 * we need to move it back to the first record
				 * that is not deleted or is sill locked.
				 * Release locks as we go, we are only
				 * reading to optimize future fetches.
				 */
				first = meta->first_recno;
				/* Don't hold the meta page long term. */
				__LPUT(dbc, metalock);
				locked = 0;

				/* reverify the skipped record */
				save_page = cp->pgno;
				save_indx = cp->indx;
				save_recno = cp->recno;
				save_lock = cp->lock;
				do {
					t_ret = __db_lget(dbc, 0, skipped,
					    DB_LOCK_READ,
					    DB_LOCK_NOWAIT | DB_LOCK_RECORD,
					    &lock);
					if (t_ret == DB_LOCK_NOTGRANTED)
						break;
					if (t_ret != 0)
						goto err1;
					if ((t_ret =
					    __qam_position(dbc, &skipped,
					    DB_LOCK_READ,
					    start, &exact)) != 0) {
						(void)__LPUT(dbc, lock);
						goto err1;
					}
					if ((t_ret = memp_fput(dbp->mpf,
					     cp->page, put_mode)) != 0)
						goto err1;
					if ((t_ret =__LPUT(dbc, lock)) != 0)
						goto err1;
					if ((t_ret =
					    __LPUT(dbc, cp->lock)) != 0)
						goto err1;
					if (exact)
						break;
				} while (++skipped <= first);

				t_ret = 0;
				if ((t_ret = __db_lget(
				    dbc, 0, metapno,
				    lock_mode, 0, &metalock)) != 0)
					goto err1;

				if (meta->first_recno > skipped) {
					meta->first_recno = skipped;
					meta_dirty = 1;
				}
				cp->pgno = save_page;
				cp->indx = save_indx;
				cp->recno = save_recno;
				cp->lock = save_lock;
			}
			locked = 1;
		}
	}

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


err1:
	if (pg != NULL) {
		if (!ret)
			ret = t_ret;
		t_ret = memp_fput(dbp->mpf, pg, put_mode);

		if (!ret)
			ret = t_ret;
		/* Doing record locking, release the page lock */
		t_ret = __LPUT(dbc, pglock);
	}

err:
	if (meta) {
		if (!ret)
			ret = t_ret;

		/* release the meta page */
		t_ret = memp_fput(
		    dbp->mpf, meta, meta_dirty ? DB_MPOOL_DIRTY : 0);

		if (!ret)
			ret = t_ret;

		/* Don't hold the meta page long term. */
		if (locked)
			t_ret = __LPUT(dbc, metalock);
	}

	if (!ret)
		ret = t_ret;

	t_ret = dbc->c_close(dbc);

	return (ret ? ret : t_ret);
}

/*
 * __qam_c_close --
 *	Close down the cursor from a single use.
 */
static int
__qam_c_close(dbc)
	DBC *dbc;
{
	QUEUE_CURSOR *cp;

	cp = dbc->internal;

	/* Discard any locks not acquired inside of a transaction. */
	if (cp->lock.off != LOCK_INVALID) {
		(void)__TLPUT(dbc, cp->lock);
		cp->lock.off = LOCK_INVALID;
	}

	cp->page = NULL;
	cp->pgno = PGNO_INVALID;
	cp->indx = 0;
	cp->lock.off = LOCK_INVALID;
	cp->lock_mode = DB_LOCK_NG;
	cp->recno = RECNO_OOB;
	cp->flags = 0;

	return (0);
}

/*
 * __qam_c_dup --
 *	Duplicate a queue cursor, such that the new one holds appropriate
 *	locks for the position of the original.
 *
 * PUBLIC: int __qam_c_dup __P((DBC *, DBC *));
 */
int
__qam_c_dup(orig_dbc, new_dbc)
	DBC *orig_dbc, *new_dbc;
{
	QUEUE_CURSOR *orig, *new;

	orig = orig_dbc->internal;
	new = new_dbc->internal;

	new->pgno = orig->pgno;
	new->indx = orig->indx;

	new->recno = orig->recno;
	new->start = orig->start;

	new->lock_mode = orig->lock_mode;

	/* reget the long term lock if we are not in a xact */
	if (orig_dbc->txn != NULL || orig->lock.off == LOCK_INVALID)
		return (0);

	return (__db_lget(new_dbc,
	    0, new->recno, new->lock_mode, DB_LOCK_RECORD, &new->lock));
}

/*
 * __qam_c_init
 *
 * PUBLIC: int __qam_c_init __P((DBC *));
 */
int
__qam_c_init(dbc)
	DBC *dbc;
{
	QUEUE_CURSOR *cp;
	DB *dbp;
	int ret;

	dbp = dbc->dbp;

	/* Allocate the internal structure. */
	if ((ret = __os_calloc(1, sizeof(QUEUE_CURSOR), &cp)) != 0)
		return (ret);

	/*
	 * Logical record numbers are always the same size, and we don't want
	 * to have to check for space every time we return one.  Allocate it
	 * in advance.
	 */
	if ((ret =
	    __os_malloc(sizeof(db_recno_t), NULL, &dbc->rkey.data)) != 0) {
		__os_free(cp, sizeof(QUEUE_CURSOR));
		return (ret);
	}
	dbc->rkey.ulen = sizeof(db_recno_t);

	/* Initialize methods. */
	dbc->internal = cp;
	dbc->c_del = __qam_c_del;
	dbc->c_get = __qam_c_get;
	dbc->c_put = __qam_c_put;
	dbc->c_am_close = __qam_c_close;
	dbc->c_am_destroy = __qam_c_destroy;

	return (0);
}
/*
 * __qam_c_destroy --
 *	Close a single cursor -- internal version.
 */
static int
__qam_c_destroy(dbc)
	DBC *dbc;
{
	/* Discard the structures. */
	__os_free(dbc->internal, sizeof(QUEUE_CURSOR));

	return (0);
}

/*
 * __qam_getno --
 *	Check the user's record number.
 */
static int
__qam_getno(dbp, key, rep)
	DB *dbp;
	const DBT *key;
	db_recno_t *rep;
{
	if ((*rep = *(db_recno_t *)key->data) == 0) {
		__db_err(dbp->dbenv, "illegal record number of 0");
		return (EINVAL);
	}
	return (0);
}
