/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1999, 2000
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char revid[] = "$Id: qam.c,v 11.52 2000/05/24 18:23:32 ubell Exp $";
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
#include "btree.h"
#include "qam.h"

static int __qam_c_close __P((DBC *, db_pgno_t, int *));
static int __qam_c_del __P((DBC *));
static int __qam_c_destroy __P((DBC *));
static int __qam_c_get __P((DBC *, DBT *, DBT *, u_int32_t, db_pgno_t *));
static int __qam_c_put __P((DBC *, DBT *, DBT *, u_int32_t, db_pgno_t *));
static int __qam_getno __P((DB *, const DBT *, db_recno_t *));
static int __qam_nrecs __P((DBC *, db_recno_t *, db_recno_t *));
static int __qam_position
	       __P((DBC *, db_recno_t *, db_lockmode_t, db_recno_t, int *));

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
	int ret, t_ret;

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

	ret = memp_fput(dbp->mpf, meta, 0);

	/* Don't hold the meta page long term. */
	if ((t_ret = __LPUT(dbc, lock)) != 0 && ret == 0)
		ret = t_ret;

	return (ret);
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
	cp = (QUEUE_CURSOR *)dbc->internal;

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

	if (PGNO(cp->page) == 0) {
		if (F_ISSET(dbp, DB_AM_RDONLY)) {
			*exactp = 0;
			return (0);
		}
		PGNO(cp->page) = pg;
		TYPE(cp->page) = P_QAMDATA;
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
		goto len_err;

	qp = QAM_GET_RECORD(dbp, pagep, indx);

	p = qp->data;
	size = data->size;
	datap = data;
	if (F_ISSET(data, DB_DBT_PARTIAL)) {
		if (data->doff + data->dlen > t->re_len) {
			alloced = data->dlen;
			goto len_err;
		}
		if (data->size != data->dlen) {
len_err:		__db_err(dbp->dbenv,
			    "Length improper for fixed length record %lu",
			    (u_long)(alloced ? alloced : data->size));
			return (EINVAL);
		}
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

			if ((ret = __os_malloc(dbp->dbenv,
			    t->re_len, NULL, &datap->data)) != 0)
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
 *	Cursor put for queued access method.
 *	BEFORE and AFTER cannot be specified.
 */
static int
__qam_c_put(dbc, key, data, flags, pgnop)
	DBC *dbc;
	DBT *key, *data;
	u_int32_t flags;
	db_pgno_t *pgnop;
{
	QUEUE_CURSOR *cp;
	DB *dbp;
	DB_LOCK lock;
	QMETA *meta;
	db_pgno_t pg;
	db_recno_t new_cur, new_first;
	u_int32_t opcode;
	int exact, ret, t_ret;

	COMPQUIET(key, NULL);

	dbp = dbc->dbp;
	if (pgnop != NULL)
		*pgnop = PGNO_INVALID;

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
		cp->page = NULL;
		return (ret == 0 ? DB_KEYEXIST : ret);
	}

	/* Put the item on the page. */
	ret = __qam_pitem(dbc, (QPAGE *)cp->page, cp->indx, cp->recno, data);

	/* Doing record locking, release the page lock */
	if ((t_ret = __LPUT(dbc, cp->lock)) != 0 && ret == 0)
		ret = t_ret;
	if ((t_ret = memp_fput(dbp->mpf, cp->page, DB_MPOOL_DIRTY)) && ret == 0)
		ret = t_ret;
	cp->page = NULL;
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
 *	Add a record to the queue.
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

	cp = (QUEUE_CURSOR *)dbc->internal;

	/* Check for invalid flags. */
	if ((ret = __db_putchk(dbp,
	    key, data, flags, F_ISSET(dbp, DB_AM_RDONLY), 0)) != 0)
		goto done;

	/* If not appending, then just call the cursor routine */
	if (flags != DB_APPEND) {
		if ((ret = __qam_getno(dbp, key, &cp->recno)) != 0)
			goto done;
		__qam_nrecs(dbc, &total, &cp->start);

		ret = __qam_c_put(dbc, NULL, data, flags, NULL);
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
	if ((ret = memp_fput(dbp->mpf, meta, DB_MPOOL_DIRTY)) != 0) {
		(void)__LPUT(dbc, lock);
		goto done;
	}

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
	ret = __qam_pitem(dbc, page,
	    QAM_RECNO_INDEX(dbp, pg, start, recno), recno, data);

	/* Doing record locking, release the page lock */
	if ((t_ret = __LPUT(dbc, lock)) != 0 && ret == 0)
		ret = t_ret;

	if ((t_ret
	    = memp_fput(dbp->mpf, page, DB_MPOOL_DIRTY)) != 0 && ret == 0)
		ret = t_ret;


	/* Return the record number to the user. */
	 ret = __db_retcopy(dbp, key,
	     &recno, sizeof(recno), &dbc->rkey.data, &dbc->rkey.ulen);

done:
	/* Discard the cursor. */
	if ((t_ret = dbc->c_close(dbc)) != 0 && ret == 0)
		ret = t_ret;

	return (ret);
}

/*
 * __qam_c_del --
 *	Qam cursor->am_del function
 */
static int
__qam_c_del(dbc)
	DBC *dbc;
{
	QUEUE_CURSOR *cp;
	DB *dbp;
	DBT data;
	DB_LOCK lock;
	PAGE *pagep;
	QAMDATA *qp;
	db_recno_t start;
	db_recno_t total;
	int exact, ret, t_ret;

	dbp = dbc->dbp;
	cp = (QUEUE_CURSOR *)dbc->internal;
	ret = 0;

	__qam_nrecs(dbc, &total, &cp->start);
	start = cp->start;

	if (cp->recno > total) {
		ret = DB_NOTFOUND;
		return (ret);
	}

	if ((ret = __db_lget(dbc,
	    0, cp->recno, DB_LOCK_WRITE, DB_LOCK_RECORD, &lock)) != 0)
		return (ret);

	cp->lock_mode = DB_LOCK_WRITE;
	/* Find the record ; delete only deletes exact matches. */
	if ((ret = __qam_position(dbc,
	    &cp->recno, DB_LOCK_WRITE, start, &exact)) != 0) {
		cp->lock = lock;
		return (ret);
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
	cp->page = NULL;
	/* Doing record locking, release the page lock */
	if ((t_ret = __LPUT(dbc, cp->lock)) != 0) {
		cp->lock = lock;
		return (ret ? ret : t_ret);
	}
	cp->lock = lock;
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

	cp = (QUEUE_CURSOR *)dbc->internal;
	if ((ret = __qam_getno(dbp, key, &cp->recno)) != 0)
		goto err;

	ret = __qam_c_del(dbc);

	/* Release the cursor. */
err:	if ((t_ret = dbc->c_close(dbc)) != 0 && ret == 0)
		ret = t_ret;

	return (ret);
}

/*
 * __qam_c_get --
 *	Queue cursor->c_get function.
 */
static int
__qam_c_get(dbc, key, data, flags, pgnop)
	DBC *dbc;
	DBT *key, *data;
	u_int32_t flags;
	db_pgno_t *pgnop;
{
	QUEUE_CURSOR *cp;
	DB *dbp;
	DB_LOCK lock, pglock, metalock, save_lock;
	DBT tmp;
	PAGE *pg;
	QAMDATA *qp;
	QMETA *meta;
	db_indx_t save_indx;
	db_lockmode_t lock_mode;
	db_pgno_t metapno, save_page;
	db_recno_t start, first, skipped, save_recno;
	int exact, is_first, locked, ret, t_ret, with_delete;
	int put_mode, meta_dirty;

	cp = (QUEUE_CURSOR *)dbc->internal;
	dbp = dbc->dbp;

	PANIC_CHECK(dbp->dbenv);

	with_delete = 0;
	lock_mode = DB_LOCK_READ;
	put_mode = 0;
	t_ret = 0;
	*pgnop = 0;

	if (F_ISSET(dbc, DBC_RMW))
		lock_mode = DB_LOCK_WRITE;

	if (flags == DB_CONSUME) {
		with_delete = 1;
		flags = DB_FIRST;
		lock_mode = DB_LOCK_WRITE;
	}

	DEBUG_LREAD(dbc, dbc->txn, "qam_c_get",
	    flags == DB_SET || flags == DB_SET_RANGE ? key : NULL, NULL, flags);

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
	case DB_NEXT_DUP:
		ret = DB_NOTFOUND;
		goto err;
		/* NOTREACHED */
	case DB_NEXT:
	case DB_NEXT_NODUP:
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
	case DB_PREV_NODUP:
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
	case DB_GET_BOTH:
	case DB_SET:
	case DB_SET_RANGE:
		if ((ret = __qam_getno(dbp, key, &cp->recno)) != 0)
			goto err;
		break;
	default:
		ret = __db_unknown_flag(dbp->dbenv, "__qam_c_get", flags);
		goto err;
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
	    &lock)) == DB_LOCK_NOTGRANTED && with_delete) {
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
		if (flags == DB_NEXT || flags == DB_NEXT_NODUP
		    || flags == DB_PREV || flags == DB_PREV_NODUP
		    || flags == DB_LAST) {
			/* Release locks and try again. */
			(void)memp_fput(dbp->mpf, cp->page, 0);
			cp->page = NULL;
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
	if (flags != DB_SET && flags != DB_GET_BOTH &&
	    (ret = __db_retcopy(dbp, key, &cp->recno, sizeof(cp->recno),
	    &dbc->rkey.data, &dbc->rkey.ulen)) != 0) {
		if (with_delete)
			goto undo_meta;
		else
			goto err1;
	}
	F_SET(key, DB_DBT_ISSET);

	qp = QAM_GET_RECORD(dbp, pg, cp->indx);

	/* Return the data item. */
	if (flags == DB_GET_BOTH) {
		/*
		 * Need to compare
		 */
		tmp.data = qp->data;
		tmp.size = ((QUEUE *)dbp->q_internal)->re_len;
		if ((ret = __bam_defcmp(data, &tmp)) != 0) {
			ret = DB_NOTFOUND;
			goto err1;
		}
	}
	if ((ret = __db_retcopy(dbp, data, qp->data,
	    ((QUEUE *)dbp->q_internal)->re_len,
	    &dbc->rdata.data, &dbc->rdata.ulen)) != 0) {
		if (with_delete)
			goto undo_meta;
		else
			goto err1;
	}
	F_SET(data, DB_DBT_ISSET);

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

err1:
	cp->page = NULL;
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

	return (ret ? ret : t_ret);
}

/*
 * __qam_c_close --
 *	Close down the cursor from a single use.
 */
static int
__qam_c_close(dbc, root_pgno, rmroot)
	DBC *dbc;
	db_pgno_t root_pgno;
	int *rmroot;
{
	QUEUE_CURSOR *cp;

	COMPQUIET(root_pgno, 0);
	COMPQUIET(rmroot, NULL);

	cp = (QUEUE_CURSOR *)dbc->internal;

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

	orig = (QUEUE_CURSOR *)orig_dbc->internal;
	new = (QUEUE_CURSOR *)new_dbc->internal;

	new->recno = orig->recno;
	new->start = orig->start;

	/* reget the long term lock if we are not in a xact */
	if (orig_dbc->txn != NULL ||
	    !STD_LOCKING(orig_dbc) || orig->lock.off == LOCK_INVALID)
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
	cp = (QUEUE_CURSOR *)dbc->internal;
	if (cp == NULL) {
		if ((ret =
		    __os_calloc(dbp->dbenv, 1, sizeof(QUEUE_CURSOR), &cp)) != 0)
			return (ret);
		dbc->internal = (DBC_INTERNAL *)cp;
	}

	/* Initialize methods. */
	dbc->c_close = __db_c_close;
	dbc->c_count = __db_c_count;
	dbc->c_del = __db_c_del;
	dbc->c_dup = __db_c_dup;
	dbc->c_get = __db_c_get;
	dbc->c_put = __db_c_put;
	dbc->c_am_close = __qam_c_close;
	dbc->c_am_del = __qam_c_del;
	dbc->c_am_destroy = __qam_c_destroy;
	dbc->c_am_get = __qam_c_get;
	dbc->c_am_put = __qam_c_put;
	dbc->c_am_writelock = NULL;

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
