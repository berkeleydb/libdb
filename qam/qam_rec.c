/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1999
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)qam_rec.c	11.8 (Sleepycat) 9/14/99";
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
#include "qam.h"
#include "log.h"

/*
 * __qam_inc_recover --
 *	Recovery function for inc.
 *
 * PUBLIC: int __qam_inc_recover __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
 */
int
__qam_inc_recover(dbenv, dbtp, lsnp, redo, info)
	DB_ENV *dbenv;
	DBT *dbtp;
	DB_LSN *lsnp;
	int redo;
	void *info;
{
	__qam_inc_args *argp;
	DB *file_dbp;
	DBC *dbc;
	DB_LOCK lock;
	DB_MPOOLFILE *mpf;
	QMETA *meta;
	db_pgno_t metapg;
	int cmp_p, modified, ret;

	COMPQUIET(info, NULL);
	REC_PRINT(__qam_inc_print);
	REC_INTRO(__qam_inc_read, 1);

	metapg = ((QUEUE *)file_dbp->q_internal)->q_meta;

	dbc->txn = argp->txnid;
	if ((ret = __db_lget(dbc, 0, metapg,  DB_LOCK_WRITE, 0, &lock)) != 0)
		goto done;
	if ((ret = memp_fget(mpf, &metapg, 0, &meta)) != 0) {
		if (redo) {
			if ((ret = memp_fget(mpf,
			    &metapg, DB_MPOOL_CREATE, &meta)) != 0) {
				(void)__LPUT(dbc, lock);
				goto out;
			}
			meta->dbmeta.pgno = metapg;
	                meta->dbmeta.type = P_QAMMETA;

		} else {
			*lsnp = argp->prev_lsn;
			ret = 0;
			(void)__LPUT(dbc, lock);
			goto out;
		}
	}

	modified = 0;
	cmp_p = log_compare(&LSN(meta), &argp->lsn);

	/* Increment the counter if the page is before this lsn */
	if (cmp_p == 0) {
		modified = 1;
		meta->cur_recno++;
		meta->dbmeta.lsn = *lsnp;
	}
	if ((ret = memp_fput(mpf, meta, modified ? DB_MPOOL_DIRTY : 0)))
		goto out;

	(void)__LPUT(dbc, lock);

done:	*lsnp = argp->prev_lsn;
	ret = 0;

out:	REC_CLOSE;
}

/*
 * __qam_incfirst_recover --
 *	Recovery function for incfirst.
 *
 * PUBLIC: int __qam_incfirst_recover
 * PUBLIC:   __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
 */
int
__qam_incfirst_recover(dbenv, dbtp, lsnp, redo, info)
	DB_ENV *dbenv;
	DBT *dbtp;
	DB_LSN *lsnp;
	int redo;
	void *info;
{
	__qam_incfirst_args *argp;
	DB *file_dbp;
	DBC *dbc;
	DB_LOCK lock;
	DB_MPOOLFILE *mpf;
	QMETA *meta;
	db_pgno_t metapg;
	int modified, ret;

	COMPQUIET(info, NULL);
	REC_PRINT(__qam_incfirst_print);
	REC_INTRO(__qam_incfirst_read, 1);

	metapg = ((QUEUE *)file_dbp->q_internal)->q_meta;

	dbc->txn = argp->txnid;
	if ((ret = __db_lget(dbc, 0, metapg,  DB_LOCK_WRITE, 0, &lock)) != 0)
		goto done;
	if ((ret = memp_fget(mpf, &metapg, 0, &meta)) != 0) {
		if (redo) {
			if ((ret = memp_fget(mpf,
			    &metapg, DB_MPOOL_CREATE, &meta)) != 0) {
				(void)__LPUT(dbc, lock);
				goto out;
			}
			meta->dbmeta.pgno = metapg;
	                meta->dbmeta.type = P_QAMMETA;
		} else {
			*lsnp = argp->prev_lsn;
			ret = 0;
			(void)__LPUT(dbc, lock);
			goto out;
		}
	}

	modified = 0;

	/*
	 * Only move first_recno backwards so we pick up the aborted delete.
	 * If we are going forward then we could patch first up, but it will
	 * get fixed by normal operations.
	 */
	if (!redo) {
		if (meta->first_recno > argp->recno) {
			meta->first_recno = argp->recno;
			modified = 1;
		}
	}


	if ((ret = memp_fput(mpf, meta, modified ? DB_MPOOL_DIRTY : 0)))
		goto out;

	(void)__LPUT(dbc, lock);

done:	*lsnp = argp->prev_lsn;
	ret = 0;

out:	REC_CLOSE;
}

/*
 * __qam_mvptr_recover --
 *	Recovery function for mvptr.
 *
 * PUBLIC: int __qam_mvptr_recover
 * PUBLIC:   __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
 */
int
__qam_mvptr_recover(dbenv, dbtp, lsnp, redo, info)
	DB_ENV *dbenv;
	DBT *dbtp;
	DB_LSN *lsnp;
	int redo;
	void *info;
{
	__qam_mvptr_args *argp;
	DB *file_dbp;
	DBC *dbc;
	DB_LOCK lock;
	DB_MPOOLFILE *mpf;
	QMETA *meta;
	db_pgno_t metapg;
	int cmp_p, cmp_n, modified, ret;

	COMPQUIET(info, NULL);
	REC_PRINT(__qam_mvptr_print);
	REC_INTRO(__qam_mvptr_read, 1);

	metapg = ((QUEUE *)file_dbp->q_internal)->q_meta;

	dbc->txn = argp->txnid;
	if ((ret = __db_lget(dbc, 0, metapg,  DB_LOCK_WRITE, 0, &lock)) != 0)
		goto done;
	if ((ret = memp_fget(mpf, &metapg, 0, &meta)) != 0) {
		if (redo) {
			if ((ret = memp_fget(mpf,
			    &metapg, DB_MPOOL_CREATE, &meta)) != 0) {
				(void)__LPUT(dbc, lock);
				goto out;
			}
			meta->dbmeta.pgno = metapg;
	                meta->dbmeta.type = P_QAMMETA;
		} else {
			*lsnp = argp->prev_lsn;
			ret = 0;
			(void)__LPUT(dbc, lock);
			goto out;
		}
	}

	modified = 0;
	cmp_n = log_compare(lsnp, &meta->dbmeta.lsn);
	cmp_p = log_compare(&meta->dbmeta.lsn, &argp->metalsn);

	/*
	 * We do not undo movements, just back out the LSN change
	 * On redo, since we are checking for exact LSN matches
	 * we can just redo the value.
	 */
	if (cmp_n == 0 && !redo) {
		modified = 1;
		meta->dbmeta.lsn = argp->metalsn;
	} else if (cmp_p == 0 && redo) {
		if (argp->opcode & QAM_SETFIRST)
			meta->first_recno = argp->new_first;

		if (argp->opcode & QAM_SETCUR)
			meta->cur_recno = argp->new_cur;

		modified = 1;
		meta->dbmeta.lsn = *lsnp;
	}


	if ((ret = memp_fput(mpf, meta, modified ? DB_MPOOL_DIRTY : 0)))
		goto out;

	(void)__LPUT(dbc, lock);

done:	*lsnp = argp->prev_lsn;
	ret = 0;

out:	REC_CLOSE;
}
/*
 * __qam_del_recover --
 *	Recovery function for del.
 *
 * PUBLIC: int __qam_del_recover __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
 */
int
__qam_del_recover(dbenv, dbtp, lsnp, redo, info)
	DB_ENV *dbenv;
	DBT *dbtp;
	DB_LSN *lsnp;
	int redo;
	void *info;
{
	__qam_del_args *argp;
	DB *file_dbp;
	DBC *dbc;
	DB_LOCK lock;
	DB_MPOOLFILE *mpf;
	QAMDATA *qp;
	QMETA *meta;
	QPAGE *pagep;
	db_pgno_t metapg;
	int cmp_n, modified, ret;

	COMPQUIET(info, NULL);
	REC_PRINT(__qam_del_print);
	REC_INTRO(__qam_del_read, 1);

	dbc->txn = argp->txnid;
	if ((ret = memp_fget(mpf, &argp->pgno, 0, &pagep)) != 0) {
		if (redo) {
			if ((ret = memp_fget(mpf,
			    &argp->pgno, DB_MPOOL_CREATE, &pagep)) != 0)
				goto out;
			pagep->pgno = argp->pgno;
	                pagep->type = P_QAMDATA;
		} else {
			*lsnp = argp->prev_lsn;
			ret = 0;
			goto out;
		}
	}

	modified = 0;
	cmp_n = log_compare(lsnp, &LSN(pagep));

	if (cmp_n > 0 && redo) {
		/* Need to redo delete - clear the valid bit */
		qp = QAM_GET_RECORD(file_dbp, pagep, argp->indx);
		F_CLR(qp, QAM_VALID);
		LSN(pagep) = *lsnp;
		modified = 1;
	} else if (cmp_n <= 0 && !redo) {
		/* make sure first is behind us */
		metapg = ((QUEUE *)file_dbp->q_internal)->q_meta;
		if ((ret =
		    __db_lget(dbc, 0, metapg, DB_LOCK_WRITE, 0, &lock)) != 0)
			return (ret);
		if ((ret = memp_fget(file_dbp->mpf, &metapg, 0, &meta)) != 0) {
			(void)__LPUT(dbc, lock);
			goto done;
		}
		if (argp->recno < meta->first_recno) {
			meta->first_recno = argp->recno;
			(void)memp_fput(file_dbp->mpf, meta, DB_MPOOL_DIRTY);
		} else
			(void)memp_fput(file_dbp->mpf, meta, 0);
		(void)__LPUT(dbc, lock);

		/* Need to undo delete - mark the record as present */
		qp = QAM_GET_RECORD(file_dbp, pagep, argp->indx);
		F_SET(qp, QAM_VALID);
		LSN(pagep) = argp->lsn;
		modified = 1;
	}
	if ((ret = memp_fput(mpf, pagep, modified ? DB_MPOOL_DIRTY : 0)))
		goto out;

done:	*lsnp = argp->prev_lsn;
	ret = 0;

out:	REC_CLOSE;
}

/*
 * __qam_add_recover --
 *	Recovery function for add.
 *
 * PUBLIC: int __qam_add_recover __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
 */
int
__qam_add_recover(dbenv, dbtp, lsnp, redo, info)
	DB_ENV *dbenv;
	DBT *dbtp;
	DB_LSN *lsnp;
	int redo;
	void *info;
{
	__qam_add_args *argp;
	DB *file_dbp;
	DBC *dbc;
	DB_MPOOLFILE *mpf;
	QAMDATA *qp;
	QPAGE *pagep;
	int cmp_n, cmp_p, modified, ret;

	COMPQUIET(info, NULL);
	REC_PRINT(__qam_add_print);
	REC_INTRO(__qam_add_read, 1);

	dbc->txn = argp->txnid;
	modified = 0;
	if ((ret = memp_fget(mpf, &argp->pgno, 0, &pagep)) != 0) {
		if ((ret = memp_fget(mpf,
		    &argp->pgno, DB_MPOOL_CREATE, &pagep)) != 0)
			goto out;
		pagep->pgno = argp->pgno;
		pagep->type = P_QAMDATA;
		modified = 1;
	}

	cmp_n = log_compare(lsnp, &LSN(pagep));
	cmp_p = log_compare(&LSN(pagep), &argp->lsn);

	if (cmp_p == 0 && redo) {
		/* Need to redo add - put the record on page */
		if ((ret = __qam_pitem(dbc, pagep, argp->indx, argp->recno,
				&argp->data)) != 0)
			goto err;
		LSN(pagep) = *lsnp;
		modified = 1;
	} else if (cmp_n == 0 && !redo) {
		/*
		 * Need to undo add
		 *	If this was an overwrite, put old record back.
		 *	Otherwise just clear the valid bit
		 */
		if (argp->olddata.size != 0) {
			if ((ret = __qam_pitem(dbc, pagep,
			    argp->indx, argp->recno, &argp->olddata)) != 0)
				goto err;

			if (!(argp->vflag & QAM_VALID)) {
				qp = QAM_GET_RECORD(
				    file_dbp, pagep, argp->indx);
				F_CLR(qp, QAM_VALID);
			}
			modified = 1;
		} else {
			qp = QAM_GET_RECORD(file_dbp, pagep, argp->indx);
			qp->flags = 0;
			modified = 1;
		}
		LSN(pagep) = argp->lsn;
	}

err:	if ((ret = memp_fput(mpf, pagep, modified ? DB_MPOOL_DIRTY : 0)))
		goto out;

done:	*lsnp = argp->prev_lsn;
	ret = 0;

out:	REC_CLOSE;
}
