/* Do not edit: automatically built by gen_rec.awk. */
#include "db_config.h"

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <ctype.h>
#include <string.h>
#endif

#include "db_int.h"
#include "db_page.h"
#include "db_dispatch.h"
#include "db_am.h"
#include "log.h"
#include "qam.h"
#include "rep.h"
#include "txn.h"

/*
 * PUBLIC: int __qam_incfirst_log __P((DB_ENV *, DB_TXN *, DB_LSN *, u_int32_t,
 * PUBLIC:      int32_t, db_recno_t, db_pgno_t));
 */
int
__qam_incfirst_log(dbenv, txnid, ret_lsnp, flags,
	fileid, recno, meta_pgno)
	DB_ENV *dbenv;
	DB_TXN *txnid;
	DB_LSN *ret_lsnp;
	u_int32_t flags;
	int32_t fileid;
	db_recno_t recno;
	db_pgno_t meta_pgno;
{
	DBT logrec;
	DB_LSN *lsnp, null_lsn;
	u_int32_t uinttmp;
	u_int32_t rectype, txn_num;
	int ret;
	u_int8_t *bp;

	rectype = DB_qam_incfirst;
	if (txnid != NULL &&
	    TAILQ_FIRST(&txnid->kids) != NULL &&
	    (ret = __txn_activekids(dbenv, rectype, txnid)) != 0)
		return (ret);
	txn_num = txnid == NULL ? 0 : txnid->txnid;
	if (txnid == NULL) {
		ZERO_LSN(null_lsn);
		lsnp = &null_lsn;
	} else
		lsnp = &txnid->last_lsn;
	logrec.size = sizeof(rectype) + sizeof(txn_num) + sizeof(DB_LSN)
	    + sizeof(u_int32_t)
	    + sizeof(u_int32_t)
	    + sizeof(u_int32_t);
	if ((ret = __os_malloc(dbenv, logrec.size, &logrec.data)) != 0)
		return (ret);

	bp = logrec.data;

	memcpy(bp, &rectype, sizeof(rectype));
	bp += sizeof(rectype);

	memcpy(bp, &txn_num, sizeof(txn_num));
	bp += sizeof(txn_num);

	memcpy(bp, lsnp, sizeof(DB_LSN));
	bp += sizeof(DB_LSN);

	uinttmp = (u_int32_t)fileid;
	memcpy(bp, &uinttmp, sizeof(uinttmp));
	bp += sizeof(uinttmp);

	uinttmp = (u_int32_t)recno;
	memcpy(bp, &uinttmp, sizeof(uinttmp));
	bp += sizeof(uinttmp);

	uinttmp = (u_int32_t)meta_pgno;
	memcpy(bp, &uinttmp, sizeof(uinttmp));
	bp += sizeof(uinttmp);

	DB_ASSERT((u_int32_t)(bp - (u_int8_t *)logrec.data) == logrec.size);
	ret = dbenv->log_put(dbenv, ret_lsnp, (DBT *)&logrec, flags);
	if (txnid != NULL && ret == 0)
		txnid->last_lsn = *ret_lsnp;
#ifdef LOG_DIAGNOSTIC
	if (ret != 0)
		(void)__qam_incfirst_print(dbenv,
		    (DBT *)&logrec, ret_lsnp, NULL, NULL);
#endif
	__os_free(dbenv, logrec.data, logrec.size);
	return (ret);
}

/*
 * PUBLIC: int __qam_incfirst_getpgnos __P((DB_ENV *, DBT *, DB_LSN *,
 * PUBLIC:      db_recops, void *));
 */
int
__qam_incfirst_getpgnos(dbenv, rec, lsnp, notused1, summary)
	DB_ENV *dbenv;
	DBT *rec;
	DB_LSN *lsnp;
	db_recops notused1;
	void *summary;
{
	DB *dbp;
	TXN_RECS *t;
	__qam_incfirst_args *argp;
	u_int32_t ret;

	COMPQUIET(notused1, DB_TXN_ABORT);

	argp = NULL;
	t = (TXN_RECS *)summary;

	if ((ret = __qam_incfirst_read(dbenv, rec->data, &argp)) != 0)
		return (ret);

	if ((ret = __db_fileid_to_db(dbenv, &dbp, argp->fileid, 0)) != 0)
		goto err;

	if ((ret = __rep_check_alloc(dbenv, t, 1)) != 0)
		goto err;

	t->array[t->npages].flags = 0;
	t->array[t->npages].fid = argp->fileid;
	t->array[t->npages].lsn = *lsnp;
	t->array[t->npages].pgdesc.pgno = argp->meta_pgno;
	t->array[t->npages].pgdesc.type = DB_PAGE_LOCK;
	memcpy(t->array[t->npages].pgdesc.fileid, dbp->fileid,
	    DB_FILE_ID_LEN);
	t->npages++;

err:	if (argp != NULL)
		__os_free(dbenv, argp, sizeof(__qam_incfirst_args));
	return (ret);
}

/*
 * PUBLIC: int __qam_incfirst_print __P((DB_ENV *, DBT *, DB_LSN *, db_recops,
 * PUBLIC:      void *));
 */
int
__qam_incfirst_print(dbenv, dbtp, lsnp, notused2, notused3)
	DB_ENV *dbenv;
	DBT *dbtp;
	DB_LSN *lsnp;
	db_recops notused2;
	void *notused3;
{
	__qam_incfirst_args *argp;
	int ret;

	notused2 = DB_TXN_ABORT;
	notused3 = NULL;

	if ((ret = __qam_incfirst_read(dbenv, dbtp->data, &argp)) != 0)
		return (ret);
	(void)printf(
	    "[%lu][%lu]qam_incfirst: rec: %lu txnid %lx prevlsn [%lu][%lu]\n",
	    (u_long)lsnp->file,
	    (u_long)lsnp->offset,
	    (u_long)argp->type,
	    (u_long)argp->txnid->txnid,
	    (u_long)argp->prev_lsn.file,
	    (u_long)argp->prev_lsn.offset);
	(void)printf("\tfileid: %ld\n", (long)argp->fileid);
	(void)printf("\trecno: %lu\n", (u_long)argp->recno);
	(void)printf("\tmeta_pgno: %lu\n", (u_long)argp->meta_pgno);
	(void)printf("\n");
	__os_free(dbenv, argp, 0);
	return (0);
}

/*
 * PUBLIC: int __qam_incfirst_read __P((DB_ENV *, void *,
 * PUBLIC:      __qam_incfirst_args **));
 */
int
__qam_incfirst_read(dbenv, recbuf, argpp)
	DB_ENV *dbenv;
	void *recbuf;
	__qam_incfirst_args **argpp;
{
	__qam_incfirst_args *argp;
	int ret;
	u_int32_t uinttmp;
	u_int8_t *bp;

	ret = __os_malloc(dbenv, sizeof(__qam_incfirst_args) +
	    sizeof(DB_TXN), &argp);
	if (ret != 0)
		return (ret);
	argp->txnid = (DB_TXN *)&argp[1];

	bp = recbuf;
	memcpy(&argp->type, bp, sizeof(argp->type));
	bp += sizeof(argp->type);

	memcpy(&argp->txnid->txnid,  bp, sizeof(argp->txnid->txnid));
	bp += sizeof(argp->txnid->txnid);

	memcpy(&argp->prev_lsn, bp, sizeof(DB_LSN));
	bp += sizeof(DB_LSN);

	memcpy(&uinttmp, bp, sizeof(uinttmp));
	argp->fileid = (int32_t)uinttmp;
	bp += sizeof(uinttmp);

	memcpy(&uinttmp, bp, sizeof(uinttmp));
	argp->recno = (db_recno_t)uinttmp;
	bp += sizeof(uinttmp);

	memcpy(&uinttmp, bp, sizeof(uinttmp));
	argp->meta_pgno = (db_pgno_t)uinttmp;
	bp += sizeof(uinttmp);

	*argpp = argp;
	return (0);
}

/*
 * PUBLIC: int __qam_mvptr_log __P((DB_ENV *, DB_TXN *, DB_LSN *, u_int32_t,
 * PUBLIC:      u_int32_t, int32_t, db_recno_t, db_recno_t, db_recno_t, db_recno_t, DB_LSN *,
 * PUBLIC:      db_pgno_t));
 */
int
__qam_mvptr_log(dbenv, txnid, ret_lsnp, flags,
	opcode, fileid, old_first, new_first, old_cur, new_cur,
	metalsn, meta_pgno)
	DB_ENV *dbenv;
	DB_TXN *txnid;
	DB_LSN *ret_lsnp;
	u_int32_t flags;
	u_int32_t opcode;
	int32_t fileid;
	db_recno_t old_first;
	db_recno_t new_first;
	db_recno_t old_cur;
	db_recno_t new_cur;
	DB_LSN * metalsn;
	db_pgno_t meta_pgno;
{
	DBT logrec;
	DB_LSN *lsnp, null_lsn;
	u_int32_t uinttmp;
	u_int32_t rectype, txn_num;
	int ret;
	u_int8_t *bp;

	rectype = DB_qam_mvptr;
	if (txnid != NULL &&
	    TAILQ_FIRST(&txnid->kids) != NULL &&
	    (ret = __txn_activekids(dbenv, rectype, txnid)) != 0)
		return (ret);
	txn_num = txnid == NULL ? 0 : txnid->txnid;
	if (txnid == NULL) {
		ZERO_LSN(null_lsn);
		lsnp = &null_lsn;
	} else
		lsnp = &txnid->last_lsn;
	logrec.size = sizeof(rectype) + sizeof(txn_num) + sizeof(DB_LSN)
	    + sizeof(u_int32_t)
	    + sizeof(u_int32_t)
	    + sizeof(u_int32_t)
	    + sizeof(u_int32_t)
	    + sizeof(u_int32_t)
	    + sizeof(u_int32_t)
	    + sizeof(*metalsn)
	    + sizeof(u_int32_t);
	if ((ret = __os_malloc(dbenv, logrec.size, &logrec.data)) != 0)
		return (ret);

	bp = logrec.data;

	memcpy(bp, &rectype, sizeof(rectype));
	bp += sizeof(rectype);

	memcpy(bp, &txn_num, sizeof(txn_num));
	bp += sizeof(txn_num);

	memcpy(bp, lsnp, sizeof(DB_LSN));
	bp += sizeof(DB_LSN);

	uinttmp = (u_int32_t)opcode;
	memcpy(bp, &uinttmp, sizeof(uinttmp));
	bp += sizeof(uinttmp);

	uinttmp = (u_int32_t)fileid;
	memcpy(bp, &uinttmp, sizeof(uinttmp));
	bp += sizeof(uinttmp);

	uinttmp = (u_int32_t)old_first;
	memcpy(bp, &uinttmp, sizeof(uinttmp));
	bp += sizeof(uinttmp);

	uinttmp = (u_int32_t)new_first;
	memcpy(bp, &uinttmp, sizeof(uinttmp));
	bp += sizeof(uinttmp);

	uinttmp = (u_int32_t)old_cur;
	memcpy(bp, &uinttmp, sizeof(uinttmp));
	bp += sizeof(uinttmp);

	uinttmp = (u_int32_t)new_cur;
	memcpy(bp, &uinttmp, sizeof(uinttmp));
	bp += sizeof(uinttmp);

	if (metalsn != NULL)
		memcpy(bp, metalsn, sizeof(*metalsn));
	else
		memset(bp, 0, sizeof(*metalsn));
	bp += sizeof(*metalsn);

	uinttmp = (u_int32_t)meta_pgno;
	memcpy(bp, &uinttmp, sizeof(uinttmp));
	bp += sizeof(uinttmp);

	DB_ASSERT((u_int32_t)(bp - (u_int8_t *)logrec.data) == logrec.size);
	ret = dbenv->log_put(dbenv, ret_lsnp, (DBT *)&logrec, flags);
	if (txnid != NULL && ret == 0)
		txnid->last_lsn = *ret_lsnp;
#ifdef LOG_DIAGNOSTIC
	if (ret != 0)
		(void)__qam_mvptr_print(dbenv,
		    (DBT *)&logrec, ret_lsnp, NULL, NULL);
#endif
	__os_free(dbenv, logrec.data, logrec.size);
	return (ret);
}

/*
 * PUBLIC: int __qam_mvptr_getpgnos __P((DB_ENV *, DBT *, DB_LSN *, db_recops,
 * PUBLIC:      void *));
 */
int
__qam_mvptr_getpgnos(dbenv, rec, lsnp, notused1, summary)
	DB_ENV *dbenv;
	DBT *rec;
	DB_LSN *lsnp;
	db_recops notused1;
	void *summary;
{
	DB *dbp;
	TXN_RECS *t;
	__qam_mvptr_args *argp;
	u_int32_t ret;

	COMPQUIET(notused1, DB_TXN_ABORT);

	argp = NULL;
	t = (TXN_RECS *)summary;

	if ((ret = __qam_mvptr_read(dbenv, rec->data, &argp)) != 0)
		return (ret);

	if ((ret = __db_fileid_to_db(dbenv, &dbp, argp->fileid, 0)) != 0)
		goto err;

	if ((ret = __rep_check_alloc(dbenv, t, 1)) != 0)
		goto err;

	t->array[t->npages].flags = 0;
	t->array[t->npages].fid = argp->fileid;
	t->array[t->npages].lsn = *lsnp;
	t->array[t->npages].pgdesc.pgno = argp->meta_pgno;
	t->array[t->npages].pgdesc.type = DB_PAGE_LOCK;
	memcpy(t->array[t->npages].pgdesc.fileid, dbp->fileid,
	    DB_FILE_ID_LEN);
	t->npages++;

err:	if (argp != NULL)
		__os_free(dbenv, argp, sizeof(__qam_mvptr_args));
	return (ret);
}

/*
 * PUBLIC: int __qam_mvptr_print __P((DB_ENV *, DBT *, DB_LSN *, db_recops,
 * PUBLIC:      void *));
 */
int
__qam_mvptr_print(dbenv, dbtp, lsnp, notused2, notused3)
	DB_ENV *dbenv;
	DBT *dbtp;
	DB_LSN *lsnp;
	db_recops notused2;
	void *notused3;
{
	__qam_mvptr_args *argp;
	int ret;

	notused2 = DB_TXN_ABORT;
	notused3 = NULL;

	if ((ret = __qam_mvptr_read(dbenv, dbtp->data, &argp)) != 0)
		return (ret);
	(void)printf(
	    "[%lu][%lu]qam_mvptr: rec: %lu txnid %lx prevlsn [%lu][%lu]\n",
	    (u_long)lsnp->file,
	    (u_long)lsnp->offset,
	    (u_long)argp->type,
	    (u_long)argp->txnid->txnid,
	    (u_long)argp->prev_lsn.file,
	    (u_long)argp->prev_lsn.offset);
	(void)printf("\topcode: %lu\n", (u_long)argp->opcode);
	(void)printf("\tfileid: %ld\n", (long)argp->fileid);
	(void)printf("\told_first: %lu\n", (u_long)argp->old_first);
	(void)printf("\tnew_first: %lu\n", (u_long)argp->new_first);
	(void)printf("\told_cur: %lu\n", (u_long)argp->old_cur);
	(void)printf("\tnew_cur: %lu\n", (u_long)argp->new_cur);
	(void)printf("\tmetalsn: [%lu][%lu]\n",
	    (u_long)argp->metalsn.file, (u_long)argp->metalsn.offset);
	(void)printf("\tmeta_pgno: %lu\n", (u_long)argp->meta_pgno);
	(void)printf("\n");
	__os_free(dbenv, argp, 0);
	return (0);
}

/*
 * PUBLIC: int __qam_mvptr_read __P((DB_ENV *, void *, __qam_mvptr_args **));
 */
int
__qam_mvptr_read(dbenv, recbuf, argpp)
	DB_ENV *dbenv;
	void *recbuf;
	__qam_mvptr_args **argpp;
{
	__qam_mvptr_args *argp;
	int ret;
	u_int32_t uinttmp;
	u_int8_t *bp;

	ret = __os_malloc(dbenv, sizeof(__qam_mvptr_args) +
	    sizeof(DB_TXN), &argp);
	if (ret != 0)
		return (ret);
	argp->txnid = (DB_TXN *)&argp[1];

	bp = recbuf;
	memcpy(&argp->type, bp, sizeof(argp->type));
	bp += sizeof(argp->type);

	memcpy(&argp->txnid->txnid,  bp, sizeof(argp->txnid->txnid));
	bp += sizeof(argp->txnid->txnid);

	memcpy(&argp->prev_lsn, bp, sizeof(DB_LSN));
	bp += sizeof(DB_LSN);

	memcpy(&uinttmp, bp, sizeof(uinttmp));
	argp->opcode = (u_int32_t)uinttmp;
	bp += sizeof(uinttmp);

	memcpy(&uinttmp, bp, sizeof(uinttmp));
	argp->fileid = (int32_t)uinttmp;
	bp += sizeof(uinttmp);

	memcpy(&uinttmp, bp, sizeof(uinttmp));
	argp->old_first = (db_recno_t)uinttmp;
	bp += sizeof(uinttmp);

	memcpy(&uinttmp, bp, sizeof(uinttmp));
	argp->new_first = (db_recno_t)uinttmp;
	bp += sizeof(uinttmp);

	memcpy(&uinttmp, bp, sizeof(uinttmp));
	argp->old_cur = (db_recno_t)uinttmp;
	bp += sizeof(uinttmp);

	memcpy(&uinttmp, bp, sizeof(uinttmp));
	argp->new_cur = (db_recno_t)uinttmp;
	bp += sizeof(uinttmp);

	memcpy(&argp->metalsn, bp,  sizeof(argp->metalsn));
	bp += sizeof(argp->metalsn);

	memcpy(&uinttmp, bp, sizeof(uinttmp));
	argp->meta_pgno = (db_pgno_t)uinttmp;
	bp += sizeof(uinttmp);

	*argpp = argp;
	return (0);
}

/*
 * PUBLIC: int __qam_del_log __P((DB_ENV *, DB_TXN *, DB_LSN *, u_int32_t,
 * PUBLIC:      int32_t, DB_LSN *, db_pgno_t, u_int32_t, db_recno_t));
 */
int
__qam_del_log(dbenv, txnid, ret_lsnp, flags,
	fileid, lsn, pgno, indx, recno)
	DB_ENV *dbenv;
	DB_TXN *txnid;
	DB_LSN *ret_lsnp;
	u_int32_t flags;
	int32_t fileid;
	DB_LSN * lsn;
	db_pgno_t pgno;
	u_int32_t indx;
	db_recno_t recno;
{
	DBT logrec;
	DB_LSN *lsnp, null_lsn;
	u_int32_t uinttmp;
	u_int32_t rectype, txn_num;
	int ret;
	u_int8_t *bp;

	rectype = DB_qam_del;
	if (txnid != NULL &&
	    TAILQ_FIRST(&txnid->kids) != NULL &&
	    (ret = __txn_activekids(dbenv, rectype, txnid)) != 0)
		return (ret);
	txn_num = txnid == NULL ? 0 : txnid->txnid;
	if (txnid == NULL) {
		ZERO_LSN(null_lsn);
		lsnp = &null_lsn;
	} else
		lsnp = &txnid->last_lsn;
	logrec.size = sizeof(rectype) + sizeof(txn_num) + sizeof(DB_LSN)
	    + sizeof(u_int32_t)
	    + sizeof(*lsn)
	    + sizeof(u_int32_t)
	    + sizeof(u_int32_t)
	    + sizeof(u_int32_t);
	if ((ret = __os_malloc(dbenv, logrec.size, &logrec.data)) != 0)
		return (ret);

	bp = logrec.data;

	memcpy(bp, &rectype, sizeof(rectype));
	bp += sizeof(rectype);

	memcpy(bp, &txn_num, sizeof(txn_num));
	bp += sizeof(txn_num);

	memcpy(bp, lsnp, sizeof(DB_LSN));
	bp += sizeof(DB_LSN);

	uinttmp = (u_int32_t)fileid;
	memcpy(bp, &uinttmp, sizeof(uinttmp));
	bp += sizeof(uinttmp);

	if (lsn != NULL)
		memcpy(bp, lsn, sizeof(*lsn));
	else
		memset(bp, 0, sizeof(*lsn));
	bp += sizeof(*lsn);

	uinttmp = (u_int32_t)pgno;
	memcpy(bp, &uinttmp, sizeof(uinttmp));
	bp += sizeof(uinttmp);

	uinttmp = (u_int32_t)indx;
	memcpy(bp, &uinttmp, sizeof(uinttmp));
	bp += sizeof(uinttmp);

	uinttmp = (u_int32_t)recno;
	memcpy(bp, &uinttmp, sizeof(uinttmp));
	bp += sizeof(uinttmp);

	DB_ASSERT((u_int32_t)(bp - (u_int8_t *)logrec.data) == logrec.size);
	ret = dbenv->log_put(dbenv, ret_lsnp, (DBT *)&logrec, flags);
	if (txnid != NULL && ret == 0)
		txnid->last_lsn = *ret_lsnp;
#ifdef LOG_DIAGNOSTIC
	if (ret != 0)
		(void)__qam_del_print(dbenv,
		    (DBT *)&logrec, ret_lsnp, NULL, NULL);
#endif
	__os_free(dbenv, logrec.data, logrec.size);
	return (ret);
}

/*
 * PUBLIC: int __qam_del_getpgnos __P((DB_ENV *, DBT *, DB_LSN *, db_recops,
 * PUBLIC:      void *));
 */
int
__qam_del_getpgnos(dbenv, rec, lsnp, notused1, summary)
	DB_ENV *dbenv;
	DBT *rec;
	DB_LSN *lsnp;
	db_recops notused1;
	void *summary;
{
	DB *dbp;
	TXN_RECS *t;
	__qam_del_args *argp;
	u_int32_t ret;

	COMPQUIET(notused1, DB_TXN_ABORT);

	argp = NULL;
	t = (TXN_RECS *)summary;

	if ((ret = __qam_del_read(dbenv, rec->data, &argp)) != 0)
		return (ret);

	if ((ret = __db_fileid_to_db(dbenv, &dbp, argp->fileid, 0)) != 0)
		goto err;

	if ((ret = __rep_check_alloc(dbenv, t, 1)) != 0)
		goto err;

	t->array[t->npages].flags = 0;
	t->array[t->npages].fid = argp->fileid;
	t->array[t->npages].lsn = *lsnp;
	t->array[t->npages].pgdesc.pgno = argp->pgno;
	t->array[t->npages].pgdesc.type = DB_PAGE_LOCK;
	memcpy(t->array[t->npages].pgdesc.fileid, dbp->fileid,
	    DB_FILE_ID_LEN);
	t->npages++;

err:	if (argp != NULL)
		__os_free(dbenv, argp, sizeof(__qam_del_args));
	return (ret);
}

/*
 * PUBLIC: int __qam_del_print __P((DB_ENV *, DBT *, DB_LSN *, db_recops,
 * PUBLIC:      void *));
 */
int
__qam_del_print(dbenv, dbtp, lsnp, notused2, notused3)
	DB_ENV *dbenv;
	DBT *dbtp;
	DB_LSN *lsnp;
	db_recops notused2;
	void *notused3;
{
	__qam_del_args *argp;
	int ret;

	notused2 = DB_TXN_ABORT;
	notused3 = NULL;

	if ((ret = __qam_del_read(dbenv, dbtp->data, &argp)) != 0)
		return (ret);
	(void)printf(
	    "[%lu][%lu]qam_del: rec: %lu txnid %lx prevlsn [%lu][%lu]\n",
	    (u_long)lsnp->file,
	    (u_long)lsnp->offset,
	    (u_long)argp->type,
	    (u_long)argp->txnid->txnid,
	    (u_long)argp->prev_lsn.file,
	    (u_long)argp->prev_lsn.offset);
	(void)printf("\tfileid: %ld\n", (long)argp->fileid);
	(void)printf("\tlsn: [%lu][%lu]\n",
	    (u_long)argp->lsn.file, (u_long)argp->lsn.offset);
	(void)printf("\tpgno: %lu\n", (u_long)argp->pgno);
	(void)printf("\tindx: %lu\n", (u_long)argp->indx);
	(void)printf("\trecno: %lu\n", (u_long)argp->recno);
	(void)printf("\n");
	__os_free(dbenv, argp, 0);
	return (0);
}

/*
 * PUBLIC: int __qam_del_read __P((DB_ENV *, void *, __qam_del_args **));
 */
int
__qam_del_read(dbenv, recbuf, argpp)
	DB_ENV *dbenv;
	void *recbuf;
	__qam_del_args **argpp;
{
	__qam_del_args *argp;
	int ret;
	u_int32_t uinttmp;
	u_int8_t *bp;

	ret = __os_malloc(dbenv, sizeof(__qam_del_args) +
	    sizeof(DB_TXN), &argp);
	if (ret != 0)
		return (ret);
	argp->txnid = (DB_TXN *)&argp[1];

	bp = recbuf;
	memcpy(&argp->type, bp, sizeof(argp->type));
	bp += sizeof(argp->type);

	memcpy(&argp->txnid->txnid,  bp, sizeof(argp->txnid->txnid));
	bp += sizeof(argp->txnid->txnid);

	memcpy(&argp->prev_lsn, bp, sizeof(DB_LSN));
	bp += sizeof(DB_LSN);

	memcpy(&uinttmp, bp, sizeof(uinttmp));
	argp->fileid = (int32_t)uinttmp;
	bp += sizeof(uinttmp);

	memcpy(&argp->lsn, bp,  sizeof(argp->lsn));
	bp += sizeof(argp->lsn);

	memcpy(&uinttmp, bp, sizeof(uinttmp));
	argp->pgno = (db_pgno_t)uinttmp;
	bp += sizeof(uinttmp);

	memcpy(&uinttmp, bp, sizeof(uinttmp));
	argp->indx = (u_int32_t)uinttmp;
	bp += sizeof(uinttmp);

	memcpy(&uinttmp, bp, sizeof(uinttmp));
	argp->recno = (db_recno_t)uinttmp;
	bp += sizeof(uinttmp);

	*argpp = argp;
	return (0);
}

/*
 * PUBLIC: int __qam_add_log __P((DB_ENV *, DB_TXN *, DB_LSN *, u_int32_t,
 * PUBLIC:      int32_t, DB_LSN *, db_pgno_t, u_int32_t, db_recno_t, const DBT *, u_int32_t,
 * PUBLIC:      const DBT *));
 */
int
__qam_add_log(dbenv, txnid, ret_lsnp, flags,
	fileid, lsn, pgno, indx, recno, data,
	vflag, olddata)
	DB_ENV *dbenv;
	DB_TXN *txnid;
	DB_LSN *ret_lsnp;
	u_int32_t flags;
	int32_t fileid;
	DB_LSN * lsn;
	db_pgno_t pgno;
	u_int32_t indx;
	db_recno_t recno;
	const DBT *data;
	u_int32_t vflag;
	const DBT *olddata;
{
	DBT logrec;
	DB_LSN *lsnp, null_lsn;
	u_int32_t zero;
	u_int32_t uinttmp;
	u_int32_t rectype, txn_num;
	int ret;
	u_int8_t *bp;

	rectype = DB_qam_add;
	if (txnid != NULL &&
	    TAILQ_FIRST(&txnid->kids) != NULL &&
	    (ret = __txn_activekids(dbenv, rectype, txnid)) != 0)
		return (ret);
	txn_num = txnid == NULL ? 0 : txnid->txnid;
	if (txnid == NULL) {
		ZERO_LSN(null_lsn);
		lsnp = &null_lsn;
	} else
		lsnp = &txnid->last_lsn;
	logrec.size = sizeof(rectype) + sizeof(txn_num) + sizeof(DB_LSN)
	    + sizeof(u_int32_t)
	    + sizeof(*lsn)
	    + sizeof(u_int32_t)
	    + sizeof(u_int32_t)
	    + sizeof(u_int32_t)
	    + sizeof(u_int32_t) + (data == NULL ? 0 : data->size)
	    + sizeof(u_int32_t)
	    + sizeof(u_int32_t) + (olddata == NULL ? 0 : olddata->size);
	if ((ret = __os_malloc(dbenv, logrec.size, &logrec.data)) != 0)
		return (ret);

	bp = logrec.data;

	memcpy(bp, &rectype, sizeof(rectype));
	bp += sizeof(rectype);

	memcpy(bp, &txn_num, sizeof(txn_num));
	bp += sizeof(txn_num);

	memcpy(bp, lsnp, sizeof(DB_LSN));
	bp += sizeof(DB_LSN);

	uinttmp = (u_int32_t)fileid;
	memcpy(bp, &uinttmp, sizeof(uinttmp));
	bp += sizeof(uinttmp);

	if (lsn != NULL)
		memcpy(bp, lsn, sizeof(*lsn));
	else
		memset(bp, 0, sizeof(*lsn));
	bp += sizeof(*lsn);

	uinttmp = (u_int32_t)pgno;
	memcpy(bp, &uinttmp, sizeof(uinttmp));
	bp += sizeof(uinttmp);

	uinttmp = (u_int32_t)indx;
	memcpy(bp, &uinttmp, sizeof(uinttmp));
	bp += sizeof(uinttmp);

	uinttmp = (u_int32_t)recno;
	memcpy(bp, &uinttmp, sizeof(uinttmp));
	bp += sizeof(uinttmp);

	if (data == NULL) {
		zero = 0;
		memcpy(bp, &zero, sizeof(u_int32_t));
		bp += sizeof(u_int32_t);
	} else {
		memcpy(bp, &data->size, sizeof(data->size));
		bp += sizeof(data->size);
		memcpy(bp, data->data, data->size);
		bp += data->size;
	}

	uinttmp = (u_int32_t)vflag;
	memcpy(bp, &uinttmp, sizeof(uinttmp));
	bp += sizeof(uinttmp);

	if (olddata == NULL) {
		zero = 0;
		memcpy(bp, &zero, sizeof(u_int32_t));
		bp += sizeof(u_int32_t);
	} else {
		memcpy(bp, &olddata->size, sizeof(olddata->size));
		bp += sizeof(olddata->size);
		memcpy(bp, olddata->data, olddata->size);
		bp += olddata->size;
	}

	DB_ASSERT((u_int32_t)(bp - (u_int8_t *)logrec.data) == logrec.size);
	ret = dbenv->log_put(dbenv, ret_lsnp, (DBT *)&logrec, flags);
	if (txnid != NULL && ret == 0)
		txnid->last_lsn = *ret_lsnp;
#ifdef LOG_DIAGNOSTIC
	if (ret != 0)
		(void)__qam_add_print(dbenv,
		    (DBT *)&logrec, ret_lsnp, NULL, NULL);
#endif
	__os_free(dbenv, logrec.data, logrec.size);
	return (ret);
}

/*
 * PUBLIC: int __qam_add_getpgnos __P((DB_ENV *, DBT *, DB_LSN *, db_recops,
 * PUBLIC:      void *));
 */
int
__qam_add_getpgnos(dbenv, rec, lsnp, notused1, summary)
	DB_ENV *dbenv;
	DBT *rec;
	DB_LSN *lsnp;
	db_recops notused1;
	void *summary;
{
	DB *dbp;
	TXN_RECS *t;
	__qam_add_args *argp;
	u_int32_t ret;

	COMPQUIET(notused1, DB_TXN_ABORT);

	argp = NULL;
	t = (TXN_RECS *)summary;

	if ((ret = __qam_add_read(dbenv, rec->data, &argp)) != 0)
		return (ret);

	if ((ret = __db_fileid_to_db(dbenv, &dbp, argp->fileid, 0)) != 0)
		goto err;

	if ((ret = __rep_check_alloc(dbenv, t, 1)) != 0)
		goto err;

	t->array[t->npages].flags = 0;
	t->array[t->npages].fid = argp->fileid;
	t->array[t->npages].lsn = *lsnp;
	t->array[t->npages].pgdesc.pgno = argp->pgno;
	t->array[t->npages].pgdesc.type = DB_PAGE_LOCK;
	memcpy(t->array[t->npages].pgdesc.fileid, dbp->fileid,
	    DB_FILE_ID_LEN);
	t->npages++;

err:	if (argp != NULL)
		__os_free(dbenv, argp, sizeof(__qam_add_args));
	return (ret);
}

/*
 * PUBLIC: int __qam_add_print __P((DB_ENV *, DBT *, DB_LSN *, db_recops,
 * PUBLIC:      void *));
 */
int
__qam_add_print(dbenv, dbtp, lsnp, notused2, notused3)
	DB_ENV *dbenv;
	DBT *dbtp;
	DB_LSN *lsnp;
	db_recops notused2;
	void *notused3;
{
	__qam_add_args *argp;
	u_int32_t i;
	u_int ch;
	int ret;

	notused2 = DB_TXN_ABORT;
	notused3 = NULL;

	if ((ret = __qam_add_read(dbenv, dbtp->data, &argp)) != 0)
		return (ret);
	(void)printf(
	    "[%lu][%lu]qam_add: rec: %lu txnid %lx prevlsn [%lu][%lu]\n",
	    (u_long)lsnp->file,
	    (u_long)lsnp->offset,
	    (u_long)argp->type,
	    (u_long)argp->txnid->txnid,
	    (u_long)argp->prev_lsn.file,
	    (u_long)argp->prev_lsn.offset);
	(void)printf("\tfileid: %ld\n", (long)argp->fileid);
	(void)printf("\tlsn: [%lu][%lu]\n",
	    (u_long)argp->lsn.file, (u_long)argp->lsn.offset);
	(void)printf("\tpgno: %lu\n", (u_long)argp->pgno);
	(void)printf("\tindx: %lu\n", (u_long)argp->indx);
	(void)printf("\trecno: %lu\n", (u_long)argp->recno);
	(void)printf("\tdata: ");
	for (i = 0; i < argp->data.size; i++) {
		ch = ((u_int8_t *)argp->data.data)[i];
		if (isprint(ch) || ch == 0xa)
			(void)putchar(ch);
		else
			(void)printf("%#x ", ch);
	}
	(void)printf("\n");
	(void)printf("\tvflag: %lu\n", (u_long)argp->vflag);
	(void)printf("\tolddata: ");
	for (i = 0; i < argp->olddata.size; i++) {
		ch = ((u_int8_t *)argp->olddata.data)[i];
		if (isprint(ch) || ch == 0xa)
			(void)putchar(ch);
		else
			(void)printf("%#x ", ch);
	}
	(void)printf("\n");
	(void)printf("\n");
	__os_free(dbenv, argp, 0);
	return (0);
}

/*
 * PUBLIC: int __qam_add_read __P((DB_ENV *, void *, __qam_add_args **));
 */
int
__qam_add_read(dbenv, recbuf, argpp)
	DB_ENV *dbenv;
	void *recbuf;
	__qam_add_args **argpp;
{
	__qam_add_args *argp;
	int ret;
	u_int32_t uinttmp;
	u_int8_t *bp;

	ret = __os_malloc(dbenv, sizeof(__qam_add_args) +
	    sizeof(DB_TXN), &argp);
	if (ret != 0)
		return (ret);
	argp->txnid = (DB_TXN *)&argp[1];

	bp = recbuf;
	memcpy(&argp->type, bp, sizeof(argp->type));
	bp += sizeof(argp->type);

	memcpy(&argp->txnid->txnid,  bp, sizeof(argp->txnid->txnid));
	bp += sizeof(argp->txnid->txnid);

	memcpy(&argp->prev_lsn, bp, sizeof(DB_LSN));
	bp += sizeof(DB_LSN);

	memcpy(&uinttmp, bp, sizeof(uinttmp));
	argp->fileid = (int32_t)uinttmp;
	bp += sizeof(uinttmp);

	memcpy(&argp->lsn, bp,  sizeof(argp->lsn));
	bp += sizeof(argp->lsn);

	memcpy(&uinttmp, bp, sizeof(uinttmp));
	argp->pgno = (db_pgno_t)uinttmp;
	bp += sizeof(uinttmp);

	memcpy(&uinttmp, bp, sizeof(uinttmp));
	argp->indx = (u_int32_t)uinttmp;
	bp += sizeof(uinttmp);

	memcpy(&uinttmp, bp, sizeof(uinttmp));
	argp->recno = (db_recno_t)uinttmp;
	bp += sizeof(uinttmp);

	memset(&argp->data, 0, sizeof(argp->data));
	memcpy(&argp->data.size, bp, sizeof(u_int32_t));
	bp += sizeof(u_int32_t);
	argp->data.data = bp;
	bp += argp->data.size;

	memcpy(&uinttmp, bp, sizeof(uinttmp));
	argp->vflag = (u_int32_t)uinttmp;
	bp += sizeof(uinttmp);

	memset(&argp->olddata, 0, sizeof(argp->olddata));
	memcpy(&argp->olddata.size, bp, sizeof(u_int32_t));
	bp += sizeof(u_int32_t);
	argp->olddata.data = bp;
	bp += argp->olddata.size;

	*argpp = argp;
	return (0);
}

/*
 * PUBLIC: int __qam_delete_log __P((DB_ENV *, DB_TXN *, DB_LSN *, u_int32_t,
 * PUBLIC:      const DBT *, DB_LSN *));
 */
int
__qam_delete_log(dbenv, txnid, ret_lsnp, flags,
	name, lsn)
	DB_ENV *dbenv;
	DB_TXN *txnid;
	DB_LSN *ret_lsnp;
	u_int32_t flags;
	const DBT *name;
	DB_LSN * lsn;
{
	DBT logrec;
	DB_LSN *lsnp, null_lsn;
	u_int32_t zero;
	u_int32_t rectype, txn_num;
	int ret;
	u_int8_t *bp;

	rectype = DB_qam_delete;
	if (txnid != NULL &&
	    TAILQ_FIRST(&txnid->kids) != NULL &&
	    (ret = __txn_activekids(dbenv, rectype, txnid)) != 0)
		return (ret);
	txn_num = txnid == NULL ? 0 : txnid->txnid;
	if (txnid == NULL) {
		ZERO_LSN(null_lsn);
		lsnp = &null_lsn;
	} else
		lsnp = &txnid->last_lsn;
	logrec.size = sizeof(rectype) + sizeof(txn_num) + sizeof(DB_LSN)
	    + sizeof(u_int32_t) + (name == NULL ? 0 : name->size)
	    + sizeof(*lsn);
	if ((ret = __os_malloc(dbenv, logrec.size, &logrec.data)) != 0)
		return (ret);

	bp = logrec.data;

	memcpy(bp, &rectype, sizeof(rectype));
	bp += sizeof(rectype);

	memcpy(bp, &txn_num, sizeof(txn_num));
	bp += sizeof(txn_num);

	memcpy(bp, lsnp, sizeof(DB_LSN));
	bp += sizeof(DB_LSN);

	if (name == NULL) {
		zero = 0;
		memcpy(bp, &zero, sizeof(u_int32_t));
		bp += sizeof(u_int32_t);
	} else {
		memcpy(bp, &name->size, sizeof(name->size));
		bp += sizeof(name->size);
		memcpy(bp, name->data, name->size);
		bp += name->size;
	}

	if (lsn != NULL)
		memcpy(bp, lsn, sizeof(*lsn));
	else
		memset(bp, 0, sizeof(*lsn));
	bp += sizeof(*lsn);

	DB_ASSERT((u_int32_t)(bp - (u_int8_t *)logrec.data) == logrec.size);
	ret = dbenv->log_put(dbenv, ret_lsnp, (DBT *)&logrec, flags);
	if (txnid != NULL && ret == 0)
		txnid->last_lsn = *ret_lsnp;
#ifdef LOG_DIAGNOSTIC
	if (ret != 0)
		(void)__qam_delete_print(dbenv,
		    (DBT *)&logrec, ret_lsnp, NULL, NULL);
#endif
	__os_free(dbenv, logrec.data, logrec.size);
	return (ret);
}

/*
 * PUBLIC: int __qam_delete_getpgnos __P((DB_ENV *, DBT *, DB_LSN *, db_recops,
 * PUBLIC:      void *));
 */
int
__qam_delete_getpgnos(dbenv, rec, lsnp, notused1, summary)
	DB_ENV *dbenv;
	DBT *rec;
	DB_LSN *lsnp;
	db_recops notused1;
	void *summary;
{
	TXN_RECS *t;
	int ret;
	COMPQUIET(rec, NULL);
	COMPQUIET(notused1, DB_TXN_ABORT);

	t = (TXN_RECS *)summary;

	if ((ret = __rep_check_alloc(dbenv, t, 1)) != 0)
		return (ret);

	t->array[t->npages].flags = LSN_PAGE_NOLOCK;
	t->array[t->npages].lsn = *lsnp;
	t->array[t->npages].fid = DB_LOGFILEID_INVALID;
	memset(&t->array[t->npages].pgdesc, 0,
	    sizeof(t->array[t->npages].pgdesc));

	t->npages++;

	return (0);
}

/*
 * PUBLIC: int __qam_delete_print __P((DB_ENV *, DBT *, DB_LSN *, db_recops,
 * PUBLIC:      void *));
 */
int
__qam_delete_print(dbenv, dbtp, lsnp, notused2, notused3)
	DB_ENV *dbenv;
	DBT *dbtp;
	DB_LSN *lsnp;
	db_recops notused2;
	void *notused3;
{
	__qam_delete_args *argp;
	u_int32_t i;
	u_int ch;
	int ret;

	notused2 = DB_TXN_ABORT;
	notused3 = NULL;

	if ((ret = __qam_delete_read(dbenv, dbtp->data, &argp)) != 0)
		return (ret);
	(void)printf(
	    "[%lu][%lu]qam_delete: rec: %lu txnid %lx prevlsn [%lu][%lu]\n",
	    (u_long)lsnp->file,
	    (u_long)lsnp->offset,
	    (u_long)argp->type,
	    (u_long)argp->txnid->txnid,
	    (u_long)argp->prev_lsn.file,
	    (u_long)argp->prev_lsn.offset);
	(void)printf("\tname: ");
	for (i = 0; i < argp->name.size; i++) {
		ch = ((u_int8_t *)argp->name.data)[i];
		if (isprint(ch) || ch == 0xa)
			(void)putchar(ch);
		else
			(void)printf("%#x ", ch);
	}
	(void)printf("\n");
	(void)printf("\tlsn: [%lu][%lu]\n",
	    (u_long)argp->lsn.file, (u_long)argp->lsn.offset);
	(void)printf("\n");
	__os_free(dbenv, argp, 0);
	return (0);
}

/*
 * PUBLIC: int __qam_delete_read __P((DB_ENV *, void *, __qam_delete_args **));
 */
int
__qam_delete_read(dbenv, recbuf, argpp)
	DB_ENV *dbenv;
	void *recbuf;
	__qam_delete_args **argpp;
{
	__qam_delete_args *argp;
	int ret;
	u_int8_t *bp;

	ret = __os_malloc(dbenv, sizeof(__qam_delete_args) +
	    sizeof(DB_TXN), &argp);
	if (ret != 0)
		return (ret);
	argp->txnid = (DB_TXN *)&argp[1];

	bp = recbuf;
	memcpy(&argp->type, bp, sizeof(argp->type));
	bp += sizeof(argp->type);

	memcpy(&argp->txnid->txnid,  bp, sizeof(argp->txnid->txnid));
	bp += sizeof(argp->txnid->txnid);

	memcpy(&argp->prev_lsn, bp, sizeof(DB_LSN));
	bp += sizeof(DB_LSN);

	memset(&argp->name, 0, sizeof(argp->name));
	memcpy(&argp->name.size, bp, sizeof(u_int32_t));
	bp += sizeof(u_int32_t);
	argp->name.data = bp;
	bp += argp->name.size;

	memcpy(&argp->lsn, bp,  sizeof(argp->lsn));
	bp += sizeof(argp->lsn);

	*argpp = argp;
	return (0);
}

/*
 * PUBLIC: int __qam_rename_log __P((DB_ENV *, DB_TXN *, DB_LSN *, u_int32_t,
 * PUBLIC:      const DBT *, const DBT *));
 */
int
__qam_rename_log(dbenv, txnid, ret_lsnp, flags,
	name, newname)
	DB_ENV *dbenv;
	DB_TXN *txnid;
	DB_LSN *ret_lsnp;
	u_int32_t flags;
	const DBT *name;
	const DBT *newname;
{
	DBT logrec;
	DB_LSN *lsnp, null_lsn;
	u_int32_t zero;
	u_int32_t rectype, txn_num;
	int ret;
	u_int8_t *bp;

	rectype = DB_qam_rename;
	if (txnid != NULL &&
	    TAILQ_FIRST(&txnid->kids) != NULL &&
	    (ret = __txn_activekids(dbenv, rectype, txnid)) != 0)
		return (ret);
	txn_num = txnid == NULL ? 0 : txnid->txnid;
	if (txnid == NULL) {
		ZERO_LSN(null_lsn);
		lsnp = &null_lsn;
	} else
		lsnp = &txnid->last_lsn;
	logrec.size = sizeof(rectype) + sizeof(txn_num) + sizeof(DB_LSN)
	    + sizeof(u_int32_t) + (name == NULL ? 0 : name->size)
	    + sizeof(u_int32_t) + (newname == NULL ? 0 : newname->size);
	if ((ret = __os_malloc(dbenv, logrec.size, &logrec.data)) != 0)
		return (ret);

	bp = logrec.data;

	memcpy(bp, &rectype, sizeof(rectype));
	bp += sizeof(rectype);

	memcpy(bp, &txn_num, sizeof(txn_num));
	bp += sizeof(txn_num);

	memcpy(bp, lsnp, sizeof(DB_LSN));
	bp += sizeof(DB_LSN);

	if (name == NULL) {
		zero = 0;
		memcpy(bp, &zero, sizeof(u_int32_t));
		bp += sizeof(u_int32_t);
	} else {
		memcpy(bp, &name->size, sizeof(name->size));
		bp += sizeof(name->size);
		memcpy(bp, name->data, name->size);
		bp += name->size;
	}

	if (newname == NULL) {
		zero = 0;
		memcpy(bp, &zero, sizeof(u_int32_t));
		bp += sizeof(u_int32_t);
	} else {
		memcpy(bp, &newname->size, sizeof(newname->size));
		bp += sizeof(newname->size);
		memcpy(bp, newname->data, newname->size);
		bp += newname->size;
	}

	DB_ASSERT((u_int32_t)(bp - (u_int8_t *)logrec.data) == logrec.size);
	ret = dbenv->log_put(dbenv, ret_lsnp, (DBT *)&logrec, flags);
	if (txnid != NULL && ret == 0)
		txnid->last_lsn = *ret_lsnp;
#ifdef LOG_DIAGNOSTIC
	if (ret != 0)
		(void)__qam_rename_print(dbenv,
		    (DBT *)&logrec, ret_lsnp, NULL, NULL);
#endif
	__os_free(dbenv, logrec.data, logrec.size);
	return (ret);
}

/*
 * PUBLIC: int __qam_rename_getpgnos __P((DB_ENV *, DBT *, DB_LSN *, db_recops,
 * PUBLIC:      void *));
 */
int
__qam_rename_getpgnos(dbenv, rec, lsnp, notused1, summary)
	DB_ENV *dbenv;
	DBT *rec;
	DB_LSN *lsnp;
	db_recops notused1;
	void *summary;
{
	TXN_RECS *t;
	int ret;
	COMPQUIET(rec, NULL);
	COMPQUIET(notused1, DB_TXN_ABORT);

	t = (TXN_RECS *)summary;

	if ((ret = __rep_check_alloc(dbenv, t, 1)) != 0)
		return (ret);

	t->array[t->npages].flags = LSN_PAGE_NOLOCK;
	t->array[t->npages].lsn = *lsnp;
	t->array[t->npages].fid = DB_LOGFILEID_INVALID;
	memset(&t->array[t->npages].pgdesc, 0,
	    sizeof(t->array[t->npages].pgdesc));

	t->npages++;

	return (0);
}

/*
 * PUBLIC: int __qam_rename_print __P((DB_ENV *, DBT *, DB_LSN *, db_recops,
 * PUBLIC:      void *));
 */
int
__qam_rename_print(dbenv, dbtp, lsnp, notused2, notused3)
	DB_ENV *dbenv;
	DBT *dbtp;
	DB_LSN *lsnp;
	db_recops notused2;
	void *notused3;
{
	__qam_rename_args *argp;
	u_int32_t i;
	u_int ch;
	int ret;

	notused2 = DB_TXN_ABORT;
	notused3 = NULL;

	if ((ret = __qam_rename_read(dbenv, dbtp->data, &argp)) != 0)
		return (ret);
	(void)printf(
	    "[%lu][%lu]qam_rename: rec: %lu txnid %lx prevlsn [%lu][%lu]\n",
	    (u_long)lsnp->file,
	    (u_long)lsnp->offset,
	    (u_long)argp->type,
	    (u_long)argp->txnid->txnid,
	    (u_long)argp->prev_lsn.file,
	    (u_long)argp->prev_lsn.offset);
	(void)printf("\tname: ");
	for (i = 0; i < argp->name.size; i++) {
		ch = ((u_int8_t *)argp->name.data)[i];
		if (isprint(ch) || ch == 0xa)
			(void)putchar(ch);
		else
			(void)printf("%#x ", ch);
	}
	(void)printf("\n");
	(void)printf("\tnewname: ");
	for (i = 0; i < argp->newname.size; i++) {
		ch = ((u_int8_t *)argp->newname.data)[i];
		if (isprint(ch) || ch == 0xa)
			(void)putchar(ch);
		else
			(void)printf("%#x ", ch);
	}
	(void)printf("\n");
	(void)printf("\n");
	__os_free(dbenv, argp, 0);
	return (0);
}

/*
 * PUBLIC: int __qam_rename_read __P((DB_ENV *, void *, __qam_rename_args **));
 */
int
__qam_rename_read(dbenv, recbuf, argpp)
	DB_ENV *dbenv;
	void *recbuf;
	__qam_rename_args **argpp;
{
	__qam_rename_args *argp;
	int ret;
	u_int8_t *bp;

	ret = __os_malloc(dbenv, sizeof(__qam_rename_args) +
	    sizeof(DB_TXN), &argp);
	if (ret != 0)
		return (ret);
	argp->txnid = (DB_TXN *)&argp[1];

	bp = recbuf;
	memcpy(&argp->type, bp, sizeof(argp->type));
	bp += sizeof(argp->type);

	memcpy(&argp->txnid->txnid,  bp, sizeof(argp->txnid->txnid));
	bp += sizeof(argp->txnid->txnid);

	memcpy(&argp->prev_lsn, bp, sizeof(DB_LSN));
	bp += sizeof(DB_LSN);

	memset(&argp->name, 0, sizeof(argp->name));
	memcpy(&argp->name.size, bp, sizeof(u_int32_t));
	bp += sizeof(u_int32_t);
	argp->name.data = bp;
	bp += argp->name.size;

	memset(&argp->newname, 0, sizeof(argp->newname));
	memcpy(&argp->newname.size, bp, sizeof(u_int32_t));
	bp += sizeof(u_int32_t);
	argp->newname.data = bp;
	bp += argp->newname.size;

	*argpp = argp;
	return (0);
}

/*
 * PUBLIC: int __qam_delext_log __P((DB_ENV *, DB_TXN *, DB_LSN *, u_int32_t,
 * PUBLIC:      int32_t, DB_LSN *, db_pgno_t, u_int32_t, db_recno_t, const DBT *));
 */
int
__qam_delext_log(dbenv, txnid, ret_lsnp, flags,
	fileid, lsn, pgno, indx, recno, data)
	DB_ENV *dbenv;
	DB_TXN *txnid;
	DB_LSN *ret_lsnp;
	u_int32_t flags;
	int32_t fileid;
	DB_LSN * lsn;
	db_pgno_t pgno;
	u_int32_t indx;
	db_recno_t recno;
	const DBT *data;
{
	DBT logrec;
	DB_LSN *lsnp, null_lsn;
	u_int32_t zero;
	u_int32_t uinttmp;
	u_int32_t rectype, txn_num;
	int ret;
	u_int8_t *bp;

	rectype = DB_qam_delext;
	if (txnid != NULL &&
	    TAILQ_FIRST(&txnid->kids) != NULL &&
	    (ret = __txn_activekids(dbenv, rectype, txnid)) != 0)
		return (ret);
	txn_num = txnid == NULL ? 0 : txnid->txnid;
	if (txnid == NULL) {
		ZERO_LSN(null_lsn);
		lsnp = &null_lsn;
	} else
		lsnp = &txnid->last_lsn;
	logrec.size = sizeof(rectype) + sizeof(txn_num) + sizeof(DB_LSN)
	    + sizeof(u_int32_t)
	    + sizeof(*lsn)
	    + sizeof(u_int32_t)
	    + sizeof(u_int32_t)
	    + sizeof(u_int32_t)
	    + sizeof(u_int32_t) + (data == NULL ? 0 : data->size);
	if ((ret = __os_malloc(dbenv, logrec.size, &logrec.data)) != 0)
		return (ret);

	bp = logrec.data;

	memcpy(bp, &rectype, sizeof(rectype));
	bp += sizeof(rectype);

	memcpy(bp, &txn_num, sizeof(txn_num));
	bp += sizeof(txn_num);

	memcpy(bp, lsnp, sizeof(DB_LSN));
	bp += sizeof(DB_LSN);

	uinttmp = (u_int32_t)fileid;
	memcpy(bp, &uinttmp, sizeof(uinttmp));
	bp += sizeof(uinttmp);

	if (lsn != NULL)
		memcpy(bp, lsn, sizeof(*lsn));
	else
		memset(bp, 0, sizeof(*lsn));
	bp += sizeof(*lsn);

	uinttmp = (u_int32_t)pgno;
	memcpy(bp, &uinttmp, sizeof(uinttmp));
	bp += sizeof(uinttmp);

	uinttmp = (u_int32_t)indx;
	memcpy(bp, &uinttmp, sizeof(uinttmp));
	bp += sizeof(uinttmp);

	uinttmp = (u_int32_t)recno;
	memcpy(bp, &uinttmp, sizeof(uinttmp));
	bp += sizeof(uinttmp);

	if (data == NULL) {
		zero = 0;
		memcpy(bp, &zero, sizeof(u_int32_t));
		bp += sizeof(u_int32_t);
	} else {
		memcpy(bp, &data->size, sizeof(data->size));
		bp += sizeof(data->size);
		memcpy(bp, data->data, data->size);
		bp += data->size;
	}

	DB_ASSERT((u_int32_t)(bp - (u_int8_t *)logrec.data) == logrec.size);
	ret = dbenv->log_put(dbenv, ret_lsnp, (DBT *)&logrec, flags);
	if (txnid != NULL && ret == 0)
		txnid->last_lsn = *ret_lsnp;
#ifdef LOG_DIAGNOSTIC
	if (ret != 0)
		(void)__qam_delext_print(dbenv,
		    (DBT *)&logrec, ret_lsnp, NULL, NULL);
#endif
	__os_free(dbenv, logrec.data, logrec.size);
	return (ret);
}

/*
 * PUBLIC: int __qam_delext_getpgnos __P((DB_ENV *, DBT *, DB_LSN *, db_recops,
 * PUBLIC:      void *));
 */
int
__qam_delext_getpgnos(dbenv, rec, lsnp, notused1, summary)
	DB_ENV *dbenv;
	DBT *rec;
	DB_LSN *lsnp;
	db_recops notused1;
	void *summary;
{
	TXN_RECS *t;
	int ret;
	COMPQUIET(rec, NULL);
	COMPQUIET(notused1, DB_TXN_ABORT);

	t = (TXN_RECS *)summary;

	if ((ret = __rep_check_alloc(dbenv, t, 1)) != 0)
		return (ret);

	t->array[t->npages].flags = LSN_PAGE_NOLOCK;
	t->array[t->npages].lsn = *lsnp;
	t->array[t->npages].fid = DB_LOGFILEID_INVALID;
	memset(&t->array[t->npages].pgdesc, 0,
	    sizeof(t->array[t->npages].pgdesc));

	t->npages++;

	return (0);
}

/*
 * PUBLIC: int __qam_delext_print __P((DB_ENV *, DBT *, DB_LSN *, db_recops,
 * PUBLIC:      void *));
 */
int
__qam_delext_print(dbenv, dbtp, lsnp, notused2, notused3)
	DB_ENV *dbenv;
	DBT *dbtp;
	DB_LSN *lsnp;
	db_recops notused2;
	void *notused3;
{
	__qam_delext_args *argp;
	u_int32_t i;
	u_int ch;
	int ret;

	notused2 = DB_TXN_ABORT;
	notused3 = NULL;

	if ((ret = __qam_delext_read(dbenv, dbtp->data, &argp)) != 0)
		return (ret);
	(void)printf(
	    "[%lu][%lu]qam_delext: rec: %lu txnid %lx prevlsn [%lu][%lu]\n",
	    (u_long)lsnp->file,
	    (u_long)lsnp->offset,
	    (u_long)argp->type,
	    (u_long)argp->txnid->txnid,
	    (u_long)argp->prev_lsn.file,
	    (u_long)argp->prev_lsn.offset);
	(void)printf("\tfileid: %ld\n", (long)argp->fileid);
	(void)printf("\tlsn: [%lu][%lu]\n",
	    (u_long)argp->lsn.file, (u_long)argp->lsn.offset);
	(void)printf("\tpgno: %lu\n", (u_long)argp->pgno);
	(void)printf("\tindx: %lu\n", (u_long)argp->indx);
	(void)printf("\trecno: %lu\n", (u_long)argp->recno);
	(void)printf("\tdata: ");
	for (i = 0; i < argp->data.size; i++) {
		ch = ((u_int8_t *)argp->data.data)[i];
		if (isprint(ch) || ch == 0xa)
			(void)putchar(ch);
		else
			(void)printf("%#x ", ch);
	}
	(void)printf("\n");
	(void)printf("\n");
	__os_free(dbenv, argp, 0);
	return (0);
}

/*
 * PUBLIC: int __qam_delext_read __P((DB_ENV *, void *, __qam_delext_args **));
 */
int
__qam_delext_read(dbenv, recbuf, argpp)
	DB_ENV *dbenv;
	void *recbuf;
	__qam_delext_args **argpp;
{
	__qam_delext_args *argp;
	int ret;
	u_int32_t uinttmp;
	u_int8_t *bp;

	ret = __os_malloc(dbenv, sizeof(__qam_delext_args) +
	    sizeof(DB_TXN), &argp);
	if (ret != 0)
		return (ret);
	argp->txnid = (DB_TXN *)&argp[1];

	bp = recbuf;
	memcpy(&argp->type, bp, sizeof(argp->type));
	bp += sizeof(argp->type);

	memcpy(&argp->txnid->txnid,  bp, sizeof(argp->txnid->txnid));
	bp += sizeof(argp->txnid->txnid);

	memcpy(&argp->prev_lsn, bp, sizeof(DB_LSN));
	bp += sizeof(DB_LSN);

	memcpy(&uinttmp, bp, sizeof(uinttmp));
	argp->fileid = (int32_t)uinttmp;
	bp += sizeof(uinttmp);

	memcpy(&argp->lsn, bp,  sizeof(argp->lsn));
	bp += sizeof(argp->lsn);

	memcpy(&uinttmp, bp, sizeof(uinttmp));
	argp->pgno = (db_pgno_t)uinttmp;
	bp += sizeof(uinttmp);

	memcpy(&uinttmp, bp, sizeof(uinttmp));
	argp->indx = (u_int32_t)uinttmp;
	bp += sizeof(uinttmp);

	memcpy(&uinttmp, bp, sizeof(uinttmp));
	argp->recno = (db_recno_t)uinttmp;
	bp += sizeof(uinttmp);

	memset(&argp->data, 0, sizeof(argp->data));
	memcpy(&argp->data.size, bp, sizeof(u_int32_t));
	bp += sizeof(u_int32_t);
	argp->data.data = bp;
	bp += argp->data.size;

	*argpp = argp;
	return (0);
}

/*
 * PUBLIC: int __qam_init_print __P((DB_ENV *, int (***)(DB_ENV *, DBT *,
 * PUBLIC:      DB_LSN *, db_recops, void *), size_t *));
 */
int
__qam_init_print(dbenv, dtabp, dtabsizep)
	DB_ENV *dbenv;
	int (***dtabp)__P((DB_ENV *, DBT *, DB_LSN *, db_recops, void *));
	size_t *dtabsizep;
{
	int ret;

	if ((ret = __db_add_recovery(dbenv, dtabp, dtabsizep,
	    __qam_incfirst_print, DB_qam_incfirst)) != 0)
		return (ret);
	if ((ret = __db_add_recovery(dbenv, dtabp, dtabsizep,
	    __qam_mvptr_print, DB_qam_mvptr)) != 0)
		return (ret);
	if ((ret = __db_add_recovery(dbenv, dtabp, dtabsizep,
	    __qam_del_print, DB_qam_del)) != 0)
		return (ret);
	if ((ret = __db_add_recovery(dbenv, dtabp, dtabsizep,
	    __qam_add_print, DB_qam_add)) != 0)
		return (ret);
	if ((ret = __db_add_recovery(dbenv, dtabp, dtabsizep,
	    __qam_delete_print, DB_qam_delete)) != 0)
		return (ret);
	if ((ret = __db_add_recovery(dbenv, dtabp, dtabsizep,
	    __qam_rename_print, DB_qam_rename)) != 0)
		return (ret);
	if ((ret = __db_add_recovery(dbenv, dtabp, dtabsizep,
	    __qam_delext_print, DB_qam_delext)) != 0)
		return (ret);
	return (0);
}

/*
 * PUBLIC: int __qam_init_getpgnos __P((DB_ENV *, int (***)(DB_ENV *, DBT *,
 * PUBLIC:      DB_LSN *, db_recops, void *), size_t *));
 */
int
__qam_init_getpgnos(dbenv, dtabp, dtabsizep)
	DB_ENV *dbenv;
	int (***dtabp)__P((DB_ENV *, DBT *, DB_LSN *, db_recops, void *));
	size_t *dtabsizep;
{
	int ret;

	if ((ret = __db_add_recovery(dbenv, dtabp, dtabsizep,
	    __qam_incfirst_getpgnos, DB_qam_incfirst)) != 0)
		return (ret);
	if ((ret = __db_add_recovery(dbenv, dtabp, dtabsizep,
	    __qam_mvptr_getpgnos, DB_qam_mvptr)) != 0)
		return (ret);
	if ((ret = __db_add_recovery(dbenv, dtabp, dtabsizep,
	    __qam_del_getpgnos, DB_qam_del)) != 0)
		return (ret);
	if ((ret = __db_add_recovery(dbenv, dtabp, dtabsizep,
	    __qam_add_getpgnos, DB_qam_add)) != 0)
		return (ret);
	if ((ret = __db_add_recovery(dbenv, dtabp, dtabsizep,
	    __qam_delete_getpgnos, DB_qam_delete)) != 0)
		return (ret);
	if ((ret = __db_add_recovery(dbenv, dtabp, dtabsizep,
	    __qam_rename_getpgnos, DB_qam_rename)) != 0)
		return (ret);
	if ((ret = __db_add_recovery(dbenv, dtabp, dtabsizep,
	    __qam_delext_getpgnos, DB_qam_delext)) != 0)
		return (ret);
	return (0);
}

/*
 * PUBLIC: int __qam_init_recover __P((DB_ENV *));
 */
int
__qam_init_recover(dbenv)
	DB_ENV *dbenv;
{
	int ret;

	if ((ret = __db_add_recovery(dbenv, &dbenv->dtab, &dbenv->dtab_size,
	    __qam_incfirst_recover, DB_qam_incfirst)) != 0)
		return (ret);
	if ((ret = __db_add_recovery(dbenv, &dbenv->dtab, &dbenv->dtab_size,
	    __qam_mvptr_recover, DB_qam_mvptr)) != 0)
		return (ret);
	if ((ret = __db_add_recovery(dbenv, &dbenv->dtab, &dbenv->dtab_size,
	    __qam_del_recover, DB_qam_del)) != 0)
		return (ret);
	if ((ret = __db_add_recovery(dbenv, &dbenv->dtab, &dbenv->dtab_size,
	    __qam_add_recover, DB_qam_add)) != 0)
		return (ret);
	if ((ret = __db_add_recovery(dbenv, &dbenv->dtab, &dbenv->dtab_size,
	    __qam_delete_recover, DB_qam_delete)) != 0)
		return (ret);
	if ((ret = __db_add_recovery(dbenv, &dbenv->dtab, &dbenv->dtab_size,
	    __qam_rename_recover, DB_qam_rename)) != 0)
		return (ret);
	if ((ret = __db_add_recovery(dbenv, &dbenv->dtab, &dbenv->dtab_size,
	    __qam_delext_recover, DB_qam_delext)) != 0)
		return (ret);
	return (0);
}
