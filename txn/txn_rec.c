/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */
/*
 * Copyright (c) 1996
 *	The President and Fellows of Harvard University.  All rights reserved.
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
 */

#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)txn_rec.c	11.2 (Sleepycat) 9/9/99";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <errno.h>
#endif

#include "db_int.h"
#include "db_page.h"
#include "txn.h"
#include "db_am.h"

/*
 * PUBLIC: int __txn_regop_recover
 * PUBLIC:    __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
 *
 * These records are only ever written for commits.
 */
int
__txn_regop_recover(dbenv, dbtp, lsnp, redo, info)
	DB_ENV *dbenv;
	DBT *dbtp;
	DB_LSN *lsnp;
	int redo;
	void *info;
{
	__txn_regop_args *argp;
	int ret;

#ifdef DEBUG_RECOVER
	(void)__txn_regop_print(dbenv, dbtp, lsnp, redo, info);
#endif
	COMPQUIET(redo, 0);
	COMPQUIET(dbenv, NULL);

	if ((ret = __txn_regop_read(dbtp->data, &argp)) != 0)
		return (ret);

	if (argp->opcode != TXN_COMMIT)
		ret = EINVAL;
	else
		if (__db_txnlist_find(info, argp->txnid->txnid) == DB_NOTFOUND)
			ret = __db_txnlist_add(info, argp->txnid->txnid);

	if (ret == 0)
		*lsnp = argp->prev_lsn;
	__os_free(argp, 0);

	return (ret);
}

/*
 * PUBLIC: int __txn_xa_regop_recover
 * PUBLIC:    __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
 *
 * These records are only ever written for prepares.
 */
int
__txn_xa_regop_recover(dbenv, dbtp, lsnp, redo, info)
	DB_ENV *dbenv;
	DBT *dbtp;
	DB_LSN *lsnp;
	int redo;
	void *info;
{
	__txn_xa_regop_args *argp;
	int ret;

#ifdef DEBUG_RECOVER
	(void)__txn_xa_regop_print(dbenv, dbtp, lsnp, redo, info);
#endif
	COMPQUIET(redo, 0);
	COMPQUIET(dbenv, NULL);

	if ((ret = __txn_xa_regop_read(dbtp->data, &argp)) != 0)
		return (ret);

	if (argp->opcode != TXN_PREPARE)
		ret = EINVAL;
	else
		/* Call __db_txnlist_find so that we update the maxid. */
		(void)__db_txnlist_find(info, argp->txnid->txnid);

	if (ret == 0)
		*lsnp = argp->prev_lsn;
	__os_free(argp, 0);

	return (ret);
}

/*
 * PUBLIC: int __txn_ckp_recover __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
 */
int
__txn_ckp_recover(dbenv, dbtp, lsnp, redo, info)
	DB_ENV *dbenv;
	DBT *dbtp;
	DB_LSN *lsnp;
	int redo;
	void *info;
{
	__txn_ckp_args *argp;
	int ret;

#ifdef DEBUG_RECOVER
	__txn_ckp_print(dbenv, dbtp, lsnp, redo, info);
#endif
	COMPQUIET(dbenv, NULL);

	if ((ret = __txn_ckp_read(dbtp->data, &argp)) != 0)
		return (ret);

	/*
	 * Check for 'restart' checkpoint record.  This occurs when the
	 * checkpoint lsn is equal to the lsn of the checkpoint record
	 * and means that we could set the transaction ID back to 1, so
	 * that we don't exhaust the transaction ID name space.
	 */
	if (argp->ckp_lsn.file == lsnp->file &&
	    argp->ckp_lsn.offset == lsnp->offset)
		__db_txnlist_gen(info, redo ? -1 : 1);

	*lsnp = argp->last_ckp;
	__os_free(argp, 0);
	return (DB_TXN_CKP);
}

/*
 * __txn_child_recover
 *	Recover a commit record for a child transaction.
 *
 * PUBLIC: int __txn_child_recover
 * PUBLIC:    __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
 */
int
__txn_child_recover(dbenv, dbtp, lsnp, redo, info)
	DB_ENV *dbenv;
	DBT *dbtp;
	DB_LSN *lsnp;
	int redo;
	void *info;
{
	__txn_child_args *argp;
	int ret;

#ifdef DEBUG_RECOVER
	(void)__txn_child_print(dbenv, dbtp, lsnp, redo, info);
#endif
	COMPQUIET(redo, 0);
	COMPQUIET(dbenv, NULL);

	if ((ret = __txn_child_read(dbtp->data, &argp)) != 0)
		return (ret);

	/*
	 * We count the child as committed only if its parent committed.
	 * So, if we are not yet in the transaction list, but our parent
	 * is, then we should go ahead and commit.
	 */
	if (argp->opcode != TXN_COMMIT)
		ret = EINVAL;
	else
		if (__db_txnlist_find(info, argp->parent) == 0 &&
		    __db_txnlist_find(info, argp->txnid->txnid) == DB_NOTFOUND)
			ret = __db_txnlist_add(info, argp->txnid->txnid);

	if (ret == 0)
		*lsnp = argp->prev_lsn;
	__os_free(argp, 0);

	return (ret);
}
