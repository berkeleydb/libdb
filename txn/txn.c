/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 1997, 1998, 1999, 2000
 *	Sleepycat Software.  All rights reserved.
 */
/*
 * Copyright (c) 1995, 1996
 *	The President and Fellows of Harvard University.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Margo Seltzer.
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
static const char revid[] = "$Id: txn.c,v 11.35.2.1 2000/07/05 18:58:57 bostic Exp $";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <time.h>
#endif
#endif

#include <errno.h>
#include <string.h>
#endif

#ifdef  HAVE_RPC
#include "db_server.h"
#endif

#include "db_int.h"
#include "db_shash.h"
#include "txn.h"
#include "lock.h"
#include "log.h"
#include "db_dispatch.h"

#ifdef HAVE_RPC
#include "gen_client_ext.h"
#include "rpc_client_ext.h"
#endif

static int  __txn_begin __P((DB_TXN *));
static int  __txn_check_running __P((const DB_TXN *, TXN_DETAIL **));
static int  __txn_count __P((DB_TXN *));
static void __txn_freekids __P((DB_TXN *));
static void __txn_lsn __P((DB_TXN *, DB_LSN **));
static int  __txn_makefamily __P((DB_ENV *, DB_TXN *, int *, DB_LSN **));
static int  __txn_undo __P((DB_TXN *));

#define	TXN_BUBBLE(AP, MAX) {						\
	int __j;							\
	DB_LSN __tmp;							\
									\
	for (__j = 0; __j < MAX - 1; __j++)				\
		if (log_compare(&AP[__j], &AP[__j + 1]) < 0) {		\
			__tmp = AP[__j];				\
			AP[__j] = AP[__j + 1];				\
			AP[__j + 1] = __tmp;				\
		}							\
}

/*
 * txn_begin --
 *	This is a wrapper to the actual begin process.  Normal txn_begin()
 * allocates a DB_TXN structure for the caller, while txn_xa_begin() does
 * not.  Other than that, both call into the common __txn_begin code().
 *
 * Internally, we use TXN_DETAIL structures, but the DB_TXN structure
 * provides access to the transaction ID and the offset in the transaction
 * region of the TXN_DETAIL structure.
 */
int
txn_begin(dbenv, parent, txnpp, flags)
	DB_ENV *dbenv;
	DB_TXN *parent, **txnpp;
	u_int32_t flags;
{
	DB_TXN *txn;
	int ret;

#ifdef HAVE_RPC
	if (F_ISSET(dbenv, DB_ENV_RPCCLIENT))
		return (__dbcl_txn_begin(dbenv, parent, txnpp, flags));
#endif

	PANIC_CHECK(dbenv);
	ENV_REQUIRES_CONFIG(dbenv, dbenv->tx_handle, DB_INIT_TXN);

	if ((ret = __db_fchk(dbenv,
	    "txn_begin", flags,
	    DB_TXN_NOWAIT | DB_TXN_NOSYNC | DB_TXN_SYNC)) != 0)
		return (ret);
	if ((ret = __db_fcchk(dbenv,
	    "txn_begin", flags, DB_TXN_NOSYNC, DB_TXN_SYNC)) != 0)
		return (ret);

	if ((ret = __os_calloc(dbenv, 1, sizeof(DB_TXN), &txn)) != 0)
		return (ret);

	txn->mgrp = dbenv->tx_handle;
	txn->parent = parent;
	TAILQ_INIT(&txn->kids);
	txn->flags = TXN_MALLOC;
	if (LF_ISSET(DB_TXN_NOSYNC))
		F_SET(txn, TXN_NOSYNC);
	if (LF_ISSET(DB_TXN_SYNC))
		F_SET(txn, TXN_SYNC);
	if (LF_ISSET(DB_TXN_NOWAIT))
		F_SET(txn, TXN_NOWAIT);

	if ((ret = __txn_begin(txn)) != 0) {
		__os_free(txn, sizeof(DB_TXN));
		txn = NULL;
	}

	if (txn != NULL && parent != NULL)
		TAILQ_INSERT_HEAD(&parent->kids, txn, klinks);

	*txnpp = txn;
	return (ret);
}

/*
 * __txn_xa_begin --
 *	XA version of txn_begin.
 *
 * PUBLIC: int __txn_xa_begin __P((DB_ENV *, DB_TXN *));
 */
int
__txn_xa_begin(dbenv, txn)
	DB_ENV *dbenv;
	DB_TXN *txn;
{
	PANIC_CHECK(dbenv);

	memset(txn, 0, sizeof(DB_TXN));

	txn->mgrp = dbenv->tx_handle;

	return (__txn_begin(txn));
}

/*
 * __txn_begin --
 *	Normal DB version of txn_begin.
 */
static int
__txn_begin(txn)
	DB_TXN *txn;
{
	DB_ENV *dbenv;
	DB_LSN begin_lsn;
	DB_TXNMGR *mgr;
	DB_TXNREGION *region;
	TXN_DETAIL *td;
	size_t off;
	u_int32_t id;
	int ret;

	mgr = txn->mgrp;
	dbenv = mgr->dbenv;
	region = mgr->reginfo.primary;

	/*
	 * We do not have to write begin records (and if we do not, then we
	 * need never write records for read-only transactions).  However,
	 * we do need to find the current LSN so that we can store it in the
	 * transaction structure, so we can know where to take checkpoints.
	 */
	if (LOGGING_ON(dbenv) &&
	    (ret = log_put(dbenv, &begin_lsn, NULL, DB_CURLSN)) != 0)
		goto err2;

	R_LOCK(dbenv, &mgr->reginfo);

	/* Make sure that last_txnid is not going to wrap around. */
	if (region->last_txnid == TXN_INVALID) {
		__db_err(dbenv, "txn_begin: %s  %s",
		    "Transaction ID wrapping.",
		    "Snapshot your database and start a new log.");
		ret = EINVAL;
		goto err1;
	}

	/* Allocate a new transaction detail structure. */
	if ((ret =
	    __db_shalloc(mgr->reginfo.addr, sizeof(TXN_DETAIL), 0, &td)) != 0) {
		__db_err(dbenv,
		     "Unable to allocate memory for transaction detail");
		goto err1;
	}

	/* Place transaction on active transaction list. */
	SH_TAILQ_INSERT_HEAD(&region->active_txn, td, links, __txn_detail);

	id = ++region->last_txnid;
	++region->nbegins;
	if (++region->nactive > region->maxnactive)
		region->maxnactive = region->nactive;

	td->txnid = id;
	td->begin_lsn = begin_lsn;
	ZERO_LSN(td->last_lsn);
	td->status = TXN_RUNNING;
	if (txn->parent != NULL)
		td->parent = txn->parent->off;
	else
		td->parent = INVALID_ROFF;

	off = R_OFFSET(&mgr->reginfo, td);
	R_UNLOCK(dbenv, &mgr->reginfo);

	ZERO_LSN(txn->last_lsn);
	txn->txnid = id;
	txn->off = off;

	/*
	 * If this is a transaction family, we must link the child to the
	 * maximal grandparent in the lock table for deadlock detection.
	 */
	if (txn->parent != NULL && LOCKING_ON(dbenv))
		if ((ret = __lock_addfamilylocker(dbenv,
		    txn->parent->txnid, txn->txnid)) != 0)
			goto err2;

	if (F_ISSET(txn, TXN_MALLOC)) {
		MUTEX_THREAD_LOCK(mgr->mutexp);
		TAILQ_INSERT_TAIL(&mgr->txn_chain, txn, links);
		MUTEX_THREAD_UNLOCK(mgr->mutexp);
	}

	return (0);

err1:	R_UNLOCK(dbenv, &mgr->reginfo);

err2:	return (ret);
}

/*
 * txn_commit --
 *	Commit a transaction.
 */
int
txn_commit(txnp, flags)
	DB_TXN *txnp;
	u_int32_t flags;
{
	DB_ENV *dbenv;
	DB_TXN *kids;
	DB_TXNMGR *mgr;
	int ret;

	mgr = txnp->mgrp;
	dbenv = mgr->dbenv;

#ifdef HAVE_RPC
	if (F_ISSET(dbenv, DB_ENV_RPCCLIENT))
		return (__dbcl_txn_commit(txnp, flags));
#endif

	PANIC_CHECK(dbenv);
	if ((ret = __db_fchk(dbenv,
	    "txn_commit", flags, DB_TXN_NOSYNC | DB_TXN_SYNC)) != 0)
		return (ret);

	if ((ret = __db_fcchk(dbenv,
	    "txn_commit", flags, DB_TXN_NOSYNC, DB_TXN_SYNC)) != 0)
		return (ret);

	if ((ret = __txn_check_running(txnp, NULL)) != 0)
		return (ret);

	if (LF_ISSET(DB_TXN_NOSYNC)) {
		F_CLR(txnp, TXN_SYNC);
		F_SET(txnp, TXN_NOSYNC);
	}
	if (LF_ISSET(DB_TXN_SYNC)) {
		F_CLR(txnp, TXN_NOSYNC);
		F_SET(txnp, TXN_SYNC);
	}

	/* Commit any uncommitted children. */
	for (kids = TAILQ_FIRST(&txnp->kids);
	    kids != NULL;
	    kids = TAILQ_NEXT(kids, klinks))
		if (!F_ISSET(kids, TXN_CHILDCOMMIT) &&
		    (ret = txn_commit(kids, flags)) != 0)
			return (ret);

	/*
	 * If there are any log records, write a log record and sync the log,
	 * else do no log writes.  If the commit is for a child transaction,
	 * we do not need to commit the child synchronously since it may still
	 * abort (if its parent aborts), and otherwise its parent or ultimate
	 * ancestor will write synchronously.
	 */
	if (LOGGING_ON(dbenv) &&
	    (F_ISSET(txnp, TXN_MUSTFLUSH) || !IS_ZERO_LSN(txnp->last_lsn))) {
		if (txnp->parent == NULL)
			ret = __txn_regop_log(dbenv, txnp, &txnp->last_lsn,
			    (F_ISSET(mgr->dbenv, DB_ENV_TXN_NOSYNC) &&
			    !F_ISSET(txnp, TXN_SYNC)) ||
			    F_ISSET(txnp, TXN_NOSYNC) ?  0 : DB_FLUSH,
			    TXN_COMMIT, (int32_t)time(NULL));
		else {
			F_SET(txnp->parent, TXN_MUSTFLUSH);
			ret = __txn_child_log(dbenv, txnp, &txnp->last_lsn, 0,
			    TXN_COMMIT, txnp->parent->txnid);
		}
		if (ret != 0)
			return (ret);
	}

	/*
	 * If this is the senior ancestor (i.e., it has no parent), then we
	 * can release all the child transactions since everyone is committing.
	 * Then we can release this transaction.  If this is not the ultimate
	 * ancestor, then we can neither free it or its children.
	 */
	if (txnp->parent == NULL)
		__txn_freekids(txnp);

	return (__txn_end(txnp, 1));
}

/*
 * txn_abort --
 *	Abort a transaction.
 */
int
txn_abort(txnp)
	DB_TXN *txnp;
{
	int ret;

#ifdef HAVE_RPC
	if (F_ISSET(txnp->mgrp->dbenv, DB_ENV_RPCCLIENT))
		return (__dbcl_txn_abort(txnp));
#endif

	PANIC_CHECK(txnp->mgrp->dbenv);
	if ((ret = __txn_check_running(txnp, NULL)) != 0)
		return (ret);

	if ((ret = __txn_undo(txnp)) != 0) {
		return (ret);
	}
	return (__txn_end(txnp, 0));
}

/*
 * txn_prepare --
 *	Flush the log so a future commit is guaranteed to succeed.
 */
int
txn_prepare(txnp)
	DB_TXN *txnp;
{
	DBT xid;
	DB_ENV *dbenv;
	TXN_DETAIL *td;
	int ret;

	dbenv = txnp->mgrp->dbenv;
#ifdef HAVE_RPC
	if (F_ISSET(dbenv, DB_ENV_RPCCLIENT))
		return (__dbcl_txn_prepare(txnp));
#endif

	if ((ret = __txn_check_running(txnp, &td)) != 0)
		return (ret);

	memset(&xid, 0, sizeof(xid));
	xid.data = td->xid;
	xid.size = sizeof(td->xid);
	if (LOGGING_ON(dbenv) &&
	    (ret = __txn_xa_regop_log(dbenv, txnp, &txnp->last_lsn,
	    (F_ISSET(dbenv, DB_ENV_TXN_NOSYNC) &&
	    !F_ISSET(txnp, TXN_SYNC)) ||
	    F_ISSET(txnp, TXN_NOSYNC) ? 0 : DB_FLUSH, TXN_PREPARE,
	    &xid, td->format, td->gtrid, td->bqual)) != 0) {
		__db_err(dbenv,
		    "txn_prepare: log_write failed %s\n", db_strerror(ret));
		return (ret);
	}

	MUTEX_THREAD_LOCK(txnp->mgrp->mutexp);
	td->status = TXN_PREPARED;
	MUTEX_THREAD_UNLOCK(txnp->mgrp->mutexp);
	return (ret);
}

/*
 * Return the transaction ID associated with a particular transaction
 */
u_int32_t
txn_id(txnp)
	DB_TXN *txnp;
{
	return (txnp->txnid);
}

/* Internal routines. */

/*
 * Return 0 if the txnp is reasonable, otherwise returns EINVAL.
 */
static int
__txn_check_running(txnp, tdp)
	const DB_TXN *txnp;
	TXN_DETAIL **tdp;
{
	DB_TXNMGR *mgrp;
	TXN_DETAIL *tp;

	tp = NULL;
	mgrp = txnp->mgrp;
	if (txnp != NULL && mgrp != NULL && mgrp->reginfo.primary != NULL) {
		tp = (TXN_DETAIL *)R_ADDR(&mgrp->reginfo, txnp->off);
		/*
		 * Child transactions could be marked committed which is OK.
		 */
		if (tp->status != TXN_RUNNING &&
		    tp->status != TXN_PREPARED && tp->status != TXN_COMMITTED)
			tp = NULL;
		if (tdp != NULL)
			*tdp = tp;
	}

	return (tp == NULL ? EINVAL : 0);
}

/*
 * __txn_end --
 *	Internal transaction end routine.
 *
 * PUBLIC: int __txn_end __P((DB_TXN *, int));
 */
int
__txn_end(txnp, is_commit)
	DB_TXN *txnp;
	int is_commit;
{
	DB_ENV *dbenv;
	DB_LOCKREQ request;
	DB_TXN *kids;
	DB_TXNMGR *mgr;
	DB_TXNREGION *region;
	TXN_DETAIL *tp;
	int ret;

	mgr = txnp->mgrp;
	dbenv = mgr->dbenv;
	region = mgr->reginfo.primary;

	/*
	 * On aborts, we've undone the children, but we still need
	 * to free the up.
	 */
	if (!is_commit) {
		while ((kids = TAILQ_FIRST(&txnp->kids)) != NULL)
			if ((ret = __txn_end(kids, is_commit)) != 0)
				return (DB_RUNRECOVERY);
	}

	/* Release the locks. */
	request.op = txnp->parent == NULL ||
	    is_commit == 0 ? DB_LOCK_PUT_ALL : DB_LOCK_INHERIT;

	if (LOCKING_ON(dbenv)) {
		ret = lock_vec(dbenv, txnp->txnid, 0, &request, 1, NULL);
		if (ret != 0 && (ret != DB_LOCK_DEADLOCK || is_commit)) {
			__db_err(dbenv, "%s: release locks failed %s",
			    is_commit ? "txn_commit" : "txn_abort",
			    db_strerror(ret));
			return (ret);
		}
	}

	/* End the transaction. */
	R_LOCK(dbenv, &mgr->reginfo);

	/*
	 * Child transactions that are committing cannot be released until
	 * the parent commits, since the parent may abort, causing the child
	 * to abort as well.
	 */
	tp = (TXN_DETAIL *)R_ADDR(&mgr->reginfo, txnp->off);
	if (txnp->parent == NULL || !is_commit) {
		SH_TAILQ_REMOVE(&region->active_txn, tp, links, __txn_detail);

		__db_shalloc_free(mgr->reginfo.addr, tp);
	} else {
		tp->status = TXN_COMMITTED;
		F_SET(txnp, TXN_CHILDCOMMIT);
	}

	if (is_commit)
		region->ncommits++;
	else
		region->naborts++;
	--region->nactive;

	R_UNLOCK(dbenv, &mgr->reginfo);

	/*
	 * If the transaction aborted, we can remove it from its parent links.
	 * If it committed, then we need to leave it on, since the parent can
	 * still abort.
	 * The transaction cannot get more locks, remove its locker info.
	 */
	if (txnp->parent != NULL) {
		if (LOCKING_ON(dbenv))
			__lock_freefamilylocker(dbenv->lk_handle, txnp->txnid);
		 if (!is_commit)
			TAILQ_REMOVE(&txnp->parent->kids, txnp, klinks);
	}

	/* Free the space. */
	if (F_ISSET(txnp, TXN_MALLOC) && (txnp->parent == NULL || !is_commit)) {
		MUTEX_THREAD_LOCK(mgr->mutexp);
		TAILQ_REMOVE(&mgr->txn_chain, txnp, links);
		MUTEX_THREAD_UNLOCK(mgr->mutexp);

		__os_free(txnp, sizeof(*txnp));
	}

	return (0);
}

/*
 * __txn_undo --
 *	Undo the transaction with id txnid.  Returns 0 on success and
 *	errno on failure.
 */
static int
__txn_undo(txnp)
	DB_TXN *txnp;
{
	DBT rdbt;
	DB_ENV *dbenv;
	DB_LSN *lsn_array, *key_lsnp;
	DB_TXNMGR *mgr;
	int ntxns, ret, threaded;

	mgr = txnp->mgrp;
	dbenv = mgr->dbenv;
	lsn_array = NULL;

	if (!LOGGING_ON(dbenv))
		return (0);

	/*
	 * This is the simplest way to code this, but if the mallocs during
	 * recovery turn out to be a performance issue, we can do the
	 * allocation here and use DB_DBT_USERMEM.
	 */
	memset(&rdbt, 0, sizeof(rdbt));
	threaded = F_ISSET(dbenv, DB_ENV_THREAD) ? 1 : 0;
	if (threaded)
		F_SET(&rdbt, DB_DBT_MALLOC);

	key_lsnp = &txnp->last_lsn;

	if (TAILQ_FIRST(&txnp->kids) != NULL) {
		if ((ret = __txn_makefamily(dbenv,
		     txnp, &ntxns, &lsn_array)) != 0)
			return (ret);
		key_lsnp = &lsn_array[0];
	}

	for (ret = 0; ret == 0 && !IS_ZERO_LSN(*key_lsnp);) {
		/*
		 * The dispatch routine returns the lsn of the record
		 * before the current one in the key_lsnp argument.
		 */
		if ((ret = log_get(dbenv, key_lsnp, &rdbt, DB_SET)) == 0) {
			ret = mgr->recover(dbenv,
			    &rdbt, key_lsnp, DB_TXN_ABORT, NULL);
			if (threaded && rdbt.data != NULL) {
				__os_free(rdbt.data, rdbt.size);
				rdbt.data = NULL;
			}
			if (lsn_array != NULL)
				TXN_BUBBLE(lsn_array, ntxns);
		}
		if (ret != 0) {
			__db_err(txnp->mgrp->dbenv,
			    "txn_abort: Log undo failed for LSN: %lu %lu: %s",
			    (u_long)key_lsnp->file, (u_long)key_lsnp->offset,
			    db_strerror(ret));
			goto out;
		}
	}

out:	if (lsn_array != NULL)
		(void)__os_free(lsn_array, ntxns * sizeof(DB_LSN));

	return (ret);
}

/*
 * Transaction checkpoint.
 * If either kbytes or minutes is non-zero, then we only take the checkpoint
 * more than "minutes" minutes have passed since the last checkpoint or if
 * more than "kbytes" of log data have been written since the last checkpoint.
 * When taking a checkpoint, find the oldest active transaction and figure out
 * its first LSN.  This is the lowest LSN we can checkpoint, since any record
 * written after since that point may be involved in a transaction and may
 * therefore need to be undone in the case of an abort.
 */
int
txn_checkpoint(dbenv, kbytes, minutes, flags)
	DB_ENV *dbenv;
	u_int32_t kbytes, minutes, flags;
{
	DB_LOG *dblp;
	DB_LSN ckp_lsn, sync_lsn, last_ckp;
	DB_TXNMGR *mgr;
	DB_TXNREGION *region;
	LOG *lp;
	TXN_DETAIL *txnp;
	time_t last_ckp_time, now;
	u_int32_t bytes, mbytes;
	int ret;

#ifdef HAVE_RPC
	if (F_ISSET(dbenv, DB_ENV_RPCCLIENT))
		return (__dbcl_txn_checkpoint(dbenv, kbytes, minutes));
#endif
	PANIC_CHECK(dbenv);
	ENV_REQUIRES_CONFIG(dbenv, dbenv->tx_handle, DB_INIT_TXN);

	mgr = dbenv->tx_handle;
	region = mgr->reginfo.primary;
	dblp = dbenv->lg_handle;
	lp = dblp->reginfo.primary;

	/*
	 * Check if we need to checkpoint.
	 */
	ZERO_LSN(ckp_lsn);

	if (LF_ISSET(DB_FORCE))
		goto do_ckp;

	R_LOCK(dbenv, &dblp->reginfo);
	mbytes = lp->stat.st_wc_mbytes;
	bytes = lp->stat.st_wc_bytes;
	ckp_lsn = lp->lsn;
	R_UNLOCK(dbenv, &dblp->reginfo);

	/* Don't checkpoint a quiescent database. */
	if (bytes == 0 && mbytes == 0)
		return (0);

	if (kbytes != 0 && mbytes * 1024 + bytes / 1024 >= (u_int32_t)kbytes)
		goto do_ckp;

	if (minutes != 0) {
		(void)time(&now);

		R_LOCK(dbenv, &mgr->reginfo);
		last_ckp_time = region->time_ckp;
		R_UNLOCK(dbenv, &mgr->reginfo);

		if (now - last_ckp_time >= (time_t)(minutes * 60))
			goto do_ckp;
	}

	/*
	 * If we checked time and data and didn't go to checkpoint,
	 * we're done.
	 */
	if (minutes != 0 || kbytes != 0)
		return (0);

do_ckp:
	if (IS_ZERO_LSN(ckp_lsn)) {
		R_LOCK(dbenv, &dblp->reginfo);
		ckp_lsn = lp->lsn;
		R_UNLOCK(dbenv, &dblp->reginfo);
	}

	/*
	 * We have to find an LSN such that all transactions begun
	 * before that LSN are complete.
	 */
	R_LOCK(dbenv, &mgr->reginfo);

	if (IS_ZERO_LSN(region->pending_ckp)) {
		for (txnp =
		    SH_TAILQ_FIRST(&region->active_txn, __txn_detail);
		    txnp != NULL;
		    txnp = SH_TAILQ_NEXT(txnp, links, __txn_detail)) {

			/*
			 * Look through the active transactions for the
			 * lowest begin lsn.
			 */
			if (!IS_ZERO_LSN(txnp->begin_lsn) &&
			    log_compare(&txnp->begin_lsn, &ckp_lsn) < 0)
				ckp_lsn = txnp->begin_lsn;
		}
		region->pending_ckp = ckp_lsn;
	} else
		ckp_lsn = region->pending_ckp;

	R_UNLOCK(dbenv, &mgr->reginfo);

	/*
	 * memp_sync may change the lsn you pass it, so don't pass it
	 * the actual ckp_lsn, pass it a temp instead.
	 */
	sync_lsn = ckp_lsn;
	if (MPOOL_ON(dbenv) && (ret = memp_sync(dbenv, &sync_lsn)) != 0) {
		/*
		 * ret == DB_INCOMPLETE means that there are still buffers to
		 * flush, the checkpoint is not complete.  Wait and try again.
		 */
		if (ret > 0)
			__db_err(dbenv,
			    "txn_checkpoint: system failure in memp_sync %s\n",
			    db_strerror(ret));
		return (ret);
	}
	if (LOGGING_ON(dbenv)) {
		R_LOCK(dbenv, &mgr->reginfo);
		last_ckp = region->last_ckp;
		ZERO_LSN(region->pending_ckp);
		R_UNLOCK(dbenv, &mgr->reginfo);

		if ((ret = __txn_ckp_log(dbenv,
		    NULL, &ckp_lsn, DB_CHECKPOINT, &ckp_lsn,
		    &last_ckp, (int32_t)time(NULL))) != 0) {
			__db_err(dbenv,
			    "txn_checkpoint: log failed at LSN [%ld %ld] %s\n",
			    (long)ckp_lsn.file, (long)ckp_lsn.offset,
			    db_strerror(ret));
			return (ret);
		}

		R_LOCK(dbenv, &mgr->reginfo);
		region->last_ckp = ckp_lsn;
		(void)time(&region->time_ckp);
		R_UNLOCK(dbenv, &mgr->reginfo);
	}
	return (0);
}

static void
__txn_freekids(txnp)
	DB_TXN *txnp;
{
	DB_ENV *dbenv;
	DB_TXN *kids;
	DB_TXNMGR *mgr;
	DB_TXNREGION *region;
	TXN_DETAIL *tp;

	mgr = txnp->mgrp;
	dbenv = mgr->dbenv;
	region = mgr->reginfo.primary;

	for (kids = TAILQ_FIRST(&txnp->kids);
	    kids != NULL;
	    kids = TAILQ_FIRST(&txnp->kids)) {
		/* Free any children of this transaction. */
		__txn_freekids(kids);

		/* Free the transaction detail in the region. */
		R_LOCK(dbenv, &mgr->reginfo);
		tp = (TXN_DETAIL *)R_ADDR(&mgr->reginfo, kids->off);
		SH_TAILQ_REMOVE(&region->active_txn, tp, links, __txn_detail);

		__db_shalloc_free(mgr->reginfo.addr, tp);
		R_UNLOCK(dbenv, &mgr->reginfo);

		/* Now remove from its parent. */
		TAILQ_REMOVE(&txnp->kids, kids, klinks);
		if (F_ISSET(txnp, TXN_MALLOC)) {
			MUTEX_THREAD_LOCK(mgr->mutexp);
			TAILQ_REMOVE(&mgr->txn_chain, kids, links);
			MUTEX_THREAD_UNLOCK(mgr->mutexp);
			__os_free(kids, sizeof(*kids));
		}
	}
}

/*
 * __txn_makefamily --
 *	Create an array of DB_LSNs for every member of the family being
 * aborted so that we can undo the records in the appropriate order.  We
 * allocate memory here and expect our caller to free it when they're done.
 */
static int
__txn_makefamily(dbenv, txnp, np, arrayp)
	DB_ENV *dbenv;
	DB_TXN *txnp;
	int *np;
	DB_LSN **arrayp;
{
	DB_LSN *ap, *tmpp;
	int i, ret;

	/* Figure out how many we have. */
	*np = __txn_count(txnp);

	/* Malloc space. */
	if ((ret = __os_malloc(dbenv, *np * sizeof(DB_LSN), NULL, arrayp)) != 0)
		return (ret);

	/* Fill in the space. */
	tmpp = *arrayp;
	__txn_lsn(txnp, &tmpp);

	/* Sort the LSNs. */
	ap = *arrayp;
	for (i = 0; i < *np; i++)
		TXN_BUBBLE(ap, *np - i);

	return (0);
}

/*
 * __txn_count --
 *	Routine to count the number of members in a transaction family.  We
 * include the incoming transaction in the count.  We assume that we never
 * call this routine with NULL.
 */
static int
__txn_count(txnp)
	DB_TXN *txnp;
{
	DB_TXN *kids;
	int n;

	n = 1;
	for (kids = TAILQ_FIRST(&txnp->kids);
	    kids != NULL;
	    kids = TAILQ_NEXT(kids, klinks))
		n += __txn_count(kids);

	return (n);
}

/*
 * __txn_lsn ---
 *	Fill in the array with the last_lsn field of every transaction
 * in the family.  Array is an in/out parameter that leaves you pointing
 * to the next space in which to place an LSN.
 */
static void
__txn_lsn(txnp, array)
	DB_TXN *txnp;
	DB_LSN **array;
{
	DB_LSN *lsn;
	DB_TXN *kids;

	lsn = *array;
	lsn[0] = txnp->last_lsn;
	*array = &lsn[1];

	for (kids = TAILQ_FIRST(&txnp->kids);
	    kids != NULL;
	    kids = TAILQ_NEXT(kids, klinks))
		__txn_lsn(kids, array);
}

/*
 * __txn_activekids --
 *	Determine if this transaction has any active children.  Returns 1
 * if any active children are present; 0 otherwise.
 *
 * PUBLIC: int __txn_activekids __P((DB_TXN *));
 */
int
__txn_activekids(txnp)
	DB_TXN *txnp;
{
	DB_TXN *kids;

	for (kids = TAILQ_FIRST(&txnp->kids);
	    kids != NULL;
	    kids = TAILQ_NEXT(kids, klinks))
		if (!F_ISSET(kids, TXN_CHILDCOMMIT))
			return (1);
	return (0);
}
