/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2000, 2013 Oracle and/or its affiliates.  All rights reserved.
 *
 * $Id$
 */

#include "db_config.h"

#include "db_int.h"
#include "dbinc/db_page.h"
#include "dbinc/db_am.h"
#include "dbinc/lock.h"
#include "dbinc/txn.h"

static int __cdsgroup_abort __P((DB_TXN *txn));
static int __cdsgroup_commit __P((DB_TXN *txn, u_int32_t flags));
static int __cdsgroup_discard __P((DB_TXN *txn, u_int32_t flags));
static u_int32_t __cdsgroup_id __P((DB_TXN *txn));
static int __cdsgroup_notsup __P((ENV *env, const char *meth));
static int __cdsgroup_prepare __P((DB_TXN *txn, u_int8_t *gid));
static int __cdsgroup_get_name __P((DB_TXN *txn, const char **namep));
static int __cdsgroup_set_name __P((DB_TXN *txn, const char *name));
static int __cdsgroup_set_timeout
    __P((DB_TXN *txn, db_timeout_t timeout, u_int32_t flags));
static int __cdsgroup_get_priority __P((DB_TXN *txn, u_int32_t *priorityp));
static int __cdsgroup_set_priority __P((DB_TXN *txn, u_int32_t priority));
static int __cdsgroup_set_commit_token __P((DB_TXN *txn, DB_TXN_TOKEN *tokenp));
static void __cdsgroup_set_txn_lsnp
    __P((DB_TXN *txn, DB_LSN **rlsnp, DB_LSN **lsnp));

/*
 * __cdsgroup_notsup --
 *	Error when CDS groups don't support a method.
 */
static int
__cdsgroup_notsup(env, meth)
	ENV *env;
	const char *meth;
{
	__db_errx(env, DB_STR_A("0687", "CDS groups do not support %s", "%s"),
	    meth);
	return (DB_OPNOTSUP);
}

static int
__cdsgroup_abort(txn)
	DB_TXN *txn;
{
	return (__cdsgroup_notsup(txn->mgrp->env, "abort"));
}

static int
__cdsgroup_commit(txn, flags)
	DB_TXN *txn;
	u_int32_t flags;
{
	DB_LOCKER *locker;
	DB_LOCKREQ lreq;
	ENV *env;
	int ret, t_ret;

	COMPQUIET(flags, 0);
	env = txn->mgrp->env;

	/* Check for live cursors. */
	if (txn->cursors != 0) {
		__db_errx(env, DB_STR("0688", "CDS group has active cursors"));
		return (EINVAL);
	}

	/* We may be holding handle locks; release them. */
	lreq.op = DB_LOCK_PUT_ALL;
	lreq.obj = NULL;
	ret = __lock_vec(env, txn->locker, 0, &lreq, 1, NULL);

	env = txn->mgrp->env;
	locker = txn->locker;
	__os_free(env, txn->mgrp);
	__os_free(env, txn);
	if ((t_ret = __lock_id_free(env, locker)) != 0 && ret == 0)
		ret = t_ret;
	return (ret);
}

static int __cdsgroup_discard(txn, flags)
	DB_TXN *txn;
	u_int32_t flags;
{
	COMPQUIET(flags, 0);
	return (__cdsgroup_notsup(txn->mgrp->env, "discard"));
}

static u_int32_t __cdsgroup_id(txn)
	DB_TXN *txn;
{
	return (txn->txnid);
}

static int __cdsgroup_prepare(txn, gid)
	DB_TXN *txn;
	u_int8_t *gid;
{
	COMPQUIET(gid, NULL);
	return (__cdsgroup_notsup(txn->mgrp->env, "prepare"));
}

static int __cdsgroup_get_name(txn, namep)
	DB_TXN *txn;
	const char **namep;
{
	COMPQUIET(namep, NULL);
	return (__cdsgroup_notsup(txn->mgrp->env, "get_name"));
}

static int __cdsgroup_set_name(txn, name)
	DB_TXN *txn;
	const char *name;
{
	COMPQUIET(name, NULL);
	return (__cdsgroup_notsup(txn->mgrp->env, "set_name"));
}

static int __cdsgroup_set_timeout(txn, timeout, flags)
	DB_TXN *txn;
	db_timeout_t timeout;
	u_int32_t flags;
{
	COMPQUIET(timeout, 0);
	COMPQUIET(flags, 0);
	return (__cdsgroup_notsup(txn->mgrp->env, "set_timeout"));
}

static int
__cdsgroup_get_priority(txn, priorityp)
	DB_TXN *txn;
	u_int32_t *priorityp;
{
	COMPQUIET(priorityp, NULL);
	return (__cdsgroup_notsup(txn->mgrp->env, "get_priority"));
}

static int
__cdsgroup_set_priority(txn, priority)
	DB_TXN *txn;
	u_int32_t priority;
{
	COMPQUIET(priority, 0);
	return (__cdsgroup_notsup(txn->mgrp->env, "set_priority"));
}

static int
__cdsgroup_set_commit_token(txn, tokenp)
	DB_TXN *txn;
	DB_TXN_TOKEN *tokenp;
{
	COMPQUIET(tokenp, NULL);
	return (__cdsgroup_notsup(txn->mgrp->env, "set_commit_token"));
}

/*
 * __cdsgroup_set_txn_lsnp --
 *	The DB_TXN method table declares this one void, so it cannot report
 *	DB_OPNOTSUP the way the others do.  A CDS group has no transactional
 *	LSNs, so hand back NULL pointers: callers in the log path test these for
 *	NULL, and leaving the slot itself NULL meant an indirect call through a
 *	NULL function pointer instead.
 */
static void
__cdsgroup_set_txn_lsnp(txn, rlsnp, lsnp)
	DB_TXN *txn;
	DB_LSN **rlsnp, **lsnp;
{
	COMPQUIET(txn, NULL);
	if (rlsnp != NULL)
		*rlsnp = NULL;
	if (lsnp != NULL)
		*lsnp = NULL;
}

/*
 * PUBLIC: int __cdsgroup_begin __P((ENV *, DB_TXN **));
 */
int
__cdsgroup_begin(env, txnpp)
	ENV *env;
	DB_TXN **txnpp;
{
	DB_TXN *txn;
	int ret;

	*txnpp = txn = NULL;
	if ((ret = __os_calloc(env, 1, sizeof(DB_TXN), &txn)) != 0)
		goto err;
	/*
	 * We need a dummy DB_TXNMGR -- it's the only way to get from a
	 * transaction handle to the environment handle.
	 */
	if ((ret = __os_calloc(env, 1, sizeof(DB_TXNMGR), &txn->mgrp)) != 0)
		goto err;
	txn->mgrp->env = env;

	if ((ret = __lock_id(env, &txn->txnid, &txn->locker)) != 0)
		goto err;

	txn->flags = TXN_FAMILY;
	txn->abort = __cdsgroup_abort;
	txn->commit = __cdsgroup_commit;
	txn->discard = __cdsgroup_discard;
	txn->id = __cdsgroup_id;
	txn->prepare = __cdsgroup_prepare;
	txn->get_name = __cdsgroup_get_name;
	txn->set_name = __cdsgroup_set_name;
	txn->set_timeout = __cdsgroup_set_timeout;
	/*
	 * A DB_TXN handle has 12 methods; the eight above are the ones a CDS
	 * group can implement.  The remaining four used to be left NULL, so an
	 * application calling one of them made an indirect call through a NULL
	 * function pointer and crashed instead of getting an error.
	 */
	txn->get_priority = __cdsgroup_get_priority;
	txn->set_priority = __cdsgroup_set_priority;
	txn->set_commit_token = __cdsgroup_set_commit_token;
	txn->set_txn_lsnp = __cdsgroup_set_txn_lsnp;

	*txnpp = txn;

	if (0) {
err:		if (txn != NULL) {
			if (txn->mgrp != NULL)
				__os_free(env, txn->mgrp);
			__os_free(env, txn);
		}
	}
	return (ret);
}

/*
 * __cds_txn_begin_pp --
 *	DB_ENV->cdsgroup_begin
 *
 * PUBLIC: int __cdsgroup_begin_pp __P((DB_ENV *, DB_TXN **));
 */
int __cdsgroup_begin_pp(dbenv, txnpp)
	DB_ENV *dbenv;
	DB_TXN **txnpp;
{
	DB_THREAD_INFO *ip;
	ENV *env;
	int ret;

	env = dbenv->env;

	ENV_ILLEGAL_BEFORE_OPEN(env, "cdsgroup_begin");
	if (!CDB_LOCKING(env))
		return (__env_not_config(env, "cdsgroup_begin", DB_INIT_CDB));

	ENV_ENTER(env, ip);
	ret = __cdsgroup_begin(env, txnpp);
	ENV_LEAVE(env, ip);
	return (ret);
 }
