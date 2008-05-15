/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996,2008 Oracle.  All rights reserved.
 *
 * $Id: xa_map.c,v 12.12 2008/01/08 20:59:00 bostic Exp $
 */

#include "db_config.h"

#include "db_int.h"
#include "dbinc/txn.h"

/*
 * This file contains all the mapping information that we need to support
 * the DB/XA interface.
 */

/*
 * __db_rmid_to_env
 *	Return the environment associated with a given XA rmid.
 *
 * PUBLIC: int __db_rmid_to_env __P((int, ENV **));
 */
int
__db_rmid_to_env(rmid, envp)
	int rmid;
	ENV **envp;
{
	ENV *env;

	env = TAILQ_FIRST(&DB_GLOBAL(envq));
	if (env != NULL && env->xa_rmid == rmid) {
		*envp = env;
		return (0);
	}

	/*
	 * When we map an rmid, move that environment to be the first one in
	 * the list of environments, so we acquire the correct environment
	 * in DB->open.
	 */
	for (; env != NULL; env = TAILQ_NEXT(env, links))
		if (env->xa_rmid == rmid) {
			TAILQ_REMOVE(&DB_GLOBAL(envq), env, links);
			TAILQ_INSERT_HEAD(&DB_GLOBAL(envq), env, links);
			*envp = env;
			return (0);
		}

	return (1);
}

/*
 * __db_xid_to_txn
 *	Return the txn that corresponds to this XID.
 *
 * PUBLIC: int __db_xid_to_txn __P((ENV *, XID *, roff_t *));
 */
int
__db_xid_to_txn(env, xid, offp)
	ENV *env;
	XID *xid;
	roff_t *offp;
{
	struct __txn_detail *td;

	return (__txn_map_gid(env, (u_int8_t *)xid->data, &td, offp));
}

/*
 * __db_map_rmid
 *	Create a mapping between the specified rmid and environment.
 *
 * PUBLIC: int __db_map_rmid __P((int, ENV *));
 */
int
__db_map_rmid(rmid, env)
	int rmid;
	ENV *env;
{
	env->xa_rmid = rmid;
	TAILQ_INSERT_TAIL(&DB_GLOBAL(envq), env, links);
	return (0);
}

/*
 * __db_unmap_rmid
 *	Destroy the mapping for the given rmid.
 *
 * PUBLIC: int __db_unmap_rmid __P((int));
 */
int
__db_unmap_rmid(rmid)
	int rmid;
{
	ENV *e;

	for (e = TAILQ_FIRST(&DB_GLOBAL(envq));
	    e->xa_rmid != rmid;
	    e = TAILQ_NEXT(e, links))
	    ;

	if (e == NULL)
		return (EINVAL);

	TAILQ_REMOVE(&DB_GLOBAL(envq), e, links);
	return (0);
}

/*
 * __db_map_xid
 *	Create a mapping between this XID and the transaction
 *	"td" in the shared region.
 *
 * PUBLIC: int __db_map_xid __P((ENV *, XID *, TXN_DETAIL *));
 */
int
__db_map_xid(env, xid, td)
	ENV *env;
	XID *xid;
	TXN_DETAIL *td;
{
	TXN_SYSTEM_LOCK(env);
	memcpy(td->xid, xid->data, XIDDATASIZE);
	td->bqual = (u_int32_t)xid->bqual_length;
	td->gtrid = (u_int32_t)xid->gtrid_length;
	td->format = (int32_t)xid->formatID;
	TXN_SYSTEM_UNLOCK(env);

	return (0);
}

/*
 * __db_unmap_xid
 *	Destroy the mapping for the specified XID.
 *
 * PUBLIC: void __db_unmap_xid __P((ENV *, XID *, size_t));
 */

void
__db_unmap_xid(env, xid, off)
	ENV *env;
	XID *xid;
	size_t off;
{
	TXN_DETAIL *td;

	COMPQUIET(xid, NULL);

	td = R_ADDR(&env->tx_handle->reginfo, off);
	memset(td->xid, 0, sizeof(td->xid));
}
