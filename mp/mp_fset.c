/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996,2008 Oracle.  All rights reserved.
 *
 * $Id: mp_fset.c,v 12.29 2008/03/13 15:21:21 mbrey Exp $
 */

#include "db_config.h"

#include "db_int.h"
#include "dbinc/log.h"
#include "dbinc/mp.h"
#include "dbinc/txn.h"

/*
 * __memp_dirty --
 *	Upgrade a page from a read-only to a writeable pointer.
 *
 * PUBLIC: int __memp_dirty __P((DB_MPOOLFILE *, void *,
 * PUBLIC:     DB_THREAD_INFO *, DB_TXN *, DB_CACHE_PRIORITY, u_int32_t));
 */
int
__memp_dirty(dbmfp, addrp, ip, txn, priority, flags)
	DB_MPOOLFILE *dbmfp;
	void *addrp;
	DB_THREAD_INFO *ip;
	DB_TXN *txn;
	DB_CACHE_PRIORITY priority;
	u_int32_t flags;
{
	BH *bhp;
	DB_MPOOL_HASH *hp;
	DB_TXN *ancestor;
	ENV *env;
#ifdef DIAG_MVCC
	MPOOLFILE *mfp;
#endif
	REGINFO *infop;
	int mvcc, ret;
	db_pgno_t pgno;
	void *pgaddr;

	env = dbmfp->env;
	pgaddr = *(void **)addrp;
	mvcc = dbmfp->mfp->multiversion;

	/* Convert the page address to a buffer header. */
	bhp = (BH *)((u_int8_t *)pgaddr - SSZA(BH, buf));
	pgno = bhp->pgno;

	if (flags == 0)
		flags = DB_MPOOL_DIRTY;
	DB_ASSERT(env, flags == DB_MPOOL_DIRTY || flags == DB_MPOOL_EDIT);

	if (F_ISSET(dbmfp, MP_READONLY)) {
		__db_errx(env, "%s: dirty flag set for readonly file page",
		    __memp_fn(dbmfp));
		return (EACCES);
	}

	for (ancestor = txn;
	    ancestor != NULL && ancestor->parent != NULL;
	    ancestor = ancestor->parent)
		;

	if (mvcc && txn != NULL &&
	    (!BH_OWNED_BY(env, bhp, ancestor) || SH_CHAIN_HASNEXT(bhp, vc))) {
slow:		if ((ret = __memp_fget(dbmfp,
		    &pgno, ip, txn, flags, addrp)) != 0) {
			if (ret != DB_LOCK_DEADLOCK)
				__db_errx(env,
				    "%s: error getting a page for writing",
				    __memp_fn(dbmfp));
			*(void **)addrp = pgaddr;
			return (ret);
		}

		DB_ASSERT(env,
		    (flags == DB_MPOOL_EDIT && *(void **)addrp == pgaddr) ||
		    (flags != DB_MPOOL_EDIT && *(void **)addrp != pgaddr));

		if ((ret = __memp_fput(dbmfp, ip, pgaddr, priority)) != 0) {
			__db_errx(env,
			    "%s: error releasing a read-only page",
			    __memp_fn(dbmfp));
			(void)__memp_fput(dbmfp, ip, *(void **)addrp, priority);
			*(void **)addrp = NULL;
			return (ret);
		}
		pgaddr = *(void **)addrp;
		bhp = (BH *)((u_int8_t *)pgaddr - SSZA(BH, buf));
		DB_ASSERT(env, pgno == bhp->pgno);
		return (0);
	}

	MP_GET_BUCKET(env, dbmfp->mfp, pgno, &infop, hp, ret);
	if (ret != 0)
		return (ret);

	/* Need to recheck in case we raced with a freeze operation. */
	if (mvcc && txn != NULL && SH_CHAIN_HASNEXT(bhp, vc)) {
		MUTEX_UNLOCK(env, hp->mtx_hash);
		goto slow;
	}

	/* Set/clear the page bits. */
	if (!F_ISSET(bhp, BH_DIRTY)) {
		++hp->hash_page_dirty;
		F_SET(bhp, BH_DIRTY);
	}
	MUTEX_UNLOCK(env, hp->mtx_hash);

#ifdef DIAG_MVCC
	mfp = R_ADDR(env->mp_handle->reginfo, bhp->mf_offset);
	MVCC_MPROTECT(bhp->buf, mfp->stat.st_pagesize, PROT_READ | PROT_WRITE);
#endif
	return (0);
}
