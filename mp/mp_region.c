/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */
#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)mp_region.c	11.4 (Sleepycat) 10/19/99";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <errno.h>
#include <string.h>
#endif

#include "db_int.h"
#include "db_shash.h"
#include "mp.h"

static int __mcache_init __P((DB_ENV *, DB_MPOOL *, int, int));
static int __mpool_init __P((DB_ENV *, DB_MPOOL *, int));

/*
 * __memp_open --
 *	Internal version of memp_open: only called from DB_ENV->open.
 *
 * PUBLIC: int __memp_open __P((DB_ENV *));
 */
int
__memp_open(dbenv)
	DB_ENV *dbenv;
{
	DB_MPOOL *dbmp;
	MPOOL *mp;
	roff_t mpr_size, reg_size, *regids;
	int i, htab_buckets, ret;

	/* Figure out how big each cache region is. */
	reg_size = dbenv->mp_gbytes / dbenv->mp_ncache;
	reg_size += (dbenv->mp_gbytes % dbenv->mp_ncache) / dbenv->mp_ncache;
	reg_size += dbenv->mp_bytes / dbenv->mp_ncache;

	/*
	 * Figure out how many hash buckets each region will have.  Assume we
	 * want to keep the hash chains with under 10 pages on each chain.  We
	 * don't know the pagesize in advance, and it may differ for different
	 * files.  Use a pagesize of 1K for the calculation -- we walk these
	 * chains a lot, they must be kept short.
	 */
	htab_buckets = __db_tablesize((reg_size / (1 * 1024)) / 10);

	/* Create and initialize the DB_MPOOL structure. */
	if ((ret = __os_calloc(1, sizeof(*dbmp), &dbmp)) != 0)
		return (ret);
	LIST_INIT(&dbmp->dbregq);
	TAILQ_INIT(&dbmp->dbmfq);
	dbmp->dbenv = dbenv;

	/*
	 * Join/create the mpool region.  If this is a local region we don't
	 * need much space because the most we'll store there is the pair of
	 * MPOOL and MPOOLFILE structures.  If we're creating a full-blown
	 * database environment, be generous -- I'd rather not fail because
	 * we ran out of space.
	 */
	dbmp->reginfo.id = REG_ID_MPOOL;
	dbmp->reginfo.mode = dbenv->db_mode;
	if (F_ISSET(dbenv, DB_ENV_DBLOCAL))
		mpr_size = sizeof(MPOOL) + sizeof(MPOOLFILE) + 5 * 1024;
	else
		mpr_size = sizeof(MPOOL) +
		    DB_MPOOLFILE_DEF * sizeof(MPOOLFILE) + 10 * 1024;
	if (F_ISSET(dbenv, DB_ENV_CREATE))
		F_SET(&dbmp->reginfo, REGION_CREATE_OK);
	if ((ret = __db_r_attach(dbenv, &dbmp->reginfo, mpr_size)) != 0)
		goto err;

	/*
	 * If we created the region, initialize it and any additional regions,
	 * otherwise join any additional regions.
	 */
	if (F_ISSET(&dbmp->reginfo, REGION_CREATE)) {
		/* Initialize the primary region. */
		if ((ret = __mpool_init(dbenv, dbmp, dbenv->mp_ncache)) != 0)
			goto err;

		/*
		 * We know how many regions there are going to be, allocate
		 * the REGINFO structures and fill in local copies of that
		 * information.
		 */
		if ((ret = __os_calloc(
		    dbenv->mp_ncache, sizeof(REGINFO), &dbmp->c_reginfo)) != 0)
			goto err;
		dbmp->nc_reg = dbenv->mp_ncache;

		/* Make sure we don't clear the wrong entries on error. */
		for (i = 0; i < dbmp->nc_reg; ++i)
			dbmp->c_reginfo[i].id = REG_ID_INVALID;

		/*
		 * Create/initialize the cache regions and copy their IDs
		 * into the primary region.
		 */
		mp = R_ADDR(&dbmp->reginfo, dbmp->reginfo.rp->primary);
		regids = R_ADDR(&dbmp->reginfo, mp->c_regids);
		for (i = 0; i < dbmp->nc_reg; ++i) {
			dbmp->c_reginfo[i].id = REG_ID_INVALID;
			dbmp->c_reginfo[i].mode = dbenv->db_mode;
			F_SET(&dbmp->c_reginfo[i], REGION_CREATE_OK);
			if ((ret = __db_r_attach(
			    dbenv, &dbmp->c_reginfo[i], reg_size)) != 0)
				goto err;
			if ((ret =
			    __mcache_init(dbenv, dbmp, htab_buckets, i)) != 0)
				goto err;
			R_UNLOCK(dbenv, &dbmp->c_reginfo[i]);

			regids[i] = dbmp->c_reginfo[i].id;
		}
	} else {
		/*
		 * We know how many regions there are going to be, allocate
		 * the REGINFO structures and fill in local copies of that
		 * information.
		 */
		mp = R_ADDR(&dbmp->reginfo, dbmp->reginfo.rp->primary);
		if ((ret = __os_calloc(
		    mp->nc_reg, sizeof(REGINFO), &dbmp->c_reginfo)) != 0)
			goto err;
		dbmp->nc_reg = mp->nc_reg;

		/* Make sure we don't clear the wrong entries on error. */
		for (i = 0; i < dbmp->nc_reg; ++i)
			dbmp->c_reginfo[i].id = REG_ID_INVALID;

		/* Join additional regions. */
		regids = R_ADDR(&dbmp->reginfo, mp->c_regids);
		for (i = 0; i < dbmp->nc_reg; ++i) {
			dbmp->c_reginfo[i].id = regids[i];
			dbmp->c_reginfo[i].mode = 0;
			if ((ret =
			    __db_r_attach(dbenv, &dbmp->c_reginfo[i], 0)) != 0)
				goto err;
			R_UNLOCK(dbenv, &dbmp->c_reginfo[i]);
		}
	}

	/* Set the local addresses for the primary and cache regions. */
	dbmp->reginfo.primary = mp =
	    R_ADDR(&dbmp->reginfo, dbmp->reginfo.rp->primary);
	for (i = 0; i < dbmp->nc_reg; ++i)
		dbmp->c_reginfo[i].primary =
		    R_ADDR(&dbmp->c_reginfo[i], dbmp->c_reginfo[i].rp->primary);

	R_UNLOCK(dbenv, &dbmp->reginfo);

	/* If the region is threaded, allocate a mutex to lock the handles. */
	if (F_ISSET(dbenv, DB_ENV_THREAD)) {
		if ((ret = __db_mutex_alloc(
		    dbenv, &dbmp->reginfo, &dbmp->mutexp)) != 0) {
			goto err;
		}
		if ((ret =
		    __db_mutex_init(dbenv, dbmp->mutexp, 0, MUTEX_THREAD)) != 0)
			goto err;
	}

	dbenv->mp_handle = dbmp;
	return (0);

err:	if (dbmp->reginfo.addr != NULL) {
		if (F_ISSET(&dbmp->reginfo, REGION_CREATE))
			for (i = 0; i < dbmp->nc_reg; ++i)
				if (dbmp->c_reginfo[i].id != REG_ID_INVALID)
					F_SET(dbmp->c_reginfo[i].rp, REG_DEAD);

		R_UNLOCK(dbenv, &dbmp->reginfo);

		for (i = 0; i < dbmp->nc_reg; ++i)
			if (dbmp->c_reginfo[i].id != REG_ID_INVALID)
				(void)__db_r_detach(
				    dbenv, &dbmp->c_reginfo[i], 0);
		__os_free(dbmp->c_reginfo,
		    dbmp->nc_reg * sizeof(*dbmp->c_reginfo));
	}
	__os_free(dbmp, sizeof(*dbmp));
	return (ret);
}

/*
 * __mpool_init --
 *	Initialize a MPOOL structure in shared memory.
 */
static int
__mpool_init(dbenv, dbmp, nc_reg)
	DB_ENV *dbenv;
	DB_MPOOL *dbmp;
	int nc_reg;
{
	MPOOL *mp;
	int ret;
	void *p;

	if ((ret = __db_shalloc(dbmp->reginfo.addr,
	    sizeof(*mp), 0, &dbmp->reginfo.primary)) != 0)
		return (ret);
	dbmp->reginfo.rp->primary =
	    R_OFFSET(&dbmp->reginfo, dbmp->reginfo.primary);
	mp = dbmp->reginfo.primary;
	memset(mp, 0, sizeof(*mp));

	SH_TAILQ_INIT(&mp->mpfq);
	if ((ret = __db_mutex_init(dbenv, &mp->sync_mutex,
	    R_OFFSET(&dbmp->reginfo, &mp->sync_mutex) + DB_FCNTL_OFF_MPOOL,
	    0)) != 0)
		return (ret);

	ZERO_LSN(mp->lsn);
	mp->lsn_cnt = 0;

	mp->nc_reg = nc_reg;
	if ((ret = __db_shalloc(
	    dbmp->reginfo.addr, nc_reg * sizeof(int), 0, &p)) != 0) {
		__db_shalloc_free(dbmp->reginfo.addr, dbmp->reginfo.primary);
		return (ret);
	}
	mp->c_regids = R_OFFSET(&dbmp->reginfo, p);

	return (0);
}

/*
 * __mcache_init --
 *	Initialize a MCACHE structure in shared memory.
 */
static int
__mcache_init(dbenv, dbmp, htab_buckets, reginfo_off)
	DB_ENV *dbenv;
	DB_MPOOL *dbmp;
	int htab_buckets, reginfo_off;
{
	DB_HASHTAB *htab;
	MCACHE *mc;
	MPOOL *mp;
	REGINFO *reginfo;
	int ret;

	COMPQUIET(dbenv, NULL);

	mp = dbmp->reginfo.primary;

	reginfo = &dbmp->c_reginfo[reginfo_off];
	if ((ret = __db_shalloc(reginfo->addr,
	    sizeof(*mc), 0, &reginfo->primary)) != 0)
		return (ret);
	reginfo->rp->primary = R_OFFSET(reginfo, reginfo->primary);
	mc = reginfo->primary;
	memset(mc, 0, sizeof(*mc));

	SH_TAILQ_INIT(&mc->bhq);

	/* Allocate hash table space and initialize it. */
	if ((ret = __db_shalloc(reginfo->addr,
	    htab_buckets * sizeof(DB_HASHTAB), 0, &htab)) != 0) {
		__db_shalloc_free(reginfo->addr, reginfo->primary);
		return (ret);
	}
	__db_hashinit(htab, htab_buckets);
	mc->htab = R_OFFSET(reginfo, htab);
	mc->htab_buckets = htab_buckets;

	return (0);
}

/*
 * __memp_close --
 *	Internal version of memp_close: only called from DB_ENV->close.
 *
 * PUBLIC: int __memp_close __P((DB_ENV *));
 */
int
__memp_close(dbenv)
	DB_ENV *dbenv;
{
	DB_MPOOL *dbmp;
	DB_MPOOLFILE *dbmfp;
	DB_MPREG *mpreg;
	int i, ret, t_ret;

	ret = 0;
	dbmp = dbenv->mp_handle;

	/* Discard DB_MPREGs. */
	while ((mpreg = LIST_FIRST(&dbmp->dbregq)) != NULL) {
		LIST_REMOVE(mpreg, q);
		__os_free(mpreg, sizeof(DB_MPREG));
	}

	/* Discard DB_MPOOLFILEs. */
	while ((dbmfp = TAILQ_FIRST(&dbmp->dbmfq)) != NULL)
		if ((t_ret = memp_fclose(dbmfp)) != 0 && ret == 0)
			ret = t_ret;

	/* Discard the thread mutex. */
	if (dbmp->mutexp != NULL)
		__db_mutex_free(dbenv, &dbmp->reginfo, dbmp->mutexp);

	/* Detach from the region(s). */
	for (i = 0; i < dbmp->nc_reg; ++i)
		if ((t_ret = __db_r_detach(
		    dbenv, &dbmp->c_reginfo[i], 0)) != 0 && ret == 0)
			ret = t_ret;
	if ((t_ret = __db_r_detach(dbenv, &dbmp->reginfo, 0)) != 0 && ret == 0)
		ret = t_ret;

	__os_free(dbmp->c_reginfo, dbmp->nc_reg * sizeof(*dbmp->c_reginfo));
	__os_free(dbmp, sizeof(*dbmp));

	return (ret);
}
