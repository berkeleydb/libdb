/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */
#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)log_register.c	11.7 (Sleepycat) 9/30/99";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <errno.h>
#include <string.h>
#endif

#include "db_int.h"
#include "log.h"

/*
 * log_register --
 *	Register a file name.
 */
int
log_register(dbenv, dbp, name, idp)
	DB_ENV *dbenv;
	DB *dbp;
	const char *name;
	int32_t *idp;
{
	DBT fid_dbt, r_name;
	DB_LOG *dblp;
	DB_LSN r_unused;
	FNAME *fnp, *reuse_fnp;
	LOG *lp;
	size_t len;
	int32_t maxid;
	int inserted, ret;
	void *namep;

	PANIC_CHECK(dbenv);
	ENV_REQUIRES_CONFIG(dbenv, dbenv->lg_handle, DB_INIT_LOG);

	dblp = dbenv->lg_handle;
	lp = dblp->reginfo.primary;
	fnp = reuse_fnp = NULL;
	inserted = ret = 0;
	namep = NULL;

	/* Check the arguments. */
	if (dbp->type != DB_BTREE && dbp->type != DB_QUEUE &&
	    dbp->type != DB_HASH && dbp->type != DB_RECNO) {
		__db_err(dbenv, "log_register: unknown DB file type");
		return (EINVAL);
	}

	R_LOCK(dbenv, &dblp->reginfo);

	/*
	 * See if we've already got this file in the log, finding the
	 * (maximum+1) in-use file id and some available file id (if we
	 * find an available fid, we'll use it, else we'll have to allocate
	 * one after the maximum that we found).
	 */
	for (maxid = 0, fnp = SH_TAILQ_FIRST(&lp->fq, __fname);
	    fnp != NULL; fnp = SH_TAILQ_NEXT(fnp, q, __fname)) {
		if (fnp->ref == 0) {		/* Entry is not in use. */
			if (reuse_fnp == NULL)
				reuse_fnp = fnp;
			continue;
		}
		if (!memcmp(dbp->fileid, fnp->ufid, DB_FILE_ID_LEN)) {
			++fnp->ref;
			goto found;
		}
		if (maxid <= fnp->id)
			maxid = fnp->id + 1;
	}

	/* Fill in fnp structure. */
	if (reuse_fnp != NULL)		/* Reuse existing one. */
		fnp = reuse_fnp;
	else {				/* Allocate a new one. */
		if ((ret = __db_shalloc(dblp->reginfo.addr,
		    sizeof(FNAME), 0, &fnp)) != 0)
			goto err;
		fnp->id = maxid;
	}

	fnp->ref = 1;
	fnp->s_type = dbp->type;
	memcpy(fnp->ufid, dbp->fileid, DB_FILE_ID_LEN);

	if (name != NULL) {
		len = strlen(name) + 1;
		if ((ret =
		    __db_shalloc(dblp->reginfo.addr, len, 0, &namep)) != 0)
			goto err;
		fnp->name_off = R_OFFSET(&dblp->reginfo, namep);
		memcpy(namep, name, len);
	} else
		fnp->name_off = INVALID_ROFF;

	/* Only do the insert if we allocated a new fnp. */
	if (reuse_fnp == NULL)
		SH_TAILQ_INSERT_HEAD(&lp->fq, fnp, q, __fname);
	inserted = 1;

	/* Log the registry. */
	if (!F_ISSET(dblp, DBC_RECOVER)) {
		/*
		 * We allow logging on in-memory databases, so the name here
		 * could be NULL.
		 */
		if (name != NULL) {
			r_name.data = (void *)name;
			r_name.size = strlen(name) + 1;
		}
		memset(&fid_dbt, 0, sizeof(fid_dbt));
		fid_dbt.data = dbp->fileid;
		fid_dbt.size = DB_FILE_ID_LEN;
		if ((ret = __log_register_log(dbenv, NULL, &r_unused,
		    0, LOG_OPEN, name == NULL ? NULL : &r_name,
		    &fid_dbt, fnp->id, dbp->type)) != 0)
			goto err;
	}

found:	/*
	 * If we found the entry in the shared area, then the file is
	 * already open, so there is no need to log the open.  We only
	 * log the open and closes on the first open and last close.
	 */
	if (!F_ISSET(dblp, DBC_RECOVER) &&
	    (ret = __log_add_logid(dblp, dbp, fnp->id)) != 0)
			goto err;

	if (idp != NULL)
		*idp = fnp->id;

	if (0) {
err:		if (inserted)
			SH_TAILQ_REMOVE(&lp->fq, fnp, q, __fname);
		if (namep != NULL)
			__db_shalloc_free(dblp->reginfo.addr, namep);
		if (fnp != NULL)
			__db_shalloc_free(dblp->reginfo.addr, fnp);
	}

	R_UNLOCK(dbenv, &dblp->reginfo);

	return (ret);
}

/*
 * log_unregister --
 *	Discard a registered file name.
 */
int
log_unregister(dbenv, fid)
	DB_ENV *dbenv;
	int32_t fid;
{
	DBT fid_dbt, r_name;
	DB_LOG *dblp;
	DB_LSN r_unused;
	FNAME *fnp;
	LOG *lp;
	int ret;

	PANIC_CHECK(dbenv);
	ENV_REQUIRES_CONFIG(dbenv, dbenv->lg_handle, DB_INIT_LOG);

	ret = 0;
	dblp = dbenv->lg_handle;
	lp = dblp->reginfo.primary;

	R_LOCK(dbenv, &dblp->reginfo);

	/* Find the entry in the log. */
	for (fnp = SH_TAILQ_FIRST(&lp->fq, __fname);
	    fnp != NULL; fnp = SH_TAILQ_NEXT(fnp, q, __fname))
		if (fid == fnp->id)
			break;
	if (fnp == NULL) {
		__db_err(dbenv, "log_unregister: non-existent file id");
		ret = EINVAL;
		goto ret1;
	}

	/*
	 * Log the unregistry only if this is the last one and we are
	 * really closing the file.
	 */
	if (!F_ISSET(dblp, DBC_RECOVER) && fnp->ref == 1) {
		if (fnp->name_off != INVALID_ROFF) {
			memset(&r_name, 0, sizeof(r_name));
			r_name.data = R_ADDR(&dblp->reginfo, fnp->name_off);
			r_name.size = strlen(r_name.data) + 1;
		}
		memset(&fid_dbt, 0, sizeof(fid_dbt));
		fid_dbt.data = fnp->ufid;
		fid_dbt.size = DB_FILE_ID_LEN;
		if ((ret = __log_register_log(dbenv, NULL, &r_unused,
		    0, LOG_CLOSE,
		    fnp->name_off == INVALID_ROFF ? NULL : &r_name,
		    &fid_dbt, fid, fnp->s_type)) != 0)
			goto ret1;
	}

	/*
	 * If more than 1 reference, just decrement the reference and return.
	 * Otherwise, free the name if one exists.
	 */
	--fnp->ref;
	if (fnp->ref == 0 && fnp->name_off != INVALID_ROFF)
		__db_shalloc_free(dblp->reginfo.addr,
		    R_ADDR(&dblp->reginfo, fnp->name_off));

	/*
	 * Remove from the process local table.  If this operation is taking
	 * place during recovery, then the logid was never added to the table,
	 * so do not remove it.
	 */
	if (!F_ISSET(dblp, DBC_RECOVER))
		__log_rem_logid(dblp, fid);

ret1:	R_UNLOCK(dbenv, &dblp->reginfo);
	return (ret);
}
