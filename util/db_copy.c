/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2011 Oracle and/or its affiliates.  All rights reserved.
 *
 * $Id$
 */

#include "db_config.h"
#include "db_int.h"
#include "dbinc/db_page.h"
#include "dbinc/db_am.h"

#ifdef HAVE_QUEUE
#include "dbinc/qam.h"
static int copy_queue_extents __P((DB *, const char *, const char *));
#endif


/*
 * db_copy --
 *	Copy a database file using mpool.
 * EXTERN: int db_copy __P((DB_ENV *,
 * EXTERN:     const char *, const char *, const char *));
 */
int
db_copy(dbenv, dbfile, target, passwd)
	DB_ENV *dbenv;
	const char *dbfile;
	const char *target;
	const char *passwd;
{
	DB *dbp;
	DB_FH *fp;
	DB_MPOOLFILE *mpf;
	ENV *env;
	void *pagep;
	db_pgno_t pgno;
	int ret, t_ret;
	char *path;
	size_t nw;

#ifdef HAVE_QUEUE
	DBTYPE type;
	u_int32_t extentsize;
#endif

	path = NULL;
	dbp = NULL;
	fp = NULL;
	env = dbenv->env;
retry:	if ((ret = db_create(&dbp, dbenv, 0)) != 0)
		return (ret);
	/*
	 * If the database is encrypted we need to encript the pages
	 * before outputting them since we will read decrypted pages.
	 */
	if (passwd != NULL &&
	    (ret = dbp->set_encrypt(dbp, passwd, DB_ENCRYPT_AES)) != 0)
	    	goto err;

	if ((ret = dbp->open(dbp, NULL,
	    dbfile, NULL, DB_UNKNOWN, DB_AUTO_COMMIT | DB_RDONLY, 0)) != 0) {
	    	if (ret == DB_LOCK_DEADLOCK || ret == DB_LOCK_NOTGRANTED) {
			dbenv->errx(dbenv, DB_STR_A("0702",
		    "Deadlock while opening %s, retrying", "%s"), dbfile);
			(void)dbp->close(dbp, DB_NOSYNC);
			goto retry;
		}
		goto err;
	}
	if ((ret = __os_malloc(env,
	     strlen(target) + strlen(dbfile) + 2, &path)) != 0) {
	    	dbenv->err(dbenv, ret, DB_STR_A("0703", 
		    "Cannot allocate space for path: %s", "%s"), target);
		goto err;
	}

	(void)strcpy(path, target);
	(void)strncat(path, &PATH_SEPARATOR[0], 1);
	(void)strcat(path, dbfile);

	if ((ret = __os_open(env,
	    path, 0, DB_OSO_CREATE | DB_OSO_TRUNC, DB_MODE_600, &fp)) != 0) {
	    	dbenv->err(dbenv, ret, DB_STR_A("0704",
		    "Cannot open traget file: %s", "%s"), path);
		goto err;
	}

	mpf = dbp->get_mpf(dbp);
	for (pgno = 0; ret == 0 ; pgno++) {
		if ((ret = mpf->get(mpf, &pgno, NULL, 0, &pagep)) != 0)
			break;
		if (F_ISSET(dbp, DB_AM_CHKSUM) || passwd != NULL) 
			ret = __db_encrypt_and_checksum_pg(env, dbp, pagep);
		if (ret == 0 && ((ret = __os_write(env,
		    fp, pagep, dbp->pgsize, &nw)) != 0 || nw != dbp->pgsize)) {
			if (ret == 0)
				ret = EIO;
		}
		if ((t_ret = mpf->put(mpf,
		    pagep, DB_PRIORITY_VERY_LOW, 0)) != 0 && ret == 0)
			ret = t_ret;
	}

	if (ret == DB_PAGE_NOTFOUND)
		ret = 0;

#ifdef HAVE_QUEUE
	/* Queue exents cannot be read directly, use the internal interface. */
	if (ret == 0) {
		if ((ret = dbp->get_type(dbp, &type) != 0))
			goto err;
		if (type == DB_QUEUE &&
		    (ret = dbp->get_q_extentsize(dbp, &extentsize)) == 0 &&
		    extentsize != 0)
			ret = copy_queue_extents(dbp, target, passwd);
	}
#endif
	/* We have read pages for which log records may still be in cache. */
	if (ret == 0)
		ret = dbenv->log_flush(dbenv, NULL);

err:	if (path != NULL)
		__os_free(env, path);
	if (fp != NULL && (t_ret = __os_closehandle(env, fp)) != 0 && ret == 0)
		ret = t_ret;
	if (dbp != NULL &&
	    (t_ret = dbp->close(dbp, DB_NOSYNC)) != 0 && ret == 0)
		ret = t_ret;
	return (ret);
}

#ifdef HAVE_QUEUE
/*
 * copy_queue_extents --
 *	Routine to safely copy the active queue extents of a database.
 * This routine must use internal BDB interfaces.
 */
static int
copy_queue_extents(dbp, target, passwd)
	DB *dbp;
	const char *target;
	const char *passwd;
{
	DBC *dbc;
	DB_ENV *dbenv;
	DB_FH *fp;
	DB_QUEUE_STAT *sp;
	ENV *env;
	void *pagep;
	db_recno_t current, first;
	db_pgno_t pgno, stop;
	u_int32_t extid, page_ext;
	char *path;
	size_t nw;
	int ret, t_ret;

	/* Find out the first and last record numbers in the database. */
	if ((ret = dbp->stat(dbp, NULL, &sp, DB_FAST_STAT)) != 0)
		return (ret);

	current = sp->qs_cur_recno;
	first = sp->qs_first_recno;
	page_ext = sp->qs_extentsize;

	dbenv = dbp->dbenv;
	env = dbp->env;
	fp = NULL;
	if ((ret = dbp->cursor(dbp, NULL, &dbc, 0)) != 0)
		return (ret);
	if ((ret = __os_malloc(env,
	     strlen(target) + strlen(dbp->fname) + strlen(QUEUE_EXTENT),
	     &path)) != 0) {
	    	dbenv->err(dbenv, ret, DB_STR_A("0705",
		    "Cannot allocate space for path: %s", "%s"), target);
		goto err;
	}

	extid = UINT32_MAX;
again:
	if (current >= first)
		stop = QAM_RECNO_PAGE(dbp, current);
	else
		stop = QAM_RECNO_PAGE(dbp, UINT32_MAX);

	for (pgno = QAM_RECNO_PAGE(dbp, first); pgno <= stop; pgno++) {
		if (extid != QAM_PAGE_EXTENT(dbp, pgno)) {
			if (fp != NULL &&
			    (ret = __os_closehandle(env, fp)) != 0)
				goto err;
			fp = NULL;
			extid = QAM_PAGE_EXTENT(dbp, pgno);
			(void)sprintf(path, QUEUE_EXTENT,
			    target, PATH_SEPARATOR[0], dbp->fname, extid);
			if ((ret = __os_open(env, path, 0,
			     DB_OSO_CREATE | DB_OSO_TRUNC,
			     DB_MODE_600, &fp)) != 0) {
				dbenv->err(dbenv, ret,  DB_STR_A("0706",
				    "Cannot open traget file: %s", "%s"), path);
				goto err;
			}
		}
		ret = __qam_fget(dbc, &pgno, 0, &pagep);

		/*
		 * Skip to the next extent if this extent has not
		 * been created yet or if it is not completly populated.
		 */
		if (ret == DB_PAGE_NOTFOUND || ret == ENOENT) {
			/*
			 * Compute the page number of the first page in
			 * the next extent.
			 */
			pgno = QAM_PAGE_EXTENT(
			    dbp, pgno + page_ext) * page_ext;
			/* Decrement, the loop will increment. */
			pgno--;
			ret = 0;
			continue;
		}
		if (ret != 0)
			goto err;

		if (F_ISSET(dbp, DB_AM_CHKSUM) || passwd != NULL) 
			ret = __db_encrypt_and_checksum_pg(env, dbp, pagep);
		if (ret == 0 && ((ret = __os_write(env,
		    fp, pagep, dbp->pgsize, &nw)) != 0 || nw != dbp->pgsize)) {
			if (ret == 0)
				ret = EIO;
		    	dbenv->err(dbenv, ret,  DB_STR_A("0707",
			     "Failed to write page %lu output to %s", "%s"),
			     (u_long)pgno, path);
		}
		if ((t_ret = __qam_fput(dbc,
		    pgno, pagep, DB_PRIORITY_VERY_LOW)) != 0 && ret == 0)
			ret = t_ret;
		if (ret != 0)
			goto err;
	}

	if (current < first) {
		first = 1;
		goto again;
	}

err:	if (fp != NULL && (t_ret = __os_closehandle(env, fp)) != 0 && ret == 0)
		ret = t_ret;

	if (dbc != NULL && (t_ret = dbc->close(dbc)) != 0 && ret == 0)
		ret = t_ret;

	__os_free(env, path);
	return (ret);
}
#endif
