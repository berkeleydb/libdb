/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 *
 *	@(#)db_am.h	11.4 (Sleepycat) 9/19/99
 */
#ifndef _DB_AM_H
#define _DB_AM_H

#define	DB_MINPAGECACHE	10		/* Min pages access methods cache. */

#define DB_ISBIG	0x01		/* DB recovery operation codes. */
#define	DB_ISSUBDB	0x02
#define	DB_ADD_DUP	0x10
#define	DB_REM_DUP	0x20
#define	DB_ADD_BIG	0x30
#define	DB_REM_BIG	0x40
#define	DB_SPLITOLD	0x50
#define	DB_SPLITNEW	0x60
#define	DB_ADD_PAGE	0x70
#define	DB_REM_PAGE	0x80
#define	DB_LOG_CREATE	0x90
#define	DB_LOG_DELETE	0xa0

/*
 * This is a grotesque naming hack.  We have modified the btree page
 * allocation and freeing functions to be generic and have therefore
 * moved them into the access-method independent portion of the code.
 * However, since we didn't want to create new log records and routines
 * for them, we left their logging and recovery functions over in btree.
 * To make the code look prettier, we macro them, but this is sure to
 * confuse the heck out of everyone.
 */
#define __db_pg_alloc_log	__bam_pg_alloc_log
#define __db_pg_free_log	__bam_pg_free_log

/*
 * Standard initialization and shutdown macros for all recovery functions.
 *
 * Requires the following local variables:
 *
 *	DB *file_dbp, *mdbp;
 *	DB_MPOOLFILE *mpf;
 *	int ret;
 */
#define	REC_INTRO(func, inc_count) {					\
	file_dbp = NULL;						\
	dbc = NULL;							\
	if ((ret = func(dbtp->data, &argp)) != 0)			\
		goto out;						\
	if ((ret = __db_fileid_to_db(dbenv,				\
	    &file_dbp, argp->fileid, inc_count)) != 0) {		\
		if (ret	== DB_DELETED) {				\
			ret = 0;					\
			goto done;					\
		}							\
		goto out;						\
	}								\
	if (file_dbp == NULL)						\
		goto out;						\
	if ((ret = file_dbp->cursor(file_dbp, NULL, &dbc, 0)) != 0)	\
		goto out;						\
	F_SET(dbc, DBC_RECOVER);					\
	mpf = file_dbp->mpf;						\
}

#define	REC_CLOSE							\
	if (argp != NULL)						\
		__os_free(argp, sizeof(*argp));				\
	if (dbc != NULL)						\
		dbc->c_close(dbc);					\
	return (ret);

/*
 * No-op versions of the same macros.
 */
#define	REC_NOOP_INTRO(func) {						\
	if ((ret = func(dbtp->data, &argp)) != 0)			\
		return (ret);						\
}
#define	REC_NOOP_CLOSE							\
	if (argp != NULL)						\
		__os_free(argp, sizeof(*argp));				\
	return (ret);							\

/*
 * Standard debugging macro for all recovery functions.
 */
#ifdef DEBUG_RECOVER
#define	REC_PRINT(func)							\
	(void)func(dbenv, dbtp, lsnp, redo, info);
#else
#define	REC_PRINT(func)
#endif

/*
 * If doing transactions we have to hold the locks associated with a data item
 * from a page for the entire transaction.  However, we don't have to hold the
 * locks associated with walking the tree.  Distinguish between the two so that
 * we don't tie up the internal pages of the tree longer than necessary.
 */
#define	__LPUT(dbc, lock)						\
	(F_ISSET((dbc)->dbp->dbenv, DB_ENV_LOCKING) ?			\
	    lock_put((dbc)->dbp->dbenv, &(lock)) : 0)
#define	__TLPUT(dbc, lock)						\
	(F_ISSET((dbc)->dbp->dbenv, DB_ENV_LOCKING) &&			\
	    (dbc)->txn == NULL ? lock_put((dbc)->dbp->dbenv, &(lock)) : 0)

#include "db_auto.h"
#include "crdel_auto.h"
#include "db_ext.h"
#endif
