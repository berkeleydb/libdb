/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)crdel_rec.c	11.17 (Sleepycat) 11/15/99";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <errno.h>
#include <string.h>
#endif

#include "db_int.h"
#include "db_page.h"
#include "log.h"
#include "hash.h"
#include "mp.h"
#include "db_dispatch.h"

/*
 * __crdel_fileopen_recover --
 *	Recovery function for fileopen.
 *
 * PUBLIC: int __crdel_fileopen_recover
 * PUBLIC:   __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
 */
int
__crdel_fileopen_recover(dbenv, dbtp, lsnp, redo, info)
	DB_ENV *dbenv;
	DBT *dbtp;
	DB_LSN *lsnp;
	int redo;
	void *info;
{
	__crdel_fileopen_args *argp;
	DBMETA ondisk;
	DB_FH fh;
	ssize_t nr;
	int do_unlink, ret;
	u_int32_t b, mb, io;
	char *real_name;

	COMPQUIET(info, NULL);

	real_name = NULL;
	REC_PRINT(__crdel_fileopen_print);

	if ((ret = __crdel_fileopen_read(dbtp->data, &argp)) != 0)
		goto out;
	/*
	 * If this is an in-memory database, then the name is going to
	 * be NULL, which looks like a 0-length name in recovery.
	 */
	if (argp->name.size == 0)
		goto done;

	if ((ret = __db_appname(dbenv, DB_APP_DATA,
	    NULL, argp->name.data, 0, NULL, &real_name)) != 0)
		goto out;
	if (redo) {
		/*
		 * The create commited, so we need to make sure that the file
		 * exists.  A simple open should suffice.
		 */
		if ((ret = __os_open(real_name,
		    DB_OSO_CREATE, argp->mode, &fh)) != 0)
			goto out;
		if ((ret = __os_closehandle(&fh)) != 0)
			goto out;
	} else if (!redo) {
		/*
		 * If the file is 0-length then it was in the process of being
		 * created, so we should unlink it.  If it is non-0 length, then
		 * either someone else created it and we need to leave it
		 * untouched or we were in the process of creating it, allocated
		 * the first page on a system that requires you to actually
		 * write pages as you allocate them, but never got any data
		 * on it.
		 * If the file doesn't exist, we never got around to creating
		 * it, so that's fine.
		 */
		if (__os_exists(real_name, NULL) != 0)
			goto done;

		if ((ret = __os_open(real_name, 0, 0, &fh)) != 0)
			goto out;
		if ((ret = __os_ioinfo(real_name, &fh, &mb, &b, &io)) != 0)
			goto out;
		do_unlink = 0;
		if (mb != 0 || b != 0) {
			/*
			 * We need to read the first page to see if its got
			 * valid data on it.
			 */
			if ((ret = __os_read(&fh,
			    &ondisk, sizeof(ondisk), &nr)) != 0 ||
			    nr != sizeof(ondisk))
				goto out;
			if (ondisk.magic == 0)
				do_unlink = 1;
		}
		if ((ret = __os_closehandle(&fh)) != 0)
			goto out;
		/* Check for 0-length and if it is, delete it. */
		if (do_unlink || (mb == 0 && b == 0))
			if ((ret = __os_unlink(real_name)) != 0)
				goto out;
	}

done:	*lsnp = argp->prev_lsn;
	ret = 0;

out:	if (argp != NULL)
		__os_free(argp, sizeof(*argp));
	if (real_name != NULL)
		__os_freestr(real_name);
	return (ret);
}

/*
 * __crdel_metasub_recover --
 *	Recovery function for metasub.
 *
 * PUBLIC: int __crdel_metasub_recover
 * PUBLIC:   __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
 */
int
__crdel_metasub_recover(dbenv, dbtp, lsnp, redo, info)
	DB_ENV *dbenv;
	DBT *dbtp;
	DB_LSN *lsnp;
	int redo;
	void *info;
{
	__crdel_metasub_args *argp;
	DB *file_dbp;
	DBC *dbc;
	DB_MPOOLFILE *mpf;
	PAGE *pagep;
	int cmp_n, cmp_p, modified, ret;

	COMPQUIET(info, NULL);
	REC_PRINT(__crdel_metasub_print);
	REC_INTRO(__crdel_metasub_read, 0);

	if ((ret = memp_fget(mpf, &argp->pgno, 0, &pagep)) != 0) {
		if (redo) {
			if ((ret = memp_fget(mpf,
			    &argp->pgno, DB_MPOOL_CREATE, &pagep)) != 0)
				goto out;
		} else {
			*lsnp = argp->prev_lsn;
			ret = 0;
			goto out;
		}
	}

	modified = 0;
	cmp_n = log_compare(lsnp, &LSN(pagep));
	cmp_p = log_compare(&LSN(pagep), &argp->lsn);

	if (cmp_p == 0 && redo) {
		memcpy(pagep, argp->page.data, argp->page.size);
		LSN(pagep) = *lsnp;
		modified = 1;
	} else if (cmp_n == 0 && !redo) {
		/*
		 * We want to undo this page creation.  The page creation
		 * happened in two parts.  First, we called __bam_new which
		 * was logged separately. Then we wrote the meta-data onto
		 * the page.  So long as we restore the LSN, then the recovery
		 * for __bam_new will do everything else.
		 */
		LSN(pagep) = argp->lsn;
		modified = 1;
	}
	if ((ret = memp_fput(mpf, pagep, modified ? DB_MPOOL_DIRTY : 0)) != 0)
		goto out;

done:	*lsnp = argp->prev_lsn;
	ret = 0;

out:	REC_CLOSE;
}

/*
 * __crdel_metapage_recover --
 *	Recovery function for metapage.
 *
 * PUBLIC: int __crdel_metapage_recover
 * PUBLIC:   __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
 */
int
__crdel_metapage_recover(dbenv, dbtp, lsnp, redo, info)
	DB_ENV *dbenv;
	DBT *dbtp;
	DB_LSN *lsnp;
	int redo;
	void *info;
{
	__crdel_metapage_args *argp;
	DB *dbp;
	DBMETA *meta, ondisk;
	DB_FH fh;
	ssize_t nr;
	u_int32_t b, io, mb, pagesize;
	int is_done, ret;
	char *real_name;

	COMPQUIET(info, NULL);

	real_name = NULL;
	memset(&fh, 0, sizeof(fh));
	REC_PRINT(__crdel_metapage_print);

	if ((ret = __crdel_metapage_read(dbtp->data, &argp)) != 0)
		goto out;

	/*
	 * If this is an in-memory database, then the name is going to
	 * be NULL, which looks like a 0-length name in recovery.
	 */
	if (argp->name.size == 0)
		goto done;

	meta = (DBMETA *)argp->page.data;
	__ua_memcpy(&pagesize, &meta->pagesize, sizeof(pagesize));

	if ((ret = __db_appname(dbenv, DB_APP_DATA,
	    NULL, argp->name.data, 0, NULL, &real_name)) != 0)
		goto out;
	if (redo) {
		/*
		 * We simply read the first page and if the LSN is 0, we
		 * write the meta-data page.
		 */
		if ((ret = __os_open(real_name, 0, 0, &fh)) != 0)
			goto out;
		if ((ret = __os_seek(&fh,
		    pagesize, argp->pgno, 0, 0, DB_OS_SEEK_SET)) != 0)
			goto out;
		/*
		 * If the read succeeds then the page exists, then we need
		 * to vrify that the page has actually been written, because
		 * on some systems (e.g., Windows) we preallocate pages because
		 * files aren't allowed to have holes in them.  If the page
		 * looks good then we're done.
		 */
		if ((ret = __os_read(&fh, &ondisk, sizeof(ondisk), &nr)) == 0 &&
		    nr == sizeof(ondisk)) {
			if (ondisk.magic != 0)
				goto done;
			if ((ret = __os_seek(&fh,
			    pagesize, argp->pgno, 0, 0, DB_OS_SEEK_SET)) != 0)
				goto out;
		}

		/*
		 * Page didn't exist, update the LSN and write a new one.
		 * (seek pointer shouldn't have moved)
		 */
		__ua_memcpy(&meta->lsn, lsnp, sizeof(DB_LSN));
		if ((ret = __os_write(&fh,
		    argp->page.data, argp->page.size, &nr)) != 0)
			goto out;
		if (nr != (ssize_t)argp->page.size) {
			__db_err(dbenv, "Write failed during recovery");
			ret = EIO;
			goto out;
		}
		/* Handle will be closed on exit. */
	} else if (!redo) {
		is_done = 0;

		/* If file does not exist, there is nothing to undo. */
		if (__os_exists(real_name, NULL) != 0)
			goto done;

		/*
		 * Before we can look at anything on disk, we have to check
		 * if there is a valid dbp for this, and if there is, we'd
		 * better flush it.
		 */
		if ((ret =
		    __db_fileid_to_db(dbenv, &dbp, argp->fileid, 0)) == 0)
			(void)dbp->sync(dbp, 0);

		/*
		 * We need to make sure that we do not remove a file that
		 * someone else created.   If the file is 0-length, then we
		 * can assume that we created it and remove it.  If it is
		 * not 0-length, then we need to check the LSN and make
		 * sure that it's the file we created.
		 */
		if ((ret = __os_open(real_name, 0, 0, &fh)) != 0)
			goto out;
		if ((ret = __os_ioinfo(real_name, &fh, &mb, &b, &io)) != 0)
			goto out;
		if (mb != 0 || b != 0) {
			/* The file has something in it. */
			if ((ret = __os_seek(&fh,
			    pagesize, argp->pgno, 0, 0, DB_OS_SEEK_SET)) != 0)
				goto out;
			if ((ret = __os_read(&fh,
			    &ondisk, sizeof(ondisk), &nr)) != 0)
				goto out;
			if (log_compare(&ondisk.lsn, lsnp) != 0)
				is_done = 1;
		}

		/*
		 * Must close here, because unlink with the file open fails
		 * on some systems.
		 */
		if ((ret = __os_closehandle(&fh)) != 0)
			goto out;

		if (!is_done) {
			/*
			 * On some systems, you cannot unlink an open file so
			 * we close the fd in the dbp here and make sure we
			 * don't try to close it again.  First, check for a
			 * saved_open_fhp, then close down the mpool.
			 */
			if (dbp->saved_open_fhp != NULL &&
			    F_ISSET(dbp->saved_open_fhp, DB_FH_VALID) &&
			    (ret = __os_closehandle(dbp->saved_open_fhp)) != 0)
				goto out;
			if (dbp->mpf != NULL) {
				(void)__memp_fremove(dbp->mpf);
				if ((ret = memp_fclose(dbp->mpf)) != 0)
					goto out;
				F_SET(dbp, DB_AM_DISCARD);
				dbp->mpf = NULL;
			}
			if ((ret = __os_unlink(real_name)) != 0)
				goto out;
		}
	}

done:	*lsnp = argp->prev_lsn;
	ret = 0;

out:	if (argp != NULL)
		__os_free(argp, sizeof(*argp));
	if (real_name != NULL)
		__os_freestr(real_name);
	if (F_ISSET(&fh, DB_FH_VALID))
		(void)__os_closehandle(&fh);
	return (ret);
}

/*
 * __crdel_delete_recover --
 *	Recovery function for delete.
 *
 * PUBLIC: int __crdel_delete_recover
 * PUBLIC:   __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
 */
int
__crdel_delete_recover(dbenv, dbtp, lsnp, redo, info)
	DB_ENV *dbenv;
	DBT *dbtp;
	DB_LSN *lsnp;
	int redo;
	void *info;
{
	__crdel_delete_args *argp;
	int ret;
	char *backup, *real_back, *real_name;

	REC_PRINT(__crdel_delete_print);

	backup = real_back = real_name = NULL;
	if ((ret = __crdel_delete_read(dbtp->data, &argp)) != 0)
		goto out;

	if (redo) {
		/*
		 * On a recovery, as we recreate what was going on, we
		 * recreate the creation of the file.  And so, even though
		 * it committed, we need to delete it.  Try to delete it,
		 * but it is not an error if that delete fails.
		 */
		if ((ret = __db_appname(dbenv, DB_APP_DATA,
		    NULL, argp->name.data, 0, NULL, &real_name)) != 0)
			goto out;
		(void)__os_unlink(real_name);
		/*
		 * The transaction committed, so the only thing that might
		 * be true is that the backup file is still around.  Try
		 * to delete it, but it's not an error if that delete fails.
		 */
		if ((ret =  __db_backup_name(argp->name.data,
		    &backup, lsnp)) != 0)
			goto out;
		if ((ret = __db_appname(dbenv,
		    DB_APP_DATA, NULL, backup, 0, NULL, &real_back)) != 0)
			goto out;
		(void)__os_unlink(real_back);
		if ((ret = __db_txnlist_delete(info,
		    argp->name.data, TXNLIST_INVALID_ID, 1)) != 0)
			goto out;
	} else if (!redo) {
		/*
		 * Trying to undo.  File may or may not have been deleted.
		 * Try to move the backup to the original.  If the backup
		 * exists, then this is right.  If it doesn't exist, then
		 * nothing will happen and that's OK.
		 */
		if ((ret =  __db_backup_name(argp->name.data,
		    &backup, lsnp)) != 0)
			goto out;
		if ((ret = __db_appname(dbenv,
		    DB_APP_DATA, NULL, backup, 0, NULL, &real_back)) != 0)
			goto out;
		if ((ret = __db_appname(dbenv, DB_APP_DATA,
		    NULL, argp->name.data, 0, NULL, &real_name)) != 0)
			goto out;
		(void)__os_rename(real_back, real_name);
	}

	*lsnp = argp->prev_lsn;
	ret = 0;

out:	if (argp != NULL)
		__os_free(argp, sizeof(*argp));
	if (backup != NULL)
		__os_freestr(backup);
	if (real_back != NULL)
		__os_freestr(real_back);
	if (real_name != NULL)
		__os_freestr(real_name);
	return (ret);
}
