/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */
/*
 * Copyright (c) 1990, 1993, 1994, 1995, 1996
 *	Keith Bostic.  All rights reserved.
 */
/*
 * Copyright (c) 1990, 1993, 1994, 1995
 *	The Regents of the University of California.  All rights reserved.
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
static const char sccsid[] = "@(#)db.c	11.31 (Sleepycat) 11/12/99";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <errno.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#endif

#include "db_int.h"
#include "db_page.h"
#include "db_shash.h"
#include "db_swap.h"
#include "btree.h"
#include "db_am.h"
#include "hash.h"
#include "lock.h"
#include "log.h"
#include "mp.h"
#include "qam.h"

static int __db_dbopen __P((DB *, const char *, u_int32_t, int, db_pgno_t));
static int __db_dbenv_setup __P((DB *, const char *, u_int32_t));
static int __db_file_setup __P((DB *,
	       const char *, u_int32_t, int, db_pgno_t, int *));
static int __db_master_open __P((DB_ENV *,
	       DB_TXN *, const char *, u_int32_t, int, DB **));
static int __db_master_update __P((DB *,
	       const char *, u_int32_t, db_pgno_t *, int, u_int32_t));
static int __db_metabegin __P((DB *, DB_LOCK *));
static int __db_metaend __P((DB *,
	       DB_LOCK *, int, int (*)(DB *, void *), void *));
static int __db_refresh __P((DB *));
static int __db_remove_callback __P((DB *, void *));
static int __db_set_pgsize __P((DB *, DB_FH *, char *));
static int __db_subdb_remove __P((DB *, const char *, const char *));
#if     CONFIG_TEST
static void __db_makecopy __P((const char *, const char *));
#endif

/*
 * __db_open --
 *	Main library interface to the DB access methods.
 *
 * PUBLIC: int __db_open __P((DB *,
 * PUBLIC:     const char *, const char *, DBTYPE, u_int32_t, int));
 */
int
__db_open(dbp, name, subdb, type, flags, mode)
	DB *dbp;
	const char *name, *subdb;
	DBTYPE type;
	u_int32_t flags;
	int mode;
{
	DB_ENV *dbenv;
	DB_LOCK open_lock;
	DB *mdbp;
	db_pgno_t meta_pgno;
	u_int32_t ok_flags;
	int ret, t_ret;

	dbenv = dbp->dbenv;
	mdbp = NULL;

	/* Validate arguments. */
#define	OKFLAGS								\
    (DB_CREATE | DB_EXCL | DB_FCNTL_LOCKING |				\
    DB_NOMMAP | DB_RDONLY | DB_THREAD | DB_TRUNCATE)
	if ((ret = __db_fchk(dbenv, "DB->open", flags, OKFLAGS)) != 0)
		return (ret);
	if (LF_ISSET(DB_EXCL) && !LF_ISSET(DB_CREATE))
		return (__db_ferr(dbenv, "DB->open", 1));
	if (LF_ISSET(DB_RDONLY) && LF_ISSET(DB_CREATE))
		return (__db_ferr(dbenv, "DB->open", 1));

	switch (type) {
	case DB_UNKNOWN:
		ok_flags = 0;
		break;
	case DB_BTREE:
		ok_flags = DB_OK_BTREE;
		break;
	case DB_HASH:
		ok_flags = DB_OK_HASH;
		break;
	case DB_QUEUE:
		ok_flags = DB_OK_QUEUE;
		break;
	case DB_RECNO:
		ok_flags = DB_OK_RECNO;
		break;
	default:
		__db_err(dbp->dbenv, "unknown type: %lu", type);
		return (EINVAL);
	}
	if (ok_flags)
		DB_ILLEGAL_METHOD(dbp, ok_flags);

	/* The environment may have been created, but never opened. */
	if (!F_ISSET(dbenv, DB_ENV_DBLOCAL | DB_ENV_OPEN_CALLED)) {
		__db_err(dbenv, "environment not yet opened");
		return (EINVAL);
	}

	/*
	 * Historically, you could pass in an environment that didn't have a
	 * mpool, and DB would create a private one behind the scenes.  This
	 * no longer works.
	 */
	if (!F_ISSET(dbenv, DB_ENV_DBLOCAL) && dbenv->mp_handle == NULL) {
		__db_err(dbenv, "environment did not include a memory pool.");
		return (EINVAL);
	}

	/*
	 * You can't specify threads during DB->open if subsystems in the
	 * environment weren't configured with them.
	 */
	if (LF_ISSET(DB_THREAD) &&
	    !F_ISSET(dbenv, DB_ENV_DBLOCAL | DB_ENV_THREAD)) {
		__db_err(dbenv, "environment not created using DB_THREAD");
		return (EINVAL);
	}

	/* DB_TRUNCATE is not transaction recoverable. */
	if (LF_ISSET(DB_TRUNCATE) && F_ISSET(dbenv, DB_ENV_TXN)) {
		__db_err(dbenv,
	    "DB_TRUNCATE illegal in a transaction protected environment");
		return (EINVAL);
	}

	/* Subdatabase checks. */
	if (subdb != NULL) {
		/* Subdatabases must be created in named files. */
		if (name == NULL) {
			__db_err(dbenv,
		    "subdatabases cannot be created in temporary files");
			return (EINVAL);
		}

		/* QAM can't be done as a subdatabase. */
		if (type == DB_QUEUE) {
			__db_err(dbenv, "subdatabases cannot be queue files");
			return (EINVAL);
		}
	}

	/* Convert any DB->open flags. */
	if (LF_ISSET(DB_RDONLY))
		F_SET(dbp, DB_AM_RDONLY);

	/* Fill in the type. */
	dbp->type = type;

	/*
	 * If we're potentially creating a database, wrap the open inside of
	 * a transaction.
	 */
	if (F_ISSET(dbenv, DB_ENV_TXN) && LF_ISSET(DB_CREATE))
		if ((ret = __db_metabegin(dbp, &open_lock)) != 0)
			return (ret);

	/*
	 * If we're opening a subdatabase, we have to open (and potentially
	 * create) the main database, and then get (and potentially store)
	 * our base page number in that database.  Then, we can finally open
	 * the subdatabase.
	 */
	if (subdb == NULL)
		meta_pgno = PGNO_BASE_MD;
	else {
		/*
		 * Open the master database, optionally updating it, and
		 * retrieving the metadata page number.
		 */
		if ((ret = __db_master_open(dbp->dbenv, dbp->open_txn,
		    name, flags, mode, &mdbp)) != 0)
			goto err;

		/* Copy the page size and file id from the master. */
		dbp->pgsize = mdbp->pgsize;
		F_SET(dbp, DB_AM_SUBDB);
		memcpy(dbp->fileid, mdbp->fileid, DB_FILE_ID_LEN);

		if ((ret = __db_master_update(mdbp,
		    subdb, type, &meta_pgno, 0, flags)) != 0)
			goto err;

		/*
		 * Clear the exclusive open and truncation flags, they only
		 * apply to the open of the master database.
		 */
		LF_CLR(DB_EXCL | DB_TRUNCATE);
	}

	ret = __db_dbopen(dbp, name, flags, mode, meta_pgno);

	/*
	 * You can open the database that describes the subdatabases in the
	 * rest of the file read-only.  The content of each key's data is
	 * unspecified and applications should never be adding new records
	 * or updating existing records.  However, during recovery, we need
	 * to open these databases R/W so we can redo/undo changes in them.
	 */
	if (subdb == NULL &&
	    (dbenv->lg_handle == NULL ||
	    !F_ISSET((DB_LOG *)(dbenv->lg_handle), DBC_RECOVER)) &&
	    !LF_ISSET(DB_RDONLY) && F_ISSET(dbp, DB_AM_SUBDB)) {
		__db_err(dbenv,
    "databases containing subdatabase lists may only be opened read-only");
		ret = EINVAL;
		goto err;
	}

err:	/*
	 * End any transaction, committing if we were successful, aborting
	 * otherwise.
	 */
	if (F_ISSET(dbenv, DB_ENV_TXN) && LF_ISSET(DB_CREATE))
		if ((t_ret = __db_metaend(dbp,
		    &open_lock, ret == 0, NULL, NULL)) != 0 && ret == 0)
			ret = t_ret;

	/* If we were successful, don't discard the file on close. */
	if (ret == 0)
		F_CLR(dbp, DB_AM_DISCARD);

	/* If we were unsuccessful, destroy the DB handle. */
	if (ret != 0)
		__db_refresh(dbp);

	if (mdbp != NULL) {
		/* If we were successful, don't discard the file on close. */
		if (ret == 0)
			F_CLR(mdbp, DB_AM_DISCARD);
		if ((t_ret = mdbp->close(mdbp, 0)) != 0 && ret == 0)
			ret = t_ret;
	}

	return (ret);
}

/*
 * __db_dbopen --
 *	Open a database.
 */
static int
__db_dbopen(dbp, name, flags, mode, meta_pgno)
	DB *dbp;
	const char *name;
	u_int32_t flags;
	int mode;
	db_pgno_t meta_pgno;
{
	DB_ENV *dbenv;
	int ret;
	int zero_length;

	dbenv = dbp->dbenv;

	/* Set up the underlying file. */
	if ((ret = __db_file_setup(dbp,
	    name, flags, mode, meta_pgno, &zero_length)) != 0)
		return (ret);

	/* Set up the underlying environment. */
	if ((ret = __db_dbenv_setup(dbp, name, flags)) != 0)
		return (ret);

	/*
	 * Do access method specific initialization.
	 *
	 * !!!
	 * Set the open flag.  (The underlying access method open functions
	 * may want to do things like acquire cursors, so the open flag has
	 * to be set before calling them.)
	 */
	F_SET(dbp, DB_OPEN_CALLED);

	if (zero_length)
		return (0);

	switch (dbp->type) {
	case DB_BTREE:
		ret = __bam_open(dbp, name, meta_pgno);
		break;
	case DB_HASH:
		ret = __ham_open(dbp, name, meta_pgno);
		break;
	case DB_RECNO:
		ret = __ram_open(dbp, name, meta_pgno);
		break;
	case DB_QUEUE:
		ret = __qam_open(dbp, name, meta_pgno);
		break;
	case DB_UNKNOWN:
		ret = EINVAL;		/* Shouldn't be possible. */
		break;
	}
	return (ret);
}

/*
 * __db_master_open --
 *	Open up a handle on a master database.
 */
static int
__db_master_open(dbenv, txn, name, flags, mode, dbpp)
	DB_ENV *dbenv;
	DB_TXN *txn;
	const char *name;
	u_int32_t flags;
	int mode;
	DB **dbpp;
{
	DB *dbp;
	int ret;

	/*
	 * Open up a handle on the main database.
	 */
	if ((ret = db_create(
	    &dbp, F_ISSET(dbenv, DB_ENV_DBLOCAL) ? NULL : dbenv, 0)) != 0)
		return (ret);
	dbp->open_txn = txn;

	/*
	 * It's always a btree; flag that we're creating a database with
	 * subdatabases.
	 */
	dbp->type = DB_BTREE;
	F_SET(dbp, DB_AM_SUBDB);

	ret = __db_dbopen(dbp, name, flags, mode, PGNO_BASE_MD);

	*dbpp = dbp;
	return (ret);
}

/*
 * __db_master_update --
 *	Add/Remove a subdatabase from a master database.
 */
static int
__db_master_update(mdbp, subdb, type, meta_pgnop, is_remove, flags)
	DB *mdbp;
	const char *subdb;
	u_int32_t type;
	db_pgno_t *meta_pgnop;		/* !NULL if creating/reading. */
	int is_remove;
	u_int32_t flags;
{
	DBC *dbc;
	DBT key, data;
	PAGE *p;
	int ret, t_ret;

	dbc = NULL;
	p = NULL;

	/* Open up a cursor. */
	if ((ret = mdbp->cursor(mdbp, mdbp->open_txn, &dbc, 0)) != 0)
		goto err;

	/*
	 * Try to point the cursor at the record.
	 *
	 * If we're removing or potentially creating an entry, lock the page
	 * with DB_RMW.
	 *
	 * !!!
	 * We don't include the name's nul termination in the database.
	 */
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = (char *)subdb;
	key.size = strlen(subdb);
	ret = dbc->c_get(dbc, &key, &data, DB_SET |
	    (meta_pgnop == NULL || (F_ISSET(
	    mdbp->dbenv, DB_ENV_LOCKING) && LF_ISSET(DB_CREATE)) ? DB_RMW : 0));

	if (is_remove) {
		/* We should have found something if we're removing it. */
		if (ret != 0)
			goto err;

		memcpy(meta_pgnop, data.data, sizeof(db_pgno_t));

		/* Delete the subdatabase entry. */
		if ((ret = dbc->c_del(dbc, 0)) != 0)
			goto err;

		if ((ret = memp_fget(mdbp->mpf, meta_pgnop, 0, &p)) != 0)
			goto err;

		/* Free and put the page. */
		if ((ret = __db_free(dbc, p)) != 0)
			goto err;
		p = NULL;
	} else {
		/*
		 * Get the subdatabase information.  If it already exists,
		 * copy out the page number and we're done.
		 */
		switch (ret) {
		case 0:
			memcpy(meta_pgnop, data.data, sizeof(db_pgno_t));
			goto done;
		case DB_NOTFOUND:
			if (LF_ISSET(DB_CREATE))
				break;
			ret = ENOENT;
			goto err;
		default:
			goto err;
		}

		if ((ret = __db_new(dbc,
		    type == DB_HASH ? P_HASHMETA : P_BTREEMETA, &p)) != 0)
			goto err;
		data.data = &PGNO(p);
		data.size = sizeof(db_pgno_t);
		if ((ret = dbc->c_put(dbc, &key, &data, DB_KEYLAST)) != 0)
			goto err;

		*meta_pgnop = PGNO(p);
	}

err:
done:	/*
	 * If we allocated a page: if we're successful, mark the page dirty
	 * and return it to the cache, otherwise, discard/free it.
	 */
	if (p != NULL) {
		if (ret == 0) {
			if ((t_ret =
			    memp_fput(mdbp->mpf, p, DB_MPOOL_DIRTY)) != 0)
				ret = t_ret;
			/*
			 * Since we cannot close this file until after
			 * transaction commit, we need to sync the dirty
			 * pages, because we'll read these directly from
			 * disk to open.
			 */
			if ((t_ret = mdbp->sync(mdbp, 0)) != 0 && ret == 0)
				ret = t_ret;
		} else
			(void)__db_free(dbc, p);
	}

	/* Discard the cursor. */
	if (dbc != NULL && (t_ret = dbc->c_close(dbc)) != 0 && ret == 0)
		ret = t_ret;

	return (ret);
}

/*
 * __db_dbenv_setup --
 *	Set up the underlying environment during a db_open.
 */
static int
__db_dbenv_setup(dbp, name, flags)
	DB *dbp;
	const char *name;
	u_int32_t flags;
{
	DB_ENV *dbenv;
	DBT pgcookie;
	DB_MPOOL_FINFO finfo;
	DB_PGINFO pginfo;
	int ret;

	dbenv = dbp->dbenv;

	/* If the environment is local, it's time to create it. */
	if (F_ISSET(dbenv, DB_ENV_DBLOCAL)) {
		/* Make sure we have at least DB_MINCACHE pages in our cache. */
		if (dbenv->mp_gbytes == 0 &&
		    dbenv->mp_bytes < dbp->pgsize * DB_MINPAGECACHE &&
		    (ret = dbenv->set_cachesize(
		    dbenv, 0, dbp->pgsize * DB_MINPAGECACHE, 0)) != 0)
			return (ret);

		if ((ret = dbenv->open(dbenv, NULL, NULL, DB_CREATE |
		    DB_INIT_MPOOL | DB_PRIVATE | LF_ISSET(DB_THREAD), 0)) != 0)
			return (ret);
	}

	/* Register DB's pgin/pgout functions. */
	if ((ret =
	    memp_register(dbenv, DB_FTYPE_SET, __db_pgin, __db_pgout)) != 0)
		return (ret);

	/*
	 * Open a backing file in the memory pool.
	 *
	 * If we need to pre- or post-process a file's pages on I/O, set the
	 * file type.  If it's a hash file, always call the pgin and pgout
	 * routines.  This means that hash files can never be mapped into
	 * process memory.  If it's a btree file and requires swapping, we
	 * need to page the file in and out.  This has to be right -- we can't
	 * mmap files that are being paged in and out.
	 */
	memset(&finfo, 0, sizeof(finfo));
	switch (dbp->type) {
	case DB_BTREE:
	case DB_RECNO:
		finfo.ftype =
		    F_ISSET(dbp, DB_AM_SWAP) ? DB_FTYPE_SET : DB_FTYPE_NOTSET;
		finfo.clear_len = DB_PAGE_DB_LEN;
		break;
	case DB_HASH:
		finfo.ftype = DB_FTYPE_SET;
		finfo.clear_len = DB_PAGE_DB_LEN;
		break;
	case DB_QUEUE:
		finfo.ftype =
		    F_ISSET(dbp, DB_AM_SWAP) ? DB_FTYPE_SET : DB_FTYPE_NOTSET;
		finfo.clear_len = DB_PAGE_QUEUE_LEN;
		break;
	case DB_UNKNOWN:
		return (EINVAL);	/* Shouldn't be possible. */
	}
	finfo.pgcookie = &pgcookie;
	finfo.fileid = dbp->fileid;
	finfo.lsn_offset = 0;

	pginfo.db_pagesize = dbp->pgsize;
	pginfo.needswap = F_ISSET(dbp, DB_AM_SWAP);
	pgcookie.data = &pginfo;
	pgcookie.size = sizeof(DB_PGINFO);

	if ((ret = memp_fopen(dbenv, name,
	    LF_ISSET(DB_RDONLY | DB_NOMMAP),
	    0, dbp->pgsize, &finfo, &dbp->mpf)) != 0)
		return (ret);

	/*
	 * We may need a per-thread mutex.  Allocate it from the environment
	 * region, there's supposed to be extra space there for that purpose.
	 */
	if (LF_ISSET(DB_THREAD)) {
		if ((ret = __db_mutex_alloc(
		    dbenv, dbenv->reginfo, (MUTEX **)&dbp->mutexp)) != 0)
			return (ret);
		if ((ret = __db_mutex_init(
		    dbenv, dbp->mutexp, 0, MUTEX_THREAD)) != 0)
			return (ret);
	}

	/* Get a log file id. */
	if (F_ISSET(dbenv, DB_ENV_LOGGING) &&
#if !defined(DEBUG_ROP)
	    !F_ISSET(dbp, DB_AM_RDONLY) &&
#endif
	    (ret = log_register(dbenv, dbp, name, &dbp->log_fileid)) != 0)
		return (ret);

	return (0);
}

/*
 * __db_file_setup --
 *	Setup the file or in-memory data.
 *	Read the database metadata and resolve it with our arguments.
 */
static int
__db_file_setup(dbp, name, flags, mode, meta_pgno, zerop)
	DB *dbp;
	const char *name;
	u_int32_t flags;
	int mode;
	db_pgno_t meta_pgno;
	int *zerop;
{
	DBT namedbt;
	DB_ENV *dbenv;
	DB_FH *fhp, fh;
	DB_LSN lsn;
	DB_TXN *txn;
	ssize_t nr;
	u_int32_t magic, oflags;
	int ret, retry_cnt, t_ret;
	char *real_name, mbuf[256];

#define	IS_SUBDB_SETUP	(meta_pgno != PGNO_BASE_MD)

	dbenv = dbp->dbenv;
	txn = NULL;
	*zerop = 0;

	/*
	 * If we open a file handle and our caller is doing fcntl(2) locking,
	 * we can't close it because that would discard the caller's lock.
	 * Save it until we close the DB handle.
	 */
	if (LF_ISSET(DB_FCNTL_LOCKING)) {
		if ((ret = __os_malloc(sizeof(*fhp), NULL, &fhp)) != 0)
			return (ret);
	} else
		fhp = &fh;
	F_CLR(fhp, DB_FH_VALID);

	/*
	 * If the file is in-memory, set up is simple.  Otherwise, do the
	 * hard work of opening and reading the file.
	 *
	 * If we have a file name, try and read the first page, figure out
	 * what type of file it is, and initialize everything we can based
	 * on that file's meta-data page.
	 *
	 * !!!
	 * There's a reason we don't push this code down into the buffer cache.
	 * The problem is that there's no information external to the file that
	 * we can use as a unique ID.  UNIX has dev/inode pairs, but they are
	 * not necessarily unique after reboot, if the file was mounted via NFS.
	 * Windows has similar problems, as the FAT filesystem doesn't maintain
	 * dev/inode numbers across reboot.  So, we must get something from the
	 * file we can use to ensure that, even after a reboot, the file we're
	 * joining in the cache is the right file for us to join.  The solution
	 * we use is to maintain a file ID that's stored in the database, and
	 * that's why we have to open and read the file before calling into the
	 * buffer cache.
	 *
	 * The secondary reason is that there's additional information that
	 * we want to have before instantiating a file in the buffer cache:
	 * the page size, file type (btree/hash), if swapping is required,
	 * and flags (DB_RDONLY, DB_CREATE, DB_TRUNCATE).  We could handle
	 * needing this information by allowing it to be set for a file in
	 * the buffer cache even after the file has been opened, and, of
	 * course, supporting the ability to flush a file from the cache as
	 * necessary, e.g., if we guessed wrongly about the page size.  Given
	 * that we have to read the file anyway to get the file ID, we might
	 * as well get the rest, too.
	 *
	 * Get the real file name.
	 */
	if (name == NULL) {
		F_SET(dbp, DB_AM_INMEM);

		if (dbp->type == DB_UNKNOWN) {
			__db_err(dbenv,
			    "DBTYPE of unknown without existing file");
			return (EINVAL);
		}
		real_name = NULL;

		/*
		 * If the file is a temporary file and we're doing locking,
		 * then we have to create a unique file ID.  We can't use our
		 * normal dev/inode pair (or whatever this OS uses in place of
		 * dev/inode pairs) because no backing file will be created
		 * until the mpool cache is filled forcing the buffers to disk.
		 * Grab a random locker ID to use as a file ID.  The created
		 * ID must never match a potential real file ID -- we know it
		 * won't because real file IDs contain a time stamp after the
		 * dev/inode pair, and we're simply storing a 4-byte value.
		 *
		 * !!!
		 * Store the locker in the file id structure -- we can get it
		 * from there as necessary, and it saves having two copies.
		 */
		if (F_ISSET(dbenv, DB_ENV_LOCKING | DB_ENV_CDB) &&
		    (ret = lock_id(dbenv, (u_int32_t *)dbp->fileid)) != 0)
			return (ret);

		return (0);
	}

	/* Get the real backing file name. */
	if ((ret = __db_appname(dbenv,
	    DB_APP_DATA, NULL, name, 0, NULL, &real_name)) != 0)
		return (ret);

	/*
	 * Open the backing file.  We need to make sure that multiple processes
	 * attempting to create the file at the same time are properly ordered
	 * so that only one of them creates the "unique" file ID, so we open it
	 * O_EXCL and O_CREAT so two simultaneous attempts to create the region
	 * will return failure in one of the attempts.  If we're the one that
	 * fails, simply retry without the O_CREAT flag, which will require the
	 * meta-data page exist.
	 */

	/* Fill in the default file mode. */
	if (mode == 0)
		mode = __db_omode("rwrw--");

	oflags = 0;
	if (LF_ISSET(DB_RDONLY))
		oflags |= DB_OSO_RDONLY;
	if (LF_ISSET(DB_TRUNCATE))
		oflags |= DB_OSO_TRUNC;

	retry_cnt = 0;
open_retry:
	*zerop = 0;
	ret = 0;
	if (LF_ISSET(DB_CREATE)) {
		if (dbp->open_txn != NULL) {
			/*
			 * Start a child transaction to wrap this individual
			 * create.
			 */
			if ((ret =
			    txn_begin(dbenv, dbp->open_txn, &txn, 0)) != 0)
				goto err_msg;

			memset(&namedbt, 0, sizeof(namedbt));
			namedbt.data = (char *)name;
			namedbt.size = strlen(name) + 1;
			if ((ret = __crdel_fileopen_log(dbenv, txn,
			    &lsn, DB_FLUSH, &namedbt, mode)) != 0)
				goto err_msg;
		}
		DB_TEST_RECOVERY(dbp, DB_TEST_PREOPEN, ret, name);
		if ((ret = __os_open(real_name,
		    oflags | DB_OSO_CREATE | DB_OSO_EXCL, mode, fhp)) == 0) {
			DB_TEST_RECOVERY(dbp, DB_TEST_POSTOPEN, ret, name);

			/* Commit the file create. */
			if (dbp->open_txn != NULL) {
				if ((ret = txn_commit(txn, DB_TXN_SYNC)) != 0)
					goto err_msg;
				txn = NULL;
			}

			/*
			 * We created the file.  This means that if we later
			 * fail, we need to delete the file and if we're going
			 * to do that, we need to trash any pages in the
			 * memory pool.  Since we only know here that we
			 * created the file, we're going to set the flag here
			 * and clear it later if we commit successfully.
			 */
			F_SET(dbp, DB_AM_DISCARD);
		} else {
			/*
			 * Abort the file create.  If the abort fails, report
			 * the error returned by txn_abort(), rather than the
			 * open error, for no particular reason.
			 */
			if (dbp->open_txn != NULL) {
				if ((t_ret = txn_abort(txn)) != 0) {
					ret = t_ret;
					goto err_msg;
				}
				txn = NULL;
			}

			/*
			 * If we were not doing an exclusive open, try again
			 * without the create flag.
			 */
			if (ret == EEXIST && !LF_ISSET(DB_EXCL)) {
				LF_CLR(DB_CREATE);
				DB_TEST_RECOVERY(dbp,
				    DB_TEST_POSTOPEN, ret, name);
				goto open_retry;
			}
		}
	} else
		ret = __os_open(real_name, oflags, mode, fhp);

	/*
	 * Be quiet if we couldn't open the file because it didn't exist,
	 * the customers don't like those messages appearing in the logs.
	 * Otherwise, complain loudly.
	 */
	if (ret != 0) {
		if (ret == ENOENT)
			goto err;
		goto err_msg;
	}

	/* Set the page size if we don't have one yet. */
	if (dbp->pgsize == 0 &&
	    (ret = __db_set_pgsize(dbp, fhp, real_name)) != 0)
		goto err;

	/*
	 * Seek to the metadata offset; if it's a master database open or a
	 * database without subdatabases, we're seeking to 0, but that's OK.
	 */
	if ((ret = __os_seek(fhp,
	    dbp->pgsize, meta_pgno, 0, 0, DB_OS_SEEK_SET)) != 0)
		goto err_msg;

	/*
	 * Read the metadata page.  We read 256 bytes, which is larger than
	 * any access method's metadata page and smaller than any disk sector.
	 */
	if ((ret = __os_read(fhp, mbuf, sizeof(mbuf), &nr)) != 0)
		goto err_msg;

	if (nr == sizeof(mbuf)) {
		/*
		 * Figure out what access method we're dealing with, and then
		 * call access method specific code to check error conditions
		 * based on conflicts between the found file and application
		 * arguments.  A found file overrides some user information --
		 * we don't consider it an error, for example, if the user set
		 * an expected byte order and the found file doesn't match it.
		 */
		F_CLR(dbp, DB_AM_SWAP);
		magic = ((DBMETA *)mbuf)->magic;

swap_retry:	switch (magic) {
		case DB_BTREEMAGIC:
			if ((ret =
			    __bam_metachk(dbp, name, (BTMETA *)mbuf)) != 0)
				goto err;
			break;
		case DB_HASHMAGIC:
			if ((ret =
			    __ham_metachk(dbp, name, (HMETA *)mbuf)) != 0)
				goto err;
			break;
		case DB_QAMMAGIC:
			if ((ret =
			    __qam_metachk(dbp, name, (QMETA *)mbuf)) != 0)
				goto err;
			break;
		case 0:
			/*
			 * There are two ways we can get a 0 magic number.
			 * If we're creating a subdatabase, then the magic
			 * number will be 0.  We allocate a page as part of
			 * finding out what the base page number will be for
			 * the new subdatabase, but it's not initialized in
			 * any way.
			 *
			 * The second case happens if we are in recovery
			 * and we are going to recreate a database, it's
			 * possible that it's page was created (on systems
			 * where pages must be created explicitly to avoid
			 * holes in files) but is still 0.
			 */
			if (IS_SUBDB_SETUP)		/* Case 1 */
				goto empty;

			if (!LF_ISSET(DB_CREATE | DB_TRUNCATE)) { /* Case 2 */
				*zerop = 1;
				goto empty;
			}
			goto bad_format;
		default:
			if (F_ISSET(dbp, DB_AM_SWAP))
				goto bad_format;

			M_32_SWAP(magic);
			F_SET(dbp, DB_AM_SWAP);
			goto swap_retry;
		}
	} else {
		/*
		 * Only newly created files are permitted to fail magic
		 * number tests.
		 */
		if (nr != 0 || IS_SUBDB_SETUP)
			goto bad_format;


		/* Let the caller know that we had a 0-length file. */
		if (!LF_ISSET(DB_CREATE | DB_TRUNCATE))
			*zerop = 1;

		/*
		 * The only way we can reach here with the DB_CREATE flag set
		 * is if we created the file.  If that's not the case, then
		 * either (a) someone else created the file but has not yet
		 * written out the metadata page, or (b) we truncated the file
		 * (DB_TRUNCATE) leaving it zero-length.  In the case of (a),
		 * we want to sleep and give the file creator time to write
		 * the metadata page.  In the case of (b), we want to continue.
		 *
		 * !!!
		 * There's a race in the case of two processes opening the file
		 * with the DB_TRUNCATE flag set at roughly the same time, and
		 * they could theoretically hurt each other.  Sure hope that's
		 * unlikely.
		 */
		if (!LF_ISSET(DB_CREATE | DB_TRUNCATE) &&
		    (dbenv->lg_handle == NULL ||
		    !F_ISSET((DB_LOG *)dbenv->lg_handle, DBC_RECOVER))) {
			if (retry_cnt++ < 3) {
				__os_sleep(1, 0);
				goto open_retry;
			}
bad_format:		__db_err(dbenv,
			    "%s: unexpected file type or format", name);
			ret = EINVAL;
			goto err;
		}
		if (dbp->type == DB_UNKNOWN) {
			__db_err(dbenv,
			    "%s: DB_UNKNOWN type specified with empty file",
			    name);
			ret = EINVAL;
			goto err;
		}

empty:		/*
		 * The file is empty, and that's OK.  If it's not a subdatabase,
		 * though, we do need to generate a unique file ID for it.  The
		 * unique file ID includes a timestampe so that we can't collide
		 * with any other files, even when the file IDs (dev/inode pair)
		 * are reused.
		 */
		if (*zerop == 1)
			memset(dbp->fileid, 0, DB_FILE_ID_LEN);
		else if (!IS_SUBDB_SETUP &&
		    (ret = __os_fileid(dbenv, real_name, 1, dbp->fileid)) != 0)
			goto err_msg;
	}

	if (0) {
err_msg:	__db_err(dbenv, "%s: %s", name, db_strerror(ret));
	}

	/*
	 * Abort any running transaction -- it can only exist if something
	 * went wrong.
	 */
err:	if (txn != NULL)
		(void)txn_abort(txn);

DB_TEST_RECOVERY_LABEL
	/*
	 * If we opened a file handle and our caller is doing fcntl(2) locking,
	 * then we can't close it because that would discard the caller's lock.
	 * Otherwise, close the handle.
	 */
	if (F_ISSET(fhp, DB_FH_VALID)) {
		if (ret == 0 && LF_ISSET(DB_FCNTL_LOCKING))
			dbp->saved_open_fhp = fhp;
		else
			if ((t_ret = __os_closehandle(fhp)) != 0 && ret == 0)
				ret = t_ret;
	}

	if (real_name != NULL)
		__os_freestr(real_name);

	return (ret);
}

/*
 * __db_set_pgsize --
 *	Set the page size based on file information.
 */
static int
__db_set_pgsize(dbp, fhp, name)
	DB *dbp;
	DB_FH *fhp;
	char *name;
{
	DB_ENV *dbenv;
	u_int32_t iopsize;
	int ret;

	dbenv = dbp->dbenv;

	/*
	 * Use the filesystem's optimum I/O size as the pagesize if a pagesize
	 * not specified.  Some filesystems have 64K as their optimum I/O size,
	 * but as that results in fairly large default caches, we limit the
	 * default pagesize to 16K.
	 */
	if ((ret = __os_ioinfo(name, fhp, NULL, NULL, &iopsize)) != 0) {
		__db_err(dbenv, "%s: %s", name, db_strerror(ret));
		return (ret);
	}
	if (iopsize < 512)
		iopsize = 512;
	if (iopsize > 16 * 1024)
		iopsize = 16 * 1024;

	/*
	 * Sheer paranoia, but we don't want anything that's not a power-of-2
	 * (we rely on that for alignment of various types on the pages), and
	 * we want a multiple of the sector size as well.
	 */
	OS_ROUNDOFF(iopsize, 512);

	dbp->pgsize = iopsize;
	F_SET(dbp, DB_AM_PGDEF);

	return (0);
}

/*
 * __db_close --
 *	DB destructor.
 *
 * PUBLIC: int __db_close __P((DB *, u_int32_t));
 */
int
__db_close(dbp, flags)
	DB *dbp;
	u_int32_t flags;
{
	DB_ENV *dbenv;
	DBC *dbc;
	int ret, t_ret;

	ret = 0;

	PANIC_CHECK(dbp->dbenv);

	/* Validate arguments. */
	if ((ret = __db_closechk(dbp, flags)) != 0)
		return (ret);

	/* If never opened, or not currently open, it's easy. */
	if (!F_ISSET((dbp), DB_OPEN_CALLED))
		goto never_opened;

	/* Sync the underlying access method. */
	if (!LF_ISSET(DB_NOSYNC) && !F_ISSET(dbp, DB_AM_DISCARD) &&
	    (t_ret = dbp->sync(dbp, 0)) != 0 && ret == 0)
		ret = t_ret;

	/*
	 * Go through the active cursors and call the cursor recycle routine,
	 * which resolves pending operations and moves the cursors onto the
	 * free list.  Then, walk the free list and call the cursor destroy
	 * routine.
	 */
	while ((dbc = TAILQ_FIRST(&dbp->active_queue)) != NULL)
		if ((t_ret = dbc->c_close(dbc)) != 0 && ret == 0)
			ret = t_ret;
	while ((dbc = TAILQ_FIRST(&dbp->free_queue)) != NULL)
		if ((t_ret = __db_c_destroy(dbc)) != 0 && ret == 0)
			ret = t_ret;

	/* Sync the memory pool. */
	if (!LF_ISSET(DB_NOSYNC) && !F_ISSET(dbp, DB_AM_DISCARD) &&
	    (t_ret = memp_fsync(dbp->mpf)) != 0 &&
	    t_ret != DB_INCOMPLETE && ret == 0)
		ret = t_ret;

	/* Close any handle we've been holding since the open.  */
	if (dbp->saved_open_fhp != NULL &&
	    F_ISSET(dbp->saved_open_fhp, DB_FH_VALID) &&
	    (t_ret = __os_closehandle(dbp->saved_open_fhp)) != 0 && ret == 0)
		ret = t_ret;

never_opened:
	/*
	 * Call the access specific close function.
	 *
	 * !!!
	 * Because of where the function is called in the close process,
	 * these routines can't do anything that would dirty pages or
	 * otherwise affect closing down the database.
	 */
	if ((t_ret = __ham_db_close(dbp)) != 0 && ret == 0)
		ret = t_ret;
	if ((t_ret = __bam_db_close(dbp)) != 0 && ret == 0)
		ret = t_ret;
	if ((t_ret = __qam_db_close(dbp)) != 0 && ret == 0)
		ret = t_ret;

	/* Refresh the structure and close any local environment. */
	dbenv = dbp->dbenv;
	if ((t_ret = __db_refresh(dbp)) != 0 && ret == 0)
		ret = t_ret;
	if (F_ISSET(dbenv, DB_ENV_DBLOCAL) &&
	    (t_ret = dbenv->close(dbenv, 0)) != 0 && ret == 0)
		ret = t_ret;

	memset(dbp, CLEAR_BYTE, sizeof(*dbp));
	__os_free(dbp, sizeof(*dbp));

	return (ret);
}

/*
 * __db_refresh --
 *	Refresh the DB structure, releasing any allocated resources.
 */
static int
__db_refresh(dbp)
	DB *dbp;
{
	DB_ENV *dbenv;
	int ret, t_ret;

	ret = 0;

	dbenv = dbp->dbenv;

	dbp->type = 0;

	/* Close the memory pool file handle. */
	if (dbp->mpf != NULL) {
		if (F_ISSET(dbp, DB_AM_DISCARD))
			(void)__memp_fremove(dbp->mpf);
		if ((t_ret = memp_fclose(dbp->mpf)) != 0 && ret == 0)
			ret = t_ret;
		dbp->mpf = NULL;
	}

	/* Discard the thread mutex. */
	if (dbp->mutexp != NULL) {
		__db_mutex_free(dbenv, dbenv->reginfo, dbp->mutexp);
		dbp->mutexp = NULL;
	}

	/* Discard the log file id. */
	if (dbp->log_fileid != DB_LOGFILEID_INVALID) {
		(void)log_unregister(dbenv, dbp->log_fileid);
		dbp->log_fileid = DB_LOGFILEID_INVALID;
	}

	TAILQ_INIT(&dbp->free_queue);
	TAILQ_INIT(&dbp->active_queue);

	F_CLR(dbp, DB_AM_DISCARD);
	F_CLR(dbp, DB_AM_INMEM);
	F_CLR(dbp, DB_AM_RDONLY);
	F_CLR(dbp, DB_AM_SWAP);
	F_CLR(dbp, DB_DBM_ERROR);
	F_CLR(dbp, DB_OPEN_CALLED);

	return (ret);
}

/*
 * __db_remove
 * 	Remove method for DB.
 *
 * PUBLIC: int __db_remove __P((DB *, const char *, const char *, u_int32_t));
 */
int
__db_remove(dbp, name, subdb, flags)
	DB *dbp;
	const char *name, *subdb;
	u_int32_t flags;
{
	DBT namedbt;
	DB_ENV *dbenv;
	DB_LOCK remove_lock;
	DB_LSN newlsn;
	int ret, t_ret;
	char *backup, *real_back, *real_name;

	dbenv = dbp->dbenv;
	ret = 0;
	backup = real_back = real_name = NULL;

	PANIC_CHECK(dbenv);
	DB_ILLEGAL_AFTER_OPEN(dbp, "remove");

	/* Validate arguments. */
	if ((ret = __db_removechk(dbp, flags)) != 0)
		return (ret);

	/*
	 * Subdatabases.
	 */
	if (subdb != NULL) {
		/* Subdatabases must be created in named files. */
		if (name == NULL) {
			__db_err(dbenv,
		    "subdatabases cannot be created in temporary files");
			return (EINVAL);
		}
		return (__db_subdb_remove(dbp, name, subdb));
	}

	/* Start the transaction and log the delete. */
	if (F_ISSET(dbenv, DB_ENV_TXN)) {
		if ((ret = __db_metabegin(dbp, &remove_lock)) != 0)
			return (ret);

		memset(&namedbt, 0, sizeof(namedbt));
		namedbt.data = (char *)name;
		namedbt.size = strlen(name) + 1;

		if ((ret = __crdel_delete_log(dbenv,
		    dbp->open_txn, &newlsn, DB_FLUSH, &namedbt)) != 0) {
			__db_err(dbenv,
			    "%s: %s", name, db_strerror(ret));
			goto err;
		}
	}

	/*
	 * XXX
	 * We need to open the file and call __memp_fremove on the mpf.  I'm
	 * not sure that we need to do this.  Is it our responsibility or the
	 * application's responsibility to make sure someone else isn't busily
	 * deleting pages behind our backs?
	 */

	/* Find the real name of the file. */
	if ((ret = __db_appname(dbenv,
	    DB_APP_DATA, NULL, name, 0, NULL, &real_name)) != 0)
		goto err;

	/* Create name for backup file. */
	if ((ret =  __db_backup_name(name, &backup, &newlsn)) != 0)
		goto err;
	if ((ret = __db_appname(dbenv,
	    DB_APP_DATA, NULL, backup, 0, NULL, &real_back)) != 0)
		goto err;

	DB_TEST_RECOVERY(dbp, DB_TEST_PRERENAME, ret, name);
	ret = __os_rename(real_name, real_back);
	DB_TEST_RECOVERY(dbp, DB_TEST_POSTRENAME, ret, name);

err:
DB_TEST_RECOVERY_LABEL
	/*
	 * End the transaction, committing the transaction if we were
	 * successful, aborting otherwise.
	 */
	if (dbp->open_txn != NULL && (t_ret = __db_metaend(dbp, &remove_lock,
	   ret == 0, __db_remove_callback, real_back)) != 0 && ret == 0)
		ret = t_ret;

	if (real_name != NULL)
		__os_freestr(real_name);
	if (backup != NULL)
		__os_freestr(backup);

	return (ret);
}

/*
 * __db_subdb_remove --
 *	Remove a subdatabase.
 */
static int
__db_subdb_remove(dbp, name, subdb)
	DB *dbp;
	const char *name, *subdb;
{
	DB *mdbp;
	DBC *dbc;
	DB_ENV *dbenv;
	DB_LOCK remove_lock;
	db_pgno_t meta_pgno;
	int ret, t_ret;

	mdbp = NULL;
	dbc = NULL;
	dbenv = dbp->dbenv;

	/* Start the transaction. */
	if (F_ISSET(dbenv, DB_ENV_TXN) &&
	    (ret = __db_metabegin(dbp, &remove_lock)) != 0)
		return (ret);

	/*
	 * Open the subdatabase.  We can use the user's DB handle for this
	 * purpose, I think.
	 */
	if ((ret = __db_open(dbp, name, subdb, DB_UNKNOWN, 0, 0)) != 0)
		goto err;

	/* Free up the pages in the subdatabase. */
	switch (dbp->type) {
		case DB_BTREE:
		case DB_RECNO:
			if ((ret = __bam_reclaim(dbp, dbp->open_txn)) != 0)
				goto err;
			break;
		case DB_HASH:
			if ((ret = __ham_reclaim(dbp, dbp->open_txn)) != 0)
				goto err;
			break;
		default:
			ret = EINVAL;		/* Shouldn't be possible. */
			goto err;
	}

	/*
	 * Remove the entry from the main database and free the subdatabase
	 * metadata page.
	 */
	if ((ret = __db_master_open(dbp->dbenv,
	    dbp->open_txn, name, 0, 0, &mdbp)) != 0)
		goto err;

	if ((ret = __db_master_update(mdbp,
		    subdb, dbp->type, &meta_pgno, 1, 0)) != 0)
			goto err;


err:	/*
	 * End the transaction, committing the transaction if we were
	 * successful, aborting otherwise.
	 */
	if (dbp->open_txn != NULL && (t_ret = __db_metaend(dbp,
	    &remove_lock, ret == 0, NULL, NULL)) != 0 && ret == 0)
		ret = t_ret;

	/*
	 * Close the user's DB handle -- do this LAST to avoid smashing the
	 * the transaction information.
	 */
	if ((t_ret = dbp->close(dbp, 0)) != 0 && ret == 0)
		ret = t_ret;

	if (mdbp != NULL && (t_ret = mdbp->close(mdbp, 0)) != 0 && ret == 0)
		ret = t_ret;

	return (ret);
}

/*
 * __db_metabegin --
 *
 * Begin a meta-data operation.  This involves doing any required locking,
 * potentially beginning a transaction and then telling the caller if you
 * did or did not begin the transaction.
 *
 * The writing flag indicates if the caller is actually allowing creates
 * or doing deletes (i.e., if the caller is opening and not creating, then
 * we don't need to do any of this).
 */
static int
__db_metabegin(dbp, lockp)
	DB *dbp;
	DB_LOCK *lockp;
{
	DB_ENV *dbenv;
	DBT dbplock;
	u_int32_t locker, lockval;
	int ret;

	dbenv = dbp->dbenv;

	lockp->off = LOCK_INVALID;

	/*
	 * There is no single place where we can know that we are or are not
	 * going to be creating any files and/or subdatabases, so we will
	 * always begin a tranasaction when we start creating one.  If we later
	 * discover that this was unnecessary, we will abort the transaction.
	 * Recovery is written so that if we log a file create, but then
	 * discover that we didn't have to do it, we recover correctly.  The
	 * file recovery design document has details.
	 *
	 * We need to single thread all create and delete operations, so if we
	 * are running with locking, we must obtain a lock. We use lock_id to
	 * generate a unique locker id and use a handcrafted DBT as the object
	 * on which we are locking.
	 */
	if (F_ISSET(dbenv, DB_ENV_LOCKING | DB_ENV_CDB)) {
		if ((ret = lock_id(dbenv, &locker)) != 0)
			return (ret);
		lockval = 0;
		dbplock.data = &lockval;
		dbplock.size = sizeof(lockval);
		if ((ret = lock_get(dbenv,
		    locker, 0, &dbplock, DB_LOCK_WRITE, lockp)) != 0)
			return(ret);
	}

	return (txn_begin(dbenv, NULL, &dbp->open_txn, 0));
}

/*
 * __db_metaend --
 * 	End a meta-data operation.
 */
static int
__db_metaend(dbp, lockp, commit, callback, cookie)
	DB *dbp;
	DB_LOCK *lockp;
	int commit, (*callback) __P((DB *, void *));
	void *cookie;
{
	DB_ENV *dbenv;
	int ret, t_ret;

	dbenv = dbp->dbenv;

	/* End the transaction. */
	if (commit) {
		if ((ret = txn_commit(dbp->open_txn, DB_TXN_SYNC)) == 0) {
			/*
			 * Unlink any underlying file, we've committed the
			 * transaction.
			 */
			if (callback != NULL)
				ret = callback(dbp, cookie);
		}
	} else
		ret = txn_abort(dbp->open_txn);

	/* Release our lock. */
	if (lockp->off != LOCK_INVALID &&
	    (t_ret = lock_put(dbenv, lockp)) != 0 && ret == 0)
		ret = t_ret;

	return (ret);
}

/*
 * __db_backup_name
 *	Create the backup file name for a given file.
 *
 * PUBLIC: int __db_backup_name __P((const char *, char **, DB_LSN *));
 */
#undef	BACKUP_PREFIX
#define	BACKUP_PREFIX	"__db."

#undef	MAX_LSN_TO_TEXT
#define	MAX_LSN_TO_TEXT	21
int
__db_backup_name(name, backup, lsn)
	const char *name;
	char **backup;
	DB_LSN *lsn;
{
	size_t len;
	int ret;
	char *retp;

	len = strlen(name) + strlen(BACKUP_PREFIX) + MAX_LSN_TO_TEXT + 1;

	if ((ret = __os_malloc(len, NULL, &retp)) != 0)
		return (ret);

	/*
	 * Create the name.  Backup file names are of the form:
	 *
	 *	__db.name.0x[lsn-file].0x[lsn-offset]
	 *
	 * which guarantees uniqueness.
	 */
	snprintf(retp, len,
	    "%s%s.0x%x0x%x", BACKUP_PREFIX, name, lsn->file, lsn->offset);

	*backup = retp;
	return (0);
}

/*
 * __db_remove_callback --
 *	Callback function -- on file remove commit, it unlinks the backing
 *	file.
 */
static int
__db_remove_callback(dbp, cookie)
	DB *dbp;
	void *cookie;
{
	COMPQUIET(dbp, NULL);

	return (__os_unlink(cookie));
}

#if	CONFIG_TEST
/*
 * __db_testcopy
 *	Create a copy of all backup files and our "main" DB.
 *
 * PUBLIC: int __db_testcopy __P((DB *, const char *));
 */
int
__db_testcopy(dbp, name)
	DB *dbp;
	const char *name;
{
	size_t len;
	int dircnt, i, ret;
	char **namesp, *backup, *copy, *dir, *p, *real_name;

	real_name = NULL;
	/* Get the real backing file name. */
	if ((ret = __db_appname(dbp->dbenv,
	    DB_APP_DATA, NULL, name, 0, NULL, &real_name)) != 0)
		return (ret);

	/*
	 * Maximum size of file, including adding a ".afterop".
	 */
	len = strlen(real_name) + strlen(BACKUP_PREFIX) + MAX_LSN_TO_TEXT + 9;

	if ((ret = __os_malloc(len, NULL, &copy)) != 0)
		goto out;

	if ((ret = __os_malloc(len, NULL, &backup)) != 0)
		goto out;

	/*
	 * First copy the file itself.
	 */
	snprintf(copy, len, "%s.afterop", real_name);
	__db_makecopy(real_name, copy);

	if ((ret = __os_strdup(real_name, &dir)) != 0)
		goto out;
	__os_freestr(real_name);
	real_name = NULL;
	/*
	 * Create the name.  Backup file names are of the form:
	 *
	 *	__db.name.0x[lsn-file].0x[lsn-offset]
	 *
	 * which guarantees uniqueness.  We want to look for the
	 * backup name, followed by a '.0x' (so that if they have
	 * files named, say, 'a' and 'abc' we won't match 'abc' when
	 * looking for 'a'.
	 */
	snprintf(backup, len, "%s%s.0x", BACKUP_PREFIX, name);

	/*
	 * We need the directory path to do the __os_dirlist.
	 */
	p = __db_rpath(dir);
	if (p != NULL)
		*p = '\0';
	ret = __os_dirlist(dir, &namesp, &dircnt);
#if DIAGNOSTIC
	/*
	 * XXX
	 * To get the memory guard code to work because
	 * it uses strlen and we just moved the end of the
	 * string somewhere sooner.  This causes the guard
	 * code to fail as it looks at one byte past the end
	 * of the string.
	 * XXX
	 */
	*p = '/';
#endif
	__os_freestr(dir);
	if (ret != 0)
		goto out;
	for (i = 0; i < dircnt; i++) {
		/*
		 * Need to check if it is a backup file for this.
		 * No idea what namesp[i] may be or how long, so
		 * must use strncmp and not memcmp.  We don't want
		 * to use strcmp either because we are only matching
		 * the first part of the real file's name.  We don't
		 * know its LSN's.
		 */
		if (strncmp(namesp[i], backup, strlen(backup)) == 0) {
			if ((ret = __db_appname(dbp->dbenv, DB_APP_DATA,
			    NULL, namesp[i], 0, NULL, &real_name)) != 0)
				goto out;

			/*
			 * This should not happen.  Check that old
			 * .afterop files aren't around.
			 * If so, just move on.
			 */
			if (strstr(real_name, ".afterop") != NULL) {
				__os_freestr(real_name);
				real_name = NULL;
				continue;
			}
			snprintf(copy, len, "%s.afterop", real_name);
			__db_makecopy(real_name, copy);
			__os_freestr(real_name);
			real_name = NULL;
		}
	}
out:
	if (real_name)
		__os_freestr(real_name);
	return (ret);
}

static void
__db_makecopy(src, dest)
	const char *src, *dest;
{
	DB_FH rfh, wfh;
	ssize_t rcnt, wcnt;
	char *buf;

	memset(&rfh, 0, sizeof(rfh));
	memset(&wfh, 0, sizeof(wfh));

	if (__os_malloc(1024, NULL, &buf) != 0)
		return;

	if (__os_open(src, DB_OSO_RDONLY, __db_omode("rw----"), &rfh) != 0)
		goto err;
	if (__os_open(dest,
	    DB_OSO_CREATE | DB_OSO_TRUNC, __db_omode("rw----"), &wfh) != 0)
		goto err;

	for (;;)
		if (__os_read(&rfh, buf, 1024, &rcnt) < 0 || rcnt == 0 ||
		    __os_write(&wfh, buf, rcnt, &wcnt) < 0 || wcnt != rcnt)
			break;

err:	__os_free(buf, 1024);
	if (F_ISSET(&rfh, DB_FH_VALID))
		__os_closehandle(&rfh);
	if (F_ISSET(&wfh, DB_FH_VALID))
		__os_closehandle(&wfh);
}
#endif
