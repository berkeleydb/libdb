/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */
#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)mp_bh.c	11.5 (Sleepycat) 9/21/99";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <errno.h>
#include <string.h>
#include <unistd.h>
#endif

#include "db_int.h"
#include "db_shash.h"
#include "mp.h"

static int __memp_upgrade __P((DB_MPOOL *, DB_MPOOLFILE *, MPOOLFILE *));

/*
 * __memp_bhwrite --
 *	Write the page associated with a given bucket header.
 *
 * PUBLIC: int __memp_bhwrite
 * PUBLIC:     __P((DB_MPOOL *, MPOOLFILE *, BH *, int *, int *));
 */
int
__memp_bhwrite(dbmp, mfp, bhp, restartp, wrotep)
	DB_MPOOL *dbmp;
	MPOOLFILE *mfp;
	BH *bhp;
	int *restartp, *wrotep;
{
	DB_MPOOLFILE *dbmfp;
	DB_MPREG *mpreg;
	int incremented, ret;

	if (restartp != NULL)
		*restartp = 0;
	if (wrotep != NULL)
		*wrotep = 0;
	incremented = 0;

	/*
	 * Walk the process' DB_MPOOLFILE list and find a file descriptor for
	 * the file.  We also check that the descriptor is open for writing.
	 * If we find a descriptor on the file that's not open for writing, we
	 * try and upgrade it to make it writeable.  If that fails, we're done.
	 */
	MUTEX_THREAD_LOCK(dbmp->mutexp);
	for (dbmfp = TAILQ_FIRST(&dbmp->dbmfq);
	    dbmfp != NULL; dbmfp = TAILQ_NEXT(dbmfp, q))
		if (dbmfp->mfp == mfp) {
			if (F_ISSET(dbmfp, MP_READONLY) &&
			    __memp_upgrade(dbmp, dbmfp, mfp)) {
				MUTEX_THREAD_UNLOCK(dbmp->mutexp);
				return (0);
			}

			/*
			 * Increment the reference count -- see the comment in
			 * memp_fclose().
			 */
			++dbmfp->ref;
			incremented = 1;
			break;
		}
	MUTEX_THREAD_UNLOCK(dbmp->mutexp);
	if (dbmfp != NULL)
		goto found;

	/*
	 * !!!
	 * Don't try to attach to temporary files.  There are two problems in
	 * trying to do that.  First, if we have different privileges than the
	 * process that "owns" the temporary file, we might create the backing
	 * disk file such that the owning process couldn't read/write its own
	 * buffers, e.g., memp_trickle() running as root creating a file owned
	 * as root, mode 600.  Second, if the temporary file has already been
	 * created, we don't have any way of finding out what its real name is,
	 * and, even if we did, it was already unlinked (so that it won't be
	 * left if the process dies horribly).  This decision causes a problem,
	 * however: if the temporary file consumes the entire buffer cache,
	 * and the owner doesn't flush the buffers to disk, we could end up
	 * with resource starvation, and the memp_trickle() thread couldn't do
	 * anything about it.  That's a pretty unlikely scenario, though.
	 */
	if (F_ISSET(mfp, MP_TEMP))
		return (0);

	/*
	 * It's possible the file has been removed.  Drop into the page-write
	 * function, which has to handle the fact that we don't have any real
	 * file descriptor information.
	 */
	if (F_ISSET(mfp, MP_REMOVED)) {
		dbmfp = NULL;
		goto found;
	}

	/*
	 * It's not a page from a file we've opened.  If the file requires
	 * input/output processing, see if this process has ever registered
	 * information as to how to write this type of file.  If not, there's
	 * nothing we can do.
	 */
	if (mfp->ftype != 0) {
		MUTEX_THREAD_LOCK(dbmp->mutexp);
		for (mpreg = LIST_FIRST(&dbmp->dbregq);
		    mpreg != NULL; mpreg = LIST_NEXT(mpreg, q))
			if (mpreg->ftype == mfp->ftype)
				break;
		MUTEX_THREAD_UNLOCK(dbmp->mutexp);
		if (mpreg == NULL)
			return (0);
	}

	/*
	 * Try and open the file, attaching to the underlying shared area.
	 * Ignore any error, assume it's a permissions problem.
	 *
	 * XXX
	 * There's no negative cache, so we may repeatedly try and open files
	 * that we have previously tried (and failed) to open.
	 */
	if (__memp_fopen(dbmp, mfp, R_ADDR(&dbmp->reginfo, mfp->path_off),
	    0, 0, mfp->stat.st_pagesize, 0, NULL, &dbmfp) != 0)
		return (0);

found:	ret = __memp_pgwrite(dbmp, dbmfp, bhp, restartp, wrotep);

	if (incremented) {
		MUTEX_THREAD_LOCK(dbmp->mutexp);
		--dbmfp->ref;
		MUTEX_THREAD_UNLOCK(dbmp->mutexp);
	}

	return (ret);
}

/*
 * __memp_pgread --
 *	Read a page from a file.
 *
 * PUBLIC: int __memp_pgread __P((DB_MPOOLFILE *, BH *, int));
 */
int
__memp_pgread(dbmfp, bhp, can_create)
	DB_MPOOLFILE *dbmfp;
	BH *bhp;
	int can_create;
{
	DB_IO db_io;
	DB_ENV *dbenv;
	DB_MPOOL *dbmp;
	MPOOLFILE *mfp;
	size_t len, pagesize;
	ssize_t nr;
	int created, ret;

	dbmp = dbmfp->dbmp;
	dbenv = dbmp->dbenv;
	mfp = dbmfp->mfp;
	pagesize = mfp->stat.st_pagesize;

	F_SET(bhp, BH_LOCKED | BH_TRASH);
	MUTEX_LOCK(&bhp->mutex, dbenv->lockfhp);
	R_UNLOCK(dbenv, &dbmp->reginfo);

	/*
	 * Temporary files may not yet have been created.  We don't create
	 * them now, we create them when the pages have to be flushed.
	 */
	nr = 0;
	if (F_ISSET(&dbmfp->fh, DB_FH_VALID)) {
		/*
		 * Ignore read errors if we have permission to create the page.
		 * Assume that the page doesn't exist, and that we'll create it
		 * when we write it out.
		 *
		 * XXX
		 * Theoretically, we could overwrite a page of data if it were
		 * possible for a file to be successfully opened for reading
		 * and then for the read to fail.  Shouldn't ever happen, but
		 * it might be worth checking to see if the offset is past the
		 * known end-of-file.
		 */
		db_io.fhp = &dbmfp->fh;
		db_io.mutexp = dbmfp->mutexp;
		db_io.pagesize = db_io.bytes = pagesize;
		db_io.pgno = bhp->pgno;
		db_io.buf = bhp->buf;

		ret = __os_io(&db_io, DB_IO_READ, &nr);
	} else
		ret = 0;

	created = 0;
	if (nr < (ssize_t)pagesize) {
		if (can_create)
			created = 1;
		else {
			/*
			 * If we had a short read, ret may be 0.  This may not
			 * be an error -- in particular DB recovery processing
			 * may request pages that have never been written to
			 * disk, in which case we won't find the page.  So, the
			 * caller must know how to handle the error.
			 */
			if (ret == 0)
				ret = EIO;
			goto err;
		}
	}

	/*
	 * Clear any bytes we didn't read that need to be cleared.  If we're
	 * running in diagnostic mode, smash any bytes on the page that are
	 * unknown quantities for the caller.
	 */
	if (nr != (ssize_t)pagesize) {
		len = mfp->clear_len == 0 ? pagesize : mfp->clear_len;
		if (nr < (ssize_t)len)
			memset(bhp->buf + nr, 0, len - nr);
#ifdef DIAGNOSTIC
		if (nr > (ssize_t)len)
			len = nr;
		if (len < pagesize)
			memset(bhp->buf + len, CLEAR_BYTE, pagesize - len);
#endif
	}

	/* Call any pgin function. */
	ret = mfp->ftype == 0 ? 0 : __memp_pg(dbmfp, bhp, 1);

	/* Unlock the buffer and reacquire the region lock. */
err:	MUTEX_UNLOCK(&bhp->mutex);
	R_LOCK(dbenv, &dbmp->reginfo);

	/*
	 * If no errors occurred, the data is now valid, clear the BH_TRASH
	 * flag; regardless, clear the lock bit and let other threads proceed.
	 */
	F_CLR(bhp, BH_LOCKED);
	if (ret == 0) {
		F_CLR(bhp, BH_TRASH);

		/* Update the statistics. */
		if (created)
			++mfp->stat.st_page_create;
		else
			++mfp->stat.st_page_in;
	}

	return (ret);
}

/*
 * __memp_pgwrite --
 *	Write a page to a file.
 *
 * PUBLIC: int __memp_pgwrite
 * PUBLIC:     __P((DB_MPOOL *, DB_MPOOLFILE *, BH *, int *, int *));
 */
int
__memp_pgwrite(dbmp, dbmfp, bhp, restartp, wrotep)
	DB_MPOOL *dbmp;
	DB_MPOOLFILE *dbmfp;
	BH *bhp;
	int *restartp, *wrotep;
{
	DB_ENV *dbenv;
	DB_IO db_io;
	DB_LSN lsn;
	MCACHE *mc;
	MPOOL *mp;
	MPOOLFILE *mfp;
	ssize_t nw;
	int callpgin, dosync, ret, syncfail;
	const char *fail;

	dbenv = dbmp->dbenv;
	mp = dbmp->reginfo.primary;
	mfp = dbmfp == NULL ? NULL : dbmfp->mfp;

	if (restartp != NULL)
		*restartp = 0;
	if (wrotep != NULL)
		*wrotep = 0;
	callpgin = 0;

	/*
	 * Check the dirty bit -- this buffer may have been written since we
	 * decided to write it.
	 */
	if (!F_ISSET(bhp, BH_DIRTY)) {
		if (wrotep != NULL)
			*wrotep = 1;
		return (0);
	}

	MUTEX_LOCK(&bhp->mutex, dbenv->lockfhp);

	/*
	 * If there were two writers, we may have just been waiting while the
	 * other writer completed I/O on this buffer.  Check the dirty bit one
	 * more time.
	 */
	if (!F_ISSET(bhp, BH_DIRTY)) {
		MUTEX_UNLOCK(&bhp->mutex);

		if (wrotep != NULL)
			*wrotep = 1;
		return (0);
	}

	F_SET(bhp, BH_LOCKED);
	R_UNLOCK(dbenv, &dbmp->reginfo);

	if (restartp != NULL)
		*restartp = 1;

	/*
	 * It's possible that the underlying file was removed.  Currently,
	 * this is only used by the filesystem operation recovery routines,
	 * but it's possible that it may be more widely used in the future.
	 *
	 * !!!
	 * Once we pass this point, we know that dbmfp and mfp aren't NULL,
	 * and that we have a valid file reference.
	 */
	if (mfp == NULL || F_ISSET(mfp, MP_REMOVED))
		goto file_removed;

	/* Copy the LSN off the page if we're going to need it. */
	if (F_ISSET(dbenv, DB_ENV_LOGGING) || F_ISSET(bhp, BH_WRITE))
		memcpy(&lsn, bhp->buf + mfp->lsn_off, sizeof(DB_LSN));

	/* Ensure the appropriate log records are on disk. */
	if (F_ISSET(dbenv, DB_ENV_LOGGING) &&
	    (ret = log_flush(dbenv, &lsn)) != 0)
		goto err;

	/*
	 * Call any pgout function.  We set the callpgin flag so that we flag
	 * that the contents of the buffer will need to be passed through pgin
	 * before they are reused.
	 */
	if (mfp->ftype == 0)
		ret = 0;
	else {
		callpgin = 1;
		if ((ret = __memp_pg(dbmfp, bhp, 0)) != 0)
			goto err;
	}

	/* Temporary files may not yet have been created. */
	if (!F_ISSET(&dbmfp->fh, DB_FH_VALID)) {
		MUTEX_THREAD_LOCK(dbmp->mutexp);
		if (!F_ISSET(&dbmfp->fh, DB_FH_VALID) &&
		    ((ret = __db_appname(dbenv, DB_APP_TMP, NULL, NULL,
		    DB_OSO_CREATE | DB_OSO_EXCL | DB_OSO_TEMP,
		    &dbmfp->fh, NULL)) != 0 ||
		    !F_ISSET(&dbmfp->fh, DB_FH_VALID))) {
			MUTEX_THREAD_UNLOCK(dbmp->mutexp);
			__db_err(dbenv,
			    "unable to create temporary backing file");
			goto err;
		}
		MUTEX_THREAD_UNLOCK(dbmp->mutexp);
	}

	/* Write the page. */
	db_io.fhp = &dbmfp->fh;
	db_io.mutexp = dbmfp->mutexp;
	db_io.pagesize = db_io.bytes = mfp->stat.st_pagesize;
	db_io.pgno = bhp->pgno;
	db_io.buf = bhp->buf;
	if ((ret = __os_io(&db_io, DB_IO_WRITE, &nw)) != 0) {
		__db_panic(dbenv, ret);
		fail = "write";
		goto syserr;
	}
	if (nw != (ssize_t)mfp->stat.st_pagesize) {
		ret = EIO;
		fail = "write";
		goto syserr;
	}

file_removed:
	/*
	 * !!!
	 * Once we pass this point, dbmfp and mfp may be NULL, we may not have
	 * a valid file reference.
	 *
	 * Unlock the buffer and reacquire the region lock.
	 */
	MUTEX_UNLOCK(&bhp->mutex);
	R_LOCK(dbenv, &dbmp->reginfo);

	/*
	 * Clean up the flags based on a successful write.
	 *
	 * If we rewrote the page, it will need processing by the pgin
	 * routine before reuse.
	 */
	if (callpgin)
		F_SET(bhp, BH_CALLPGIN);
	F_CLR(bhp, BH_DIRTY | BH_LOCKED);

	/*
	 * If we write a buffer for which a checkpoint is waiting, update
	 * the count of pending buffers (both in the mpool as a whole and
	 * for this file).  If the count for this file goes to zero, set a
	 * flag so we flush the writes.
	 */
	dosync = 0;
	if (F_ISSET(bhp, BH_WRITE)) {
		F_CLR(bhp, BH_WRITE);

		--mp->lsn_cnt;
		if (mfp != NULL)
			dosync = --mfp->lsn_cnt == 0 ? 1 : 0;
	}

	/* Update the page clean/dirty statistics. */
	mc = BH_TO_CACHE(dbmp, bhp);
	++mc->stat.st_page_clean;
	--mc->stat.st_page_dirty;

	/* Update I/O statistics. */
	if (mfp != NULL)
		++mfp->stat.st_page_out;

	/*
	 * Do the sync after everything else has been updated, so any incoming
	 * checkpoint doesn't see inconsistent information.
	 *
	 * XXX:
	 * Don't lock the region around the sync, fsync(2) has no atomicity
	 * issues.
	 *
	 * XXX:
	 * We ignore errors from the sync -- it makes no sense to return an
	 * error to the calling process, so set a flag causing the checkpoint
	 * to be retried later.  There is a possibility, of course, that a
	 * subsequent checkpoint was started and that we're going to force it
	 * to fail.  That should be unlikely, and fixing it would be difficult.
	 */
	if (dosync) {
		R_UNLOCK(dbenv, &dbmp->reginfo);
		syncfail = __os_fsync(&dbmfp->fh) != 0;
		R_LOCK(dbenv, &dbmp->reginfo);
		if (syncfail)
			F_SET(mp, MP_LSN_RETRY);
	}

	if (wrotep != NULL)
		*wrotep = 1;

	return (0);

syserr:	__db_err(dbenv, "%s: %s failed for page %lu",
	    __memp_fn(dbmfp), fail, (u_long)bhp->pgno);

err:	/* Unlock the buffer and reacquire the region lock. */
	MUTEX_UNLOCK(&bhp->mutex);
	R_LOCK(dbenv, &dbmp->reginfo);

	/*
	 * Clean up the flags based on a failure.
	 *
	 * The page remains dirty but we remove our lock.  If we rewrote the
	 * page, it will need processing by the pgin routine before reuse.
	 */
	if (callpgin)
		F_SET(bhp, BH_CALLPGIN);
	F_CLR(bhp, BH_LOCKED);

	return (ret);
}

/*
 * __memp_pg --
 *	Call the pgin/pgout routine.
 *
 * PUBLIC: int __memp_pg __P((DB_MPOOLFILE *, BH *, int));
 */
int
__memp_pg(dbmfp, bhp, is_pgin)
	DB_MPOOLFILE *dbmfp;
	BH *bhp;
	int is_pgin;
{
	DBT dbt, *dbtp;
	DB_MPOOL *dbmp;
	DB_MPREG *mpreg;
	MPOOLFILE *mfp;
	int ftype, ret;

	dbmp = dbmfp->dbmp;
	mfp = dbmfp->mfp;

	MUTEX_THREAD_LOCK(dbmp->mutexp);

	ftype = mfp->ftype;
	for (mpreg = LIST_FIRST(&dbmp->dbregq);
	    mpreg != NULL; mpreg = LIST_NEXT(mpreg, q)) {
		if (ftype != mpreg->ftype)
			continue;
		if (mfp->pgcookie_len == 0)
			dbtp = NULL;
		else {
			dbt.size = mfp->pgcookie_len;
			dbt.data = R_ADDR(&dbmp->reginfo, mfp->pgcookie_off);
			dbtp = &dbt;
		}
		MUTEX_THREAD_UNLOCK(dbmp->mutexp);

		if (is_pgin) {
			if (mpreg->pgin != NULL && (ret =
			    mpreg->pgin(bhp->pgno, bhp->buf, dbtp)) != 0)
				goto err;
		} else
			if (mpreg->pgout != NULL && (ret =
			    mpreg->pgout(bhp->pgno, bhp->buf, dbtp)) != 0)
				goto err;
		break;
	}

	if (mpreg == NULL)
		MUTEX_THREAD_UNLOCK(dbmp->mutexp);

	return (0);

err:	MUTEX_THREAD_UNLOCK(dbmp->mutexp);
	__db_err(dbmp->dbenv, "%s: %s failed for page %lu",
	    __memp_fn(dbmfp), is_pgin ? "pgin" : "pgout", (u_long)bhp->pgno);
	return (ret);
}

/*
 * __memp_bhfree --
 *	Free a bucket header and its referenced data.
 *
 * PUBLIC: void __memp_bhfree __P((DB_MPOOL *, BH *, int));
 */
void
__memp_bhfree(dbmp, bhp, free_mem)
	DB_MPOOL *dbmp;
	BH *bhp;
	int free_mem;
{
	DB_HASHTAB *dbht;
	MCACHE *mc;
	MPOOL *mp;
	int n_bucket, n_cache;

	mp = dbmp->reginfo.primary;
	mc = BH_TO_CACHE(dbmp, bhp);
	n_cache = NCACHE(mp, bhp->pgno);
	n_bucket = NBUCKET(mc, bhp->mf_offset, bhp->pgno);
	dbht = R_ADDR(&dbmp->c_reginfo[n_cache], mc->htab);

	/* Delete the buffer header from the hash bucket queue. */
	SH_TAILQ_REMOVE(&dbht[n_bucket], bhp, hq, __bh);

	/* Delete the buffer header from the LRU queue. */
	SH_TAILQ_REMOVE(&mc->bhq, bhp, q, __bh);

	/*
	 * If we're not reusing it immediately, free the buffer header
	 * and data for real.
	 */
	if (free_mem) {
		--mc->stat.st_page_clean;
		__db_shalloc_free(dbmp->c_reginfo[n_cache].addr, bhp);
	}
}

/*
 * __memp_upgrade --
 *	Upgrade a file descriptor from readonly to readwrite.
 */
static int
__memp_upgrade(dbmp, dbmfp, mfp)
	DB_MPOOL *dbmp;
	DB_MPOOLFILE *dbmfp;
	MPOOLFILE *mfp;
{
	DB_FH fh;
	int ret;
	char *rpath;

	/*
	 * !!!
	 * We expect the handle to already be locked.
	 */

	/* Check to see if we've already upgraded. */
	if (F_ISSET(dbmfp, MP_UPGRADE))
		return (0);

	/* Check to see if we've already failed. */
	if (F_ISSET(dbmfp, MP_UPGRADE_FAIL))
		return (1);

	/*
	 * Calculate the real name for this file and try to open it read/write.
	 * We know we have a valid pathname for the file because it's the only
	 * way we could have gotten a file descriptor of any kind.
	 */
	if ((ret = __db_appname(dbmp->dbenv, DB_APP_DATA,
	    NULL, R_ADDR(&dbmp->reginfo, mfp->path_off), 0, NULL, &rpath)) != 0)
		return (ret);
	if (__os_open(rpath, 0, 0, &fh) != 0) {
		F_SET(dbmfp, MP_UPGRADE_FAIL);
		ret = 1;
	} else {
		/* Swap the descriptors and set the upgrade flag. */
		(void)__os_closehandle(&dbmfp->fh);
		dbmfp->fh = fh;
		F_SET(dbmfp, MP_UPGRADE);
		ret = 0;
	}
	__os_freestr(rpath);
	return (ret);
}
