/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 2013 Oracle and/or its affiliates.  All rights reserved.
 *
 * $Id$
 */

#include "db_config.h"

#include "db_int.h"
#include "dbinc/db_page.h"		/* Required for diagnostic code. */
#include "dbinc/mp.h"
#include "dbinc/os_aio.h"
#include "dbinc/log.h"
#include "dbinc/txn.h"

static int __memp_pgwrite
	       __P((ENV *, DB_MPOOLFILE *, DB_MPOOL_HASH *, BH *));

/*
 * __memp_bhwrite --
 *	Write the page associated with a given buffer header.
 *
 * PUBLIC: int __memp_bhwrite __P((DB_MPOOL *,
 * PUBLIC:      DB_MPOOL_HASH *, MPOOLFILE *, BH *, int));
 */
int
__memp_bhwrite(dbmp, hp, mfp, bhp, open_extents)
	DB_MPOOL *dbmp;
	DB_MPOOL_HASH *hp;
	MPOOLFILE *mfp;
	BH *bhp;
	int open_extents;
{
	DB_MPOOLFILE *dbmfp;
	DB_MPREG *mpreg;
	ENV *env;
	int opened, ret;

	env = dbmp->env;
	opened = 0;

	/*
	 * If the file has been removed or is a closed temporary file, we're
	 * done -- the page-write function knows how to handle the fact that
	 * we don't have (or need!) any real file descriptor information.
	 */
	if (mfp->deadfile)
		return (__memp_pgwrite(env, NULL, hp, bhp));

	/*
	 * Walk the process' DB_MPOOLFILE list and find a file descriptor for
	 * the file.  We also check that the descriptor is open for writing.
	 */
	MUTEX_LOCK(env, dbmp->mutex);
	TAILQ_FOREACH(dbmfp, &dbmp->dbmfq, q)
		if (dbmfp->mfp == mfp && !F_ISSET(dbmfp, MP_READONLY)) {
			++dbmfp->ref;
			break;
		}
	MUTEX_UNLOCK(env, dbmp->mutex);

	if (dbmfp != NULL) {
		/*
		 * Temporary files may not have been created.  We only handle
		 * temporary files in this path, because only the process that
		 * created a temporary file will ever flush buffers to it.
		 */
		if (dbmfp->fhp == NULL) {
			/* We may not be allowed to create backing files. */
			if (mfp->no_backing_file) {
				--dbmfp->ref;
				return (EPERM);
			}

			MUTEX_LOCK(env, dbmp->mutex);
			if (dbmfp->fhp == NULL) {
				ret = __db_tmp_open(env,
				    F_ISSET(env->dbenv, DB_ENV_DIRECT_DB) ?
				    DB_OSO_DIRECT : 0, &dbmfp->fhp);
			} else
				ret = 0;
			MUTEX_UNLOCK(env, dbmp->mutex);
			if (ret != 0) {
				__db_errx(env, DB_STR("3014",
			    "unable to create temporary backing file"));
				--dbmfp->ref;
				return (ret);
			}
		}

		goto pgwrite;
	}

	/*
	 * There's no file handle for this file in our process.
	 *
	 * !!!
	 * It's the caller's choice if we're going to open extent files.
	 */
	if (!open_extents && F_ISSET(mfp, MP_EXTENT))
		return (EPERM);

	/*
	 * !!!
	 * Don't try to attach to temporary files.  There are two problems in
	 * trying to do that.  First, if we have different privileges than the
	 * process that "owns" the temporary file, we might create the backing
	 * disk file such that the owning process couldn't read/write its own
	 * buffers, e.g., memp_trickle running as root creating a file owned
	 * as root, mode 600.  Second, if the temporary file has already been
	 * created, we don't have any way of finding out what its real name is,
	 * and, even if we did, it was already unlinked (so that it won't be
	 * left if the process dies horribly).  This decision causes a problem,
	 * however: if the temporary file consumes the entire buffer cache,
	 * and the owner doesn't flush the buffers to disk, we could end up
	 * with resource starvation, and the memp_trickle thread couldn't do
	 * anything about it.  That's a pretty unlikely scenario, though.
	 *
	 * Note we should never get here when the temporary file in question
	 * has already been closed in another process, in which case it should
	 * be marked dead.
	 */
	if (F_ISSET(mfp, MP_TEMP) || mfp->no_backing_file)
		return (EPERM);

	/*
	 * It's not a page from a file we've opened.  If the file requires
	 * application-specific input/output processing, see if this process
	 * has ever registered information as to how to write this type of
	 * file.  If not, there's nothing we can do.
	 */
	if (mfp->ftype != 0 && mfp->ftype != DB_FTYPE_SET) {
		MUTEX_LOCK(env, dbmp->mutex);
		LIST_FOREACH(mpreg, &dbmp->dbregq, q)
			if (mpreg->ftype == mfp->ftype)
				break;
		MUTEX_UNLOCK(env, dbmp->mutex);
		if (mpreg == NULL)
			return (EPERM);
	}

	/*
	 * Try and open the file, specifying the known underlying shared area.
	 *
	 * !!!
	 * There's no negative cache, so we may repeatedly try and open files
	 * that we have previously tried (and failed) to open.
	 */
	if ((ret = __memp_fcreate(env, &dbmfp)) != 0)
		return (ret);
	/*
	 * The open will set MP_FLUSH and so we need to keep
	 * a checkpoint from closing this before we finish with it.
	 */
	dbmfp->ref++;
	opened = 1;
	if ((ret = __memp_fopen(dbmfp, mfp, NULL,
	    NULL, DB_FLUSH | DB_DURABLE_UNKNOWN, 0, mfp->pagesize)) != 0) {
	    	dbmfp->ref--;
		(void)__memp_fclose(dbmfp, 0);

		/*
		 * Ignore any error if the file is marked dead, assume the file
		 * was removed from under us.
		 */
		if (!mfp->deadfile)
			return (ret);

		dbmfp = NULL;
	}

pgwrite:
	MVCC_MPROTECT(bhp->buf, mfp->pagesize,
	    PROT_READ | PROT_WRITE | PROT_EXEC);
	ret = __memp_pgwrite(env, dbmfp, hp, bhp);
	if (dbmfp == NULL)
		return (ret);

	/*
	 * Discard our reference, and, if we're the last reference, make sure
	 * the file eventually gets closed.
	 */
	MUTEX_LOCK(env, dbmp->mutex);
	if (!opened && dbmfp->ref == 1) {
		/*
		 * If we are the last reference, then we need to mark
		 * this as having been used to flush.  If this dbmf
		 * has not been counted as a neutral reference do it.
		 *
		 * Getting the mfp mutex while holding the dbmp is
		 * ok we never do it in the reverse order.
		 */
		if (!F_ISSET(dbmfp, MP_FLUSH)) {
			F_SET(dbmfp, MP_FLUSH);
			MUTEX_LOCK(env,dbmfp->mfp->mutex);
			if (!F_ISSET(dbmfp, MP_FOR_FLUSH)) {
				mfp->neutral_cnt++;
				F_SET(dbmfp, MP_FOR_FLUSH);
			}
			MUTEX_UNLOCK(env, dbmfp->mfp->mutex);
		}
	} else
		--dbmfp->ref;
	MUTEX_UNLOCK(env, dbmp->mutex);

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
	ENV *env;
	MPOOLFILE *mfp;
	size_t len, nr;
	u_int32_t pagesize;
	int ret;

	env = dbmfp->env;
	mfp = dbmfp->mfp;
	pagesize = mfp->pagesize;

	/* We should never be called with a dirty or unlocked buffer. */
	DB_ASSERT(env, !F_ISSET(bhp, BH_DIRTY_CREATE | BH_FROZEN));
	DB_ASSERT(env, can_create ||
	    F_ISSET(bhp, BH_TRASH) || !F_ISSET(bhp, BH_DIRTY));
	DB_ASSERT(env, F_ISSET(bhp, BH_EXCLUSIVE));

	/* Mark the buffer as in transition. */
	F_SET(bhp, BH_TRASH);

	/*
	 * Temporary files may not yet have been created.  We don't create
	 * them now, we create them when the pages have to be flushed.
	 */
	nr = 0;
	if (dbmfp->fhp != NULL) {
		PERFMON3(env, mpool, read, __memp_fn(dbmfp), bhp->pgno, bhp);
		if ((ret = __os_io(env, DB_IO_READ, dbmfp->fhp,
		    bhp->pgno, pagesize, 0, pagesize, bhp->buf, &nr)) != 0)
			goto err;
	}

	/*
	 * The page may not exist; if it doesn't, nr may well be 0, but we
	 * expect the underlying OS calls not to return an error code in
	 * this case.
	 */
	if (nr < pagesize) {
		/*
		 * Don't output error messages for short reads.  In particular,
		 * DB recovery processing may request pages never written to
		 * disk or for which only some part have been written to disk,
		 * in which case we won't find the page.  The caller must know
		 * how to handle the error.
		 */
		if (!can_create) {
			ret = DB_PAGE_NOTFOUND;
			goto err;
		}

		/* Clear any bytes that need to be cleared. */
		len = mfp->clear_len == DB_CLEARLEN_NOTSET ?
		    pagesize : mfp->clear_len;
		memset(bhp->buf, 0, len);

#if defined(DIAGNOSTIC) || defined(UMRW)
		/*
		 * If we're running in diagnostic mode, corrupt any bytes on
		 * the page that are unknown quantities for the caller.
		 */
		if (len < pagesize)
			memset(bhp->buf + len, CLEAR_BYTE, pagesize - len);
#endif
		STAT_INC_VERB(env, mpool, page_create,
		    mfp->stat.st_page_create, __memp_fn(dbmfp), bhp->pgno);
	} else
		STAT_INC_VERB(env, mpool, page_in,
		    mfp->stat.st_page_in, __memp_fn(dbmfp), bhp->pgno);

	/* Call any pgin function. */
	ret = mfp->ftype == 0 ? 0 : __memp_pg(dbmfp, bhp->pgno, bhp->buf, 1);

	/*
	 * If no errors occurred, the data is now valid, clear the BH_TRASH
	 * flag.
	 */
	if (ret == 0)
		F_CLR(bhp, BH_TRASH);
err:	return (ret);
}

/*
 * __memp_pgwrite_prep --
 *	Everything before the page write: WAL flush, write-ahead verification,
 *	backup coordination, and pgout.  On return *do_iop is set if there is a
 *	page image (c->buf) to write; otherwise the caller skips the I/O and
 *	calls __memp_pgwrite_finish directly.  Returns 0 or an errno.
 */
static int
__memp_pgwrite_prep(env, dbmfp, hp, bhp, c, do_iop)
	ENV *env;
	DB_MPOOLFILE *dbmfp;
	DB_MPOOL_HASH *hp;
	BH *bhp;
	MEMP_PGW *c;
	int *do_iop;
{
	DB_LSN lsn;
	int ret;

	memset(c, 0, sizeof(*c));
	c->env = env;
	c->dbmfp = dbmfp;
	c->hp = hp;
	c->bhp = bhp;
	*do_iop = 0;

	/* Another thread could have already written this buffer. */
	if (!F_ISSET(bhp, BH_DIRTY))
		return (0);

	c->mfp = dbmfp == NULL ? NULL : dbmfp->mfp;
	ret = 0;

	/* We should never be called with a frozen or trashed buffer. */
	DB_ASSERT(env, !F_ISSET(bhp, BH_FROZEN | BH_TRASH));

	/* The underlying file may not exist; finish discards the dirty page. */
	if (c->mfp == NULL || c->mfp->deadfile)
		return (0);

	/* Ensure the page's log records are on disk (WAL). */
	if (LOGGING_ON(env) && c->mfp->lsn_off != DB_LSN_OFF_NOTSET &&
	    !IS_CLIENT_PGRECOVER(env)) {
		memcpy(&lsn, bhp->buf + c->mfp->lsn_off, sizeof(DB_LSN));
		if (!IS_NOT_LOGGED_LSN(lsn) &&
		    (ret = __log_flush(env, &lsn)) != 0)
			return (ret);
	}

#ifdef DIAGNOSTIC
	if (LOGGING_ON(env) && !IS_NOT_LOGGED_LSN(LSN(bhp->buf)) &&
	    !IS_CLIENT_PGRECOVER(env)) {
		DB_LOG *dblp;
		LOG *lp;

		dblp = env->lg_handle;
		lp = dblp->reginfo.primary;
		if (!lp->db_log_inmemory &&
		    LOG_COMPARE(&lp->s_lsn, &LSN(bhp->buf)) <= 0) {
			MUTEX_LOCK(env, lp->mtx_flush);
			DB_ASSERT(env, F_ISSET(env->dbenv, DB_ENV_NOLOCKING) ||
			    LOG_COMPARE(&lp->s_lsn, &LSN(bhp->buf)) > 0);
			MUTEX_UNLOCK(env, lp->mtx_flush);
		}
	}
#endif

#ifndef HAVE_ATOMICFILEREAD
	if (c->mfp->backup_in_progress != 0) {
		MUTEX_READLOCK(env, c->mfp->mtx_write);
		if (bhp->pgno >= c->mfp->low_pgno &&
		    bhp->pgno <= c->mfp->high_pgno) {
			MUTEX_UNLOCK(env, c->mfp->mtx_write);
			return (EAGAIN);
		}
		atomic_inc(env, &c->mfp->writers);
		c->writers_inced = 1;
		MUTEX_UNLOCK(env, c->mfp->mtx_write);
	} else {
		atomic_inc(env, &c->mfp->writers);
		c->writers_inced = 1;
	}
#endif

	/*
	 * Call any pgout function.  With the page exclusive we reuse it;
	 * otherwise copy it so others can read it while we write.
	 */
	c->buf = bhp->buf;
	if (c->mfp->ftype != 0) {
		if (F_ISSET(bhp, BH_EXCLUSIVE))
			F_SET(bhp, BH_TRASH);
		else {
			if ((ret =
			    __os_malloc(env, c->mfp->pagesize, &c->buf)) != 0) {
				c->buf = NULL;
				return (ret);
			}
			memcpy(c->buf, bhp->buf, c->mfp->pagesize);
		}
		if ((ret = __memp_pg(dbmfp, bhp->pgno, c->buf, 0)) != 0)
			return (ret);
	}

	*do_iop = 1;
	return (0);
}

/*
 * __memp_pgwrite_finish --
 *	Everything after the page write: release the backup-writer count and
 *	page-image copy, update statistics on a successful write, and clear
 *	BH_DIRTY/BH_TRASH under the hash-bucket latch.  "did_io" is set if a
 *	write was actually issued; "io_ret" is its result (0 on success).
 */
static int
__memp_pgwrite_finish(c, did_io, io_ret)
	MEMP_PGW *c;
	int did_io;
	int io_ret;
{
	ENV *env;
	BH *bhp;
	DB_MPOOL_HASH *hp;
	int ret;

	env = c->env;
	bhp = c->bhp;
	hp = c->hp;
	ret = io_ret;

#ifndef HAVE_ATOMICFILEREAD
	if (c->writers_inced)
		atomic_dec(env, &c->mfp->writers);
#endif

	if (did_io && ret == 0 && c->mfp != NULL) {
		STAT_INC_VERB(env, mpool, page_out,
		    c->mfp->stat.st_page_out, __memp_fn(c->dbmfp), bhp->pgno);
		if (bhp->pgno > c->mfp->last_flushed_pgno) {
			MUTEX_LOCK(env, c->mfp->mutex);
			if (bhp->pgno > c->mfp->last_flushed_pgno)
				c->mfp->last_flushed_pgno = bhp->pgno;
			MUTEX_UNLOCK(env, c->mfp->mutex);
		}
	}

	if (c->buf != NULL && c->buf != bhp->buf)
		__os_free(env, c->buf);

	/*
	 * Update the hash bucket statistics, reset the flags.  On success the
	 * page is no longer dirty.  We latch the hash bucket because this is
	 * the only place the flags are updated holding only a shared latch.
	 */
	if (F_ISSET(bhp, BH_DIRTY | BH_TRASH)) {
		MUTEX_LOCK(env, hp->mtx_hash);
		DB_ASSERT(env, !SH_CHAIN_HASNEXT(bhp, vc));
		if (ret == 0 && F_ISSET(bhp, BH_DIRTY)) {
			F_CLR(bhp, BH_DIRTY | BH_DIRTY_CREATE);
			DB_ASSERT(env, atomic_read(&hp->hash_page_dirty) > 0);
			atomic_dec(env, &hp->hash_page_dirty);
		}

		/* put the page back if necessary. */
		if ((ret != 0 || BH_REFCOUNT(bhp) > 1) &&
		    F_ISSET(bhp, BH_TRASH)) {
			ret = __memp_pg(c->dbmfp, bhp->pgno, bhp->buf, 1);
			F_CLR(bhp, BH_TRASH);
		}
		MUTEX_UNLOCK(env, hp->mtx_hash);
	}

	return (ret);
}

/*
 * __memp_pgwrite --
 *	Write a page to a file.
 */
static int
__memp_pgwrite(env, dbmfp, hp, bhp)
	ENV *env;
	DB_MPOOLFILE *dbmfp;
	DB_MPOOL_HASH *hp;
	BH *bhp;
{
	MEMP_PGW c;
	size_t nw;
	int did_io, do_io, ret;

	ret = __memp_pgwrite_prep(env, dbmfp, hp, bhp, &c, &do_io);
	did_io = 0;
	if (ret == 0 && do_io) {
		PERFMON3(env, mpool, write, __memp_fn(dbmfp), bhp->pgno, bhp);
		did_io = 1;
		if ((ret = __os_io(env, DB_IO_WRITE, dbmfp->fhp, bhp->pgno,
		    c.mfp->pagesize, 0, c.mfp->pagesize, c.buf, &nw)) != 0)
			__db_errx(env, DB_STR_A("3015",
			    "%s: write failed for page %lu", "%s %lu"),
			    __memp_fn(dbmfp), (u_long)bhp->pgno);
	}
	return (__memp_pgwrite_finish(&c, did_io, ret));
}

/*
 * __memp_aio_writeback_done --
 *	os_aio completion callback for an async checkpoint write.  Records the
 *	result only; the finish + reference release run in the reaping thread
 *	(via __memp_aio_drain), so no latches are taken in this callback.
 */
static void
__memp_aio_writeback_done(env, cookie, io_ret)
	ENV *env;
	void *cookie;
	int io_ret;
{
	MEMP_AIO_W *w;

	COMPQUIET(env, NULL);
	w = cookie;
	w->io_ret = io_ret;
	w->done = 1;
}

/*
 * __memp_aio_writeback_finish --
 *	Complete one async checkpoint write: run the shared page-write finish,
 *	then release the held file-handle reference (mirrors __memp_bhwrite's
 *	tail; opened is always 0 on the async fast path).
 */
static int
__memp_aio_writeback_finish(dbmp, w)
	DB_MPOOL *dbmp;
	MEMP_AIO_W *w;
{
	DB_MPOOLFILE *dbmfp;
	ENV *env;
	MPOOLFILE *mfp;
	int ret;

	env = dbmp->env;
	ret = __memp_pgwrite_finish(&w->ctx, 1, w->io_ret);

	dbmfp = w->dbmfp;
	mfp = dbmfp->mfp;
	MUTEX_LOCK(env, dbmp->mutex);
	if (!w->opened && dbmfp->ref == 1) {
		if (!F_ISSET(dbmfp, MP_FLUSH)) {
			F_SET(dbmfp, MP_FLUSH);
			MUTEX_LOCK(env, mfp->mutex);
			if (!F_ISSET(dbmfp, MP_FOR_FLUSH)) {
				mfp->neutral_cnt++;
				F_SET(dbmfp, MP_FOR_FLUSH);
			}
			MUTEX_UNLOCK(env, mfp->mutex);
		}
	} else
		--dbmfp->ref;
	MUTEX_UNLOCK(env, dbmp->mutex);
	return (ret);
}

/*
 * __memp_bhwrite_async --
 *	Asynchronous variant of __memp_bhwrite for the checkpoint/sync path.
 *	For the common case -- a durable, already-open file handle -- it
 *	prepares the write and submits it via os_aio, holding the buffer pin
 *	(the caller's ref + shared mtx_buf) and a file-handle reference until
 *	completion; *deferredp is set and the caller must later reap via
 *	__memp_aio_drain.  Dead/temporary/extent/unopened/read-only files and
 *	the skip/error cases are handled synchronously here (*deferredp == 0).
 *
 * PUBLIC: int __memp_bhwrite_async __P((DB_MPOOL *, DB_MPOOL_HASH *,
 * PUBLIC:     MPOOLFILE *, BH *, struct __db_aio_context *, MEMP_AIO_W *,
 * PUBLIC:     int *));
 */
int
__memp_bhwrite_async(dbmp, hp, mfp, bhp, aioc, w, deferredp)
	DB_MPOOL *dbmp;
	DB_MPOOL_HASH *hp;
	MPOOLFILE *mfp;
	BH *bhp;
	struct __db_aio_context *aioc;
	MEMP_AIO_W *w;
	int *deferredp;
{
	DB_AIO_OP op;
	DB_MPOOLFILE *dbmfp;
	ENV *env;
	size_t nw;
	int do_io, ret;

	env = dbmp->env;
	*deferredp = 0;

	/* Only an already-open durable handle takes the async fast path. */
	if (mfp->deadfile)
		return (__memp_bhwrite(dbmp, hp, mfp, bhp, 1));
	MUTEX_LOCK(env, dbmp->mutex);
	TAILQ_FOREACH(dbmfp, &dbmp->dbmfq, q)
		if (dbmfp->mfp == mfp && !F_ISSET(dbmfp, MP_READONLY)) {
			++dbmfp->ref;
			break;
		}
	MUTEX_UNLOCK(env, dbmp->mutex);
	if (dbmfp == NULL || dbmfp->fhp == NULL) {
		if (dbmfp != NULL) {
			MUTEX_LOCK(env, dbmp->mutex);
			--dbmfp->ref;
			MUTEX_UNLOCK(env, dbmp->mutex);
		}
		return (__memp_bhwrite(dbmp, hp, mfp, bhp, 1));
	}

	/* Prepare the write (WAL flush + pgout) into the slot's context. */
	ret = __memp_pgwrite_prep(env, dbmfp, hp, bhp, &w->ctx, &do_io);
	if (ret != 0 || !do_io) {
		ret = __memp_pgwrite_finish(&w->ctx, 0, ret);
		MUTEX_LOCK(env, dbmp->mutex);
		--dbmfp->ref;
		MUTEX_UNLOCK(env, dbmp->mutex);
		return (ret);
	}

	w->bhp = bhp;
	w->dbmfp = dbmfp;
	w->opened = 0;
	w->io_ret = 0;
	w->done = 0;

	op.op = DB_IO_WRITE;
	op.fhp = dbmfp->fhp;
	op.pgno = bhp->pgno;
	op.pagesize = w->ctx.mfp->pagesize;
	op.buf = w->ctx.buf;
	op.cookie = w;
	op.done = __memp_aio_writeback_done;
	if ((ret = __os_aio_submit(env, aioc, &op)) != 0) {
		/* Submit failed: complete the write synchronously. */
		ret = __os_io(env, DB_IO_WRITE, dbmfp->fhp, bhp->pgno,
		    op.pagesize, 0, op.pagesize, w->ctx.buf, &nw);
		ret = __memp_pgwrite_finish(&w->ctx, 1, ret);
		MUTEX_LOCK(env, dbmp->mutex);
		--dbmfp->ref;
		MUTEX_UNLOCK(env, dbmp->mutex);
		return (ret);
	}
	*deferredp = 1;
	return (0);
}

/*
 * __memp_aio_drain --
 *	Reap all "n" outstanding async checkpoint writes, run each completion
 *	(BH_DIRTY clear + file-handle release), and release each buffer pin.
 *	Returns the number of writes completed (n).
 *
 * PUBLIC: int __memp_aio_drain __P((ENV *, DB_MPOOL *,
 * PUBLIC:     struct __db_aio_context *, MEMP_AIO_W *, int));
 */
int
__memp_aio_drain(env, dbmp, aioc, w, n)
	ENV *env;
	DB_MPOOL *dbmp;
	struct __db_aio_context *aioc;
	MEMP_AIO_W *w;
	int n;
{
	int got, j;

	for (got = 0; got < n; )
		got += __os_aio_reap(env, aioc, -1, 1);
	for (j = 0; j < n; j++) {
		(void)__memp_aio_writeback_finish(dbmp, &w[j]);
		DB_ASSERT(env, atomic_read(&w[j].bhp->ref) > 0);
		atomic_dec(env, &w[j].bhp->ref);
		MUTEX_UNLOCK(env, w[j].bhp->mtx_buf);
	}
	return (n);
}

/*
 * __memp_pg --
 *	Call the pgin/pgout routine.
 *
 * PUBLIC: int __memp_pg __P((DB_MPOOLFILE *, db_pgno_t, void *, int));
 */
int
__memp_pg(dbmfp, pgno, buf, is_pgin)
	DB_MPOOLFILE *dbmfp;
	db_pgno_t pgno;
	void *buf;
	int is_pgin;
{
	DBT dbt, *dbtp;
	DB_MPOOL *dbmp;
	DB_MPREG *mpreg;
	ENV *env;
	MPOOLFILE *mfp;
	int ftype, ret;

	env = dbmfp->env;
	dbmp = env->mp_handle;
	mfp = dbmfp->mfp;

	if ((ftype = mfp->ftype) == DB_FTYPE_SET)
		mpreg = dbmp->pg_inout;
	else {
		MUTEX_LOCK(env, dbmp->mutex);
		LIST_FOREACH(mpreg, &dbmp->dbregq, q)
			if (ftype == mpreg->ftype)
				break;
		MUTEX_UNLOCK(env, dbmp->mutex);
	}
	if (mpreg == NULL)
		return (0);

	if (mfp->pgcookie_len == 0)
		dbtp = NULL;
	else {
		DB_SET_DBT(dbt, R_ADDR(
		    dbmp->reginfo, mfp->pgcookie_off), mfp->pgcookie_len);
		dbtp = &dbt;
	}

	if (is_pgin) {
		if (mpreg->pgin != NULL && (ret =
		    mpreg->pgin(env->dbenv, pgno, buf, dbtp)) != 0)
			goto err;
	} else
		if (mpreg->pgout != NULL && (ret =
		    mpreg->pgout(env->dbenv, pgno, buf, dbtp)) != 0)
			goto err;

	return (0);

err:	__db_errx(env, DB_STR_A("3016",
	    "%s: %s failed for page %lu", "%s %s %lu"), __memp_fn(dbmfp),
	    is_pgin ? DB_STR_P("pgin") : DB_STR_P("pgout"), (u_long)pgno);
	return (ret);
}

/*
 * __memp_bhfree --
 *	Free a bucket header and its referenced data.
 *
 * PUBLIC: int __memp_bhfree __P((DB_MPOOL *,
 * PUBLIC:	REGINFO *, MPOOLFILE *, DB_MPOOL_HASH *, BH *, u_int32_t));
 */
int
__memp_bhfree(dbmp, infop, mfp, hp, bhp, flags)
	DB_MPOOL *dbmp;
	REGINFO *infop;
	MPOOLFILE *mfp;
	DB_MPOOL_HASH *hp;
	BH *bhp;
	u_int32_t flags;
{
	ENV *env;
#ifdef DIAGNOSTIC
	DB_LSN vlsn;
#endif
	BH *prev_bhp;
	MPOOL *c_mp;
	int ret, t_ret;
#ifdef DIAG_MVCC
	size_t pagesize;
#endif

	ret = 0;

	/*
	 * Assumes the hash bucket is locked and the MPOOL is not.
	 */
	env = dbmp->env;
#ifdef DIAG_MVCC
	if (mfp != NULL)
		pagesize = mfp->pagesize;
#endif

	DB_ASSERT(env, LF_ISSET(BH_FREE_UNLOCKED) ||
	    (hp != NULL && MUTEX_IS_OWNED(env, hp->mtx_hash)));
	DB_ASSERT(env, BH_REFCOUNT(bhp) == 1 &&
	    !F_ISSET(bhp, BH_DIRTY | BH_FROZEN));
	DB_ASSERT(env, LF_ISSET(BH_FREE_UNLOCKED) ||
	    SH_CHAIN_SINGLETON(bhp, vc) || (SH_CHAIN_HASNEXT(bhp, vc) &&
	    (SH_CHAIN_NEXTP(bhp, vc, __bh)->td_off == bhp->td_off ||
	    bhp->td_off == INVALID_ROFF ||
	    IS_MAX_LSN(*VISIBLE_LSN(env, bhp)) ||
	    BH_OBSOLETE(bhp, hp->old_reader, vlsn))));

	PERFMON3(env, mpool, evict, __memp_fns(dbmp, mfp), bhp->pgno, bhp);

	/*
	 * Delete the buffer header from the hash bucket queue or the
	 * version chain.
	 */
	if (hp == NULL)
		goto no_hp;
	prev_bhp = SH_CHAIN_PREV(bhp, vc, __bh);
	if (!SH_CHAIN_HASNEXT(bhp, vc)) {
		if (prev_bhp != NULL)
			SH_TAILQ_INSERT_AFTER(&hp->hash_bucket,
			    bhp, prev_bhp, hq, __bh);
		SH_TAILQ_REMOVE(&hp->hash_bucket, bhp, hq, __bh);
	}
	SH_CHAIN_REMOVE(bhp, vc, __bh);

	/*
	 * Remove the reference to this buffer from the transaction that
	 * created it, if any.  When the BH_FREE_UNLOCKED flag is set, we're
	 * discarding the environment, so the transaction region is already
	 * gone.
	 */
	if (bhp->td_off != INVALID_ROFF && !LF_ISSET(BH_FREE_UNLOCKED)) {
		ret = __txn_remove_buffer(
		    env, BH_OWNER(env, bhp), hp->mtx_hash);
		bhp->td_off = INVALID_ROFF;
	}

	/*
	 * We're going to use the memory for something else -- it had better be
	 * accessible.
	 */
no_hp:	if (mfp != NULL)
		MVCC_MPROTECT(bhp->buf,
		    pagesize, PROT_READ | PROT_WRITE | PROT_EXEC);

	/*
	 * Discard the hash bucket's mutex, it's no longer needed, and
	 * we don't want to be holding it when acquiring other locks.
	 */
	if (!LF_ISSET(BH_FREE_UNLOCKED))
		MUTEX_UNLOCK(env, hp->mtx_hash);

	/*
	 * If we're only removing this header from the chain for reuse, we're
	 * done.
	 */
	if (LF_ISSET(BH_FREE_REUSE))
		return (ret);

	/*
	 * If we're not reusing the buffer immediately, free the buffer for
	 * real.
	 */
	if (!LF_ISSET(BH_FREE_UNLOCKED))
		MUTEX_UNLOCK(env, bhp->mtx_buf);
	if (LF_ISSET(BH_FREE_FREEMEM)) {
		if ((ret = __mutex_free(env, &bhp->mtx_buf)) != 0)
			return (ret);
		MPOOL_REGION_LOCK(env, infop);

		MVCC_BHUNALIGN(bhp);
		__memp_free(infop, bhp);
		c_mp = infop->primary;
		c_mp->pages--;

		MPOOL_REGION_UNLOCK(env, infop);
	}

	if (mfp == NULL)
		return (ret);

	/*
	 * Decrement the reference count of the underlying MPOOLFILE.
	 * If this is its last reference, remove it.
	 */
	MUTEX_LOCK(env, mfp->mutex);
	if (--mfp->block_cnt == 0 && mfp->mpf_cnt == 0) {
		if ((t_ret = __memp_mf_discard(dbmp, mfp, 0)) != 0 && ret == 0)
			ret = t_ret;
	} else
		MUTEX_UNLOCK(env, mfp->mutex);

	return (ret);
}
