/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)db_am.c	11.8 (Sleepycat) 11/15/99";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <errno.h>
#include <string.h>
#endif

#include "db_int.h"
#include "db_page.h"
#include "db_shash.h"
#include "btree.h"
#include "hash.h"
#include "qam.h"
#include "lock.h"
#include "mp.h"
#include "txn.h"
#include "db_am.h"
#include "db_ext.h"

static int __db_c_close __P((DBC *));

/*
 * __db_cursor --
 *	Allocate and return a cursor.
 *
 * PUBLIC: int __db_cursor __P((DB *, DB_TXN *, DBC **, u_int32_t));
 */
int
__db_cursor(dbp, txn, dbcp, flags)
	DB *dbp;
	DB_TXN *txn;
	DBC **dbcp;
	u_int32_t flags;
{
	DBC *dbc, *adbc;
	db_lockmode_t mode;
	u_int32_t op;
	int ret;

	PANIC_CHECK(dbp->dbenv);
	DB_ILLEGAL_BEFORE_OPEN(dbp, "DB->cursor");

	/* Check for invalid flags. */
	if ((ret = __db_cursorchk(dbp, flags, F_ISSET(dbp, DB_AM_RDONLY))) != 0)
		return (ret);

	/* Take one from the free list if it's available. */
	MUTEX_THREAD_LOCK(dbp->mutexp);
	if ((dbc = TAILQ_FIRST(&dbp->free_queue)) != NULL)
		TAILQ_REMOVE(&dbp->free_queue, dbc, links);
	else {
		MUTEX_THREAD_UNLOCK(dbp->mutexp);

		if ((ret = __os_calloc(1, sizeof(DBC), &dbc)) != 0)
			return (ret);

		dbc->dbp = dbp;
		dbc->c_close = __db_c_close;
		dbc->c_dup = __db_c_dup;

		/* Set up locking information. */
		if (F_ISSET(dbp->dbenv, DB_ENV_CDB | DB_ENV_LOCKING)) {
			/*
			 * If we are not threaded, then there is no need to
			 * create new locker ids.  We know that no one else
			 * is running concurrently using this DB, so we can
			 * take a peek at any cursors on the active queue.
			 */
			if (!F_ISSET(dbp->dbenv, DB_ENV_THREAD) &&
			    (adbc = TAILQ_FIRST(&dbp->active_queue)) != NULL)
				dbc->lid = adbc->lid;
			else
				if ((ret = lock_id(dbp->dbenv, &dbc->lid)) != 0)
					goto err;

			memcpy(dbc->lock.fileid, dbp->fileid, DB_FILE_ID_LEN);
			if (F_ISSET(dbp->dbenv, DB_ENV_CDB)) {
				dbc->lock_dbt.size = DB_FILE_ID_LEN;
				dbc->lock_dbt.data = dbc->lock.fileid;
			} else {
				dbc->lock.type = DB_PAGE_LOCK;
				dbc->lock_dbt.size = sizeof(dbc->lock);
				dbc->lock_dbt.data = &dbc->lock;
			}
		}

		switch (dbp->type) {
		case DB_BTREE:
		case DB_RECNO:
			if ((ret = __bam_c_init(dbc)) != 0)
				goto err;
			break;
		case DB_HASH:
			if ((ret = __ham_c_init(dbc)) != 0)
				goto err;
			break;
		case DB_QUEUE:
			if ((ret = __qam_c_init(dbc)) != 0)
				goto err;
			break;
		default:
			ret = EINVAL;
			goto err;
		}

		MUTEX_THREAD_LOCK(dbp->mutexp);
	}

	if ((dbc->txn = txn) == NULL)
		dbc->locker = dbc->lid;
	else
		dbc->locker = txn->txnid;

	TAILQ_INSERT_TAIL(&dbp->active_queue, dbc, links);
	MUTEX_THREAD_UNLOCK(dbp->mutexp);

	/*
	 * If this is CDB, then we do all locking in the interface, which is
	 * right here.  However, if we are duplicating a cursor, then we do
	 * not want to acquire any locks here, because we'll do that in the
	 * dup code for the correct locker.
	 */
	op = LF_ISSET(DB_OPFLAGS_MASK);
	if (op != DB_DUPCURSOR && F_ISSET(dbp->dbenv, DB_ENV_CDB)) {
		mode = (op == DB_WRITELOCK) ? DB_LOCK_WRITE :
		    (LF_ISSET(DB_WRITECURSOR) ? DB_LOCK_IWRITE : DB_LOCK_READ);
		if ((ret = lock_get(dbp->dbenv, dbc->locker, 0,
		    &dbc->lock_dbt, mode, &dbc->mylock)) != 0) {
			(void)__db_c_close(dbc);
			return (ret);
		}
		if (LF_ISSET(DB_WRITECURSOR))
			F_SET(dbc, DBC_WRITECURSOR);
		if (op == DB_WRITELOCK)
			F_SET(dbc, DBC_WRITER);
	}

	*dbcp = dbc;
	return (0);

err:	__os_free(dbc, sizeof(*dbc));
	return (ret);
}

/*
 * __db_c_close --
 *	Close the cursor (recycle for later use).
 */
static int
__db_c_close(dbc)
	DBC *dbc;
{
	DB *dbp;
	int ret, t_ret;

	dbp = dbc->dbp;

	PANIC_CHECK(dbp->dbenv);

	ret = 0;

	/*
	 * Remove the cursor from the active queue.
	 *
	 * !!!
	 * This must happen before the access specific cursor close routine
	 * is called, Btree depends on it.
	 */
	MUTEX_THREAD_LOCK(dbp->mutexp);
	TAILQ_REMOVE(&dbp->active_queue, dbc, links);
	MUTEX_THREAD_UNLOCK(dbp->mutexp);

	/* Call the access specific cursor close routine. */
	if ((t_ret = dbc->c_am_close(dbc)) != 0 && ret == 0)
		ret = t_ret;

	/*
	 * Release the lock after calling the access method specific close
	 * routine, a Btree cursor may have had pending deletes.
	 */
	if (F_ISSET(dbc->dbp->dbenv, DB_ENV_CDB) &&
	    dbc->mylock.off != LOCK_INVALID) {
		ret = lock_put(dbc->dbp->dbenv, &dbc->mylock);
		dbc->mylock.off = LOCK_INVALID;
	}

	/* Clean up the cursor. */
	dbc->flags = 0;

#ifdef CLOSE_CURSOR_CHECK_FOR_LEFTOVER_LOCKS
	/*
	 * Check for leftover locks, unless we're running with transactions.
	 *
	 * If we're running tests, display any locks currently held.  It's
	 * possible that some applications may hold locks for long periods,
	 * e.g., conference room locks, but the DB tests should never close
	 * holding locks.
	 */
	if (F_ISSET(dbp->dbenv, DB_ENV_LOCKING) && dbc->lid == dbc->locker) {
		DB_LOCKREQ request;

		request.op = DB_LOCK_DUMP;
		if ((t_ret = lock_vec(dbp->dbenv,
		    dbc->locker, 0, &request, 1, NULL)) != 0 && ret == 0)
			ret = EINVAL;
	}
#endif
	/* Move the cursor to the free queue. */
	MUTEX_THREAD_LOCK(dbp->mutexp);
	TAILQ_INSERT_TAIL(&dbp->free_queue, dbc, links);
	MUTEX_THREAD_UNLOCK(dbp->mutexp);

	return (ret);
}

/*
 * __db_c_dup --
 *	Duplicate a cursor
 *
 * PUBLIC: int __db_c_dup __P((DBC *, DBC **, u_int32_t));
 */
int
__db_c_dup(orig_dbc, dbcp, flags)
	DBC *orig_dbc;
	DBC **dbcp;
	u_int32_t flags;
{
	DB *dbp;
	DBC *dbc;
	int ret;

	PANIC_CHECK(orig_dbc->dbp->dbenv);

	/*
	 * We can never have two write cursors open in CDB, so do not
	 * allow duplication of a write cursor.
	 */
	if (F_ISSET(orig_dbc, DBC_WRITER | DBC_WRITECURSOR) &&
	    flags != DB_POSITIONI)
		return (EINVAL);

	dbp = orig_dbc->dbp;

	/* Allocate a new cursor. */
	if ((ret = dbp->cursor(dbp, orig_dbc->txn, &dbc, DB_DUPCURSOR)) != 0)
		return (ret);

	/* Assign local locker to be the same as the original. */
	dbc->locker = orig_dbc->locker;

	/* If the user wants the cursor positioned, do it here.  */
	if (flags == DB_POSITION || flags == DB_POSITIONI) {
		switch(dbp->type) {
		case DB_QUEUE:
			if ((ret = __qam_c_dup(orig_dbc, dbc)) != 0)
				goto err;
			break;
		case DB_BTREE:
		case DB_RECNO:
			if ((ret = __bam_c_dup(orig_dbc, dbc)) != 0)
				goto err;
			break;
		case DB_HASH:
			if ((ret = __ham_c_dup(orig_dbc, dbc)) != 0)
				goto err;
			break;
		default:
			ret = EINVAL;
			goto err;
		}
		dbc->flags = orig_dbc->flags;
	}
	*dbcp = dbc;
	return (0);

err:	(void)dbc->c_close(dbc);
	return (ret);
}

#ifdef DEBUG
/*
 * __db_cprint --
 *	Display the current cursor list.
 *
 * PUBLIC: int __db_cprint __P((DB *));
 */
int
__db_cprint(dbp)
	DB *dbp;
{
	static const FN fn[] = {
		{ DBC_RECOVER, 		"recover" },
		{ DBC_RMW, 		"read-modify-write" },
		{ DBC_WRITECURSOR,	"write cursor" },
		{ DBC_WRITER, 		"short-term write cursor" },
		{ 0,			NULL }
	};
	BTREE_CURSOR *cp;
	DBC *dbc;

	MUTEX_THREAD_LOCK(dbp->mutexp);
	for (dbc = TAILQ_FIRST(&dbp->active_queue);
	    dbc != NULL; dbc = TAILQ_NEXT(dbc, links)) {
		fprintf(stderr,
		    "%#0x: dbp: %#0x txn: %#0x lid: %lu locker: %lu",
		    (u_int)dbc, (u_int)dbc->dbp, (u_int)dbc->txn,
		    (u_long)dbc->lid, (u_long)dbc->locker);
		if (dbp->type == DB_BTREE) {
			cp = dbc->internal;
			fprintf(stderr, "p/i: %lu/%lu dp/di: %lu/%lu",
			    (u_long)cp->pgno, (u_long)cp->indx,
			    (u_long)cp->dpgno, (u_long)cp->dindx);
		}
		__db_prflags(dbc->flags, fn, stderr);
		fprintf(stderr, "\n");
	}
	MUTEX_THREAD_UNLOCK(dbp->mutexp);

	return (0);
}
#endif /* DEBUG */

/*
 * __db_c_destroy --
 *	Destroy the cursor.
 *
 * PUBLIC: int __db_c_destroy __P((DBC *));
 */
int
__db_c_destroy(dbc)
	DBC *dbc;
{
	DB *dbp;
	int ret;

	dbp = dbc->dbp;

	/* Remove the cursor from the free queue. */
	MUTEX_THREAD_LOCK(dbp->mutexp);
	TAILQ_REMOVE(&dbp->free_queue, dbc, links);
	MUTEX_THREAD_UNLOCK(dbp->mutexp);

	/* Call the access specific cursor destroy routine. */
	ret = dbc->c_am_destroy == NULL ? 0 : dbc->c_am_destroy(dbc);

	/* Free up allocated memory. */
	if (dbc->rkey.data != NULL)
		__os_free(dbc->rkey.data, dbc->rkey.ulen);
	if (dbc->rdata.data != NULL)
		__os_free(dbc->rdata.data, dbc->rdata.ulen);
	__os_free(dbc, sizeof(*dbc));

	return (ret);
}

/*
 * db_fd --
 *	Return a file descriptor for flock'ing.
 *
 * PUBLIC: int __db_fd __P((DB *, int *));
 */
int
__db_fd(dbp, fdp)
        DB *dbp;
	int *fdp;
{
	DB_FH *fhp;
	int ret;

	PANIC_CHECK(dbp->dbenv);
	DB_ILLEGAL_BEFORE_OPEN(dbp, "DB->fd");

	/*
	 * XXX
	 * Truly spectacular layering violation.
	 */
	if ((ret = __mp_xxx_fh(dbp->mpf, &fhp)) != 0)
		return (ret);

	if (F_ISSET(fhp, DB_FH_VALID)) {
		*fdp = fhp->fd;
		return (0);
	} else {
		*fdp = -1;
		return (ENOENT);
	}
}

/*
 * __db_get --
 *	Return a key/data pair.
 *
 * PUBLIC: int __db_get __P((DB *, DB_TXN *, DBT *, DBT *, u_int32_t));
 */
int
__db_get(dbp, txn, key, data, flags)
	DB *dbp;
	DB_TXN *txn;
	DBT *key, *data;
	u_int32_t flags;
{
	DBC *dbc;
	int ret, t_ret;

	PANIC_CHECK(dbp->dbenv);
	DB_ILLEGAL_BEFORE_OPEN(dbp, "DB->get");

	if ((ret = __db_getchk(dbp, key, data, flags)) != 0)
		return (ret);

	if ((ret = dbp->cursor(dbp, txn, &dbc, 0)) != 0)
		return (ret);

	DEBUG_LREAD(dbc, txn, "__db_get", key, NULL, flags);

	ret = dbc->c_get(dbc, key, data,
	    flags == 0 || flags == DB_RMW ? flags | DB_SET : flags);

	if ((t_ret = __db_c_close(dbc)) != 0 && ret == 0)
		ret = t_ret;

	return (ret);
}

/*
 * __db_put --
 *	Store a key/data pair.
 *
 * PUBLIC: int __db_put __P((DB *, DB_TXN *, DBT *, DBT *, u_int32_t));
 */
int
__db_put(dbp, txn, key, data, flags)
	DB *dbp;
	DB_TXN *txn;
	DBT *key, *data;
	u_int32_t flags;
{
	DBC *dbc;
	DBT tdata;
	int ret, t_ret;

	PANIC_CHECK(dbp->dbenv);
	DB_ILLEGAL_BEFORE_OPEN(dbp, "DB->put");

	if ((ret = __db_putchk(dbp, key, data,
	    flags, F_ISSET(dbp, DB_AM_RDONLY), F_ISSET(dbp, DB_AM_DUP))) != 0)
		return (ret);

	if ((ret = dbp->cursor(dbp, txn, &dbc, DB_WRITELOCK)) != 0)
		return (ret);

	DEBUG_LWRITE(dbc, txn, "__db_put", key, data, flags);

	if (flags == DB_NOOVERWRITE) {
		/*
		 * Set DB_DBT_USERMEM, this might be a threaded application and
		 * the flags checking will catch us.  We don't want the actual
		 * data, so request a partial of length 0.
		 */
		memset(&tdata, 0, sizeof(tdata));
		F_SET(&tdata, DB_DBT_USERMEM | DB_DBT_PARTIAL);

		/*
		 * If we're locking, set the read-modify-write flag, we're
		 * going to overwrite immediately.
		 */
		if ((ret = dbc->c_get(dbc, key, &tdata, DB_SET |
		    (F_ISSET(dbp->dbenv, DB_ENV_LOCKING) ? DB_RMW : 0))) == 0)
			ret = DB_KEYEXIST;
		else if (ret == DB_NOTFOUND)
			ret = 0;
	}
	if (ret == 0)
		ret = dbc->c_put(dbc, key, data, DB_KEYLAST);

	if ((t_ret = __db_c_close(dbc)) != 0 && ret == 0)
		ret = t_ret;

	return (ret);
}

/*
 * __db_sync --
 *	Flush the database cache.
 *
 * PUBLIC: int __db_sync __P((DB *, u_int32_t));
 */
int
__db_sync(dbp, flags)
	DB *dbp;
	u_int32_t flags;
{
	int ret, t_ret;

	PANIC_CHECK(dbp->dbenv);
	DB_ILLEGAL_BEFORE_OPEN(dbp, "DB->sync");

	if ((ret = __db_syncchk(dbp, flags)) != 0)
		return (ret);

	/* Read-only trees never need to be sync'd. */
	if (F_ISSET(dbp, DB_AM_RDONLY))
		return (0);

	/* If it's a Recno tree, write the backing source text file. */
	if (dbp->type == DB_RECNO)
		ret = __ram_writeback(dbp);

	/* If the tree was never backed by a database file, we're done. */
	if (F_ISSET(dbp, DB_AM_INMEM))
		return (0);

	/* Flush any dirty pages from the cache to the backing file. */
	if ((t_ret = memp_fsync(dbp->mpf)) != 0 && ret == 0)
		ret = t_ret;
	return (ret);
}

/*
 * __db_log_page
 *	Log a meta-data or root page during a create operation.
 *
 * PUBLIC: int __db_log_page __P((DB *,
 * PUBLIC:     const char *, DB_LSN *, db_pgno_t, PAGE *));
 */
int
__db_log_page(dbp, name, lsn, pgno, page)
	DB *dbp;
	const char *name;
	DB_LSN *lsn;
	db_pgno_t pgno;
	PAGE *page;
{
	DBT name_dbt, page_dbt;
	DB_LSN new_lsn;
	int ret;

	if (dbp->open_txn == NULL)
		return (0);

	memset(&page_dbt, 0, sizeof(page_dbt));
	page_dbt.size = dbp->pgsize;
	page_dbt.data = page;
	if (pgno == PGNO_BASE_MD) {
		/*
		 * !!!
		 * Make sure that we properly handle a null name.  The old
		 * Tcl sent us pathnames of the form ""; it may be the case
		 * that the new Tcl doesn't do that, so we can get rid of
		 * the second check here.
		 */
		memset(&name_dbt, 0, sizeof(name_dbt));
		name_dbt.data = (char *)name;
		if (name == NULL || *name == '\0')
			name_dbt.size = 0;
		else
			name_dbt.size = strlen(name) + 1;

		ret = __crdel_metapage_log(dbp->dbenv,
		    dbp->open_txn, &new_lsn, DB_FLUSH,
		    dbp->log_fileid, &name_dbt, pgno, &page_dbt);
	} else
		ret = __crdel_metasub_log(dbp->dbenv, dbp->open_txn,
		    &new_lsn, 0, dbp->log_fileid, pgno, &page_dbt, lsn);

	if (ret == 0)
		page->lsn = new_lsn;
	return (ret);
}
