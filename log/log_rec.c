/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */
/*
 * Copyright (c) 1995, 1996
 *	The President and Fellows of Harvard University.  All rights reserved.
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
static const char sccsid[] = "@(#)log_rec.c	11.16 (Sleepycat) 10/19/99";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <assert.h>
#include <errno.h>
#include <string.h>
#endif

#include "db_int.h"
#include "log.h"
#include "db_dispatch.h"
#include "db_page.h"
#include "db_ext.h"

static int __log_do_open __P((DB_LOG *, u_int8_t *, char *, DBTYPE, u_int32_t));
static int __log_lid_to_fname __P((DB_LOG *, int32_t, FNAME **));
static int __log_open_file __P((DB_LOG *, __log_register_args *));

/*
 * PUBLIC: int __log_register_recover
 * PUBLIC:     __P((DB_ENV *, DBT *, DB_LSN *, int, void *));
 */
int
__log_register_recover(dbenv, dbtp, lsnp, redo, info)
	DB_ENV *dbenv;
	DBT *dbtp;
	DB_LSN *lsnp;
	int redo;
	void *info;
{
	DB_ENTRY *dbe;
	DB_LOG *logp;
	__log_register_args *argp;
	int do_rem, ret, t_ret;

	logp = dbenv->lg_handle;

#ifdef DEBUG_RECOVER
	__log_register_print(logp, dbtp, lsnp, redo, info);
#endif
	COMPQUIET(lsnp, NULL);

	F_SET(logp, DBC_RECOVER);

	if ((ret = __log_register_read(dbtp->data, &argp)) != 0)
		goto out;

	if ((argp->opcode == LOG_OPEN &&
	    (redo == TXN_REDO || redo == TXN_OPENFILES ||
	     redo == TXN_FORWARD_ROLL)) ||
	    (argp->opcode == LOG_CLOSE &&
	    (redo == TXN_UNDO || redo == TXN_BACKWARD_ROLL))) {
		/*
		 * If we are redoing an open or undoing a close, then we need
		 * to open a file.
		 */
		ret = __log_open_file(logp, argp);
		if (ret == ENOENT || ret == EINVAL) {
			if (redo == TXN_OPENFILES && argp->name.size != 0 &&
			    (ret = __db_txnlist_delete(info,
			        argp->name.data, argp->id, 0)) != 0)
				goto out;
			ret = 0;
		}
	} else if (argp->opcode != LOG_CHECKPOINT) {
		/*
		 * If we are undoing an open, then we need to close the file.
		 *
  		 * If the file is deleted, then we can just ignore this close.
 		 * Otherwise, we should usually have a valid dbp we should
  		 * close or whose reference count should be decremented.
 		 * However, if we shut down without closing a file, we may, in
 		 * fact, not have the file open, and that's OK.
		 */
		do_rem = 0;
		MUTEX_THREAD_LOCK(logp->mutexp);
		if (argp->id < logp->dbentry_cnt) {
			dbe = &logp->dbentry[argp->id];
#ifdef DIAGNOSTIC
			assert(dbe->refcount == 1);
#endif
			ret = __db_txnlist_close(info, argp->id, dbe->count);
			if (dbe->dbp != NULL &&
			    (t_ret = dbe->dbp->close(dbe->dbp, 0)) != 0
			    && ret == 0)
				ret = t_ret;
			do_rem = 1;
		}
		MUTEX_THREAD_UNLOCK(logp->mutexp);
		if (do_rem)
			(void)__log_rem_logid(logp, argp->id);
 	} else if ((redo == TXN_UNDO || redo == TXN_OPENFILES) &&
	    (argp->id >= logp->dbentry_cnt ||
 	    (!logp->dbentry[argp->id].deleted &&
 	    logp->dbentry[argp->id].dbp == NULL))) {
 		/*
 		 * It's a checkpoint and we are rolling backward.  It
 		 * is possible that the system was shut down and thus
 		 * ended with a stable checkpoint; this file was never
 		 * closed and has therefore not been reopened yet.  If
 		 * so, we need to try to open it.
 		 */
 		ret = __log_open_file(logp, argp);
 		if (ret == ENOENT || ret == EINVAL) {
			if (argp->name.size != 0 && (ret =
			    __db_txnlist_delete(info,
			        argp->name.data, argp->id, 0)) != 0)
				goto out;
 			ret = 0;
 		}
	}

out:	F_CLR(logp, DBC_RECOVER);
	if (argp != NULL)
		__os_free(argp, 0);
	return (ret);
}

/*
 * __log_open_file --
 *	Called during log_register recovery.  Make sure that we have an
 *	entry in the dbentry table for this ndx.  Returns 0 on success,
 *	non-zero on error.
 */
static int
__log_open_file(lp, argp)
	DB_LOG *lp;
	__log_register_args *argp;
{
	DB_ENTRY *dbe;

	/*
	 * We never re-open temporary files.  Temp files are only
	 * useful during aborts in which case the dbp was entered
	 * when the file was registered.  During recovery, we treat
	 * temp files as properly deleted files, allowing the open to
	 * fail and not reporting any errors when recovery fails to
	 * get a valid dbp from db_fileid_to_db.
	 */
	if (argp->name.size == 0) {
		(void)__log_add_logid(lp, NULL, argp->id);
		return (ENOENT);
	}

	/*
	 * Because of reference counting, we cannot automatically close files
	 * during recovery, so when we're opening, we have to check that the
	 * name we are opening is what we expect.  If it's not, then we close
	 * the old file and open the new one.
	 */
	MUTEX_THREAD_LOCK(lp->mutexp);
	if (argp->id < lp->dbentry_cnt)
		dbe = &lp->dbentry[argp->id];
	else
		dbe = NULL;

	if (dbe != NULL && (dbe->deleted == 1 || dbe->dbp != NULL)) {
		dbe->refcount++;
		MUTEX_THREAD_UNLOCK(lp->mutexp);
		return (0);
	}

	MUTEX_THREAD_UNLOCK(lp->mutexp);

	return (__log_do_open(lp,
	    argp->uid.data, argp->name.data, argp->ftype, argp->id));
}

/*
 * __log_do_open --
 * 	Open files referenced in the log.  This is the part of the open that
 * is not protected by the thread mutex.
 */
static int
__log_do_open(lp, uid, name, ftype, ndx)
	DB_LOG *lp;
	u_int8_t *uid;
	char *name;
	DBTYPE ftype;
	u_int32_t ndx;
{
	DB *dbp;
	int ret;
	u_int8_t zeroid[DB_FILE_ID_LEN];

	if ((ret = db_create(&dbp, lp->dbenv, 0)) != 0)
		return (ret);
	if ((ret = dbp->open(dbp, name, NULL, ftype, 0, 0600)) == 0) {
		/*
		 * Verify that we are opening the same file that we were
		 * referring to when we wrote this log record.
		 */
		memset(zeroid, 0, DB_FILE_ID_LEN);
		if (memcmp(uid, dbp->fileid, DB_FILE_ID_LEN) == 0 ||
		    memcmp(dbp->fileid, zeroid, DB_FILE_ID_LEN) == 0) {
			(void)__log_add_logid(lp, dbp, ndx);
			return (0);
		}
	}
	(void)dbp->close(dbp, 0);
	(void)__log_add_logid(lp, NULL, ndx);

	return (ENOENT);
}

/*
 * __log_add_logid --
 *	Adds a DB entry to the log's DB entry table.
 *
 * PUBLIC: int __log_add_logid __P((DB_LOG *, DB *, u_int32_t));
 */
int
__log_add_logid(logp, dbp, ndx)
	DB_LOG *logp;
	DB *dbp;
	u_int32_t ndx;
{
	u_int32_t i;
	int ret;

	ret = 0;

	MUTEX_THREAD_LOCK(logp->mutexp);

	/*
	 * Check if we need to grow the table.  Note, ndx is 0-based (the
	 * index into the DB entry table) an dbentry_cnt is 1-based, the
	 * number of available slots.
	 */
	if (logp->dbentry_cnt <= ndx) {
		if ((ret = __os_realloc((ndx + DB_GROW_SIZE) * sizeof(DB_ENTRY),
		    NULL, &logp->dbentry)) != 0)
			goto err;

		/* Initialize the new entries. */
		for (i = logp->dbentry_cnt; i < ndx + DB_GROW_SIZE; i++) {
			logp->dbentry[i].count = 0;
			logp->dbentry[i].dbp = NULL;
			logp->dbentry[i].deleted = 0;
			logp->dbentry[i].refcount = 0;
		}

		logp->dbentry_cnt = i;
	}

	if (logp->dbentry[ndx].deleted == 0 &&
	    logp->dbentry[ndx].dbp == NULL) {
		logp->dbentry[ndx].count = 0;
		logp->dbentry[ndx].dbp = dbp;
		logp->dbentry[ndx].deleted = dbp == NULL;
		logp->dbentry[ndx].refcount = 1;
	} else
		logp->dbentry[ndx].refcount++;


err:	MUTEX_THREAD_UNLOCK(logp->mutexp);
	return (ret);
}

/*
 * __db_fileid_to_db --
 *	Return the DB corresponding to the specified fileid.
 *
 * PUBLIC: int __db_fileid_to_db __P((DB_ENV *, DB **, int32_t, int));
 */
int
__db_fileid_to_db(dbenv, dbpp, ndx, inc)
	DB_ENV *dbenv;
	DB **dbpp;
	int32_t ndx;
	int inc;
{
	DB_LOG *logp;
	FNAME *fname;
	int ret;
	char *name;

	ret = 0;
	logp = dbenv->lg_handle;

	MUTEX_THREAD_LOCK(logp->mutexp);

	/*
	 * Under XA, a process different than the one issuing DB operations
	 * may abort a transaction.  In this case, recovery routines are run
	 * by a process that does not necessarily have the file open, so we
	 * we must open the file explicitly.
	 */
	if ((u_int32_t)ndx >= logp->dbentry_cnt ||
	    (!logp->dbentry[ndx].deleted && logp->dbentry[ndx].dbp == NULL)) {
		if (__log_lid_to_fname(logp, ndx, &fname) != 0) {
			/* Couldn't find entry; this is a fatal error. */
			ret = EINVAL;
			goto err;
		}
		name = R_ADDR(&logp->reginfo, fname->name_off);

		/*
		 * __log_do_open is called without protection of the
		 * log thread lock.
		 */
		MUTEX_THREAD_UNLOCK(logp->mutexp);

		/*
		 * At this point, we are not holding the thread lock, so exit
		 * directly instead of going through the exit code at the
		 * bottom.  If the __log_do_open succeeded, then we don't need
		 * to do any of the remaining error checking at the end of this
		 * routine.
		 */
		if ((ret = __log_do_open(logp,
		    fname->ufid, name, fname->s_type, ndx)) != 0)
			return (ret);

		*dbpp = logp->dbentry[ndx].dbp;
		return (0);
	}

	/*
	 * Return DB_DELETED if the file has been deleted (it's not an error).
	 */
	if (logp->dbentry[ndx].deleted) {
		ret = DB_DELETED;
		if (inc)
			logp->dbentry[ndx].count++;
		goto err;
	}

	/*
	 * Otherwise return 0, but if we don't have a corresponding DB, it's
	 * an error.
	 */
	if ((*dbpp = logp->dbentry[ndx].dbp) == NULL)
		ret = ENOENT;

err:	MUTEX_THREAD_UNLOCK(logp->mutexp);
	return (ret);
}

/*
 * Close files that were opened by the recovery daemon.
 *
 * PUBLIC: void __log_close_files __P((DB_ENV *));
 */
void
__log_close_files(dbenv)
	DB_ENV *dbenv;
{
	DB_ENTRY *dbe;
	DB_LOG *logp;
	u_int32_t i;

	logp = dbenv->lg_handle;
	MUTEX_THREAD_LOCK(logp->mutexp);
	F_SET(logp, DBC_RECOVER);
	for (i = 0; i < logp->dbentry_cnt; i++) {
		dbe = &logp->dbentry[i];
		if (dbe->dbp != NULL) {
			(void)dbe->dbp->close(dbe->dbp, 0);
			dbe->dbp = NULL;
		}
		dbe->deleted = 0;
		dbe->refcount = 0;
	}
	F_CLR(logp, DBC_RECOVER);
	MUTEX_THREAD_UNLOCK(logp->mutexp);
}

/*
 * PUBLIC: void __log_rem_logid __P((DB_LOG *, u_int32_t));
 */
void
__log_rem_logid(logp, ndx)
	DB_LOG *logp;
	u_int32_t ndx;
{
	MUTEX_THREAD_LOCK(logp->mutexp);
	if (--logp->dbentry[ndx].refcount == 0) {
		logp->dbentry[ndx].dbp = NULL;
		logp->dbentry[ndx].deleted = 0;
	}
	MUTEX_THREAD_UNLOCK(logp->mutexp);
}

/*
 * __log_lid_to_fname --
 * 	Traverse the shared-memory region looking for the entry that
 *	matches the passed log fileid.  Returns 0 on success; -1 on error.
 */
static int
__log_lid_to_fname(dblp, lid, fnamep)
	DB_LOG *dblp;
	int32_t lid;
	FNAME **fnamep;
{
	FNAME *fnp;
	LOG *lp;

	lp = dblp->reginfo.primary;

	for (fnp = SH_TAILQ_FIRST(&lp->fq, __fname);
	    fnp != NULL; fnp = SH_TAILQ_NEXT(fnp, q, __fname)) {
		if (fnp->ref == 0)	/* Entry not in use. */
			continue;
		if (fnp->id == lid) {
			*fnamep = fnp;
			return (0);
		}
	}
	return (-1);
}
