/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */
#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)log.c	11.8 (Sleepycat) 9/20/99";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#endif

#include "db_int.h"
#include "log.h"
#include "db_dispatch.h"
#include "txn.h"
#include "txn_auto.h"

static int __log_init __P((DB_ENV *, DB_LOG *));
static int __log_recover __P((DB_LOG *));

/*
 * __log_open --
 *	Internal version of log_open: only called from DB_ENV->open.
 *
 * PUBLIC: int __log_open __P((DB_ENV *));
 */
int
__log_open(dbenv)
	DB_ENV *dbenv;
{
	DB_LOG *dblp;
	LOG *lp;
	int ret;

	/* Create/initialize the DB_LOG structure. */
	if ((ret = __os_calloc(1, sizeof(DB_LOG), &dblp)) != 0)
		return (ret);
	ZERO_LSN(dblp->c_lsn);
	dblp->dbenv = dbenv;

	/* Join/create the log region. */
	dblp->reginfo.id = REG_ID_LOG;
	dblp->reginfo.mode = dbenv->db_mode;
	if (F_ISSET(dbenv, DB_ENV_CREATE))
		F_SET(&dblp->reginfo, REGION_CREATE_OK);
	if ((ret = __db_r_attach(
	    dbenv, &dblp->reginfo, LG_BASE_REGION_SIZE + dbenv->lg_bsize)) != 0)
		goto err;

	/* If we created the region, initialize it. */
	if (F_ISSET(&dblp->reginfo, REGION_CREATE))
		if ((ret = __log_init(dbenv, dblp)) != 0)
			goto err;

	/* Set the local addresses. */
	lp = dblp->reginfo.primary =
	    R_ADDR(&dblp->reginfo, dblp->reginfo.rp->primary);
	dblp->bufp = R_ADDR(&dblp->reginfo, lp->buffer_off);

	R_UNLOCK(dbenv, &dblp->reginfo);

	/*
	 * If the region is threaded, then we have to lock both the handles
	 * and the region, and we need to allocate a mutex for that purpose.
	 */
	if (F_ISSET(dbenv, DB_ENV_THREAD)) {
		if ((ret = __db_mutex_alloc(
		    dbenv, &dblp->reginfo, &dblp->mutexp)) != 0)
			goto detach;
		if ((ret = __db_mutex_init(
		    dbenv, dblp->mutexp, 0, MUTEX_THREAD)) != 0)
			goto detach;
	}

	dbenv->lg_handle = dblp;
	return (0);

err:	if (dblp->reginfo.addr != NULL) {
		if (F_ISSET(&dblp->reginfo, REGION_CREATE))
			F_SET(dblp->reginfo.rp, REG_DEAD);
		R_UNLOCK(dbenv, &dblp->reginfo);

detach:		(void)__db_r_detach(dbenv, &dblp->reginfo, 0);
	}
	__os_free(dblp, sizeof(*dblp));
	return (ret);
}

/*
 * __log_init --
 *	Initialize a log region in shared memory.
 */
static int
__log_init(dbenv, dblp)
	DB_ENV *dbenv;
	DB_LOG *dblp;
{
	LOG *region;
	int ret;
	void *p;

	if ((ret = __db_shalloc(dblp->reginfo.addr,
	    sizeof(*region), 0, &dblp->reginfo.primary)) != 0)
		return (ret);
	dblp->reginfo.rp->primary =
	    R_OFFSET(&dblp->reginfo, dblp->reginfo.primary);
	region = dblp->reginfo.primary;
	memset(region, 0, sizeof(*region));

	region->persist.lg_max = dbenv->lg_max;
	region->persist.magic = DB_LOGMAGIC;
	region->persist.version = DB_LOGVERSION;
	region->persist.mode = dbenv->db_mode;
	SH_TAILQ_INIT(&region->fq);

	/* Initialize LOG LSNs. */
	region->lsn.file = 1;
	region->lsn.offset = 0;

	/* Initialize the buffer. */
	if ((ret =
	    __db_shalloc(dblp->reginfo.addr, dbenv->lg_bsize, 0, &p)) != 0)
		return (ret);
	region->buffer_size = dbenv->lg_bsize;
	region->buffer_off = R_OFFSET(&dblp->reginfo, p);

	/*
	 * XXX:
	 * Initialize the log file size.  This is a hack to push the log's
	 * maximum size down into the Windows __os_open routine, because it
	 * wants to pre-allocate it.
	 */
	dblp->lfh.log_size = dbenv->lg_max;

	/* Try and recover any previous log files before releasing the lock. */
	return (__log_recover(dblp));
}

/*
 * __log_recover --
 *	Recover a log.
 */
static int
__log_recover(dblp)
	DB_LOG *dblp;
{
	DBT dbt;
	DB_LSN lsn;
	LOG *lp;
	u_int32_t chk;
	int cnt, found_checkpoint, ret;

	lp = dblp->reginfo.primary;

	/*
	 * Find a log file.  If none exist, we simply return, leaving
	 * everything initialized to a new log.
	 */
	if ((ret = __log_find(dblp, 0, &cnt)) != 0)
		return (ret);
	if (cnt == 0)
		return (0);

	/*
	 * We have the last useful log file and we've loaded any persistent
	 * information.  Pretend that the log is larger than it can possibly
	 * be, and read the last file, looking for the last checkpoint and
	 * the log's end.
	 */
	lp->lsn.file = cnt + 1;
	lp->lsn.offset = 0;
	lsn.file = cnt;
	lsn.offset = 0;

	/* Set the cursor.  Shouldn't fail, leave error messages on. */
	memset(&dbt, 0, sizeof(dbt));
	if ((ret = __log_get(dblp, &lsn, &dbt, DB_SET, 0)) != 0)
		return (ret);

	/*
	 * Read to the end of the file, saving checkpoints.  This will fail
	 * at some point, so turn off error messages.
	 */
	found_checkpoint = 0;
	while (__log_get(dblp, &lsn, &dbt, DB_NEXT, 1) == 0) {
		if (dbt.size < sizeof(u_int32_t))
			continue;
		memcpy(&chk, dbt.data, sizeof(u_int32_t));
		if (chk == DB_txn_ckp) {
			lp->chkpt_lsn = lsn;
			found_checkpoint = 1;
		}
	}

	/*
	 * We now know where the end of the log is.  Set the first LSN that
	 * we want to return to an application and the LSN of the last known
	 * record on disk.
	 */
	lp->lsn = lp->s_lsn = lsn;
	lp->lsn.offset += dblp->c_len;

	/* Set up the current buffer information, too. */
	lp->len = dblp->c_len;
	lp->b_off = 0;
	lp->w_off = lp->lsn.offset;

	/*
	 * It's possible that we didn't find a checkpoint because there wasn't
	 * one in the last log file.  Start searching.
	 */
	while (!found_checkpoint && cnt > 1) {
		lsn.file = --cnt;
		lsn.offset = 0;

		/* Set the cursor.  Shouldn't fail, leave error messages on. */
		if ((ret = __log_get(dblp, &lsn, &dbt, DB_SET, 0)) != 0)
			return (ret);

		/*
		 * Read to the end of the file, saving checkpoints.  Again,
		 * this can fail if there are no checkpoints in any log file,
		 * so turn error messages off.
		 */
		while (__log_get(dblp, &lsn, &dbt, DB_NEXT, 1) == 0) {
			if (dbt.size < sizeof(u_int32_t))
				continue;
			memcpy(&chk, dbt.data, sizeof(u_int32_t));
			if (chk == DB_txn_ckp) {
				lp->chkpt_lsn = lsn;
				found_checkpoint = 1;
			}
		}
	}

	/* If we never find a checkpoint, that's okay, just 0 it out. */
	if (!found_checkpoint)
		ZERO_LSN(lp->chkpt_lsn);

	/*
	 * Reset the cursor lsn to the beginning of the log, so that an
	 * initial call to DB_NEXT does the right thing.
	 */
	ZERO_LSN(dblp->c_lsn);

	if (FLD_ISSET(dblp->dbenv->verbose, DB_VERB_RECOVERY))
		__db_err(dblp->dbenv,
		    "Finding last valid log LSN: file: %lu offset %lu",
		    (u_long)lp->lsn.file, (u_long)lp->lsn.offset);

	return (0);
}

/*
 * __log_find --
 *	Try to find a log file.  If find_first is set, valp will contain
 * the number of the first log file, else it will contain the number of
 * the last log file.
 *
 * PUBLIC: int __log_find __P((DB_LOG *, int, int *));
 */
int
__log_find(dblp, find_first, valp)
	DB_LOG *dblp;
	int find_first, *valp;
{
	u_int32_t clv, logval;
	int cnt, fcnt, ret;
	const char *dir;
	char **names, *p, *q;

	*valp = 0;

	/* Find the directory name. */
	if ((ret = __log_name(dblp, 1, &p, NULL, 0)) != 0)
		return (ret);
	if ((q = __db_rpath(p)) == NULL)
		dir = PATH_DOT;
	else {
		*q = '\0';
		dir = p;
	}

	/* Get the list of file names. */
	ret = __os_dirlist(dir, &names, &fcnt);

	/*
	 * !!!
	 * We overwrote a byte in the string with a nul.  We have to restore
	 * the string so that the diagnostic checks in the memory allocation
	 * code work.
	 */
	if (q != NULL)
		*q = 'a';
	__os_freestr(p);

	if (ret != 0) {
		__db_err(dblp->dbenv, "%s: %s", dir, db_strerror(ret));
		return (ret);
	}

	/*
	 * Search for a valid log file name, return a value of 0 on
	 * failure.
	 *
	 * XXX
	 * Assumes that atoi(3) returns a 32-bit number.
	 */
	for (cnt = fcnt, clv = logval = 0; --cnt >= 0;) {
		if (strncmp(names[cnt], LFPREFIX, sizeof(LFPREFIX) - 1) != 0)
			continue;

		clv = atoi(names[cnt] + (sizeof(LFPREFIX) - 1));
		if (find_first) {
			if (logval != 0 && clv > logval)
				continue;
		} else
			if (logval != 0 && clv < logval)
				continue;

		if (__log_valid(dblp, clv, 1) == 0)
			logval = clv;
	}

	*valp = logval;

	/* Discard the list. */
	__os_dirfree(names, fcnt);

	return (0);
}

/*
 * log_valid --
 *	Validate a log file.
 *
 * PUBLIC: int __log_valid __P((DB_LOG *, u_int32_t, int));
 */
int
__log_valid(dblp, number, set_persist)
	DB_LOG *dblp;
	u_int32_t number;
	int set_persist;
{
	DB_FH fh;
	LOG *region;
	LOGP persist;
	ssize_t nw;
	int ret;
	char *fname;

	/* Try to open the log file. */
	if ((ret = __log_name(dblp,
	    number, &fname, &fh, DB_OSO_RDONLY | DB_OSO_SEQ)) != 0) {
		__os_freestr(fname);
		return (ret);
	}

	/* Try to read the header. */
	if ((ret = __os_seek(&fh, 0, 0, sizeof(HDR), 0, DB_OS_SEEK_SET)) != 0 ||
	    (ret = __os_read(&fh, &persist, sizeof(LOGP), &nw)) != 0 ||
	    nw != sizeof(LOGP)) {
		if (ret == 0)
			ret = EIO;

		(void)__os_closehandle(&fh);

		__db_err(dblp->dbenv,
		    "Ignoring log file: %s: %s", fname, db_strerror(ret));
		goto err;
	}
	(void)__os_closehandle(&fh);

	/* Validate the header. */
	if (persist.magic != DB_LOGMAGIC) {
		__db_err(dblp->dbenv,
		    "Ignoring log file: %s: magic number %lx, not %lx",
		    fname, (u_long)persist.magic, (u_long)DB_LOGMAGIC);
		ret = EINVAL;
		goto err;
	}
	if (persist.version < DB_LOGOLDVER || persist.version > DB_LOGVERSION) {
		__db_err(dblp->dbenv,
		    "Ignoring log file: %s: unsupported log version %lu",
		    fname, (u_long)persist.version);
		ret = EINVAL;
		goto err;
	}

	/*
	 * If we're going to use this log file, set the region's persistent
	 * information based on the headers.
	 */
	if (set_persist) {
		region = dblp->reginfo.primary;
		region->persist.lg_max = persist.lg_max;
		region->persist.mode = persist.mode;
	}
	ret = 0;

err:	__os_freestr(fname);
	return (ret);
}

/*
 * __log_close --
 *	Internal version of log_close: only called from db_appinit.
 *
 * PUBLIC: int __log_close __P((DB_ENV *));
 */
int
__log_close(dbenv)
	DB_ENV *dbenv;
{
	DB_LOG *dblp;
	int ret, t_ret;

	ret = 0;
	dblp = dbenv->lg_handle;

	/* We may have opened files as part of XA; if so, close them. */
	__log_close_files(dbenv);

	/* Discard the per-thread lock. */
	if (dblp->mutexp != NULL)
		__db_mutex_free(dbenv, &dblp->reginfo, dblp->mutexp);

	/* Detach from the region. */
	ret = __db_r_detach(dbenv, &dblp->reginfo, 0);

	/* Close open files, release allocated memory. */
	if (F_ISSET(&dblp->lfh, DB_FH_VALID) &&
	    (t_ret = __os_closehandle(&dblp->lfh)) != 0 && ret == 0)
		ret = t_ret;
	if (dblp->c_dbt.data != NULL)
		__os_free(dblp->c_dbt.data, dblp->c_dbt.ulen);
	if (F_ISSET(&dblp->c_fh, DB_FH_VALID) &&
	    (t_ret = __os_closehandle(&dblp->c_fh)) != 0 && ret == 0)
		ret = t_ret;
	if (dblp->dbentry != NULL)
		__os_free(dblp->dbentry,
		    (dblp->dbentry_cnt * sizeof(DB_ENTRY)));

	__os_free(dblp, sizeof(*dblp));
	return (ret);
}

/*
 * log_stat --
 *	Return LOG statistics.
 */
int
log_stat(dbenv, statp, db_malloc)
	DB_ENV *dbenv;
	DB_LOG_STAT **statp;
	void *(*db_malloc) __P((size_t));
{
	DB_LOG *dblp;
	DB_LOG_STAT *stats;
	LOG *region;
	int ret;

	PANIC_CHECK(dbenv);
	ENV_REQUIRES_CONFIG(dbenv, dbenv->lg_handle, DB_INIT_LOG);

	*statp = NULL;

	dblp = dbenv->lg_handle;
	region = dblp->reginfo.primary;

	if ((ret = __os_malloc(sizeof(DB_LOG_STAT), db_malloc, &stats)) != 0)
		return (ret);

	/* Copy out the global statistics. */
	R_LOCK(dbenv, &dblp->reginfo);
	*stats = region->stat;

	stats->st_magic = region->persist.magic;
	stats->st_version = region->persist.version;
	stats->st_mode = region->persist.mode;
	stats->st_lg_bsize = region->buffer_size;
	stats->st_lg_max = region->persist.lg_max;

	stats->st_region_wait = dblp->reginfo.rp->mutex.mutex_set_wait;
	stats->st_region_nowait = dblp->reginfo.rp->mutex.mutex_set_nowait;
	stats->st_regsize = dblp->reginfo.rp->size;

	stats->st_cur_file = region->lsn.file;
	stats->st_cur_offset = region->lsn.offset;

	R_UNLOCK(dbenv, &dblp->reginfo);

	*statp = stats;
	return (0);
}
