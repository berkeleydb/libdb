/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)lock_region.c	11.9 (Sleepycat) 11/10/99";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <ctype.h>
#include <errno.h>
#include <string.h>
#endif

#include "db_int.h"
#include "db_shash.h"
#include "lock.h"

static void __lock_dump_locker __P((DB_LOCKTAB *, DB_LOCKER *, FILE *));
static void __lock_dump_object __P((DB_LOCKTAB *, DB_LOCKOBJ *, FILE *));
static const char *
	    __lock_dump_status __P((db_status_t));
static int  __lock_init __P((DB_ENV *, DB_LOCKTAB *));
static size_t
	    __lock_region_size __P((DB_ENV *));
static int  __lock_set_lk_conflicts __P((DB_ENV *, u_int8_t *, int));
static int  __lock_set_lk_detect __P((DB_ENV *, u_int32_t));
static int  __lock_set_lk_max __P((DB_ENV *, u_int32_t));

/*
 * This conflict array is used for concurrent db access (CDB).  It
 * uses the same locks as the db_rw_conflict array, but adds an IW
 * mode to be used for write cursors.
 */
static u_int8_t const db_cdb_conflicts[] = {
	/*		N   R   W  IW */
	/*    N */	0,  0,  0,  0,
	/*    R */	0,  0,  1,  0,
	/*    W */	0,  1,  1,  1,
	/*   IW */	0,  0,  1,  1
};

/*
 * __lock_dbenv_create --
 *	Lock specific creation of the DB_ENV structure.
 *
 * PUBLIC: void __lock_dbenv_create __P((DB_ENV *));
 */
void
__lock_dbenv_create(dbenv)
	DB_ENV *dbenv;
{
	dbenv->lk_max = DB_LOCK_DEFAULT_N;

	dbenv->set_lk_conflicts = __lock_set_lk_conflicts;
	dbenv->set_lk_detect = __lock_set_lk_detect;
	dbenv->set_lk_max = __lock_set_lk_max;
}

/*
 * __lock_dbenv_close --
 *	Lock specific destruction of the DB_ENV structure.
 *
 * PUBLIC: void __lock_dbenv_close __P((DB_ENV *));
 */
void
__lock_dbenv_close(dbenv)
	DB_ENV *dbenv;
{
	if (!F_ISSET(dbenv, DB_ENV_USER_ALLOC) && dbenv->lk_conflicts != NULL) {
		__os_free(dbenv->lk_conflicts,
		    dbenv->lk_modes * dbenv->lk_modes);
		dbenv->lk_conflicts = NULL;
	}
}

/*
 * __lock_set_lk_conflicts
 *	Set the conflicts matrix.
 */
static int
__lock_set_lk_conflicts(dbenv, lk_conflicts, lk_modes)
	DB_ENV *dbenv;
	u_int8_t *lk_conflicts;
	int lk_modes;
{
	int ret;

	ENV_ILLEGAL_AFTER_OPEN(dbenv, "set_lk_conflicts");

	if (dbenv->lk_conflicts != NULL) {
		__os_free(dbenv->lk_conflicts,
		    dbenv->lk_modes * dbenv->lk_modes);
		dbenv->lk_conflicts = NULL;
	}
	if ((ret =
	    __os_malloc(lk_modes * lk_modes, NULL, &dbenv->lk_conflicts)) != 0)
		return (ret);
	memcpy(dbenv->lk_conflicts, lk_conflicts, lk_modes * lk_modes);
	dbenv->lk_modes = lk_modes;

	return (0);
}

/*
 * __lock_set_lk_detect
 *	Set the automatic deadlock detection.
 */
static int
__lock_set_lk_detect(dbenv, lk_detect)
	DB_ENV *dbenv;
	u_int32_t lk_detect;
{
	ENV_ILLEGAL_AFTER_OPEN(dbenv, "set_lk_detect");

	switch (lk_detect) {
	case DB_LOCK_DEFAULT:
	case DB_LOCK_OLDEST:
	case DB_LOCK_RANDOM:
	case DB_LOCK_YOUNGEST:
		break;
	default:
		return (EINVAL);
	}
	dbenv->lk_detect = lk_detect;
	return (0);
}

/*
 * __lock_set_lk_max
 *	Set the lock table size.
 */
static int
__lock_set_lk_max(dbenv, lk_max)
	DB_ENV *dbenv;
	u_int32_t lk_max;
{
	ENV_ILLEGAL_AFTER_OPEN(dbenv, "set_lk_max");

	dbenv->lk_max = lk_max;
	return (0);
}

/*
 * __lock_open --
 *	Internal version of lock_open: only called from DB_ENV->open.
 *
 * PUBLIC: int __lock_open __P((DB_ENV *));
 */
int
__lock_open(dbenv)
	DB_ENV *dbenv;
{
	DB_LOCKREGION *region;
	DB_LOCKTAB *lt;
	size_t size;
	int ret;

	/* Create the lock table structure. */
	if ((ret = __os_calloc(1, sizeof(DB_LOCKTAB), &lt)) != 0)
		return (ret);
	lt->dbenv = dbenv;

	/* Join/create the lock region. */
	lt->reginfo.id = REG_ID_LOCK;
	lt->reginfo.mode = dbenv->db_mode;
	if (F_ISSET(dbenv, DB_ENV_CREATE))
		F_SET(&lt->reginfo, REGION_CREATE_OK);
	size = __lock_region_size(dbenv);
	if ((ret = __db_r_attach(dbenv, &lt->reginfo, size)) != 0)
		goto err;

	/* If we created the region, initialize it. */
	if (F_ISSET(&lt->reginfo, REGION_CREATE))
		if ((ret = __lock_init(dbenv, lt)) != 0)
			goto err;

	/* Set the local addresses. */
	region = lt->reginfo.primary =
	    R_ADDR(&lt->reginfo, lt->reginfo.rp->primary);

	/* Check for incompatible automatic deadlock detection requests. */
	if (dbenv->lk_detect != DB_LOCK_NORUN) {
		if (region->detect != DB_LOCK_NORUN &&
		    dbenv->lk_detect != DB_LOCK_DEFAULT &&
		    region->detect != dbenv->lk_detect) {
			__db_err(dbenv,
		    "lock_open: incompatible deadlock detector mode");
			ret = EINVAL;
			goto err;
		}

		/*
		 * Upgrade if our caller wants automatic detection, and it
		 * was not currently being done, whether or not we created
		 * the region.
		 */
		if (region->detect == DB_LOCK_NORUN)
			region->detect = dbenv->lk_detect;
	}

	/* Set remaining pointers into region. */
	lt->memlock = (MUTEX *)R_ADDR(&lt->reginfo, region->memlock_off);
	lt->conflicts = (u_int8_t *)R_ADDR(&lt->reginfo, region->conf_off);
	lt->obj_tab = (DB_HASHTAB *)R_ADDR(&lt->reginfo, region->obj_off);
	lt->osynch_tab = (MUTEX *)R_ADDR(&lt->reginfo, region->osynch_off);
	lt->locker_tab = (DB_HASHTAB *)R_ADDR(&lt->reginfo, region->locker_off);
	lt->lsynch_tab = (MUTEX *)R_ADDR(&lt->reginfo, region->lsynch_off);

	R_UNLOCK(dbenv, &lt->reginfo);

	dbenv->lk_handle = lt;
	return (0);

err:	if (lt->reginfo.addr != NULL) {
		if (F_ISSET(&lt->reginfo, REGION_CREATE))
			F_SET(lt->reginfo.rp, REG_DEAD);
		R_UNLOCK(dbenv, &lt->reginfo);
		(void)__db_r_detach(dbenv, &lt->reginfo, 0);
	}
	__os_free(lt, sizeof(*lt));
	return (ret);
}

/*
 * __lock_init --
 *	Initialize the lock region.
 */
static int
__lock_init(dbenv, lt)
	DB_ENV *dbenv;
	DB_LOCKTAB *lt;
{
	const u_int8_t *lk_conflicts;
	struct __db_lock *lp;
	DB_LOCKER *lidp;
	DB_LOCKOBJ *op;
	DB_LOCKREGION *region;
#ifdef FINE_GRAIN
	MUTEX *m;
#endif
	u_int32_t i, lk_modes, nelements;
	u_int8_t *addr;
	int ret;

	if ((ret = __db_shalloc(lt->reginfo.addr,
	    sizeof(DB_LOCKREGION), 0, &lt->reginfo.primary)) != 0)
		return (ret);
	lt->reginfo.rp->primary = R_OFFSET(&lt->reginfo, lt->reginfo.primary);
	region = lt->reginfo.primary;
	memset(region, 0, sizeof(*region));

	/* Select a conflict matrix if none specified. */
	if (dbenv->lk_modes == 0)
		if (F_ISSET(dbenv, DB_ENV_CDB)) {
			lk_modes = DB_LOCK_RW_N + 1;
			lk_conflicts = db_cdb_conflicts;
		} else {
			lk_modes = DB_LOCK_RW_N;
			lk_conflicts = db_rw_conflicts;
		}
	else {
		lk_modes = dbenv->lk_modes;
		lk_conflicts = dbenv->lk_conflicts;
	}

	region->id = 0;
	region->need_dd = 0;
	region->detect = DB_LOCK_NORUN;
	region->maxlocks = dbenv->lk_max;
	region->table_size = __db_tablesize(dbenv->lk_max);
	region->nmodes = lk_modes;
	region->nlockers = 0;
	region->maxnlockers = 0;
	region->nconflicts = 0;
	region->nrequests = 0;
	region->nreleases = 0;
	region->ndeadlocks = 0;

	nelements = region->table_size;

	/* Allocate room for the conflict matrix and initialize it. */
	if ((ret =
	    __db_shalloc(lt->reginfo.addr, lk_modes * lk_modes, 0, &addr)) != 0)
		return (ret);
	memcpy(addr, lk_conflicts, lk_modes * lk_modes);
	region->conf_off = R_OFFSET(&lt->reginfo, addr);

	/* Allocate room for the memory mutex and initialize it. */
#ifdef FINE_GRAIN
	if ((ret = __db_shalloc(lt->reginfo.addr,
	    sizeof(MUTEX), MUTEX_ALIGN, &addr)) != 0)
		return (ret);
	region->memlock_off = R_OFFSET(&lt->reginfo, addr);
	if ((ret = __db_mutex_init(dbenv, (MUTEX *)addr,
	    R_OFFSET(&lt->reginfo, addr) + DB_FCNTL_OFF_LOCK, 0)) != 0)
		return (ret);

	/* Allocate room for the mutex tables and initialize them. */
	if ((ret = __db_shalloc(lt->reginfo.addr,
	    nelements * sizeof(MUTEX), MUTEX_ALIGN, &addr)) != 0)
		return (ret);
	region->osynch_off = R_OFFSET(&lt->reginfo, addr);
	for (i = 0; i < nelements; i++) {
		m = (MUTEX *)addr + i;
		if ((ret = __db_mutex_init(dbenv, m,
		    R_OFFSET(&lt->reginfo, m) + DB_FCNTL_OFF_LOCK, 0)) != 0)
			return (ret);
	}
	if ((ret = __db_shalloc(lt->reginfo.addr,
	    nelements * sizeof(MUTEX), MUTEX_ALIGN, &addr)) != 0)
		return (ret);
	region->lsynch_off = R_OFFSET(&lt->reginfo, addr);
	for (i = 0; i < nelements; i++) {
		m = (MUTEX *)addr + i;
		if ((ret = __db_mutex_init(dbenv, m,
		    R_OFFSET(&lt->reginfo, m) + DB_FCNTL_OFF_LOCK, 0)) != 0)
			return (ret);
	}
#endif
	/* Allocate room for the object hash table and initialize it. */
	if ((ret = __db_shalloc(lt->reginfo.addr,
	    nelements * sizeof(DB_HASHTAB), 0, &addr)) != 0)
		return (ret);
	__db_hashinit(addr, nelements);
	region->obj_off = R_OFFSET(&lt->reginfo, addr);

	/* Allocate room for the locker hash table and initialize it. */
	if ((ret = __db_shalloc(lt->reginfo.addr,
	    nelements * sizeof(DB_HASHTAB), 0, &addr)) != 0)
		return (ret);
	__db_hashinit(addr, nelements);
	region->locker_off = R_OFFSET(&lt->reginfo, addr);

	/*
	 * Initialize locks onto a free list. Initialize and lock the mutex
	 * so that when we need to block, all we need do is try to acquire
	 * the mutex.
	 */
	SH_TAILQ_INIT(&region->free_locks);
	for (i = 0; i < region->maxlocks; ++i) {
		if ((ret = __db_shalloc(lt->reginfo.addr,
		    sizeof(struct __db_lock), MUTEX_ALIGN, &lp)) != 0)
			return (ret);
		lp->status = DB_LSTAT_FREE;
		if ((ret = __db_mutex_init(dbenv, &lp->mutex,
		    R_OFFSET(&lt->reginfo, &lp->mutex) + DB_FCNTL_OFF_LOCK,
		    MUTEX_SELF_BLOCK)) != 0)
			return (ret);
		MUTEX_LOCK(&lp->mutex, lt->dbenv->lockfhp);
		SH_TAILQ_INSERT_HEAD(&region->free_locks, lp, links, __db_lock);
	}

	/* Initialize objects onto a free list.  */
	SH_TAILQ_INIT(&region->free_objs);
	for (i = 0; i < region->maxlocks; ++i) {
		if ((ret = __db_shalloc(lt->reginfo.addr,
		    sizeof(DB_LOCKOBJ), 0, &op)) != 0)
			return (ret);
		SH_TAILQ_INSERT_HEAD(
		    &region->free_objs, op, links, __db_lockobj);
	}

	/* Initialize lockers onto a free list.  */
	SH_TAILQ_INIT(&region->free_lockers);
	for (i = 0; i < region->maxlocks; ++i) {
		if ((ret = __db_shalloc(lt->reginfo.addr,
		    sizeof(DB_LOCKER), 0, &lidp)) != 0)
			return (ret);
		SH_TAILQ_INSERT_HEAD(
		    &region->free_lockers, lidp, links, __db_locker);
	}

	return (0);
}

/*
 * __lock_close --
 *	Internal version of lock_close: only called from db_appinit.
 *
 * PUBLIC: int __lock_close __P((DB_ENV *));
 */
int
__lock_close(dbenv)
	DB_ENV *dbenv;
{
	DB_LOCKTAB *lt;
	int ret;

	lt = dbenv->lk_handle;

	/* Detach from the region. */
	ret = __db_r_detach(dbenv, &lt->reginfo, 0);

	__os_free(lt, sizeof(*lt));
	return (ret);
}

/*
 * lock_stat --
 *	Return LOCK statistics.
 */
int
lock_stat(dbenv, statp, db_malloc)
	DB_ENV *dbenv;
	DB_LOCK_STAT **statp;
	void *(*db_malloc) __P((size_t));
{
	DB_LOCKREGION *region;
	DB_LOCKTAB *lt;
	DB_LOCK_STAT *stats;
	int ret;

	PANIC_CHECK(dbenv);
	ENV_REQUIRES_CONFIG(dbenv, dbenv->lk_handle, DB_INIT_LOCK);

	*statp = NULL;

	lt = dbenv->lk_handle;

	if ((ret = __os_malloc(sizeof(*stats), db_malloc, &stats)) != 0)
		return (ret);

	/* Copy out the global statistics. */
	R_LOCK(dbenv, &lt->reginfo);

	region = lt->reginfo.primary;
	stats->st_lastid = region->id;
	stats->st_maxlocks = region->maxlocks;
	stats->st_nmodes = region->nmodes;
	stats->st_nlockers = region->nlockers;
	stats->st_maxnlockers = region->maxnlockers;
	stats->st_nconflicts = region->nconflicts;
	stats->st_nrequests = region->nrequests;
	stats->st_nreleases = region->nreleases;
	stats->st_ndeadlocks = region->ndeadlocks;

	stats->st_region_wait = lt->reginfo.rp->mutex.mutex_set_wait;
	stats->st_region_nowait = lt->reginfo.rp->mutex.mutex_set_nowait;
	stats->st_regsize = lt->reginfo.rp->size;

	R_UNLOCK(dbenv, &lt->reginfo);

	*statp = stats;
	return (0);
}

#define	LOCK_DUMP_CONF		0x001		/* Conflict matrix. */
#define	LOCK_DUMP_FREE		0x002		/* Display lock free list. */
#define	LOCK_DUMP_LOCKERS	0x004		/* Display lockers. */
#define	LOCK_DUMP_MEM		0x008		/* Display region memory. */
#define	LOCK_DUMP_OBJECTS	0x010		/* Display objects. */
#define	LOCK_DUMP_ALL		0x01f		/* Display all. */

/*
 * __lock_dump_region --
 *
 * PUBLIC: void __lock_dump_region __P((DB_ENV *, char *, FILE *));
 */
void
__lock_dump_region(dbenv, area, fp)
	DB_ENV *dbenv;
	char *area;
	FILE *fp;
{
	struct __db_lock *lp;
	DB_LOCKER *lip;
	DB_LOCKOBJ *op;
	DB_LOCKREGION *lrp;
	DB_LOCKTAB *lt;
	u_int32_t flags, i, j;
	int label;

	/* Make it easy to call from the debugger. */
	if (fp == NULL)
		fp = stderr;

	for (flags = 0; *area != '\0'; ++area)
		switch (*area) {
		case 'A':
			LF_SET(LOCK_DUMP_ALL);
			break;
		case 'c':
			LF_SET(LOCK_DUMP_CONF);
			break;
		case 'f':
			LF_SET(LOCK_DUMP_FREE);
			break;
		case 'l':
			LF_SET(LOCK_DUMP_LOCKERS);
			break;
		case 'm':
			LF_SET(LOCK_DUMP_MEM);
			break;
		case 'o':
			LF_SET(LOCK_DUMP_OBJECTS);
			break;
		}

	lt = dbenv->lk_handle;
	lrp = lt->reginfo.primary;
	LOCKREGION(dbenv, lt);

	fprintf(fp, "%s\nLock region parameters\n", DB_LINE);
	fprintf(fp,
	    "%s: %lu, %s: %lu, %s: %lu, %s: %lu, %s: %lu, %s: %lu, %s: %lu\n",
	    "table size", (u_long)lrp->table_size,
	    "obj_off", (u_long)lrp->obj_off,
	    "osynch_off", (u_long)lrp->osynch_off,
	    "locker_off", (u_long)lrp->locker_off,
	    "lsynch_off", (u_long)lrp->lsynch_off,
	    "memlock_off", (u_long)lrp->memlock_off,
	    "need_dd", (u_long)lrp->need_dd);

	if (LF_ISSET(LOCK_DUMP_CONF)) {
		fprintf(fp, "\n%s\nConflict matrix\n", DB_LINE);
		for (i = 0; i < lrp->nmodes; i++) {
			for (j = 0; j < lrp->nmodes; j++)
				fprintf(fp, "%lu\t",
				    (u_long)lt->conflicts[i * lrp->nmodes + j]);
			fprintf(fp, "\n");
		}
	}

	if (LF_ISSET(LOCK_DUMP_LOCKERS)) {
		fprintf(fp, "%s\nLocker hash buckets\n", DB_LINE);
		for (i = 0; i < lrp->table_size; i++) {
			label = 1;
			LOCKER_LOCK_NDX(lt, i);
			for (lip =
			    SH_TAILQ_FIRST(&lt->locker_tab[i], __db_locker);
			    lip != NULL;
			    lip = SH_TAILQ_NEXT(lip, links, __db_locker)) {
				if (label) {
					fprintf(fp, "Bucket %lu:\n", (u_long)i);
					label = 0;
				}
				__lock_dump_locker(lt, lip, fp);
			}
			LOCKER_UNLOCK(lt, i);
		}
	}

	if (LF_ISSET(LOCK_DUMP_OBJECTS)) {
		fprintf(fp, "%s\nObject hash buckets\n", DB_LINE);
		for (i = 0; i < lrp->table_size; i++) {
			label = 1;
			OBJECT_LOCK_NDX(lt, i);
			for (op = SH_TAILQ_FIRST(&lt->obj_tab[i], __db_lockobj);
			    op != NULL;
			    op = SH_TAILQ_NEXT(op, links, __db_lockobj)) {
				if (label) {
					fprintf(fp, "Bucket %lu:\n", (u_long)i);
					label = 0;
				}
				__lock_dump_object(lt, op, fp);
			}
			OBJECT_UNLOCK(lt, i);
		}
	}

	if (LF_ISSET(LOCK_DUMP_FREE)) {
		fprintf(fp, "%s\nLock free list\n", DB_LINE);
		for (lp = SH_TAILQ_FIRST(&lrp->free_locks, __db_lock);
		    lp != NULL;
		    lp = SH_TAILQ_NEXT(lp, links, __db_lock))
			fprintf(fp, "0x%lx: %lu\t%lu\t%s\t0x%lx\n", (u_long)lp,
			    (u_long)lp->holder, (u_long)lp->mode,
			    __lock_dump_status(lp->status), (u_long)lp->obj);

		fprintf(fp, "%s\nObject free list\n", DB_LINE);
		for (op = SH_TAILQ_FIRST(&lrp->free_objs, __db_lockobj);
		    op != NULL;
		    op = SH_TAILQ_NEXT(op, links, __db_lockobj))
			fprintf(fp, "0x%lx\n", (u_long)op);

		fprintf(fp, "%s\nLocker free list\n", DB_LINE);
		for (lip = SH_TAILQ_FIRST(&lrp->free_lockers, __db_locker);
		    lip != NULL;
		    lip = SH_TAILQ_NEXT(lip, links, __db_locker))
			fprintf(fp, "0x%lx\n", (u_long)lip);
	}

	if (LF_ISSET(LOCK_DUMP_MEM))
		__db_shalloc_dump(lt->reginfo.addr, fp);

	UNLOCKREGION(dbenv, lt);
}

static void
__lock_dump_locker(lt, lip, fp)
	DB_LOCKTAB *lt;
	DB_LOCKER *lip;
	FILE *fp;
{
	struct __db_lock *lp;

	fprintf(fp, "L %lx [%ld]", (u_long)lip->id, (long)lip->dd_id);
	fprintf(fp, " %s ", F_ISSET(lip, DB_LOCKER_DELETED) ? "(D)" : "   ");

	if ((lp = SH_LIST_FIRST(&lip->heldby, __db_lock)) == NULL)
		fprintf(fp, "\n");
	else
		for (; lp != NULL;
		    lp = SH_LIST_NEXT(lp, locker_links, __db_lock))
			__lock_printlock(lt, lp, 1);
}

static void
__lock_dump_object(lt, op, fp)
	DB_LOCKTAB *lt;
	DB_LOCKOBJ *op;
	FILE *fp;
{
	struct __db_lock *lp;
	u_int32_t j;
	u_int8_t *ptr;
	u_int ch;

	ptr = SH_DBT_PTR(&op->lockobj);
	for (j = 0; j < op->lockobj.size; ptr++, j++) {
		ch = *ptr;
		fprintf(fp, isprint(ch) ? "%c" : "\\%o", ch);
	}
	fprintf(fp, "\n");

	fprintf(fp, "H:");
	for (lp =
	    SH_TAILQ_FIRST(&op->holders, __db_lock);
	    lp != NULL;
	    lp = SH_TAILQ_NEXT(lp, links, __db_lock))
		__lock_printlock(lt, lp, 1);
	lp = SH_TAILQ_FIRST(&op->waiters, __db_lock);
	if (lp != NULL) {
		fprintf(fp, "\nW:");
		for (; lp != NULL; lp = SH_TAILQ_NEXT(lp, links, __db_lock))
			__lock_printlock(lt, lp, 1);
	}
}

static const char *
__lock_dump_status(status)
	db_status_t status;
{
	switch (status) {
	case DB_LSTAT_ABORTED:
		return ("aborted");
	case DB_LSTAT_ERR:
		return ("err");
	case DB_LSTAT_FREE:
		return ("free");
	case DB_LSTAT_HELD:
		return ("held");
	case DB_LSTAT_NOGRANT:
		return ("nogrant");
	case DB_LSTAT_PENDING:
		return ("pending");
	case DB_LSTAT_WAITING:
		return ("waiting");
	}
	return ("unknown status");
}

/*
 * __lock_region_size --
 *	Return the region size.
 */
static size_t
__lock_region_size(dbenv)
	DB_ENV *dbenv;
{
	size_t retval;
	u_int32_t i, nelements, nlocks;

	nlocks = dbenv->lk_max;
	nelements = __db_tablesize(dbenv->lk_max);

	/*
	 * Figure out how much space we're going to need.  This list should
	 * map one-to-one with the __db_shalloc calls in __lock_init.
	 */
	retval = 0;
	retval += ALIGN(sizeof(DB_LOCKREGION), 1);
	retval += ALIGN(dbenv->lk_modes * dbenv->lk_modes, 1);
#ifdef FINE_GRAIN
	retval += ALIGN(sizeof(MUTEX), MUTEX_ALIGN);
	retval += ALIGN(nelements * sizeof(MUTEX), MUTEX_ALIGN);
	retval += ALIGN(nelements * sizeof(MUTEX), MUTEX_ALIGN);
#endif
	retval += ALIGN(nelements * sizeof(DB_HASHTAB), 1);
	retval += ALIGN(nelements * sizeof(DB_HASHTAB), 1);
	for (i = 0; i < nlocks; ++i)
		retval += ALIGN(sizeof(struct __db_lock), MUTEX_ALIGN);
	for (i = 0; i < nlocks; ++i)
		retval += ALIGN(sizeof(DB_LOCKOBJ), 1);
	for (i = 0; i < nlocks; ++i)
		retval += ALIGN(sizeof(DB_LOCKER), 1);

	/*
	 * Aproximate the memory allocation overhead.  This breaks the
	 * abstraction a little, but seems better than a WAG.
	 */
	 retval += nlocks * (2 * sizeof(ssize_t) + sizeof(size_t));

	/*
	 * Include 16 bytes of string space per lock.  DB doesn't use it
	 * because we pre-allocate lock space for DBTs in the structure.
	 */
	retval += ALIGN(nlocks * 16, sizeof(size_t));

#ifdef DIAGNOSTIC
        /*
         * The above precise calculation doesn't leave enough space for guard
         * bytes, of which there is one plus whatever alignment wastage for
         * each __db_shalloc.  Give ourselves some breathing room.
         */
        retval += nlocks * 5;
#endif

	/* And we keep getting this wrong, let's be generous. */
	retval += 16 * 1024;

	return (retval);
}
