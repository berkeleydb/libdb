/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1998, 1999, 2000
 *	Sleepycat Software.  All rights reserved.
 *
 * This code is derived from software contributed to Sleepycat Software by
 * Frederick G.M. Roeber of Netscape Communications Corp.
 */

#include "db_config.h"

#ifndef lint
static const char revid[] = "$Id: os_map.c,v 1.4 2000/04/26 19:17:41 sue Exp $";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>
#include <errno.h>
#include <string.h>
#endif

#include "db_int.h"
#include "common_ext.h"

/*
 * DB uses memory-mapped files for two things:
 *	faster access of read-only databases, and
 *	shared memory for process synchronization and locking.
 * The code carefully does not mix the two uses.  The first-case uses are
 * actually written such that memory-mapping isn't really required -- it's
 * merely a convenience -- so we don't have to worry much about it.  In the
 * second case, it's solely used as a shared memory mechanism, so that's
 * all we have to replace.
 *
 * All memory in VxWorks is shared, and a task can allocate memory and keep
 * notes.  So I merely have to allocate memory, remember the "filename" for
 * that memory, and issue small-integer segment IDs which index the list of
 * these shared-memory segments. Subsequent opens are checked against the
 * list of already open segments.
 */
typedef struct {
	void *segment;			/* Segment address. */
	u_int32_t size;			/* Segment size. */
	char *name;			/* Segment name. */
} os_segdata_t;

static os_segdata_t *__os_segdata;	/* Segment table. */
static int __os_segdata_size;		/* Segment table size. */

#define	OS_SEGDATA_STARTING_SIZE 16
#define	OS_SEGDATA_INCREMENT	 16

static int __os_segdata_allocate
	       __P((DB_ENV *, const char *, REGINFO *, REGION *));
static int __os_segdata_find_byname
	       __P((DB_ENV *, const char *, REGINFO *, REGION *));
static int __os_segdata_init __P((DB_ENV *));
static int __os_segdata_new __P((DB_ENV *, int *));
static int __os_segdata_release __P((DB_ENV *, int));

/*
 * __os_r_sysattach --
 *	Create/join a shared memory region.
 *
 * PUBLIC: int __os_r_sysattach __P((DB_ENV *, REGINFO *, REGION *));
 */
int
__os_r_sysattach(dbenv, infop, rp)
	DB_ENV *dbenv;
	REGINFO *infop;
	REGION *rp;
{
	int ret;

	if (__os_segdata == NULL)
		__os_segdata_init(dbenv);

	/* Try to find an already existing segment. */
	DB_BEGIN_SINGLE_THREAD;
	if (__os_segdata_find_byname(dbenv, infop->name, infop, rp) == 0) {
		DB_END_SINGLE_THREAD;
		return (0);
	}

	/*
	 * If we're trying to join the region and failing, assume
	 * that there was a reboot and the region no longer exists.
	 */
	if (!F_ISSET(infop, REGION_CREATE)) {
		__db_err(dbenv, "segment %s does not exist", infop->name);
		DB_END_SINGLE_THREAD;
		return (EAGAIN);
	}

	/* Create a new segment. */
	ret = __os_segdata_allocate(dbenv, infop->name, infop, rp);
	DB_END_SINGLE_THREAD;
	return (ret);
}

/*
 * __os_r_sysdetach --
 *	Detach from a shared region.
 *
 * PUBLIC: int __os_r_sysdetach __P((DB_ENV *, REGINFO *, int));
 */
int
__os_r_sysdetach(dbenv, infop, destroy)
	DB_ENV *dbenv;
	REGINFO *infop;
	int destroy;
{
	REGION *rp;

	rp = infop->rp;
	/*
	 * If just detaching, there is no mapping to discard.
	 * If destroying, remove the region.
	 */
	if (destroy)
		return (__os_segdata_release(dbenv, rp->segid));
	return(0);
}

/*
 * __os_mapfile --
 *	Map in a shared memory file.
 *
 * PUBLIC: int __os_mapfile __P((DB_ENV *,
 * PUBLIC:    char *, DB_FH *, size_t, int, void **));
 */
int
__os_mapfile(dbenv, path, fhp, len, is_rdonly, addrp)
	DB_ENV *dbenv;
	char *path;
	DB_FH *fhp;
	int is_rdonly;
	size_t len;
	void **addrp;
{
	/* We cannot map in regular files in VxWorks. */

	COMPQUIET(path, NULL);
	COMPQUIET(fhp, NULL);
	COMPQUIET(is_rdonly, 0);
	COMPQUIET(len, 0);
	COMPQUIET(addrp, NULL);

	__db_err(dbenv, "architecture doesn't support mapping in files");
	return (__db_eopnotsup(dbenv));
}

/*
 * __os_unmapfile --
 *	Unmap the shared memory file.
 *
 * PUBLIC: int __os_unmapfile __P((DB_ENV *, void *, size_t));
 */
int
__os_unmapfile(dbenv, addr, len)
	DB_ENV *dbenv;
	void *addr;
	size_t len;
{
	/* We cannot map in regular files in VxWorks. */

	COMPQUIET(addr, NULL);
	COMPQUIET(len, 0);

	__db_err(dbenv, "architecture doesn't support mapping in files");
	return (__db_eopnotsup(dbenv));
}

/*
 * __os_segdata_init --
 *	Initializes the library's table of shared memory segments.
 *	Called once on the first time through __os_segdata_new().
 */
static int
__os_segdata_init(dbenv)
	DB_ENV *dbenv;
{
	int ret;

	if (__os_segdata != NULL) {
		__db_err(dbenv, "shared memory segment already exists");
		return (EEXIST);
	}

	/*
	 * The lock init call returns a locked lock
	 */
	DB_BEGIN_SINGLE_THREAD;
	__os_segdata_size = OS_SEGDATA_STARTING_SIZE;
	if ((ret = __os_calloc(dbenv, __os_segdata_size,
	    sizeof(os_segdata_t), &__os_segdata)) != 0) {
		DB_END_SINGLE_THREAD;
		return (ret);
	}
	DB_END_SINGLE_THREAD;

	return (0);
}

/*
 * __os_segdata_destroy --
 *	Destroys the library's table of shared memory segments.  It also
 *	frees all linked data: the segments themselves, and their names.
 *	Currently not called.  This function should be called if the
 *	user creates a function to unload or shutdown.
 *
 * PUBLIC: int __os_segdata_destroy __P((void));
 */
int
__os_segdata_destroy()
{
	os_segdata_t *p;
	int i;

	if (__os_segdata == NULL)
		return (0);

	DB_BEGIN_SINGLE_THREAD;
	for (i = 0; i < __os_segdata_size; i++) {
		p = &__os_segdata[i];
		if (p->name != NULL) {
			__os_freestr(p->name);
			p->name = NULL;
		}
		if (p->segment != NULL) {
			__os_free(p->segment, p->size);
			p->segment = NULL;
		}
		p->size = 0;
	}

	__os_free(__os_segdata, __os_segdata_size * sizeof(os_segdata_t));
	__os_segdata = NULL;
	__os_segdata_size = 0;
	DB_END_SINGLE_THREAD;

	return (0);
}

/*
 * __os_segdata_allocate --
 *	Creates a new segment of the specified size, optionally with the
 *	specified name.
 *
 * Assumes it is called with the SEGDATA lock taken.
 */
static int
__os_segdata_allocate(dbenv, name, infop, rp)
	DB_ENV *dbenv;
	const char *name;
	REGINFO *infop;
	REGION *rp;
{
	os_segdata_t *p;
	int segid, ret;

	if ((ret = __os_segdata_new(dbenv, &segid)) != 0)
		return (ret);

	p = &__os_segdata[segid];
	if ((ret = __os_calloc(dbenv, rp->size, 1, &p->segment)) != 0)
		return (ret);
	if ((ret = __os_strdup(dbenv, name, &p->name)) != 0) {
		__os_free(p->segment, rp->size);
		p->segment = NULL;
		return (ret);
	}
	p->size = rp->size;

	infop->addr = p->segment;
	rp->segid = segid;

	return (0);
}

/*
 * __os_segdata_new --
 *	Finds a new segdata slot.  Does not initialise it, so the fd returned
 *	is only valid until you call this again.
 *
 * Assumes it is called with the SEGDATA lock taken.
 */
static int
__os_segdata_new(dbenv, segidp)
	DB_ENV *dbenv;
	int *segidp;
{
	os_segdata_t *p;
	int i, newsize, ret;

	if (__os_segdata == NULL) {
		__db_err(dbenv, "shared memory segment not initialized");
		return (EAGAIN);
	}

	for (i = 0; i < __os_segdata_size; i++) {
		p = &__os_segdata[i];
		if (p->segment == NULL) {
			*segidp = i;
			return (0);
		}
	}

	/*
	 * No more free slots, expand.
	 */
	newsize = __os_segdata_size + OS_SEGDATA_INCREMENT;
	if ((ret = __os_realloc(dbenv, newsize * sizeof(os_segdata_t),
	    NULL, &__os_segdata)) != 0)
		return (ret);
	memset(&__os_segdata[__os_segdata_size],
	    0, OS_SEGDATA_INCREMENT * sizeof(os_segdata_t));

	*segidp = __os_segdata_size;
	__os_segdata_size = newsize;

	return (0);
}

/*
 * __os_segdata_find_byname --
 *	Finds a segment by its name.
 *
 * Assumes it is called with the SEGDATA lock taken.
 *
 * PUBLIC: __os_segdata_find_byname
 * PUBLIC:     __P((DB_ENV *, const char *, REGINFO *, REGION *));
 */
static int
__os_segdata_find_byname(dbenv, name, infop, rp)
	DB_ENV *dbenv;
	const char *name;
	REGINFO *infop;
	REGION *rp;
{
	os_segdata_t *p;
	int i;

	if (__os_segdata == NULL) {
		__db_err(dbenv, "shared memory segment not initialized");
		return (EAGAIN);
	}

	if (name == NULL) {
		__db_err(dbenv, "no segment name given");
		return (ENOENT);
	}

	for (i = 0; i < __os_segdata_size; i++) {
		p = &__os_segdata[i];
		if (p->name != NULL && strcmp(name, p->name) == 0) {
			infop->addr = p->segment;
			rp->segid = i;
			return (0);
		}
	}
	return (ENOENT);
}

/*
 * __os_segdata_release --
 *	Free a segdata entry.
 */
static int
__os_segdata_release(dbenv, segid)
	DB_ENV *dbenv;
	int segid;
{
	os_segdata_t *p;

	if (__os_segdata == NULL) {
		__db_err(dbenv, "shared memory segment not initialized");
		return (EAGAIN);
	}

	if (segid < 0 || segid >= __os_segdata_size) {
		__db_err(dbenv, "segment id %d out of range", segid);
		return (EINVAL);
	}

	DB_BEGIN_SINGLE_THREAD;
	p = &__os_segdata[segid];
	if (p->name != NULL) {
		__os_freestr(p->name);
		p->name = NULL;
	}
	if (p->segment != NULL) {
		__os_free(p->segment, p->size);
		p->segment = NULL;
	}
	p->size = 0;
	DB_END_SINGLE_THREAD;

	/* Any shrink-table logic could go here */

	return (0);
}
