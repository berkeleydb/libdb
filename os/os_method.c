/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1999
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)os_method.c	11.2 (Sleepycat) 9/28/99";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>
#endif

#include "db_int.h"
#include "os_jump.h"

struct __db_jumptab __db_jump;

static int  __os_set_func_close __P((DB_ENV *, int (*)(int)));
static int  __os_set_func_dirfree __P((DB_ENV *, void (*)(char **, int)));
static int  __os_set_func_dirlist __P((DB_ENV *,
		int (*)(const char *, char ***, int *)));
static int  __os_set_func_exists __P((DB_ENV *,
		int (*)(const char *, int *)));
static int  __os_set_func_free __P((DB_ENV *, void (*)(void *)));
static int  __os_set_func_fsync __P((DB_ENV *, int (*)(int)));
static int  __os_set_func_ioinfo __P((DB_ENV *, int (*)(const char *,
		int, u_int32_t *, u_int32_t *, u_int32_t *)));
static int  __os_set_func_malloc __P((DB_ENV *, void *(*)(size_t)));
static int  __os_set_func_map __P((DB_ENV *,
		int (*)(char *, size_t, int, int, void **)));
static int  __os_set_func_open __P((DB_ENV *,
		int (*)(const char *, int, ...)));
static int  __os_set_func_read __P((DB_ENV *,
	       ssize_t (*)(int, void *, size_t)));
static int  __os_set_func_realloc __P((DB_ENV *, void *(*)(void *, size_t)));
static int  __os_set_func_rename __P((DB_ENV *,
		int (*)(const char *, const char *)));
static int  __os_set_func_seek __P((DB_ENV *,
		int (*)(int, size_t, db_pgno_t, u_int32_t, int, int)));
static int  __os_set_func_sleep __P((DB_ENV *, int (*)(u_long, u_long)));
static int  __os_set_func_unlink __P((DB_ENV *, int (*)(const char *)));
static int  __os_set_func_unmap __P((DB_ENV *, int (*)(void *, size_t)));
static int  __os_set_func_write __P((DB_ENV *,
		ssize_t (*)(int, const void *, size_t)));
static int  __os_set_func_yield __P((DB_ENV *, int (*)(void)));

/*
 * __os_dbenv_create --
 *	Set OS specific methods, i.e., the jump table.
 *
 * PUBLIC: void __os_dbenv_create __P((DB_ENV *));
 */
void
__os_dbenv_create(dbenv)
	DB_ENV *dbenv;
{
	dbenv->set_func_close = __os_set_func_close;
	dbenv->set_func_dirfree = __os_set_func_dirfree;
	dbenv->set_func_dirlist = __os_set_func_dirlist;
	dbenv->set_func_exists = __os_set_func_exists;
	dbenv->set_func_free = __os_set_func_free;
	dbenv->set_func_fsync = __os_set_func_fsync;
	dbenv->set_func_ioinfo = __os_set_func_ioinfo;
	dbenv->set_func_malloc = __os_set_func_malloc;
	dbenv->set_func_map = __os_set_func_map;
	dbenv->set_func_open = __os_set_func_open;
	dbenv->set_func_read = __os_set_func_read;
	dbenv->set_func_realloc = __os_set_func_realloc;
	dbenv->set_func_rename = __os_set_func_rename;
	dbenv->set_func_seek = __os_set_func_seek;
	dbenv->set_func_sleep = __os_set_func_sleep;
	dbenv->set_func_unlink = __os_set_func_unlink;
	dbenv->set_func_unmap = __os_set_func_unmap;
	dbenv->set_func_write = __os_set_func_write;
	dbenv->set_func_yield = __os_set_func_yield;
}

static int
__os_set_func_close(dbenv, func_close)
	DB_ENV *dbenv;
	int (*func_close) __P((int));
{
	ENV_ILLEGAL_AFTER_OPEN(dbenv, "set_func_close");

	__db_jump.j_close = func_close;
	return (0);
}

static int
__os_set_func_dirfree(dbenv, func_dirfree)
	DB_ENV *dbenv;
	void (*func_dirfree) __P((char **, int));
{
	ENV_ILLEGAL_AFTER_OPEN(dbenv, "set_func_dirfree");

	__db_jump.j_dirfree = func_dirfree;
	return (0);
}

static int
__os_set_func_dirlist(dbenv, func_dirlist)
	DB_ENV *dbenv;
	int (*func_dirlist) __P((const char *, char ***, int *));
{
	ENV_ILLEGAL_AFTER_OPEN(dbenv, "set_func_dirlist");

	__db_jump.j_dirlist = func_dirlist;
	return (0);
}

static int
__os_set_func_exists(dbenv, func_exists)
	DB_ENV *dbenv;
	int (*func_exists) __P((const char *, int *));
{
	ENV_ILLEGAL_AFTER_OPEN(dbenv, "set_func_exists");

	__db_jump.j_exists = func_exists;
	return (0);
}

static int
__os_set_func_free(dbenv, func_free)
	DB_ENV *dbenv;
	void (*func_free) __P((void *));
{
	ENV_ILLEGAL_AFTER_OPEN(dbenv, "set_func_free");

	__db_jump.j_free = func_free;
	return (0);
}

static int
__os_set_func_fsync(dbenv, func_fsync)
	DB_ENV *dbenv;
	int (*func_fsync) __P((int));
{
	ENV_ILLEGAL_AFTER_OPEN(dbenv, "set_func_fsync");

	__db_jump.j_fsync = func_fsync;
	return (0);
}

static int
__os_set_func_ioinfo(dbenv, func_ioinfo)
	DB_ENV *dbenv;
	int (*func_ioinfo)
	    __P((const char *, int, u_int32_t *, u_int32_t *, u_int32_t *));
{
	ENV_ILLEGAL_AFTER_OPEN(dbenv, "set_func_ioinfo");

	__db_jump.j_ioinfo = func_ioinfo;
	return (0);
}

static int
__os_set_func_malloc(dbenv, func_malloc)
	DB_ENV *dbenv;
	void *(*func_malloc) __P((size_t));
{
	ENV_ILLEGAL_AFTER_OPEN(dbenv, "set_func_malloc");

	__db_jump.j_malloc = func_malloc;
	return (0);
}

static int
__os_set_func_map(dbenv, func_map)
	DB_ENV *dbenv;
	int (*func_map) __P((char *, size_t, int, int, void **));
{
	ENV_ILLEGAL_AFTER_OPEN(dbenv, "set_func_map");

	__db_jump.j_map = func_map;
	return (0);
}

static int
__os_set_func_open(dbenv, func_open)
	DB_ENV *dbenv;
	int (*func_open) __P((const char *, int, ...));
{
	ENV_ILLEGAL_AFTER_OPEN(dbenv, "set_func_open");

	__db_jump.j_open = func_open;
	return (0);
}

static int
__os_set_func_read(dbenv, func_read)
	DB_ENV *dbenv;
	ssize_t (*func_read) __P((int, void *, size_t));
{
	ENV_ILLEGAL_AFTER_OPEN(dbenv, "set_func_read");

	__db_jump.j_read = func_read;
	return (0);
}

static int
__os_set_func_realloc(dbenv, func_realloc)
	DB_ENV *dbenv;
	void *(*func_realloc) __P((void *, size_t));
{
	ENV_ILLEGAL_AFTER_OPEN(dbenv, "set_func_realloc");

	__db_jump.j_realloc = func_realloc;
	return (0);
}

static int
__os_set_func_rename(dbenv, func_rename)
	DB_ENV *dbenv;
	int (*func_rename) __P((const char *, const char *));
{
	ENV_ILLEGAL_AFTER_OPEN(dbenv, "set_func_rename");

	__db_jump.j_rename = func_rename;
	return (0);
}

static int
__os_set_func_seek(dbenv, func_seek)
	DB_ENV *dbenv;
	int (*func_seek) __P((int, size_t, db_pgno_t, u_int32_t, int, int));
{
	ENV_ILLEGAL_AFTER_OPEN(dbenv, "set_func_seek");

	__db_jump.j_seek = func_seek;
	return (0);
}

static int
__os_set_func_sleep(dbenv, func_sleep)
	DB_ENV *dbenv;
	int (*func_sleep) __P((u_long, u_long));
{
	ENV_ILLEGAL_AFTER_OPEN(dbenv, "set_func_sleep");

	__db_jump.j_sleep = func_sleep;
	return (0);
}

static int
__os_set_func_unlink(dbenv, func_unlink)
	DB_ENV *dbenv;
	int (*func_unlink) __P((const char *));
{
	ENV_ILLEGAL_AFTER_OPEN(dbenv, "set_func_unlink");

	__db_jump.j_unlink = func_unlink;
	return (0);
}

static int
__os_set_func_unmap(dbenv, func_unmap)
	DB_ENV *dbenv;
	int (*func_unmap) __P((void *, size_t));
{
	ENV_ILLEGAL_AFTER_OPEN(dbenv, "set_func_unmap");

	__db_jump.j_unmap = func_unmap;
	return (0);
}

static int
__os_set_func_write(dbenv, func_write)
	DB_ENV *dbenv;
	ssize_t (*func_write) __P((int, const void *, size_t));
{
	ENV_ILLEGAL_AFTER_OPEN(dbenv, "set_func_write");

	__db_jump.j_write = func_write;
	return (0);
}

static int
__os_set_func_yield(dbenv, func_yield)
	DB_ENV *dbenv;
	int (*func_yield) __P((void));
{
	ENV_ILLEGAL_AFTER_OPEN(dbenv, "set_func_yield");

	__db_jump.j_yield = func_yield;
	return (0);
}
