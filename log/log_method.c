/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1999
 *	Sleepycat Software.  All rights reserved.
 */
#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)log_method.c	11.3 (Sleepycat) 8/11/99";
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

static int __log_set_lg_max __P((DB_ENV *, u_int32_t));
static int __log_set_lg_bsize __P((DB_ENV *, u_int32_t));

/*
 * __log_dbenv_create --
 *	Log specific initialization of the DB_ENV structure.
 *
 * PUBLIC: void __log_dbenv_create __P((DB_ENV *));
 */
void
__log_dbenv_create(dbenv)
	DB_ENV *dbenv;
{
	dbenv->lg_bsize = LG_BSIZE_DEFAULT;
	dbenv->set_lg_bsize = __log_set_lg_bsize;

	dbenv->lg_max = LG_MAX_DEFAULT;
	dbenv->set_lg_max = __log_set_lg_max;
}

/*
 * __log_set_lg_bsize --
 *	Set the log buffer size.
 */
static int
__log_set_lg_bsize(dbenv, lg_bsize)
	DB_ENV *dbenv;
	u_int32_t lg_bsize;
{
	ENV_ILLEGAL_AFTER_OPEN(dbenv, "set_lg_bsize");

					/* Let's not be silly. */
	if (lg_bsize > dbenv->lg_max / 4) {
		__db_err(dbenv, "log buffer size must be <= log file size / 4");
		return (EINVAL);
	}

	dbenv->lg_bsize = lg_bsize;
	return (0);
}

/*
 * __log_set_lg_max --
 *	Set the maximum log file size.
 */
static int
__log_set_lg_max(dbenv, lg_max)
	DB_ENV *dbenv;
	u_int32_t lg_max;
{
	ENV_ILLEGAL_AFTER_OPEN(dbenv, "set_lg_max");

					/* Let's not be silly. */
	if (lg_max < dbenv->lg_bsize * 4) {
		__db_err(dbenv, "log file size must be >= log buffer size * 4");
		return (EINVAL);
	}

	dbenv->lg_max = lg_max;
	return (0);
}
