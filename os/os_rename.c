/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997-2001
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char revid[] = "$Id: os_rename.c,v 11.8 2001/09/07 18:17:49 krinsky Exp $";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <string.h>
#include <unistd.h>
#endif

#include "db_int.h"
#include "os_jump.h"

/*
 * __os_rename --
 *	Rename a file.
 *
 * PUBLIC: int __os_rename __P((DB_ENV *, const char *, const char *));
 */
int
__os_rename(dbenv, old, new)
	DB_ENV *dbenv;
	const char *old, *new;
{
	int ret;

	do {
		ret = __db_jump.j_rename != NULL ?
		    __db_jump.j_rename(old, new) : rename(old, new);
	} while (ret != 0 && (ret = __os_get_errno()) == EINTR);

	if (ret != 0)
		__db_err(dbenv, "rename %s %s: %s", old, new, strerror(ret));
	return (ret);
}
