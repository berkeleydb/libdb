/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)os_unlink.c	11.1 (Sleepycat) 7/25/99";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <unistd.h>
#endif

#include "db_int.h"
#include "os_jump.h"

/*
 * __os_unlink --
 *	Remove a file.
 *
 * PUBLIC: int __os_unlink __P((const char *));
 */
int
__os_unlink(path)
	const char *path;
{
	int ret;

	ret = __db_jump.j_unlink != NULL ?
	    __db_jump.j_unlink(path) : unlink(path);
	return (ret == -1 ? __os_get_errno() : 0);
}
