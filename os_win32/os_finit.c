/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1999
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)os_finit.c	11.4 (Sleepycat) 9/22/99";
#endif /* not lint */

#include "db_int.h"

/*
 * __os_fpinit --
 *	Initialize a page in a regular file.
 *
 * PUBLIC: int __os_fpinit __P((DB_FH *, db_pgno_t, int, int));
 */
int
__os_fpinit(fhp, pgno, pagecount, pagesize)
	DB_FH *fhp;
	db_pgno_t pgno;
	int pagecount, pagesize;
{
	ssize_t nw;
	int ret;
	char buf[DB_MAX_PGSIZE];

	/*
	 * Windows/NT zero-fills pages that were never explicitly written to
	 * the file.  Windows 95/98 gives you random garbage, and that breaks
	 * DB.
	 */
	if (__os_is_winnt())
		return (0);

	if ((ret = __os_seek(fhp, pagesize, pgno, 0, 0, DB_OS_SEEK_SET)) != 0)
		return (ret);

	memset(buf, 0, sizeof(buf));

	while (pagecount--) {
		if ((ret = __os_write(fhp, buf, pagesize, &nw)) != 0)
			return (ret);
		if (nw != pagesize)
			return (EIO);
	}
	return (0);
}
