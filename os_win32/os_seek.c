/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)os_seek.c	11.2 (Sleepycat) 10/29/99";
#endif /* not lint */

#include "db_int.h"
#include "os_jump.h"

/*
 * __os_seek --
 *	Seek to a page/byte offset in the file.
 */
int
__os_seek(fhp, pgsize, pageno, relative, isrewind, db_whence)
	DB_FH *fhp;
	size_t pgsize;
	db_pgno_t pageno;
	u_int32_t relative;
	int isrewind;
	DB_OS_SEEK db_whence;
{
	__int64 offset;
	int whence;

	switch (db_whence) {
	case DB_OS_SEEK_CUR:
		whence = SEEK_CUR;
		break;
	case DB_OS_SEEK_END:
		whence = SEEK_END;
		break;
	case DB_OS_SEEK_SET:
		whence = SEEK_SET;
		break;
	default:
		return (EINVAL);
	}

	if (__db_jump.j_seek != NULL)
		return (__db_jump.j_seek(fhp->fd, pgsize, pageno,
		    relative, isrewind, whence) == -1 ? __os_get_errno() : 0);

	offset = (__int64)pgsize * pageno + relative;
	if (isrewind)
		offset = -offset;
	return (_lseeki64(
	    fhp->fd, offset, whence) == -1 ? __os_get_errno() : 0);
}
