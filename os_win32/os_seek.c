/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998
 *	Sleepycat Software.  All rights reserved.
 */

#include "config.h"

#ifndef lint
static const char sccsid[] = "@(#)os_seek.c	10.12 (Sleepycat) 10/28/98";
#endif /* not lint */

#include "db_int.h"
#include "os_jump.h"

/*
 * __os_seek --
 *	Seek to a page/byte offset in the file.
 *
 * PUBLIC: int __os_seek __P((int, size_t, db_pgno_t, u_int32_t, int, int));
 */
int
__os_seek(fd, pgsize, pageno, relative, isrewind, whence)
	int fd;
	size_t pgsize;
	db_pgno_t pageno;
	u_int32_t relative;
	int isrewind, whence;
{
	off_t offset;
	int ret;

	if (__db_jump.j_seek != NULL)
		ret = __db_jump.j_seek(fd,
		    pgsize, pageno, relative, isrewind, whence);
	else {
		offset = (off_t)pgsize * pageno + relative;
		if (isrewind)
			offset = -offset;

		ret = (int)_lseeki64(fd, offset, whence);
	}
	return (ret == -1 ? errno : 0);
}
