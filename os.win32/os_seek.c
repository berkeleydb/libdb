/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997
 *	Sleepycat Software.  All rights reserved.
 */

#include "config.h"

#ifndef lint
static const char sccsid[] = "@(#)os_seek.c	10.6 (Sleepycat) 10/25/97";
#endif /* not lint */

#include "db_int.h"

/*
 * __os_seek --
 *	Seek to a page/byte offset in the file.
 */
int
__os_seek(fd, pgsize, pageno, relative, whence)
	int fd;
	size_t pgsize;
	db_pgno_t pageno;
	u_long relative;
	int whence;
{
	off_t offset;

	offset = pgsize * pageno + relative;

	return (_lseeki64(fd, offset, whence) == -1 ? errno : 0);
}
