/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 1997, 1998
 *	Sleepycat Software.  All rights reserved.
 *
 * This code is derived from software contributed to Sleepycat Software by
 * Frederick G.M. Roeber of Netscape Communications Corp.
 */

#include "config.h"

#ifndef lint
static const char sccsid[] = "@(#)os_seek.c	10.5 (Sleepycat) 7/21/98";
#endif /* not lint */

#include "db_int.h"

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
	long int offset;
	int ret;

	if (__db_jump.j_seek != NULL)
		ret = __db_jump.j_seek(fd,
		    pgsize, pageno, relative, isrewind, whence);
	else {
		offset = (long int)pgsize * pageno + relative;
		if (isrewind)
			offset = -offset;

		ret = lseek(fd, offset, whence);
	}
	return (ret == -1 ? errno : 0);
}
