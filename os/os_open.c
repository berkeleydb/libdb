/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998, 1999, 2000
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char revid[] = "$Id: os_open.c,v 11.8 2000/05/09 13:48:22 bostic Exp $";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <fcntl.h>
#include <string.h>
#endif

#include "db_int.h"

/*
 * __os_open --
 *	Open a file.
 *
 * PUBLIC: int __os_open __P((DB_ENV *, const char *, u_int32_t, int, DB_FH *));
 */
int
__os_open(dbenv, name, flags, mode, fhp)
	DB_ENV *dbenv;
	const char *name;
	u_int32_t flags;
	int mode;
	DB_FH *fhp;
{
	int oflags, ret;

	oflags = 0;

	/*
	 * DB requires the POSIX 1003.1 semantic that two files opened at the
	 * same time with DB_OSO_CREATE/O_CREAT and DB_OSO_EXCL/O_EXCL flags
	 * set return an EEXIST failure in at least one.
	 */
	if (LF_ISSET(DB_OSO_CREATE))
		oflags |= O_CREAT;

	if (LF_ISSET(DB_OSO_EXCL))
		oflags |= O_EXCL;

#if defined(O_DSYNC) && defined(XXX_NEVER_SET)
	/*
	 * !!!
	 * We should get better performance if we push the log files to disk
	 * immediately instead of waiting for the sync.  However, Solaris
	 * (and likely any other system based on the 4BSD filesystem releases),
	 * doesn't implement O_DSYNC correctly, only flushing data blocks and
	 * not inode or indirect blocks.
	 */
	if (LF_ISSET(DB_OSO_LOG))
		oflags |= O_DSYNC;
#endif

	if (LF_ISSET(DB_OSO_RDONLY))
		oflags |= O_RDONLY;
	else
		oflags |= O_RDWR;

	if (LF_ISSET(DB_OSO_TRUNC))
		oflags |= O_TRUNC;

	/* Open the file. */
	if ((ret = __os_openhandle(dbenv, name, oflags, mode, fhp)) != 0)
		return (ret);

	/*
	 * Delete any temporary file.
	 *
	 * !!!
	 * There's a race here, where we've created a file and we crash before
	 * we can unlink it.  Temporary files aren't common in DB, regardless,
	 * it's not a security problem because the file is empty.  There's no
	 * reasonable way to avoid the race (playing signal games isn't worth
	 * the portability nightmare), so we just live with it.
	 */
	if (LF_ISSET(DB_OSO_TEMP))
		(void)__os_unlink(dbenv, name);

	return (0);
}
