/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)os_open.c	11.1 (Sleepycat) 7/25/99";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <fcntl.h>
#include <signal.h>
#endif

#include "db_int.h"

/*
 * __os_open --
 *	Open a file.
 *
 * PUBLIC: int __os_open __P((const char *, u_int32_t, int, DB_FH *));
 */
int
__os_open(name, flags, mode, fhp)
	const char *name;
	u_int32_t flags;
	int mode;
	DB_FH *fhp;
{
#if defined(HAVE_SIGFILLSET)
	sigset_t set, oset;
#endif
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

#if defined(HAVE_SIGFILLSET)
	/*
	 * We block every signal we can get our hands on so that the temporary
	 * file isn't left around if we're interrupted at the wrong time.  Of
	 * course, if we drop core in-between the calls we'll hang forever, but
	 * that's probably okay.  ;-)
	 */
	if (LF_ISSET(DB_OSO_TEMP)) {
		(void)sigfillset(&set);
		(void)sigprocmask(SIG_BLOCK, &set, &oset);
	}
#endif

	/* Open the file. */
	if ((ret = __os_openhandle(name, oflags, mode, fhp)) != 0)
		return (ret);

	/* Delete any temporary file. */
	if (LF_ISSET(DB_OSO_TEMP)) {
		(void)__os_unlink(name);
#if defined(HAVE_SIGFILLSET)
		(void)sigprocmask(SIG_SETMASK, &oset, NULL);
#endif
	}

#if defined(HAVE_FCNTL_F_SETFD)
	/* Deny file descriptor access to any child process. */
	if (fcntl(fhp->fd, F_SETFD, 1) == -1) {
		ret = __os_get_errno();
		(void)__os_closehandle(fhp);
		return (ret);
	}
#endif
	return (0);
}
