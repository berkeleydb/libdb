/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char copyright[] =
"@(#) Copyright (c) 1996, 1997, 1998, 1999\n\
	Sleepycat Software Inc.  All rights reserved.\n";
static const char sccsid[] = "@(#)db_checkpoint.c	11.4 (Sleepycat) 10/20/99";
#endif

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#if TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else
#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <time.h>
#endif
#endif

#include <errno.h>
#include <limits.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#endif

#include "db_int.h"
#include "db_page.h"
#include "btree.h"
#include "hash.h"
#include "qam.h"
#include "clib_ext.h"

char	*check __P((DB_ENV *, long, long));
int	 logpid __P((char *, int));
int	 main __P((int, char *[]));
void	 onint __P((int));
void	 siginit __P((void));
void	 usage __P((void));

DB_ENV	*dbenv;
int	 interrupted;
const char
	*progname = "db_checkpoint";		/* Program name. */

int
main(argc, argv)
	int argc;
	char *argv[];
{
	extern char *optarg;
	extern int optind;
	time_t now;
	long argval;
	u_int32_t kbytes, minutes, seconds;
	int ch, e_close, exitval, once, ret, verbose;
	char *home, *logfile;

	/*
	 * XXX
	 * Don't allow a fully unsigned 32-bit number, some compilers get
	 * upset and require it to be specified in hexadecimal and so on.
	 */
#define	MAX_UINT32_T	2147483647

	kbytes = minutes = 0;
	e_close = exitval = once = verbose = 0;
	home = logfile = NULL;
	while ((ch = getopt(argc, argv, "1h:k:L:p:v")) != EOF)
		switch (ch) {
		case '1':
			once = 1;
			break;
		case 'h':
			home = optarg;
			break;
		case 'k':
			(void)__db_getlong(NULL, progname,
			    optarg, 1, (long)MAX_UINT32_T, &argval);
			kbytes = argval;
			break;
		case 'L':
			logfile = optarg;
			break;
		case 'p':
			(void)__db_getlong(NULL, progname,
			    optarg, 1, (long)MAX_UINT32_T, &argval);
			minutes = argval;
			break;
		case 'v':
			verbose = 1;
			break;
		case '?':
		default:
			usage();
			goto shutdown;
		}
	argc -= optind;
	argv += optind;

	if (argc != 0) {
		usage();
		goto shutdown;
	}

	if (once == 0 && kbytes == 0 && minutes == 0) {
		(void)fprintf(stderr,
		    "%s: at least one of -1, -k and -p must be specified\n",
		    progname);
		exit (1);
	}

	/* Handle possible interruptions. */
	siginit();

	/*
	 * Create an environment object and initialize it for error
	 * reporting.
	 */
	if ((ret = db_env_create(&dbenv, 0)) != 0) {
		fprintf(stderr,
		    "%s: db_env_create: %s\n", progname, db_strerror(ret));
		exit (1);
	}
	dbenv->set_errfile(dbenv, stderr);
	dbenv->set_errpfx(dbenv, progname);

	/* Initialize the environment. */
	if ((ret = dbenv->open(dbenv, home, NULL,
	   DB_INIT_LOG | DB_INIT_TXN | DB_INIT_MPOOL | DB_USE_ENVIRON,
	   0)) != 0) {
		dbenv->err(dbenv, ret, "open");
		goto shutdown;
	}
	e_close = 1;

	/* Register the standard pgin/pgout functions, in case we do I/O. */
	if ((ret =
	    memp_register(dbenv, DB_FTYPE_SET, __db_pgin, __db_pgout)) != 0) {
		dbenv->err(dbenv, ret,
		    "failed to register access method functions");
		goto shutdown;
	}

	/* Log our process ID. */
	if (logfile != NULL && logpid(logfile, 1)) {
		exitval = 1;
		goto shutdown;
	}

	/*
	 * If we have only a time delay, then we'll sleep the right amount
	 * to wake up when a checkpoint is necessary.  If we have a "kbytes"
	 * field set, then we'll check every 30 seconds.
	 */
	seconds = kbytes != 0 ? 30 : minutes * 60;
	while (!interrupted) {
		if (verbose) {
			(void)time(&now);
			dbenv->errx(dbenv, "checkpoint: %s", ctime(&now));
		}

		ret = txn_checkpoint(dbenv, kbytes, minutes);
		while (ret == DB_INCOMPLETE) {
			if (verbose)
				dbenv->errx(dbenv,
				    "checkpoint did not finish, retrying\n");
			(void)__os_sleep(2, 0);
			ret = txn_checkpoint(dbenv, 0, 0);
		}
		if (ret != 0) {
			dbenv->err(dbenv, ret, "txn_checkpoint");
			goto shutdown;
		}

		if (once)
			break;

		(void)__os_sleep(seconds, 0);
	}


	if (0) {
shutdown:	exitval = 1;
	}

	/* Clean up the logfile. */
	if (logfile != NULL && logpid(logfile, 0))
		exitval = 1;

	/* Clean up the environment. */
	if (e_close && (ret = dbenv->close(dbenv, 0)) != 0) {
		exitval = 1;
		fprintf(stderr,
		    "%s: dbenv->close: %s\n", progname, db_strerror(ret));
	}

	if (interrupted) {
		(void)signal(interrupted, SIG_DFL);
		(void)raise(interrupted);
		/* NOTREACHED */
	}

	return (exitval);
}

/*
 * logpid --
 *	Log that we're running.
 */
int
logpid(fname, is_open)
	char *fname;
	int is_open;
{
	FILE *fp;
	time_t now;

	if (is_open) {
		if ((fp = fopen(fname, "w")) == NULL) {
			dbenv->err(dbenv, errno, "%s", fname);
			return (1);
		}
		(void)time(&now);
		fprintf(fp,
		    "%s: %lu %.24s", progname, (u_long)getpid(), ctime(&now));
		fclose(fp);
	} else
		(void)remove(fname);
	return (0);
}

/*
 * siginit --
 *	Initialize the set of signals for which we want to clean up.
 *	Generally, we try not to leave the shared regions locked if
 *	we can.
 */
void
siginit()
{
#ifdef SIGHUP
	(void)signal(SIGHUP, onint);
#endif
	(void)signal(SIGINT, onint);
#ifdef SIGPIPE
	(void)signal(SIGPIPE, onint);
#endif
	(void)signal(SIGTERM, onint);
}

/*
 * onint --
 *	Interrupt signal handler.
 */
void
onint(signo)
	int signo;
{
	if ((interrupted = signo) == 0)
		interrupted = SIGINT;
}

void
usage()
{
	(void)fprintf(stderr,
    "usage: db_checkpoint [-1v] [-h home] [-k kbytes] [-L file] [-p min]\n");
	exit(1);
}
