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
static const char sccsid[] = "@(#)db_deadlock.c	11.1 (Sleepycat) 7/24/99";
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
#include "clib_ext.h"

int	 logpid __P((char *, int));
int	 main __P((int, char *[]));
void	 onint __P((int));
void	 siginit __P((void));
void	 usage __P((void));

DB_ENV  *dbenv;
int	 interrupted;
const char
	*progname = "db_deadlock";			/* Program name. */

int
main(argc, argv)
	int argc;
	char *argv[];
{
	extern char *optarg;
	extern int optind;
	u_int32_t atype;
	time_t now;
	long usecs;
	u_int32_t flags;
	int ch, e_close, exitval, ret, verbose;
	char *home, *logfile;

	atype = DB_LOCK_DEFAULT;
	home = logfile = NULL;
	usecs = 0;
	flags = 0;
	e_close = exitval = verbose = 0;
	while ((ch = getopt(argc, argv, "a:h:L:t:vw")) != EOF)
		switch (ch) {
		case 'a':
			switch (optarg[0]) {
			case 'o':
				atype = DB_LOCK_OLDEST;
				break;
			case 'y':
				atype = DB_LOCK_YOUNGEST;
				break;
			default:
				usage();
				/* NOTREACHED */
			}
			if (optarg[1] != '\0')
				usage();
			break;
		case 'h':
			home = optarg;
			break;
		case 'L':
			logfile = optarg;
			break;
		case 't':
			(void)__db_getlong(NULL,
			    progname, optarg, 1, LONG_MAX, &usecs);
			usecs *= 1000000;
			break;
		case 'v':
			verbose = 1;
			break;
		case 'w':
			LF_SET(DB_LOCK_CONFLICT);
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (argc != 0)
		usage();

	if (usecs == 0 && !LF_ISSET(DB_LOCK_CONFLICT)) {
		fprintf(stderr,
		    "%s: at least one of -t and -w must be specified\n",
		    progname);
		exit(1);
	}

	/*
	 * We detect every 100ms (100000 us) when we're running in
	 * DB_LOCK_CONFLICT mode.
	 */
	if (usecs == 0)
		usecs = 100000;

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
	if (verbose) {
		(void)dbenv->set_verbose(dbenv, DB_VERB_DEADLOCK, 1);
		(void)dbenv->set_verbose(dbenv, DB_VERB_WAITSFOR, 1);
	}

	/* An environment is required. */
	if ((ret = dbenv->open(dbenv,
	    home, NULL, DB_INIT_LOCK | DB_USE_ENVIRON, 0)) != 0) {
		dbenv->err(dbenv, ret, "open");
		goto shutdown;
	}
	e_close = 1;

	/* Log our process ID. */
	if (logfile != NULL && logpid(logfile, 1)) {
		exitval = 1;
		goto shutdown;
	}

	while (!interrupted) {
		if (verbose) {
			(void)time(&now);
			dbenv->errx(dbenv, "running at %.24s", ctime(&now));
		}

		if ((ret = lock_detect(dbenv, flags, atype, NULL)) != 0) {
			dbenv->err(dbenv, ret, "lock_detect");
			goto shutdown;
		}

		/* Make a pass every "usecs" usecs. */
		(void)__os_sleep(0, usecs);
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
    "usage: db_deadlock [-vw] [-a m | o | y] [-h home] [-L file] [-t sec]\n");
	exit(1);
}
