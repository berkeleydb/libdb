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
static const char sccsid[] = "@(#)db_archive.c	11.2 (Sleepycat) 10/29/99";
#endif

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#endif

#include "db_int.h"

int	 main __P((int, char *[]));
void	 onint __P((int));
void	 siginit __P((void));
void	 usage __P((void));

DB_ENV	*dbenv;
int	 interrupted;
const char
	*progname = "db_archive";			/* Program name. */

int
main(argc, argv)
	int argc;
	char *argv[];
{
	extern char *optarg;
	extern int optind;
	u_int32_t flags;
	int ch, e_close, exitval, ret, verbose;
	char *home, **list;

	flags = 0;
	e_close = exitval = verbose = 0;
	home = NULL;
	while ((ch = getopt(argc, argv, "ah:lsv")) != EOF)
		switch (ch) {
		case 'a':
			LF_SET(DB_ARCH_ABS);
			break;
		case 'h':
			home = optarg;
			break;
		case 'l':
			LF_SET(DB_ARCH_LOG);
			break;
		case 's':
			LF_SET(DB_ARCH_DATA);
			break;
		case 'v':
			verbose = 1;
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (argc != 0)
		usage();

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
	if (verbose)
		(void)dbenv->set_verbose(dbenv, DB_VERB_CHKPOINT, 1);

	/*
	 * An environment is required.
	 *
	 * XXX
	 * It just *might* be reasonable for a region to be corrupted, and
	 * we're getting called to decide what log files to keep.  I don't
	 * think so, but I'm not positive.
	 */
	if ((ret = dbenv->open(dbenv, home, NULL,
	    DB_CREATE | DB_INIT_LOG | DB_INIT_TXN | DB_USE_ENVIRON, 0)) != 0) {
		dbenv->err(dbenv, ret, "open");
		goto shutdown;
	}
	e_close = 1;

	/* Get the list of names. */
	if ((ret = log_archive(dbenv, &list, flags, NULL)) != 0) {
		dbenv->err(dbenv, ret, "log_archive");
		goto shutdown;
	}

	/* Print the list of names. */
	if (list != NULL)
		for (; *list != NULL; ++list)
			printf("%s\n", *list);

	if (0) {
shutdown:	exitval = 1;
	}
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
	(void)fprintf(stderr, "usage: db_archive [-alsv] [-h home]\n");
	exit (1);
}
