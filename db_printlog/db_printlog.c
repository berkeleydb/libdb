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
static const char sccsid[] = "@(#)db_printlog.c	11.3 (Sleepycat) 8/27/99";
#endif

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#endif

#include "db_int.h"
#include "db_page.h"
#include "btree.h"
#include "db_am.h"
#include "hash.h"
#include "log.h"
#include "qam.h"
#include "txn.h"

void	db_init __P((char *));
int	main __P((int, char *[]));
void	onint __P((int));
void	siginit __P((void));
void	usage __P((void));

DB_ENV	*dbenv;
int	 interrupted;
const char
	*progname = "db_printlog";			/* Program name. */

int
main(argc, argv)
	int argc;
	char *argv[];
{
	extern char *optarg;
	extern int optind;
	DBT data;
	DB_LSN key;
	int ch, e_close, exitval, Nflag, ret;
	char *home;

	e_close = exitval = Nflag = 0;
	home = NULL;
	while ((ch = getopt(argc, argv, "h:N")) != EOF)
		switch (ch) {
		case 'h':
			home = optarg;
			break;
		case 'N':
			Nflag = 1;
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (argc > 0)
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

	/* Optionally turn mutexes and panic checks off. */
	if (Nflag) {
		if ((ret = dbenv->set_mutexlocks(dbenv, 0)) != 0) {
			dbenv->err(dbenv, ret, "set_mutexlocks");
			goto shutdown;
		}
		if ((ret = dbenv->set_panic(dbenv, 0)) != 0) {
			dbenv->err(dbenv, ret, "set_panic");
			goto shutdown;
		}
	}

	/*
	 * An environment is required, but as we may be called to display
	 * information for a single log file, we create one if it does not
	 * already exist.  If we create it, we create it private so that
	 * it automatically goes away when we're done.
	 */
	if (dbenv->open(dbenv,
	    home, NULL, DB_INIT_LOG | DB_USE_ENVIRON, 0) != 0 &&
	    (ret = dbenv->open(dbenv, home, NULL,
	    DB_CREATE | DB_INIT_LOG | DB_PRIVATE | DB_USE_ENVIRON, 0)) != 0) {
		dbenv->err(dbenv, ret, "open");
		goto shutdown;
	}
	e_close = 1;

	/* Initialize print callbacks. */
	if ((ret = __bam_init_print(dbenv)) != 0 ||
	    (ret = __crdel_init_print(dbenv)) != 0 ||
	    (ret = __db_init_print(dbenv)) != 0 ||
	    (ret = __qam_init_print(dbenv)) != 0 ||
	    (ret = __ham_init_print(dbenv)) != 0 ||
	    (ret = __log_init_print(dbenv)) != 0 ||
	    (ret = __txn_init_print(dbenv)) != 0) {
		dbenv->err(dbenv, ret, "callback: initialization");
		goto shutdown;
	}

	memset(&data, 0, sizeof(data));
	while (!interrupted) {
		if ((ret = log_get(dbenv, &key, &data, DB_NEXT)) != 0) {
			if (ret == DB_NOTFOUND)
				break;
			dbenv->err(dbenv, ret, "log_get");
			goto shutdown;
		}

		/*
		 * XXX
		 * We're looking into an opaque structure, here.
		 */
		if (dbenv->tx_recover != NULL)
			ret = dbenv->tx_recover(dbenv, &data, &key, 0, NULL);
		else
			ret = __db_dispatch(dbenv, &data, &key, 0, NULL);

		/*
		 * XXX
		 * Just in case the underlying routines don't flush.
		 */
		(void)fflush(stdout);

		if (ret != 0) {
			dbenv->err(dbenv, ret, "tx: dispatch");
			goto shutdown;
		}
	}

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
	fprintf(stderr, "usage: db_printlog [-N] [-h home]\n");
	exit (1);
}
