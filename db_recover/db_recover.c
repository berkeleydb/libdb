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
static const char sccsid[] = "@(#)db_recover.c	11.1 (Sleepycat) 7/24/99";
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

#include <signal.h>
#include <stdlib.h>
#include <unistd.h>
#endif

#include "db_int.h"
#include "txn.h"

void	 db_init __P((char *, int));
int	 main __P((int, char *[]));
void	 onint __P((int));
void	 siginit __P((void));
void	 usage __P((void));

DB_ENV	*dbenv;
int	 interrupted;
const char
	*progname = "db_recover";			/* Program name. */

int
main(argc, argv)
	int argc;
	char *argv[];
{
	extern char *optarg;
	extern int optind;
	DB_TXNREGION *region;
	time_t now;
	u_int32_t flags;
	int ch, exitval, fatal_recover, ret, verbose;
	char *home;

	home = NULL;
	exitval = fatal_recover = verbose = 0;
	while ((ch = getopt(argc, argv, "ch:v")) != EOF)
		switch (ch) {
		case 'c':
			fatal_recover = 1;
			break;
		case 'h':
			home = optarg;
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
	if (verbose) {
		(void)dbenv->set_verbose(dbenv, DB_VERB_RECOVERY, 1);
		(void)dbenv->set_verbose(dbenv, DB_VERB_CHKPOINT, 1);
	}

	/*
	 * Initialize the environment -- we don't actually do anything
	 * else, that all that's needed to run recovery.
	 *
	 * Note that we specify a private environment, as we're about to
	 * create a region, and we don't want to to leave it around.  If
	 * we leave the region around, the application that should create
	 * it will simply join it instead, and will then be running with
	 * incorrectly sized (and probably terribly small) caches.
	 */
	flags = 0;
	LF_SET(DB_CREATE | DB_INIT_LOCK | DB_INIT_LOG |
	    DB_INIT_MPOOL | DB_INIT_TXN | DB_PRIVATE | DB_USE_ENVIRON);
	LF_SET(fatal_recover ?  DB_RECOVER_FATAL : DB_RECOVER);
	if ((ret = dbenv->open(dbenv, home, NULL, flags, 0)) != 0) {
		dbenv->err(dbenv, ret, "open");
		goto shutdown;
	}

	if (verbose) {
		(void)time(&now);
		region = ((DB_TXNMGR *)dbenv->tx_handle)->reginfo.primary;
		dbenv->errx(dbenv, "Recovery complete at %.24s", ctime(&now));
		dbenv->errx(dbenv, "%s %lx %s [%lu][%lu]",
		    "Maximum transaction id", (u_long)region->last_txnid,
		    "Recovery checkpoint", (u_long)region->last_ckp.file,
		    (u_long)region->last_ckp.offset);
	}

	/*
	 * We need to ensure that log records written on behalf of the
	 * recovery are flushed (the log region will disappear as soon
	 * as we close, because we opened it DB_PRIVATE.
	 */
	if ((ret = log_flush(dbenv, NULL)) != 0) {
		dbenv->err(dbenv, ret, "log_flush");
		goto shutdown;
	}

	if (0) {
shutdown:	exitval = 1;
	}

	/* Clean up the environment. */
	if ((ret = dbenv->close(dbenv, 0)) != 0) {
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
	(void)fprintf(stderr, "usage: db_recover [-cv] [-h home]\n");
	exit(1);
}
