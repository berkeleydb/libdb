/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 1997, 1998, 1999, 2000
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char copyright[] =
    "Copyright (c) 1996-2000\nSleepycat Software Inc.  All rights reserved.\n";
static const char revid[] =
    "$Id: db_verify.c,v 1.8 2000/05/31 15:10:00 bostic Exp $";
#endif

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#endif

#include "db_int.h"

void	db_init __P((char *));
int	main __P((int, char *[]));
void	usage __P((void));

const char
	*progname = "db_verify";			/* Program name. */

int
main(argc, argv)
	int argc;
	char *argv[];
{
	extern char *optarg;
	extern int optind;
	DB *dbp;
	DB_ENV *dbenv;
	int ch, e_close, exitval, quiet, ret, t_ret;
	char *home;

	dbenv = NULL;
	quiet = 0;
	e_close = exitval = 0;
	home = NULL;
	while ((ch = getopt(argc, argv, "h:NqV")) != EOF)
		switch (ch) {
		case 'h':
			home = optarg;
			break;
		case 'N':
			if ((ret = db_env_set_mutexlocks(0)) != 0) {
				fprintf(stderr,
				    "%s: db_env_set_mutexlocks: %s\n",
				    progname, db_strerror(ret));
				exit (1);
			}
			if ((ret = db_env_set_panicstate(0)) != 0) {
				fprintf(stderr,
				    "%s: db_env_set_panicstate: %s\n",
				    progname, db_strerror(ret));
				exit (1);
			}
			break;
			break;
		case 'q':
			quiet = 1;
			break;
		case 'V':
			printf("%s\n", db_version(NULL, NULL, NULL));
			exit(0);
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (argc <= 0)
		usage();

	/* Handle possible interruptions. */
	__db_util_siginit();

	/*
	 * Create an environment object and initialize it for error
	 * reporting.
	 */
	if ((ret = db_env_create(&dbenv, 0)) != 0) {
		fprintf(stderr, "%s: db_env_create: %s\n",
		    progname, db_strerror(ret));
		goto shutdown;
	}
	e_close = 1;

	/*
	 * XXX
	 * We'd prefer to have error output configured while calling
	 * db_env_create, but there's no way to turn it off once it's
	 * turned on.
	 */
	if (!quiet) {
		dbenv->set_errfile(dbenv, stderr);
		dbenv->set_errpfx(dbenv, progname);
	}

	/*
	 * If attaching to a pre-existing environment fails, create a
	 * private one and try again.
	 */
	if ((ret = dbenv->open(dbenv,
	    home, DB_INIT_MPOOL | DB_USE_ENVIRON, 0)) != 0 &&
	    (ret = dbenv->open(dbenv,
	    home, DB_CREATE | DB_INIT_MPOOL | DB_PRIVATE, 0)) != 0) {
		dbenv->err(dbenv, ret, "open");
		goto shutdown;
	}

	for (; !__db_util_interrupted() && argv[0] != NULL; ++argv) {
		if ((ret = db_create(&dbp, dbenv, 0)) != 0) {
			fprintf(stderr,
			    "%s: db_create: %s\n", progname, db_strerror(ret));
			goto shutdown;
		}
		if (!quiet) {
			dbp->set_errfile(dbp, stderr);
			dbp->set_errpfx(dbp, progname);
		}
		if ((ret = dbp->verify(dbp, argv[0], NULL, NULL, 0)) != 0)
			dbp->err(dbp, ret, "DB->verify: %s", argv[0]);
		if ((t_ret = dbp->close(dbp, 0)) != 0 && ret == 0) {
			dbp->err(dbp, ret, "DB->close: %s", argv[0]);
			ret = t_ret;
		}
		if (ret != 0)
			goto shutdown;
	}

	if (0) {
shutdown:	exitval = 1;
	}
	if (e_close && (ret = dbenv->close(dbenv, 0)) != 0) {
		exitval = 1;
		fprintf(stderr,
		    "%s: dbenv->close: %s\n", progname, db_strerror(ret));
	}

	/* Resend any caught signal. */
	__db_util_sigresend();

	return (exitval);
}

void
usage()
{
	fprintf(stderr, "usage: db_verify [-NqV] [-h home] db_file ...\n");
	exit (1);
}
