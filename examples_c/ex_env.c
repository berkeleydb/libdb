/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 *
 *	@(#)ex_env.c	11.3 (Sleepycat) 9/21/99
 */

#include "db_config.h"

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#endif

#include <db.h>

#ifdef macintosh
#define	DATABASE_HOME	":database"
#define	CONFIG_DATA_DIR	"DB_DATA_DIR :database"
#else
#ifdef _WIN32
#define	DATABASE_HOME	"\\tmp\\database"
#define	CONFIG_DATA_DIR	"DB_DATA_DIR \\database\\files"
#else
#define	DATABASE_HOME	"/tmp/database"
#define	CONFIG_DATA_DIR	"DB_DATA_DIR /database/files"
#endif
#endif

void db_setup(char *, char *[], FILE *);
void db_teardown(char *, char *[], FILE *);
int  main(void);

const char
	*progname = "ex_env";			/* Program name. */

/*
 * An example of a program creating/configuring a Berkeley DB environment.
 */
int
main()
{
	char *config[2], *home;

	/*
	 * All of the shared database files live in DATABASE_HOME, but
	 * data files will live in CONFIG_DATA_DIR.
	 */
	home = DATABASE_HOME;
	config[0] = CONFIG_DATA_DIR;
	config[1] = NULL;

	db_setup(home, config, stderr);

	db_teardown(home, config, stderr);

	return (0);
}

void
db_setup(home, config, errfp)
	char *home, *config[];
	FILE *errfp;
{
	DB_ENV *dbenv;
	int ret;

	/*
	 * Create an environment object and initialize it for error
	 * reporting.
	 */
	if ((ret = db_env_create(&dbenv, 0)) != 0) {
		fprintf(errfp, "%s: %s\n", progname, db_strerror(ret));
		exit (1);
	}
	dbenv->set_errfile(dbenv, errfp);
	dbenv->set_errpfx(dbenv, progname);

	/*
	 * We want to specify the shared memory buffer pool cachesize,
	 * but everything else is the default.
	 */
	if ((ret = dbenv->set_cachesize(dbenv, 0, 64 * 1024, 0)) != 0) {
		dbenv->err(dbenv, ret, "set_cachesize");
		dbenv->close(dbenv, 0);
		exit (1);
	}

	/*
	 * We have multiple processes reading/writing these files, so
	 * we need concurrency control and a shared buffer pool, but
	 * not logging or transactions.
	 */
	if ((ret = dbenv->open(dbenv, home, config,
	    DB_CREATE | DB_INIT_LOCK | DB_INIT_MPOOL, 0)) != 0) {
		dbenv->err(dbenv, ret, "environment open: %s", home);
		dbenv->close(dbenv, 0);
		exit (1);
	}

	/* Do something interesting... */

	/* Close the handle. */
	if ((ret = dbenv->close(dbenv, 0)) != 0) {
		fprintf(stderr, "DBENV->close: %s\n", db_strerror(ret));
		exit (1);
	}
}

void
db_teardown(home, config, errfp)
	char *home, *config[];
	FILE *errfp;
{
	DB_ENV *dbenv;
	int ret;

	/* Remove the shared database regions. */
	if ((ret = db_env_create(&dbenv, 0)) != 0) {
		fprintf(errfp, "%s: %s\n", progname, db_strerror(ret));
		exit (1);
	}
	dbenv->set_errfile(dbenv, errfp);
	dbenv->set_errpfx(dbenv, progname);

	if ((ret = dbenv->remove(dbenv, home, config, 0)) != 0) {
		fprintf(stderr, "DBENV->remove: %s\n", db_strerror(ret));
		exit (1);
	}
}
