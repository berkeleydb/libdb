/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 *
 *	@(#)EnvExample.cpp	11.3 (Sleepycat) 9/20/99
 */

#include "db_config.h"

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <errno.h>
#include <iostream.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#endif

#include <db_cxx.h>

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

void	db_setup(char *, char *[], ostream&);
void	db_teardown(char *, char *[], ostream&);

char *progname = "EnvExample";			/* Program name. */

//
// An example of a program creating/configuring a Berkeley DB environment.
//
int
main(int, char **)
{
	//
	// Note: it may be easiest to put all Berkeley DB operations in a
	// try block, as seen here.  Alternatively, you can change the
	// ErrorModel in the DbEnv so that exceptions are never thrown
	// and check error returns from all methods.
	//
	try {
		char *config[2], *home;

		//
		// All of the shared database files live in /home/database,
		// but data files live in /database.
		//
		home = DATABASE_HOME;
		config[0] = "DB_DATA_DIR /database/files";
		config[1] = NULL;

		db_setup(DATABASE_HOME, config, cerr);

		db_teardown(DATABASE_HOME, config, cerr);
		return 0;
	}
	catch (DbException &dbe) {
		cerr << "AccessExample: " << dbe.what() << "\n";
		return 1;
	}
}

// Note that any of the db calls can throw DbException
void
db_setup(char *home, char *config[], ostream& err_stream)
{
	DbEnv *dbenv = new DbEnv(0);

	// Output errors to the application's log.
	//
	dbenv->set_error_stream(&err_stream);
	dbenv->set_errpfx(progname);

	//
	// We want to specify the shared memory buffer pool cachesize,
	// but everything else is the default.
	//
	dbenv->set_cachesize(0, 64 * 1024, 0);

	//
	// We have multiple processes reading/writing these files, so
	// we need concurrency control and a shared buffer pool, but
	// not logging or transactions.
	//
	dbenv->open(DATABASE_HOME, config,
		    DB_CREATE | DB_INIT_LOCK | DB_INIT_MPOOL, 0);

	// Do something interesting...

	// Close the handle.
	dbenv->close(0);
}

void
db_teardown(char *home, char *config[], ostream& err_stream)
{
	// Remove the shared database regions.
	DbEnv *dbenv = new DbEnv(0);

	dbenv->set_error_stream(&err_stream);
	dbenv->set_errpfx(progname);

	dbenv->remove(home, config, 0);
	delete dbenv;
}

