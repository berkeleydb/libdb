/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 *
 *	@(#)ex_mpool.c	11.1 (Sleepycat) 7/25/99
 */

#include "db_config.h"

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
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#endif

#include <db.h>

#define	MPOOL	"mpool"					/* File. */

void	init(char *, int, int);
int	main(int, char *[]);
void	run(int, int, int, int);
void	usage(void);

const char
	*progname = "ex_mpool";				/* Program name. */

int
main(argc, argv)
	int argc;
	char *argv[];
{
	extern char *optarg;
	extern int optind;
	int cachesize, ch, hits, npages, pagesize;

	cachesize = 20 * 1024;
	hits = 1000;
	npages = 50;
	pagesize = 1024;
	while ((ch = getopt(argc, argv, "c:h:n:p:")) != EOF)
		switch (ch) {
		case 'c':
			if ((cachesize = atoi(optarg)) < 20 * 1024)
				usage();
			break;
		case 'h':
			if ((hits = atoi(optarg)) <= 0)
				usage();
			break;
		case 'n':
			if ((npages = atoi(optarg)) <= 0)
				usage();
			break;
		case 'p':
			if ((pagesize = atoi(optarg)) <= 0)
				usage();
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	/* Initialize the file. */
	init(MPOOL, pagesize, npages);

	/* Get the pages. */
	run(hits, cachesize, pagesize, npages);

	return (0);
}

/*
 * init --
 *	Create a backing file.
 */
void
init(file, pagesize, npages)
	char *file;
	int pagesize, npages;
{
	int cnt, flags, fd;
	char *p;

	/*
	 * Create a file with the right number of pages, and store a page
	 * number on each page.
	 */
        flags = O_CREAT | O_RDWR | O_TRUNC;
#ifdef WIN32
        flags |= O_BINARY;
#endif
	if ((fd = open(file, flags, 0666)) < 0) {
		fprintf(stderr,
		    "%s: %s: %s\n", progname, file, strerror(errno));
		exit (1);
	}
	if ((p = (char *)malloc(pagesize)) == NULL) {
		fprintf(stderr, "%s: %s\n", progname, strerror(ENOMEM));
		exit (1);
	}

	/* The pages are numbered from 0. */
	for (cnt = 0; cnt <= npages; ++cnt) {
		*(int *)p = cnt;
		if (write(fd, p, pagesize) != pagesize) {
			fprintf(stderr,
			    "%s: %s: %s\n", progname, file, strerror(errno));
			exit (1);
		}
	}
	free(p);
}

/*
 * run --
 *	Get a set of pages.
 */
void
run(hits, cachesize, pagesize, npages)
	int hits, cachesize, pagesize, npages;
{
	DB_ENV *dbenv;
	DB_MPOOLFILE *dbmfp;
	db_pgno_t pageno;
	int cnt, ret;
	void *p;

	printf("%s: cachesize: %d; pagesize: %d; N pages: %d\n",
	    progname, cachesize, pagesize, npages);

	/*
	 * Open a memory pool, specify a cachesize, output error messages
	 * to stderr.
	 */
	if ((ret = db_env_create(&dbenv, 0)) != 0) {
		fprintf(stderr,
		    "%s: db_env_create: %s\n", progname, db_strerror(ret));
		exit (1);
	}
	dbenv->set_errfile(dbenv, stderr);
	dbenv->set_errpfx(dbenv, progname);

	/* Set the cachesize. */
	if ((ret = dbenv->set_cachesize(dbenv, 0, cachesize, 0)) != 0) {
		dbenv->err(dbenv, ret, "set_cachesize");
		goto err1;
	}

	/* Open the environment. */
	if ((ret = dbenv->open(
	    dbenv, NULL, NULL, DB_CREATE | DB_INIT_MPOOL, 0)) != 0) {
		dbenv->err(dbenv, ret, "open");
		goto err1;
	}

	/* Open the file in the environment. */
	if ((ret =
	    memp_fopen(dbenv, MPOOL, 0, 0, pagesize, NULL, &dbmfp)) != 0) {
		dbenv->err(dbenv, ret, "memp_fopen: %s", MPOOL);
		goto err1;
	}

	printf("retrieve %d random pages... ", hits);

	srand((u_int)time(NULL));
	for (cnt = 0; cnt < hits; ++cnt) {
		pageno = (rand() % npages) + 1;
		if ((ret = memp_fget(dbmfp, &pageno, 0, &p)) != 0) {
			dbenv->err(dbenv, ret,
			    "unable to retrieve page %lu", (u_long)pageno);
			goto err2;
		}
		if (*(db_pgno_t *)p != pageno) {
			dbenv->errx(dbenv,
			    "wrong page retrieved (%lu != %d)",
			    (u_long)pageno, *(int *)p);
			goto err2;
		}
		if ((ret = memp_fput(dbmfp, p, 0)) != 0) {
			dbenv->err(dbenv, ret,
			    "unable to return page %lu", (u_long)pageno);
			goto err2;
		}
	}

	printf("successful.\n");

	/* Close the file. */
	if ((ret = memp_fclose(dbmfp)) != 0) {
		dbenv->err(dbenv, ret, "memp_fclose");
		goto err1;
	}

	/* Close the pool. */
	if ((ret = dbenv->close(dbenv, 0)) != 0) {
		fprintf(stderr,
		    "%s: db_env_create: %s\n", progname, db_strerror(ret));
		exit (1);
	}
	return;

err2:	(void)memp_fclose(dbmfp);
err1:	(void)dbenv->close(dbenv, 0);
	exit (1);
}

void
usage()
{
	(void)fprintf(stderr,
	    "usage: %s [-c cachesize] [-h hits] [-n npages] [-p pagesize]\n",
	    progname);
	exit(1);
}
