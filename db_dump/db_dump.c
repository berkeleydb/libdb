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
static const char sccsid[] = "@(#)db_dump.c	11.12 (Sleepycat) 11/10/99";
#endif

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#endif

#include "db_int.h"
#include "db_page.h"
#include "db_shash.h"
#include "btree.h"
#include "hash.h"
#include "lock.h"

void	 configure __P((char *));
int	 db_init __P((char *, int));
int	 dump __P((DB *, int, int));
int	 dump_sub __P((DB *, char *, int, int));
int	 is_sub __P((DB *, int *));
int	 main __P((int, char *[]));
void	 onint __P((int));
int	 pheader __P((DB *, char *, int, int));
int	 show_subs __P((DB *));
void	 siginit __P((void));
void	 usage __P((void));

DB_ENV	*dbenv;
int	 interrupted;
const char
	*progname = "db_dump";				/* Program name. */

int
main(argc, argv)
	int argc;
	char *argv[];
{
	extern char *optarg;
	extern int optind;
	DB *dbp;
	int ch, d_close;
	int e_close, exitval, lflag, Nflag, pflag, ret, subs, keyflag;
	char *dopt, *home, *subname;

	dbp = NULL;
	d_close = e_close = exitval = lflag = Nflag = pflag = 0;
	keyflag = 0;
	dopt = home = subname = NULL;
	while ((ch = getopt(argc, argv, "d:f:h:klNps:")) != EOF)
		switch (ch) {
		case 'd':
			dopt = optarg;
			break;
		case 'f':
			if (freopen(optarg, "w", stdout) == NULL) {
				fprintf(stderr, "%s: %s: reopen: %s\n",
				    progname, optarg, strerror(errno));
				exit (1);
			}
			break;
		case 'h':
			home = optarg;
			break;
		case 'k':
			keyflag = 1;
			break;
		case 'l':
			lflag = 1;
			break;
		case 'N':
			Nflag = 1;
			break;
		case 'p':
			pflag = 1;
			break;
		case 's':
			subname = optarg;
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (argc != 1)
		usage();

	if (dopt != NULL && pflag) {
		fprintf(stderr,
		    "%s: the -d and -p options may not both be specified\n",
		    progname);
		exit (1);
	}
	if (lflag && subname != NULL) {
		fprintf(stderr,
		    "%s: the -l and -s options may not both be specified\n",
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
	e_close = 1;
	dbenv->set_errfile(dbenv, stderr);
	dbenv->set_errpfx(dbenv, progname);

	/* Initialize the environment. */
	if (db_init(home, Nflag) != 0)
		goto err;

	/* Create the DB object and open the file. */
	if ((ret = db_create(&dbp, dbenv, 0)) != 0) {
		dbenv->err(dbenv, ret, "db_create");
		goto err;
	}
	d_close = 1;
	if ((ret = dbp->open(dbp,
	    argv[0], subname, DB_UNKNOWN, DB_RDONLY, 0)) != 0) {
		dbp->err(dbp, ret, "open: %s", argv[0]);
		goto err;
	}

	if (dopt != NULL) {
		if (__db_dump(dbp, dopt, NULL)) {
			dbp->err(dbp, ret, "__db_dump: %s", argv[0]);
			goto err;
		}
	} else if (lflag) {
		if (is_sub(dbp, &subs))
			goto err;
		if (subs == 0) {
			dbp->errx(dbp,
			    "%s: does not contain subdatabases", argv[0]);
			goto err;
		}
		if (show_subs(dbp))
			goto err;
	} else {
		subs = 0;
		if (subname == NULL && is_sub(dbp, &subs))
			goto err;
		if (subs) {
			if (dump_sub(dbp, argv[0], pflag, keyflag))
				goto err;
		} else
			if (pheader(dbp, NULL, pflag, keyflag)
			    || dump(dbp, pflag, keyflag))
				goto err;
	}

	if (0) {
err:		exitval = 1;
	}
	if (d_close && (ret = dbp->close(dbp, 0)) != 0) {
		exitval = 1;
		dbp->err(dbp, ret, "close");
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
 * db_init --
 *	Initialize the environment.
 */
int
db_init(home, Nflag)
	char *home;
	int Nflag;
{
	u_int32_t flags;
	int ret;

	/* Optionally turn mutexes off. */
	if (Nflag) {
		if ((ret = dbenv->set_mutexlocks(dbenv, 0)) != 0) {
			dbenv->err(dbenv, ret, "set_mutexlocks");
			return (1);
		}
		if ((ret = dbenv->set_panic(dbenv, 0)) != 0) {
			dbenv->err(dbenv, ret, "set_panic");
			return (1);
		}
	}

	/*
	 * Try and use the shared memory pool region when dumping a database,
	 * so our information is as up-to-date as possible, even if the mpool
	 * cache hasn't been flushed.
	 */
	flags = DB_USE_ENVIRON | DB_INIT_MPOOL;
	if (dbenv->open(dbenv, home, NULL, flags, 0) == 0)
		return (0);

	/*
	 * An environment is required because we may be trying to look at
	 * databases in directories other than the current one.  We could
	 * avoid using an environment iff the -h option wasn't specified,
	 * but that seems like more work than it's worth.
	 *
	 * No environment exists (or, at least no environment that includes
	 * an mpool region exists).  Create one, but make it private so that
	 * no files are actually created.
	 */
	LF_SET(DB_CREATE | DB_PRIVATE);
	if ((ret = dbenv->open(dbenv, home, NULL, flags, 0)) == 0)
		return (0);

	/* An environment is required. */
	dbenv->err(dbenv, ret, "open");
	return (1);
}

/*
 * pheader --
 *	Write out the header information.
 */
int
pheader(dbp, subname, pflag, keyflag)
	DB *dbp;
	char *subname;
	int pflag;
	int keyflag;
{
	DB_BTREE_STAT *btsp;
	DB_HASH_STAT *hsp;
	DB_QUEUE_STAT *qsp;
	int ret;

	printf("VERSION=2\n");
	printf("format=%s\n", pflag ? "print" : "bytevalue");
	if (subname != NULL)
		printf("subdatabase=%s\n", subname);
	switch (dbp->type) {
	case DB_BTREE:
		printf("type=btree\n");
		if ((ret = dbp->stat(dbp, &btsp, NULL, 0)) != 0) {
			dbp->err(dbp, ret, "DB->stat");
			return (1);
		}
		if (F_ISSET(dbp, BTM_RECNUM))
			printf("recnum=1\n");
		if (btsp->bt_maxkey != 0)
			printf("bt_maxkey=%lu\n", (u_long)btsp->bt_maxkey);
		if (btsp->bt_minkey != 0 && btsp->bt_minkey != DEFMINKEYPAGE)
			printf("bt_minkey=%lu\n", (u_long)btsp->bt_minkey);
		break;
	case DB_HASH:
		printf("type=hash\n");
		if ((ret = dbp->stat(dbp, &hsp, NULL, 0)) != 0) {
			dbp->err(dbp, ret, "DB->stat");
			return (1);
		}
		if (hsp->hash_ffactor != 0)
			printf("h_ffactor=%lu\n", (u_long)hsp->hash_ffactor);
		if (hsp->hash_nelem != 0 || hsp->hash_nrecs != 0)
			printf("h_nelem=%lu\n",
			    hsp->hash_nelem > hsp->hash_nrecs ?
			    (u_long)hsp->hash_nelem : (u_long)hsp->hash_nrecs);
		break;
	case DB_QUEUE:
		printf("type=queue\n");
		if ((ret = dbp->stat(dbp, &qsp, NULL, 0)) != 0) {
			dbp->err(dbp, ret, "DB->stat");
			return (1);
		}
		printf("re_len=%lu\n", (u_long)qsp->qs_re_len);
		if (qsp->qs_re_pad != 0 && qsp->qs_re_pad != ' ')
			printf("re_pad=%#x\n", qsp->qs_re_pad);
		break;
	case DB_RECNO:
		printf("type=recno\n");
		if ((ret = dbp->stat(dbp, &btsp, NULL, 0)) != 0) {
			dbp->err(dbp, ret, "DB->stat");
			return (1);
		}
		if (F_ISSET(dbp, BTM_RENUMBER))
			printf("renumber=1\n");
		if (F_ISSET(dbp, BTM_FIXEDLEN))
			printf("re_len=%lu\n", (u_long)btsp->bt_re_len);
		if (btsp->bt_re_pad != 0 && btsp->bt_re_pad != ' ')
			printf("re_pad=%#x\n", btsp->bt_re_pad);
		break;
	case DB_UNKNOWN:
		abort();			/* Impossible. */
		/* NOTREACHED */
	}

	if (F_ISSET(dbp, DB_AM_DUP))
		printf("duplicates=1\n");

	if (!F_ISSET(dbp, DB_AM_PGDEF))
		printf("db_pagesize=%lu\n", (u_long)dbp->pgsize);

	if (keyflag)
		printf("keys=1\n");

	printf("HEADER=END\n");
	return (0);
}

/*
 * is_sub --
 *	Return if the database contains subdatabases.
 */
int
is_sub(dbp, yesno)
	DB *dbp;
	int *yesno;
{
	DB_BTREE_STAT *btsp;
	DB_HASH_STAT *hsp;
	int ret;

	switch (dbp->type) {
	case DB_BTREE:
	case DB_RECNO:
		if ((ret = dbp->stat(dbp, &btsp, NULL, 0)) != 0) {
			dbp->err(dbp, ret, "DB->stat");
			break;
		}
		*yesno = btsp->bt_metaflags & BTM_SUBDB ? 1 : 0;
		break;
	case DB_HASH:
		if ((ret = dbp->stat(dbp, &hsp, NULL, 0)) != 0) {
			dbp->err(dbp, ret, "DB->stat");
			break;
		}
		*yesno = hsp->hash_metaflags & DB_HASH_SUBDB ? 1 : 0;
		break;
	case DB_QUEUE:
		return (0);
	default:
		abort();
		/* NOTREACHED */
	}
	return (ret);
}

/*
 * dump_sub --
 *	Dump out the records for a DB containing subdatabases.
 */
int
dump_sub(parent_dbp, parent_name, pflag, keyflag)
	DB *parent_dbp;
	char *parent_name;
	int pflag, keyflag;
{
	DB *dbp;
	DBC *dbcp;
	DBT key, data;
	int ret;
	char *subdb;

	/*
	 * Get a cursor and step through the database, dumping out each
	 * subdatabase.
	 */
	if ((ret = parent_dbp->cursor(parent_dbp, NULL, &dbcp, 0)) != 0) {
		dbenv->err(dbenv, ret, "DB->cursor");
		return (1);
	}

	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	while ((ret = dbcp->c_get(dbcp, &key, &data, DB_NEXT)) == 0) {
		/* Nul terminate the subdatabase name. */
		if ((subdb = malloc(key.size + 1)) == NULL) {
			dbenv->err(dbenv, ENOMEM, NULL);
			return (1);
		}
		memcpy(subdb, key.data, key.size);
		subdb[key.size] = '\0';

		/* Create the DB object and open the file. */
		if ((ret = db_create(&dbp, dbenv, 0)) != 0) {
			dbenv->err(dbenv, ret, "db_create");
			free(subdb);
			return (1);
		}
		if ((ret = dbp->open(dbp,
		    parent_name, subdb, DB_UNKNOWN, DB_RDONLY, 0)) != 0)
			dbp->err(dbp, ret,
			    "DB->open: %s:%s", parent_name, subdb);
		if (ret == 0 &&
		    (pheader(dbp, subdb, pflag, keyflag) ||
		    dump(dbp, pflag, keyflag)))
			ret = 1;
		(void)dbp->close(dbp, 0);
		free(subdb);
		if (ret != 0)
			return (1);
	}
	if (ret != DB_NOTFOUND) {
		dbp->err(dbp, ret, "DBcursor->get");
		return (1);
	}

	if ((ret = dbcp->c_close(dbcp)) != 0) {
		dbp->err(dbp, ret, "DBcursor->close");
		return (1);
	}

	return (0);
}

/*
 * show_subs --
 *	Display the subdatabases for a database.
 */
int
show_subs(dbp)
	DB *dbp;
{
	DBC *dbcp;
	DBT key, data;
	int ret;

	/*
	 * Get a cursor and step through the database, printing out the key
	 * of each key/data pair.
	 */
	if ((ret = dbp->cursor(dbp, NULL, &dbcp, 0)) != 0) {
		dbp->err(dbp, ret, "DB->cursor");
		return (1);
	}

	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	while ((ret = dbcp->c_get(dbcp, &key, &data, DB_NEXT)) == 0) {
		if ((ret = __db_prdbt(&key, 1, NULL, stdout, 0)) != 0) {
			dbp->errx(dbp, NULL);
			return (1);
		}
	}
	if (ret != DB_NOTFOUND) {
		dbp->err(dbp, ret, "DBcursor->get");
		return (1);
	}

	if ((ret = dbcp->c_close(dbcp)) != 0) {
		dbp->err(dbp, ret, "DBcursor->close");
		return (1);
	}
	return (0);
}

/*
 * dump --
 *	Dump out the records for a DB.
 */
int
dump(dbp, pflag, keyflag)
	DB *dbp;
	int pflag, keyflag;
{
	DBC *dbcp;
	DBT key, data;
	int ret, is_recno;

	/*
	 * Get a cursor and step through the database, printing out each
	 * key/data pair.
	 */
	if ((ret = dbp->cursor(dbp, NULL, &dbcp, 0)) != 0) {
		dbp->err(dbp, ret, "DB->cursor");
		return (1);
	}

	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	is_recno = (dbp->type == DB_RECNO || dbp->type == DB_QUEUE);
	keyflag = is_recno ? keyflag : 1;
	while ((ret = dbcp->c_get(dbcp, &key, &data, DB_NEXT)) == 0)
		if ((keyflag && (ret =
		    __db_prdbt(&key, pflag, " ", stdout, is_recno)) != 0) ||
		    (ret = __db_prdbt(&data, pflag, " ", stdout, 0)) != 0) {
			dbp->errx(dbp, NULL);
			return (1);
		}
	if (ret != DB_NOTFOUND) {
		dbp->err(dbp, ret, "DBcursor->get");
		return (1);
	}

	if ((ret = dbcp->c_close(dbcp)) != 0) {
		dbp->err(dbp, ret, "DBcursor->close");
		return (1);
	}

	printf("DATA=END\n");
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

/*
 * usage --
 *	Display the usage message.
 */
void
usage()
{
	(void)fprintf(stderr,
"usage: db_dump [-klNp] [-d ahr] [-f file] [-h home] [-s subdb] db_file\n");
	exit(1);
}
