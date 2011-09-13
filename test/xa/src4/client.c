/*
 * #19612 Data not rolled back when transaction aborted after timout.
 */

#include <sys/types.h>
#include <sys/time.h>

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <tx.h>
#include <atmi.h>
#include <fml32.h>
#include <fml1632.h>

#include <db.h>

#define HOME	"../data"
#define	TABLE1	"../data/table1.db"
#define	TABLE2	"../data/table2.db"

#ifdef VERBOSE
static int verbose = 1;				/* Debugging output. */
#else
static int verbose = 0;
#endif

DB_ENV *dbenv;
char *progname;					/* Client run-time name. */

int   check_data();
int   usage(void);

int
main(int argc, char* argv[])
{
	DB *dbp3;
	DBT key, data;
	TPINIT *initBuf;
        FBFR *replyBuf;
	long replyLen;
	int ch, ret, i;
	char *target;
	char *home = HOME;
	u_int32_t flags = DB_INIT_MPOOL | DB_INIT_LOG | DB_INIT_TXN |
	  DB_INIT_LOCK | DB_CREATE | DB_THREAD | DB_RECOVER | DB_REGISTER;
	u_int32_t dbflags = DB_CREATE | DB_THREAD;

	progname = argv[0];

	initBuf = NULL;
        ret = 0;
        replyBuf = NULL;
        replyLen = 1024;

	while ((ch = getopt(argc, argv, "v")) != EOF)
		switch (ch) {
		case 'v':
			verbose = 1;
			break;
		case '?':
		default:
			return (usage());
		}
	argc -= optind;
	argv += optind;

	if (verbose)
		printf("%s: called\n", progname);

	if (tpinit((TPINIT *)NULL) == -1)
		goto tuxedo_err;
	if (verbose)
		printf("%s: tpinit() OK\n", progname);

	/* Create the DB environment. */
	if ((ret = db_env_create(&dbenv, 0)) != 0 ||
	    (ret = dbenv->open(dbenv, home, flags, 0)) != 0) {
		fprintf(stderr,
		    "%s: %s: %s\n", progname, home, db_strerror(ret));
		goto err;
	}
	dbenv->set_errfile(dbenv, stderr);
	if (verbose)
		printf("%s: opened %s OK\n", progname, home);

	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));

        /* Allocate reply buffer. */
	if ((replyBuf = (FBFR*)tpalloc("FML32", NULL, replyLen)) 
	    == NULL) 
		goto tuxedo_err;
	if (verbose)
		printf("%s: tpalloc(\"FML32\"), reply buffer OK\n", 
		    progname);
	for (i = 0; i < 2; i++) {
        	if (tpbegin(10L, 0L) == -1)
			goto tuxedo_err;
		if (verbose)
			printf("%s: tpbegin() OK\n", progname);

		if (tpcall("TestTxn1", NULL, 0L, (char **)&replyBuf, 
		    &replyLen, TPSIGRSTRT) == -1) 
			goto tuxedo_err;

        	/* This call will timeout. */
        	tpcall("TestTxn2", NULL, 0L, (char **)&replyBuf, &replyLen, 
            	    TPSIGRSTRT);
        	if (tperrno != TPETIME)
	        	goto tuxedo_err;

		if (i == 0) {
			if (tpabort(0L) == -1)
				goto tuxedo_err;
			if (verbose)
				printf("%s: tpabort() OK\n", progname);
		} else {
		  	/* Commit will fail due to the time out. */
			tpcommit(0L);
			if (tperrno != TPEABORT)
				goto tuxedo_err;
			if (verbose)
				printf("%s: tpcommit() OK\n", progname);
		}


		ret = check_data();
	}

	if (0) {
tuxedo_err:	fprintf(stderr, "%s: TUXEDO ERROR: %s (code %d)\n",
		    progname, tpstrerror(tperrno), tperrno);
		goto err;
	}
	if (0) {
err:		ret = EXIT_FAILURE;
	}

	if (replyBuf != NULL)
		tpfree((char *)replyBuf);
	if (dbenv != NULL)
		(void)dbenv->close(dbenv, 0);

	tpterm();
	if (verbose)
		printf("%s: tpterm() OK\n", progname);

 	if (verbose && ret == 0)
		printf("%s: test passed.\n", progname);
	else if (verbose)
 		printf("%s: test failed.\n", progname);
	return (ret);
}

/*
 * check_data --
 *	Check that both databases are empty.
 */
int
check_data()
{
	DB *dbp1, *dbp2;
	DBC *dbc1, *dbc2;
	DBT key1, data1, key2, data2;
	int ret, ret1, ret2;
	u_int32_t flags = DB_INIT_MPOOL | DB_INIT_LOG | DB_INIT_TXN |
	  DB_INIT_LOCK | DB_THREAD;

	dbp1 = dbp2 = NULL;
	dbc1 = dbc2 = NULL;

	/* Open table #1. */
	if ((ret = db_create(&dbp1, dbenv, 0)) != 0 ||
	    (ret = dbp1->open(
	    dbp1, NULL, TABLE1, NULL, DB_UNKNOWN, DB_RDONLY, 0)) != 0) {
		fprintf(stderr,
		    "%s: %s: %s\n", progname, TABLE1, db_strerror(ret));
		goto err;
	}
	if (verbose)
		printf("%s: opened %s OK\n", progname, TABLE1);

        /* Open table #2. */
	if ((ret = db_create(&dbp2, dbenv, 0)) != 0 ||
	    (ret = dbp2->open(
	    dbp2, NULL, TABLE2, NULL, DB_UNKNOWN, DB_RDONLY, 0)) != 0) {
		fprintf(stderr,
		    "%s: %s: %s\n", progname, TABLE2, db_strerror(ret));
		goto err;
	}
	if (verbose)
		printf("%s: opened %s OK\n", progname, TABLE2);

	/* Open cursors. */
	if ((ret = dbp1->cursor(dbp1, NULL, &dbc1, 0)) != 0 ||
	    (ret = dbp2->cursor(dbp2, NULL, &dbc2, 0)) != 0) {
		fprintf(stderr,
		    "%s: DB->cursor: %s\n", progname, db_strerror(ret));
		goto err;
	}
	if (verbose)
		printf("%s: opened cursors OK\n", progname);

	/* Compare the two databases. */
	memset(&key1, 0, sizeof(key1));
	memset(&data1, 0, sizeof(data1));
	memset(&key2, 0, sizeof(key2));
	memset(&data2, 0, sizeof(data2));
	ret1 = dbc1->get(dbc1, &key1, &data1, DB_NEXT);
	ret2 = dbc2->get(dbc2, &key2, &data2, DB_NEXT);
	if (ret1 != DB_NOTFOUND || ret2 !=DB_NOTFOUND) {
		fprintf(stderr, 
		"%s: DB_ERROR: databases 1 and 2 weren't identical\n", 
		progname);
	    ret = 1;
        }

err:	if (dbc1 != NULL)
		(void)dbc1->close(dbc1);
	if (dbc2 != NULL)
		(void)dbc2->close(dbc2);
	if (dbp1 != NULL)
		(void)dbp1->close(dbp1, 0);
        if (dbp2 != NULL)
		(void)dbp2->close(dbp2, 0);

	if (verbose && ret == 0)
		printf("%s: data check passed.\n", progname);
	else if (verbose)
		printf("%s: data check failed.\n", progname);

	return (ret);
}

int
usage()
{
	fprintf(stderr, "usage: %s [-v] [-n txn]\n", progname);
	return (EXIT_FAILURE);
}
