/*
 * This code is for servers used in test 4.  Server 1 inserts
 * into database 1 and Server 2 inserts into database 2, then
 * sleeps for 30 seconds to cause a timeout error in the client.
 */
#include <sys/types.h>

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include <atmi.h>
#include <fml1632.h>
#include <fml32.h>
#include <tx.h>
#include <xa.h>

#include <db.h>

/*
 * The two servers are largely identical, #ifdef the source code.
 */
#ifdef SERVER1
#define	TXN_FUNC		TestTxn1
#define	TXN_STRING		"TestTxn1"
#define TABLE			"table1.db"
#endif
#ifdef SERVER2
#define	TXN_FUNC		TestTxn2
#define	TXN_STRING		"TestTxn2"
#define TABLE			"table2.db"
#endif
void TXN_FUNC(TPSVCINFO *);

#define	HOME	"../data"

#ifdef VERBOSE
static int verbose = 1;				/* Debugging output. */
#else
static int verbose = 0;
#endif

DB *db;					/* Table handle. */

char *progname;					/* Server run-time name. */

/*
 * Called when each server is started.  It creates and opens a database.
 */
int
tpsvrinit(int argc, char* argv[])
{
	int ret;

	progname = argv[0];
	if (verbose)
		printf("%s: called\n", progname);

	/* Open resource managers. */
	if (tx_open() == TX_ERROR) {
		fprintf(stderr, "tx_open: TX_ERROR\n");
		return (-1);
	}

	/* Seed random number generator. */
	srand((u_int)(time(NULL) | getpid()));

	/* Open permanent XA handles. */
	if ((ret = db_create(&db, NULL, DB_XA_CREATE)) != 0) {
		fprintf(stderr, "db_create: %s\n", db_strerror(ret));
		return (-1);
	}
	db->set_errfile(db, stderr);
	if ((ret = db->open(db, NULL,
	    TABLE, NULL, DB_BTREE, DB_AUTO_COMMIT | DB_CREATE, 0660)) != 0) {
		fprintf(stderr, "DB open: %s: %s\n", TABLE, db_strerror(ret));
		return (-1);
	}

	if (verbose)
		printf("%s: tpsvrinit: initialization done\n", progname);

	return (0);
}

/* Called when the servers are shutdown.  This closes the database. */
void
tpsvrdone()
{
	if (db != NULL)
		(void)db->close(db, 0);

	tx_close();

	if (verbose)
		printf("%s: tpsvrdone: shutdown done\n", progname);
}

/* 
 * This function is called by the client.  Here Server 1 and Server 2 insert
 * data into a table using XA transactions.  */
void
TXN_FUNC(TPSVCINFO *msg)
{
	DBT data;
	DBT key;
	int ret, val;

        val = 1;

	memset(&key, 0, sizeof(key));
	key.data = &val;
	key.size = sizeof(val);
	memset(&data, 0, sizeof(data));
	data.data = &val;
	data.size = sizeof(val);

	/* Table 1. */
	if (verbose) {
		printf("put: key in %s: %i\n", val, TABLE);
		printf("put: data in %s: %i\n", val, TABLE);
	}
	if ((ret = db->put(db, NULL, &key, &data, 0)) != 0) {
		if (ret == DB_LOCK_DEADLOCK)
			goto abort;
		fprintf(stderr, "%s: %s: Table->put: %s\n",
		    progname, TXN_STRING, db_strerror(ret));
		goto err;
	}

        /* Sleep for 30 seconds to force a timeout error. */
#ifdef SERVER2
        sleep(30);
#endif

	tpreturn(TPSUCCESS, 0L, 0, 0L, 0);
        if (0) {
abort:		if (verbose)
			printf("%s: %s: abort\n", progname, TXN_STRING);
		tpreturn(TPSUCCESS, 1L, 0, 0L, 0);
	}
	return;
err:
	tpreturn(TPFAIL, 0L, 0, 0L, 0);
}

