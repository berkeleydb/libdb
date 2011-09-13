/*
 * Multi-threaded servers that insert data into databases 1 and 2 at the
 * request of the client, and can die at the request of the client.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <atmi.h>

#include <tx.h>
#include <xa.h>

#include <db.h>

/*
 * The two servers are largely identical, #ifdef the source code.
 */
#ifdef SERVER1
#define	TXN_FUNC		TestThread1
#define	TXN_STRING		"TestThread1"
#endif
#ifdef SERVER2
#define	TXN_FUNC		TestThread2
#define	TXN_STRING		"TestThread2"
#endif
void TXN_FUNC(TPSVCINFO *);

#define	HOME	"../data"
#define	TABLE1	"table1.db"
#define	TABLE2	"table2.db"

#ifdef VERBOSE
static int verbose = 1;				/* Debugging output. */
#else
static int verbose = 0;
#endif

DB *db1, *db2;					/* Table handles. */

int cnt_request;				/* Total requests. */

char *progname;					/* Server run-time name. */

/* Called once when the server is started. Creates and opens the databases. */
int
tpsvrinit(int argc, char* argv[])
{
	int ret;
	int dbflags = DB_AUTO_COMMIT | DB_CREATE | DB_THREAD;

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

	/* Open XA database handles. */
	if ((ret = db_create(&db1, NULL, DB_XA_CREATE)) != 0) {
		fprintf(stderr, "db_create: %s\n", db_strerror(ret));
		return (-1);
	}
	db1->set_errfile(db1, stderr);
	if ((ret = db1->open(db1, NULL,
	    TABLE1, NULL, DB_BTREE, dbflags, 0660)) != 0) {
		fprintf(stderr, "DB open: %s: %s\n", TABLE1, db_strerror(ret));
		return (-1);
	}
	if ((ret = db_create(&db2, NULL, DB_XA_CREATE)) != 0) {
		fprintf(stderr, "db_create: %s\n", db_strerror(ret));
		return (-1);
	}
	db2->set_errfile(db2, stderr);
	if ((ret = db2->open(db2, NULL,
	    TABLE2, NULL, DB_BTREE, dbflags, 0660)) != 0) {
		fprintf(stderr, "DB open: %s: %s\n", TABLE2, db_strerror(ret));
		return (-1);
	}

	if (verbose)
		printf("%s: tpsvrinit: initialization done\n", progname);

	return (0);
}

/* Called once when the servers are shutdown.  Closes the databases. */
void
tpsvrdone()
 {
	if (verbose)
		printf("%s: tpsvrdone: shutdown done\n", progname);
	if (db1 != NULL)
	(void)db1->close(db1, 0);
	if (db2 != NULL)
	(void)db2->close(db2, 0);
	db1 = db2 = NULL;

	tx_close();

	if (verbose)
		printf("%s: tpsvrdone: shutdown done\n", progname);
}
/* 
 * Called by the client to insert data into the databases.  Also can kill this
 * thread if commanded to by the client.
 */
void
TXN_FUNC(TPSVCINFO *msg)
{
  	int ret, i, commit, key_value, data_value;
	DBT key, data;

	memset(&key, 0, sizeof key);
	memset(&data, 0, sizeof data);
	commit = 1;
	++cnt_request;

#ifdef SERVER1
	key_value = data_value = cnt_request + 1;
#else
	key_value = data_value = (rand() % 1000) + 1;
#endif
	data.data = &data_value;
	data.size = sizeof(data_value);
	key.data = &key_value;
	key.size = sizeof(key_value);

	/* Kill the server to see what happens. */
	if (msg->data != NULL) {
	  	pthread_exit(NULL);
	}

	/* Insert data into the tables. */
	if (verbose) {
		printf("put1: key: n");
		printf("put1: data:\n");
	}
	if ((ret = db1->put(db1, NULL, &key, &data, 0)) != 0) {
	        if (ret == DB_LOCK_DEADLOCK) 
			goto abort;
		fprintf(stderr, "%s: %s: Table1->put: %s\n",
		    progname, TXN_STRING, db_strerror(ret));
		goto err;
	}

	if ((ret = db2->put(db2, NULL, &key, &data, 0)) != 0) {
		if (ret == DB_LOCK_DEADLOCK)
			goto abort;
		fprintf(stderr, "%s: %s: Table2->put: %s\n",
		    progname, TXN_STRING, db_strerror(ret));
		goto err;
	}

	/* Returns a commit or abort command to the client. */
	if (verbose)
		printf("%s: %s: commit\n", progname, TXN_STRING);
	tpreturn(TPSUCCESS, 0L, 0, 0L, 0);
	if (0) {
abort:		if (verbose)
			printf("%s: %s: abort\n", progname, TXN_STRING);
		tpreturn(TPSUCCESS, 1L, 0, 0L, 0);
	}
	return;

err:	tpreturn(TPFAIL, 1L, 0, 0L, 0);
}

