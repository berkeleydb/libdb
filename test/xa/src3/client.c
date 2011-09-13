/*
 * This is the multithreaded test for XA.  The client creates several threads
 * and uses each thread to send requests to the servers, which are also
 * multithreaded.  There are two tests.  The first one runs the client with
 * two threads that sends requests to the servers then exit.  In the second
 * test the client creates 3 threads.  The first 2 execute the same as in
 * the first test, but the third thread calls the servers with a command
 * to kill that server.  This is done to test that the environment and
 * servers can recover from a thread failure.
 */
#include <sys/types.h>
#include <sys/time.h>

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>

#include <tx.h>
#include <atmi.h>
#include <fml32.h>
#include <fml1632.h>

#include <db.h>

#define	HOME	"../data"
#define	TABLE1	"../data/table1.db"
#define	TABLE2	"../data/table2.db"
#define NUM_SERVERS 3

#ifdef VERBOSE
static int verbose = 1;				/* Debugging output. */
#else
static int verbose = 0;
#endif
static int expected_error = 0;

char *progname;					/* Client run-time name. */

int   check_data(void);

int
usage()
{
	fprintf(stderr, "usage: %s [-v] [-k]\n", progname);
	return (EXIT_FAILURE);
}

static int thread_error = 1; 

/*
 * This function is called by the client threads.  Threads 1 and 2 randomly 
 * call each of the servers.  If thread 3 is created it calls one of the
 * servers and orders it to exit.
 */
void *
call_server_thread(void *server_name)
{
	FBFR *replyBuf;
	long replyLen;
	char *server_names[NUM_SERVERS];
	char *name, *thread_name, *kill_thread;
	int commit, j, iterations;
	void *result = NULL;
	TPINIT *initBuf = NULL;
	kill_thread = NULL;
	iterations = 100;
	replyBuf = NULL;

	/* Names of the function to call in the servers. */
	server_names[0] = "TestThread1";
	server_names[1] = "TestThread2";
	thread_name = (char *)server_name;
	
	/* Allocate init buffer */
	if ((initBuf = (TPINIT *)tpalloc("TPINIT", NULL, TPINITNEED(0))) == 0)
		goto tuxedo_err;
	initBuf->flags = TPMULTICONTEXTS;

	if (tpinit(initBuf) == -1)
		goto tuxedo_err;
	if (verbose)
	        printf("%s:%s: tpinit() OK\n", progname, thread_name);

	/* Create the command to kill the server. */
	if (strcmp(thread_name, "3") == 0) {
		kill_thread = (char *)tpalloc("STRING", NULL, 1);
		if (kill_thread == NULL)
		  	goto tuxedo_err;
		iterations = 1;
	} else if (expected_error)
		sleep(30);   

	for (j = 0; j < iterations; j++) {
	  	commit = 1;
		if (replyBuf != NULL)
			tpfree((char *)replyBuf);

		/* Randomly select a server. */
		name = server_names[j % 2];

		/* Allocate reply buffer. */
		replyLen = 1024;
		replyBuf = NULL;
		if ((replyBuf = (FBFR*)tpalloc("FML32", NULL, replyLen)) 
		    == NULL) 
			goto tuxedo_err;
		if (verbose)
		        printf("%s:%s: tpalloc(\"FML32\"), reply buffer OK\n", 
			    progname, thread_name);

		/* Begin the XA transaction. */
		if (tpbegin(60L, 0L) == -1)
			goto tuxedo_err;
		if (verbose)
		        printf("%s:%s: tpbegin() OK\n", progname, thread_name);
		/* Call the server to kill it. */
		if (kill_thread != NULL) {
		  	tpcall(name, kill_thread,  1L, (char **)&replyBuf, 
			    &replyLen, 0L);
			goto abort;
		} else {
		        if (tpcall(name, NULL, 0L, (char **)&replyBuf, 
			    &replyLen, TPSIGRSTRT) == -1) 
			  
			        /* 
				 * When one of the servers is killed TPNOENT or 
				 * TPESVCERR is an expected error.
				 */
			  if (expected_error && (tperrno == TPESVCERR || tperrno == TPENOENT || tperrno == TPETIME)) 
			                goto abort;
				else
			                goto tuxedo_err;
		}

		/* 
		 * Commit or abort the transaction depending the what the 
		 * server returns. 
		 */
		commit = !tpurcode;
		if (commit) {
commit:			if (verbose) {
			        printf("%s:%s: txn success\n", progname, 
				    thread_name);
			}
			if (tpcommit(0L) == -1) {
			  	if (expected_error && tperrno == TPETIME) 
			      	  	continue;
			  	else if (tperrno == TPEABORT)
			  	  	continue;
				else
				    	goto tuxedo_err;
			}
			if (verbose) {
				printf("%s:%s: tpcommit() OK\n", progname, 
				    thread_name);
			}
		} else {
abort:			if (verbose) {
				printf("%s:%s: txn failure\n", progname, 
				    thread_name);
			}
		  	if (tpabort(0L) == -1) {
			  	if (expected_error && tperrno == TPETIME) 
			    		continue;
			  	else
			  		goto tuxedo_err;
		  	}
			if (verbose) {
				printf("%s:%s: tpabort() OK\n", progname, 
				    thread_name);
			}
			if (strcmp(thread_name, "3") == 0) 
			  	break;
		}
	}

	if (0) {
tuxedo_err:	fprintf(stderr, "%s:%s: TUXEDO ERROR: %s (code %d)\n",
		    progname, thread_name, tpstrerror(tperrno), tperrno);
	  	result = (void *)&thread_error;
	}
end:	tpterm();
	if (verbose)
		printf("%s:%s: tpterm() OK\n", progname, thread_name);	

	if (replyBuf != NULL)
		tpfree((char *)replyBuf);
	if (initBuf != NULL)
		tpfree((char *)initBuf);
	if(kill_thread != NULL)
		tpfree((char *)kill_thread);	  

	return(result);
}

/*
 * Create the threads to call the servers, and check that data in the two
 * databases is identical.
 */
int
main(int argc, char* argv[])
{
  int ch, i, ret, num_threads;
	pthread_t threads[NUM_SERVERS];
	void *results = NULL;
	char *names[NUM_SERVERS];

	names[0] = "1";
	names[1] = "2";
	names[2] = "3";
	progname = argv[0];
	num_threads = 2;

	while ((ch = getopt(argc, argv, "n:vk")) != EOF)
		switch (ch) {
		case 'k':
		  	num_threads = 3;
			expected_error = 1;
			break;
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

	
	/* Create threads for different contexts*/
	for (i = 0; i < num_threads; i++) {
		if (verbose)
		       printf("calling server thread\n");
		ret = pthread_create(&threads[i], NULL, 
		    call_server_thread, names[i]);
		if (ret) {
		       fprintf(stderr, "%s: failed to create thread %s.\n",
			    progname, ret);
		       goto err;
		}
        }

	/* Wait for each thread to finish. */
	for (i = 0; i < num_threads; i++) {
		if ((ret = pthread_join(threads[i], &results)) != 0) {
			fprintf(stderr, "%s: failed to join thread %s.\n",
			    progname, ret);
			goto err;
		}
		if (results != NULL)
		 	goto err; 
	}	

	/* If the kill thread was not used, check the data in the two tables.*/
	if (num_threads < NUM_SERVERS)
	        ret = check_data();

	if (0) {
err:		ret = EXIT_FAILURE;
	}

	return (ret);
}

/*
 * check_data --
 *	Compare committed data in the two tables, should be identical.
 */
int
check_data()
{
	DB *dbp1, *dbp2;
	DBC *dbc1, *dbc2;
	DB_ENV *dbenv;
	DBT key1, data1, key2, data2;
	int ret, ret1, ret2;
	u_int32_t flags = DB_INIT_MPOOL | DB_INIT_LOG | DB_INIT_TXN |
	  DB_INIT_LOCK | DB_CREATE | DB_THREAD | DB_RECOVER | DB_REGISTER;
	char *home = HOME;

	dbp1 = dbp2 =  NULL;
	dbc1 = dbc2 = NULL;
	dbenv = NULL;

	/* Join the DB environment. */
	if ((ret = db_env_create(&dbenv, 0)) != 0 ||
	    (ret = dbenv->open(dbenv, home, flags, 0)) != 0) {
		fprintf(stderr,
		    "%s: %s: %s\n", progname, home, db_strerror(ret));
		goto err;
	}

	/* Open the tables. */
	if ((ret = db_create(&dbp1, dbenv, 0)) != 0 ||
	    (ret = db_create(&dbp2, dbenv, 0)) != 0 ||
	    ((ret = dbp1->open(
	    dbp1, NULL, TABLE1, NULL, DB_UNKNOWN, DB_RDONLY, 0)) != 0) ||
	    ((ret = dbp2->open(
	    dbp2, NULL, TABLE2, NULL, DB_UNKNOWN, DB_RDONLY, 0)) != 0)) {
		fprintf(stderr,
		    "%s: %s: %s\n", progname, TABLE1, db_strerror(ret));
		goto err;
	}
	if (verbose)
		printf("%s: opened tables OK\n", progname);

	/* Open cursors. */
	if ((ret = dbp1->cursor(dbp1, NULL, &dbc1, 0)) != 0 ||
	    (ret = dbp2->cursor(dbp2, NULL, &dbc2, 0)) != 0) {
		fprintf(stderr,
		    "%s: DB->cursor: %s\n", progname, db_strerror(ret));
		goto err;
	}
	if (verbose)
		printf("%s: opened cursors OK\n", progname);

	/* Compare the two databases, they should be identical. */
	memset(&key1, 0, sizeof(key1));
	memset(&data1, 0, sizeof(data1));
	memset(&key2, 0, sizeof(key2));
	memset(&data2, 0, sizeof(data2));
	for (;;) {
	  
		ret1 = dbc1->c_get(dbc1, &key1, &data1, DB_NEXT);
		ret2 = dbc2->c_get(dbc2, &key2, &data2, DB_NEXT);
		if (ret1 != 0 || ret2 != 0)
			break;
		if (key1.size != key2.size ||
		    memcmp(key1.data, key2.data, key1.size) != 0 ||
		    data1.size != data2.size ||
		    memcmp(data1.data, data2.data, data1.size) != 0)
			goto mismatch;
	}
	if (ret1 != DB_NOTFOUND || ret2 != DB_NOTFOUND) {
mismatch:	fprintf(stderr,
		    "%s: DB_ERROR: databases 1 and 2 weren't identical\n",
		    progname);
		ret = 1;
	}

err:	if (dbc1 != NULL)
		(void)dbc1->c_close(dbc1);
	if (dbc2 != NULL)
		(void)dbc2->c_close(dbc2);
	if (dbp1 != NULL)
		(void)dbp1->close(dbp1, 0);
	if (dbp2 != NULL)
		(void)dbp2->close(dbp2, 0);
	if (dbenv != NULL)
		(void)dbenv->close(dbenv, 0);

	return (ret == 0 ? EXIT_SUCCESS : EXIT_FAILURE);
}

