/*
 * This is the code for the two servers used in the first XA test.  Server 1
 * and Server 2 are both called by the client to insert data into table 1
 * using XA transactions.  Server 1 also can forward requests to Server 2.
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

#include "datafml.h"
#include "hdbrec.h"
#include "htimestampxa.h"

/*
 * The two servers are largely identical, #ifdef the source code.
 */
#ifdef SERVER1
#define	TXN_FUNC		TestTxn1
#define	TXN_STRING		"TestTxn1"
#endif
#ifdef SERVER2
#define	TXN_FUNC		TestTxn2
#define	TXN_STRING		"TestTxn2"
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

int cnt_forward;				/* Forwarded requests. */
int cnt_request;				/* Total requests. */

char *progname;					/* Server run-time name. */

char *db_buf(DBT *);

/*
 * Called when each server is started.  It creates and opens the
 * two databases.
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
	if ((ret = db_create(&db1, NULL, DB_XA_CREATE)) != 0) {
		fprintf(stderr, "db_create: %s\n", db_strerror(ret));
		return (-1);
	}
	db1->set_errfile(db1, stderr);
	if ((ret = db1->open(db1, NULL,
	    TABLE1, NULL, DB_BTREE, DB_AUTO_COMMIT | DB_CREATE, 0660)) != 0) {
		fprintf(stderr, "DB open: %s: %s\n", TABLE1, db_strerror(ret));
		return (-1);
	}
	if ((ret = db_create(&db2, NULL, DB_XA_CREATE)) != 0) {
		fprintf(stderr, "db_create: %s\n", db_strerror(ret));
		return (-1);
	}
	db2->set_errfile(db2, stderr);
	if ((ret = db2->open(db2, NULL,
	    TABLE2, NULL, DB_BTREE, DB_AUTO_COMMIT | DB_CREATE, 0660)) != 0) {
		fprintf(stderr, "DB open: %s: %s\n", TABLE2, db_strerror(ret));
		return (-1);
	}

	if (verbose)
		printf("%s: tpsvrinit: initialization done\n", progname);

	return (0);
}

/* Called when the servers are shutdown.  This closes the databases. */
void
tpsvrdone()
{
	if (db1 != NULL)
		(void)db1->close(db1, 0);
	if (db2 != NULL)
		(void)db2->close(db2, 0);

	tx_close();

	if (verbose)
		printf("%s: tpsvrdone: shutdown done\n", progname);

	printf("%s: %d requests, %d requests forwarded to the other server\n",
	    progname, cnt_request, cnt_forward);
}

/* 
 * This function is called by the client.  Here Server 1 and Server 2 insert
 * data into table 1 using XA transactions.  Server 1 can also forward its 
 * request to Server 2.
 */
void
TXN_FUNC(TPSVCINFO *msg)
{
	DBT data;
	DBT key;
	FBFR *replyBuf;
	HDbRec rcrd;
	long replyLen, seqNo;
	int ret;

	++cnt_request;

#ifdef SERVER1
	/*
	 * Test that servers can forward to other servers.  Randomly forward
	 * half of server #1's requests to server #2.
	 */
	if (rand() % 2 > 0) {
		++cnt_forward;

		replyLen = 1024;
		if ((replyBuf =
		    (FBFR*)tpalloc("FML32", NULL, replyLen)) == NULL ||
		    tpcall("TestTxn2", msg->data,
		    0, (char**)&replyBuf, &replyLen, TPSIGRSTRT) == -1) {
			fprintf(stderr, "%s: TUXEDO ERROR: %s (code %d)\n",
			    progname, tpstrerror(tperrno), tperrno);
			tpfree((char*)replyBuf);
			tpreturn(TPFAIL, 0L, 0, 0L, 0);
		} else {
			tpfree((char*)replyBuf);
			tpreturn(TPSUCCESS, tpurcode, 0, 0L, 0);
		}
		return;
	}
#endif
						/* Read the record. */
	if (Fget((FBFR*)msg->data, SEQ_NO, 0, (char *)&rcrd.SeqNo, 0) == -1)
		goto fml_err;
	if (Fget((FBFR*)msg->data, TS_SEC, 0, (char *)&rcrd.Ts.Sec, 0) == -1)
		goto fml_err;
	if (Fget(
	    (FBFR*)msg->data, TS_USEC, 0, (char *)&rcrd.Ts.Usec, 0) == -1) {
fml_err:	fprintf(stderr, "%s: FML ERROR: %s (code %d)\n",
		    progname, Fstrerror(Ferror), Ferror);
		goto err;
	}

	seqNo = rcrd.SeqNo;			/* Update the record. */
	memset(&key, 0, sizeof(key));
	key.data = &seqNo;
	key.size = sizeof(seqNo);
	memset(&data, 0, sizeof(data));
	data.data = &rcrd;
	data.size = sizeof(rcrd);

	strcpy(rcrd.Msg, "Table1");		/* Table 1. */
	if (verbose) {
		printf("put1: key: %s\n", db_buf(&key));
		printf("put1: data: %s\n", db_buf(&data));
	}
	if ((ret = db1->put(db1, NULL, &key, &data, 0)) != 0) {
		if (ret == DB_LOCK_DEADLOCK)
			goto abort;
		fprintf(stderr, "%s: %s: Table1->put: %s\n",
		    progname, TXN_STRING, db_strerror(ret));
		goto err;
	}

	strcpy(rcrd.Msg, "Table2");		/* Table 2. */
	if ((ret = db2->put(db2, NULL, &key, &data, 0)) != 0) {
		if (ret == DB_LOCK_DEADLOCK)
			goto abort;
		fprintf(stderr, "%s: %s: Table2->put: %s\n",
		    progname, TXN_STRING, db_strerror(ret));
		goto err;
	}

	/*
	 * Decide if the client is going to commit the global transaction or
	 * not, testing the return-value path back to the client; this is the
	 * path we'd use to resolve deadlock, for example.  Commit 80% of the
	 * time.  Returning 0 causes the client to commit, 1 to abort.
	 */
	if (rand() % 10 > 7) {
		if (verbose)
			printf("%s: %s: commit\n", progname, TXN_STRING);
		tpreturn(TPSUCCESS, 0L, 0, 0L, 0);
	} else {
abort:		if (verbose)
			printf("%s: %s: abort\n", progname, TXN_STRING);
		tpreturn(TPSUCCESS, 1L, 0, 0L, 0);
	}
	return;

err:	tpreturn(TPFAIL, 1L, 0, 0L, 0);
}

char *
db_buf(dbt)
	DBT *dbt;
{
	static u_char buf[1024];
	size_t len;
	u_char *p, *b;

	for (p = dbt->data, len = dbt->size, b = buf; len > 0; ++p, --len)
		if (isprint(*p))
			b += sprintf((char *)b, "%c", *p);
		else
			b += sprintf((char *)b, "%#o", *p);
	return ((char *)buf);
}
