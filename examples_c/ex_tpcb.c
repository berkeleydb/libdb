/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 *
 *	@(#)ex_tpcb.c	11.3 (Sleepycat) 11/3/99
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
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#endif

#ifdef _WIN32
#include <sys/types.h>
#include <sys/timeb.h>
#endif

#include <db.h>

typedef enum { ACCOUNT, BRANCH, TELLER } FTYPE;

DB_ENV	 *db_init(char *, int, int, int);
void	  hpopulate(DB *, int, int, int, int);
void	  invarg(int, char *);
int	  main(int, char *[]);
void	  populate(DB *, u_int32_t, u_int32_t, int, char *);
u_int32_t random_id(FTYPE, int, int, int);
u_int32_t random_int(u_int32_t, u_int32_t);
void	  tp_populate(DB_ENV *, int, int, int, int);
void	  tp_run(DB_ENV *, int, int, int, int);
int	  tp_txn(DB_ENV *, DB *, DB *, DB *, DB *, int, int, int);
void	  usage(void);

int verbose;
const char
	*progname = "ex_tpcb";				/* Program name. */

/*
 * This program implements a basic TPC/B driver program.  To create the
 * TPC/B database, run with the -i (init) flag.  The number of records
 * with which to populate the account, history, branch, and teller tables
 * is specified by the a, s, b, and t flags respectively.  To run a TPC/B
 * test, use the n flag to indicate a number of transactions to run (note
 * that you can run many of these processes in parallel to simulate a
 * multiuser test run).
 */
#define	TELLERS_PER_BRANCH	10
#define	ACCOUNTS_PER_TELLER	10000
#define	HISTORY_PER_BRANCH	2592000

/*
 * The default configuration that adheres to TPCB scaling rules requires
 * nearly 3 GB of space.  To avoid requiring that much space for testing,
 * we set the parameters much lower.  If you want to run a valid 10 TPS
 * configuration, define VALID_SCALING.
 */
#ifdef	VALID_SCALING
#define	ACCOUNTS	 1000000
#define	BRANCHES	      10
#define	TELLERS		     100
#define HISTORY		25920000
#endif

#ifdef	TINY
#define	ACCOUNTS	    1000
#define	BRANCHES	      10
#define	TELLERS		     100
#define HISTORY		   10000
#endif

#if !defined(VALID_SCALING) && !defined(TINY)
#define	ACCOUNTS	  100000
#define	BRANCHES	      10
#define	TELLERS		     100
#define HISTORY		  259200
#endif

#define HISTORY_LEN	    100
#define	RECLEN		    100
#define	BEGID		1000000

typedef struct _defrec {
	u_int32_t	id;
	u_int32_t	balance;
	u_int8_t	pad[RECLEN - sizeof(u_int32_t) - sizeof(u_int32_t)];
} defrec;

typedef struct _histrec {
	u_int32_t	aid;
	u_int32_t	bid;
	u_int32_t	tid;
	u_int32_t	amount;
	u_int8_t	pad[RECLEN - 4 * sizeof(u_int32_t)];
} histrec;

#ifdef _WIN32
/* Simulation of UNIX gettimeofday(2). */
struct timeval {
	long tv_sec;
	long tv_usec;
};

struct timezone {
	int tz_minuteswest;
	int tz_dsttime;
};

int
gettimeofday(tp, tzp)
	struct timeval *tp;
	struct timezone *tzp;
{
	struct _timeb tb;

	_ftime(&tb);
	if (tp != 0) {
		tp->tv_sec = tb.time;
		tp->tv_usec = tb.millitm * 1000;
	}
	if (tzp != 0) {
		tzp->tz_minuteswest = tb.timezone;
		tzp->tz_dsttime = tb.dstflag;
	}
	return (0);
}
#endif

int
main(argc, argv)
	int argc;
	char *argv[];
{
	extern char *optarg;
	extern int optind;
	DB_ENV *dbenv;
	u_int seed;
	int accounts, branches, tellers, history;
	int ch, iflag, mpool, ntxns, ret, txn_no_sync;
	char *home, *endarg;

	home = "TESTDIR";
	accounts = branches = history = tellers = 0;
	txn_no_sync = 0;
	mpool = ntxns = 0;
	verbose = 0;
	iflag = 0;
	seed = (u_int)getpid() | time(NULL);
	while ((ch = getopt(argc, argv, "a:b:c:fh:in:S:s:t:v")) != EOF)
		switch (ch) {
		case 'a':			/* Number of account records */
			if ((accounts = atoi(optarg)) <= 0)
				invarg(ch, optarg);
			break;
		case 'b':			/* Number of branch records */
			if ((branches = atoi(optarg)) <= 0)
				invarg(ch, optarg);
			break;
		case 'c':			/* Cachesize in bytes */
			if ((mpool = atoi(optarg)) <= 0)
				invarg(ch, optarg);
			break;
		case 'f':			/* Fast mode: no txn sync. */
			txn_no_sync = 1;
			break;
		case 'h':			/* DB  home. */
			home = optarg;
			break;
		case 'i':			/* Initialize the test. */
			iflag = 1;
			break;
		case 'n':			/* Number of transactions */
			if ((ntxns = atoi(optarg)) <= 0)
				invarg(ch, optarg);
			break;
		case 'S':			/* Random number seed. */
			seed = (u_int)strtoul(optarg, &endarg, 0);
			if (*endarg != '\0')
				invarg(ch, optarg);
			break;
		case 's':			/* Number of history records */
			if ((history = atoi(optarg)) <= 0)
				invarg(ch, optarg);
			break;
		case 't':			/* Number of teller records */
			if ((tellers = atoi(optarg)) <= 0)
				invarg(ch, optarg);
			break;
		case 'v':			/* Verbose option. */
			verbose = 1;
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	srand((u_int)seed);

	/* Initialize the database environment. */
	dbenv = db_init(home, mpool, iflag, txn_no_sync ? DB_TXN_NOSYNC : 0);

	accounts = accounts == 0 ? ACCOUNTS : accounts;
	branches = branches == 0 ? BRANCHES : branches;
	tellers = tellers == 0 ? TELLERS : tellers;
	history = history == 0 ? HISTORY : history;

	if (verbose)
		printf("%ld Accounts, %ld Branches, %ld Tellers, %ld History\n",
		    (long)accounts, (long)branches,
		    (long)tellers, (long)history);

	if (iflag) {
		if (ntxns != 0)
			usage();
		tp_populate(dbenv, accounts, branches, history, tellers);
	} else {
		if (ntxns == 0)
			usage();
		tp_run(dbenv, ntxns, accounts, branches, tellers);
	}

	if ((ret = dbenv->close(dbenv, 0)) != 0) {
		fprintf(stderr, "%s: %s\n", progname, db_strerror(ret));
		return (1);
	}

	return (0);
}

/*
 * db_init --
 *	Initialize the environment.
 */
DB_ENV *
db_init(home, cachesize, initializing, flags)
	char *home;
	int cachesize, initializing, flags;
{
	DB_ENV *dbenv;
	u_int32_t local_flags;
	int ret;

	if ((ret = db_env_create(&dbenv, 0)) != 0) {
		fprintf(stderr,
		    "%s: db_env_create: %s\n", progname, db_strerror(ret));
		exit (1);
	}
	dbenv->set_errfile(dbenv, stderr);
	dbenv->set_errpfx(dbenv, progname);
	(void)dbenv->set_cachesize(dbenv, 0,
	    cachesize == 0 ? 4 * 1024 * 1024 : (u_int32_t)cachesize, 0);

	local_flags = flags | DB_CREATE | (initializing ? DB_INIT_MPOOL :
	    DB_INIT_TXN | DB_INIT_LOCK | DB_INIT_LOG | DB_INIT_MPOOL);
	if ((ret = dbenv->open(dbenv, home, NULL, local_flags, 0)) != 0) {
		dbenv->err(dbenv, ret, "DBENV->open: %s", home);
		(void)dbenv->close(dbenv, 0);
		exit (1);
	}
	return (dbenv);
}

void
invarg(arg, str)
	int arg;
	char *str;
{
	(void)fprintf(stderr,
	    "%s: invalid argument for -%c: %s\n", progname, arg, str);
	exit (1);
}

void
usage()
{
	char *a1, *a2;

	a1 = "[-fv] [-a accounts] [-b branches]\n";
	a2 = "\t[-c cache_size] [-h home] [-S seed] [-s history] [-t tellers]";
	(void)fprintf(stderr, "usage: %s -i %s %s\n", progname, a1, a2);
	(void)fprintf(stderr,
	    "       %s -n transactions %s %s\n", progname, a1, a2);
	exit(1);
}

/*
 * Initialize the database to the specified number of accounts, branches,
 * history records, and tellers.
 */
void
tp_populate(env, accounts, branches, history, tellers)
	DB_ENV *env;
	int accounts, branches, history, tellers;
{
	DB *dbp;
	u_int32_t balance, idnum;
	u_int32_t end_anum, end_bnum, end_tnum;
	u_int32_t start_anum, start_bnum, start_tnum;
	int ret;

	idnum = BEGID;
	balance = 500000;

	if ((ret = db_create(&dbp, env, 0)) != 0) {
		env->err(env, ret, "db_create");
		exit (1);
	}
	(void)dbp->set_h_nelem(dbp, (u_int32_t)accounts);

	if ((ret = dbp->open(dbp, "account", NULL,
	    DB_HASH, DB_CREATE | DB_TRUNCATE, 0644)) != 0) {
		env->err(env, ret, "DB->open: account");
		exit (1);
	}

	start_anum = idnum;
	populate(dbp, idnum, balance, accounts, "account");
	idnum += accounts;
	end_anum = idnum - 1;
	if ((ret = dbp->close(dbp, 0)) != 0) {
		env->err(env, ret, "DB->close: account");
		exit (1);
	}
	if (verbose)
		printf("Populated accounts: %ld - %ld\n",
		    (long)start_anum, (long)end_anum);

	/*
	 * Since the number of branches is very small, we want to use very
	 * small pages and only 1 key per page, i.e., key-locking instead
	 * of page locking.
	 */
	if ((ret = db_create(&dbp, env, 0)) != 0) {
		env->err(env, ret, "db_create");
		exit (1);
	}
	(void)dbp->set_h_ffactor(dbp, 1);
	(void)dbp->set_h_nelem(dbp, (u_int32_t)branches);
	(void)dbp->set_pagesize(dbp, 512);
	if ((ret = dbp->open(dbp, "branch", NULL,
	    DB_HASH, DB_CREATE | DB_TRUNCATE, 0644)) != 0) {
		env->err(env, ret, "DB->open: branch");
		exit (1);
	}
	start_bnum = idnum;
	populate(dbp, idnum, balance, branches, "branch");
	idnum += branches;
	end_bnum = idnum - 1;
	if ((ret = dbp->close(dbp, 0)) != 0) {
		env->err(env, ret, "DB->close: branch");
		exit (1);
	}
	if (verbose)
		printf("Populated branches: %ld - %ld\n",
		    (long)start_bnum, (long)end_bnum);

	/*
	 * In the case of tellers, we also want small pages, but we'll let
	 * the fill factor dynamically adjust itself.
	 */
	if ((ret = db_create(&dbp, env, 0)) != 0) {
		env->err(env, ret, "db_create");
		exit (1);
	}
	(void)dbp->set_h_ffactor(dbp, 0);
	(void)dbp->set_h_nelem(dbp, (u_int32_t)tellers);
	(void)dbp->set_pagesize(dbp, 512);
	if ((ret = dbp->open(dbp, "teller", NULL,
	    DB_HASH, DB_CREATE | DB_TRUNCATE, 0644)) != 0) {
		env->err(env, ret, "DB->open: teller");
		exit (1);
	}

	start_tnum = idnum;
	populate(dbp, idnum, balance, tellers, "teller");
	idnum += tellers;
	end_tnum = idnum - 1;
	if ((ret = dbp->close(dbp, 0)) != 0) {
		env->err(env, ret, "DB->close: teller");
		exit (1);
	}
	if (verbose)
		printf("Populated tellers: %ld - %ld\n",
		    (long)start_tnum, (long)end_tnum);

	if ((ret = db_create(&dbp, env, 0)) != 0) {
		env->err(env, ret, "db_create");
		exit (1);
	}
	(void)dbp->set_re_len(dbp, HISTORY_LEN);
	if ((ret = dbp->open(dbp, "history", NULL,
	    DB_RECNO, DB_CREATE | DB_TRUNCATE, 0644)) != 0) {
		env->err(env, ret, "DB->open: history");
		exit (1);
	}

	hpopulate(dbp, history, accounts, branches, tellers);
	if ((ret = dbp->close(dbp, 0)) != 0) {
		env->err(env, ret, "DB->close: history");
		exit (1);
	}
}

void
populate(dbp, start_id, balance, nrecs, msg)
	DB *dbp;
	u_int32_t start_id, balance;
	int nrecs;
	char *msg;
{
	DBT kdbt, ddbt;
	defrec drec;
	int i, ret;

	kdbt.flags = 0;
	kdbt.data = &drec.id;
	kdbt.size = sizeof(u_int32_t);
	ddbt.flags = 0;
	ddbt.data = &drec;
	ddbt.size = sizeof(drec);
	memset(&drec.pad[0], 1, sizeof(drec.pad));

	for (i = 0; i < nrecs; i++) {
		drec.id = start_id + (u_int32_t)i;
		drec.balance = balance;
		if ((ret =
		    (dbp->put)(dbp, NULL, &kdbt, &ddbt, DB_NOOVERWRITE)) != 0) {
			fprintf(stderr, "%s: Failure initializing %s file\n",
			    progname, msg);
			exit (1);
		}
	}
}

void
hpopulate(dbp, history, accounts, branches, tellers)
	DB *dbp;
	int history, accounts, branches, tellers;
{
	DBT kdbt, ddbt;
	histrec hrec;
	db_recno_t key;
	int i, ret;

	memset(&kdbt, 0, sizeof(kdbt));
	memset(&ddbt, 0, sizeof(ddbt));
	ddbt.data = &hrec;
	ddbt.size = sizeof(hrec);
	kdbt.data = &key;
	kdbt.size = sizeof(key);
	memset(&hrec.pad[0], 1, sizeof(hrec.pad));
	hrec.amount = 10;

	for (i = 1; i <= history; i++) {
		hrec.aid = random_id(ACCOUNT, accounts, branches, tellers);
		hrec.bid = random_id(BRANCH, accounts, branches, tellers);
		hrec.tid = random_id(TELLER, accounts, branches, tellers);
		if ((ret = dbp->put(dbp, NULL, &kdbt, &ddbt, DB_APPEND)) != 0) {
			dbp->err(dbp, ret, "dbp->put");
			exit (1);
		}
	}
}

u_int32_t
random_int(lo, hi)
	u_int32_t lo, hi;
{
	u_int32_t ret;
	int t;

	t = rand();
	ret = (u_int32_t)(((double)t / ((double)(RAND_MAX) + 1)) *
	    (hi - lo + 1));
	ret += lo;
	return (ret);
}

u_int32_t
random_id(type, accounts, branches, tellers)
	FTYPE type;
	int accounts, branches, tellers;
{
	u_int32_t min, max, num;

	max = min = BEGID;
	num = accounts;
	switch(type) {
	case TELLER:
		min += branches;
		num = tellers;
		/* FALLTHROUGH */
	case BRANCH:
		if (type == BRANCH)
			num = branches;
		min += accounts;
		/* FALLTHROUGH */
	case ACCOUNT:
		max = min + num - 1;
	}
	return (random_int(min, max));
}

void
tp_run(dbenv, n, accounts, branches, tellers)
	DB_ENV *dbenv;
	int n, accounts, branches, tellers;
{
	DB *adb, *bdb, *hdb, *tdb;
	double gtps, itps;
	int failed, ifailed, ret, txns, gus, ius;
	struct timeval starttime, curtime, lasttime;
#ifndef _WIN32
	pid_t pid;

	pid = getpid();
#else
	int pid;

	pid = 0;
#endif

	/*
	 * Open the database files.
	 */
	if ((ret = db_create(&adb, dbenv, 0)) != 0) {
		dbenv->err(dbenv, ret, "db_create");
		exit (1);
	}
	if ((ret = adb->open(adb, "account", NULL, DB_UNKNOWN, 0, 0)) != 0) {
		dbenv->err(dbenv, ret, "DB->open: account");
		exit (1);
	}

	if ((ret = db_create(&bdb, dbenv, 0)) != 0) {
		dbenv->err(dbenv, ret, "db_create");
		exit (1);
	}
	if ((ret = bdb->open(bdb, "branch", NULL, DB_UNKNOWN, 0, 0)) != 0) {
		dbenv->err(dbenv, ret, "DB->open: branch");
		exit (1);
	}

	if ((ret = db_create(&tdb, dbenv, 0)) != 0) {
		dbenv->err(dbenv, ret, "db_create");
		exit (1);
	}
	if ((ret = tdb->open(tdb, "teller", NULL, DB_UNKNOWN, 0, 0)) != 0) {
		dbenv->err(dbenv, ret, "DB->open: teller");
		exit (1);
	}

	if ((ret = db_create(&hdb, dbenv, 0)) != 0) {
		dbenv->err(dbenv, ret, "db_create");
		exit (1);
	}
	if ((ret = hdb->open(hdb, "history", NULL, DB_UNKNOWN, 0, 0)) != 0) {
		dbenv->err(dbenv, ret, "DB->open: history");
		exit (1);
	}

	txns = failed = ifailed = 0;
	(void)gettimeofday(&starttime, NULL);
	lasttime = starttime;
	while (n-- >= 0) {
		txns++;
		ret = tp_txn(dbenv,
		    adb, bdb, tdb, hdb, accounts, branches, tellers);
		if (ret != 0) {
			failed++;
			ifailed++;
		}
		if (n % 1000 == 0) {
			(void)gettimeofday(&curtime, NULL);
			gus = curtime.tv_usec >= starttime.tv_usec ?
			    curtime.tv_usec - starttime.tv_usec +
			    1000000 * (curtime.tv_sec - starttime.tv_sec) :
			    1000000 + curtime.tv_usec - starttime.tv_usec +
			    1000000 * (curtime.tv_sec - starttime.tv_sec - 1);
			ius = curtime.tv_usec >= lasttime.tv_usec ?
			    curtime.tv_usec - lasttime.tv_usec +
			    1000000 * (curtime.tv_sec - lasttime.tv_sec) :
			    1000000 + curtime.tv_usec - lasttime.tv_usec +
			    1000000 * (curtime.tv_sec - lasttime.tv_sec - 1);
			gtps = (double)(txns - failed) /
			    ((double)gus / 1000000);
			itps = (double)(1000 - ifailed) /
			    ((double)ius / 1000000);
			printf("[%d] %d txns %d failed ", (int)pid,
			    txns, failed);
			printf("%6.2f TPS (gross) %6.2f TPS (interval)\n",
			   gtps, itps);
			lasttime = curtime;
			ifailed = 0;
		}
	}

	(void)adb->close(adb, 0);
	(void)bdb->close(bdb, 0);
	(void)tdb->close(tdb, 0);
	(void)hdb->close(hdb, 0);

	printf("%ld transactions begun %ld failed\n", (long)txns, (long)failed);
}

/*
 * XXX Figure out the appropriate way to pick out IDs.
 */
int
tp_txn(dbenv, adb, bdb, tdb, hdb, accounts, branches, tellers)
	DB_ENV *dbenv;
	DB *adb, *bdb, *tdb, *hdb;
	int accounts, branches, tellers;
{
	DBC *acurs, *bcurs, *tcurs;
	DBT d_dbt, d_histdbt, k_dbt, k_histdbt;
	DB_TXN *t;
	db_recno_t key;
	defrec rec;
	histrec hrec;
	int account, branch, teller;

	t = NULL;
	acurs = bcurs = tcurs = NULL;

	/*
	 * XXX We could move a lot of this into the driver to make this
	 * faster.
	 */
	account = random_id(ACCOUNT, accounts, branches, tellers);
	branch = random_id(BRANCH, accounts, branches, tellers);
	teller = random_id(TELLER, accounts, branches, tellers);

	memset(&d_histdbt, 0, sizeof(d_histdbt));

	memset(&k_histdbt, 0, sizeof(k_histdbt));
	k_histdbt.data = &key;
	k_histdbt.size = sizeof(key);

	memset(&k_dbt, 0, sizeof(k_dbt));
	k_dbt.size = sizeof(int);

	memset(&d_dbt, 0, sizeof(d_dbt));
	d_dbt.flags = DB_DBT_USERMEM;
	d_dbt.data = &rec;
	d_dbt.ulen = sizeof(rec);

	hrec.aid = account;
	hrec.bid = branch;
	hrec.tid = teller;
	hrec.amount = 10;
	/* Request 0 bytes since we're just positioning. */
	d_histdbt.flags = DB_DBT_PARTIAL;

	/* START TIMING */
	if (txn_begin(dbenv, NULL, &t, 0) != 0)
		goto err;

	if (adb->cursor(adb, t, &acurs, 0) != 0 ||
	    bdb->cursor(bdb, t, &bcurs, 0) != 0 ||
	    tdb->cursor(tdb, t, &tcurs, 0) != 0)
		goto err;

	/* Account record */
	k_dbt.data = &account;
	if (acurs->c_get(acurs, &k_dbt, &d_dbt, DB_SET) != 0)
		goto err;
	rec.balance += 10;
	if (acurs->c_put(acurs, &k_dbt, &d_dbt, DB_CURRENT) != 0)
		goto err;

	/* Branch record */
	k_dbt.data = &branch;
	if (bcurs->c_get(bcurs, &k_dbt, &d_dbt, DB_SET) != 0)
		goto err;
	rec.balance += 10;
	if (bcurs->c_put(bcurs, &k_dbt, &d_dbt, DB_CURRENT) != 0)
		goto err;

	/* Teller record */
	k_dbt.data = &teller;
	if (tcurs->c_get(tcurs, &k_dbt, &d_dbt, DB_SET) != 0)
		goto err;
	rec.balance += 10;
	if (tcurs->c_put(tcurs, &k_dbt, &d_dbt, DB_CURRENT) != 0)
		goto err;

	/* History record */
	d_histdbt.flags = 0;
	d_histdbt.data = &hrec;
	d_histdbt.ulen = sizeof(hrec);
	if (hdb->put(hdb, t, &k_histdbt, &d_histdbt, DB_APPEND) != 0)
		goto err;

	if (acurs->c_close(acurs) != 0 || bcurs->c_close(bcurs) != 0 ||
	    tcurs->c_close(tcurs) != 0)
		goto err;

	if (txn_commit(t, 0) != 0)
		goto err;

	/* END TIMING */
	return (0);

err:	if (acurs != NULL)
		(void)acurs->c_close(acurs);
	if (bcurs != NULL)
		(void)bcurs->c_close(bcurs);
	if (tcurs != NULL)
		(void)tcurs->c_close(tcurs);
	if (t != NULL)
		(void)txn_abort(t);

	if (verbose)
		printf("Transaction A=%ld B=%ld T=%ld failed\n",
		    (long)account, (long)branch, (long)teller);
	return (-1);
}
