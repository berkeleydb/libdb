/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2001, 2010 Oracle and/or its affiliates.  All rights reserved. 
 *
 * $Id$
 */

/*
 * bench_001 - time bulk fetch/insert/delete interface.
 *	Without -R builds a btree acording to the arguments.
 *	With -R runs and times bulk fetches.  If -d is specified
 *	during reads the DB_MULTIPLE interface is used
 *	otherwise the DB_MULTIPLE_KEY interface is used.
 *
 * ARGUMENTS:
 *	-c	cachesize [1000 * pagesize]
 *	-d	number of duplicates [none]
 *	-E	don't use environment
 *	-I	Just initialize the environment
 *	-i	number of read iterations [1000000]
 *	-l	length of data item [20]
 *	-n	number of keys [1000000]
 *	-p	pagesize [65536]
 *	-R	perform read test.
 *	-T	incorporate transactions.
 *	-B      perform bulk updates.
 *	-D      perform bulk deletes.
 *
 * COMPILE:
 *	cc -I /usr/local/BerkeleyDB/include \
 *	    -o bench_001 -O2 bench_001.c /usr/local/BerkeleyDB/lib/libdb.so
 */
#include <sys/types.h>
#include <sys/time.h>

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
extern int getopt(int, char * const *, const char *);
#else
#include <unistd.h>
#endif

#include <db.h>

#define	DATABASE	"bench_001.db"

int	bulk_delete(DB_ENV *, DB *, int, u_int, int, int);
int	bulk_fill(DB_ENV *, DB *, int, u_int, int, int, int *, int *);
int	compare_int(DB *, const DBT *, const DBT *);
DB_ENV *db_init(char *, char *, u_int, int);
int	fill(DB_ENV *, DB *, int, u_int, int, int, int *);
int	get(DB *, int, u_int, int, int, int, int *);
int	main(int, char *[]);
void	usage(void);

const char
	*progname = "bench_001";		/* Program name. */
/*
 * db_init --
 *	Initialize the environment.
 */
DB_ENV *
db_init(home, prefix, cachesize, txn)
	char *home, *prefix;
	u_int cachesize;
	int txn;
{
	DB_ENV *dbenv;
	u_int32_t flags;
	int ret;

	if ((ret = db_env_create(&dbenv, 0)) != 0) {
		dbenv->err(dbenv, ret, "db_env_create");
		return (NULL);
	}
	dbenv->set_errfile(dbenv, stderr);
	dbenv->set_errpfx(dbenv, prefix);
	(void)dbenv->set_cachesize(dbenv, 0,
	    cachesize == 0 ? 50 * 1024 * 1024 : (u_int32_t)cachesize, 0);

	flags = DB_CREATE | DB_INIT_MPOOL;
	if (txn)
		flags |= DB_INIT_TXN | DB_INIT_LOCK;
	if ((ret = dbenv->open(dbenv, home, flags, 0)) != 0) {
		dbenv->err(dbenv, ret, "DB_ENV->open: %s", home);
		(void)dbenv->close(dbenv, 0);
		return (NULL);
	}
	return (dbenv);
}

/*
 * get -- loop getting batches of records.
 *
 */
int
get(dbp, txn, datalen, num, dups, iter, countp)
	DB *dbp;
	u_int datalen;
	int txn, num, dups, iter, *countp;
{
	DBC *dbcp;
	DBT key, data;
	DB_ENV *dbenv;
	DB_TXN *txnp;
	u_int32_t flags, len, klen;
	int count, i, j, ret;
	void *pointer, *dp, *kp;

	dbenv = dbp->dbenv;

	klen = 0;				/* Lint. */
	klen = klen;

	memset(&key, 0, sizeof(key));
	key.data = &j;
	key.size = sizeof(j);
	memset(&data, 0, sizeof(data));
	data.flags = DB_DBT_USERMEM;
	data.data = malloc(datalen*1024*1024);
	data.ulen = data.size = datalen*1024*1024;

	count = 0;
	flags = DB_SET;
	if (!dups)
		flags |= DB_MULTIPLE_KEY;
	else
		flags |= DB_MULTIPLE;
	for (i = 0; i < iter; i++) {
		txnp = NULL;
		if (txn)
			if ((ret =
			    dbenv->txn_begin(dbenv, NULL, &txnp, 0)) != 0)
				goto err;
		if ((ret = dbp->cursor(dbp, txnp, &dbcp, 0)) != 0)
			goto err;

		j = random() % num;
		if ((ret = dbcp->get(dbcp, &key, &data, flags)) != 0)
			goto err;
		DB_MULTIPLE_INIT(pointer, &data);
		if (dups)
			while (pointer != NULL) {
				DB_MULTIPLE_NEXT(pointer, &data, dp, len);
				if (dp != NULL)
					count++;
			}
		else
			while (pointer != NULL) {
				DB_MULTIPLE_KEY_NEXT(pointer,
				    &data, kp, klen, dp, len);
				if (kp != NULL)
					count++;
			}
		if ((ret = dbcp->close(dbcp)) != 0)
			goto err;
		if (txn)
			if ((ret = txnp->commit(txnp, 0)) != 0)
				goto err;
	}

	*countp = count;
	return (0);

err:	dbp->err(dbp, ret, "get");
	return (ret);
}

/*
 * fill - fill a db
 *	Since we open/created the db with transactions (potentially),
 * we need to populate it with transactions.  We'll bundle the puts
 * 10 to a transaction.
 */
#define	PUTS_PER_TXN	10
int
fill(dbenv, dbp, txn, datalen, num, dups, countp)
	DB_ENV *dbenv;
	DB *dbp;
	u_int datalen;
	int txn, num, dups;
	int *countp;
	
{
	DBT key, data;
	DB_TXN *txnp;
	struct data {
		int id;
		char str[1];
	} *data_val;
	int count;
	int i, ret;

	/*
	 * Insert records into the database, where the key is the user
	 * input and the data is the user input in reverse order.
	 */
	txnp = NULL;
	ret = 0;
	count = 0;
	memset(&key, 0, sizeof(DBT));
	memset(&data, 0, sizeof(DBT));
	key.data = &i;
	key.size = sizeof(i);
	data.data = data_val = malloc(datalen);
	memcpy(data_val->str, "0123456789012345678901234567890123456789",
	    datalen - sizeof(data_val->id));
	data.size = datalen;
	data.flags = DB_DBT_USERMEM;

	for (i = 0; i < num; i++) {
		if (txn != 0 && i % PUTS_PER_TXN == 0) {
			if (txnp != NULL) {
				ret = txnp->commit(txnp, 0);
				txnp = NULL;
				if (ret != 0)
					goto err;
			}
			if ((ret =
			    dbenv->txn_begin(dbenv, NULL, &txnp, 0)) != 0)
				goto err;
		}
		data_val->id = 0;
		do {
			switch (ret = dbp->put(dbp, txnp, &key, &data, 0)) {
			case 0:
				count++;
				break;
			default:
				dbp->err(dbp, ret, "DB->put");
				goto err;
			}
		} while (++data_val->id < dups);
	}
	if (txnp != NULL)
		ret = txnp->commit(txnp, 0);

	printf("%d\n", count);
	*countp = count;
	return (ret);

err:	if (txnp != NULL)
		(void)txnp->abort(txnp);
	return (ret);
}

/*
 * bulk_fill - bulk_fill a db
 *	Since we open/created the db with transactions (potentially),
 * we need to populate it with transactions.  We'll bundle the puts
 * UPDATES_PER_BULK_PUT to a transaction.
 */
#define	UPDATES_PER_BULK_PUT	100
int
bulk_fill(dbenv, dbp, txn, datalen, num, dups, countp, iterp)
	DB_ENV *dbenv;
	DB *dbp;
	u_int datalen;
	int txn, num, dups;
	int *countp, *iterp;
{
	DBT key, data;
	u_int32_t flag = 0;
	DB_TXN *txnp;
	struct data {
		int id;
		char str[1];
	} *data_val;
	int count, i, iter, ret;
	void *ptrk, *ptrd;

	/*
	 * Insert records into the database, where the key is the user
	 * input and the data is the user input in reverse order.
	 */
	txnp = NULL;
	ret = 0;
	count = 0;
	iter = 0;
	ptrk = ptrd = NULL;

	data_val = malloc(datalen);
	memcpy(data_val->str, "0123456789012345678901234567890123456789",
		datalen - sizeof(data_val->id));

	memset(&key, 0, sizeof(DBT));
	memset(&data, 0, sizeof(DBT));

	/* 
	 * Need to account for proper buffer size,
	 * The buffer must be at least as large as the page size of
	 * the underlying database, aligned for unsigned integer
	 * access, and be a multiple of 1024 bytes in size.
	 */
	key.ulen = (u_int32_t)UPDATES_PER_BULK_PUT * 
		sizeof(u_int32_t) * datalen * 1024;
	key.flags = DB_DBT_USERMEM | DB_DBT_BULK;
	key.data = malloc(key.ulen);

	data.ulen = (u_int32_t)UPDATES_PER_BULK_PUT * 
		(u_int32_t)datalen * 1024;
	data.flags = DB_DBT_USERMEM | DB_DBT_BULK;
	data.data = malloc(data.ulen);


	if (dups)
		flag |= DB_MULTIPLE;
	else
		flag |= DB_MULTIPLE_KEY;

	DB_MULTIPLE_WRITE_INIT(ptrk, &key);
	if (dups)
		DB_MULTIPLE_WRITE_INIT(ptrd, &data);

	for (i = 0; i < num; i++) {
		if (txn != 0 && (i+1) % UPDATES_PER_BULK_PUT == 0) {
			if (txnp != NULL) {
				ret = txnp->commit(txnp, 0);
				txnp = NULL;
				if (ret != 0)
					goto err;
			}
			if ((ret =
				dbenv->txn_begin(dbenv, NULL, &txnp, 0)) != 0)
				goto err;
		}
		data_val->id = 0;
		do {
			if (dups) {
				DB_MULTIPLE_WRITE_NEXT(ptrk, &key, 
					&i, sizeof(i));
				assert(ptrk != NULL);
				DB_MULTIPLE_WRITE_NEXT(ptrd, &data, 
					data_val, datalen);
				assert(ptrd != NULL);
			}
			else {
				DB_MULTIPLE_KEY_WRITE_NEXT(ptrk, 
				    &key, &i, sizeof(i), data_val, datalen);
				assert(ptrk != NULL);
			}
		if ((i+1) % UPDATES_PER_BULK_PUT == 0) {
			switch (ret = dbp->put(dbp, txnp, &key, &data, flag)) {
				case 0:
					iter++;
					free (key.data);
					free (data.data);

					memset(&key, 0, sizeof(DBT));
					memset(&data, 0, sizeof(DBT));

					key.ulen = 
					(u_int32_t)UPDATES_PER_BULK_PUT *
					sizeof(u_int32_t) * datalen * 1024;
					key.flags = DB_DBT_USERMEM|DB_DBT_BULK;
					key.data = malloc(key.ulen);

					data.ulen = 
					(u_int32_t)UPDATES_PER_BULK_PUT * 
					(u_int32_t)datalen * 1024;
					data.flags = 
					    DB_DBT_USERMEM|DB_DBT_BULK;
					data.data = malloc(data.ulen);

					DB_MULTIPLE_WRITE_INIT(ptrk, &key);
					if (dups)
					DB_MULTIPLE_WRITE_INIT(ptrd, &data);
					break;
				default:
					dbp->err(dbp, ret, "Bulk DB->put");
					free (key.data);
					free (data.data);
					goto err;
				} 
			} 
			count++;
		} while (++data_val->id < dups);
	} 
	if ((num % UPDATES_PER_BULK_PUT) != 0) {
		switch (ret = dbp->put(dbp, txnp, &key, &data, flag)) {
			case 0:
				iter++;
				free (key.data);
				free (data.data);
				break;
			default:
				free (key.data);
				free (data.data);
				dbp->err(dbp, ret, "Bulk DB->put");
				goto err;
		}
	}

	if (txnp != NULL)
		ret = txnp->commit(txnp, 0);

	printf("%d\n", count);
	*countp = count;
	*iterp = iter;

	return (ret);

err:	if (txnp != NULL)
		(void)txnp->abort(txnp);
	return (ret);
}

/*
 * bulk_delete - bulk_delete from a db
 *	Since we open/created the db with transactions (potentially),
 * we need to delete from it with transactions.  We'll bundle the deletes
 * UPDATES_PER_BULK_PUT to a transaction.
 */
int
bulk_delete(dbenv, dbp, txn, datalen, num, dups)
	DB_ENV *dbenv;
	DB *dbp;
	u_int datalen;
	int txn, num, dups;
{
	DBT key;
	u_int32_t flag = 0;
	DB_TXN *txnp;
	struct data {
		int id;
		char str[1];
	} *data_val;
	int count, i, j, iter, ret;
	void *ptrk;

	txnp = NULL;
	ret = 0;
	count = 0;
	iter = 0;
	j = random() % num;

	memset(&key, 0, sizeof(DBT));

	/*
	 * Need to account for proper buffer size,
	 * The buffer must be at least as large as the page size of
	 * the underlying database, aligned for unsigned integer
	 * access, and be a multiple of 1024 bytes in size.
	 */
	key.ulen = (u_int32_t)UPDATES_PER_BULK_PUT *
		sizeof(u_int32_t) * datalen * 1024;
	key.flags = DB_DBT_USERMEM | DB_DBT_BULK;
	key.data = malloc(key.ulen);

	data_val = malloc(datalen);
	memcpy(data_val->str, "0123456789012345678901234567890123456789",
	    datalen - sizeof(data_val->id));
	data_val->id = 0;

	if (dups)
		flag |= DB_MULTIPLE;
	else
		flag |= DB_MULTIPLE_KEY;

	DB_MULTIPLE_WRITE_INIT(ptrk, &key);

	for (i = 0; i < j; i++) {
		if (txn != 0 && (i+1) % UPDATES_PER_BULK_PUT == 0) {
			if (txnp != NULL) {
				ret = txnp->commit(txnp, 0);
				txnp = NULL;
				if (ret != 0)
					goto err;
			}
			if ((ret =
				dbenv->txn_begin(dbenv, NULL, &txnp, 0)) != 0)
				goto err;
		}
		if (dups) {
			DB_MULTIPLE_WRITE_NEXT(ptrk, &key, &i, sizeof(i));
			assert(ptrk != NULL);
		}
		else {
			DB_MULTIPLE_KEY_WRITE_NEXT(ptrk,
				&key, &i, sizeof(i), data_val, datalen);
			assert(ptrk != NULL);
		}
		if ((i+1) % UPDATES_PER_BULK_PUT == 0) {
			switch (ret = dbp->del(dbp, txnp, &key, flag)) {
				case 0:
					iter++;
					free (key.data);

					memset(&key, 0, sizeof(DBT));

					key.ulen =
					(u_int32_t)UPDATES_PER_BULK_PUT *
					sizeof(u_int32_t) * datalen * 1024;
					key.flags = DB_DBT_USERMEM|DB_DBT_BULK;
					key.data = malloc(key.ulen);

					DB_MULTIPLE_WRITE_INIT(ptrk, &key);
					break;
				default:
					dbp->err(dbp, ret, "Bulk DB->del");
					free (key.data);
					goto err;
			}
		}
		count++;
	}
	if ((j % UPDATES_PER_BULK_PUT) != 0) {
		switch (ret = dbp->del(dbp, txnp, &key, flag)) {
			case 0:
				iter++;
				free (key.data);
				break;
			default:
				free (key.data);
				dbp->err(dbp, ret, "Bulk DB->del");
				goto err;
		}
	}

	if (txnp != NULL)
		ret = txnp->commit(txnp, 0);

	printf("Deleted %d records in %d batches\n", count, iter);
	return (ret);

err:    if (txnp != NULL)
		(void)txnp->abort(txnp);
	return (ret);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	extern char *optarg;
	extern int optind;
	DB *dbp;
	DB_ENV *dbenv;
	DB_TXN *txnp;
	struct timeval start_time, end_time;
	double secs;
	u_int cache, datalen, pagesize;
	int biter, ch, count, dups, env, init, iter, num;
	int ret, rflag, txn, bulk, delete;

	txnp = NULL;
	datalen = 20;
	iter = num = 1000000;
	env = 1;
	dups = init = rflag = txn = bulk = delete = 0;

	pagesize = 65536;
	cache = 1000 * pagesize;

	while ((ch = getopt(argc, argv, "c:d:EIi:l:n:p:RTBD")) != EOF)
		switch (ch) {
		case 'c':
			cache = (u_int)atoi(optarg);
			break;
		case 'd':
			dups = atoi(optarg);
			break;
		case 'E':
			env = 0;
			break;
		case 'I':
			init = 1;
			break;
		case 'i':
			iter = atoi(optarg);
			break;
		case 'l':
			datalen = (u_int)atoi(optarg);
			break;
		case 'n':
			num = atoi(optarg);
			break;
		case 'p':
			pagesize = (u_int)atoi(optarg);
			break;
		case 'R':
			rflag = 1;
			break;
		case 'T':
			txn = 1;
			break;
		case 'B':
			bulk = 1;
			break;
		case 'D':
			delete = 1;
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	/* Remove the previous database. */
	if (!rflag) {
		if (env)
			(void)system("rm -rf BENCH_001; mkdir BENCH_001");
		else
			(void)unlink(DATABASE);
	}

	dbenv = NULL;
	if (env == 1 &&
	    (dbenv = db_init("BENCH_001", "bench_001", cache, txn)) == NULL)
		return (-1);
	if (init)
		exit(0);
	/* Create and initialize database object, open the database. */
	if ((ret = db_create(&dbp, dbenv, 0)) != 0) {
		fprintf(stderr,
		    "%s: db_create: %s\n", progname, db_strerror(ret));
		exit(EXIT_FAILURE);
	}
	dbp->set_errfile(dbp, stderr);
	dbp->set_errpfx(dbp, progname);
	if ((ret = dbp->set_bt_compare(dbp, compare_int)) != 0) {
		dbp->err(dbp, ret, "set_bt_compare");
		goto err;
	}
	if ((ret = dbp->set_pagesize(dbp, pagesize)) != 0) {
		dbp->err(dbp, ret, "set_pagesize");
		goto err;
	}
	if (dups && (ret = dbp->set_flags(dbp, DB_DUP)) != 0) {
		dbp->err(dbp, ret, "set_flags");
		goto err;
	}

	if (env == 0 && (ret = dbp->set_cachesize(dbp, 0, cache, 0)) != 0) {
		dbp->err(dbp, ret, "set_cachesize");
		goto err;
	}

	if ((ret = dbp->set_flags(dbp, DB_DUP)) != 0) {
		dbp->err(dbp, ret, "set_flags");
		goto err;
	}

	if (txn != 0)
		if ((ret = dbenv->txn_begin(dbenv, NULL, &txnp, 0)) != 0)
			goto err;

	if ((ret = dbp->open(
	    dbp, txnp, DATABASE, NULL, DB_BTREE, DB_CREATE, 0664)) != 0) {
		dbp->err(dbp, ret, "%s: open", DATABASE);
		if (txnp != NULL)
			(void)txnp->abort(txnp);
		goto err;
	}

	if (txnp != NULL)
		ret = txnp->commit(txnp, 0);
	txnp = NULL;
	if (ret != 0)
		goto err;

	if (rflag) {
		/* If no environment, fill the cache. */
		if (!env && (ret =
		    get(dbp, txn, datalen, num, dups, iter, &count)) != 0)
			goto err;

		/* Time the get loop. */
		(void)gettimeofday(&start_time, NULL);
		if ((ret =
		    get(dbp, txn, datalen, num, dups, iter, &count)) != 0)
			goto err;
		(void)gettimeofday(&end_time, NULL);
		secs =
		    (((double)end_time.tv_sec * 1000000 + end_time.tv_usec) -
		    ((double)start_time.tv_sec * 1000000 + start_time.tv_usec))
		    / 1000000;
		printf("%d records read using %d batches in %.2f seconds: ",
		    count, iter, secs);
		printf("%.0f records/second\n", (double)count / secs);

	} else {
		if (bulk) {
			/* Time the get loop. */
			(void)gettimeofday(&start_time, NULL);
			if ((ret = bulk_fill(dbenv, dbp, txn, datalen, 
				num, dups, &count, &biter)) != 0)
				goto err;
			(void)gettimeofday(&end_time, NULL);
			secs =
				(((double)end_time.tv_sec * 1000000 + 
					end_time.tv_usec) -
				((double)start_time.tv_sec * 1000000 +
					start_time.tv_usec))
				/ 1000000;
			printf("%d records put using %d batches", 
			    count, biter);
			printf(" in %.2f seconds: ", secs);
			printf("%.0f records/second\n", (double)count / secs);
		}
		
		else {
			/* Time the get loop. */
			(void)gettimeofday(&start_time, NULL);
			if ((ret = fill(dbenv, dbp, txn, datalen, num, 
				dups, &count)) != 0)
				goto err;
			(void)gettimeofday(&end_time, NULL);
			secs =
				(((double)end_time.tv_sec * 1000000 + 
					end_time.tv_usec) -
				((double)start_time.tv_sec * 1000000 + 
					start_time.tv_usec))
				/ 1000000;
			printf("%d records put in %.2f seconds: ", 
			    count, secs);
			printf("%.0f records/second\n", (double)count / secs);
		}
		
		if (delete) {
			if ((ret = 
			bulk_delete(dbenv, dbp, txn, datalen, num, dups)) != 0)
				goto err;
		} 
	}

	/* Close everything down. */
	if ((ret = dbp->close(dbp, rflag ? DB_NOSYNC : 0)) != 0) {
		fprintf(stderr,
		    "%s: DB->close: %s\n", progname, db_strerror(ret));
		return (1);
	}
	return (ret);

err:	(void)dbp->close(dbp, 0);
	return (1);
}

int
compare_int(dbp, a, b)
	DB *dbp;
	const DBT *a, *b;
{
	int ai, bi;

	dbp = dbp;				/* Lint. */

	/*
	 * Returns:
	 *	< 0 if a < b
	 *	= 0 if a = b
	 *	> 0 if a > b
	 */
	memcpy(&ai, a->data, sizeof(int));
	memcpy(&bi, b->data, sizeof(int));
	return (ai - bi);
}

void
usage()
{
	(void)fprintf(stderr, "usage: %s %s\n\t%s\n",
	    progname, "[-EIRTBE] [-c cachesize] [-d dups]",
	    "[-i iterations] [-l datalen] [-n keys] [-p pagesize]");
	exit(EXIT_FAILURE);
}
