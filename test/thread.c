/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 1997
 *	Sleepycat Software.  All rights reserved.
 */

#include "config.h"

#ifndef lint
static const char sccsid[] = "@(#)thread.c	8.22 (Sleepycat) 1/9/98";
#endif /* not lint */

/*
 * Test thread support in DB.
 */
#include <sys/types.h>

#include <errno.h>
#include <limits.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "db_int.h"
#include "db_page.h"
#include "db_am.h"
#include "clib_ext.h"
#include "common_ext.h"

#define	MAXLOGREC	5000

typedef struct {
	union {
		char	c[32];
		struct {
			db_mutex_t	real_m;
			int32_t		real_val;
		} r;
	} u;
} mu_struct;
#define m	u.r.real_m
#define	val	u.r.real_val

#define	CHECK_ERROR(ID, OP, E, K, T)					\
{									\
	if (E < 0) {							\
		fprintf(stderr, "[%ld] Key %s not found (%ld) on %s\n",	\
		    (long)ID, K, (long)E, OP);				\
		goto err;						\
	} else if (E == EAGAIN) {					\
		fprintf(stderr,						\
		    "[%ld] Warning, txn aborting on %s %s\n",		\
		    (long)ID, K, OP);					\
		if (curs != NULL)					\
			(void)curs->c_close(curs);			\
		if ((E = txn_abort(T)) != 0) {				\
			fprintf(stderr,					\
			    "[%ld] transaction abort failed\n",		\
			    (long)ID);					\
			goto err;					\
		}							\
		goto retry;						\
	} else {							\
		fprintf(stderr, "[%ld] %s failed for key %s %s\n",	\
		    (long)ID, OP, K, strerror(E));			\
		goto err;						\
	}								\
}

/* Internal functions */
void	 am_test __P((DBTYPE));
void	*am_thread __P((void *));
int	 check_page __P((int32_t *, int32_t, int32_t, long));
void	 debug_check __P((void));
DB_MPOOLFILE
	*get_mpf __P((int));
void	 init_db __P((DBTYPE));
void	 lock_test __P((void));
void	*lock_thread __P((void *));
void	 log_test __P((void));
void	*log_thread __P((void *));
int	 main __P((int, char *[]));
void	 mpool_test __P((void));
void	*mp_thread __P((void *));
void	 mutex_test __P((void));
void	*mutex_thread __P((void *));
void	 parse_pages __P((char *));
int	 sched_yield __P((void));
void	 random_data __P((char *, int));
int	 random_int __P((int, int));
void	 set_page __P((int32_t *, int32_t, int32_t, long));
void	 spawn_kids __P((char *, void *(*)(void *)));
void	 usage __P((void));

/* Debugging variables */
int debug_on, debug_print, debug_stop, debug_test;
int am_shutdown;

/* Globals that are accessible to all threads. */
int debug, mutex_fd;
long iterations, lock_degree, nfiles, npages, nthreads, readratio;
long *pagesizes;
char *home;
DB *dbp;
DB_LOCKTAB *lt;
DB_LOG *logp;
DB_MPOOL *mp;
DB_MPOOLFILE **mpf_array;
DB_TXNMGR *tm;
mu_struct *mutex_array;

char    *progname = "ttest";                       	/* Program name. */

int
main(argc, argv)
	int argc;
	char *argv[];
{
	extern char *optarg;
	extern int optind;
	int ch, do_init;
	char *test;

#define	XMALLOC_DEBUG
#ifdef	MALLOC_DEBUG
	extern char *malloc_options;
	malloc_options="AJVX";
#endif
	debug = 0;
	do_init = 0;
	home = __db_exists("TESTDIR", NULL) == 0 ? "TESTDIR" : ".";
	iterations = 1000;
	lock_degree = 3;
	logp = NULL;
	lt = NULL;
	mp = NULL;
	mpf_array = NULL;
	mutex_array = NULL;
	nfiles = 10;
	npages = 10;
	nthreads = 5;
	pagesizes = NULL;
	readratio = 50;
	test = "hash";

	while ((ch = getopt(argc, argv, "c:d:Df:h:i:Ip:P:r:t:")) != EOF)
		switch (ch) {
		case 'c':			/* Concurrency */
			get_long(optarg, 1, LONG_MAX, &nthreads);
			break;
		case 'd':			/* Lock Degree */
			get_long(optarg, 1, LONG_MAX, &lock_degree);
			break;
		case 'D':			/* Debug */
			debug = 1;
			break;
		case 'f':			/* N files */
			get_long(optarg, 1, LONG_MAX, &nfiles);
			break;
		case 'h':			/* DBHOME */
			home = optarg;
			break;
		case 'i':			/* Iterations */
			get_long(optarg, 1, LONG_MAX, &iterations);
			break;
		case 'I':			/* Init (for am test) */
			do_init = 1;
			break;
		case 'p':			/* Pagesizes */
			parse_pages(optarg);
			break;
		case 'P':			/* How many pages */
			get_long(optarg, 1, LONG_MAX, &npages);
			break;
		case 'r':			/* read ratio */
			get_long(optarg, 1, 100, &readratio);
			break;
		case 't':			/* Which test */
			test = optarg;
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (argc != 0)
		usage();

	if (do_init && strcmp(test, "btree") == 0)
		init_db(DB_BTREE);
	else if (do_init && strcmp(test, "hash") == 0)
		init_db(DB_HASH);
	else if (strcmp(test, "btree") == 0)
		am_test(DB_BTREE);
	else if (strcmp(test, "hash") == 0)
		am_test(DB_HASH);
	else if (strcmp(test, "lock") == 0)
		lock_test();
	else if (strcmp(test, "log") == 0)
		log_test();
	else if (strcmp(test, "mutex") == 0)
		mutex_test();
	else if (strcmp(test, "mpool") == 0)
		mpool_test();
	else
		usage();
	exit (0);
}

void
usage()
{
	fprintf(stderr, "%s\n\t%s\n\t%s\n\t%s\n",
    "usage: thread [-DI]",
    "[-c threads] [-d lock_degree] [-f files] [-h home] [-i iterations]",
    "[-p pagesizes] [-P pages] [-r readratio]",
    "[-t btree | hash | lock | log | mutex | mpool]");
	exit (1);
}

void
lock_test()
{
	DB_ENV env;

	memset(&env, 0, sizeof(DB_ENV));
	env.db_errpfx = "lock_test";
	env.db_errfile = stderr;
	env.db_verbose = 1;
	(void)db_jump_set(sched_yield, DB_FUNC_YIELD);

	/* Open lock region. */
	if ((errno = db_appinit(home, NULL, &env,
	    DB_CREATE | DB_THREAD | DB_INIT_LOCK)) != 0) {
		fprintf(stderr, "lock_test: db_appinit failed %s\n",
		    strerror(errno));
		exit (1);
	}

	lt = env.lk_info;

	spawn_kids("lock_test", lock_thread);

	(void)db_appexit(&env);
}

void *
lock_thread(arg)
	void *arg;
{
	DBT obj_dbt;
	DB_LOCK *held;
	DB_LOCK lock;
	db_lockmode_t mode;
	pthread_t me;
	int32_t id;
	int i, j, last, obj, nlocks, r;
	void *ret;

	ret = NULL;
	id = (int32_t)arg;
	me = pthread_self();
	if ((held = (DB_LOCK *)calloc(sizeof(DB_LOCK), lock_degree)) == NULL)
		return (NULL);

	printf("Thread: %lu, id %ld\n", (u_long)me, (long)id);

	for (i = 0; i < iterations; i++) {
		nlocks = random_int(1, lock_degree);
		/* Always lock in ascending order to avoid deadlocks. */
		last = 1;

		for (j = 0; j < nlocks; j++) {
			obj = random_int(last, npages - 1);
			last = obj + 1;

			r = random_int(1, 100);
			if (r <= readratio)
				mode = DB_LOCK_READ;
			else
				mode = DB_LOCK_WRITE;

			printf("[%ld] Getting %ld (%s)\n", (long)id, (long)obj,
			    mode == DB_LOCK_READ ? "read" : "write");
			/* Acquire the lock. */
			obj_dbt.data = &obj;
			obj_dbt.size = sizeof(obj);
			if ((errno =
			    lock_get(lt, id, 0, &obj_dbt, mode, &lock)) != 0) {
				fprintf(stderr, "[%ld] lock_get failed %s\n",
				    (long)id, strerror(errno));
				goto err;
			}
			printf("[%ld] got %lu\n", (long)id, (u_long)lock);
			held[j] = lock;

			/* Keep track of which ones we got. */
			if (obj >= npages)
				break;
		}
		__db_sleep(0, random_int(1, 999999));
		for (j = 0; j < nlocks; j++)
			if (held[j] != 0) {
				printf("[%ld] Releasing %lu\n",
				    (long)id, (u_long)held[j]);
				if ((errno = lock_put(lt, held[j])) != 0) {
					fprintf(stderr,
					    "[%ld] lock_put failed %s\n",
					    (long)id, strerror(errno));
					goto err;
				}
				held[j] = 0;
			}

		printf("[%ld] Released locks\n", (long)id);
	}
err:	free (held);
	printf("[%ld] Exiting\n", (long) id);
	return (&tm);
}

void
log_test()
{
	DB_ENV env;
	DBT data;
	DB_LOG *local_log;
	DB_LSN lsn;
	int32_t *idptr;
	int *counts, i, r;

	memset(&env, 0, sizeof(DB_ENV));
	env.db_errpfx = "log_test";
	env.db_errfile = stderr;
	env.db_verbose = 1;
	(void)db_jump_set(sched_yield, DB_FUNC_YIELD);

	/* Open log region. */
	if ((errno = db_appinit(home, NULL, &env,
	    DB_CREATE | DB_THREAD | DB_INIT_LOG)) != 0) {
		fprintf(stderr, "log_test: db_appinit failed %s\n",
		    strerror(errno));
		exit (1);
	}

	logp = env.lg_info;

	spawn_kids("log_test", log_thread);

	/* Verify log contents */
	if ((counts = (int *)calloc(sizeof(int), nthreads)) == NULL) {
		fprintf(stderr,
		    "log_test: Unable to malloc memory to validate log.\n");
		goto err;
	}

	/*
	 * Open a local copy of the log so that we can check content
	 * using sequential operations.
	 */
	if ((r = log_open(home, 0, 0, NULL, &local_log)) != 0) {
		fprintf(stderr, "log_test: unable to open local log %s\n",
		    strerror(r));
		goto err;
	}
	memset(&data, 0, sizeof(data));
	for (errno = log_get(local_log, &lsn, &data, DB_FIRST);
	    errno == 0;
	    errno = log_get(local_log, &lsn, &data, DB_NEXT)) {
		idptr = (int32_t *)data.data;
		counts[*idptr]++;
	}
	(void)log_close(local_log);

	for (i = 0; i < nthreads; i++)
		if (counts[i] != iterations)
			fprintf(stderr, "log_test: %s %ld. %s %ld %s %ld\n",
			    "Mismatch on thread", (long) i, "Expected",
			    (long)iterations, "records, got", (long)counts[i]);

err:	(void)db_appexit(&env);
}

void *
log_thread(arg)
	void *arg;
{
	DBT get_data;
	DBT rec_dbt;
	DB_LSN lsn;
	DB_LOG *local_log;
	pthread_t me;
	int32_t id, *idptr;
	int i, next_flag, r, rec_size, start_flag;
	char *rec, *rec_start;
	void *ret;

	ret = NULL;
	id = (int32_t)arg;
	me = pthread_self();

	printf("Thread: %lu, id %ld\n", (u_long)me, (long)id);

	/*
	 * Open a local copy of the log so that we can check content
	 * using sequential operations.
	 */
	if ((r = log_open(home, 0, 0, NULL, &local_log)) != 0) {
		fprintf(stderr, "[%ld] unable to open local log\n", (long)id);
		return (NULL);
	}

	if ((rec = (char *)malloc(sizeof(int32_t) + MAXLOGREC)) == NULL) {
		fprintf(stderr, "[%ld] malloc for record failed\n", (long)id);
		return (&logp);
	}
	*((int32_t *)rec) = id;
	rec_start = (char *)((u_int8_t *) rec + sizeof(int32_t));
	memset(&rec_dbt, 0, sizeof(rec_dbt));
	rec_dbt.data = rec;

	for (i = 0; i < iterations; i++) {
		rec_size = random_int(2, MAXLOGREC - 1);
		rec_dbt.size = sizeof(int32_t) + rec_size;
		random_data(rec_start, rec_size);
		if ((errno = log_put(logp, &lsn, &rec_dbt, 0)) != 0) {
			fprintf(stderr, "[%ld] log_put failed %s\n",
			    (long)id, strerror(errno));
			return (&logp);
		}
		__db_sleep(0, random_int(1, 999999));

		/*
		 * Every ten records or so, do a read check.  Sometimes do
		 * it from the front of the log, sometimes from the back
		 * and sometimes from the last one you wrote.
		 */
		if ((r = random_int(1, 30)) == 1) {
			start_flag = DB_LAST;
			next_flag = DB_PREV;
		} else if (r == 11) {
			start_flag = DB_FIRST;
			next_flag = DB_NEXT;
		} else if (r == 21) {
			start_flag = DB_SET;
			next_flag = DB_PREV;
		} else
			continue;
		printf("[%ld] Doing read\n", (long)id);
		memset(&get_data, 0, sizeof(get_data));
		for (errno = log_get(local_log, &lsn, &get_data, start_flag);
		    errno == 0;
		    errno = log_get(local_log, &lsn, &get_data, next_flag)) {
			idptr = (int32_t *)get_data.data;
			if (*idptr == id)
				break;
		}
		if (errno > 0) {
			fprintf(stderr, "[%ld] log_get failed %s\n",
			    (long)id, strerror(errno));
			return (&logp);
		}
	}
	(void)log_close(local_log);
	return (NULL);
}

void
mpool_test()
{
	DB_ENV env;
	int i;

	memset(&env, 0, sizeof(DB_ENV));
	env.db_errpfx = "mpool_test";
	env.db_errfile = stderr;
	env.db_verbose = 1;
	(void)db_jump_set(sched_yield, DB_FUNC_YIELD);

	/* Open lock and mpool regions. */
	if ((errno = db_appinit(home, NULL, &env,
	    DB_CREATE | DB_THREAD | DB_INIT_LOCK | DB_INIT_MPOOL)) != 0) {
		fprintf(stderr, "mpool_test: db_appinit failed %s\n",
		    strerror(errno));
		exit (1);
	}

	lt = env.lk_info;
	mp = env.mp_info;

	/* Create the mpf array. */
	if ((mpf_array =
	    (DB_MPOOLFILE **)calloc(sizeof(DB_MPOOLFILE *), nfiles)) == NULL) {
		fprintf(stderr, "mpool_test: unable to allocate mpf array\n");
		exit (1);
	}

	/* Fill in pagesizes if there aren't any. */
	if (pagesizes == NULL &&
	    (pagesizes = (long *)malloc(sizeof(long) * nfiles)) == NULL) {
		fprintf(stderr, "mpool_test: unable to allocate pagesizes\n");
		exit (1);
	}
	pagesizes[0] = 512;
	for (i = 1; i < nfiles; i++)
		pagesizes[i] = 4096;

	spawn_kids("mpool_test", mp_thread);

	(void)db_appexit(&env);
}

/*
 * This implements the same basic strategy as the mpool multi-process
 * test.
 * Process globals:
 *	mp	mpool cookie
 *	lt	lock region cookie
 *	mpf_array[N]	Array of mpool file cookies.  If it's NULL, open the
 *			file.
 *	pages[N]	Page sizes for the different files.
 *	iterations	Number of iterations to run.
 *	nfiles		Number of files in set.
 *	nthreads	Number of threads running.
 * Argument is your local id
 */
void *
mp_thread(arg)
	void *arg;
{
	DBT obj;
	DB_LOCK lock;
	DB_MPOOLFILE *mpf;
	pthread_t me;
	u_long flags;
	int32_t *pagep, *master, id, lockinfo[2];
	int f, i, p, pagecount, pred, sv_errno;

	mpf = NULL;
	pagep = master = NULL;
	lock = 0;
	id = (int32_t)arg;

	/* Figure out who we are. */
	me = pthread_self();
	printf("Thread: %lu, id %ld\n", (u_long)me, (long)id);
	printf("[%lu] Establishing longterm pin on file 0 page %ld\n",
	    (u_long)me, (long)id);

	lockinfo[0] = 0;		/* File number. */
	lockinfo[1] = id;		/* Page Number. */

	memset(&obj, 0, sizeof(obj));
	obj.data = &lockinfo;
	obj.size = sizeof(lockinfo);
	errno = lock_get(lt, (u_int32_t)me, 0, &obj, DB_LOCK_WRITE, &lock);
	if (errno != 0)
		goto err;

	/* Now get the page for file 0. */
	mpf = get_mpf(0);
	if ((errno = memp_fget(mpf, (db_pgno_t *)&id, DB_CREATE, &master)) != 0)
		goto err;

	set_page(master, id, (int32_t)id, pagesizes[0]);
	/*

	 * Now release the lock; since we have the page pinned, it should stay
	 * in cache.
	 */
	if ((errno = lock_put(lt, lock)) != 0)
		goto err;
	lock = 0;

	/*
	 * Main loop.  On each iteration, we'll check every page in each of
	 * of the files.  On any file, if we see the appropriate tag in the
	 * field, we'll rewrite the page, else we won't.  Keep track of
	 * how many pages we actually process.
	 */

	pagecount = 0;
	for (i = 0; i < iterations; i++) {
		printf("[%ld] iteration %ld, %ld pages set so far\n",
		    (long)id, (long) i, (long)pagecount);

		for (f = 1; f < nfiles; f++) {
			if (f % 2 == 0)
				pred = id + nthreads - 1 % nthreads;
			else
				pred = id + nthreads + 1 % nthreads;

			mpf = get_mpf(f);

			/*
			 * Now loop through all the pages in the file.  For
			 * each page, lock it then get it.
			 */
			for (p = 0; p < npages; p++) {
				lockinfo[0] = f;
				lockinfo[1] = p;
				if ((errno = lock_get(lt, (u_int32_t)me, 0,
				    &obj, DB_LOCK_WRITE, &lock)) != 0)
					goto err;

				if ((errno = memp_fget(mpf,
				    (db_pgno_t *)&p, DB_CREATE, &pagep)) != 0)
					goto err;
				if (check_page(pagep,
				    pred, (int32_t)p, pagesizes[f])) {
					flags = DB_MPOOL_DIRTY;
					pagecount++;
					set_page(pagep,
					    id, (int32_t)p, pagesizes[f]);
				} else
					flags = 0;
				if ((errno = memp_fput(mpf, pagep, 0)) != 0 ||
				    (errno = lock_put(lt, lock)) != 0)
					goto err;
				lock = 0;

			}
		}
		__db_sleep(0, random_int(1, 999999));
	}

	mpf = get_mpf(0);

	/* Verify the master page and release its pin. */
	printf("[%ld] Verifying master page\n", (long)id);
	if (check_page(master, id, (int32_t)id, pagesizes[0]))
		fprintf(stderr, "[%ld] WARNING: Error on my master page\n",
		    (long)id);
	if ((errno = memp_fput(mpf, master, DB_MPOOL_DIRTY)) != 0)
		goto err;
	master = NULL;

	/* Now verify everyone else's master. */
	for (p = (id + 1) % nthreads; p != id; p = (p + 1) % nthreads) {
		if ((errno = memp_fget(mpf,
		    (db_pgno_t *)&p, DB_CREATE, &pagep)) != 0)
			goto err;

		if (check_page(pagep, p, (int32_t)p, pagesizes[0]))
			fprintf(stderr,
			    "[%ld] WARNING: Error on master page %ld\n",
			        (long)id, (long)p);
		if ((errno = memp_fput(mpf, pagep, 0)) != 0)
			goto err;

	}

	return (NULL);

err:	sv_errno = errno;
	if (master != NULL)
		(void)memp_fput(mpf, master, 0);
	if (pagep != NULL)
		(void)memp_fput(mpf, pagep, 0);
	if (lock != NULL)
		(void)lock_put(lt, lock);
	strerror(sv_errno);
	return (&mp);
}


DB_MPOOLFILE *
get_mpf(n)
	int n;
{
	char name[32];

	if (mpf_array[n] != NULL)
		return (mpf_array[n]);

	sprintf(name, "file%d", n);
	if ((errno = memp_fopen(mp, name, 0, DB_CREATE,
	    0644, pagesizes[n], -1, NULL, NULL, &mpf_array[n])) != 0) {
		fprintf(stderr,
		    "memp_fopen of %s failed: %s\n", name, strerror(errno));
		free(name);
		mpf_array[n] = NULL;
		return (NULL);
	}

	return (mpf_array[n]);
}

int
check_page(p, v, pno, psize)
	int32_t *p, v;
	int32_t pno;
	long psize;
{
	u_int i;

	if (p[0] != pno)
		return (1);
	for (i = 1;  i < psize / sizeof(v); i++)
		if (p[i] != v)
			return (1);
	return (0);
}

void
set_page(p, id, pno, psize)
	int32_t *p, id;
	int32_t pno;
	long psize;
{
	u_int i;

	p[0] = pno;
	for (i = 1;  i < psize / sizeof(id); i++)
		p[i] = id;
}

int
random_int(lo, hi)
	int lo, hi;
{
#ifndef	RAND_MAX
#define	RAND_MAX	0x7fffffff
#endif
	int ret, t;

	t = rand();
	if (t > RAND_MAX)
		printf("Max random is higher than %d\n", RAND_MAX);
	ret = (int)(((double)t / ((double)(RAND_MAX) + 1)) * (hi - lo + 1));
	ret += lo;
	return (ret);
}


void
parse_pages(str)
	char *str;
{
	int i, nfiles;
	char *n;

	/* Count the files. */
	for (nfiles = 1, n = str; n != NULL && *n != '\0';)
		if (*n == ',')
			++nfiles;

	/* Allocate space for the pages */
	if ((pagesizes = (long *)malloc(sizeof(long) * nfiles)) == NULL)
		errx(1, "thread_test: parse_pages out of memory");

	for (i = 0, n = strtok(str, ","); n != NULL && i < nfiles;
	    n = strtok(NULL, ","), ++i)
		get_long(n, 1, LONG_MAX, &pagesizes[i]);
}

void
mutex_test()
{
	DB_ENV env;
	size_t region_size;
	int i;
	void *mutex_region;

	memset(&env, 0, sizeof(DB_ENV));
	env.db_errpfx = "mutex_test";
	env.db_errfile = stderr;
	env.db_verbose = 1;
	(void)db_jump_set(sched_yield, DB_FUNC_YIELD);

	if ((errno =
	    db_appinit(home, NULL, &env, DB_CREATE | DB_THREAD)) != 0) {
		fprintf(stderr, "mutex_test: db_appinit failed %s\n",
		    strerror(errno));
		exit (1);
	}

	/* Create and initialize mutex region.  Create global handle. */
	unlink("TESTDIR/mutex_file");
	region_size =
	    ALIGN(sizeof(RLAYOUT), 32) + npages * sizeof(mu_struct);
	if ((errno = __db_rcreate(&env, DB_APP_NONE, "TESTDIR", "mutex_file",
	    0644, region_size, 0, &mutex_fd, &mutex_region)) != 0) {
		fprintf(stderr,
		    "mutex_test: __db_rcreate failed %s\n", strerror(errno));
		(void)db_appexit(&env);
		exit (1);
	}

	/* Initialize the mutexes in the region. */
	mutex_array = (mu_struct *)((u_int8_t *)mutex_region +
	    ALIGN(sizeof(RLAYOUT), 32));
	for (i = 0; i < npages; i++) {
		mutex_array[i].val = 0;
		__db_mutex_init(&mutex_array[i].m, i);
	}

	/* Unlock the region. */
	(void)__db_mutex_unlock(&((RLAYOUT *)mutex_region)->lock, mutex_fd);

	/* Spawn off threads and wait on them. */
	spawn_kids("mutex_test", mutex_thread);

	if ((errno = __db_rclose(&env, mutex_fd, mutex_region)) != 0)
		fprintf(stderr, "mutex_test: __db_rclose failed %s\n",
			strerror(errno));
	exit(db_appexit(&env));
}

void *
mutex_thread(arg)
	void *arg;
{
	pthread_t me;
	int32_t id;
	int i, j, last, obj, nlocks, ret, *held;

	ret = 0;
	id = (int32_t)arg;
	me = pthread_self();
	if ((held = (int *)calloc(sizeof(int), lock_degree)) == NULL)
		return (&mutex_array);

	printf("Thread: %lu, id %ld\n", (u_long)me, (long)id);

	for (i = 0; i < iterations; i++) {
		nlocks = random_int(1, lock_degree);
		/* Always lock in ascending order to avoid deadlocks. */
		last = 1;

		for (j = 0; j < nlocks; j++) {
			obj = random_int(last, npages - 1);
			held[j] = obj;
			last = obj + 1;
			printf("[%ld] %ld: %ld\n",
			    (long)id, (long)j, (long)obj);
			/* Acquire the mutex. */
			__db_mutex_lock(&mutex_array[obj].m, mutex_fd);
			mutex_array[obj].val = id;
			/* Keep track of which ones we got. */
			if (obj >= npages)
				break;
		}
		__db_sleep(0, random_int(1, 999999));
		for (j = 0; j < nlocks; j++)
			if (held[j] != 0) {
				if (mutex_array[held[j]].val != id) {
					fprintf(stderr, "[%ld] Bad val (%ld)\n",
					    (long)id,
					    (long)mutex_array[held[j]].val);
					ret = 1;
					goto out;
				}
				mutex_array[held[j]].val = -id;
				__db_mutex_unlock(&mutex_array[held[j]].m,
				    mutex_fd);
				held[j] = 0;
			}

		printf("[%ld] Released mutexes\n", (long)id);
	}

out:	free(held);
	printf("[%ld] Exiting\n", (long) id);
	if (ret != 0)
		return (&mutex_array);
	else
		return (NULL);
}

void
spawn_kids(msg, func)
	char *msg;
	void *(*func)__P((void *));
{
	pthread_t *kids;
	int i;
	void *retp;

	/*
	 * Spawn off threads and wait on them.  We spawn off n-1 separate
	 * threads and run the last thread in the current thread of control.
	 * The theory was that this would make it easier to debug.
	 */
	if (nthreads == 1) {
		if (func((void *)(nthreads - 1)) != 0)
			fprintf(stderr, "%s: %ld exited with error\n",
			    msg, (long)(nthreads - 1));
		return;
	}

	if ((kids =
	    (pthread_t *)calloc(sizeof(pthread_t), nthreads - 1)) == NULL) {
		fprintf(stderr, "%s\n", strerror(errno));
		return;
	}
	for (i = 0; i < (nthreads - 1); i++)
		if ((errno =
		    pthread_create(kids + i, NULL, func, (void *)i)) != 0) {
			fprintf(stderr,
			    "%s: failed spawning thread %ld %s\n",
			    msg, (long)i, strerror(errno));
			break;
		}

	if (func((void *)(nthreads - 1)) != 0)
		fprintf(stderr,
		    "%s: %ld exited with error\n", msg, (long)(nthreads - 1));

	for (i = 0; i < nthreads - 1; i++)
		if (kids[i] != 0) {
			pthread_join(kids[i], &retp);
			if (retp != NULL)
				fprintf(stderr, "%s: %ld exited with error\n",
				    msg, (long)i);
		}

	free(kids);
}

#define	MAXDATAREC	512

void
init_db(method)
	DBTYPE method;
{
	DBT data_dbt, key_dbt;
	DB_ENV env;
	DB_INFO info;
	int i, j, k, l, nrecs;
	char *m_name, keystr[4], data[MAXDATAREC];

	memset(&env, 0, sizeof(DB_ENV));
	env.db_errpfx = method == DB_HASH ? "am hash" : "am btree";
	env.db_errfile = stderr;
	env.db_verbose = 1;
	(void)db_jump_set(sched_yield, DB_FUNC_YIELD);

	m_name = method == DB_HASH ? "hash" : "btree";

#define INIT_FLAGS (DB_CREATE | DB_INIT_MPOOL)

	/* Open lock region. */
	if ((errno = db_appinit(home, NULL, &env, INIT_FLAGS)) != 0) {
		fprintf(stderr, "access method %s: db_appinit failed %s\n",
		    m_name, strerror(errno));
		exit (1);
	}

	/* Create the database. */
	memset(&info, 0, sizeof(info));
	info.flags = DB_DUP;
	if ((errno = db_open(method == DB_HASH ? "hash.db" : "btree.db",
	    method, DB_CREATE | DB_TRUNCATE,
	    0644, &env, &info, &dbp)) != 0) {
		fprintf(stderr, "access method %s: db_open failed %s\n",
		    m_name, strerror(errno));
		    goto err;
	}

	/*
	 * Now, initialize the database.  We store 26^3 keys and
	 * nthreads-duplicates for each key.  Each key consists of
	 * a three-character string of lower-case alphabetic
	 * characters.  The data fields are 4 bytes of ID with
	 * a random string attached.
	 */

	keystr[3] = '\0';
	memset(&key_dbt, 0, sizeof(key_dbt));
	key_dbt.data = keystr;
	key_dbt.size = 4;
	memset(&data_dbt, 0, sizeof(data_dbt));
	data_dbt.data = data;
	for (nrecs = 0, i = 0; i < 1; i++) {
		keystr[0] = 'a' + i;
		for (j = 0; j < 26; j++) {
			keystr[2] = 'a' + j;
			for (k = 0; k < 26; k++) {
				keystr[1] = 'a' + k;
				for (l = 0; l < nthreads; l++) {
					*((int32_t *)data) = l;
					data_dbt.size =
					    random_int(2, MAXDATAREC - 1);
					random_data(data + sizeof(int32_t),
					    data_dbt.size - sizeof(int32_t));
					if ((errno = dbp->put(dbp, NULL,
					    &key_dbt, &data_dbt, 0)) != 0) {
						fprintf(stderr, "%s: put %s\n",
						    m_name, strerror(errno));
						goto err;
					}
					nrecs++;
				}
			}
		}
		printf("init_db: %d records done\n", nrecs);
	}

	if ((errno = dbp->close(dbp, 0)) != 0) {
		fprintf(stderr, "access method %s: close failed %s\n",
		    m_name, strerror(errno));
	}
err:	(void)db_appexit(&env);
}

void
am_test(method)
	DBTYPE method;
{
	DB_ENV env;
	char *m_name;

	memset(&env, 0, sizeof(DB_ENV));
	env.db_errpfx = method == DB_HASH ? "am hash" : "am btree";
	env.db_errfile = stderr;
	env.db_verbose = 0;
	env.lk_detect = DB_LOCK_DEFAULT;
	(void)db_jump_set(sched_yield, DB_FUNC_YIELD);

	m_name = method == DB_HASH ? "hash" : "btree";

#define AM_FLAGS \
	(DB_CREATE | DB_THREAD | DB_INIT_LOCK | DB_INIT_MPOOL | DB_INIT_TXN)

	/* Open lock region. */
	if ((errno = db_appinit(home, NULL, &env, AM_FLAGS)) != 0) {
		fprintf(stderr, "access method %s: db_appinit failed %s\n",
		    m_name, strerror(errno));
		exit (1);
	}

	/* Open the database. */
	if ((errno = db_open(method == DB_HASH ? "hash.db" : "btree.db",
	    method, DB_THREAD, 0644, &env, NULL, &dbp)) != 0) {
		fprintf(stderr, "access method %s: db_open failed %s\n",
		    m_name, strerror(errno));
		    goto err;
	}
	lt = env.lk_info;
	tm = env.tx_info;
	printf("dbp = %lu\n", (unsigned long)dbp);

	am_shutdown = 0;
	spawn_kids(m_name, am_thread);
	if ((errno = dbp->close(dbp, 0)) != 0)
		fprintf(stderr, "access method %s: close failed %s\n",
		    m_name, strerror(errno));

err:	(void)db_appexit(&env);
}

void *
am_thread(arg)
	void *arg;
{
	extern int __sleep_on_every_page_get;
	DBC *curs;
	DBT data_dbt, key_dbt;
	DB_TXN *txn;
	pthread_t me;
	int32_t id;
	int i, l, move_flag, op, put_flag;
	char keystr[4], tmpkey[4], data_str[MAXDATAREC];
	void *ret;

	__sleep_on_every_page_get = 1;

	ret = NULL;
	id = (int32_t)arg;
	me = pthread_self();
	txn = NULL;
	curs = NULL;

	printf("Thread: %lu, id %ld\n", (u_long)me, (long)id);
	srand((unsigned long)arg);

	memset(&key_dbt, 0, sizeof(key_dbt));
	memset(&data_dbt, 0, sizeof(data_dbt));
	key_dbt.data = keystr;
	key_dbt.ulen = 4;

	data_dbt.data = data_str;
	data_dbt.ulen = MAXDATAREC;
	for (i = 0; i < iterations; i++) {
		/*
		 * On each iteration we're going to randomly pick a key,
		 * and do one of the following operations:
		 *
		 * Simply get it and verify its contents. (1)
		 * Find the duplicate that belongs to us and change it. (2)
		 * Find the duplicate that belongs to us, delete it and
		 *	re-add it. (3)
		 * Delete all of it and then re-enter it entirely. (4)
		 * Get it and all its duplicates (one per thread). (5)
		 */
		op = random_int(1, 5);
		random_data(keystr, 4);
		keystr[0] = 'a';
		key_dbt.size = 4;
		printf("[%ld] Operation %ld key %s\n",
		    (long)id, (long)op, keystr);

retry:		if (am_shutdown == 1) {
			printf("[%ld] Shutting down.\n", (long)id);
			return (NULL);
		}
		if ((errno = txn_begin(tm, NULL, &txn)) != 0) {
			fprintf(stderr, "[%ld] txn_begin failed %s\n",
			    (long)id, strerror(errno));
			return (&tm);
		}
		debug_check();
		switch(op) {
		case 1:
			key_dbt.flags = 0;
			data_dbt.flags = DB_DBT_USERMEM;
			if ((errno = dbp->get(dbp, txn, &key_dbt, &data_dbt, 0))
			    != 0)
				CHECK_ERROR(id, "get", errno, keystr, txn);

			if (*((int32_t *)data_dbt.data) != 0) {
				fprintf(stderr,
			"[%ld] data mismatch for key %s: expected %d got %ld\n",
				    (long)id, keystr, 0,
				    (long)(*((int32_t *)data_dbt.data)));
				goto err;
			}

			break;
		case 2:
		case 3:
		case 5:
			if ((errno = dbp->cursor(dbp, txn, &curs)) != 0)
				CHECK_ERROR(id, "cursor", errno, keystr, txn);

			key_dbt.size = 4;
			key_dbt.flags = 0;
			data_dbt.flags = DB_DBT_USERMEM;

			for (l = 0, errno =
			    curs->c_get(curs, &key_dbt, &data_dbt, DB_SET);
			    errno == 0 && l < nthreads;
			    l++, errno =
			    curs->c_get(curs, &key_dbt, &data_dbt, DB_NEXT)) {
				if (*((int32_t *)data_dbt.data) != l) {
				    fprintf(stderr,
					"[%ld] %s %s %s %ld got %ld\n",
					(long)id, "data mismatch for key ",
					keystr, " expected ", (long)l,
					(long)(*((int32_t *)data_dbt.data)));
				    goto err;
				}
				if (op != 5 && l == id)
					break;
				key_dbt.flags = DB_DBT_USERMEM;
			}

			if (op == 5)
				break;

			/* Either errno should be set or n is our id. */
			if (errno != 0)
				CHECK_ERROR(id, "search for dup", errno,
				    keystr, txn);

			key_dbt.flags = 0;
			data_dbt.flags = 0;

			if (op == 3)
				goto op3;

			/* Now, change our data. */
			data_dbt.size = random_int(6, MAXDATAREC);
			random_data(data_str + sizeof(int32_t),
			    data_dbt.size - sizeof(int32_t));

			/* Replace */
			if ((errno = curs->c_put(curs,
			    &key_dbt, &data_dbt, DB_CURRENT)) != 0)
				CHECK_ERROR(id, "cursor put", errno,
				    keystr, txn);
			break;

			/* Delete and reput */
op3:			if ((errno = curs->c_del(curs, 0)) != 0)
				CHECK_ERROR(id, "cdelete", errno, keystr, txn);

			if (id != 0) {
				move_flag = DB_PREV;
				put_flag = DB_AFTER;
			} else {
				move_flag = DB_NEXT;
				put_flag = DB_BEFORE;
			}
			/*
			 * Since we don't really want any data on this get,
			 * return 0 bytes.
			 */
			data_dbt.flags = DB_DBT_USERMEM | DB_DBT_PARTIAL;
			data_dbt.dlen = 0;
			key_dbt.flags = DB_DBT_USERMEM;
			key_dbt.data = tmpkey;
			if ((errno = curs->c_get(curs,
			    &key_dbt, &data_dbt, move_flag)) != 0 &&
			    errno != DB_NOTFOUND)
				CHECK_ERROR(id, "seq cursor get", errno,
				    keystr, txn);

			/*
			 * I am assuming that if you return 0 bytes, you do
			 * not overwrite the existing buffer, but let's put
			 * our id in just to make sure.
			 */
			*((int32_t *)data_str) = id;

			/*
			 * If we are running with one thread, then there is
			 * the possibility that we're now on a new key or
			 * that we have run off the end of the file, in
			 * which case we need to do a regular put.
			 */
			/* Now, do the put. */
			data_dbt.flags = 0;
			data_dbt.size = random_int(6, MAXDATAREC);
			random_data(data_str + sizeof(int32_t),
			    data_dbt.size - sizeof(int32_t));
			key_dbt.flags = 0;

			if (nthreads == 1 && (errno == DB_NOTFOUND ||
			    strcmp(tmpkey, keystr)) != 0) {
				key_dbt.data = keystr;
				if ((errno = dbp->put(dbp, txn, &key_dbt,
				    &data_dbt, 0)) != 0)
					CHECK_ERROR(id, "put", errno,
					    keystr, txn);
			} else if ((errno = curs->c_put(curs,
			    &key_dbt, &data_dbt, put_flag)) != 0)
				CHECK_ERROR(id, "cursor put", errno,
				    keystr, txn);
			key_dbt.data = keystr;
			break;
		case 4:
			key_dbt.flags = 0;
			if ((errno = dbp->del(dbp, txn, &key_dbt, 0)) != 0)
				CHECK_ERROR(id, "delete", errno, keystr, txn);

			data_dbt.flags = 0;
			for (l = 0; l < nthreads; l++) {
				*((int32_t *)data_str) = l;
				data_dbt.size =
				    random_int(6, MAXDATAREC - 1);
				random_data(data_str + sizeof(int32_t),
				    data_dbt.size - sizeof(int32_t));
				if ((errno = dbp->put(dbp, txn,
				    &key_dbt, &data_dbt, 0)) != 0)
					CHECK_ERROR(id, "put", errno,
					    keystr, txn);
			}
			break;
		}

		/* Check that this key looks OK. */
		if (curs != NULL && debug) {
			key_dbt.flags = 0;
			data_dbt.flags = DB_DBT_USERMEM;
			printf("[%ld] Debugging key %s\n", (long)id, keystr);
			for (l = 0, errno =
			    curs->c_get(curs, &key_dbt, &data_dbt, DB_SET);
			    errno == 0 && l < nthreads;
			    l++, errno =
			    curs->c_get(curs, &key_dbt, &data_dbt, DB_NEXT)) {
				key_dbt.flags = DB_DBT_USERMEM;
				if (*((int32_t *)data_dbt.data) != l) {
				    fprintf(stderr,
			"[%ld] data mismatch for key %s: expected %d got %ld\n",
					(long)id, keystr, l,
					(long)(*((int32_t *)data_dbt.data)));
				    goto err;
				}
				key_dbt.flags = DB_DBT_USERMEM;
			}
		}

		if (curs != NULL)
			if ((errno = curs->c_close(curs)) != 0) {
				fprintf(stderr, "[%ld]: c_close failed %s\n",
				    (long)id, strerror(errno));
				goto err;
			}
		curs = NULL;

		/* Now commit the transaction. */
		if ((errno = txn_commit(txn)) != 0) {
			fprintf(stderr, "[%ld] txn_commit failed %s\n",
			    (long)id, strerror(errno));
			goto err;
		}
		txn = NULL;
	}
	printf("[%ld]: exiting cleanly\n", (long)id);
	return (NULL);

err:	if (curs != NULL)
		(void)curs->c_close(curs);
	if (txn != NULL)
		(void)txn_abort(txn);
	printf("[%ld]: exiting with error\n", (long)id);

	am_shutdown = 1;
	return (dbp);
}

void
random_data(p, len)
	char *p;
	int len;
{
	while (--len > 0)
		*p++ = 'a' + random_int(0, 25);
	*p = '\0';
}

void
debug_check()
{
	if (debug_on == 0)
		return;

	if (debug_print != 0) {
		printf("\r%6d:", debug_on);
		fflush(stdout);
	}

	if (debug_on++ == debug_test || debug_stop)
		__db_loadme();
}

void
stupid(dbmp)
	DB_MPOOL *dbmp;
{
	memp_stat(dbmp, NULL, NULL, NULL);
}
