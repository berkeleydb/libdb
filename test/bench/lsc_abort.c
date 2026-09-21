/*-
 * lsc_abort.c -- abort-heavy insert load, to test whether early release of the
 * PGNO_BASE_MD lock lets one transaction's abort damage another's allocation.
 *
 * See test/bench/BTREE-LOCK-SCOPE-2026-09.md.  __db_pg_alloc_recover's UNDO
 * path (src/db/db_rec.c:735-745) restores meta->free and meta->last_pgno from
 * the log record, and the do_truncate path calls __memp_ftruncate at
 * argp->pgno; none of the three consults whether a later transaction has
 * allocated past that point.  Hold-to-commit (2PL) is what makes that safe.
 * This driver applies the load that would expose it if the hold were dropped.
 *
 * Each thread inserts a key in a growing tree, then aborts ABORT_PCT of the
 * transactions.  Aborting an insert that split a page is exactly the case the
 * undo path handles.
 *
 * env: KEYRANGE, ABORT_PCT (default 50), BATCH (inserts per txn, default 1)
 * usage: lsc_abort <home> <nthreads> <secs>
 */
#include <sys/types.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include "db.h"

static DB_ENV *env;
static DB *db;
static volatile int stop;
static int nthreads, abort_pct, batch = 1;
static unsigned keyrange = 1000000;

struct st {
	pthread_t th;
	int id;
	unsigned long commits, aborts, deadlocks, errs;
};
static struct st *ts;

static void *
worker(void *a)
{
	struct st *s = a;
	DB_TXN *txn;
	DBT key, data;
	char kbuf[32], dbuf[128];
	unsigned int seed = (unsigned int)(s->id * 7919 + 13);
	unsigned kbase, nb;
	int ret, doabort;

	memset(dbuf, 'x', sizeof(dbuf));
	while (!stop) {
		if ((ret = env->txn_begin(env, NULL, &txn, DB_TXN_NOSYNC)) != 0)
			break;
		doabort = (int)(rand_r(&seed) % 100) < abort_pct;
		kbase = (unsigned)(rand_r(&seed) % keyrange);
		for (nb = 0; nb < (unsigned)batch; nb++) {
			(void)snprintf(kbuf, sizeof(kbuf), "%010u",
			    (kbase + nb) % keyrange);
			memset(&key, 0, sizeof(key));
			memset(&data, 0, sizeof(data));
			key.data = kbuf; key.size = (u_int32_t)strlen(kbuf);
			data.data = dbuf; data.size = sizeof(dbuf);
			if ((ret = db->put(db, txn, &key, &data, 0)) != 0)
				break;
		}
		if (ret != 0) {
			(void)txn->abort(txn);
			if (ret == DB_LOCK_DEADLOCK)
				s->deadlocks++;
			else
				s->errs++;
			continue;
		}
		if (doabort) {
			if ((ret = txn->abort(txn)) != 0)
				s->errs++;
			else
				s->aborts++;
		} else if ((ret = txn->commit(txn, 0)) != 0) {
			if (ret == DB_LOCK_DEADLOCK)
				s->deadlocks++;
			else
				s->errs++;
		} else
			s->commits++;
	}
	return (NULL);
}

int
main(int argc, char **argv)
{
	unsigned long c, ab, dl, er;
	int i, ret;

	if (argc < 4) {
		fprintf(stderr, "usage: %s <home> <nthreads> <secs>\n",
		    argv[0]);
		return (1);
	}
	nthreads = atoi(argv[2]);
	{ const char *e = getenv("KEYRANGE");
	  if (e != NULL && atoi(e) > 0) keyrange = (unsigned)atoi(e); }
	abort_pct = 50;
	{ const char *e = getenv("ABORT_PCT");
	  if (e != NULL) abort_pct = atoi(e); }
	{ const char *e = getenv("BATCH");
	  if (e != NULL && atoi(e) > 0) batch = atoi(e); }

	if ((ret = db_env_create(&env, 0)) != 0)
		goto err;
	(void)env->set_cachesize(env, 0, 512 * 1024 * 1024, 1);
	(void)env->set_lk_max_lockers(env, 20000);
	(void)env->set_lk_max_locks(env, 200000);
	(void)env->set_lk_max_objects(env, 200000);
	(void)env->set_lg_bsize(env, 8 * 1024 * 1024);
	(void)env->set_lk_detect(env, DB_LOCK_DEFAULT);
	env->set_errfile(env, stderr);
	if ((ret = env->open(env, argv[1], DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | DB_THREAD, 0644)) != 0)
		goto err;
	if ((ret = db_create(&db, env, 0)) != 0)
		goto err;
	if ((ret = db->open(db, NULL, "bench.db", NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT | DB_THREAD, 0644)) != 0)
		goto err;

	if ((ts = calloc((size_t)nthreads, sizeof(*ts))) == NULL)
		return (1);
	for (i = 0; i < nthreads; i++) {
		ts[i].id = i;
		if (pthread_create(&ts[i].th, NULL, worker, &ts[i]) != 0)
			return (1);
	}
	sleep((unsigned)atoi(argv[3]));
	stop = 1;
	for (i = 0; i < nthreads; i++)
		(void)pthread_join(ts[i].th, NULL);

	c = ab = dl = er = 0;
	for (i = 0; i < nthreads; i++) {
		c += ts[i].commits; ab += ts[i].aborts;
		dl += ts[i].deadlocks; er += ts[i].errs;
	}
	printf("abort_run threads=%d abort_pct=%d batch=%d commits=%lu "
	    "aborts=%lu deadlocks=%lu errors=%lu\n",
	    nthreads, abort_pct, batch, c, ab, dl, er);
	fflush(stdout);

	/*
	 * Close cleanly.  A corruption that only appears because the
	 * environment was killed mid-write would not be evidence about the
	 * lock rule, so the run must shut down properly and let db_verify
	 * judge a quiescent file.
	 */
	if ((ret = db->close(db, 0)) != 0)
		goto err;
	if ((ret = env->close(env, 0)) != 0)
		goto err;
	return (0);

err:	fprintf(stderr, "lsc_abort: %s\n", db_strerror(ret));
	return (1);
}
