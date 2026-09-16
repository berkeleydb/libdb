/*
 * commit_bench -- measure commit throughput, fsyncs-per-commit and commit
 * latency percentiles under N concurrent fully-durable (DB_TXN_SYNC)
 * committers.
 *
 * usage: commit_bench <dir> <nthreads> <secs> [sync|nosync|wrnosync]
 * env:   KEYRANGE=<n>  key space size (default 1000000)
 *        PREPOP=0      skip warmup and measure the insert ramp instead
 *
 * Reports: commits, commits/sec, log flushes (st_scount), flushes/commit,
 *          max/min commits-per-flush, whole-transaction latency percentiles,
 *          and a PER-PHASE breakdown of begin/put/commit.  Against a library
 *          built --enable-handoff-trace it also dumps the group-commit handoff
 *          histograms.
 *
 * TWO THINGS THIS DRIVER GOT WRONG, both of which produced a published
 * conclusion about the wrong mechanism (see test/bench/WRITE-TAIL-2026-09.md):
 *
 * 1. It measured an EMPTY database filling up.  Every put was an insert into a
 *    growing btree, so the run was dominated by page splits and the numbers
 *    described a load ramp, not steady state.  It now prepopulates the key
 *    range first; PREPOP=0 restores the old behaviour for comparison, and says
 *    so on stdout so a ramp measurement can never be mistaken for a steady
 *    one.
 *
 * 2. It timed txn_begin + put + commit as a single interval and labelled the
 *    result "commit latency".  That attributed to the durable commit a tail
 *    that in fact belonged to page-lock contention inside put: at 96 threads
 *    the whole-transaction p99 was 272ms while the commit phase's own p99 was
 *    5.7ms.  The phases are now timed and reported separately.
 */
#include <sys/types.h>
#include <stddef.h>
#include <errno.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include "db.h"

/*
 * Weak: defined only by a library built --enable-handoff-trace.  Declaring it
 * weak lets ONE driver serve both builds -- against a stock library the symbol
 * is NULL and the handoff dump is skipped, rather than needing a duplicate
 * driver that drifts out of step with this one.
 */
#pragma weak __db_hoff_dump
extern void __db_hoff_dump(const char *);

static DB_ENV *env;
static DB *db;
static volatile int stop;
static int nthreads, secs, txnflag;
static unsigned keyrange = 1000000;

/*
 * Prepopulate the key range so the measured window is steady state.
 *
 * Without this every put is an insert into a growing btree and the run
 * measures page splits and new-page allocation.  That is not a cosmetic
 * difference: splits take locks an overwrite never touches, and holding one
 * across a ~3.7ms durable commit convoys every other writer behind it, which
 * is precisely the effect that was previously misread as commit-path
 * starvation.  Populated single-threaded under DB_TXN_NOSYNC -- durability of
 * the setup is irrelevant and fsyncing every insert would take hours -- then
 * the log is flushed so nothing from setup is left unflushed when measurement
 * begins.
 */
static int
prepopulate(void)
{
	DB_TXN *txn;
	DBT key, data;
	char kbuf[32], dbuf[128];
	unsigned i, batch;
	int ret;

	memset(dbuf, 'x', sizeof(dbuf));
	batch = 0;
	txn = NULL;
	for (i = 0; i < keyrange; i++) {
		if (txn == NULL && (ret = env->txn_begin(env, NULL, &txn,
		    DB_TXN_NOSYNC)) != 0)
			return (ret);
		(void)snprintf(kbuf, sizeof(kbuf), "%010u", i);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf);
		data.data = dbuf; data.size = sizeof(dbuf);
		if ((ret = db->put(db, txn, &key, &data, 0)) != 0) {
			(void)txn->abort(txn);
			return (ret);
		}
		if (++batch >= 1000) {
			if ((ret = txn->commit(txn, DB_TXN_NOSYNC)) != 0)
				return (ret);
			txn = NULL;
			batch = 0;
		}
	}
	if (txn != NULL && (ret = txn->commit(txn, DB_TXN_NOSYNC)) != 0)
		return (ret);
	return (env->log_flush(env, NULL));
}

#define	MAXLAT	4000000
struct tstate {
	pthread_t th;
	int id;
	unsigned long commits;
	unsigned long *lat;		/* whole txn: begin+put+commit, us */
	unsigned long *lat_begin;	/* txn_begin alone */
	unsigned long *lat_put;		/* db->put alone (takes page locks) */
	unsigned long *lat_commit;	/* txn->commit alone (logs and flushes) */
	unsigned long nlat;
};
static struct tstate *ts;

static double
now(void)
{
	struct timespec t;
	(void)clock_gettime(CLOCK_MONOTONIC, &t);
	return (t.tv_sec + t.tv_nsec / 1e9);
}

static void *
worker(void *arg)
{
	struct tstate *s = arg;
	DB_TXN *txn;
	DBT key, data;
	char kbuf[32], dbuf[128];
	unsigned int seed = (unsigned int)(s->id * 7919 + 13);
	double t0, tb, tp, t1;
	int ret;

	memset(dbuf, 'x', sizeof(dbuf));
	while (!stop) {
		(void)snprintf(kbuf, sizeof(kbuf), "%010u",
		    (unsigned)(rand_r(&seed) % keyrange));
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf);
		data.data = dbuf; data.size = sizeof(dbuf);

		t0 = now();
		if ((ret = env->txn_begin(env, NULL, &txn, txnflag)) != 0) {
			fprintf(stderr, "txn_begin: %s\n", db_strerror(ret));
			exit(1);
		}
		tb = now();
		if ((ret = db->put(db, txn, &key, &data, 0)) != 0) {
			(void)txn->abort(txn);
			if (ret == DB_LOCK_DEADLOCK)
				continue;
			fprintf(stderr, "put: %s\n", db_strerror(ret));
			exit(1);
		}
		tp = now();
		if ((ret = txn->commit(txn, 0)) != 0) {
			if (ret == DB_LOCK_DEADLOCK)
				continue;
			fprintf(stderr, "commit: %s\n", db_strerror(ret));
			exit(1);
		}
		t1 = now();
		s->commits++;
		if (s->nlat < MAXLAT) {
			s->lat_begin[s->nlat] = (unsigned long)((tb - t0) * 1e6);
			s->lat_put[s->nlat] = (unsigned long)((tp - tb) * 1e6);
			s->lat_commit[s->nlat] =
			    (unsigned long)((t1 - tp) * 1e6);
			s->lat[s->nlat++] = (unsigned long)((t1 - t0) * 1e6);
		}
	}
	return (NULL);
}

static int
cmpul(const void *a, const void *b)
{
	unsigned long x = *(const unsigned long *)a, y = *(const unsigned long *)b;
	return (x < y ? -1 : x > y ? 1 : 0);
}

/* Pool one per-thread array across every thread and print its percentiles. */
static void
phase(const char *tag, size_t off, unsigned long nall)
{
	unsigned long *v, i, j;

	if (nall == 0 || (v = malloc(nall * sizeof(*v))) == NULL)
		return;
	for (i = 0, j = 0; i < (unsigned long)nthreads; i++) {
		memcpy(v + j, *(unsigned long **)((char *)&ts[i] + off),
		    ts[i].nlat * sizeof(*v));
		j += ts[i].nlat;
	}
	qsort(v, nall, sizeof(*v), cmpul);
	printf("%s_us p50=%lu p90=%lu p99=%lu p999=%lu max=%lu\n", tag,
	    v[nall * 50 / 100], v[nall * 90 / 100], v[nall * 99 / 100],
	    v[nall * 999 / 1000], v[nall - 1]);
	free(v);
}

int
main(int argc, char **argv)
{
	DB_LOG_STAT *lsp;
	unsigned long total, *all, nall, i, j;
	double t0, elapsed;
	const char *mode;
	int ret;

	if (argc < 4) {
		fprintf(stderr,
		    "usage: %s <dir> <nthreads> <secs> [sync|nosync|wrnosync]\n",
		    argv[0]);
		return (1);
	}
	nthreads = atoi(argv[2]);
	secs = atoi(argv[3]);
	mode = argc > 4 ? argv[4] : "sync";
	{ const char *kr = getenv("KEYRANGE");
	  if (kr != NULL && atoi(kr) > 0)
		keyrange = (unsigned)atoi(kr); }
	if (strcmp(mode, "nosync") == 0)
		txnflag = DB_TXN_NOSYNC;
	else if (strcmp(mode, "wrnosync") == 0)
		txnflag = DB_TXN_WRITE_NOSYNC;
	else
		txnflag = DB_TXN_SYNC;

	if ((ret = db_env_create(&env, 0)) != 0)
		goto err;
	(void)env->set_cachesize(env, 0, 512 * 1024 * 1024, 1);
	(void)env->set_lk_max_lockers(env, 20000);
	(void)env->set_lk_max_locks(env, 200000);
	(void)env->set_lk_max_objects(env, 200000);
	(void)env->set_lg_bsize(env, 8 * 1024 * 1024);
	env->set_errfile(env, stderr);
	if ((ret = env->open(env, argv[1], DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | DB_THREAD, 0644)) != 0)
		goto err;
	if ((ret = db_create(&db, env, 0)) != 0)
		goto err;
	if ((ret = db->open(db, NULL, "bench.db", NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT | DB_THREAD, 0644)) != 0)
		goto err;

	{ const char *pp = getenv("PREPOP");
	  if (pp == NULL || atoi(pp) != 0) {
		double pt0 = now();
		if ((ret = prepopulate()) != 0)
			goto err;
		printf("prepopulated keys=%u in %.1fs\n", keyrange,
		    now() - pt0);
	  } else
		printf("prepopulate SKIPPED (PREPOP=0): measuring the insert "
		    "ramp, NOT steady state\n");
	  fflush(stdout);
	}

	/* Reset log stats so the measured window is clean. */
	if ((ret = env->log_stat(env, &lsp, DB_STAT_CLEAR)) != 0)
		goto err;
	free(lsp);

	if ((ts = calloc((size_t)nthreads, sizeof(*ts))) == NULL)
		return (1);
	for (i = 0; i < (unsigned long)nthreads; i++) {
		ts[i].id = (int)i;
		if ((ts[i].lat =
		    malloc(MAXLAT * sizeof(unsigned long))) == NULL ||
		    (ts[i].lat_begin =
		    malloc(MAXLAT * sizeof(unsigned long))) == NULL ||
		    (ts[i].lat_put =
		    malloc(MAXLAT * sizeof(unsigned long))) == NULL ||
		    (ts[i].lat_commit =
		    malloc(MAXLAT * sizeof(unsigned long))) == NULL)
			return (1);
	}

	t0 = now();
	for (i = 0; i < (unsigned long)nthreads; i++)
		(void)pthread_create(&ts[i].th, NULL, worker, &ts[i]);
	while (now() - t0 < secs)
		usleep(50000);
	stop = 1;
	for (i = 0; i < (unsigned long)nthreads; i++)
		(void)pthread_join(ts[i].th, NULL);
	elapsed = now() - t0;

	if ((ret = env->log_stat(env, &lsp, 0)) != 0)
		goto err;

	total = nall = 0;
	for (i = 0; i < (unsigned long)nthreads; i++) {
		total += ts[i].commits;
		nall += ts[i].nlat;
	}
	if ((all = malloc((nall ? nall : 1) * sizeof(unsigned long))) == NULL)
		return (1);
	for (i = 0, j = 0; i < (unsigned long)nthreads; i++) {
		memcpy(all + j, ts[i].lat,
		    ts[i].nlat * sizeof(unsigned long));
		j += ts[i].nlat;
	}
	qsort(all, nall, sizeof(unsigned long), cmpul);

	printf("mode=%s threads=%d secs=%.2f commits=%lu ops_sec=%.0f "
	    "flushes=%ju flush_per_commit=%.4f maxcpf=%lu mincpf=%lu "
	    "p50_us=%lu p99_us=%lu p999_us=%lu\n",
	    mode, nthreads, elapsed, total, total / elapsed,
	    (uintmax_t)lsp->st_scount,
	    total ? (double)lsp->st_scount / (double)total : 0.0,
	    (unsigned long)lsp->st_maxcommitperflush,
	    (unsigned long)lsp->st_mincommitperflush,
	    nall ? all[nall * 50 / 100] : 0,
	    nall ? all[nall * 99 / 100] : 0,
	    nall ? all[(nall * 999) / 1000] : 0);
	free(lsp);

	/*
	 * Per-phase percentiles.  The whole-transaction number above cannot say
	 * whether a large p99 was spent making the commit durable or waiting
	 * for a page lock another thread holds across ITS commit; these can.
	 */
	phase("PHASE begin", offsetof(struct tstate, lat_begin), nall);
	phase("PHASE put", offsetof(struct tstate, lat_put), nall);
	phase("PHASE commit", offsetof(struct tstate, lat_commit), nall);
	fflush(stdout);

	/* Handoff histograms, if this library was built to record them. */
	if (__db_hoff_dump != NULL)
		__db_hoff_dump("-");

	(void)db->close(db, 0);
	(void)env->close(env, 0);
	return (0);
err:
	fprintf(stderr, "error: %s\n", db_strerror(ret));
	return (1);
}
