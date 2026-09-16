/*
 * commit_bench -- measure commit throughput, fsyncs-per-commit and p99 commit
 * latency under N concurrent fully-durable (DB_TXN_SYNC) committers.
 *
 * usage: commit_bench <dir> <nthreads> <secs> [sync|nosync|wrnosync]
 *
 * Reports: commits, commits/sec, log flushes (st_scount), flushes/commit,
 *          max/min commits-per-flush, p50/p99/p999 commit latency in us.
 */
#include <sys/types.h>
#include <errno.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include "db.h"

static DB_ENV *env;
static DB *db;
static volatile int stop;
static int nthreads, secs, txnflag;

#define	MAXLAT	4000000
struct tstate {
	pthread_t th;
	int id;
	unsigned long commits;
	unsigned long *lat;		/* microseconds */
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
	double t0, t1;
	int ret;

	memset(dbuf, 'x', sizeof(dbuf));
	while (!stop) {
		(void)snprintf(kbuf, sizeof(kbuf), "%010u",
		    (unsigned)(rand_r(&seed) % 1000000));
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf);
		data.data = dbuf; data.size = sizeof(dbuf);

		t0 = now();
		if ((ret = env->txn_begin(env, NULL, &txn, txnflag)) != 0) {
			fprintf(stderr, "txn_begin: %s\n", db_strerror(ret));
			exit(1);
		}
		if ((ret = db->put(db, txn, &key, &data, 0)) != 0) {
			(void)txn->abort(txn);
			if (ret == DB_LOCK_DEADLOCK)
				continue;
			fprintf(stderr, "put: %s\n", db_strerror(ret));
			exit(1);
		}
		if ((ret = txn->commit(txn, 0)) != 0) {
			if (ret == DB_LOCK_DEADLOCK)
				continue;
			fprintf(stderr, "commit: %s\n", db_strerror(ret));
			exit(1);
		}
		t1 = now();
		s->commits++;
		if (s->nlat < MAXLAT)
			s->lat[s->nlat++] = (unsigned long)((t1 - t0) * 1e6);
	}
	return (NULL);
}

static int
cmpul(const void *a, const void *b)
{
	unsigned long x = *(const unsigned long *)a, y = *(const unsigned long *)b;
	return (x < y ? -1 : x > y ? 1 : 0);
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

	/* Reset log stats so the measured window is clean. */
	if ((ret = env->log_stat(env, &lsp, DB_STAT_CLEAR)) != 0)
		goto err;
	free(lsp);

	if ((ts = calloc((size_t)nthreads, sizeof(*ts))) == NULL)
		return (1);
	for (i = 0; i < (unsigned long)nthreads; i++) {
		ts[i].id = (int)i;
		if ((ts[i].lat = malloc(MAXLAT * sizeof(unsigned long))) == NULL)
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

	(void)db->close(db, 0);
	(void)env->close(env, 0);
	return (0);
err:
	fprintf(stderr, "error: %s\n", db_strerror(ret));
	return (1);
}
