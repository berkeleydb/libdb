/*-
 * pin_bench: read-path A/B driver for the buffer-header-pin re-measurement.
 *
 * A fork of test/bench/batch_bench.c with the four things that measurement
 * needs and batch_bench does not have:
 *
 *   1. DB_TXN_NOSYNC on the load, so loading N keys is not fsync-per-put.
 *   2. A warmup window before every measured window, so a cache-warming
 *      curve cannot be mistaken for a scaling curve.
 *   3. An optional DB_PRIVATE + set_thread_count environment ($PIN_PRIVATE),
 *      which is the ONLY configuration in which perf/bhpin-r1's optimistic
 *      fast path fires at all (it is gated on F_ISSET(env, ENV_PRIVATE)).
 *   4. A hard non-vacuity check: zero throughput, or any worker error, is
 *      reported as FAIL with a non-zero exit.  rc=0 alone never means pass.
 *
 * The API path under test is argv[1]: "indiv" = N x DB->get(), "batch" = one
 * db_get_multiple().  Both arms fetch the same BATCHSZ scattered random keys
 * per iteration and report KEYS/sec, so they are directly comparable.
 *
 * usage: pin_bench <indiv|batch> <nkeys> <batchsz> <warmup> <secs> <t1> [t2..]
 *
 * $PIN_HOME must name an existing directory.  This driver never removes
 * anything.
 */
#include <sys/types.h>
#include <errno.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "db.h"

#define	MAXT	128
#define	MAXB	256
#define	VALSZ	100
#define	NBUCK	64

static DB_ENV *env;
static DB *db;
static volatile int stop, go;
static int g_batch_mode, g_batchsz;
static uint32_t g_nkeys;
static const char *g_tag = "-";

typedef struct {
	uint64_t keys;			/* keys fetched */
	uint64_t nf;			/* not-founds */
	uint64_t hist[NBUCK];		/* log2 ns buckets */
	unsigned seed;
	int tid;
	int ret;
	char pad[64];
} targ_t;

static double
now_sec(void)
{
	struct timespec ts;
	clock_gettime(CLOCK_MONOTONIC, &ts);
	return (ts.tv_sec + ts.tv_nsec / 1e9);
}

static uint64_t
now_ns(void)
{
	struct timespec ts;
	clock_gettime(CLOCK_MONOTONIC, &ts);
	return ((uint64_t)ts.tv_sec * 1000000000ull + ts.tv_nsec);
}

static int
bucket_of(uint64_t ns)
{
	int b = 0;
	while (ns > 1 && b < NBUCK - 1) { ns >>= 1; b++; }
	return (b);
}

static void *
worker(void *a)
{
	targ_t *t = a;
	DBT keys[MAXB], datas[MAXB];
	uint32_t kb[MAXB];
	int rets[MAXB];
	char vbuf[MAXB][VALSZ + 8];
	uint64_t t0, t1;
	int i, ret;

	while (!go) { }
	while (!stop) {
		for (i = 0; i < g_batchsz; i++) {
			kb[i] = (uint32_t)(rand_r(&t->seed) % g_nkeys);
			memset(&keys[i], 0, sizeof(DBT));
			keys[i].data = &kb[i];
			keys[i].size = sizeof(kb[i]);
			memset(&datas[i], 0, sizeof(DBT));
			datas[i].data = vbuf[i];
			datas[i].ulen = sizeof(vbuf[i]);
			datas[i].flags = DB_DBT_USERMEM;
		}
		t0 = now_ns();
		if (g_batch_mode) {
			ret = db_get_multiple(db, NULL, keys, datas, rets,
			    (u_int32_t)g_batchsz, 0);
			if (ret != 0 && ret != DB_NOTFOUND) {
				env->err(env, ret, "db_get_multiple");
				t->ret = ret;
				return (NULL);
			}
			for (i = 0; i < g_batchsz; i++)
				if (rets[i] == DB_NOTFOUND)
					t->nf++;
		} else {
			for (i = 0; i < g_batchsz; i++) {
				ret = db->get(db, NULL, &keys[i], &datas[i], 0);
				if (ret == DB_NOTFOUND) { t->nf++; continue; }
				if (ret != 0) {
					env->err(env, ret, "DB->get");
					t->ret = ret;
					return (NULL);
				}
			}
		}
		t1 = now_ns();
		t->hist[bucket_of(t1 - t0)]++;
		t->keys += (uint64_t)g_batchsz;
	}
	return (NULL);
}

static double
pctile(uint64_t hist[NBUCK], double p)
{
	uint64_t tot = 0, acc = 0;
	int b;

	for (b = 0; b < NBUCK; b++) tot += hist[b];
	if (tot == 0) return (0.0);
	for (b = 0; b < NBUCK; b++) {
		acc += hist[b];
		if ((double)acc / (double)tot >= p)
			return (((double)(1ull << b)) / 1000.0); /* usec */
	}
	return (0.0);
}

/*
 * one_window --
 *	Run nthreads workers for secs seconds.  Returns keys/sec, or a negative
 *	value if any worker failed.  Aggregated latency histogram in *agg.
 */
static double
one_window(int nthreads, double secs, uint64_t agg[NBUCK], uint64_t *nfp,
    double *p50, double *p99)
{
	pthread_t th[MAXT];
	static targ_t ta[MAXT];
	double t0, dur;
	uint64_t total = 0, nf = 0;
	int i, b, failed = 0;

	memset(ta, 0, sizeof(ta));
	memset(agg, 0, sizeof(uint64_t) * NBUCK);
	stop = 0; go = 0;
	for (i = 0; i < nthreads; i++) {
		ta[i].seed = (unsigned)(i * 2654435761u + 12345);
		ta[i].tid = i;
		if (pthread_create(&th[i], NULL, worker, &ta[i]) != 0) {
			fprintf(stderr, "pthread_create failed\n");
			return (-1.0);
		}
	}
	go = 1;
	t0 = now_sec();
	{
		struct timespec sl = { (time_t)secs,
		    (long)((secs - (long)secs) * 1e9) };
		(void)nanosleep(&sl, NULL);
	}
	stop = 1;
	for (i = 0; i < nthreads; i++) {
		(void)pthread_join(th[i], NULL);
		if (ta[i].ret != 0)
			failed = 1;
		total += ta[i].keys;
		nf += ta[i].nf;
		for (b = 0; b < NBUCK; b++) agg[b] += ta[i].hist[b];
	}
	dur = now_sec() - t0;
	if (failed || dur <= 0.0)
		return (-1.0);
	*nfp = nf;
	*p50 = pctile(agg, 0.50);
	*p99 = pctile(agg, 0.99);
	return ((double)total / dur);
}

/* Returns 0 on success, 1 if the point produced no usable verdict. */
static int
run(int nthreads, double warmup, double secs)
{
	uint64_t agg[NBUCK], nf;
	double kps, p50, p99;

	/* Warmup: same work, result discarded, so we measure steady state. */
	if (warmup > 0.0 &&
	    one_window(nthreads, warmup, agg, &nf, &p50, &p99) < 0.0) {
		printf("FAIL tag=%s mode=%s thr=%d warmup window failed\n",
		    g_tag, g_batch_mode ? "batch" : "indiv", nthreads);
		return (1);
	}

	kps = one_window(nthreads, secs, agg, &nf, &p50, &p99);
	if (kps <= 0.0) {
		printf("FAIL tag=%s mode=%s thr=%d no throughput\n",
		    g_tag, g_batch_mode ? "batch" : "indiv", nthreads);
		return (1);
	}
	printf("RESULT tag=%s mode=%s thr=%d batch=%d keys_per_sec=%.0f "
	    "batch_p50_us=%.1f batch_p99_us=%.1f notfound=%llu\n",
	    g_tag, g_batch_mode ? "batch" : "indiv", nthreads, g_batchsz,
	    kps, p50, p99, (unsigned long long)nf);
	fflush(stdout);
	return (0);
}

int
main(int argc, char **argv)
{
	DBT key, data;
	uint32_t kb, i;
	u_int32_t oflags;
	char vbuf[VALSZ];
	const char *home, *s;
	double warmup, secs;
	int ret, ai, maxt, private, cache_mb, bad = 0;

	if (argc < 7) {
		fprintf(stderr, "usage: %s <indiv|batch> <nkeys> <batchsz>"
		    " <warmup_secs> <secs> <t..>\n", argv[0]);
		return (1);
	}
	g_batch_mode = strcmp(argv[1], "batch") == 0;
	if (!g_batch_mode && strcmp(argv[1], "indiv") != 0) {
		fprintf(stderr, "arm must be indiv or batch\n");
		return (1);
	}
	g_nkeys = (uint32_t)atoi(argv[2]);
	g_batchsz = atoi(argv[3]);
	warmup = atof(argv[4]);
	secs = atof(argv[5]);
	if (g_nkeys < 1000 || g_batchsz < 1 || g_batchsz > MAXB || secs <= 0.0) {
		fprintf(stderr, "bad nkeys/batchsz/secs\n");
		return (1);
	}
	if ((home = getenv("PIN_HOME")) == NULL) {
		fprintf(stderr, "set PIN_HOME to an existing dir\n");
		return (1);
	}
	if ((s = getenv("PIN_TAG")) != NULL)
		g_tag = s;
	private = getenv("PIN_PRIVATE") != NULL;
	cache_mb = (s = getenv("PIN_CACHE_MB")) != NULL ? atoi(s) : 512;
	if (cache_mb < 16) {
		fprintf(stderr, "PIN_CACHE_MB too small\n");
		return (1);
	}

	maxt = 1;
	for (ai = 6; ai < argc; ai++)
		if (atoi(argv[ai]) > maxt)
			maxt = atoi(argv[ai]);
	if (maxt > MAXT) {
		fprintf(stderr, "thread count > %d\n", MAXT);
		return (1);
	}

	if ((ret = db_env_create(&env, 0)) != 0) return (1);
	env->set_errfile(env, stderr);
	(void)env->set_cachesize(env,
	    (u_int32_t)(cache_mb / 1024), (u_int32_t)(cache_mb % 1024) *
	    1024 * 1024, cache_mb >= 1024 ? cache_mb / 1024 : 1);
	/*
	 * DB_TXN_NOSYNC: the load is autocommit db->put, so without this every
	 * put costs a log flush and loading is glacial.  Reads are unaffected.
	 */
	(void)env->set_flags(env, DB_TXN_NOSYNC, 1);
	oflags = DB_CREATE | DB_INIT_MPOOL | DB_INIT_LOCK | DB_INIT_TXN |
	    DB_INIT_LOG | DB_THREAD;
	if (private) {
		oflags |= DB_PRIVATE;
		/* Needed so a DB_PRIVATE env has per-thread info. */
		(void)env->set_thread_count(env, (u_int32_t)maxt + 16);
	}
	if ((ret = env->open(env, home, oflags, 0)) != 0) {
		env->err(env, ret, "env open %s", home);
		return (1);
	}
	if ((ret = db_create(&db, env, 0)) != 0) return (1);
	if ((ret = db->open(db, NULL, "bench.db", NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT | DB_THREAD, 0)) != 0) {
		env->err(env, ret, "db open"); return (1);
	}
	memset(vbuf, 'v', sizeof(vbuf));
	for (i = 0; i < g_nkeys; i++) {
		memset(&key, 0, sizeof(key));
		kb = i; key.data = &kb; key.size = sizeof(kb);
		memset(&data, 0, sizeof(data));
		data.data = vbuf; data.size = sizeof(vbuf);
		if ((ret = db->put(db, NULL, &key, &data, 0)) != 0) {
			env->err(env, ret, "load"); return (1);
		}
	}
	printf("# tag=%s loaded %u keys mode=%s batch=%d env=%s cache=%dMB\n",
	    g_tag, g_nkeys, argv[1], g_batchsz, private ? "private" : "shared",
	    cache_mb);
	fflush(stdout);

	for (ai = 6; ai < argc; ai++)
		bad += run(atoi(argv[ai]), warmup, secs);

	(void)db->close(db, 0);
	(void)env->close(env, 0);
	if (bad != 0) {
		printf("FAIL tag=%s %d point(s) produced no verdict\n",
		    g_tag, bad);
		return (1);
	}
	return (0);
}
