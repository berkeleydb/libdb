/*-
 * batch_bench: A/B driver for db_get_multiple() (one API crossing, one cursor
 * for N keys) versus N individual DB->get() calls.  The SAME binary does both
 * arms; the arm is argv[1].
 *
 * usage: batch_bench <indiv|batch> <nkeys> <batchsz> <secs> <t1> [t2 ...]
 *
 * Both arms fetch BATCHSZ scattered random keys per iteration, so they do
 * identical logical work and differ only in how many API crossings and
 * cursor allocate/free pairs it costs.  Throughput is reported in KEYS/sec so
 * the arms are directly comparable, plus per-batch p50/p99 latency.
 *
 * $BATCH_HOME must name an existing, empty directory (this driver never
 * removes anything, so it is safe to run on a shared box).
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

static void
run(int nthreads, double secs)
{
	pthread_t th[MAXT];
	static targ_t ta[MAXT];
	uint64_t agg[NBUCK];
	double t0, dur;
	uint64_t total = 0, nf = 0;
	int i, b;

	memset(ta, 0, sizeof(ta));
	memset(agg, 0, sizeof(agg));
	stop = 0; go = 0;
	for (i = 0; i < nthreads; i++) {
		ta[i].seed = (unsigned)(i * 2654435761u + 12345);
		ta[i].tid = i;
		(void)pthread_create(&th[i], NULL, worker, &ta[i]);
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
		total += ta[i].keys;
		nf += ta[i].nf;
		for (b = 0; b < NBUCK; b++) agg[b] += ta[i].hist[b];
	}
	dur = now_sec() - t0;

	printf("RESULT mode=%s thr=%d batch=%d keys_per_sec=%.0f "
	    "batch_p50_us=%.1f batch_p99_us=%.1f notfound=%llu\n",
	    g_batch_mode ? "batch" : "indiv", nthreads, g_batchsz,
	    (double)total / dur, pctile(agg, 0.50), pctile(agg, 0.99),
	    (unsigned long long)nf);
	fflush(stdout);
}

int
main(int argc, char **argv)
{
	DBT key, data;
	uint32_t kb, i;
	char vbuf[VALSZ];
	const char *home;
	int ret, ai;

	if (argc < 6) {
		fprintf(stderr, "usage: %s <indiv|batch> <nkeys> <batchsz>"
		    " <secs> <t..>\n", argv[0]);
		return (1);
	}
	g_batch_mode = strcmp(argv[1], "batch") == 0;
	g_nkeys = (uint32_t)atoi(argv[2]);
	g_batchsz = atoi(argv[3]);
	if (g_batchsz < 1 || g_batchsz > MAXB) {
		fprintf(stderr, "batchsz must be 1..%d\n", MAXB);
		return (1);
	}
	if ((home = getenv("BATCH_HOME")) == NULL) {
		fprintf(stderr, "set BATCH_HOME to an existing empty dir\n");
		return (1);
	}

	if ((ret = db_env_create(&env, 0)) != 0) return (1);
	env->set_errfile(env, stderr);
	(void)env->set_cachesize(env, 0, 512 * 1024 * 1024, 1);
	if ((ret = env->open(env, home,
	    DB_CREATE | DB_INIT_MPOOL | DB_INIT_LOCK | DB_INIT_TXN |
	    DB_INIT_LOG | DB_THREAD, 0)) != 0) {
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
	printf("# loaded %u keys mode=%s batch=%d\n",
	    g_nkeys, argv[1], g_batchsz);

	for (ai = 5; ai < argc; ai++)
		run(atoi(argv[ai]), atof(argv[4]));

	(void)db->close(db, 0);
	(void)env->close(env, 0);
	return (0);
}
