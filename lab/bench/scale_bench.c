/*-
 * libdb scaling probe: drive a shared environment from T threads and read
 * BDB's own wait counters to see which subsystem serializes as cores scale.
 *
 * Workloads:
 *   rrand  - read random keys      (exposes lock-mgr read locks + mpool hash)
 *   rhot   - read ONE hot key       (forces single-page / single-bucket contention)
 *   wrand  - write random keys      (exposes log region + write locks + mpool)
 *
 * For each (workload, threads) run it resets the mpool/lock/mutex stats, runs
 * for a fixed wall-clock, then prints throughput and the wait ratios so the
 * bottleneck is visible.
 *
 *   cc -O2 -pthread scale_bench.c -I<build> -L<build>/.libs -ldb-5.3 -o scale_bench
 *   ./scale_bench <workload> <nkeys> <secs> <t1> [t2 ...]
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

static DB_ENV *env;
static DB *db;
static DB *g_dbs[256];	/* per-thread DBs for the sepdb workload */
static int g_maxthreads;
static volatile int stop;
static volatile int go;		/* portable start gate (pthread_barrier is optional in POSIX) */
static int g_workload;	/* 0=rrand 1=rhot 2=wrand */
static uint32_t g_nkeys;

enum { W_RRAND = 0, W_RHOT = 1, W_WRAND = 2, W_SEPDB = 3, W_SNAP = 4 };

static double
now_sec(void)
{
	struct timespec ts;
	clock_gettime(CLOCK_MONOTONIC, &ts);
	return (ts.tv_sec + ts.tv_nsec / 1e9);
}

static void
fill_key(DBT *k, uint32_t *kb, uint32_t v)
{
	*kb = v;
	memset(k, 0, sizeof(*k));
	k->data = kb;
	k->size = sizeof(*kb);
}

typedef struct { uint64_t ops; unsigned seed; int tid; } targ_t;

static void *
worker(void *a)
{
	targ_t *t = a;
	DB *mydb = (g_workload == W_SEPDB) ? g_dbs[t->tid] : db;
	DB_TXN *txn = NULL;
	DBT key, data;
	uint32_t kb, vbuf[32];
	char wbuf[100];
	int ret;

	memset(wbuf, 'x', sizeof(wbuf));
	while (!go) { /* spin until released */ }
	if (g_workload == W_SNAP)
		(void)env->txn_begin(env, NULL, &txn, DB_TXN_SNAPSHOT);
	while (!stop) {
		uint32_t k = (g_workload == W_RHOT) ? 0 :
		    (uint32_t)(rand_r(&t->seed) % g_nkeys);
		fill_key(&key, &kb, k);
		if (g_workload == W_WRAND) {
			memset(&data, 0, sizeof(data));
			data.data = wbuf; data.size = sizeof(wbuf);
			ret = db->put(db, NULL, &key, &data, 0);
		} else {
			memset(&data, 0, sizeof(data));
			data.data = vbuf; data.ulen = sizeof(vbuf);
			data.flags = DB_DBT_USERMEM;
			ret = mydb->get(mydb, txn, &key, &data, 0);
			if (ret == DB_BUFFER_SMALL) ret = 0;
		}
		if (ret != 0 && ret != DB_NOTFOUND) {
			env->err(env, ret, "op k=%u", k);
			return (NULL);
		}
		t->ops++;
	}
	if (txn != NULL)
		(void)txn->commit(txn, 0);
	return (NULL);
}

static void
run(int nthreads, double secs)
{
	pthread_t th[256];
	targ_t ta[256];
	DB_MPOOL_STAT *mp;
	DB_LOCK_STAT *lk;
	DB_MUTEX_STAT *mx;
	double t0, dur;
	uint64_t total = 0;
	int i;

	/* Reset wait counters. */
	(void)env->memp_stat(env, &mp, NULL, DB_STAT_CLEAR); free(mp);
	(void)env->lock_stat(env, &lk, DB_STAT_CLEAR); free(lk);
	(void)env->mutex_stat(env, &mx, DB_STAT_CLEAR); free(mx);

	stop = 0;
	go = 0;
	for (i = 0; i < nthreads; i++) {
		ta[i].ops = 0; ta[i].seed = (unsigned)(i * 2654435761u + 1); ta[i].tid = i;
		pthread_create(&th[i], NULL, worker, &ta[i]);
	}
	go = 1;
	t0 = now_sec();
	struct timespec sl = { (time_t)secs, (long)((secs - (long)secs) * 1e9) };
	nanosleep(&sl, NULL);
	stop = 1;
	for (i = 0; i < nthreads; i++) { pthread_join(th[i], NULL); total += ta[i].ops; }
	dur = now_sec() - t0;

	(void)env->memp_stat(env, &mp, NULL, 0);
	(void)env->lock_stat(env, &lk, 0);
	(void)env->mutex_stat(env, &mx, 0);

#define PCT(w, nw) (((w) + (nw)) ? 100.0 * (double)(w) / ((double)(w) + (double)(nw)) : 0.0)
	printf("%-6s %3d %12.0f  conflict%%=%4.1f lockpart%%=%4.1f "
	    "mpoolhash%%=%4.1f objs%%=%4.1f lockers%%=%4.1f  "
	    "lockreg_w=%llu mpoolreg_w=%llu mtxreg_w=%llu npart=%u\n",
	    g_workload == W_RRAND ? "rrand" : g_workload == W_RHOT ? "rhot" :
	    g_workload == W_SEPDB ? "sepdb" : g_workload == W_SNAP ? "snap" : "wrand",
	    nthreads, total / dur,
	    PCT(lk->st_lock_wait, lk->st_lock_nowait),
	    PCT(lk->st_part_wait, lk->st_part_nowait),
	    PCT(mp->st_hash_wait, mp->st_hash_nowait),
	    PCT(lk->st_objs_wait, lk->st_objs_nowait),
	    PCT(lk->st_lockers_wait, lk->st_lockers_nowait),
	    (unsigned long long)lk->st_region_wait,
	    (unsigned long long)mp->st_region_wait,
	    (unsigned long long)mx->st_region_wait,
	    lk->st_partitions);
	fflush(stdout);
	free(mp); free(lk); free(mx);
}

int
main(int argc, char **argv)
{
	DBT key, data;
	uint32_t kb, i;
	char vbuf[100];
	int ret, ai;

	if (argc < 5) {
		fprintf(stderr, "usage: %s <rrand|rhot|wrand> <nkeys> <secs> <t..>\n", argv[0]);
		return (1);
	}
	g_workload = strcmp(argv[1], "rhot") == 0 ? W_RHOT :
	    strcmp(argv[1], "wrand") == 0 ? W_WRAND :
	    strcmp(argv[1], "sepdb") == 0 ? W_SEPDB :
	    strcmp(argv[1], "snap") == 0 ? W_SNAP : W_RRAND;
	g_nkeys = (uint32_t)atoi(argv[2]);
	double secs = atof(argv[3]);

	system("rm -rf ./SCALEDB && mkdir ./SCALEDB");
	if ((ret = db_env_create(&env, 0)) != 0) { fprintf(stderr, "env_create %d\n", ret); return 1; }
	env->set_errfile(env, stderr);
	env->set_cachesize(env, 0, 512 * 1024 * 1024, 1);	/* 512MB: all in cache */
	if ((ret = env->open(env, "./SCALEDB",
	    DB_CREATE | DB_INIT_MPOOL | DB_INIT_LOCK | DB_INIT_TXN | DB_INIT_LOG |
	    DB_THREAD, 0)) != 0) { env->err(env, ret, "env open"); return 1; }

	/* Highest thread count in the sweep -> how many per-thread DBs to make. */
	for (ai = 4; ai < argc; ai++)
		if (atoi(argv[ai]) > g_maxthreads) g_maxthreads = atoi(argv[ai]);

	memset(vbuf, 'v', sizeof(vbuf));
	if (g_workload == W_SEPDB) {
		int d;
		char nm[32];
		for (d = 0; d < g_maxthreads; d++) {
			if ((ret = db_create(&g_dbs[d], env, 0)) != 0) { env->err(env, ret, "db_create"); return 1; }
			snprintf(nm, sizeof(nm), "bench_%d.db", d);
			if ((ret = g_dbs[d]->open(g_dbs[d], NULL, nm, NULL, DB_BTREE,
			    DB_CREATE | DB_AUTO_COMMIT | DB_THREAD, 0)) != 0) { env->err(env, ret, "db open"); return 1; }
			for (i = 0; i < g_nkeys; i++) {
				fill_key(&key, &kb, i);
				memset(&data, 0, sizeof(data)); data.data = vbuf; data.size = sizeof(vbuf);
				if ((ret = g_dbs[d]->put(g_dbs[d], NULL, &key, &data, 0)) != 0) { env->err(env, ret, "load"); return 1; }
			}
		}
		printf("# sepdb: %d separate DBs x %u keys; secs=%.1f\n", g_maxthreads, g_nkeys, secs);
	} else {
		if ((ret = db_create(&db, env, 0)) != 0) { env->err(env, ret, "db_create"); return 1; }
		if ((ret = db->open(db, NULL, "bench.db", NULL, DB_BTREE,
		    DB_CREATE | DB_AUTO_COMMIT | DB_THREAD, 0)) != 0) { env->err(env, ret, "db open"); return 1; }
		for (i = 0; i < g_nkeys; i++) {
			fill_key(&key, &kb, i);
			memset(&data, 0, sizeof(data)); data.data = vbuf; data.size = sizeof(vbuf);
			if ((ret = db->put(db, NULL, &key, &data, 0)) != 0) { env->err(env, ret, "load"); return 1; }
		}
		if (g_workload == W_SNAP) {
			/* Reopen multiversion for snapshot reads (bulk load above
			 * used a plain handle to avoid MVCC version retention). */
			db->close(db, 0); db = NULL;
			if ((ret = db_create(&db, env, 0)) != 0) { env->err(env, ret, "db_create"); return 1; }
			if ((ret = db->open(db, NULL, "bench.db", NULL, DB_BTREE,
			    DB_AUTO_COMMIT | DB_THREAD | DB_MULTIVERSION, 0)) != 0) { env->err(env, ret, "db reopen mv"); return 1; }
		}
		printf("# loaded %u keys; workload=%s secs=%.1f\n", g_nkeys, argv[1], secs);
	}
	printf("# wkld  thr      ops/sec  contention signals\n");

	for (ai = 4; ai < argc; ai++)
		run(atoi(argv[ai]), secs);

	if (db) db->close(db, 0);
	env->close(env, 0);
	return (0);
}
