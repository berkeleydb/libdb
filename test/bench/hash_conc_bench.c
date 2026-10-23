/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*-
 * hash_conc_bench -- RFC 0017 change 1 concurrency benchmark.
 *
 * Drives one shared database from T threads and reports throughput, latency
 * percentiles (p50/p99/p999) and the worst single-op stall, so the effect of
 * dropping the per-operation hash metapage lock is visible.  It also prints
 * the lock-manager deadlock and wait counters, because the safety argument for
 * change 1 (hazard H1) is that the deadlock rate does not rise.
 *
 * Axes (each a separate process invocation):
 *   type  = hash | btree        (btree is the control: change 1 must not move it)
 *   mix   = insert | lookup      (insert is split-inducing; lookup is read-heavy)
 *   txn   = txn | notxn          (transactional vs. autocommit-free)
 *   T     = 1 2 4 8 16 32        (thread counts, passed on the command line)
 *
 * A small fill factor (set_h_ffactor) forces frequent bucket splits in the
 * insert mix, which is exactly where the old whole-table metapage WRITE fence
 * used to serialise everyone.
 *
 * Usage:
 *   ./hash_conc_bench <hash|btree> <insert|lookup> <txn|notxn> \
 *       <keyspace> <secs> <t1> [t2 ...]
 *
 * Output is one TSV line per thread count plus a leading comment, so the
 * harness (run_hash_conc.sh) can collect it directly.
 */
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include "db.h"
#include "db_quickselect.h"

enum { T_HASH = 0, T_BTREE = 1 };
enum { M_INSERT = 0, M_LOOKUP = 1 };

static DB_ENV *env;
static DB *db;
static volatile int stop;
static volatile int go;

static int g_type;
static int g_mix;
static int g_txn;
static uint32_t g_keyspace;

#define	MAXTHREADS	256
#define	MAXLAT		2000000		/* per-thread latency samples kept */

typedef struct {
	uint64_t ops;			/* committed/succeeded ops (goodput) */
	uint64_t aborts;		/* deadlock-aborted attempts (no work) */
	uint64_t *lat;			/* nanoseconds per SUCCEEDED op */
	uint32_t nlat;
	unsigned seed;
	int tid;
} targ_t;

static targ_t ts[MAXTHREADS];

static double
now_sec(void)
{
	struct timespec t;
	(void)clock_gettime(CLOCK_MONOTONIC, &t);
	return (t.tv_sec + t.tv_nsec / 1e9);
}

static uint64_t
now_ns(void)
{
	struct timespec t;
	(void)clock_gettime(CLOCK_MONOTONIC, &t);
	return ((uint64_t)t.tv_sec * 1000000000ull + (uint64_t)t.tv_nsec);
}

static int
cmp_u64(const void *a, const void *b)
{
	uint64_t x = *(const uint64_t *)a, y = *(const uint64_t *)b;
	return (x < y ? -1 : x > y ? 1 : 0);
}

static void *
worker(void *a)
{
	targ_t *t = a;
	DB_TXN *txn;
	DBT key, data;
	uint32_t kb;
	char vbuf[32], rbuf[256];
	uint64_t t0, t1;
	int ret;

	memset(vbuf, 'v', sizeof(vbuf));
	while (!go)
		;
	while (!stop) {
		kb = (uint32_t)(rand_r(&t->seed) % g_keyspace);
		memset(&key, 0, sizeof(key));
		key.data = &kb;
		key.size = sizeof(kb);
		memset(&data, 0, sizeof(data));

		t0 = now_ns();
		txn = NULL;
		if (g_txn && (ret = env->txn_begin(env, NULL, &txn, 0)) != 0) {
			env->err(env, ret, "txn_begin");
			return (NULL);
		}
		if (g_mix == M_INSERT) {
			data.data = vbuf;
			data.size = sizeof(vbuf);
			ret = db->put(db, txn, &key, &data, 0);
		} else {
			data.data = rbuf;
			data.ulen = sizeof(rbuf);
			data.flags = DB_DBT_USERMEM;
			ret = db->get(db, txn, &key, &data, 0);
			if (ret == DB_NOTFOUND || ret == DB_BUFFER_SMALL)
				ret = 0;
		}
		if (txn != NULL) {
			if (ret == 0)
				ret = txn->commit(txn, 0);
			else
				(void)txn->abort(txn);
		}
		t1 = now_ns();

		/*
		 * Count GOODPUT: only an op that committed (or, for a
		 * non-txn op, returned 0) is work done.  A DB_LOCK_DEADLOCK
		 * aborted the txn -- no row changed -- so it is counted as an
		 * abort, not throughput, and its latency is not recorded (it
		 * is retry churn, not a completed operation).  Counting aborts
		 * as ops is the measurement error the first A/B had: it scored
		 * base's ~22k-28k deadlock-retries as throughput against the
		 * branch's ~1-2.  DB_KEYEXIST means the key was already there,
		 * which for this random-key insert mix is a real completed
		 * no-op, so it counts as goodput.
		 */
		if (ret == DB_LOCK_DEADLOCK) {
			t->aborts++;
			continue;
		}
		if (ret != 0 && ret != DB_KEYEXIST) {
			env->err(env, ret, "op");
			return (NULL);
		}
		if (t->nlat < MAXLAT)
			t->lat[t->nlat++] = t1 - t0;
		t->ops++;
	}
	return (NULL);
}

static void
run(int nthreads, double secs)
{
	pthread_t th[MAXTHREADS];
	DB_LOCK_STAT *lk;
	uint64_t *all, total_ops, total_aborts, nall;
	double t0, dur;
	size_t ks[4];
	int i;

	(void)env->lock_stat(env, &lk, DB_STAT_CLEAR);
	free(lk);

	stop = go = 0;
	for (i = 0; i < nthreads; i++) {
		ts[i].ops = 0;
		ts[i].aborts = 0;
		ts[i].nlat = 0;
		ts[i].seed = (unsigned)(i * 2654435761u + 1);
		ts[i].tid = i;
		pthread_create(&th[i], NULL, worker, &ts[i]);
	}
	go = 1;
	t0 = now_sec();
	{
		struct timespec sl;
		sl.tv_sec = (time_t)secs;
		sl.tv_nsec = (long)((secs - (long)secs) * 1e9);
		nanosleep(&sl, NULL);
	}
	stop = 1;
	total_ops = total_aborts = nall = 0;
	for (i = 0; i < nthreads; i++) {
		pthread_join(th[i], NULL);
		total_ops += ts[i].ops;
		total_aborts += ts[i].aborts;
		nall += ts[i].nlat;
	}
	dur = now_sec() - t0;

	(void)env->lock_stat(env, &lk, 0);

	/* Pool latencies (SUCCEEDED ops only) and place percentiles. */
	if (nall > 0 && (all = malloc(nall * sizeof(*all))) != NULL) {
		uint64_t j = 0;
		for (i = 0; i < nthreads; i++) {
			memcpy(all + j, ts[i].lat,
			    ts[i].nlat * sizeof(*all));
			j += ts[i].nlat;
		}
		ks[0] = (size_t)(nall * 50 / 100);
		ks[1] = (size_t)(nall * 99 / 100);
		ks[2] = (size_t)(nall * 999 / 1000);
		ks[3] = (size_t)(nall - 1);
		db_quickselect(all, (size_t)nall, sizeof(*all), cmp_u64, ks, 4);
		/*
		 * TSV: thr goodput_per_s aborts_per_s p50 p99 p999 max
		 *      deadlocks lock_waits.  goodput = committed ops only;
		 * aborts (deadlock retries) are reported separately, not as
		 * throughput.  Latency percentiles are over succeeded ops.
		 */
		printf("%d\t%.0f\t%.0f\t%.2f\t%.2f\t%.2f\t%.2f\t%llu\t%llu\n",
		    nthreads, total_ops / dur, total_aborts / dur,
		    all[ks[0]] / 1000.0, all[ks[1]] / 1000.0,
		    all[ks[2]] / 1000.0, all[ks[3]] / 1000.0,
		    (unsigned long long)lk->st_ndeadlocks,
		    (unsigned long long)lk->st_lock_wait);
		free(all);
	} else {
		printf("%d\t%.0f\t%.0f\t0\t0\t0\t0\t%llu\t%llu\n",
		    nthreads, total_ops / dur, total_aborts / dur,
		    (unsigned long long)lk->st_ndeadlocks,
		    (unsigned long long)lk->st_lock_wait);
	}
	fflush(stdout);
	free(lk);
}

int
main(int argc, char **argv)
{
	DBT key, data;
	uint32_t kb, i;
	char vbuf[32];
	DBTYPE dbtype;
	int ret, ai, maxthreads;

	if (argc < 7) {
		fprintf(stderr, "usage: %s <hash|btree> <insert|lookup> "
		    "<txn|notxn> <keyspace> <secs> <t1> [t2 ...]\n", argv[0]);
		return (1);
	}
	g_type = strcmp(argv[1], "btree") == 0 ? T_BTREE : T_HASH;
	g_mix = strcmp(argv[2], "lookup") == 0 ? M_LOOKUP : M_INSERT;
	g_txn = strcmp(argv[3], "txn") == 0 ? 1 : 0;
	g_keyspace = (uint32_t)atoi(argv[4]);
	double secs = atof(argv[5]);
	dbtype = g_type == T_BTREE ? DB_BTREE : DB_HASH;

	maxthreads = 0;
	for (ai = 6; ai < argc; ai++)
		if (atoi(argv[ai]) > maxthreads)
			maxthreads = atoi(argv[ai]);
	if (maxthreads > MAXTHREADS)
		maxthreads = MAXTHREADS;
	for (i = 0; i < (uint32_t)maxthreads; i++)
		if ((ts[i].lat = malloc(MAXLAT * sizeof(uint64_t))) == NULL) {
			fprintf(stderr, "oom lat\n");
			return (1);
		}

	/* The harness creates and mounts HCDB; we only (re)create our files. */
	if (mkdir("./HCDB", 0755) != 0 && errno != EEXIST) {
		perror("mkdir HCDB");
		return (1);
	}

	if ((ret = db_env_create(&env, 0)) != 0) {
		fprintf(stderr, "env_create %d\n", ret);
		return (1);
	}
	env->set_errfile(env, stderr);
	env->set_cachesize(env, 0, 512 * 1024 * 1024, 1);
	/* Deadlock detection resolves H1-style waits so threads make progress. */
	(void)env->set_lk_detect(env, DB_LOCK_DEFAULT);
	if ((ret = env->open(env, "./HCDB", DB_CREATE | DB_INIT_MPOOL |
	    DB_INIT_LOCK | DB_INIT_TXN | DB_INIT_LOG | DB_THREAD, 0)) != 0) {
		env->err(env, ret, "env open");
		return (1);
	}

	if ((ret = db_create(&db, env, 0)) != 0) {
		env->err(env, ret, "db_create");
		return (1);
	}
	if (dbtype == DB_HASH) {
		/* Small fill factor -> frequent splits in the insert mix. */
		(void)db->set_h_ffactor(db, 4);
	}
	if ((ret = db->open(db, NULL, "hc.db", NULL, dbtype,
	    DB_CREATE | DB_AUTO_COMMIT | DB_THREAD, 0)) != 0) {
		env->err(env, ret, "db open");
		return (1);
	}

	/* Preload for the lookup mix so gets hit; insert mix starts near-empty
	 * so splits happen under load. */
	memset(vbuf, 'v', sizeof(vbuf));
	if (g_mix == M_LOOKUP) {
		for (i = 0; i < g_keyspace; i++) {
			kb = i;
			memset(&key, 0, sizeof(key));
			key.data = &kb;
			key.size = sizeof(kb);
			memset(&data, 0, sizeof(data));
			data.data = vbuf;
			data.size = sizeof(vbuf);
			if ((ret = db->put(db, NULL, &key, &data, 0)) != 0) {
				env->err(env, ret, "load");
				return (1);
			}
		}
	}

	printf("# type=%s mix=%s txn=%s keyspace=%u secs=%.1f\n",
	    argv[1], argv[2], argv[3], g_keyspace, secs);
	printf("# thr\tgoodput_per_s\taborts_per_s\tp50_us\tp99_us\t"
	    "p999_us\tmax_us\tdeadlocks\tlock_waits\n");

	for (ai = 6; ai < argc; ai++)
		run(atoi(argv[ai]), secs);

	db->close(db, 0);
	env->close(env, 0);
	for (i = 0; i < (uint32_t)maxthreads; i++)
		free(ts[i].lat);
	return (0);
}
