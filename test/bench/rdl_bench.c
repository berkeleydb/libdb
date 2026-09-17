/*-
 * rdl_bench.c -- read-descent lock-manager cost, two key regimes x five
 * isolation levels, WITHOUT the app-level confounder.
 *
 * Why not scale_bench: it shares ONE DB handle across all threads, so every
 * DB->get allocates a transient cursor on that handle under dbp->mutex.  At
 * t=96 that single mutex is ~85% of self time in BOTH key regimes, which
 * swamps the lock-manager signal this task is about (measured: LOCKMGR 0.65%
 * uniform / 1.32% hot with the handle mutex dominating both).  Following
 * scale_iso, each thread opens its OWN handle on the SAME file, so the
 * remaining contention is the engine's.
 *
 * Key regimes (label every result with which one):
 *   uniform  every read a different random key -> lock objects spread over
 *            partitions (LOCK_PART = ndx % part_t_size).
 *   hot      every read the SAME key -> ONE leaf -> ONE lock object -> ONE
 *            partition mutex regardless of lk_partitions.
 *
 * Isolation levels, matching the design question:
 *   none    DB->get(txn=NULL)               auto per-op page read lock
 *   plain   per-op txn, no flags            read lock held to commit
 *   rc      per-op txn, DB_READ_COMMITTED   lock coupled
 *   uncom   DB_READ_UNCOMMITTED             degree-1
 *   si      long-lived DB_TXN_SNAPSHOT on a DB_MULTIVERSION handle
 *   ssi     long-lived DB_TXN_SERIALIZABLE on a DB_MULTIVERSION handle
 *
 * si vs ssi is the measurement that matters: si takes ZERO lock objects
 * (db_meta.c:1184-1189), ssi takes one DB_LOCK_SIREAD marker per read
 * (db_meta.c:1190-1191).  Their difference IS the cost of the SSI read set.
 *
 * Usage: rdl_bench <uniform|hot> <none|plain|rc|uncom|si|ssi> <nkeys> <secs>
 *                  <home> <t1> [t2 ...]
 * Prints one line per thread count: "<regime> <iso> <thr> <ops/sec> <counters>"
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

enum { R_UNIFORM = 0, R_HOT = 1 };
enum { I_NONE = 0, I_PLAIN, I_RC, I_UNCOM, I_SI, I_SSI };

static DB_ENV *env;
static int g_regime, g_iso;
static uint32_t g_nkeys;
static volatile int stop, go;
static const char *g_home;

typedef struct {
	uint64_t ops;
	uint64_t errs;
	unsigned seed;
	int tid;
	DB *db;
	DB_TXN *rtxn;		/* long-lived read txn for si/ssi */
	char pad[24];
} targ_t __attribute__((aligned(64)));

static double
now_sec(void)
{
	struct timespec ts;
	clock_gettime(CLOCK_MONOTONIC, &ts);
	return (ts.tv_sec + ts.tv_nsec / 1e9);
}

static void *
worker(void *a)
{
	targ_t *t = a;
	DBT key, data;
	uint32_t kb, vbuf[64];
	DB_TXN *txn;
	int ret;

	while (!go) { }
	while (!stop) {
		kb = g_regime == R_HOT ? 0 :
		    (uint32_t)(rand_r(&t->seed) % g_nkeys);
		memset(&key, 0, sizeof(key));
		key.data = &kb; key.size = sizeof(kb);
		memset(&data, 0, sizeof(data));
		data.data = vbuf; data.ulen = sizeof(vbuf);
		data.flags = DB_DBT_USERMEM;

		txn = NULL;
		switch (g_iso) {
		case I_SI:
		case I_SSI:
			ret = t->db->get(t->db, t->rtxn, &key, &data, 0);
			break;
		case I_RC:
		case I_PLAIN:
			if ((ret = env->txn_begin(env, NULL, &txn,
			    g_iso == I_RC ? DB_READ_COMMITTED : 0)) != 0)
				break;
			ret = t->db->get(t->db, txn, &key, &data, 0);
			if (ret == DB_BUFFER_SMALL)
				ret = 0;
			if (ret == 0 || ret == DB_NOTFOUND)
				(void)txn->commit(txn, DB_TXN_NOSYNC);
			else
				(void)txn->abort(txn);
			break;
		case I_UNCOM:
			ret = t->db->get(t->db, NULL, &key, &data,
			    DB_READ_UNCOMMITTED);
			break;
		default:
			ret = t->db->get(t->db, NULL, &key, &data, 0);
			break;
		}
		if (ret == DB_BUFFER_SMALL)
			ret = 0;
		if (ret != 0 && ret != DB_NOTFOUND) {
			/*
			 * Count, do not abort the run: an SSI reader can legally
			 * get DB_SNAPSHOT_UNSAFE / DEADLOCK.  A nonzero errs
			 * column is the anti-vacuity signal for this benchmark
			 * (a run that errors on every op would otherwise post a
			 * flattering ops/sec).
			 */
			t->errs++;
			if (t->errs < 4)
				env->err(env, ret, "get k=%u", kb);
			if (g_iso == I_SI || g_iso == I_SSI) {
				/* Restart the long-lived read txn. */
				(void)t->rtxn->abort(t->rtxn);
				t->rtxn = NULL;
				if (env->txn_begin(env, NULL, &t->rtxn,
				    g_iso == I_SSI ? DB_TXN_SERIALIZABLE :
				    DB_TXN_SNAPSHOT) != 0)
					return (NULL);
			}
			continue;
		}
		t->ops++;
	}
	return (NULL);
}

static int
open_handle(uint32_t extra, DB **dbp)
{
	int ret;

	if ((ret = db_create(dbp, env, 0)) != 0)
		return (ret);
	return ((*dbp)->open(*dbp, NULL, "rdl.db", NULL, DB_BTREE,
	    DB_THREAD | extra, 0));
}

static void
run(int nthreads, double secs)
{
	pthread_t th[256];
	static targ_t ta[256];
	DB_LOCK_STAT *lk;
	DB_MPOOL_STAT *mp;
	double t0, dur;
	uint64_t total = 0, errs = 0;
	struct timespec sl;
	uint32_t mvflag;
	int i;

	mvflag = (g_iso == I_SI || g_iso == I_SSI) ? DB_MULTIVERSION : 0;
	stop = go = 0;
	memset(ta, 0, sizeof(ta));
	/* Clear the counters so the reported deltas belong to THIS thread count. */
	(void)env->lock_stat(env, &lk, DB_STAT_CLEAR); free(lk);
	(void)env->memp_stat(env, &mp, NULL, DB_STAT_CLEAR); free(mp);

	for (i = 0; i < nthreads; i++) {
		ta[i].seed = (unsigned)(i * 2654435761u + 1);
		ta[i].tid = i;
		if (open_handle(mvflag, &ta[i].db) != 0) {
			fprintf(stderr, "FAIL open_handle %d\n", i);
			exit(1);
		}
		if (mvflag != 0 && env->txn_begin(env, NULL, &ta[i].rtxn,
		    g_iso == I_SSI ? DB_TXN_SERIALIZABLE : DB_TXN_SNAPSHOT)
		    != 0) {
			fprintf(stderr, "FAIL txn_begin %d\n", i);
			exit(1);
		}
	}
	for (i = 0; i < nthreads; i++)
		pthread_create(&th[i], NULL, worker, &ta[i]);
	go = 1;
	t0 = now_sec();
	sl.tv_sec = (time_t)secs;
	sl.tv_nsec = (long)((secs - (long)secs) * 1e9);
	nanosleep(&sl, NULL);
	stop = 1;
	for (i = 0; i < nthreads; i++) {
		pthread_join(th[i], NULL);
		total += ta[i].ops;
		errs += ta[i].errs;
	}
	dur = now_sec() - t0;

	(void)env->lock_stat(env, &lk, 0);
	(void)env->memp_stat(env, &mp, NULL, 0);

	for (i = 0; i < nthreads; i++) {
		if (ta[i].rtxn != NULL)
			(void)ta[i].rtxn->commit(ta[i].rtxn, 0);
		(void)ta[i].db->close(ta[i].db, 0);
	}

#define	PCT(w, nw) (((w) + (nw)) ? 100.0 * (double)(w) / \
	    ((double)(w) + (double)(nw)) : 0.0)
	printf("RDLB %-7s %-5s thr=%-3d ops/sec=%12.0f locks/op=%6.3f "
	    "pages/op=%6.3f lockpart%%=%5.1f conflict%%=%5.1f "
	    "objs%%=%5.1f lockreg_w=%llu errs=%llu npart=%u\n",
	    g_regime == R_HOT ? "hot" : "uniform",
	    g_iso == I_NONE ? "none" : g_iso == I_PLAIN ? "plain" :
	    g_iso == I_RC ? "rc" : g_iso == I_UNCOM ? "uncom" :
	    g_iso == I_SI ? "si" : "ssi",
	    nthreads, total / dur,
	    total ? (double)lk->st_nrequests / total : 0.0,
	    total ? (double)(mp->st_cache_hit + mp->st_cache_miss) / total : 0.0,
	    PCT(lk->st_part_wait, lk->st_part_nowait),
	    PCT(lk->st_lock_wait, lk->st_lock_nowait),
	    PCT(lk->st_objs_wait, lk->st_objs_nowait),
	    (unsigned long long)lk->st_region_wait,
	    (unsigned long long)errs,
	    lk->st_partitions);
	fflush(stdout);
	free(lk); free(mp);
}

int
main(int argc, char **argv)
{
	DB *db;
	DBT key, data;
	DB_TXN *txn;
	DB_BTREE_STAT *bs;
	uint32_t kb, i;
	char vbuf[100];
	int ai, ret;

	if (argc < 7) {
		fprintf(stderr, "usage: %s <uniform|hot> "
		    "<none|plain|rc|uncom|si|ssi> <nkeys> <secs> <home> "
		    "<t1> [t2 ...]\n", argv[0]);
		return (1);
	}
	g_regime = strcmp(argv[1], "hot") == 0 ? R_HOT : R_UNIFORM;
	g_iso = strcmp(argv[2], "plain") == 0 ? I_PLAIN :
	    strcmp(argv[2], "rc") == 0 ? I_RC :
	    strcmp(argv[2], "uncom") == 0 ? I_UNCOM :
	    strcmp(argv[2], "si") == 0 ? I_SI :
	    strcmp(argv[2], "ssi") == 0 ? I_SSI : I_NONE;
	g_nkeys = (uint32_t)atoi(argv[3]);
	g_home = argv[5];

	if ((ret = db_env_create(&env, 0)) != 0) {
		fprintf(stderr, "env_create %d\n", ret); return (1);
	}
	env->set_errfile(env, stderr);
	env->set_cachesize(env, 0, 1024 * 1024 * 1024, 1);
	env->set_lk_max_locks(env, 400000);
	env->set_lk_max_objects(env, 400000);
	env->set_lk_max_lockers(env, 40000);
	if ((ret = env->open(env, g_home, DB_CREATE | DB_INIT_MPOOL |
	    DB_INIT_LOCK | DB_INIT_TXN | DB_INIT_LOG | DB_THREAD, 0)) != 0) {
		env->err(env, ret, "env open"); return (1);
	}

	/* Bulk load on a plain handle: a DB_MULTIVERSION load would retain
	 * versions of every page and measure version-chain walking instead. */
	if ((ret = db_create(&db, env, 0)) != 0) {
		env->err(env, ret, "db_create"); return (1);
	}
	if ((ret = db->open(db, NULL, "rdl.db", NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT | DB_THREAD, 0)) != 0) {
		env->err(env, ret, "db open"); return (1);
	}
	memset(vbuf, 'v', sizeof(vbuf));
	if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0) {
		env->err(env, ret, "txn_begin"); return (1);
	}
	for (i = 0; i < g_nkeys; i++) {
		kb = i;
		memset(&key, 0, sizeof(key));
		key.data = &kb; key.size = sizeof(kb);
		memset(&data, 0, sizeof(data));
		data.data = vbuf; data.size = sizeof(vbuf);
		if ((ret = db->put(db, txn, &key, &data, 0)) != 0) {
			env->err(env, ret, "load"); return (1);
		}
		if ((i % 5000) == 4999) {
			(void)txn->commit(txn, 0);
			if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0) {
				env->err(env, ret, "txn_begin"); return (1);
			}
		}
	}
	(void)txn->commit(txn, 0);
	(void)db->stat(db, NULL, &bs, 0);
	printf("RDLB setup regime=%s iso=%s nkeys=%u levels=%u\n",
	    argv[1], argv[2], g_nkeys, bs->bt_levels);
	free(bs);
	(void)db->close(db, 0);

	for (ai = 6; ai < argc; ai++)
		run(atoi(argv[ai]), atof(argv[4]));

	(void)env->close(env, 0);
	return (0);
}
