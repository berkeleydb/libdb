/*-
 * libdb isolation-scaling probe.
 *
 * The original scale_bench shares ONE DB handle across all threads, so every
 * DB->get allocates+frees a transient cursor on that handle's active-cursor
 * queue under dbp->mutex -- a single mutex that serializes every read and
 * caps scaling at ~8 cores (measured).  This probe removes that app-level
 * bottleneck the supported way -- each thread opens its OWN handle on the
 * SAME database file -- and measures how far BDB then scales under several
 * isolation levels, WITHOUT resorting to uncommitted reads:
 *
 *   none    DB->get(txn=NULL)            per-op auto page read lock
 *   rc      per-op txn + DB_READ_COMMITTED
 *   snap    long-lived per-thread DB_TXN_SNAPSHOT (MVCC, no read locks)
 *   uncom   DB_READ_UNCOMMITTED          (baseline; isolation sacrificed)
 *
 * All threads read the same shared dataset (full isolation, not separate
 * DBs), so this isolates handle/cursor scaling from the data.
 *
 *   cc -O2 -pthread scale_iso.c -I<build> -L<build>/.libs -ldb-5.3 -o scale_iso
 *   ./scale_iso <none|rc|snap|uncom> <nkeys> <secs> <t1> [t2 ...]
 */
#include <sys/types.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "db.h"

enum { ISO_NONE = 0, ISO_RC = 1, ISO_SNAP = 2, ISO_UNCOM = 3 };

static DB_ENV *env;
static int g_iso;
static uint32_t g_nkeys;
static volatile int stop, go;

/* Cache-line-padded per-thread state: no false sharing of the op counter, and
 * each thread carries its own handle + (for snap) its long-lived read txn. */
typedef struct {
	uint64_t ops;
	unsigned seed;
	int tid;
	DB *db;			/* this thread's own handle on the shared file */
	DB_TXN *rtxn;		/* long-lived read txn (snap), else NULL */
	char pad[64];
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
	uint32_t kb, vbuf[32];
	DB_TXN *txn;
	int ret;

	while (!go) { }
	while (!stop) {
		uint32_t k = (uint32_t)(rand_r(&t->seed) % g_nkeys);
		memset(&key, 0, sizeof(key)); key.data = &kb; key.size = sizeof(kb); kb = k;
		memset(&data, 0, sizeof(data));
		data.data = vbuf; data.ulen = sizeof(vbuf); data.flags = DB_DBT_USERMEM;

		txn = NULL;
		ret = 0;
		switch (g_iso) {
		case ISO_SNAP:		/* long-lived snapshot txn, no read locks */
			ret = t->db->get(t->db, t->rtxn, &key, &data, 0);
			break;
		case ISO_RC:		/* per-op txn, read-committed (locks held to commit) */
			if ((ret = env->txn_begin(env, NULL, &txn, DB_READ_COMMITTED)) != 0)
				break;
			ret = t->db->get(t->db, txn, &key, &data, 0);
			if (ret == DB_BUFFER_SMALL) ret = 0;
			if (ret == 0 || ret == DB_NOTFOUND)
				(void)txn->commit(txn, DB_TXN_NOSYNC);
			else
				(void)txn->abort(txn);
			break;
		case ISO_UNCOM:		/* uncommitted: no read locks (isolation sacrificed) */
			ret = t->db->get(t->db, NULL, &key, &data, DB_READ_UNCOMMITTED);
			break;
		default:		/* none: auto per-op page read lock */
			ret = t->db->get(t->db, NULL, &key, &data, 0);
			break;
		}
		if (ret == DB_BUFFER_SMALL) ret = 0;
		if (ret != 0 && ret != DB_NOTFOUND) {
			env->err(env, ret, "get k=%u", k);
			return (NULL);
		}
		t->ops++;
	}
	return (NULL);
}

static int
open_handle(const char *file, uint32_t flags, DB **dbp)
{
	int ret;
	if ((ret = db_create(dbp, env, 0)) != 0) return (ret);
	return ((*dbp)->open(*dbp, NULL, file, NULL, DB_BTREE, flags, 0));
}

static void
run(int nthreads, double secs)
{
	pthread_t th[256];
	static targ_t ta[256];
	double t0, dur;
	uint64_t total = 0;
	uint32_t rdflag;
	int i;

	stop = go = 0;
	rdflag = g_iso == ISO_UNCOM ? DB_READ_UNCOMMITTED :
	    g_iso == ISO_SNAP ? DB_MULTIVERSION : 0;
	for (i = 0; i < nthreads; i++) {
		ta[i].ops = 0; ta[i].seed = (unsigned)(i * 2654435761u + 1); ta[i].tid = i;
		ta[i].rtxn = NULL;
		/* Per-thread handle on the SHARED file. */
		if (open_handle("bench.db", DB_AUTO_COMMIT | DB_THREAD | rdflag, &ta[i].db) != 0) {
			fprintf(stderr, "open_handle %d failed\n", i); exit(1);
		}
		if (g_iso == ISO_SNAP &&
		    env->txn_begin(env, NULL, &ta[i].rtxn, DB_TXN_SNAPSHOT) != 0) {
			fprintf(stderr, "txn_begin snap %d failed\n", i); exit(1);
		}
		pthread_create(&th[i], NULL, worker, &ta[i]);
	}
	go = 1;
	t0 = now_sec();
	struct timespec sl = { (time_t)secs, (long)((secs - (long)secs) * 1e9) };
	nanosleep(&sl, NULL);
	stop = 1;
	for (i = 0; i < nthreads; i++) {
		pthread_join(th[i], NULL);
		total += ta[i].ops;
		if (ta[i].rtxn != NULL) (void)ta[i].rtxn->commit(ta[i].rtxn, 0);
		(void)ta[i].db->close(ta[i].db, 0);
	}
	dur = now_sec() - t0;
	printf("%-6s %3d %12.0f ops/sec\n",
	    g_iso == ISO_SNAP ? "snap" : g_iso == ISO_RC ? "rc" :
	    g_iso == ISO_UNCOM ? "uncom" : "none", nthreads, total / dur);
	fflush(stdout);
}

int
main(int argc, char **argv)
{
	DB *db;
	DBT key, data;
	uint32_t kb, i;
	char vbuf[100];
	int ret, ai;

	if (argc < 5) {
		fprintf(stderr, "usage: %s <none|rc|snap|uncom> <nkeys> <secs> <t..>\n", argv[0]);
		return (1);
	}
	g_iso = strcmp(argv[1], "snap") == 0 ? ISO_SNAP :
	    strcmp(argv[1], "rc") == 0 ? ISO_RC :
	    strcmp(argv[1], "uncom") == 0 ? ISO_UNCOM : ISO_NONE;
	g_nkeys = (uint32_t)atoi(argv[2]);
	double secs = atof(argv[3]);

	system("rm -rf ./ISODB && mkdir ./ISODB");
	if ((ret = db_env_create(&env, 0)) != 0) { fprintf(stderr, "env_create %d\n", ret); return 1; }
	env->set_errfile(env, stderr);
	env->set_cachesize(env, 0, 512 * 1024 * 1024, 1);
	/* MVCC needs to be enabled on the env for snapshot reads. */
	if ((ret = env->open(env, "./ISODB", DB_CREATE | DB_INIT_MPOOL |
	    DB_INIT_LOCK | DB_INIT_TXN | DB_INIT_LOG | DB_THREAD | DB_MULTIVERSION, 0)) != 0) {
		env->err(env, ret, "env open"); return 1;
	}

	/* Load the shared dataset once with a plain handle. */
	if ((ret = open_handle("bench.db", DB_CREATE | DB_AUTO_COMMIT | DB_THREAD, &db)) != 0) {
		env->err(env, ret, "load open"); return 1;
	}
	memset(vbuf, 'v', sizeof(vbuf));
	for (i = 0; i < g_nkeys; i++) {
		memset(&key, 0, sizeof(key)); key.data = &kb; key.size = sizeof(kb); kb = i;
		memset(&data, 0, sizeof(data)); data.data = vbuf; data.size = sizeof(vbuf);
		if ((ret = db->put(db, NULL, &key, &data, 0)) != 0) { env->err(env, ret, "load"); return 1; }
	}
	(void)db->close(db, 0);
	printf("# iso=%s nkeys=%u secs=%.1f (per-thread handle on shared bench.db)\n",
	    argv[1], g_nkeys, secs);

	for (ai = 4; ai < argc; ai++)
		run(atoi(argv[ai]), secs);

	env->close(env, 0);
	return (0);
}
