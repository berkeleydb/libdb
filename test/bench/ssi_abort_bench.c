/*-
 * libdb SSI abort-rate probe.
 *
 * Serializable Snapshot Isolation (DB_TXN_SNAPSHOT) prevents write-skew
 * and other snapshot anomalies by aborting the pivot of a dangerous rw-
 * dependency structure.  The cost is a nonzero abort rate that grows with
 * contention, and -- because Berkeley DB tracks conflicts at PAGE granularity,
 * not row/key -- two transactions touching *different* keys that happen to
 * share a leaf page can produce a "false" conflict.  This probe measures that
 * abort rate directly, so the page-granularity cost is measured, not asserted
 * (ROADMAP #17).
 *
 * Each thread runs a stream of short snapshot-safe transactions: read one key,
 * write another, commit.  Keys are drawn from a configurable hot set.  A small
 * hot set + many threads maximizes both genuine and page-sharing conflicts; a
 * large hot set approximates the low-contention floor.  We report committed vs
 * aborted (DB_SNAPSHOT_CONFLICT) and deadlock counts and the abort rate.
 *
 *   cc -O2 -pthread ssi_abort_bench.c -I<build> -L<build>/.libs -ldb-5.3 \
 *       -o ssi_abort_bench
 *   ./ssi_abort_bench <hotkeys> <secs> <t1> [t2 ...]
 *
 * e.g.  ./ssi_abort_bench 16 5 1 2 4 8 16     # sweep thread counts, hot=16
 *
 * NOT a TPC benchmark and not comparable to any TPC result.
 */
#include <sys/types.h>
#include <sys/stat.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "db.h"
#include <stdatomic.h>

static DB_ENV *env;
static DB *db;
static int hotkeys;
static atomic_int stop;
static atomic_int go;
static atomic_int panicked;	/* set on DB_RUNRECOVERY: stop, don't spin */

typedef struct {
	pthread_t tid;
	unsigned seed;
	long committed, aborted, deadlock, other;
} targ_t;

/* One snapshot-safe read-then-write transaction; classifies the outcome. */
static void
one_txn(targ_t *t)
{
	DB_TXN *txn;
	DBT key, data;
	char kbuf[32], vbuf[32];
	int rk, wk, ret, val;

	rk = rand_r(&t->seed) % hotkeys;
	wk = rand_r(&t->seed) % hotkeys;

	if (env->txn_begin(env, NULL, &txn, DB_TXN_SNAPSHOT) != 0) {
		t->other++;
		return;
	}

	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	/* Read rk. */
	sprintf(kbuf, "k%d", rk);
	key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
	data.flags = DB_DBT_MALLOC;
	ret = db->get(db, txn, &key, &data, 0);
	if (ret == 0 && data.data != NULL) {
		val = atoi((char *)data.data);
		free(data.data);
	} else
		val = 0;
	if (ret == DB_LOCK_DEADLOCK || ret == DB_SNAPSHOT_CONFLICT)
		goto conflict;

	/* Write wk = f(read value): a real read/write dependency. */
	sprintf(kbuf, "k%d", wk);
	sprintf(vbuf, "%d", val + 1);
	memset(&data, 0, sizeof(data));
	key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
	data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
	ret = db->put(db, txn, &key, &data, 0);
	if (ret == DB_LOCK_DEADLOCK || ret == DB_SNAPSHOT_CONFLICT)
		goto conflict;
	if (ret != 0) {
		(void)txn->abort(txn);
		if (ret == DB_RUNRECOVERY) atomic_store(&panicked, 1);
		t->other++;
		return;
	}

	ret = txn->commit(txn, 0);
	if (ret == 0) { t->committed++; return; }
	if (ret == DB_SNAPSHOT_CONFLICT) { t->aborted++; return; }
	if (ret == DB_LOCK_DEADLOCK) { t->deadlock++; return; }
	if (ret == DB_RUNRECOVERY) atomic_store(&panicked, 1);
	t->other++;
	return;

conflict:
	(void)txn->abort(txn);
	if (ret == DB_SNAPSHOT_CONFLICT) t->aborted++;
	else t->deadlock++;
}

static void *
worker(void *arg)
{
	targ_t *t = arg;
	while (!atomic_load(&go)) ;
	while (!atomic_load(&stop) && !atomic_load(&panicked))
		one_txn(t);
	return (NULL);
}

static void
run(int nthreads, int secs)
{
	targ_t *ta;
	int i;
	long c = 0, a = 0, d = 0, o = 0, total;

	ta = calloc((size_t)nthreads, sizeof(*ta));
	atomic_store(&stop, 0); atomic_store(&go, 0); atomic_store(&panicked, 0);
	for (i = 0; i < nthreads; i++) {
		ta[i].seed = (unsigned)(i * 2654435761u + 1);
		pthread_create(&ta[i].tid, NULL, worker, &ta[i]);
	}
	atomic_store(&go, 1);
	sleep(secs);
	atomic_store(&stop, 1);
	for (i = 0; i < nthreads; i++) {
		pthread_join(ta[i].tid, NULL);
		c += ta[i].committed; a += ta[i].aborted;
		d += ta[i].deadlock;  o += ta[i].other;
	}
	total = c + a + d + o;
	printf("threads=%-3d hot=%-4d  commit=%-8ld ssi_abort=%-7ld "
	    "deadlock=%-6ld other=%-5ld  abort_rate=%.1f%% (%.0f txn/s)%s\n",
	    nthreads, hotkeys, c, a, d, o,
	    total ? 100.0 * (double)(a + d) / (double)total : 0.0,
	    (double)total / secs, atomic_load(&panicked) ? "  [ENV PANIC -- see note]" : "");
	free(ta);
}

int
main(int argc, char **argv)
{
	DB_TXN *txn;
	DBT key, data;
	char kbuf[32], vbuf[32];
	int i, secs, ret;

	if (argc < 4) {
		fprintf(stderr,
		    "usage: %s <hotkeys> <secs> <t1> [t2 ...]\n", argv[0]);
		return (1);
	}
	hotkeys = atoi(argv[1]);
	secs = atoi(argv[2]);

	(void)mkdir("/tmp/ssi_abort_env", 0755);
	if ((ret = db_env_create(&env, 0)) != 0) goto err;
	env->set_cachesize(env, 0, 64 * 1024 * 1024, 1);
	/* Size the lock region generously so exhaustion isn't the variable. */
	env->set_lk_max_locks(env, 20000);
	env->set_lk_max_objects(env, 20000);
	env->set_lk_max_lockers(env, 20000);
	/* Resolve lock cycles automatically so contention can't wedge us. */
	env->set_lk_detect(env, DB_LOCK_MINWRITE);
	/*
	 * DB_RECOVER: a benchmark run may be killed (timeout) mid-transaction,
	 * leaving a dirty region.  Always run recovery on open so a stale
	 * environment is cleaned rather than hanging or crashing the next run.
	 */
	if ((ret = env->open(env, "/tmp/ssi_abort_env",
	    DB_CREATE | DB_RECOVER | DB_INIT_LOCK | DB_INIT_LOG | DB_INIT_MPOOL |
	    DB_INIT_TXN | DB_THREAD | DB_MULTIVERSION, 0644)) != 0) goto err;
	if ((ret = db_create(&db, env, 0)) != 0) goto err;
	if ((ret = db->open(db, NULL, "ssi.db", NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT | DB_THREAD | DB_MULTIVERSION,
	    0644)) != 0) goto err;

	/* Seed the hot set. */
	env->txn_begin(env, NULL, &txn, 0);
	for (i = 0; i < hotkeys; i++) {
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		sprintf(kbuf, "k%d", i);
		sprintf(vbuf, "0");
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
		db->put(db, txn, &key, &data, 0);
	}
	txn->commit(txn, 0);

	for (i = 3; i < argc; i++)
		run(atoi(argv[i]), secs);

	db->close(db, 0);
	env->close(env, 0);
	return (0);
err:
	fprintf(stderr, "error: %s\n", db_strerror(ret));
	return (1);
}
