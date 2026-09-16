/*-
 * pin_why: WHY does the perf/bhpin-r1 optimistic fast path FIRE on the workload
 * we are benchmarking?  A "neutral" result from a code path that never executes
 * is not a measurement of that path, so this is a gate on the whole R1 arm.
 *
 * Requires a --enable-diagnostic build of the bhpin branch, which is where
 * __memp_bhpin_hits / __memp_bhpin_attempts live (mp_fget.c, DIAGNOSTIC only).
 *
 * usage: pin_fires <indiv|batch> <nkeys> <batchsz> <threads> <secs>
 * env:   PIN_HOME (existing dir), PIN_PRIVATE (DB_PRIVATE + set_thread_count)
 */
#include <sys/types.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "db.h"

#define	MAXB	256
#define	VALSZ	100

extern unsigned long __memp_bhpin_hits;
extern unsigned long __memp_bhpin_attempts;
extern unsigned long __memp_bhpin_notwired;
extern unsigned long __memp_bhpin_badflag;
extern unsigned long __memp_bhpin_chain;
extern unsigned long __memp_bhpin_flagbits;

static DB_ENV *env;
static DB *db;
static volatile int stop, go;
static int g_batch, g_batchsz;
static unsigned g_nkeys;
static unsigned long long g_reads;

static void *
worker(void *a)
{
	DBT keys[MAXB], datas[MAXB];
	unsigned kb[MAXB];
	int rets[MAXB];
	char vbuf[MAXB][VALSZ + 8];
	unsigned seed = (unsigned)(size_t)a * 2654435761u + 7;
	unsigned long long n = 0;
	int i, ret;

	while (!go) { }
	while (!stop) {
		for (i = 0; i < g_batchsz; i++) {
			kb[i] = (unsigned)(rand_r(&seed) % g_nkeys);
			memset(&keys[i], 0, sizeof(DBT));
			keys[i].data = &kb[i]; keys[i].size = sizeof(kb[i]);
			memset(&datas[i], 0, sizeof(DBT));
			datas[i].data = vbuf[i]; datas[i].ulen = sizeof(vbuf[i]);
			datas[i].flags = DB_DBT_USERMEM;
		}
		if (g_batch) {
			ret = db_get_multiple(db, NULL, keys, datas, rets,
			    (u_int32_t)g_batchsz, 0);
			if (ret != 0 && ret != DB_NOTFOUND) return (NULL);
		} else
			for (i = 0; i < g_batchsz; i++) {
				ret = db->get(db, NULL, &keys[i], &datas[i], 0);
				if (ret != 0 && ret != DB_NOTFOUND) return (NULL);
			}
		n += (unsigned long long)g_batchsz;
	}
	(void)__sync_fetch_and_add(&g_reads, n);
	return (NULL);
}

int
main(int argc, char **argv)
{
	pthread_t th[128];
	DBT key, data;
	unsigned kb, i;
	u_int32_t oflags;
	char vbuf[VALSZ];
	const char *home;
	unsigned long h0, a0, h1, a1;
	unsigned long nw0, bf0, ch0, nw1, bf1, ch1;
	int ret, nthreads, secs, private;
	struct timespec sl;

	if (argc != 6) {
		fprintf(stderr, "usage: %s <indiv|batch> <nkeys> <batchsz>"
		    " <threads> <secs>\n", argv[0]);
		return (1);
	}
	g_batch = strcmp(argv[1], "batch") == 0;
	g_nkeys = (unsigned)atoi(argv[2]);
	g_batchsz = atoi(argv[3]);
	nthreads = atoi(argv[4]);
	secs = atoi(argv[5]);
	if (g_nkeys < 1000 || g_batchsz < 1 || g_batchsz > MAXB ||
	    nthreads < 1 || nthreads > 128 || secs < 1) {
		fprintf(stderr, "bad args\n"); return (1);
	}
	if ((home = getenv("PIN_HOME")) == NULL) {
		fprintf(stderr, "set PIN_HOME\n"); return (1);
	}
	private = getenv("PIN_PRIVATE") != NULL;

	if ((ret = db_env_create(&env, 0)) != 0) return (1);
	env->set_errfile(env, stderr);
	(void)env->set_cachesize(env, 0, 512 * 1024 * 1024, 1);
	(void)env->set_flags(env, DB_TXN_NOSYNC, 1);
	oflags = DB_CREATE | DB_INIT_MPOOL | DB_INIT_LOCK | DB_INIT_TXN |
	    DB_INIT_LOG | DB_THREAD;
	if (private) {
		oflags |= DB_PRIVATE;
		(void)env->set_thread_count(env, (u_int32_t)nthreads + 16);
	}
	if ((ret = env->open(env, home, oflags, 0)) != 0) {
		env->err(env, ret, "env open %s", home); return (1);
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

	/*
	 * Clean the pages before measuring.  The load dirties every page it
	 * touches, and with DB_TXN_NOSYNC and no checkpoint they STAY dirty for
	 * the whole run -- which silently disarms perf/bhpin-r1: its optimistic
	 * fast path refuses any buffer with BH_DIRTY set, so its own DIAGNOSTIC
	 * counters showed 0 hits in 5.6M attempts (bail reason: badflag,
	 * flagbits 0x6 = BH_DIRTY|BH_DIRTY_CREATE) until this checkpoint was
	 * added.  A read benchmark measured on a dirty buffer pool is not a
	 * read benchmark, so this is a correctness property of the harness, not
	 * a favour to one arm.
	 */
	if ((ret = env->txn_checkpoint(env, 0, 0, DB_FORCE)) != 0) {
		env->err(env, ret, "txn_checkpoint"); return (1);
	}
	{
		int nwrote = 0;
		(void)env->memp_trickle(env, 100, &nwrote);
	}

	/* Counters are read AFTER the load, so only read traffic is counted. */
	h0 = __memp_bhpin_hits; a0 = __memp_bhpin_attempts;
	nw0 = __memp_bhpin_notwired; bf0 = __memp_bhpin_badflag;
	ch0 = __memp_bhpin_chain;
	stop = 0; go = 0;
	for (i = 0; i < (unsigned)nthreads; i++)
		(void)pthread_create(&th[i], NULL, worker, (void *)(size_t)i);
	go = 1;
	sl.tv_sec = secs; sl.tv_nsec = 0;
	(void)nanosleep(&sl, NULL);
	stop = 1;
	for (i = 0; i < (unsigned)nthreads; i++)
		(void)pthread_join(th[i], NULL);
	h1 = __memp_bhpin_hits; a1 = __memp_bhpin_attempts;
	nw1 = __memp_bhpin_notwired; bf1 = __memp_bhpin_badflag;
	ch1 = __memp_bhpin_chain;

	(void)db->close(db, 0);
	(void)env->close(env, 0);

	if (g_reads == 0) {
		printf("FAIL bhpin-why mode=%s env=%s no reads performed\n",
		    argv[1], private ? "private" : "shared");
		return (1);
	}
	printf("VERDICT bhpin-why mode=%s env=%s reads=%llu attempts=%lu "
	    "hits=%lu notwired=%lu badflag=%lu chain=%lu flagbits=0x%lx "
	    "unaccounted=%ld\n",
	    argv[1], private ? "private" : "shared", g_reads,
	    a1 - a0, h1 - h0, nw1 - nw0, bf1 - bf0, ch1 - ch0,
	    __memp_bhpin_flagbits,
	    (long)((a1 - a0) - (h1 - h0) - (nw1 - nw0) - (bf1 - bf0) -
	    (ch1 - ch0)));
	return (0);
}
