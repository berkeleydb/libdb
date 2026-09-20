/*-
 * P5 probe: the log-region latch as the append bottleneck.
 *
 * Insert-heavy load on a growing btree, N threads, each insert in its own
 * transaction, so every iteration appends at least one log record and every
 * commit appends a commit record.  The thing under test is __log_put's
 * critical section, so the workload is "append a lot".
 *
 * Deliberately a near-copy of the P1/P4 probe (/tmp/p1bench.c) so the numbers
 * are comparable to the recorded P1/P4 baselines, with two additions:
 *
 *   - argv[5] sets the log buffer size (DB_ENV->set_lg_bsize).  The default is
 *     LG_BSIZE_DEFAULT (32000 bytes).  This matters because __log_fill calls
 *     __log_write -- a pwrite(2) -- *while holding the log region latch*
 *     whenever the buffer fills, so the buffer size sets how often a syscall
 *     lands inside the serialized append.
 *   - argv[6] selects the durability mode: nosync (default, DB_TXN_NOSYNC),
 *     wrnosync (DB_TXN_WRITE_NOSYNC) or sync (neither -- fsync per commit).
 *
 * It prints the log subsystem's own counters (st_wcount, st_wcount_fill,
 * st_scount, st_region_wait/nowait, commits-per-flush) so "how many writes
 * happened under the latch" is derived from the engine, not guessed.
 *
 *	cc -O2 -pthread p5_log_bench.c -I<build> -L<build>/.libs -ldb-2026.0 \
 *	    -o p5_log_bench
 *	./p5_log_bench <dir> <threads> <secs> <valsz> [lg_bsize] [mode] [spins] [batch]
 *
 * Two controls separate "the log is the ceiling" from "the per-transaction path
 * is the ceiling":
 *
 *   mode=nolog    no transaction and no logging (DB->put with a NULL txn on a
 *                 non-transactional environment).  Only valid to t=8 --
 *                 concurrent DB->put without DB_INIT_LOCK returns EINVAL above
 *                 that, so it cannot answer the high-thread question.
 *   mode=notdur   DB_TXN_NOT_DURABLE on the DB handle: full transactions, full
 *                 locking, the same commit path, but __log_put_record_int takes
 *                 the is_durable == 0 branch and queues the record on the txn
 *                 instead of appending it (src/log/log_put.c:2278-2302), so the
 *                 log region latch is never taken for data records.  This IS
 *                 valid at every thread count and is the control that isolates
 *                 the log's share of the per-transaction cost.
 *
 * [batch] puts N keys per transaction instead of 1.  A single-key insert costs
 * ~3.1 log records (2 x __db_addrem for key and data, 1 x __txn_regop), so it
 * takes the log region latch 3+ times.  Batching amortizes the commit record
 * over N puts and is the zero-code estimate of what per-transaction append
 * batching (design D2) could buy -- it changes how many times the serialized
 * stage is entered per unit of user work, which is the quantity that matters.
 */
#include <sys/types.h>
#include <errno.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include "db.h"

static DB_ENV *env;
static DB *dbp;
static int nthreads, secs, valsz, batch, nolog, notdur;
static volatile int stop;
static unsigned long long total_ops;
static pthread_mutex_t tally = PTHREAD_MUTEX_INITIALIZER;

static void die(int ret, const char *what)
{
	fprintf(stderr, "FATAL %s: %s\n", what, db_strerror(ret));
	exit(2);
}

static double now(void)
{
	struct timespec ts;
	clock_gettime(CLOCK_MONOTONIC, &ts);
	return ts.tv_sec + ts.tv_nsec / 1e9;
}

static void *worker(void *arg)
{
	DB_TXN *txn;
	DBT key, data;
	char kbuf[32], *vbuf;
	unsigned long long n = 0;
	unsigned long seed = (unsigned long)(uintptr_t)arg * 2654435761u + 12345;
	int ret;

	if ((vbuf = malloc(valsz)) == NULL)
		return (NULL);
	memset(vbuf, 'v', valsz);

	while (!stop) {
		int i;

		if (!nolog && (ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
			die(ret, "txn_begin");
		for (i = 0; i < batch; i++) {
			/* Unique, non-colliding keys: every insert grows the tree. */
			seed = seed * 1103515245u + 12345;
			snprintf(kbuf, sizeof(kbuf), "%016lx%08lx",
			    (unsigned long)(uintptr_t)arg, seed & 0xffffffffu);

			memset(&key, 0, sizeof(key));
			memset(&data, 0, sizeof(data));
			key.data = kbuf; key.size = (u_int32_t)strlen(kbuf);
			data.data = vbuf; data.size = (u_int32_t)valsz;

			if ((ret = dbp->put(dbp,
			    nolog ? NULL : txn, &key, &data, 0)) != 0)
				break;
		}
		if (ret != 0) {
			if (nolog)
				die(ret, "put");
			(void)txn->abort(txn);
			if (ret == DB_LOCK_DEADLOCK || ret == DB_LOCK_NOTGRANTED)
				continue;
			die(ret, "put");
		}
		if (!nolog && (ret = txn->commit(txn, 0)) != 0) {
			if (ret == DB_LOCK_DEADLOCK)
				continue;
			die(ret, "commit");
		}
		/* Count user-level rows, so arms with different batch sizes
		 * are compared on the same unit of work. */
		n += batch;
	}
	pthread_mutex_lock(&tally);
	total_ops += n;
	pthread_mutex_unlock(&tally);
	free(vbuf);
	return (NULL);
}

int main(int argc, char **argv)
{
	pthread_t *th;
	DB_TXN *txn;
	DB_LOG_STAT *ls;
	const char *home, *mode;
	double t0, el;
	u_int32_t bsize, spins;
	int i, ret;

	home = argc > 1 ? argv[1] : "/nvme/P5DIR";
	nthreads = argc > 2 ? atoi(argv[2]) : 8;
	secs = argc > 3 ? atoi(argv[3]) : 10;
	valsz = argc > 4 ? atoi(argv[4]) : 100;
	bsize = argc > 5 ? (u_int32_t)strtoul(argv[5], NULL, 0) : 0;
	mode = argc > 6 ? argv[6] : "nosync";
	spins = argc > 7 ? (u_int32_t)strtoul(argv[7], NULL, 0) : 0;
	batch = argc > 8 ? atoi(argv[8]) : 1;
	if (batch < 1)
		batch = 1;

	if ((ret = db_env_create(&env, 0)) != 0) die(ret, "env_create");
	env->set_errfile(env, stderr);
	env->set_errpfx(env, "p5");
	(void)env->set_cachesize(env, 0, 512 * 1024 * 1024, 1);
	(void)env->set_thread_count(env, nthreads + 8);
	/*
	 * Deadlock detection.  Without it, a genuine deadlock blocks forever
	 * instead of one party being chosen as a victim -- and a batch > 1 puts
	 * several keys in one transaction, which really can deadlock against a
	 * concurrent transaction on a growing tree.  The other benches in this
	 * directory set this (see test/bench/bdb_bench.h); its absence here
	 * cost one debugging detour that looked like a library hang and was
	 * this probe's own missing configuration.
	 */
	(void)env->set_lk_detect(env, DB_LOCK_DEFAULT);
	/* The default region holds ~1000 locks/lockers/objects; a 96-thread
	 * run with batching needs more, and exhausting them returns ENOMEM. */
	(void)env->set_lk_max_locks(env, 200000);
	(void)env->set_lk_max_lockers(env, 200000);
	(void)env->set_lk_max_objects(env, 200000);
	if (bsize != 0)
		(void)env->set_lg_bsize(env, bsize);
	/*
	 * Spin count per mutex acquisition attempt.  The default is
	 * cpu_count * MUTEX_SPINS_PER_PROCESSOR (50), i.e. 4800 on a 96-vCPU
	 * box -- so a contended log latch has every waiter burning up to 4800
	 * test-and-set attempts on one shared cacheline before yielding.  This
	 * knob exists to separate "the critical section is long" from "the
	 * handoff is expensive", which are different defects with different
	 * fixes.
	 */
	if (spins != 0)
		(void)env->mutex_set_tas_spins(env, spins);
	if (strcmp(mode, "nolog") == 0)
		nolog = 1;
	else if (strcmp(mode, "notdur") == 0) {
		notdur = 1;
		(void)env->set_flags(env, DB_TXN_NOSYNC, 1);
	} else if (strcmp(mode, "nosync") == 0)
		(void)env->set_flags(env, DB_TXN_NOSYNC, 1);
	else if (strcmp(mode, "wrnosync") == 0)
		(void)env->set_flags(env, DB_TXN_WRITE_NOSYNC, 1);
	else if (strcmp(mode, "sync") != 0) {
		fprintf(stderr,
		    "mode must be nosync|wrnosync|sync|nolog|notdur\n");
		return (2);
	}
	if ((ret = env->open(env, home, DB_CREATE | DB_INIT_MPOOL | DB_THREAD |
	    (nolog ? 0 : DB_INIT_TXN | DB_INIT_LOG | DB_INIT_LOCK), 0644)) != 0)
		die(ret, "env->open");

	if ((ret = db_create(&dbp, env, 0)) != 0) die(ret, "db_create");
	(void)dbp->set_pagesize(dbp, 4096);
	if (notdur)
		(void)dbp->set_flags(dbp, DB_TXN_NOT_DURABLE);
	if (nolog) {
		if ((ret = dbp->open(dbp, NULL, "p5.db", NULL, DB_BTREE,
		    DB_CREATE | DB_THREAD, 0644)) != 0) die(ret, "db->open");
	} else {
		if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
			die(ret, "open txn");
		if ((ret = dbp->open(dbp, txn, "p5.db", NULL, DB_BTREE,
		    DB_CREATE | DB_THREAD, 0644)) != 0) die(ret, "db->open");
		if ((ret = txn->commit(txn, 0)) != 0) die(ret, "open commit");
	}

	/* Reset stats so the counts below cover only the measured window. */
	if (!nolog) {
		(void)env->log_stat(env, &ls, DB_STAT_CLEAR);
		free(ls);
	}

	if ((th = calloc(nthreads, sizeof(*th))) == NULL) return (2);
	t0 = now();
	for (i = 0; i < nthreads; i++)
		if (pthread_create(&th[i], NULL, worker, (void *)(uintptr_t)(i + 1)) != 0)
			die(errno, "pthread_create");
	sleep(secs);
	stop = 1;
	for (i = 0; i < nthreads; i++)
		(void)pthread_join(th[i], NULL);
	el = now() - t0;

	printf("threads=%d ops=%llu secs=%.2f ops_per_sec=%.0f\n",
	    nthreads, total_ops, el, total_ops / el);
	if (nolog) {
		printf("  mode=nolog (no txn, no log) batch=%d\n", batch);
		(void)dbp->close(dbp, 0);
		(void)env->close(env, 0);
		free(th);
		return (0);
	}
	if ((ret = env->log_stat(env, &ls, 0)) != 0) die(ret, "log_stat");
	printf("  lg_bsize=%lu records=%lu w_bytes=%luMB+%lu "
	    "wcount=%lu wcount_fill=%lu scount=%lu\n",
	    (u_long)ls->st_lg_bsize, (u_long)ls->st_record,
	    (u_long)ls->st_w_mbytes, (u_long)ls->st_w_bytes,
	    (u_long)ls->st_wcount, (u_long)ls->st_wcount_fill,
	    (u_long)ls->st_scount);
	printf("  spins=%lu batch=%d notdur=%d records_per_row=%.2f\n",
	    (u_long)spins, batch, notdur,
	    total_ops == 0 ? 0.0 : (double)ls->st_record / total_ops);
	printf("  region_wait=%lu region_nowait=%lu "
	    "mincommitperflush=%lu maxcommitperflush=%lu\n",
	    (u_long)ls->st_region_wait, (u_long)ls->st_region_nowait,
	    (u_long)ls->st_mincommitperflush, (u_long)ls->st_maxcommitperflush);
	/*
	 * Bytes actually pushed at the device, and the implied write size.  The
	 * device ceiling on the reference box is 494k IOPS / 1930 MiB/s (fio 4k
	 * random write, io_uring, iodepth 64, 4 jobs), so this is the number to
	 * compare against when asking "is the log anywhere near the device?".
	 */
	printf("  MB_per_sec=%.1f writes_per_sec=%.0f avg_write_bytes=%.0f\n",
	    (ls->st_w_mbytes + ls->st_w_bytes / 1048576.0) / el,
	    ls->st_wcount / el,
	    ls->st_wcount == 0 ? 0.0 :
	    (ls->st_w_mbytes * 1048576.0 + ls->st_w_bytes) / ls->st_wcount);
	free(ls);

	(void)dbp->close(dbp, 0);
	(void)env->close(env, 0);
	free(th);
	return (0);
}
