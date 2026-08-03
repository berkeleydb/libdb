/*-
 * See the file LICENSE for redistribution information.
 *
 * bdb_bench.h -- shared harness for the TPROC-* benchmark suite.
 *
 * These are HammerDB-style ("TPROC-C/H/B") workloads, independently
 * implemented against the libdb C API; they are not the TPC benchmarks and
 * produce no TPC-comparable metrics.
 *
 * Every benchmark links this header for a common set of safety-feature
 * toggles, so each workload can be run with full ACID guarantees or with
 * individual protections disabled, to isolate their cost:
 *
 *   -X txn       run WITHOUT transactions (no DB_INIT_TXN; ops are autocommit
 *                only where the AM requires it, otherwise raw)
 *   -X lock      run WITHOUT the lock manager (no DB_INIT_LOCK; single-thread
 *                or app-serialized only)
 *   -X log       run WITHOUT logging/recovery (no DB_INIT_LOG)
 *   -d sync      fully durable commits (default is nosync)
 *   -d wnosync   write-nosync (flush log to OS, not disk)
 *   -d nosync    no log sync at commit (default)
 *   -m           use MVCC / snapshot isolation (DB_MULTIVERSION + snapshot txns)
 *   -C           use Concurrent Data Store (DB_INIT_CDB) instead of full txns
 *
 * plus shared knobs: -c cachebytes, -t threads, -S scale, -s seconds, -i init.
 */
#ifndef BDB_BENCH_H
#define BDB_BENCH_H

#include <sys/types.h>
#include <sys/time.h>
#include <errno.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include <db.h>

/* Durability levels at commit. */
enum bb_durability { BB_NOSYNC = 0, BB_WRITE_NOSYNC, BB_SYNC };

/* Shared configuration parsed from argv. */
typedef struct {
	const char *home;	/* environment directory */
	uint64_t cachebytes;	/* mpool cache size */
	int threads;		/* worker threads */
	int scale;		/* warehouses (C), scale factor (H/B) */
	int seconds;		/* measured run length */
	int init;		/* (re)create + populate, then exit */
	unsigned seed;

	/* Safety toggles. */
	int use_txn;		/* DB_INIT_TXN + per-op transactions */
	int use_lock;		/* DB_INIT_LOCK */
	int use_log;		/* DB_INIT_LOG */
	int use_mvcc;		/* DB_MULTIVERSION + DB_TXN_SNAPSHOT readers */
	int use_cdb;		/* DB_INIT_CDB (concurrent data store) */
	int dd_periodic;	/* deadlock detect: 0=on every conflict (default),
				 * else run a background detector every N ms and
				 * set_lk_detect(NONE) on the hot path */
	enum bb_durability durability;
} bb_config;

/* Sensible defaults: full safety, nosync, all in cache, single thread. */
static void
bb_config_defaults(bb_config *c)
{
	memset(c, 0, sizeof(*c));
	c->home = "TPROCDIR";
	c->cachebytes = (uint64_t)256 * 1024 * 1024;
	c->threads = 1;
	c->scale = 1;
	c->seconds = 10;
	c->seed = (unsigned)time(NULL);
	c->use_txn = 1;
	c->use_lock = 1;
	c->use_log = 1;
	c->use_mvcc = 0;
	c->use_cdb = 0;
	c->durability = BB_NOSYNC;
}

/*
 * bb_getopt -- parse the shared options.  A benchmark may pre-scan argv for
 * its own flags; unknown flags are reported by the caller's usage().  Returns
 * 0 on success, -1 on a parse error (after printing a message).
 *
 * Recognized: -h home -c cache -t threads -S scale -s secs -i -m -C
 *             -d sync|wnosync|nosync  -X txn|lock|log  -R seed
 */
static int
bb_getopt(int argc, char **argv, bb_config *c)
{
	int ch;
	extern char *optarg;

	while ((ch = getopt(argc, argv, "h:c:t:S:s:imCd:X:R:D:")) != EOF)
		switch (ch) {
		case 'h': c->home = optarg; break;
		case 'c': c->cachebytes = strtoull(optarg, NULL, 10); break;
		case 't': c->threads = atoi(optarg); break;
		case 'S': c->scale = atoi(optarg); break;
		case 's': c->seconds = atoi(optarg); break;
		case 'i': c->init = 1; break;
		case 'm': c->use_mvcc = 1; break;
		case 'C': c->use_cdb = 1; break;
		case 'R': c->seed = (unsigned)strtoul(optarg, NULL, 10); break;
		case 'D': c->dd_periodic = atoi(optarg); break;	/* dd interval ms */
		case 'd':
			if (strcmp(optarg, "sync") == 0) c->durability = BB_SYNC;
			else if (strcmp(optarg, "wnosync") == 0)
				c->durability = BB_WRITE_NOSYNC;
			else if (strcmp(optarg, "nosync") == 0)
				c->durability = BB_NOSYNC;
			else { fprintf(stderr, "bad -d %s\n", optarg); return -1; }
			break;
		case 'X':
			if (strcmp(optarg, "txn") == 0) c->use_txn = 0;
			else if (strcmp(optarg, "lock") == 0) c->use_lock = 0;
			else if (strcmp(optarg, "log") == 0) c->use_log = 0;
			else { fprintf(stderr, "bad -X %s\n", optarg); return -1; }
			break;
		default: return -1;
		}

	/*
	 * Consistency: MVCC and CDB both require transactions; CDB and full
	 * txns are mutually exclusive.  Resolve quietly toward the safe option
	 * so a partial flag set still runs.
	 */
	if ((c->use_mvcc || c->use_cdb) && !c->use_txn && !c->use_cdb)
		c->use_txn = 1;
	if (c->use_cdb)
		c->use_txn = 0;		/* CDB is its own concurrency model */
	if (c->use_mvcc)
		c->use_log = c->use_txn = 1;
	return 0;
}

/* Build the env open flags from the toggles. */
static u_int32_t
bb_env_flags(const bb_config *c)
{
	u_int32_t f = DB_CREATE | DB_INIT_MPOOL | DB_THREAD;

	if (c->use_cdb)
		return f | DB_INIT_CDB;
	if (c->use_lock) f |= DB_INIT_LOCK;
	if (c->use_log)  f |= DB_INIT_LOG;
	if (c->use_txn)  f |= DB_INIT_TXN | DB_INIT_LOCK | DB_INIT_LOG;
	return f;
}

/* Open (and configure) the environment per the safety toggles. */
static int
bb_env_open(bb_config *c, DB_ENV **envp)
{
	DB_ENV *env;
	int ret;

	if ((ret = db_env_create(&env, 0)) != 0) {
		fprintf(stderr, "db_env_create: %s\n", db_strerror(ret));
		return ret;
	}
	env->set_errfile(env, stderr);
	env->set_errpfx(env, "tproc");
	(void)env->set_cachesize(env, (u_int32_t)(c->cachebytes >> 32),
	    (u_int32_t)(c->cachebytes & 0xffffffff), 1);

	/*
	 * With locking on, enable automatic deadlock detection so a genuine
	 * deadlock between concurrent transactions is broken (one is chosen as
	 * victim and returns DB_LOCK_DEADLOCK) instead of blocking forever --
	 * the workloads here intentionally contend on shared rows.
	 */
	/*
	 * Deadlock detection.  By default BDB can run the detector on every
	 * lock conflict (set_lk_detect) -- correct, but every blocked acquire
	 * pays the detector's cost inline.  With -D N we instead leave the hot
	 * path free of detection and run a background detector every N ms
	 * (started by bb_start_dd after open); a victim is chosen at the next
	 * sweep rather than immediately.  This A/B isolates the detector cost.
	 */
	if ((c->use_lock || c->use_txn) && c->dd_periodic == 0)
		(void)env->set_lk_detect(env, DB_LOCK_DEFAULT);

	/*
	 * Size the lock subsystem generously.  The default region holds only
	 * ~1000 locks/lockers/objects; a batched bulk load or a many-thread
	 * run needs far more (each held lock and each active transaction
	 * consumes entries), and exhausting them returns ENOMEM mid-run.
	 */
	if (c->use_lock || c->use_txn) {
		(void)env->set_lk_max_locks(env, 200000);
		(void)env->set_lk_max_objects(env, 200000);
		(void)env->set_lk_max_lockers(env, 200000);
	}

	/*
	 * Size the in-memory log buffer so a write-heavy run does not stall
	 * rolling tiny (default) log segments.  Durability is governed
	 * separately by the -d toggle below.
	 */
	if (c->use_log || c->use_txn)
		(void)env->set_lg_bsize(env, 16 * 1024 * 1024);

	if (c->use_txn) {
		if (c->durability == BB_NOSYNC)
			(void)env->set_flags(env, DB_TXN_NOSYNC, 1);
		else if (c->durability == BB_WRITE_NOSYNC)
			(void)env->set_flags(env, DB_TXN_WRITE_NOSYNC, 1);
	}

	if ((ret = env->open(env, c->home, bb_env_flags(c), 0)) != 0) {
		env->err(env, ret, "DB_ENV->open: %s", c->home);
		(void)env->close(env, 0);
		return ret;
	}
	*envp = env;
	return 0;
}

/*
 * bb_begin / bb_commit / bb_abort -- transaction helpers that become no-ops
 * when -X txn is set, so each workload's body is written once and runs in both
 * modes.  A snapshot (MVCC) read transaction is requested when use_mvcc and rdonly.
 */
static int
bb_begin(const bb_config *c, DB_ENV *env, DB_TXN **txnp, int rdonly)
{
	u_int32_t flags = 0;

	*txnp = NULL;
	if (!c->use_txn)
		return 0;
	if (c->use_mvcc && rdonly)
		flags |= DB_TXN_SNAPSHOT;
	if (c->durability == BB_NOSYNC)
		flags |= DB_TXN_NOSYNC;
	return env->txn_begin(env, NULL, txnp, flags);
}

static int
bb_commit(DB_TXN *txn)
{
	return txn == NULL ? 0 : txn->commit(txn, 0);
}

static int
bb_abort(DB_TXN *txn)
{
	return txn == NULL ? 0 : txn->abort(txn);
}

/* Per-DB open flags: add DB_MULTIVERSION when MVCC is requested. */
static u_int32_t
bb_db_flags(const bb_config *c)
{
	u_int32_t f = DB_CREATE | DB_THREAD;

	if (c->use_txn)
		f |= DB_AUTO_COMMIT;
	if (c->use_mvcc)
		f |= DB_MULTIVERSION;
	return f;
}

/* Monotonic milliseconds. */
static double
bb_now_ms(void)
{
	struct timeval tv;

	(void)gettimeofday(&tv, NULL);
	return (double)tv.tv_sec * 1000.0 + (double)tv.tv_usec / 1000.0;
}

/* Cheap thread-local PRNG (xorshift) so threads don't share rand() state. */
typedef struct { uint64_t s; } bb_rng;

static void
bb_rng_seed(bb_rng *r, uint64_t seed)
{
	r->s = seed ? seed : 0x9e3779b97f4a7c15ULL;
}

static uint64_t
bb_rng_next(bb_rng *r)
{
	uint64_t x = r->s;

	x ^= x << 13;
	x ^= x >> 7;
	x ^= x << 17;
	return (r->s = x);
}

/* Uniform integer in [lo, hi]. */
static uint32_t
bb_rand_between(bb_rng *r, uint32_t lo, uint32_t hi)
{
	return lo + (uint32_t)(bb_rng_next(r) % (uint64_t)(hi - lo + 1));
}

/* One-line summary of the active safety configuration. */
static void
bb_print_config(const bb_config *c, const char *name)
{
	printf("# %s: threads=%d scale=%d secs=%d cache=%lluMB "
	    "txn=%d lock=%d log=%d mvcc=%d cdb=%d durability=%s\n",
	    name, c->threads, c->scale, c->seconds,
	    (unsigned long long)(c->cachebytes >> 20),
	    c->use_txn, c->use_lock, c->use_log, c->use_mvcc, c->use_cdb,
	    c->durability == BB_SYNC ? "sync" :
	    c->durability == BB_WRITE_NOSYNC ? "wnosync" : "nosync");
}

/* ---- background deadlock detector (for -D N) -------------------- */
struct bb_dd_arg { DB_ENV *env; int interval_ms; volatile int *stop; };

static void *
bb_dd_thread(void *a)
{
	struct bb_dd_arg *arg = a;
	int rejected;

	while (!*arg->stop) {
		usleep((useconds_t)arg->interval_ms * 1000);
		(void)arg->env->lock_detect(arg->env, 0,
		    DB_LOCK_YOUNGEST, &rejected);
	}
	return NULL;
}

/*
 * bb_start_dd / bb_stop_dd -- run a periodic deadlock detector when
 * c->dd_periodic > 0.  No-ops otherwise.  *stop must outlive the thread.
 */
static int
bb_start_dd(const bb_config *c, DB_ENV *env, pthread_t *tid,
    struct bb_dd_arg *arg, volatile int *stop)
{
	if (c->dd_periodic <= 0)
		return 0;
	arg->env = env;
	arg->interval_ms = c->dd_periodic;
	arg->stop = stop;
	return pthread_create(tid, NULL, bb_dd_thread, arg);
}

#endif /* BDB_BENCH_H */
