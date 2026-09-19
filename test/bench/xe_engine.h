/*-
 * See the file LICENSE for redistribution information.
 *
 * xe_engine.h -- single-source dual-backend storage abstraction for the
 * cross-engine TPROC benchmarks (libdb vs WiredTiger).
 *
 * WHY THIS EXISTS.  A cross-engine benchmark is only honest if both engines
 * run the SAME workload code: the same transaction mix, the same key
 * distributions, the same row counts, the same retry policy.  Two separate
 * drivers drift -- one gets a batching trick, the other an accidentally
 * different value size -- and the resulting ratio measures the drivers, not
 * the engines.  So the workload is written once against this interface and
 * every engine-specific decision is confined to the ~600 lines below.
 *
 * A previous campaign (see CROSS-ENGINE-2026-09.md) used exactly this shape in
 * a file called kvbench.c that was never committed and is now lost.  This one
 * is committed.
 *
 * THE INTERFACE, and what it deliberately does NOT abstract:
 *
 *   xe_open/xe_close            environment/connection lifecycle
 *   xe_table_open               one keyspace, with an access method
 *   xe_txn_begin/commit/abort   transaction boundaries
 *   xe_get/xe_put/xe_del        point operations
 *   xe_cursor_*                 ordered iteration (BTREE/row-store only)
 *   xe_stats                    cache hit rate, bytes read, page counts
 *
 * It does NOT abstract away the access-method distinction.  xe_table_open
 * takes XE_AM_BTREE or XE_AM_HASH, and xe_cursor_seek_ge FAILS LOUDLY with
 * XE_ENOORDER on a hash table rather than silently degrading to a full scan.
 * That is the point of arm 2: a workload step that needs ordered access must
 * either be reimplemented or reported N/A, never quietly replaced with a
 * different query that happens to run.
 *
 * ENGINES
 *   XE_ENGINE_LIBDB   libdb (this fork), DB_BTREE or DB_HASH
 *   XE_ENGINE_WT      WiredTiger, type=row only (never type=lsm)
 *
 * WiredTiger has no hash access method, so the HASH and MIXED arms are
 * libdb-only by construction; WT's arm is always row-store B-tree.  That is a
 * property of WiredTiger, and the report says so rather than inventing a
 * comparison.
 */
#ifndef XE_ENGINE_H
#define XE_ENGINE_H

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <errno.h>
#include <math.h>
#include <pthread.h>
#include <stdarg.h>
#include <stdint.h>
#include <stddef.h>		/* offsetof, used by the workload record sizes */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include <db.h>
#ifdef XE_HAVE_WIREDTIGER
#include <wiredtiger.h>
#endif

/* Engine selection. */
enum xe_engine { XE_ENGINE_LIBDB = 0, XE_ENGINE_WT };

/* Access method for one table. */
enum xe_am { XE_AM_BTREE = 0, XE_AM_HASH };

/* Arm: which access-method policy the whole run uses. */
enum xe_amcfg { XE_AMCFG_BTREE = 0, XE_AMCFG_HASH, XE_AMCFG_MIXED };

/*
 * Return codes.  The workload code compares against these, never against
 * engine-native values, so one body serves both engines.
 */
#define	XE_OK		0
#define	XE_NOTFOUND	(-30000)	/* no such key */
#define	XE_CONFLICT	(-30001)	/* deadlock / write conflict: retry */
#define	XE_ENOORDER	(-30002)	/* ordered op attempted on a hash table */
#define	XE_ERROR	(-30003)	/* anything else; details already printed */

/* Durability at commit. */
enum xe_dur { XE_DUR_NOSYNC = 0, XE_DUR_WRITE_NOSYNC, XE_DUR_SYNC };

typedef struct xe_env xe_env;
typedef struct xe_table xe_table;
typedef struct xe_txn xe_txn;
typedef struct xe_cursor xe_cursor;

struct xe_config {
	enum xe_engine engine;
	enum xe_amcfg amcfg;
	const char *home;
	uint64_t cachebytes;
	int threads;
	int scale;
	int seconds;
	int warmup;		/* warm-to-steady-state seconds before measuring */
	int init;
	int direct_io;		/* O_DIRECT: broken on libdb here, no-op on WT */
	/*
	 * aio: libdb DB_MPOOL_AIO (io_uring buffer-pool I/O).  Opt-in and
	 * default-OFF upstream because of the S1 deadlock class (fixed this
	 * cycle, 5.7% -> 0/384, still shipped default-off).
	 *
	 * BOTH settings are measured in this campaign, because with only the
	 * io_uring arm a libdb-vs-WT difference is unattributable between "the
	 * engine" and "the I/O path" -- WiredTiger has no io_uring backend at
	 * all (verified: zero io_uring references in its source, does not link
	 * liburing), so io_uring parity is impossible and the like-for-like
	 * comparison is libdb-sync vs WT.
	 */
	int aio;
	int use_mvcc;
	unsigned seed;
	enum xe_dur durability;
	int pagesize_kb;
	int verbose;
};

/* ---- libdb backend state ---- */
struct xe_env {
	struct xe_config cfg;
	DB_ENV *dbenv;
#ifdef XE_HAVE_WIREDTIGER
	WT_CONNECTION *wtconn;
	char wtconfig[2048];	/* the EXACT wiredtiger_open string, recorded */
#endif
};

struct xe_table {
	xe_env *env;
	enum xe_am am;
	char name[64];
	DB *db;
#ifdef XE_HAVE_WIREDTIGER
	char uri[96];
#endif
};

/*
 * A transaction handle.  For WiredTiger the transaction lives on the SESSION,
 * not on a separate object, and every cursor must come from the same session
 * as its transaction -- so a WT "txn" carries its session, and each worker
 * thread owns one session for its whole life (opening a session per
 * transaction would measure session setup, not the workload).
 */
struct xe_txn {
	xe_env *env;
	DB_TXN *dbtxn;
#ifdef XE_HAVE_WIREDTIGER
	WT_SESSION *session;
	int active;
#endif
	int rdonly;
};

struct xe_cursor {
	xe_table *tbl;
	xe_txn *txn;
	DBC *dbc;
#ifdef XE_HAVE_WIREDTIGER
	WT_CURSOR *wtc;
#endif
};

/*
 * Per-thread engine context.  libdb needs nothing per thread (DB_THREAD
 * handles are shared); WiredTiger needs one long-lived session.  The workload
 * calls xe_thread_init once per worker and passes the context to every
 * txn_begin.
 */
typedef struct {
	xe_env *env;
#ifdef XE_HAVE_WIREDTIGER
	WT_SESSION *session;
#endif
	int tid;
} xe_thread;

/* ---------------- small shared utilities ---------------- */

static double
xe_now_ms(void)
{
	struct timespec ts;

	(void)clock_gettime(CLOCK_MONOTONIC, &ts);
	return (double)ts.tv_sec * 1000.0 + (double)ts.tv_nsec / 1e6;
}

static double
xe_now_us(void)
{
	struct timespec ts;

	(void)clock_gettime(CLOCK_MONOTONIC, &ts);
	return (double)ts.tv_sec * 1e6 + (double)ts.tv_nsec / 1e3;
}

/* xorshift PRNG, per thread so threads never share state. */
typedef struct { uint64_t s; } xe_rng;

static void
xe_rng_seed(xe_rng *r, uint64_t seed)
{
	r->s = seed ? seed : 0x9e3779b97f4a7c15ULL;
}

static uint64_t
xe_rng_next(xe_rng *r)
{
	uint64_t x = r->s;

	x ^= x << 13;
	x ^= x >> 7;
	x ^= x << 17;
	return (r->s = x);
}

static uint32_t
xe_rand_between(xe_rng *r, uint32_t lo, uint32_t hi)
{
	if (hi <= lo)
		return lo;
	return lo + (uint32_t)(xe_rng_next(r) % (uint64_t)(hi - lo + 1));
}

/*
 * Keys are fixed-width big-endian byte strings.  Big-endian matters: it makes
 * lexicographic byte order agree with numeric order, so an ordered cursor scan
 * over a composite key visits rows in the schema's natural order in BOTH
 * engines.  WiredTiger's default row-store collator is a memcmp, and libdb's
 * default btree compare is also a memcmp, so one encoding gives both engines
 * identical ordering with no custom comparator on either side -- which keeps
 * the comparison fair (a custom C comparator callback in one engine and a
 * built-in in the other would be measuring callback overhead).
 */
#define	XE_KEYLEN	12		/* three big-endian uint32 fields */

typedef struct { uint8_t b[XE_KEYLEN]; } xe_key;

static void
xe_key_enc(xe_key *k, uint32_t a, uint32_t b, uint32_t c)
{
	k->b[0]  = (uint8_t)(a >> 24); k->b[1]  = (uint8_t)(a >> 16);
	k->b[2]  = (uint8_t)(a >> 8);  k->b[3]  = (uint8_t)a;
	k->b[4]  = (uint8_t)(b >> 24); k->b[5]  = (uint8_t)(b >> 16);
	k->b[6]  = (uint8_t)(b >> 8);  k->b[7]  = (uint8_t)b;
	k->b[8]  = (uint8_t)(c >> 24); k->b[9]  = (uint8_t)(c >> 16);
	k->b[10] = (uint8_t)(c >> 8);  k->b[11] = (uint8_t)c;
}

static uint32_t
xe_key_field(const xe_key *k, int which)
{
	const uint8_t *p = k->b + which * 4;

	return ((uint32_t)p[0] << 24) | ((uint32_t)p[1] << 16) |
	       ((uint32_t)p[2] << 8) | (uint32_t)p[3];
}

/* ---------------- latency histogram (p50/p99/p99.9) ---------------- */

/*
 * A log-linear bucketed histogram: 1 us resolution to 1 ms, then coarser.
 * Fixed 4096 buckets covering 1 us .. ~60 s.  Per thread, merged at the end,
 * so recording costs one increment and no lock -- the requirement is p99.9
 * per transaction type, which needs the tail retained, and storing every
 * sample for a 96-thread 60-second run would be gigabytes.
 *
 * Bucket i holds latencies [base(i), base(i+1)).  base grows geometrically at
 * 2^(1/64), giving ~1.1% relative error at every magnitude -- far finer than
 * the run-to-run CV we are trying to resolve.
 */
#define	XE_HIST_BUCKETS	1600
#define	XE_HIST_SUBBITS	6		/* 64 buckets per octave */

typedef struct {
	uint64_t count[XE_HIST_BUCKETS];
	uint64_t n;
	double sum_us;
	double max_us;
} xe_hist;

static int
xe_hist_bucket(double us)
{
	int oct, sub, idx;
	double m;

	if (us < 1.0)
		return 0;
	oct = 0;
	m = us;
	while (m >= 2.0) { m /= 2.0; oct++; }
	/* m in [1,2): split the octave into 2^XE_HIST_SUBBITS pieces. */
	sub = (int)((m - 1.0) * (double)(1 << XE_HIST_SUBBITS));
	if (sub >= (1 << XE_HIST_SUBBITS)) sub = (1 << XE_HIST_SUBBITS) - 1;
	idx = (oct << XE_HIST_SUBBITS) + sub;
	if (idx >= XE_HIST_BUCKETS) idx = XE_HIST_BUCKETS - 1;
	return idx;
}

static double
xe_hist_bucket_low(int idx)
{
	int oct = idx >> XE_HIST_SUBBITS;
	int sub = idx & ((1 << XE_HIST_SUBBITS) - 1);

	return ldexp(1.0 + (double)sub / (double)(1 << XE_HIST_SUBBITS), oct);
}

static void
xe_hist_add(xe_hist *h, double us)
{
	h->count[xe_hist_bucket(us)]++;
	h->n++;
	h->sum_us += us;
	if (us > h->max_us) h->max_us = us;
}

static void
xe_hist_merge(xe_hist *dst, const xe_hist *src)
{
	int i;

	for (i = 0; i < XE_HIST_BUCKETS; i++)
		dst->count[i] += src->count[i];
	dst->n += src->n;
	dst->sum_us += src->sum_us;
	if (src->max_us > dst->max_us) dst->max_us = src->max_us;
}

/* Quantile by cumulative count.  Returns the bucket's lower bound. */
static double
xe_hist_q(const xe_hist *h, double q)
{
	uint64_t target, cum = 0;
	int i;

	if (h->n == 0)
		return 0.0;
	target = (uint64_t)(q * (double)h->n);
	for (i = 0; i < XE_HIST_BUCKETS; i++) {
		cum += h->count[i];
		if (cum >= target)
			return xe_hist_bucket_low(i);
	}
	return h->max_us;
}

/* ---------------- libdb backend ---------------- */

/*
 * RFC 0007 optimistic-read counters.  These are plain globals in
 * src/btree/bt_search.c, exported so a benchmark can VERIFY that the
 * optimistic descent actually engaged rather than assuming it did because
 * set_thread_count was called.  Declared weak so a libdb without them still
 * links (the value then stays 0 and the harness reports "unavailable" instead
 * of claiming engagement it cannot see).
 */
#ifndef XE_HAVE_WIREDTIGER_ONLY
extern u_int32_t __bam_opt_tries __attribute__((weak));
extern u_int32_t __bam_opt_pages __attribute__((weak));
extern u_int32_t __bam_opt_invalid __attribute__((weak));
extern u_int32_t __bam_opt_bailouts __attribute__((weak));
#endif

static int
xe_libdb_err(xe_env *env, int ret, const char *what)
{
	if (ret == DB_LOCK_DEADLOCK || ret == DB_LOCK_NOTGRANTED)
		return XE_CONFLICT;
	if (ret == DB_NOTFOUND || ret == DB_KEYEMPTY)
		return XE_NOTFOUND;
	if (ret != 0) {
		fprintf(stderr, "libdb %s: %s\n", what, db_strerror(ret));
		return XE_ERROR;
	}
	return XE_OK;
}

static int
xe_libdb_open(xe_env *env)
{
	DB_ENV *dbenv;
	struct xe_config *c = &env->cfg;
	u_int32_t flags;
	int ret;

	if ((ret = db_env_create(&dbenv, 0)) != 0) {
		fprintf(stderr, "db_env_create: %s\n", db_strerror(ret));
		return XE_ERROR;
	}
	dbenv->set_errfile(dbenv, stderr);
	dbenv->set_errpfx(dbenv, "xe-libdb");

	(void)dbenv->set_cachesize(dbenv,
	    (u_int32_t)(c->cachebytes >> 32),
	    (u_int32_t)(c->cachebytes & 0xffffffff),
	    /*
	     * ncache: split the cache into regions.  A 139 GiB cache in one
	     * region would be one hash table and one set of region locks for
	     * 96 threads; BDB's own guidance is multiple caches for large
	     * pools.  8 gives ~17 GiB per region.
	     */
	    8);

	/*
	 * O_DIRECT on the database files.  DISABLED BY DEFAULT and not used in
	 * this campaign, because it is BROKEN on this platform: libdb really
	 * does pass O_DIRECT to open() (strace confirms it, unlike WiredTiger's
	 * dead direct_io option), but the very first meta-page read then fails
	 *
	 *   BDB0134 read: 0x7f7ff9a148f0, 8192: Invalid argument
	 *
	 * and DB->open returns an error, so no database can be opened at all.
	 * The address in that message is not 512-byte aligned, which is exactly
	 * what O_DIRECT rejects: __fop_read_meta reads the meta page through
	 * __os_read into an ordinary (unaligned) buffer, with no alignment
	 * provision for the O_DIRECT case.  Reproduced on a fresh create AND on
	 * reopening a file created without it, so it is not a create-path race.
	 *
	 * Consequence for this experiment: NEITHER engine can be given
	 * O_DIRECT (WT's option is a no-op, libdb's is broken), so the OS page
	 * cache is bounded for both from OUTSIDE, by a cgroup v2 memory.max --
	 * which is symmetric, does not depend on either engine cooperating, and
	 * is the honest way to get the property the experiment needs.
	 */
	if (c->direct_io)
		(void)dbenv->set_flags(dbenv, DB_DIRECT_DB, 1);

	/*
	 * io_uring for buffer-pool I/O.  Opt-in and default-OFF upstream
	 * because of the S1 deadlock class (fixed this cycle, 5.7% -> 0/384,
	 * still default-off).  Enabling it is a deliberate condition of this
	 * run; if it stalls, that is a finding.
	 */
	if (c->aio)
		(void)dbenv->set_flags(dbenv, DB_MPOOL_AIO, 1);

	/*
	 * RFC 0007: the optimistic read path allocates its pin list in the
	 * thread region, which exists only when thr_max != 0.  Without this
	 * call the path correctly refuses to engage -- so a run that forgets it
	 * measures the pessimistic path while believing otherwise.  We verify
	 * engagement from the counters at the end rather than trusting this.
	 */
	(void)dbenv->set_thread_count(dbenv, (u_int32_t)(c->threads * 2 + 16));

	(void)dbenv->set_lk_detect(dbenv, DB_LOCK_DEFAULT);
	(void)dbenv->set_lk_max_locks(dbenv, 2000000);
	(void)dbenv->set_lk_max_objects(dbenv, 2000000);
	(void)dbenv->set_lk_max_lockers(dbenv, 2000000);
	(void)dbenv->set_lg_bsize(dbenv, 256 * 1024 * 1024);
	(void)dbenv->set_lg_max(dbenv, 1024 * 1024 * 1024);
	/*
	 * Reclaim log files once they are no longer needed for recovery.
	 *
	 * WITHOUT THIS the load of a ~107 GiB dataset left 205 log segments
	 * totalling 205 GB -- nearly 2x the data itself -- and then every
	 * measured run opening the environment with DB_RECOVER scanned all of
	 * them before doing any work.  Observed directly: a 75-second run sat
	 * burning CPU for 9+ minutes having produced no output, which is
	 * indistinguishable from the S1 stall class we are separately trying to
	 * measure on the io_uring arms.  That confusion is the real damage: it
	 * would have been logged as a DB_MPOOL_AIO stall finding.
	 *
	 * The log is still enabled and commits are still logged -- durability is
	 * unchanged; only the retention of already-checkpointed segments changes.
	 */
	(void)dbenv->log_set_config(dbenv, DB_LOG_AUTO_REMOVE, 1);
	/* Transaction slots for a 96-thread run with long analytic readers. */
	(void)dbenv->set_tx_max(dbenv, 200000);

	if (c->durability == XE_DUR_NOSYNC)
		(void)dbenv->set_flags(dbenv, DB_TXN_NOSYNC, 1);
	else if (c->durability == XE_DUR_WRITE_NOSYNC)
		(void)dbenv->set_flags(dbenv, DB_TXN_WRITE_NOSYNC, 1);

	/*
	 * DB_RECOVER on open.  Correct for a fresh environment, but note it
	 * SCANS THE LOG, so it is only cheap when the log is small -- see the
	 * DB_LOG_AUTO_REMOVE note above for what happens when it is not.
	 */
	flags = DB_CREATE | DB_INIT_MPOOL | DB_INIT_LOCK | DB_INIT_LOG |
	    DB_INIT_TXN | DB_THREAD | DB_RECOVER;
	if ((ret = dbenv->open(dbenv, c->home, flags, 0)) != 0) {
		dbenv->err(dbenv, ret, "DB_ENV->open: %s", c->home);
		(void)dbenv->close(dbenv, 0);
		return XE_ERROR;
	}
	env->dbenv = dbenv;
	return XE_OK;
}

/* ---------------- WiredTiger backend ---------------- */
#ifdef XE_HAVE_WIREDTIGER

static int
xe_wt_err(int ret, const char *what)
{
	if (ret == WT_ROLLBACK)
		return XE_CONFLICT;
	if (ret == WT_NOTFOUND)
		return XE_NOTFOUND;
	if (ret != 0) {
		fprintf(stderr, "wt %s: %s\n", what, wiredtiger_strerror(ret));
		return XE_ERROR;
	}
	return XE_OK;
}

static int
xe_wt_open(xe_env *env)
{
	struct xe_config *c = &env->cfg;
	int ret;
	const char *sync;

	switch (c->durability) {
	case XE_DUR_SYNC:		sync = "fsync";	break;
	case XE_DUR_WRITE_NOSYNC:	sync = "dsync"; break;
	/*
	 * NOSYNC.  WT's transaction_sync method choices are none/fsync/dsync --
	 * "off" is NOT permitted and fails wiredtiger_open outright (caught in
	 * smoke, which is why the smoke test exists).  "none" means the log is
	 * written but not flushed at commit, which is what libdb's
	 * DB_TXN_NOSYNC also means, so the two engines match here.
	 */
	default:			sync = "none";	break;
	}

	/*
	 * The EXACT wiredtiger_open configuration, recorded verbatim in the
	 * report.  Notes on each choice, because every one of them is a
	 * fairness decision, and three of the options I first reached for were
	 * WRONG in ways that did not fail loudly (see xe_wt_probe.c):
	 *
	 *   cache_size          same byte budget as libdb's set_cachesize.
	 *   log=(enabled)       WT's write-ahead log, matching DB_INIT_LOG.
	 *                       NOTE: no path= -- setting path=journal fails
	 *                       ENOENT unless the directory already exists.
	 *   transaction_sync    matched to libdb's -d setting.
	 *   eviction=(threads)  WT needs eviction workers to keep a large cache
	 *                       from stalling; this is its own documented
	 *                       guidance for this size.  libdb's equivalent is
	 *                       its trickle/sync path, which is not a thread
	 *                       count -- so this is NOT perfectly symmetric and
	 *                       the report says so.
	 *   statistics=(fast)   needed for the cache-hit / bytes-read evidence.
	 *                       "fast" not "all": "all" adds per-op cost.
	 *   checkpoint=(wait)   periodic checkpoint, matching libdb's.
	 *   mmap=false          without this WT may map read-only files, which
	 *                       would be a page-cache path outside the cgroup
	 *                       accounting we rely on.
	 *
	 * DELIBERATELY NOT SET: direct_io.  It PARSES (so a run could set it,
	 * get rc=0 and claim O_DIRECT) but dist/api_data.py marks it "no longer
	 * supported, retained for backward compatibility", and an strace of a
	 * real 80 MB write through a 64 MB cache shows ZERO O_DIRECT opens on
	 * the data file with it set.  Setting a dead option and reporting it as
	 * a control would be exactly the fabricated-parity error this campaign
	 * exists to avoid.  The OS page cache is instead bounded for BOTH
	 * engines from outside, by a cgroup v2 memory.max -- see xe_run.sh.
	 *
	 * ALSO NOT SET: type=lsm (rejected by this build -- LSM is REMOVED from
	 * WiredTiger 12.0.0, so "B-tree only" is structural here, not a config
	 * choice), and any block_compressor.  Neither engine compresses.
	 */
	(void)snprintf(env->wtconfig, sizeof(env->wtconfig),
	    "create,"
	    "cache_size=%lluM,"
	    "log=(enabled=true,file_max=1024MB),"
	    "transaction_sync=(enabled=true,method=%s),"
	    "eviction=(threads_min=8,threads_max=16),"
	    "eviction_target=80,eviction_trigger=95,"
	    "checkpoint=(wait=60,log_size=2GB),"
	    "session_max=%d,"
	    "statistics=(fast),"
	    "mmap=false",
	    (unsigned long long)(c->cachebytes >> 20),
	    sync,
	    c->threads * 2 + 64);

	if ((ret = wiredtiger_open(c->home, NULL, env->wtconfig,
	    &env->wtconn)) != 0) {
		fprintf(stderr, "wiredtiger_open(%s, \"%s\"): %s\n",
		    c->home, env->wtconfig, wiredtiger_strerror(ret));
		return XE_ERROR;
	}
	return XE_OK;
}
#endif /* XE_HAVE_WIREDTIGER */

/* ---------------- public: env lifecycle ---------------- */

static const char *
xe_engine_name(enum xe_engine e)
{
	return e == XE_ENGINE_WT ? "wiredtiger" : "libdb";
}

static const char *
xe_amcfg_name(enum xe_amcfg a)
{
	return a == XE_AMCFG_HASH ? "hash" :
	       a == XE_AMCFG_MIXED ? "mixed" : "btree";
}

/* Arm label for output rows: engine + io mode + access method. */
static const char *
xe_arm_name(const struct xe_config *c)
{
	static char buf[64];

	if (c->engine == XE_ENGINE_WT)
		return "wt-btree";
	(void)snprintf(buf, sizeof(buf), "libdb-%s-%s",
	    c->aio ? "uring" : "sync", xe_amcfg_name(c->amcfg));
	return buf;
}

/*
 * xe_version_banner -- print the library identity of EVERY engine this binary
 * can reach, at the start of every run.
 *
 * This is not decoration.  A hardcoded -ldb-5.3 once resolved to Debian's 2013
 * Berkeley DB behind a benchmark's back and every published number described
 * Oracle's code instead of this fork.  The rule that came out of it: a run
 * that does not print the version of the library it is measuring is not a
 * measurement.  So this prints db_version() (the RUNTIME value, from the .so
 * actually loaded) next to DB_VERSION_STRING (the COMPILE-TIME value, from the
 * db.h actually included) -- if those two disagree, the harness is linked
 * against a different library than it was built against, which is precisely
 * the failure mode.
 */
static int
xe_version_banner(void)
{
	int major, minor, patch, bad = 0;
	const char *rt;

	rt = db_version(&major, &minor, &patch);
	printf("# libdb runtime : %s\n", rt);
	printf("# libdb compiled: %s\n", DB_VERSION_STRING);
	if (strcmp(rt, DB_VERSION_STRING) != 0) {
		printf("FAIL version mismatch: runtime \"%s\" != compiled "
		    "\"%s\" -- the harness is linked against a different "
		    "libdb than it was built against\n", rt, DB_VERSION_STRING);
		bad = 1;
	}
#ifdef XE_HAVE_WIREDTIGER
	printf("# wiredtiger    : %s\n", wiredtiger_version(NULL, NULL, NULL));
#else
	printf("# wiredtiger    : not compiled in\n");
#endif
	return bad;
}

static void
xe_config_defaults(struct xe_config *c)
{
	memset(c, 0, sizeof(*c));
	c->engine = XE_ENGINE_LIBDB;
	c->amcfg = XE_AMCFG_BTREE;
	c->home = "XEDIR";
	c->cachebytes = (uint64_t)1024 * 1024 * 1024;
	c->threads = 1;
	c->scale = 1;
	c->seconds = 30;
	c->warmup = 0;
	c->direct_io = 0;	/* see xe_libdb_open: broken on libdb, no-op on WT */
	c->aio = 0;
	c->seed = 42;
	c->durability = XE_DUR_NOSYNC;
	c->pagesize_kb = 8;
}

static int
xe_open(struct xe_config *cfg, xe_env **envp)
{
	xe_env *env;
	int ret;

	if ((env = calloc(1, sizeof(*env))) == NULL)
		return XE_ERROR;
	env->cfg = *cfg;

	if (cfg->engine == XE_ENGINE_WT) {
#ifdef XE_HAVE_WIREDTIGER
		ret = xe_wt_open(env);
#else
		fprintf(stderr, "built without WiredTiger support\n");
		ret = XE_ERROR;
#endif
	} else
		ret = xe_libdb_open(env);

	if (ret != XE_OK) { free(env); return ret; }
	*envp = env;
	return XE_OK;
}

static int
xe_close(xe_env *env)
{
	int ret = 0;

	if (env == NULL)
		return 0;
	if (env->cfg.engine == XE_ENGINE_WT) {
#ifdef XE_HAVE_WIREDTIGER
		if (env->wtconn != NULL)
			ret = env->wtconn->close(env->wtconn, NULL);
#endif
	} else if (env->dbenv != NULL)
		ret = env->dbenv->close(env->dbenv, 0);
	free(env);
	return ret;
}

/* ---------------- public: per-thread context ---------------- */

static int
xe_thread_init(xe_env *env, xe_thread *th, int tid)
{
	memset(th, 0, sizeof(*th));
	th->env = env;
	th->tid = tid;
#ifdef XE_HAVE_WIREDTIGER
	if (env->cfg.engine == XE_ENGINE_WT) {
		int ret;
		/*
		 * One session per thread for the thread's whole life.  A
		 * session per transaction would put session open/close in the
		 * measured path -- that is WT's documented anti-pattern and
		 * would understate WT, which is the kind of unfairness that
		 * makes a cross-engine number worthless.
		 */
		if ((ret = env->wtconn->open_session(env->wtconn, NULL,
		    "isolation=snapshot", &th->session)) != 0)
			return xe_wt_err(ret, "open_session");
	}
#endif
	return XE_OK;
}

static void
xe_thread_done(xe_thread *th)
{
#ifdef XE_HAVE_WIREDTIGER
	if (th->env->cfg.engine == XE_ENGINE_WT && th->session != NULL) {
		(void)th->session->close(th->session, NULL);
		th->session = NULL;
	}
#endif
}

/* ---------------- public: tables ---------------- */

/*
 * xe_table_open -- open (or create) one table with a specific access method.
 *
 * libdb: DB_BTREE or DB_HASH.
 * WiredTiger: type=row ALWAYS.  WT has no hash access method, so an
 * XE_AM_HASH request on WT is a harness bug, and we say so rather than
 * silently opening a B-tree and letting the report imply WT ran the hash arm.
 */
static int
xe_table_open(xe_env *env, xe_table **tblp, const char *name, enum xe_am am)
{
	xe_table *t;
	int ret;

	if ((t = calloc(1, sizeof(*t))) == NULL)
		return XE_ERROR;
	t->env = env;
	t->am = am;
	(void)snprintf(t->name, sizeof(t->name), "%s", name);

	if (env->cfg.engine == XE_ENGINE_WT) {
#ifdef XE_HAVE_WIREDTIGER
		WT_SESSION *s;
		char cfg[512];

		if (am != XE_AM_BTREE) {
			fprintf(stderr, "xe_table_open: WiredTiger has no hash "
			    "access method; refusing to substitute a B-tree "
			    "for table %s\n", name);
			free(t);
			return XE_ERROR;
		}
		(void)snprintf(t->uri, sizeof(t->uri), "table:%s", name);
		if ((ret = env->wtconn->open_session(env->wtconn, NULL, NULL,
		    &s)) != 0) { free(t); return xe_wt_err(ret, "open_session"); }
		/*
		 * A default `table:` IS a row-store B-tree in WiredTiger.
		 *
		 * NOTE: do NOT write type=row here.  WT's `type` selects a data
		 * SOURCE (file / lsm / an extension), not a page format, and
		 * `type=row` is REJECTED by this build with "unknown object
		 * type".  An earlier draft used it, every create failed, and the
		 * O_DIRECT probe built on it produced a vacuous result that
		 * happened to agree with the truth.  `type=lsm` is likewise
		 * rejected -- LSM is removed in 12.0.0 -- so the "B-tree only,
		 * no LSM" condition is guaranteed by the build itself.
		 *
		 * key_format=u/value_format=u: raw bytes, so WT stores exactly
		 * the same bytes libdb does and neither engine pays for a schema
		 * layer the other does not.  Page sizes matched to libdb's.
		 */
		(void)snprintf(cfg, sizeof(cfg),
		    "key_format=u,value_format=u,"
		    "internal_page_max=%dk,leaf_page_max=%dk,"
		    "leaf_value_max=1MB,memory_page_max=%dk,"
		    "block_compressor=,prefix_compression=false",
		    env->cfg.pagesize_kb, env->cfg.pagesize_kb,
		    env->cfg.pagesize_kb * 4);
		ret = s->create(s, t->uri, cfg);
		if (ret != 0 && ret != EEXIST) {
			(void)s->close(s, NULL);
			free(t);
			return xe_wt_err(ret, "create");
		}
		(void)s->close(s, NULL);
#endif
	} else {
		DB *db;
		u_int32_t f = DB_CREATE | DB_THREAD | DB_AUTO_COMMIT;

		if ((ret = db_create(&db, env->dbenv, 0)) != 0) {
			free(t);
			return xe_libdb_err(env, ret, "db_create");
		}
		(void)db->set_pagesize(db, (u_int32_t)env->cfg.pagesize_kb * 1024);
		if (env->cfg.use_mvcc)
			f |= DB_MULTIVERSION;
		/*
		 * For HASH, presize the table.  A hash DB that grows from
		 * nothing spends the load doing incremental splits; h_nelem
		 * lets it pick its initial bucket count once.  This is the
		 * hash-specific tuning equivalent of nothing on the btree
		 * side, and it is applied in the HASH and MIXED arms alike.
		 */
		if (am == XE_AM_HASH)
			(void)db->set_h_nelem(db,
			    (u_int32_t)(env->cfg.scale > 0 ?
			    env->cfg.scale * 1000000u : 1000000u));
		if ((ret = db->open(db, NULL, name, NULL,
		    am == XE_AM_HASH ? DB_HASH : DB_BTREE, f, 0)) != 0) {
			env->dbenv->err(env->dbenv, ret, "open %s", name);
			free(t);
			return XE_ERROR;
		}
		t->db = db;
	}
	*tblp = t;
	return XE_OK;
}

static int
xe_table_close(xe_table *t)
{
	int ret = 0;

	if (t == NULL)
		return 0;
	if (t->env->cfg.engine != XE_ENGINE_WT && t->db != NULL)
		ret = t->db->close(t->db, 0);
	free(t);
	return ret;
}

/* ---------------- public: transactions ---------------- */

static int
xe_txn_begin(xe_thread *th, xe_txn *txn, int rdonly)
{
	xe_env *env = th->env;
	int ret;

	memset(txn, 0, sizeof(*txn));
	txn->env = env;
	txn->rdonly = rdonly;

	if (env->cfg.engine == XE_ENGINE_WT) {
#ifdef XE_HAVE_WIREDTIGER
		txn->session = th->session;
		/*
		 * WT is always snapshot-isolated.  NOTE: `read_only=true` is
		 * NOT a begin_transaction option in this build (the valid set is
		 * ignore_cache_size, ignore_prepare, isolation, name,
		 * no_timestamp, operation_timeout_ms, priority, read_timestamp,
		 * roundup_timestamps, prepared, read, sync, claim_prepared_id) --
		 * an earlier draft passed it and would have failed every
		 * read-only transaction.  The rdonly flag is therefore advisory
		 * on the WT side and only steers libdb's DB_TXN_SNAPSHOT.
		 */
		if ((ret = txn->session->begin_transaction(txn->session,
		    "isolation=snapshot")) != 0)
			return xe_wt_err(ret, "begin_transaction");
		txn->active = 1;
#endif
	} else {
		u_int32_t f = 0;

		if (env->cfg.use_mvcc && rdonly)
			f |= DB_TXN_SNAPSHOT;
		if (env->cfg.durability == XE_DUR_NOSYNC)
			f |= DB_TXN_NOSYNC;
		if ((ret = env->dbenv->txn_begin(env->dbenv, NULL,
		    &txn->dbtxn, f)) != 0)
			return xe_libdb_err(env, ret, "txn_begin");
	}
	return XE_OK;
}

static int
xe_txn_commit(xe_txn *txn)
{
	int ret;

	if (txn->env->cfg.engine == XE_ENGINE_WT) {
#ifdef XE_HAVE_WIREDTIGER
		if (!txn->active)
			return XE_OK;
		txn->active = 0;
		ret = txn->session->commit_transaction(txn->session, NULL);
		return xe_wt_err(ret, "commit");
#else
		return XE_ERROR;
#endif
	}
	if (txn->dbtxn == NULL)
		return XE_OK;
	ret = txn->dbtxn->commit(txn->dbtxn, 0);
	txn->dbtxn = NULL;
	return xe_libdb_err(txn->env, ret, "commit");
}

static int
xe_txn_abort(xe_txn *txn)
{
	if (txn->env->cfg.engine == XE_ENGINE_WT) {
#ifdef XE_HAVE_WIREDTIGER
		if (!txn->active)
			return XE_OK;
		txn->active = 0;
		(void)txn->session->rollback_transaction(txn->session, NULL);
		return XE_OK;
#else
		return XE_ERROR;
#endif
	}
	if (txn->dbtxn == NULL)
		return XE_OK;
	(void)txn->dbtxn->abort(txn->dbtxn);
	txn->dbtxn = NULL;
	return XE_OK;
}

/* ---------------- public: point operations ---------------- */

static int
xe_get(xe_table *t, xe_txn *txn, const xe_key *key, void *val, size_t valsz,
    size_t *gotsz)
{
	int ret;

	if (t->env->cfg.engine == XE_ENGINE_WT) {
#ifdef XE_HAVE_WIREDTIGER
		WT_CURSOR *c;
		WT_ITEM ki, vi;

		/*
		 * WT caches cursors per (session, uri) when they are closed
		 * with the "cached" default, so open_cursor here is a cache
		 * lookup, not a construction, after the first call.
		 */
		if ((ret = txn->session->open_cursor(txn->session, t->uri, NULL,
		    NULL, &c)) != 0)
			return xe_wt_err(ret, "open_cursor");
		ki.data = key->b; ki.size = XE_KEYLEN;
		c->set_key(c, &ki);
		ret = c->search(c);
		if (ret == 0) {
			if ((ret = c->get_value(c, &vi)) == 0) {
				size_t n = vi.size < valsz ? vi.size : valsz;
				memcpy(val, vi.data, n);
				if (gotsz) *gotsz = vi.size;
			}
		}
		(void)c->close(c);
		return xe_wt_err(ret, "get");
#else
		return XE_ERROR;
#endif
	} else {
		DBT k, d;

		memset(&k, 0, sizeof(k));
		k.data = (void *)key->b; k.size = XE_KEYLEN;
		memset(&d, 0, sizeof(d));
		d.data = val; d.ulen = (u_int32_t)valsz;
		d.flags = DB_DBT_USERMEM;
		ret = t->db->get(t->db, txn->dbtxn, &k, &d, 0);
		if (ret == 0 && gotsz) *gotsz = d.size;
		return xe_libdb_err(t->env, ret, "get");
	}
}

static int
xe_put(xe_table *t, xe_txn *txn, const xe_key *key, const void *val,
    size_t valsz)
{
	int ret;

	if (t->env->cfg.engine == XE_ENGINE_WT) {
#ifdef XE_HAVE_WIREDTIGER
		WT_CURSOR *c;
		WT_ITEM ki, vi;

		if ((ret = txn->session->open_cursor(txn->session, t->uri, NULL,
		    NULL, &c)) != 0)
			return xe_wt_err(ret, "open_cursor");
		ki.data = key->b; ki.size = XE_KEYLEN;
		vi.data = val; vi.size = (uint32_t)valsz;
		c->set_key(c, &ki);
		c->set_value(c, &vi);
		ret = c->insert(c);
		(void)c->close(c);
		return xe_wt_err(ret, "put");
#else
		return XE_ERROR;
#endif
	} else {
		DBT k, d;

		memset(&k, 0, sizeof(k));
		k.data = (void *)key->b; k.size = XE_KEYLEN;
		memset(&d, 0, sizeof(d));
		d.data = (void *)val; d.size = (u_int32_t)valsz;
		ret = t->db->put(t->db, txn->dbtxn, &k, &d, 0);
		return xe_libdb_err(t->env, ret, "put");
	}
}

static int
xe_del(xe_table *t, xe_txn *txn, const xe_key *key)
{
	int ret;

	if (t->env->cfg.engine == XE_ENGINE_WT) {
#ifdef XE_HAVE_WIREDTIGER
		WT_CURSOR *c;
		WT_ITEM ki;

		if ((ret = txn->session->open_cursor(txn->session, t->uri, NULL,
		    NULL, &c)) != 0)
			return xe_wt_err(ret, "open_cursor");
		ki.data = key->b; ki.size = XE_KEYLEN;
		c->set_key(c, &ki);
		ret = c->remove(c);
		(void)c->close(c);
		return xe_wt_err(ret, "del");
#else
		return XE_ERROR;
#endif
	} else {
		DBT k;

		memset(&k, 0, sizeof(k));
		k.data = (void *)key->b; k.size = XE_KEYLEN;
		ret = t->db->del(t->db, txn->dbtxn, &k, 0);
		return xe_libdb_err(t->env, ret, "del");
	}
}

/* ---------------- public: ordered cursors ---------------- */

/*
 * xe_cursor_open / seek_ge / next / del / close.
 *
 * ORDERED ACCESS ON A HASH TABLE RETURNS XE_ENOORDER.  It does not fall back
 * to a full scan, and it does not pretend to succeed.  libdb's DB_HASH does
 * support a cursor, but DB_SET_RANGE is not available on it and DB_NEXT walks
 * BUCKET order, which has no relation to key order -- so a range predicate
 * evaluated over it would return a subset of the correct answer while looking
 * like it worked.  The workload must therefore either reimplement the step
 * without ordering (e.g. by enumerating the key space it would have scanned)
 * or report the query N/A.  Both happen in tproc_h.c, explicitly labelled.
 */
static int
xe_cursor_open(xe_table *t, xe_txn *txn, xe_cursor *cur)
{
	int ret;

	memset(cur, 0, sizeof(*cur));
	cur->tbl = t;
	cur->txn = txn;

	if (t->am == XE_AM_HASH)
		return XE_ENOORDER;

	if (t->env->cfg.engine == XE_ENGINE_WT) {
#ifdef XE_HAVE_WIREDTIGER
		if ((ret = txn->session->open_cursor(txn->session, t->uri, NULL,
		    NULL, &cur->wtc)) != 0)
			return xe_wt_err(ret, "open_cursor");
#endif
	} else {
		if ((ret = t->db->cursor(t->db, txn->dbtxn, &cur->dbc, 0)) != 0)
			return xe_libdb_err(t->env, ret, "cursor");
	}
	return XE_OK;
}

static int
xe_cursor_close(xe_cursor *cur)
{
	if (cur->tbl->env->cfg.engine == XE_ENGINE_WT) {
#ifdef XE_HAVE_WIREDTIGER
		if (cur->wtc != NULL) {
			(void)cur->wtc->close(cur->wtc);
			cur->wtc = NULL;
		}
#endif
	} else if (cur->dbc != NULL) {
		(void)cur->dbc->close(cur->dbc);
		cur->dbc = NULL;
	}
	return XE_OK;
}

/* Position at the first key >= *key.  Returns XE_NOTFOUND past the end. */
static int
xe_cursor_seek_ge(xe_cursor *cur, const xe_key *key, xe_key *gotkey,
    void *val, size_t valsz)
{
	int ret;

	if (cur->tbl->am == XE_AM_HASH)
		return XE_ENOORDER;

	if (cur->tbl->env->cfg.engine == XE_ENGINE_WT) {
#ifdef XE_HAVE_WIREDTIGER
		WT_ITEM ki, vi;
		int exact;

		ki.data = key->b; ki.size = XE_KEYLEN;
		cur->wtc->set_key(cur->wtc, &ki);
		if ((ret = cur->wtc->search_near(cur->wtc, &exact)) != 0)
			return xe_wt_err(ret, "search_near");
		/*
		 * search_near can land BEFORE the sought key (exact < 0).
		 * SET_RANGE semantics are "first key >= sought", so step
		 * forward once in that case.  Getting this wrong would give WT
		 * one extra row per scan -- small, but it is the kind of
		 * asymmetry that turns into a percent.
		 */
		if (exact < 0 && (ret = cur->wtc->next(cur->wtc)) != 0)
			return xe_wt_err(ret, "next");
		if ((ret = cur->wtc->get_key(cur->wtc, &ki)) != 0)
			return xe_wt_err(ret, "get_key");
		if ((ret = cur->wtc->get_value(cur->wtc, &vi)) != 0)
			return xe_wt_err(ret, "get_value");
		if (ki.size >= XE_KEYLEN) memcpy(gotkey->b, ki.data, XE_KEYLEN);
		memcpy(val, vi.data, vi.size < valsz ? vi.size : valsz);
		return XE_OK;
#else
		return XE_ERROR;
#endif
	} else {
		DBT k, d;

		memset(&k, 0, sizeof(k));
		k.data = (void *)key->b; k.size = XE_KEYLEN;
		/*
		 * DB_DBT_USERMEM on the key too: DB_SET_RANGE REPLACES the key
		 * with the found key, and without USERMEM libdb would write
		 * into our stack buffer's pointer.  gotkey is the destination.
		 */
		memcpy(gotkey->b, key->b, XE_KEYLEN);
		k.data = gotkey->b; k.ulen = XE_KEYLEN; k.size = XE_KEYLEN;
		k.flags = DB_DBT_USERMEM;
		memset(&d, 0, sizeof(d));
		d.data = val; d.ulen = (u_int32_t)valsz;
		d.flags = DB_DBT_USERMEM;
		ret = cur->dbc->get(cur->dbc, &k, &d, DB_SET_RANGE);
		return xe_libdb_err(cur->tbl->env, ret, "c_get SET_RANGE");
	}
}

static int
xe_cursor_first(xe_cursor *cur, xe_key *gotkey, void *val, size_t valsz)
{
	int ret;

	if (cur->tbl->am == XE_AM_HASH)
		return XE_ENOORDER;

	if (cur->tbl->env->cfg.engine == XE_ENGINE_WT) {
#ifdef XE_HAVE_WIREDTIGER
		WT_ITEM ki, vi;

		if ((ret = cur->wtc->next(cur->wtc)) != 0)
			return xe_wt_err(ret, "next(first)");
		if ((ret = cur->wtc->get_key(cur->wtc, &ki)) != 0)
			return xe_wt_err(ret, "get_key");
		if ((ret = cur->wtc->get_value(cur->wtc, &vi)) != 0)
			return xe_wt_err(ret, "get_value");
		if (ki.size >= XE_KEYLEN) memcpy(gotkey->b, ki.data, XE_KEYLEN);
		memcpy(val, vi.data, vi.size < valsz ? vi.size : valsz);
		return XE_OK;
#else
		return XE_ERROR;
#endif
	} else {
		DBT k, d;

		memset(&k, 0, sizeof(k));
		k.data = gotkey->b; k.ulen = XE_KEYLEN; k.flags = DB_DBT_USERMEM;
		memset(&d, 0, sizeof(d));
		d.data = val; d.ulen = (u_int32_t)valsz; d.flags = DB_DBT_USERMEM;
		ret = cur->dbc->get(cur->dbc, &k, &d, DB_FIRST);
		return xe_libdb_err(cur->tbl->env, ret, "c_get FIRST");
	}
}

static int
xe_cursor_next(xe_cursor *cur, xe_key *gotkey, void *val, size_t valsz)
{
	int ret;

	if (cur->tbl->am == XE_AM_HASH)
		return XE_ENOORDER;

	if (cur->tbl->env->cfg.engine == XE_ENGINE_WT) {
#ifdef XE_HAVE_WIREDTIGER
		WT_ITEM ki, vi;

		if ((ret = cur->wtc->next(cur->wtc)) != 0)
			return xe_wt_err(ret, "next");
		if ((ret = cur->wtc->get_key(cur->wtc, &ki)) != 0)
			return xe_wt_err(ret, "get_key");
		if ((ret = cur->wtc->get_value(cur->wtc, &vi)) != 0)
			return xe_wt_err(ret, "get_value");
		if (ki.size >= XE_KEYLEN) memcpy(gotkey->b, ki.data, XE_KEYLEN);
		memcpy(val, vi.data, vi.size < valsz ? vi.size : valsz);
		return XE_OK;
#else
		return XE_ERROR;
#endif
	} else {
		DBT k, d;

		memset(&k, 0, sizeof(k));
		k.data = gotkey->b; k.ulen = XE_KEYLEN; k.flags = DB_DBT_USERMEM;
		memset(&d, 0, sizeof(d));
		d.data = val; d.ulen = (u_int32_t)valsz; d.flags = DB_DBT_USERMEM;
		ret = cur->dbc->get(cur->dbc, &k, &d, DB_NEXT);
		return xe_libdb_err(cur->tbl->env, ret, "c_get NEXT");
	}
}

static int
xe_cursor_del(xe_cursor *cur)
{
	int ret;

	if (cur->tbl->am == XE_AM_HASH)
		return XE_ENOORDER;

	if (cur->tbl->env->cfg.engine == XE_ENGINE_WT) {
#ifdef XE_HAVE_WIREDTIGER
		ret = cur->wtc->remove(cur->wtc);
		return xe_wt_err(ret, "c_remove");
#else
		return XE_ERROR;
#endif
	}
	ret = cur->dbc->del(cur->dbc, 0);
	return xe_libdb_err(cur->tbl->env, ret, "c_del");
}

/* ---------------- public: cache / IO statistics ---------------- */

/*
 * The out-of-cache evidence.  Without these numbers "the working set is 10x
 * RAM" is an assertion about the loader, not a measurement of the run: a
 * workload could touch 1% of a huge dataset and be entirely cache-resident.
 * cache_hit/cache_miss and bytes_read are what show the reader that reads
 * actually went to the device.
 */
struct xe_stats {
	uint64_t cache_hit;
	uint64_t cache_miss;
	uint64_t pages_in;	/* pages read from disk */
	uint64_t pages_out;	/* pages written to disk */
	uint64_t bytes_read;
	uint64_t bytes_written;
	int have;
};

#ifdef XE_HAVE_WIREDTIGER
static int
xe_wt_stat(WT_SESSION *s, const char *uri, int want, uint64_t *out)
{
	WT_CURSOR *c;
	uint64_t v;
	const char *desc, *pval;
	int ret, key;

	*out = 0;
	if ((ret = s->open_cursor(s, uri, NULL, NULL, &c)) != 0)
		return ret;
	while ((ret = c->next(c)) == 0) {
		if (c->get_key(c, &key) != 0) continue;
		if (key != want) continue;
		if (c->get_value(c, &desc, &pval, &v) == 0) *out = v;
		break;
	}
	(void)c->close(c);
	return 0;
}
#endif

static void
xe_stats_get(xe_env *env, struct xe_stats *st)
{
	memset(st, 0, sizeof(*st));

	if (env->cfg.engine == XE_ENGINE_WT) {
#ifdef XE_HAVE_WIREDTIGER
		WT_SESSION *s;
		uint64_t v;

		if (env->wtconn->open_session(env->wtconn, NULL, NULL, &s) != 0)
			return;
		/*
		 * WT_STAT_CONN_* ids from wiredtiger.h.  cache_pages_requested
		 * counts logical page requests; cache_read counts pages read
		 * from disk into cache, so hit = requested - read.
		 *
		 * These five ids are VERIFIED PRESENT in this build's
		 * wiredtiger.h (1332, 1327, 1348, 1069, 1078).  Checked rather
		 * than assumed: the first set of names I reached for
		 * (WT_STAT_CONN_BLOCK_BYTE_READ aside) did not all exist, and a
		 * missing id here would silently report 0 cache misses and make
		 * an out-of-cache run look perfectly cached.
		 */
		if (xe_wt_stat(s, "statistics:", WT_STAT_CONN_CACHE_PAGES_REQUESTED, &v) == 0)
			st->cache_hit = v;
		if (xe_wt_stat(s, "statistics:", WT_STAT_CONN_CACHE_READ, &v) == 0)
			st->pages_in = v;
		if (xe_wt_stat(s, "statistics:", WT_STAT_CONN_CACHE_WRITE, &v) == 0)
			st->pages_out = v;
		if (xe_wt_stat(s, "statistics:", WT_STAT_CONN_BLOCK_BYTE_READ, &v) == 0)
			st->bytes_read = v;
		if (xe_wt_stat(s, "statistics:", WT_STAT_CONN_BLOCK_BYTE_WRITE, &v) == 0)
			st->bytes_written = v;
		st->cache_miss = st->pages_in;
		if (st->cache_hit > st->cache_miss)
			st->cache_hit -= st->cache_miss;
		st->have = 1;
		(void)s->close(s, NULL);
#endif
	} else {
		DB_MPOOL_STAT *mp;

		if (env->dbenv->memp_stat(env->dbenv, &mp, NULL, 0) != 0)
			return;
		st->cache_hit = mp->st_cache_hit;
		st->cache_miss = mp->st_cache_miss;
		st->pages_in = mp->st_page_in;
		st->pages_out = mp->st_page_out;
		st->bytes_read = mp->st_page_in *
		    (uint64_t)env->cfg.pagesize_kb * 1024;
		st->bytes_written = mp->st_page_out *
		    (uint64_t)env->cfg.pagesize_kb * 1024;
		st->have = 1;
		free(mp);
	}
}

/*
 * xe_stats_delta_print -- the out-of-cache evidence line.
 *
 * hit_rate is computed over the MEASURED INTERVAL only (end minus start), not
 * cumulatively, because the load phase's misses would otherwise swamp the
 * measurement and a warm run would look colder than it is.  read_amp is pages
 * read per committed transaction: for a genuinely out-of-cache workload it is
 * bounded below by the tree depth and well above zero, and an in-cache run
 * shows ~0.  That single number is the strongest available check that the
 * experiment actually had the property it claims.
 */
static void
xe_stats_delta_print(const char *tag, const struct xe_stats *a,
    const struct xe_stats *b, uint64_t txns)
{
	uint64_t hit, miss, pin, pout, br, bw, req;
	double hr;

	if (!a->have || !b->have) {
		printf("# %s cache-stats UNAVAILABLE\n", tag);
		return;
	}
	hit  = b->cache_hit  - a->cache_hit;
	miss = b->cache_miss - a->cache_miss;
	pin  = b->pages_in   - a->pages_in;
	pout = b->pages_out  - a->pages_out;
	br   = b->bytes_read - a->bytes_read;
	bw   = b->bytes_written - a->bytes_written;
	req  = hit + miss;
	hr   = req ? 100.0 * (double)hit / (double)req : 0.0;

	printf("IOSTAT %s cache_hit=%llu cache_miss=%llu hit_rate_pct=%.3f "
	    "pages_in=%llu pages_out=%llu bytes_read=%llu bytes_written=%llu "
	    "read_amp_pages_per_txn=%.3f\n",
	    tag, (unsigned long long)hit, (unsigned long long)miss, hr,
	    (unsigned long long)pin, (unsigned long long)pout,
	    (unsigned long long)br, (unsigned long long)bw,
	    txns ? (double)pin / (double)txns : 0.0);
}

/*
 * xe_data_bytes -- on-disk size of the DATA files only.
 *
 * NOT the size of the environment home.  A `du -s` of the home directory also
 * counts things that are NOT the working set and would inflate the reported
 * working-set:RAM ratio badly:
 *
 *   libdb  __db.001..__db.0NN   the shared regions -- __db.001 alone is the
 *                               mpool region, i.e. a file the same size as the
 *                               CACHE (1.9 GB of a 2 GB cache in smoke, and it
 *                               would be ~139 GiB in the real run)
 *          log.NNNNNNNNNN       write-ahead log segments (1 GB each here)
 *   WT     WiredTigerLog.*      ditto, and WiredTigerPreplog.*
 *
 * Counting the mpool region as "data" would mean a 139 GiB cache reported
 * itself as 139 GiB of working set -- the achieved ratio would be inflated by
 * the very quantity it is supposed to be measured against.  So this sums the
 * named table files explicitly, which is the actual live data.
 *
 * Measured, not assumed: at S=2/pad=200 the home was 3.75 GB while the data was
 * 16.6 MB.  Reporting the former as the working set would have overstated it by
 * 226x and produced a completely fabricated "10x RAM" claim.
 */
static uint64_t
xe_data_bytes(const char *home, const char *const *names, int n,
    const char *suffix)
{
	char path[1024];
	struct stat sb;
	uint64_t total = 0;
	int i;

	for (i = 0; i < n; i++) {
		(void)snprintf(path, sizeof(path), "%s/%s%s", home, names[i],
		    suffix ? suffix : "");
		if (stat(path, &sb) == 0)
			total += (uint64_t)sb.st_blocks * 512;
	}
	return total;
}

/* On-disk size of the whole env home, for context (NOT the working set). */
static uint64_t
xe_dir_bytes(const char *path)
{
	char cmd[512];
	FILE *f;
	unsigned long long kb = 0;

	(void)snprintf(cmd, sizeof(cmd), "du -sk --apparent-size %s 2>/dev/null",
	    path);
	if ((f = popen(cmd, "r")) == NULL)
		return 0;
	if (fscanf(f, "%llu", &kb) != 1) kb = 0;
	(void)pclose(f);
	return (uint64_t)kb * 1024;
}

/*
 * xe_optread_report -- did RFC 0007's optimistic read path actually engage?
 *
 * set_thread_count() is necessary but not sufficient: the path also refuses a
 * dirty pool and bails on various frame states.  So we read the counters and
 * report what happened, rather than asserting engagement because we made the
 * call.  "pages=0" means it never read a page optimistically and any claim
 * about the optimistic path in this run would be vacuous.
 */
static void
xe_optread_report(const char *tag, u_int32_t t0, u_int32_t p0,
    u_int32_t v0, u_int32_t b0)
{
#ifndef XE_HAVE_WIREDTIGER_ONLY
	if (&__bam_opt_tries == NULL) {
		printf("# %s optread counters UNAVAILABLE in this libdb\n", tag);
		return;
	}
	printf("OPTREAD %s tries=%u pages=%u invalid=%u bailouts=%u engaged=%s\n",
	    tag, __bam_opt_tries - t0, __bam_opt_pages - p0,
	    __bam_opt_invalid - v0, __bam_opt_bailouts - b0,
	    (__bam_opt_pages - p0) > 0 ? "yes" : "NO");
#else
	printf("# %s optread not applicable\n", tag);
#endif
}

#endif /* XE_ENGINE_H */
