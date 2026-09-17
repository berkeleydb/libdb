/*-
 * opt_fires: the TEETH TEST for RFC 0007 phase 1 (optimistic read validation).
 *
 * Two questions, and a "no" to either is a failure:
 *
 *   1. Does the optimistic descent actually RUN?  A neutral or positive
 *      performance result from a code path that never executes is not a
 *      measurement of that path.  Gate: opt_pages > 0 and a nonzero fraction of
 *      reads descended optimistically.
 *
 *   2. Does validation actually FIRE?  If the generation never moves under a
 *      reader, the optimistic path is vacuously "correct" because it never had
 *      to retry, and the mechanism that makes it safe has never been exercised.
 *      Gate: opt_invalid > 0.  To make that happen this test runs writers that
 *      insert and delete enough to split and merge interior pages while the
 *      readers descend, plus an eviction-pressure knob (a deliberately small
 *      cache) so frames get reused under readers as well.
 *
 * AND, the part that makes it a correctness test rather than a counter test:
 * every read is CHECKED against the value the writer last committed for that
 * key.  A descent that validates when it should not would land on the wrong
 * leaf and return the wrong data (or DB_NOTFOUND for a key that exists), and
 * that is reported as a mismatch, not as a retry.
 *
 * The verdict line is asserted by the caller; "rc=0" proves nothing here.
 *
 * usage: opt_fires <nkeys> <readers> <writers> <secs>
 * env:   OPT_HOME (existing dir; reused by both arms -- see the DB_PRIVATE
 *          layout warning atop run_bench.sh)
 *        OPT_CACHE_MB (default 64; set small to force eviction)
 *        DB_NO_OPTREAD (set: optimistic descent off -- the control arm)
 */
#include <sys/types.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "db.h"

#define	VALSZ	180

extern u_int32_t __bam_opt_tries;
extern u_int32_t __bam_opt_pages;
extern u_int32_t __bam_opt_invalid;
extern u_int32_t __bam_opt_bailouts;

static DB_ENV *env;
static DB *db;
static volatile int stop, go;
static unsigned g_nkeys;
static unsigned long long g_reads, g_hits, g_mismatch, g_readerr;

/*
 * Value for a key is a pure function of the key, so any reader can check any
 * value it gets back without coordinating with the writers.  A wrong-leaf
 * descent returns a DIFFERENT key's value, which this catches.
 */
static void
fillval(unsigned k, char *v)
{
	unsigned i;

	for (i = 0; i < VALSZ; i++)
		v[i] = (char)('a' + ((k + i) % 26));
}

static void *
reader(void *a)
{
	DBT key, data;
	char got[VALSZ + 8], want[VALSZ];
	unsigned seed = (unsigned)(size_t)a * 2654435761u + 7;
	unsigned long long n, hit, bad, err;
	unsigned kb;
	int ret;

	n = hit = bad = err = 0;
	while (!go) { }
	while (!stop) {
		kb = (unsigned)(rand_r(&seed) % g_nkeys);
		memset(&key, 0, sizeof(key));
		key.data = &kb;
		key.size = sizeof(kb);
		memset(&data, 0, sizeof(data));
		data.data = got;
		data.ulen = sizeof(got);
		data.flags = DB_DBT_USERMEM;
		ret = db->get(db, NULL, &key, &data, 0);
		n++;
		if (ret == 0) {
			hit++;
			fillval(kb, want);
			if (data.size != VALSZ ||
			    memcmp(got, want, VALSZ) != 0)
				bad++;
		} else if (ret == DB_NOTFOUND)
			/*
			 * Keys in [0, nkeys) are loaded before the run and no
			 * writer ever touches them, so DB_NOTFOUND is a WRONG
			 * ANSWER, not a race: it means the descent landed on a
			 * leaf that does not cover this key.  Counted as a
			 * mismatch.  (Observed exactly once in the sabotaged
			 * no-gen-bump build -- which is the point.)
			 */
			bad++;
		else if (ret != DB_LOCK_DEADLOCK)
			err++;
	}
	(void)__sync_fetch_and_add(&g_reads, n);
	(void)__sync_fetch_and_add(&g_hits, hit);
	(void)__sync_fetch_and_add(&g_mismatch, bad);
	(void)__sync_fetch_and_add(&g_readerr, err);
	return (NULL);
}

/*
 * Writers churn the UPPER keyspace: they insert a dense run of keys and then
 * delete it, repeatedly.  That is what splits and merges interior pages, which
 * is what moves the generation on the pages readers are descending through.
 * They never touch [0, g_nkeys), so a reader's value check stays exact.
 */
static void *
writer(void *a)
{
	DBT key, data;
	char vbuf[VALSZ];
	unsigned seed = (unsigned)(size_t)a * 40503u + 11;
	unsigned base, kb, i;
	int ret;

	while (!go) { }
	while (!stop) {
		base = g_nkeys + (unsigned)(rand_r(&seed) % 64) * 4096;
		for (i = 0; i < 4096 && !stop; i++) {
			kb = base + i;
			memset(&key, 0, sizeof(key));
			key.data = &kb;
			key.size = sizeof(kb);
			fillval(kb, vbuf);
			memset(&data, 0, sizeof(data));
			data.data = vbuf;
			data.size = VALSZ;
			ret = db->put(db, NULL, &key, &data, 0);
			if (ret != 0 && ret != DB_LOCK_DEADLOCK &&
			    ret != DB_KEYEXIST)
				return (NULL);
		}
		for (i = 0; i < 4096 && !stop; i++) {
			kb = base + i;
			memset(&key, 0, sizeof(key));
			key.data = &kb;
			key.size = sizeof(kb);
			ret = db->del(db, NULL, &key, 0);
			if (ret != 0 && ret != DB_NOTFOUND &&
			    ret != DB_LOCK_DEADLOCK)
				return (NULL);
		}
	}
	return (NULL);
}

int
main(argc, argv)
	int argc;
	char **argv;
{
	pthread_t rth[256], wth[64];
	DBT key, data;
	struct timespec sl;
	char vbuf[VALSZ];
	const char *home;
	u_int32_t t0, p0, v0, b0, t1, p1, v1, b1;
	unsigned kb, i, cache_mb;
	int ret, nread, nwrite, secs, optoff;

	if (argc != 5) {
		fprintf(stderr,
		    "usage: %s <nkeys> <readers> <writers> <secs>\n", argv[0]);
		return (1);
	}
	g_nkeys = (unsigned)atoi(argv[1]);
	nread = atoi(argv[2]);
	nwrite = atoi(argv[3]);
	secs = atoi(argv[4]);
	if (g_nkeys < 10000 || nread < 1 || nread > 256 ||
	    nwrite < 0 || nwrite > 64 || secs < 1) {
		fprintf(stderr, "bad args\n");
		return (1);
	}
	if ((home = getenv("OPT_HOME")) == NULL) {
		fprintf(stderr, "set OPT_HOME\n");
		return (1);
	}
	cache_mb = getenv("OPT_CACHE_MB") == NULL ?
	    64 : (unsigned)atoi(getenv("OPT_CACHE_MB"));
	optoff = getenv("DB_NO_OPTREAD") != NULL;

	if ((ret = db_env_create(&env, 0)) != 0)
		return (1);
	env->set_errfile(env, stderr);
	(void)env->set_cachesize(env, 0, cache_mb * 1024 * 1024, 1);
	(void)env->set_flags(env, DB_TXN_NOSYNC, 1);
	(void)env->set_lk_detect(env, DB_LOCK_MINWRITE);
	/*
	 * REQUIRED for the optimistic path.  The per-thread pin list -- which
	 * is what makes a frame ineligible for eviction while a pin-free reader
	 * is on it -- lives in the thread region, and that region is only
	 * allocated when thr_max != 0 (env_failchk.c:__env_thread_init).  With
	 * no thread region __memp_fget records no pins at all today, so
	 * __memp_fget_opt refuses to engage (ip == NULL) and every descent falls
	 * back.  Discovered the hard way: without this line the fast path
	 * bailed 20000/20000 times with "guard".
	 */
	(void)env->set_thread_count(env,
	    (u_int32_t)(nread + nwrite + 16));
	if ((ret = env->open(env, home, DB_CREATE | DB_INIT_MPOOL |
	    DB_INIT_LOCK | DB_INIT_TXN | DB_INIT_LOG | DB_THREAD, 0)) != 0) {
		env->err(env, ret, "env open %s", home);
		return (1);
	}
	if ((ret = db_create(&db, env, 0)) != 0)
		return (1);
	if ((ret = db->open(db, NULL, "opt.db", NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT | DB_THREAD, 0)) != 0) {
		env->err(env, ret, "db open");
		return (1);
	}

	for (i = 0; i < g_nkeys; i++) {
		memset(&key, 0, sizeof(key));
		kb = i;
		key.data = &kb;
		key.size = sizeof(kb);
		fillval(kb, vbuf);
		memset(&data, 0, sizeof(data));
		data.data = vbuf;
		data.size = VALSZ;
		if ((ret = db->put(db, NULL, &key, &data, 0)) != 0) {
			env->err(env, ret, "load");
			return (1);
		}
	}
	/*
	 * Clean the pool before measuring: the optimistic path refuses a dirty
	 * frame, and a read benchmark run against an all-dirty pool silently
	 * disarms the very path under test (this exact mistake disarmed
	 * perf/bhpin-r1's measurement -- see test/bench/pin_fires.c).
	 */
	if ((ret = env->txn_checkpoint(env, 0, 0, DB_FORCE)) != 0) {
		env->err(env, ret, "txn_checkpoint");
		return (1);
	}
	{
		int nwrote = 0;
		(void)env->memp_trickle(env, 100, &nwrote);
	}

	t0 = __bam_opt_tries;
	p0 = __bam_opt_pages;
	v0 = __bam_opt_invalid;
	b0 = __bam_opt_bailouts;

	stop = go = 0;
	for (i = 0; i < (unsigned)nread; i++)
		(void)pthread_create(&rth[i], NULL, reader, (void *)(size_t)i);
	for (i = 0; i < (unsigned)nwrite; i++)
		(void)pthread_create(&wth[i], NULL, writer, (void *)(size_t)i);
	go = 1;
	sl.tv_sec = secs;
	sl.tv_nsec = 0;
	(void)nanosleep(&sl, NULL);
	stop = 1;
	for (i = 0; i < (unsigned)nread; i++)
		(void)pthread_join(rth[i], NULL);
	for (i = 0; i < (unsigned)nwrite; i++)
		(void)pthread_join(wth[i], NULL);

	t1 = __bam_opt_tries;
	p1 = __bam_opt_pages;
	v1 = __bam_opt_invalid;
	b1 = __bam_opt_bailouts;

	(void)db->close(db, 0);
	(void)env->close(env, 0);

	if (g_reads == 0) {
		printf("FAIL opt-fires no reads performed\n");
		return (1);
	}
	/*
	 * Correctness first: a wrong-leaf descent shows up here, and no counter
	 * value excuses it.
	 */
	if (g_mismatch != 0 || g_readerr != 0) {
		printf("FAIL opt-fires reads=%llu mismatch=%llu readerr=%llu\n",
		    g_reads, g_mismatch, g_readerr);
		return (1);
	}

	printf("VERDICT opt-fires optread=%s reads=%llu hits=%llu "
	    "tries=%u pages=%u invalid=%u bailouts=%u "
	    "pages_per_read=%.3f invalid_per_1k_tries=%.3f\n",
	    optoff ? "off" : "on", g_reads, g_hits,
	    t1 - t0, p1 - p0, v1 - v0, b1 - b0,
	    (double)(p1 - p0) / (double)g_reads,
	    (t1 - t0) == 0 ? 0.0 :
	    1000.0 * (double)(v1 - v0) / (double)(t1 - t0));

	if (optoff) {
		/* Control arm: the path must be entirely inert. */
		if (p1 != p0 || t1 != t0) {
			printf("FAIL opt-fires DB_NO_OPTREAD but path ran "
			    "tries=%u pages=%u\n", t1 - t0, p1 - p0);
			return (1);
		}
		printf("VERDICT opt-fires-control inert\n");
		return (0);
	}

	/* Teeth 1: the path ran. */
	if (p1 - p0 == 0) {
		printf("FAIL opt-fires optimistic descent NEVER READ A PAGE "
		    "(tries=%u bailouts=%u) -- any perf result is vacuous\n",
		    t1 - t0, b1 - b0);
		return (1);
	}
	/* Teeth 2: validation fired.  This is what a no-bump build fails. */
	if (v1 - v0 == 0) {
		printf("FAIL opt-fires validation NEVER FIRED in %u tries "
		    "(pages=%u) -- the safety mechanism is untested; a build "
		    "that never bumps BH.gen produces exactly this line\n",
		    t1 - t0, p1 - p0);
		return (1);
	}
	printf("VERDICT opt-fires-teeth ran=%u fired=%u\n", p1 - p0, v1 - v0);
	return (0);
}
