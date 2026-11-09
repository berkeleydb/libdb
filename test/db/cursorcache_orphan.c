/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * cursorcache_orphan.c --
 *	Regression test for the transient-cursor-cache ORPHAN defect (RFC 0021,
 *	review #238 BLOCKER).
 *
 * Before the fix, a close of ANY handle bumped a process-global epoch that the
 * cache treated as a validity stamp, so the next transient get/put NULLed every
 * cache slot -- including slots whose handle was still open.  That dropped a
 * live handle's cached cursor from its slot while the cursor stayed DBC_ACTIVE
 * on the handle's active queue forever.  Every later insert/delete then walked
 * the orphan (__db_walk_cursors), unbounded: inserts got hundreds of times
 * slower and RSS grew linearly with the number of unrelated handle closes.
 *
 * The test: one long-lived handle H.  Loop CYCLES times: DB->get on H (caches a
 * transient cursor), then open + close an unrelated temp handle T (bumps the
 * epoch).  Then time a batch of inserts on H and measure RSS growth across the
 * loop.  With the fix a close of T leaves H's cached cursor intact (reconcile
 * keeps a live handle's slot), so inserts stay flat and RSS does not grow.
 *
 * CI-safe bounds (small CYCLES; a FAIL threshold well clear of noise but far
 * below the orphaned behaviour, which was >100x slower and tens of MB):
 *	insert-time ratio phase1/phase0 <= RATIO_MAX
 *	RSS growth across the loop        <= RSS_MAX_KB
 */

#include <sys/types.h>
#include <sys/resource.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "db.h"

#define	CYCLES_DEFAULT	5000
#define	NKEYS		2000
#define	BATCH		100000
#define	RATIO_MAX	2.0		/* FAIL if inserts >2x slower. */
#define	RSS_MAX_KB	2048		/* FAIL if RSS grows >2 MB. */
/*
 * Each phase is timed REPS times and the fastest run is compared.  The
 * regression this catches is a persistent slowdown (every insert walks an
 * active queue full of orphans), so it survives the minimum; a one-off stall
 * on a shared runner does not.  One sample per phase gave 1.00-1.13 on 11 CI
 * runs and 2.25 on the twelfth.
 */
#define	REPS		5

/*
 * The RSS bound is meaningful only on Linux with a non-sanitized build:
 *  - under AddressSanitizer the shadow memory + redzones balloon RSS with the
 *    5000 temp-handle cycles regardless of any orphan;
 *  - on macOS/BSD getrusage(2) reports ru_maxrss in BYTES, not kilobytes as on
 *    Linux, so the number is not comparable to a KB bound.
 * In both cases the insert-time ratio is the primary orphan signal (the orphan
 * made inserts >20x slower), so we keep the ratio check and skip RSS there.
 */
#if defined(__SANITIZE_ADDRESS__) || defined(__APPLE__) ||		\
    defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__)
#define	CC_ORPHAN_NO_RSS	1
#elif defined(__has_feature)
#if __has_feature(address_sanitizer)
#define	CC_ORPHAN_NO_RSS	1
#endif
#endif
#ifndef CC_ORPHAN_NO_RSS
#define	CC_ORPHAN_NO_RSS	0
#endif

static double
now_s(void)
{
	struct timespec t;

	(void)clock_gettime(CLOCK_MONOTONIC, &t);
	return (t.tv_sec + t.tv_nsec / 1e9);
}

static long
rss_kb(void)
{
	struct rusage r;

	(void)getrusage(RUSAGE_SELF, &r);
	return (r.ru_maxrss);
}

static int
mkkey(int k, char *b, size_t n)
{
	return ((int)snprintf(b, n, "k%07d", k));
}

static double
timed_inserts(DB_ENV *env, DB *db, int phase)
{
	DB_TXN *t;
	DBT k, d;
	char kb[16], vb[16];
	int i;
	double t0;

	if (env->txn_begin(env, NULL, &t, 0) != 0)
		return (-1.0);
	t0 = now_s();
	for (i = 0; i < BATCH; i++) {
		(void)mkkey(i % NKEYS, kb, sizeof(kb));
		(void)snprintf(vb, sizeof(vb), "%d.%d", phase, i);
		memset(&k, 0, sizeof(k));
		memset(&d, 0, sizeof(d));
		k.data = kb;
		k.size = (u_int32_t)strlen(kb) + 1;
		d.data = vb;
		d.size = (u_int32_t)strlen(vb) + 1;
		if (db->put(db, t, &k, &d, 0) != 0) {
			(void)t->abort(t);
			return (-1.0);
		}
	}
	t0 = now_s() - t0;
	(void)t->commit(t, 0);
	return (t0);
}

static double
min_inserts(DB_ENV *env, DB *db, int phase)
{
	double best, t;
	int r;

	best = -1.0;
	for (r = 0; r < REPS; r++) {
		if ((t = timed_inserts(env, db, phase)) < 0.0)
			return (-1.0);
		if (best < 0.0 || t < best)
			best = t;
	}
	return (best);
}

int
main(int argc, char *argv[])
{
	DB_ENV *env;
	DB *db, *tmp;
	DBT k, d;
	char kb[16];
	const char *dir;
	int i, cycles, ret;
	double p0, p1, ratio;
	long r0, r1, grow;

	dir = argc > 1 ? argv[1] : "CURSORCACHE_ORPHAN_DIR";
	cycles = argc > 2 ? atoi(argv[2]) : CYCLES_DEFAULT;

	if ((ret = db_env_create(&env, 0)) != 0)
		goto err_env;
	(void)env->set_cachesize(env, 0, 64 << 20, 1);
	if ((ret = env->open(env, dir, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_MPOOL | DB_INIT_LOG | DB_INIT_TXN | DB_THREAD |
	    DB_PRIVATE, 0644)) != 0)
		goto err_env;

	if ((ret = db_create(&db, env, 0)) != 0)
		goto err;
	if ((ret = db->open(db, NULL, "h.db", NULL, DB_BTREE,
	    DB_CREATE | DB_THREAD | DB_AUTO_COMMIT, 0644)) != 0)
		goto err;

	/* A temp handle to create its file once. */
	if ((ret = db_create(&tmp, env, 0)) != 0)
		goto err;
	if ((ret = tmp->open(tmp, NULL, "t.db", NULL, DB_BTREE,
	    DB_CREATE | DB_THREAD | DB_AUTO_COMMIT, 0644)) != 0)
		goto err;
	(void)tmp->close(tmp, 0);

	for (i = 0; i < NKEYS; i++) {
		(void)mkkey(i, kb, sizeof(kb));
		memset(&k, 0, sizeof(k));
		memset(&d, 0, sizeof(d));
		k.data = kb;
		k.size = (u_int32_t)strlen(kb) + 1;
		d.data = (void *)"s";
		d.size = 2;
		if ((ret = db->put(db, NULL, &k, &d, 0)) != 0)
			goto err;
	}

	p0 = min_inserts(env, db, 0);
	r0 = rss_kb();

	for (i = 0; i < cycles; i++) {
		(void)mkkey(i % NKEYS, kb, sizeof(kb));
		memset(&k, 0, sizeof(k));
		memset(&d, 0, sizeof(d));
		k.data = kb;
		k.size = (u_int32_t)strlen(kb) + 1;
		d.flags = DB_DBT_MALLOC;
		if (db->get(db, NULL, &k, &d, 0) == 0)
			free(d.data);		/* caches a cursor on H */

		if ((ret = db_create(&tmp, env, 0)) != 0)
			goto err;
		if ((ret = tmp->open(tmp, NULL, "t.db", NULL, DB_BTREE,
		    DB_THREAD, 0644)) != 0)
			goto err;
		if ((ret = tmp->close(tmp, 0)) != 0)	/* epoch++ */
			goto err;
	}

	p1 = min_inserts(env, db, 1);
	r1 = rss_kb();

	(void)db->close(db, 0);
	(void)env->close(env, 0);

	if (p0 <= 0.0 || p1 < 0.0) {
		printf("cursorcache_orphan: FAIL insert timing error\n");
		return (1);
	}
	ratio = p1 / p0;
	grow = r1 - r0;
	printf("cursorcache_orphan: cycles=%d insert0=%.3fs insert1=%.3fs "
	    "ratio=%.2f rss +%ld KB\n", cycles, p0, p1, ratio, grow);

	if (ratio > RATIO_MAX) {
		printf("cursorcache_orphan: FAIL insert ratio %.2f > %.2f "
		    "(orphaned cursors on the active queue)\n",
		    ratio, RATIO_MAX);
		return (1);
	}
	if (grow > RSS_MAX_KB && !CC_ORPHAN_NO_RSS) {
		printf("cursorcache_orphan: FAIL rss grew %ld KB > %d KB "
		    "(unbounded orphan accumulation)\n", grow, RSS_MAX_KB);
		return (1);
	}
	printf("PASS cursorcache_orphan\n");
	return (0);

err:	printf("cursorcache_orphan: setup error: %s\n", db_strerror(ret));
	(void)env->close(env, 0);
	return (2);
err_env:
	printf("cursorcache_orphan: env setup error: %s\n", db_strerror(ret));
	return (2);
}
