/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*-
 * Regression test for R3 (XREL-2026-10.md) / review item O-MP1 -- the COOL-first
 * CLOCK eviction sweep examined 80-520 hash buckets per allocation, vs 1-2 in
 * 5.3.28.
 *
 * Stage 0.5 (RFC 0002 §11, dea815bb4) made __memp_alloc only cool the HOT band
 * once a FULL sweep of all htab_buckets finds no COOL victim.  Under a bulk
 * insert of fresh dirty pages every unreferenced buffer is HOT, so that full
 * sweep ran on every allocation -- an O(htab_buckets) tax measured as
 * st_alloc_buckets / st_alloc (buckets examined per allocation; the same ratio
 * db_stat -m reports and test/bench/xrel_evict_probe.c prints).
 *
 * The fix (rfc/0002/R3-FIX.md) caps the pre-aggressive COOL-first window at
 * MPOOL_COOL_SWEEP_LIMIT buckets, so an all-HOT cache escalates promptly instead
 * of re-scanning the whole table.  This test fills a deliberately small cache
 * with fresh dirty btree pages and asserts the buckets-examined-per-allocation
 * ratio stays under a bound that the bounded sweep clears and the unbounded
 * (master) sweep blows past.
 *
 * Deterministic: single-threaded, DB_PRIVATE mpool-only env, no transactions, a
 * fixed record count and cache size, so the bucket ratio is reproducible across
 * runs (the raw numbers in evict.tsv are identical across reps).
 */
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <db.h>

/*
 * 4 MB cache: the worst measured cell (queue 0.05x, btree 0.73x), where master
 * sweeps ~460-520 buckets per allocation and 5.3.28 ~2.  1M inserts forces heavy
 * eviction; the key/value sizes match xrel_evict_probe.c so the two agree.
 */
#define	CACHE_KB	4096
#define	NRECS		1000000

/*
 * Pass/fail boundary for buckets examined per allocation.  5.3.28 sits at ~2;
 * master at 460-520.  The bounded sweep is MPOOL_COOL_SWEEP_LIMIT (16) + the
 * aggressive tail (~2 buckets) plus the two-bucket comparison, so it lands well
 * under 40.  40 is chosen to separate BOUNDED from the unbounded regression with
 * a wide margin, not to track the exact window size -- a regression reverts to
 * the full sweep and blows past it by an order of magnitude.  Override for
 * experimentation with LIBDB_R3_MAX_BUCKETS.
 */
#define	MAX_BUCKETS_PER_ALLOC	40.0

static int
check(int ret, const char *what)
{
	if (ret != 0) {
		fprintf(stderr, "FAIL: %s: %s\n", what, db_strerror(ret));
		exit(1);
	}
	return (ret);
}

int
main()
{
	DB_ENV *env;
	DB *db;
	DBT key, data;
	DB_MPOOL_STAT *gsp;
	char kbuf[16], vbuf[20];
	double per_alloc, limit;
	const char *envp;
	int i, ret;

	limit = MAX_BUCKETS_PER_ALLOC;
	if ((envp = getenv("LIBDB_R3_MAX_BUCKETS")) != NULL)
		limit = atof(envp);

	(void)system("rm -rf R3_EVICT_TESTDIR && mkdir R3_EVICT_TESTDIR");

	check(db_env_create(&env, 0), "db_env_create");
	/* A small cache: ck/1GB gbytes, remainder bytes, 1 region. */
	check(env->set_cachesize(env, CACHE_KB / (1024 * 1024),
	    (CACHE_KB % (1024 * 1024)) * 1024, 1), "set_cachesize");
	check(env->open(env, "R3_EVICT_TESTDIR",
	    DB_CREATE | DB_INIT_MPOOL | DB_PRIVATE, 0), "env->open");

	check(db_create(&db, env, 0), "db_create");
	check(db->open(db, NULL, "r3.db", NULL, DB_BTREE, DB_CREATE, 0644),
	    "db->open");

	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	memset(vbuf, 'v', sizeof(vbuf));
	data.data = vbuf;
	data.size = sizeof(vbuf);
	for (i = 0; i < NRECS; i++) {
		(void)snprintf(kbuf, sizeof(kbuf), "%09d", i);
		key.data = kbuf;
		key.size = 10;
		if ((ret = db->put(db, NULL, &key, &data, 0)) != 0)
			check(ret, "db->put");
	}

	check(env->memp_stat(env, &gsp, NULL, 0), "memp_stat");

	if (gsp->st_alloc == 0) {
		fprintf(stderr,
		    "FAIL: no allocations happened -- cache not under pressure "
		    "(st_alloc=0); test is not exercising eviction\n");
		return (1);
	}
	per_alloc = (double)gsp->st_alloc_buckets / (double)gsp->st_alloc;

	printf("R3 evict: st_alloc=%ju st_alloc_buckets=%ju "
	    "st_alloc_max_buckets=%ju  buckets/alloc=%.1f  (limit %.1f)\n",
	    (uintmax_t)gsp->st_alloc, (uintmax_t)gsp->st_alloc_buckets,
	    (uintmax_t)gsp->st_alloc_max_buckets, per_alloc, limit);

	(void)db->close(db, 0);
	(void)env->close(env, 0);
	(void)system("rm -rf R3_EVICT_TESTDIR");

	if (per_alloc > limit) {
		fprintf(stderr,
		    "FAIL: COOL-first sweep examined %.1f buckets per "
		    "allocation (limit %.1f) -- the bound regressed (R3)\n",
		    per_alloc, limit);
		return (1);
	}
	printf("run_evict_bound: PASS (%.1f buckets/alloc <= %.1f)\n",
	    per_alloc, limit);
	return (0);
}
