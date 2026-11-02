/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * cursorcache_adjust.c --
 *	Correctness test for the per-thread transient-cursor cache (RFC 0021).
 *
 * The cache lets DB->get/DB->put reuse a transient cursor from per-thread
 * storage without the cursor-queue mutex.  Two invariants make that safe:
 *
 *   (1) RESET: a reused cached cursor must be returned to a pristine state
 *       (no stale page/index) before the next operation uses it, or it reads
 *       or writes the wrong record.
 *   (2) VISIBILITY: a cached cursor stays on the handle's ACTIVE queue, so
 *       while it is positioned on a page another thread is changing,
 *       __db_walk_cursors still finds and adjusts it.
 *
 * This test has two parts.
 *
 * PART A (deterministic, single-threaded) exercises RESET: it does a long run
 * of transient gets and puts in an order that, if a reused cursor kept the
 * previous operation's position, would return a neighbouring record.  Every
 * value is verified against what was written.  The -DDBC_CACHE_SABOTAGE build
 * skips the reuse reset, so Part A returns a wrong value there.
 *
 * PART B (concurrent) exercises VISIBILITY and the cache under the adjustment
 * storm: many threads hammer one shared DB_THREAD handle with transient
 * puts/gets on crowded leaves while readers sweep and verify.  It guards against
 * a reset/visibility defect that only shows under races and against any crash
 * or corruption in __db_walk_cursors when cached cursors are present.
 *
 * No lock subsystem: cursor adjustment is a property of in-memory cursor
 * positions, and the page latch is the only serialization we want so that the
 * only thing that can corrupt a result is a cache defect -- the code under test.
 */

#include <sys/types.h>

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "db.h"

#define	NWRITERS	8
#define	NREADERS	4
#define	NKEYS		400		/* All threads share this range. */
#define	ITERS		4000

static DB *db;
static DB_ENV *env;
static volatile int stop;
static volatile int failed;

/* The value written for key k at "version" v is "<k>.<v>". */
static void
mkkey(int k, char *buf, size_t len)
{
	(void)snprintf(buf, len, "k%06d", k);
}
static void
mkval(int k, int v, char *buf, size_t len)
{
	(void)snprintf(buf, len, "%06d.%06d", k, v);
}

static int
fail(const char *what, int ret)
{
	fprintf(stderr, "FAIL cursorcache_adjust: %s: %s\n",
	    what, ret == 0 ? "(logic)" : db_strerror(ret));
	failed = 1;
	return (1);
}

/* A single transient put of (k -> value-for-k). */
static int
put_key(int k, int v)
{
	DBT key, data;
	char kbuf[32], vbuf[32];

	mkkey(k, kbuf, sizeof(kbuf));
	mkval(k, v, vbuf, sizeof(vbuf));
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = kbuf;
	key.size = (u_int32_t)strlen(kbuf) + 1;
	data.data = vbuf;
	data.size = (u_int32_t)strlen(vbuf) + 1;
	return (db->put(db, NULL, &key, &data, 0));
}

/* A single transient get; returns value in got[], or DB_NOTFOUND. */
static int
get_key(int k, char *got, size_t len)
{
	DBT key, data;
	char kbuf[32];

	mkkey(k, kbuf, sizeof(kbuf));
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = kbuf;
	key.size = (u_int32_t)strlen(kbuf) + 1;
	data.data = got;
	data.ulen = (u_int32_t)len;
	data.flags = DB_DBT_USERMEM;
	return (db->get(db, NULL, &key, &data, 0));
}

/*
 * PART A: a deterministic reset probe.  Write every key once, then repeatedly
 * get key A, get key B, where A and B are far apart; a cursor reused with a
 * stale position after the get of A will, on the get of B, start from A's page
 * and either return A's value for key B or fail to find B.  We verify each get.
 */
static int
part_a(void)
{
	char got[32], want[32];
	int i, k;

	for (k = 0; k < NKEYS; k++)
		if (put_key(k, 0) != 0)
			return (fail("A put", 1));

	for (i = 0; i < NKEYS * 8; i++) {
		/* Alternate between the low and high end of the keyspace. */
		k = (i & 1) ? (NKEYS - 1 - (i % NKEYS) / 2) : ((i % NKEYS) / 2);
		if (k < 0 || k >= NKEYS)
			k = i % NKEYS;
		if (get_key(k, got, sizeof(got)) != 0)
			return (fail("A get", 1));
		mkval(k, 0, want, sizeof(want));
		if (strcmp(got, want) != 0) {
			fprintf(stderr, "FAIL cursorcache_adjust: part A key "
			    "%d got '%s' want '%s'\n", k, got, want);
			return (1);
		}
	}
	return (0);
}

static void *
writer(void *arg)
{
	int base, i, k, v;
	char got[32], want[32];

	base = (int)(intptr_t)arg;

	for (i = 0; i < ITERS && !stop && !failed; i++) {
		/* Stride through the shared range so writers collide. */
		k = (base + i) % NKEYS;
		v = i + 1;
		if (put_key(k, v) != 0) {
			(void)fail("writer put", 1);
			return (NULL);
		}
		/* Read our own write straight back; it must round-trip. */
		if (get_key(k, got, sizeof(got)) != 0) {
			(void)fail("writer get", 1);
			return (NULL);
		}
		mkval(k, v, want, sizeof(want));
		/*
		 * Another writer may have overwritten k between our put and
		 * get, so accept any well-formed "<k>.<v'>" for THIS key; a
		 * mis-positioned cursor instead returns a DIFFERENT key's
		 * value, which fails the prefix check.
		 */
		if (strncmp(got, want, 7) != 0) {	/* "kkkkkk." prefix */
			fprintf(stderr, "FAIL cursorcache_adjust: writer key "
			    "%d got '%s' (wrong key prefix, want '%.7s')\n",
			    k, got, want);
			failed = 1;
			return (NULL);
		}
	}
	return (NULL);
}

static void *
reader(void *arg)
{
	int k, ret;
	char got[32], pfx[8];

	(void)arg;
	while (!stop && !failed) {
		for (k = 0; k < NKEYS && !failed; k++) {
			ret = get_key(k, got, sizeof(got));
			if (ret == DB_NOTFOUND)
				continue;
			if (ret != 0) {
				(void)fail("reader get", ret);
				return (NULL);
			}
			/* The value's key prefix MUST be this key. */
			(void)snprintf(pfx, sizeof(pfx), "%06d.", k);
			if (strncmp(got, pfx, 7) != 0) {
				fprintf(stderr, "FAIL cursorcache_adjust: "
				    "reader key %d got '%s' (wrong key)\n",
				    k, got);
				failed = 1;
				return (NULL);
			}
		}
	}
	return (NULL);
}

int
main(int argc, char *argv[])
{
	pthread_t wt[NWRITERS], rt[NREADERS];
	int i, k, ret;
	char got[32], pfx[8];
	const char *dir;

	dir = argc > 1 ? argv[1] : "CURSORCACHE_TESTDIR";

	if ((ret = db_env_create(&env, 0)) != 0)
		return (fail("db_env_create", ret));
	if ((ret = env->open(env, dir,
	    DB_CREATE | DB_INIT_MPOOL | DB_THREAD, 0644)) != 0)
		return (fail("env->open", ret));

	if ((ret = db_create(&db, env, 0)) != 0)
		return (fail("db_create", ret));
	if ((ret = db->set_pagesize(db, 512)) != 0)	/* crowd the leaves */
		return (fail("set_pagesize", ret));
	if ((ret = db->open(db, NULL, "cursorcache.db", NULL, DB_BTREE,
	    DB_CREATE | DB_THREAD, 0644)) != 0)
		return (fail("db->open", ret));

	/* Part A: deterministic reset probe. */
	if (part_a() != 0) {
		(void)db->close(db, 0);
		(void)env->close(env, 0);
		return (1);
	}

	/* Part B: concurrent adjustment storm. */
	for (i = 0; i < NWRITERS; i++)
		if ((ret = pthread_create(&wt[i], NULL,
		    writer, (void *)(intptr_t)(i * 37))) != 0)
			return (fail("pthread_create writer", ret));
	for (i = 0; i < NREADERS; i++)
		if ((ret = pthread_create(&rt[i], NULL, reader, NULL)) != 0)
			return (fail("pthread_create reader", ret));

	for (i = 0; i < NWRITERS; i++)
		(void)pthread_join(wt[i], NULL);
	stop = 1;
	for (i = 0; i < NREADERS; i++)
		(void)pthread_join(rt[i], NULL);

	if (failed) {
		(void)db->close(db, 0);
		(void)env->close(env, 0);
		return (1);
	}

	/* Final serial check: every key's value names that key. */
	for (k = 0; k < NKEYS; k++) {
		ret = get_key(k, got, sizeof(got));
		if (ret != 0)
			return (fail("final get", ret));
		(void)snprintf(pfx, sizeof(pfx), "%06d.", k);
		if (strncmp(got, pfx, 7) != 0) {
			fprintf(stderr, "FAIL cursorcache_adjust: final key %d "
			    "got '%s' (wrong key)\n", k, got);
			return (1);
		}
	}

	if ((ret = db->close(db, 0)) != 0)
		return (fail("db->close", ret));
	if ((ret = env->close(env, 0)) != 0)
		return (fail("env->close", ret));

	printf("PASS cursorcache_adjust: part A reset probe + %d keys under %d "
	    "writers + %d readers round-tripped on a shared cached handle\n",
	    NKEYS, NWRITERS, NREADERS);
	return (0);
}
