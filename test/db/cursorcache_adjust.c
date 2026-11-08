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
 * storage without the cursor-queue mutex.  The invariants that make it safe:
 *
 *   (S4a) STALE-HANDLE GUARD: when a DB handle closes, its cached cursors are
 *         destroyed (they ride the handle's active queue, which __db_refresh
 *         walks).  A process-global epoch bump makes every thread's stale cache
 *         slot a guaranteed miss, so a REOPENED handle that reuses the freed
 *         memory never touches a dangling cursor pointer.
 *   (S5)  VISIBILITY: a cached cursor stays on the handle's active queue, so
 *         while positioned on a page another thread is changing,
 *         __db_walk_cursors still finds and adjusts it.
 *
 * PART A (deterministic) exercises S4a: open a handle, do transient gets/puts
 * (filling this thread's cache with a cursor for that handle), close the
 * handle, then open a fresh handle and operate again -- many times.  Without
 * the epoch guard the reopened handle picks up the destroyed cursor from the
 * stale cache slot and the next operation touches freed memory.  Under
 * --enable-diagnostic that is a crash/abort; the -DDBC_CACHE_SABOTAGE build
 * disables the guard, so Part A faults there and the test's sabotage arm fails.
 *
 * PART B (concurrent) exercises the cache and S5 under an adjustment storm:
 * many threads hammer one shared DB_THREAD handle with transient puts/gets on
 * crowded leaves while readers sweep and verify every value names its own key,
 * guarding against any crash or corruption in __db_walk_cursors with cached
 * cursors present.
 *
 * No lock subsystem: adjustment is a property of in-memory cursor positions and
 * the page latch is the only serialization we want.
 */

#include <sys/types.h>

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "db.h"

#define	NWRITERS	8
#define	NREADERS	4
#define	NKEYS		400
#define	ITERS		4000
#define	REOPENS		400		/* Part A handle close/reopen cycles. */

static DB_ENV *env;
static volatile int stop;
static volatile int failed;

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

static int
put_key(DB *db, int k, int v)
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

static int
get_key(DB *db, int k, char *got, size_t len)
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
 * PART A: open/use/close a handle REOPENS times.  Each cycle caches a transient
 * cursor for the just-opened handle and then closes the handle, which destroys
 * that cursor.  A correct cache invalidates the stale slot via the epoch bump;
 * the sabotage build does not, so a later cycle dereferences freed memory.
 */
static int
part_a(void)
{
	DB *db;
	char got[32], want[32];
	int cyc, ret;

	for (cyc = 0; cyc < REOPENS; cyc++) {
		if ((ret = db_create(&db, env, 0)) != 0)
			return (fail("A db_create", ret));
		if ((ret = db->set_pagesize(db, 512)) != 0)
			return (fail("A set_pagesize", ret));
		if ((ret = db->open(db, NULL, "cursorcache_a.db", NULL,
		    DB_BTREE, DB_CREATE | DB_THREAD, 0644)) != 0)
			return (fail("A db->open", ret));

		/* Populate + read a couple of keys: fills the thread cache. */
		if (put_key(db, 0, cyc) != 0 || put_key(db, 1, cyc) != 0)
			return (fail("A put", 1));
		if (get_key(db, 0, got, sizeof(got)) != 0)
			return (fail("A get0", 1));
		mkval(0, cyc, want, sizeof(want));
		if (strcmp(got, want) != 0) {
			fprintf(stderr, "FAIL cursorcache_adjust: part A cyc "
			    "%d got '%s' want '%s'\n", cyc, got, want);
			return (1);
		}
		if ((ret = db->close(db, 0)) != 0)
			return (fail("A db->close", ret));
	}
	return (0);
}

static void *
writer(void *arg)
{
	DB *db = (DB *)arg;
	int i, k, v;
	char got[32], want[32];

	for (i = 0; i < ITERS && !stop && !failed; i++) {
		k = (i * 37 + (int)(intptr_t)pthread_self()) % NKEYS;
		if (k < 0)
			k = -k % NKEYS;
		v = i + 1;
		if (put_key(db, k, v) != 0) {
			(void)fail("writer put", 1);
			return (NULL);
		}
		if (get_key(db, k, got, sizeof(got)) != 0) {
			(void)fail("writer get", 1);
			return (NULL);
		}
		mkval(k, v, want, sizeof(want));
		/* Accept any version of THIS key; reject a different key. */
		if (strncmp(got, want, 7) != 0) {
			fprintf(stderr, "FAIL cursorcache_adjust: writer key "
			    "%d got '%s' (wrong key, want '%.7s')\n",
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
	DB *db = (DB *)arg;
	int k, ret;
	char got[32], pfx[8];

	while (!stop && !failed) {
		for (k = 0; k < NKEYS && !failed; k++) {
			ret = get_key(db, k, got, sizeof(got));
			if (ret == DB_NOTFOUND)
				continue;
			if (ret != 0) {
				(void)fail("reader get", ret);
				return (NULL);
			}
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

static int
part_b(void)
{
	pthread_t wt[NWRITERS], rt[NREADERS];
	DB *db;
	char got[32], pfx[8];
	int i, k, ret;

	if ((ret = db_create(&db, env, 0)) != 0)
		return (fail("B db_create", ret));
	if ((ret = db->set_pagesize(db, 512)) != 0)
		return (fail("B set_pagesize", ret));
	if ((ret = db->open(db, NULL, "cursorcache_b.db", NULL, DB_BTREE,
	    DB_CREATE | DB_THREAD, 0644)) != 0)
		return (fail("B db->open", ret));

	for (i = 0; i < NWRITERS; i++)
		if ((ret = pthread_create(&wt[i], NULL, writer, db)) != 0)
			return (fail("pthread_create writer", ret));
	for (i = 0; i < NREADERS; i++)
		if ((ret = pthread_create(&rt[i], NULL, reader, db)) != 0)
			return (fail("pthread_create reader", ret));

	for (i = 0; i < NWRITERS; i++)
		(void)pthread_join(wt[i], NULL);
	stop = 1;
	for (i = 0; i < NREADERS; i++)
		(void)pthread_join(rt[i], NULL);

	if (failed) {
		(void)db->close(db, 0);
		return (1);
	}

	for (k = 0; k < NKEYS; k++) {
		ret = get_key(db, k, got, sizeof(got));
		if (ret != 0)
			return (fail("B final get", ret));
		(void)snprintf(pfx, sizeof(pfx), "%06d.", k);
		if (strncmp(got, pfx, 7) != 0) {
			fprintf(stderr, "FAIL cursorcache_adjust: B final key "
			    "%d got '%s' (wrong key)\n", k, got);
			return (1);
		}
	}
	if ((ret = db->close(db, 0)) != 0)
		return (fail("B db->close", ret));
	return (0);
}

int
main(int argc, char *argv[])
{
	int ret;
	const char *dir;

	dir = argc > 1 ? argv[1] : "CURSORCACHE_TESTDIR";

	if ((ret = db_env_create(&env, 0)) != 0)
		return (fail("db_env_create", ret));
	if ((ret = env->open(env, dir,
	    DB_CREATE | DB_INIT_LOCK | DB_INIT_MPOOL | DB_THREAD, 0644)) != 0)
		return (fail("env->open", ret));

	if (part_a() != 0) {
		(void)env->close(env, 0);
		return (1);
	}
	if (part_b() != 0) {
		(void)env->close(env, 0);
		return (1);
	}

	if ((ret = env->close(env, 0)) != 0)
		return (fail("env->close", ret));

	printf("PASS cursorcache_adjust: part A %d handle reopens + part B %d "
	    "keys under %d writers + %d readers on a cached-cursor handle\n",
	    REOPENS, NKEYS, NWRITERS, NREADERS);
	return (0);
}
