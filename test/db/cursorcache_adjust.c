/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * cursorcache_adjust.c --
 *	Stress the per-thread transient-cursor cache (RFC 0021) against
 *	concurrent cursor adjustment on shared pages, and verify correctness.
 *
 * WHAT IS UNDER TEST
 *
 * DB->get/DB->put allocate a transient cursor and, with the cache, reuse one
 * from per-thread storage without touching the handle's cursor-queue mutex.
 * The load-bearing invariant (rfc/0021 Design S5) is that a cached cursor stays
 * on the handle's ACTIVE queue, so that while it is positioned on a page another
 * thread is changing, __db_walk_cursors still finds and adjusts it.  If a
 * reused cached cursor were positioned while OFF the active queue, a concurrent
 * insert/delete on the same page would not adjust it and it would silently
 * address the wrong record.
 *
 * WHAT THIS DOES
 *
 * One shared DB_THREAD btree handle, small pages so many keys share one leaf.
 * NWRITERS threads each own a disjoint band of keys and, in a tight loop, put a
 * key then immediately get it back and check the value round-trips.  Every op
 * is a transient DB->put / DB->get that exercises the cache, and the puts shift
 * sibling indices on the shared leaf, which is exactly the adjustment event.
 * Concurrently NREADERS threads sweep the whole key range with DB->get and
 * verify each present key carries its expected value.  A missed adjustment
 * corrupts a cursor's index mid-operation, so a get returns the wrong value (or
 * a put stores under the wrong slot), which the round-trip and sweep checks
 * catch.
 *
 * Against the sabotage build (-DDBC_CACHE_SABOTAGE, which positions a reused
 * cached cursor while off the active queue) this reports a value mismatch and
 * exits non-zero.  Against the correct build every value round-trips and it
 * exits zero.
 *
 * DB_INIT_LOCK + transactions are deliberately NOT used: cursor adjustment is a
 * property of in-memory cursor positions.  We retry DB_LOCK_DEADLOCK-free by
 * using the default (no lock subsystem) so the only thing serializing access is
 * the page latch, and the only thing that can corrupt a result is a missed
 * adjustment -- the code under test.
 */

#include <sys/types.h>

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "db.h"

#define	NWRITERS	8
#define	NREADERS	4
#define	KEYS_PER_WRITER	64		/* Small: keep bands on few pages. */
#define	ITERS		3000

static DB *db;
static DB_ENV *env;
static volatile int stop;
static volatile int failed;

/* Expected value for key k is the string "v<k>". */
static void
expected_value(int k, char *buf, size_t len)
{
	(void)snprintf(buf, len, "v%08d", k);
}

static int
fail(const char *what, int ret)
{
	fprintf(stderr, "FAIL cursorcache_adjust: %s: %s\n",
	    what, ret == 0 ? "(logic)" : db_strerror(ret));
	failed = 1;
	return (1);
}

static void *
writer(void *arg)
{
	DBT key, data;
	int base, i, k, ret;
	char kbuf[32], vbuf[32], got[32];

	base = (int)(intptr_t)arg * KEYS_PER_WRITER;

	for (i = 0; i < ITERS && !stop && !failed; i++) {
		k = base + (i % KEYS_PER_WRITER);
		(void)snprintf(kbuf, sizeof(kbuf), "k%08d", k);
		expected_value(k, vbuf, sizeof(vbuf));

		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf;
		key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = vbuf;
		data.size = (u_int32_t)strlen(vbuf) + 1;
		if ((ret = db->put(db, NULL, &key, &data, 0)) != 0) {
			(void)fail("writer put", ret);
			return (NULL);
		}

		/* Immediately read it back: must round-trip. */
		memset(&data, 0, sizeof(data));
		data.data = got;
		data.ulen = sizeof(got);
		data.flags = DB_DBT_USERMEM;
		if ((ret = db->get(db, NULL, &key, &data, 0)) != 0) {
			(void)fail("writer get", ret);
			return (NULL);
		}
		if (strcmp(got, vbuf) != 0) {
			fprintf(stderr, "FAIL cursorcache_adjust: writer "
			    "key %s round-trip got '%s' want '%s'\n",
			    kbuf, got, vbuf);
			failed = 1;
			return (NULL);
		}
	}
	return (NULL);
}

static void *
reader(void *arg)
{
	DBT key, data;
	int k, nkeys, ret;
	char kbuf[32], vbuf[32], got[32];

	(void)arg;
	nkeys = NWRITERS * KEYS_PER_WRITER;

	while (!stop && !failed) {
		for (k = 0; k < nkeys && !failed; k++) {
			(void)snprintf(kbuf, sizeof(kbuf), "k%08d", k);
			expected_value(k, vbuf, sizeof(vbuf));

			memset(&key, 0, sizeof(key));
			memset(&data, 0, sizeof(data));
			key.data = kbuf;
			key.size = (u_int32_t)strlen(kbuf) + 1;
			data.data = got;
			data.ulen = sizeof(got);
			data.flags = DB_DBT_USERMEM;

			ret = db->get(db, NULL, &key, &data, 0);
			if (ret == DB_NOTFOUND)
				continue;	/* Not written yet; fine. */
			if (ret != 0) {
				(void)fail("reader get", ret);
				return (NULL);
			}
			/*
			 * A present key MUST carry its own value.  A missed
			 * adjustment makes a get return a neighbouring record's
			 * value, which this catches.
			 */
			if (strcmp(got, vbuf) != 0) {
				fprintf(stderr, "FAIL cursorcache_adjust: "
				    "reader key %s got '%s' want '%s'\n",
				    kbuf, got, vbuf);
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
	DBT key, data;
	int i, k, nkeys, ret;
	char kbuf[32], vbuf[32], got[32];
	const char *dir;

	dir = argc > 1 ? argv[1] : "CURSORCACHE_TESTDIR";

	if ((ret = db_env_create(&env, 0)) != 0)
		return (fail("db_env_create", ret));
	/*
	 * DB_THREAD is required: it allocates the per-handle cursor-queue
	 * mutexes the cache avoids, and the cache only activates on a threaded
	 * handle.  No lock subsystem on purpose (see the header comment).
	 */
	if ((ret = env->open(env, dir,
	    DB_CREATE | DB_INIT_MPOOL | DB_THREAD, 0644)) != 0)
		return (fail("env->open", ret));

	if ((ret = db_create(&db, env, 0)) != 0)
		return (fail("db_create", ret));
	/* Small pages so writer bands crowd onto shared leaves. */
	if ((ret = db->set_pagesize(db, 512)) != 0)
		return (fail("set_pagesize", ret));
	if ((ret = db->open(db, NULL, "cursorcache.db", NULL, DB_BTREE,
	    DB_CREATE | DB_THREAD, 0644)) != 0)
		return (fail("db->open", ret));

	for (i = 0; i < NWRITERS; i++)
		if ((ret = pthread_create(&wt[i], NULL,
		    writer, (void *)(intptr_t)i)) != 0)
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

	/* Final serial verification: every key carries its own value. */
	nkeys = NWRITERS * KEYS_PER_WRITER;
	for (k = 0; k < nkeys; k++) {
		(void)snprintf(kbuf, sizeof(kbuf), "k%08d", k);
		expected_value(k, vbuf, sizeof(vbuf));
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kbuf;
		key.size = (u_int32_t)strlen(kbuf) + 1;
		data.data = got;
		data.ulen = sizeof(got);
		data.flags = DB_DBT_USERMEM;
		if ((ret = db->get(db, NULL, &key, &data, 0)) != 0)
			return (fail("final get", ret));
		if (strcmp(got, vbuf) != 0) {
			fprintf(stderr, "FAIL cursorcache_adjust: final key %s "
			    "got '%s' want '%s'\n", kbuf, got, vbuf);
			return (1);
		}
	}

	if ((ret = db->close(db, 0)) != 0)
		return (fail("db->close", ret));
	if ((ret = env->close(env, 0)) != 0)
		return (fail("env->close", ret));

	printf("PASS cursorcache_adjust: %d keys round-tripped under %d "
	    "writers + %d readers on a shared cached-cursor handle\n",
	    nkeys, NWRITERS, NREADERS);
	return (0);
}
