/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * cursorcache_parkcost.c --
 *	Regression test for the transient-cursor-cache THREAD-EXIT defect
 *	(RFC 0021, review #238 concern 2).
 *
 * A thread that uses DB->get/DB->put on a shared handle caches up to
 * DBC_CACHE_SLOTS transient cursors in its thread-local storage, left linked on
 * the handle's active queue.  Before the fix, those cursors stayed on the
 * active queue until the HANDLE closed, even after the thread exited; a
 * churning thread pool therefore piled hundreds of inert cursors there, and
 * every subsequent index-shifting insert walked them (__db_walk_cursors),
 * slowing writes by up to 2.3x for 256 exited pool threads.
 *
 * The fix flushes an exiting thread's cached cursors back to their handles'
 * free queues (a pthread-key destructor), so the active queue stays small.
 *
 * The test: measure single-thread insert time on one shared handle before
 * (phase 0, clean queue) and after (phase 1) a pool of POOL worker threads
 * fills its cache slots and EXITS.  With the fix, phase 1 is no slower than
 * phase 0.  A DB->verify closes.
 *
 * CI-safe bounds: moderate POOL and BATCH; FAIL if ratio > RATIO_MAX, which is
 * clear of the fixed behaviour (~1.0) and well below the regressed 1.4-2.0.
 */

#include <sys/types.h>

#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "db.h"

#define	POOL_DEFAULT	256
#define	GETS_PER	8		/* > DBC_CACHE_SLOTS(4). */
#define	BATCH		100000
#define	NKEYS		2000
#define	RATIO_MAX	1.5

static DB_ENV *env;
static DB *db;

static int
mkkey(int k, char *b, size_t n)
{
	return ((int)snprintf(b, n, "k%08d", k));
}

static double
now_s(void)
{
	struct timespec ts;

	(void)clock_gettime(CLOCK_MONOTONIC, &ts);
	return (ts.tv_sec + ts.tv_nsec / 1e9);
}

static void *
filler(void *a)
{
	unsigned seed;
	DBT key, data;
	char kb[32];
	int i;

	seed = (unsigned)(uintptr_t)a * 2654435761u + 1;
	for (i = 0; i < GETS_PER; i++) {
		(void)mkkey((int)(rand_r(&seed) % NKEYS), kb, sizeof(kb));
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kb;
		key.size = (u_int32_t)strlen(kb) + 1;
		data.flags = DB_DBT_MALLOC;
		if (db->get(db, NULL, &key, &data, 0) == 0)
			free(data.data);
	}
	return (NULL);		/* exits with cursors cached for db */
}

static double
timed_inserts(int phase)
{
	DBT key, data;
	char kb[32], vb[32];
	int i;
	double t0, t1;
	DB_TXN *t;

	if (env->txn_begin(env, NULL, &t, 0) != 0)
		return (-1.0);
	t0 = now_s();
	for (i = 0; i < BATCH; i++) {
		(void)mkkey(i % NKEYS, kb, sizeof(kb));
		(void)snprintf(vb, sizeof(vb), "p%d.%06d", phase, i);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kb;
		key.size = (u_int32_t)strlen(kb) + 1;
		data.data = vb;
		data.size = (u_int32_t)strlen(vb) + 1;
		if (db->put(db, t, &key, &data, 0) != 0) {
			(void)t->abort(t);
			return (-1.0);
		}
	}
	t1 = now_s();
	(void)t->commit(t, 0);
	return (t1 - t0);
}

int
main(int argc, char *argv[])
{
	DBT key, data;
	DB *v;
	char kb[32];
	const char *dir;
	int i, pool, ret;
	double p0, p1, ratio;
	pthread_t *t;

	dir = argc > 1 ? argv[1] : "CURSORCACHE_PARK_DIR";
	pool = argc > 2 ? atoi(argv[2]) : POOL_DEFAULT;
	if (pool < 1)
		pool = POOL_DEFAULT;

	if ((ret = db_env_create(&env, 0)) != 0)
		goto err_env;
	(void)env->set_cachesize(env, 0, 256 << 20, 1);
	if ((ret = env->open(env, dir, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_MPOOL | DB_INIT_LOG | DB_INIT_TXN | DB_THREAD |
	    DB_PRIVATE, 0644)) != 0)
		goto err_env;
	if ((ret = db_create(&db, env, 0)) != 0)
		goto err;
	if ((ret = db->open(db, NULL, "park.db", NULL, DB_BTREE,
	    DB_CREATE | DB_THREAD | DB_AUTO_COMMIT, 0644)) != 0)
		goto err;

	for (i = 0; i < NKEYS; i++) {
		(void)mkkey(i, kb, sizeof(kb));
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = kb;
		key.size = (u_int32_t)strlen(kb) + 1;
		data.data = (void *)"s";
		data.size = 2;
		if ((ret = db->put(db, NULL, &key, &data, 0)) != 0)
			goto err;
	}

	p0 = timed_inserts(0);

	if ((t = malloc(sizeof(*t) * (size_t)pool)) == NULL) {
		printf("cursorcache_parkcost: OOM\n");
		goto err2;
	}
	for (i = 0; i < pool; i++)
		(void)pthread_create(&t[i], NULL, filler, (void *)(uintptr_t)i);
	for (i = 0; i < pool; i++)
		(void)pthread_join(t[i], NULL);
	free(t);

	p1 = timed_inserts(1);

	(void)db->close(db, 0);
	if ((ret = db_create(&v, env, 0)) != 0)
		goto err2;
	if ((ret = v->verify(v, "park.db", NULL, NULL, 0)) != 0) {
		printf("cursorcache_parkcost: FAIL verify %s\n",
		    db_strerror(ret));
		(void)env->close(env, 0);
		return (1);
	}
	(void)env->close(env, 0);

	if (p0 <= 0.0 || p1 < 0.0) {
		printf("cursorcache_parkcost: FAIL insert timing error\n");
		return (1);
	}
	ratio = p1 / p0;
	printf("cursorcache_parkcost: pool=%d phase0=%.3fs phase1=%.3fs "
	    "ratio=%.2f\n", pool, p0, p1, ratio);
	if (ratio > RATIO_MAX) {
		printf("cursorcache_parkcost: FAIL ratio %.2f > %.2f "
		    "(parked cursors from exited threads slow inserts)\n",
		    ratio, RATIO_MAX);
		return (1);
	}
	printf("PASS cursorcache_parkcost\n");
	return (0);

err2:	(void)env->close(env, 0);
	return (2);
err:	printf("cursorcache_parkcost: setup error: %s\n", db_strerror(ret));
	(void)env->close(env, 0);
	return (2);
err_env:
	printf("cursorcache_parkcost: env setup error: %s\n", db_strerror(ret));
	return (2);
}
