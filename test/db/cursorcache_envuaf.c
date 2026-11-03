/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * cursorcache_envuaf.c --
 *	Regression test for the transient-cursor-cache ENV use-after-free
 *	(RFC 0021, review #238): a cache slot must never dereference its
 *	handle or env once that handle may be closed, because the thread that
 *	CLOSES a handle need not be the thread that CACHED a cursor on it.
 *
 * A worker thread does one DB->get on handle H in env E (caching a transient
 * cursor) and stays alive.  The main thread then closes H and E -- legal,
 * because the worker is idle.  Two arms exercise the two reconcile paths that
 * the worker's stale slot can reach:
 *   "exit"  -- the worker exits, firing the pthread-key destructor, which
 *              reconciles the slot;
 *   "reuse" -- the worker opens a NEW env E2 and does a get there, so
 *              __dbc_cache_check_epoch reconciles the old slot.
 * Before the live-handle-registry fix both reconcile paths called
 * MUTEX_LOCK on the freed ENV E (heap-use-after-free under ASan at
 * __dbc_cache_handle_live).  With the fix, liveness is a registry lookup that
 * dereferences nothing, so both arms are clean.
 *
 * Built with AddressSanitizer by the runner when available: the fault is a
 * read of freed heap, which ASan catches deterministically.  Without ASan the
 * test still runs both arms for exit-code sanity (it may not fault, since the
 * freed memory often still looks valid, but it must not crash on a correct
 * build).  The runner does both: FAIL on the pre-fix head under ASan, PASS
 * fixed; exit 0 either way without ASan.
 */

#include <sys/types.h>

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "db.h"

static pthread_mutex_t mu = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t cv = PTHREAD_COND_INITIALIZER;
static int stage;
static DB *gdb;
static int reuse;
static int worker_failed;

static void
wait_stage(int s)
{
	pthread_mutex_lock(&mu);
	while (stage < s)
		pthread_cond_wait(&cv, &mu);
	pthread_mutex_unlock(&mu);
}

static void
set_stage(int s)
{
	pthread_mutex_lock(&mu);
	stage = s;
	pthread_cond_broadcast(&cv);
	pthread_mutex_unlock(&mu);
}

static int
open_ed(const char *home, DB_ENV **ep, DB **dp)
{
	char cmd[256];
	DBT k, d;

	(void)snprintf(cmd, sizeof(cmd),
	    "test -d %s && find %s -mindepth 1 -delete; mkdir -p %s",
	    home, home, home);
	if (system(cmd) != 0)
		return (1);
	if (db_env_create(ep, 0) != 0 || (*ep)->open(*ep, home,
	    DB_CREATE | DB_INIT_LOCK | DB_INIT_MPOOL | DB_INIT_LOG |
	    DB_INIT_TXN | DB_THREAD | DB_PRIVATE, 0644) != 0)
		return (1);
	if (db_create(dp, *ep, 0) != 0 || (*dp)->open(*dp, NULL, "u.db", NULL,
	    DB_BTREE, DB_CREATE | DB_THREAD | DB_AUTO_COMMIT, 0644) != 0)
		return (1);
	memset(&k, 0, sizeof(k));
	memset(&d, 0, sizeof(d));
	k.data = (void *)"k";
	k.size = 1;
	d.data = (void *)"v";
	d.size = 1;
	(void)(*dp)->put(*dp, NULL, &k, &d, 0);
	return (0);
}

static void
get1(DB *db)
{
	DBT k, d;

	memset(&k, 0, sizeof(k));
	memset(&d, 0, sizeof(d));
	k.data = (void *)"k";
	k.size = 1;
	d.flags = DB_DBT_MALLOC;
	if (db->get(db, NULL, &k, &d, 0) == 0)
		free(d.data);
}

static void *
worker(void *a)
{
	DB_ENV *e2;
	DB *d2;

	COMPQUIET(a, NULL);
	get1(gdb);		/* caches a cursor on H (env E) */
	set_stage(1);
	wait_stage(2);		/* main has closed H and E */
	if (reuse) {
		if (open_ed("CCUAF2", &e2, &d2) != 0) {
			worker_failed = 1;
			return (NULL);
		}
		get1(d2);	/* check_epoch -> reconcile old slot */
		(void)d2->close(d2, 0);
		(void)e2->close(e2, 0);
	}
	return (NULL);		/* destructor -> reconcile(1) */
}

int
main(int argc, char *argv[])
{
	DB_ENV *env;
	pthread_t t;

	reuse = argc > 1 && strcmp(argv[1], "reuse") == 0;
	if (open_ed("CCUAF", &env, &gdb) != 0) {
		printf("cursorcache_envuaf: env/db open failed\n");
		return (2);
	}
	if (pthread_create(&t, NULL, worker, NULL) != 0) {
		printf("cursorcache_envuaf: pthread_create failed\n");
		return (2);
	}
	wait_stage(1);
	(void)gdb->close(gdb, 0);
	(void)env->close(env, 0);	/* frees ENV E */
	set_stage(2);
	(void)pthread_join(t, NULL);
	if (worker_failed) {
		printf("cursorcache_envuaf: worker setup failed\n");
		return (2);
	}
	printf("PASS cursorcache_envuaf (%s): no use-after-free\n",
	    reuse ? "reuse" : "exit");
	return (0);
}
