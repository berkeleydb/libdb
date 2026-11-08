/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * ssi_livelock.c -- SSI must keep making progress when it is the only log
 * traffic.
 *
 * An SSI txn's read_lsn is the log end at its first page access, and an SSI
 * txn that aborts writes no log record.  When every transaction is an SSI
 * retry, the log end can stop moving, and every new txn's read_lsn then ties
 * with the commit LSN of readers that already committed.  An inclusive
 * rw-edge test (COMMITLSN(R) >= read_lsn(W)) turned each tie into an edge, so
 * every retry aborted DB_SNAPSHOT_UNSAFE, wrote nothing, and the next retry
 * tied again (#257: 22 commits and 482,744 UNSAFE aborts in 4 s, against
 * 4,120 commits for plain SI).
 *
 * The workload: N threads, 64 keys on ~4 leaves, each txn reads 1-4 keys and
 * writes 1-2 different keys (disjoint read/write sets, the write-skew shape).
 * It runs once under DB_TXN_SNAPSHOT and once under DB_TXN_SERIALIZABLE in
 * fresh environments.  SSI aborts more than SI by design; it must still
 * commit at least SSI_LL_FLOOR percent of SI's commits (default 50).
 *
 * Args: [threads (default 2)] [seconds (default 3)].  Exit 0 PASS, 1 FAIL.
 */
#include <errno.h>
#include <pthread.h>
#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "db.h"

#define	NKEYS	64
#define	VALSZ	208

static DB_ENV *env;
static DB *db;
static atomic_int stop;
static atomic_long n_commit, n_abort;
static u_int32_t level;

static void
die(const char *what, int rc)
{
	fprintf(stderr, "ssi_livelock: %s: %s\n", what, db_strerror(rc));
	exit(2);
}

static void
setkey(DBT *k, char *kb, int i)
{
	(void)snprintf(kb, 16, "k%08d", i);
	memset(k, 0, sizeof(*k));
	k->data = kb;
	k->size = (u_int32_t)strlen(kb);
}

static void *
worker(void *arg)
{
	DB_TXN *t;
	DBT k, d;
	char kb[16], vb[VALSZ];
	unsigned seed;
	int i, j, nr, nw, rc, rk[4], wk[2], dup;

	seed = (unsigned)(long)arg * 2654435761u + 7;
	memset(vb, 'v', sizeof(vb));
	while (!atomic_load(&stop)) {
		nw = 1 + (int)(rand_r(&seed) % 2);
		nr = 1 + (int)(rand_r(&seed) % 4);
		for (i = 0; i < nw; i++)
			do
				wk[i] = (int)(rand_r(&seed) % NKEYS);
			while (i == 1 && wk[1] == wk[0]);
		for (i = 0; i < nr; i++)
			do {
				rk[i] = (int)(rand_r(&seed) % NKEYS);
				for (dup = 0, j = 0; j < nw; j++)
					dup |= rk[i] == wk[j];
				for (j = 0; j < i; j++)
					dup |= rk[i] == rk[j];
			} while (dup);
		if ((rc = env->txn_begin(env, NULL, &t, level)) != 0)
			die("txn_begin", rc);
		rc = 0;
		for (i = 0; i < nr && rc == 0; i++) {
			setkey(&k, kb, rk[i]);
			memset(&d, 0, sizeof(d));
			d.flags = DB_DBT_MALLOC;
			if ((rc = db->get(db, t, &k, &d, 0)) == 0)
				free(d.data);
		}
		for (i = 0; i < nw && rc == 0; i++) {
			setkey(&k, kb, wk[i]);
			memset(&d, 0, sizeof(d));
			d.data = vb;
			d.size = sizeof(vb);
			rc = db->put(db, t, &k, &d, 0);
		}
		if (rc != 0)
			(void)t->abort(t);
		else
			rc = t->commit(t, 0);
		if (rc == 0)
			n_commit++;
		else if (rc == DB_SNAPSHOT_CONFLICT || rc == DB_SNAPSHOT_UNSAFE ||
		    rc == DB_LOCK_DEADLOCK || rc == DB_LOCK_NOTGRANTED)
			n_abort++;
		else
			die("txn", rc);
	}
	return (NULL);
}

static long
run(u_int32_t lvl, int nthreads, int secs)
{
	pthread_t th[64];
	DBT k, d;
	char kb[16], vb[VALSZ];
	int i, rc;

	if (system("test -d SSILL && find SSILL -mindepth 1 -delete; "
	    "mkdir -p SSILL") != 0)
		die("setup", EIO);
	level = lvl;
	atomic_store(&stop, 0);
	n_commit = n_abort = 0;
	if ((rc = db_env_create(&env, 0)) != 0)
		die("db_env_create", rc);
	(void)env->set_lk_detect(env, DB_LOCK_DEFAULT);
	(void)env->set_timeout(env, 2000000, DB_SET_LOCK_TIMEOUT);
	(void)env->set_cachesize(env, 0, 64 << 20, 1);
	if ((rc = env->open(env, "SSILL", DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | DB_THREAD, 0)) != 0)
		die("env->open", rc);
	if ((rc = db_create(&db, env, 0)) != 0)
		die("db_create", rc);
	if ((rc = db->open(db, NULL, "ll.db", NULL, DB_BTREE, DB_CREATE |
	    DB_MULTIVERSION | DB_AUTO_COMMIT | DB_THREAD, 0)) != 0)
		die("db->open", rc);
	memset(vb, '0', sizeof(vb));
	for (i = 0; i < NKEYS; i++) {
		setkey(&k, kb, i);
		memset(&d, 0, sizeof(d));
		d.data = vb;
		d.size = sizeof(vb);
		if ((rc = db->put(db, NULL, &k, &d, 0)) != 0)
			die("load", rc);
	}
	for (i = 0; i < nthreads; i++)
		(void)pthread_create(&th[i], NULL, worker, (void *)(long)i);
	(void)sleep((unsigned)secs);
	atomic_store(&stop, 1);
	for (i = 0; i < nthreads; i++)
		(void)pthread_join(th[i], NULL);
	(void)db->close(db, 0);
	(void)env->close(env, 0);
	printf("  %-12s commits=%ld aborts=%ld\n",
	    lvl == DB_TXN_SERIALIZABLE ? "SERIALIZABLE" : "SNAPSHOT",
	    (long)n_commit, (long)n_abort);
	return ((long)n_commit);
}

int
main(int argc, char *argv[])
{
	long si, ssi;
	int floor, nthreads, secs;
	const char *f;

	nthreads = argc > 1 ? atoi(argv[1]) : 2;
	secs = argc > 2 ? atoi(argv[2]) : 3;
	floor = (f = getenv("SSI_LL_FLOOR")) != NULL ? atoi(f) : 50;
	if (nthreads < 1 || nthreads > 64)
		nthreads = 2;
	setvbuf(stdout, NULL, _IOLBF, 0);
	printf("%s\nssi_livelock: threads=%d secs=%d floor=%d%%\n",
	    db_version(NULL, NULL, NULL), nthreads, secs, floor);
	si = run(DB_TXN_SNAPSHOT, nthreads, secs);
	ssi = run(DB_TXN_SERIALIZABLE, nthreads, secs);
	(void)system("find SSILL -delete 2>/dev/null");
	if (si == 0) {
		printf("FAIL: plain SI committed nothing -- the harness is "
		    "broken, nothing to compare\n");
		return (1);
	}
	if (ssi * 100 < si * floor) {
		printf("FAIL: SSI committed %ld = %ld%% of SI's %ld (< %d%%) -- "
		    "SSI is livelocking\n", ssi, ssi * 100 / si, si, floor);
		return (1);
	}
	printf("PASS: SSI committed %ld = %ld%% of SI's %ld (>= %d%%)\n",
	    ssi, ssi * 100 / si, si, floor);
	return (0);
}
