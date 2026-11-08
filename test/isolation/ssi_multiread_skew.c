/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*-
 * ssi_multiread_skew.c -- INDEPENDENT reproducer probe.
 *
 * The Adya cycle checker (test_ssi_cycle) reported, under DB_TXN_SERIALIZABLE
 * at high concurrency, a 2-transaction write skew where EACH transaction has a
 * MULTI-KEY read set, not the single read of the canonical ssi write_skew gate.
 * This program reproduces that exact shape deterministically, with NO cycle
 * checker, NO shared buffers, NO version model -- just two threads, barriers,
 * and a direct check of the committed outcome.  It answers one question: can
 * two serializable transactions, each reading several keys (one of which is the
 * key the other writes) and writing one key the other reads, BOTH commit?
 *
 * SHAPE (keys on DIFFERENT pages, so no page write-write collision):
 *   initial: X=0, Y=0 (plus filler to spread the tree)
 *   T1: read Y (and some other keys), then write X=1
 *   T2: read X (and some other keys), then write Y=1
 *   Both take their snapshot before either writes.  Serial orders give
 *   (X,Y) in {(1,0),(0,1)} ... never (1,1).  (1,1) == both committed == a
 *   write skew that SSI must prevent.
 *
 * The ONLY difference from the known-good ssi write_skew gate is that each
 * txn also reads EXTRA keys before its decisive read.  If (1,1) appears here
 * but not in the single-read gate, the extra reads are the trigger.
 *
 * Env/knobs: SMR_ATTEMPTS (default 200), SMR_EXTRA (extra reads per txn,
 * default 4), SMR_PAGESIZE (default 512), SMR_FILLER (default 400).
 *
 * Exit: 0 = SSI prevented the skew in every attempt (no (1,1)); 1 = at least
 * one attempt committed both (a write skew under DB_TXN_SERIALIZABLE); 2 =
 * harness error.
 */
#include <errno.h>
#include <pthread.h>
#include <sched.h>
#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#include "db.h"
#include "iso_knobs.h"

static DB_ENV	*env;
static DB	*db;
static char	 home[512];
static int	 smr_extra = 4, smr_pagesize = 512, smr_filler = 400;
static int	 smr_sequential = 0;	/* T1 fully commits before T2 writes */
static int	 nkeys;			/* total keys loaded (filler + X + Y) */
static int	 kx, ky;		/* the two decisive keys (far apart) */

static pthread_barrier_t bar;
static atomic_int t1_committed, t2_committed;

static void
die(const char *what, int rc)
{
	fprintf(stderr, "harness error: %s: %s (%d)\n", what, db_strerror(rc), rc);
	exit(2);
}

static void
rmtree(const char *dir)
{
	char cmd[600];
	(void)snprintf(cmd, sizeof(cmd),
	    "find '%s' -mindepth 1 -delete 2>/dev/null", dir);
	(void)system(cmd);
}

static int
rd(DB_TXN *txn, int key, int *out)
{
	DBT k, d;
	char kb[16];
	int v, rc;

	(void)snprintf(kb, sizeof(kb), "k%08d", key);
	memset(&k, 0, sizeof(k));
	memset(&d, 0, sizeof(d));
	k.data = kb; k.size = (u_int32_t)strlen(kb);
	d.data = &v; d.ulen = sizeof(v); d.flags = DB_DBT_USERMEM;
	if ((rc = db->get(db, txn, &k, &d, 0)) != 0)
		return (rc);
	if (out) *out = v;
	return (0);
}

static int
wr(DB_TXN *txn, int key, int val)
{
	DBT k, d;
	char kb[16];

	(void)snprintf(kb, sizeof(kb), "k%08d", key);
	memset(&k, 0, sizeof(k));
	memset(&d, 0, sizeof(d));
	k.data = kb; k.size = (u_int32_t)strlen(kb);
	d.data = &val; d.size = sizeof(val);
	return (db->put(db, txn, &k, &d, 0));
}

static void
open_env(u_int32_t create)
{
	int rc;

	if ((rc = db_env_create(&env, 0)) != 0) die("db_env_create", rc);
	(void)iso_set_partitions(env);
	if ((rc = env->set_lk_detect(env, DB_LOCK_DEFAULT)) != 0)
		die("set_lk_detect", rc);
	if ((rc = env->set_timeout(env, 2000000, DB_SET_LOCK_TIMEOUT)) != 0)
		die("set_timeout", rc);
	if ((rc = env->set_cachesize(env, 0, 16 * 1024 * 1024, 1)) != 0)
		die("set_cachesize", rc);
	if ((rc = env->open(env, home, create | DB_INIT_LOCK | DB_INIT_LOG |
	    DB_INIT_MPOOL | DB_INIT_TXN | DB_THREAD, 0600)) != 0)
		die("env->open", rc);
	if ((rc = db_create(&db, env, 0)) != 0) die("db_create", rc);
	if (create && (rc = db->set_pagesize(db, (u_int32_t)smr_pagesize)) != 0)
		die("set_pagesize", rc);
	if ((rc = db->open(db, NULL, "skew.db", NULL, DB_BTREE,
	    create | DB_MULTIVERSION | DB_AUTO_COMMIT | DB_THREAD, 0600)) != 0)
		die("db->open", rc);
}

static void
close_env(void)
{
	int rc;
	if ((rc = db->close(db, 0)) != 0) die("db->close", rc);
	if ((rc = env->close(env, 0)) != 0) die("env->close", rc);
	env = NULL; db = NULL;
}

static void
sync_pt(void)
{
	int rc = pthread_barrier_wait(&bar);
	if (rc != 0 && rc != PTHREAD_BARRIER_SERIAL_THREAD)
		die("barrier", rc);
}

/* T2: read X (+ extra), write Y. */
static void *
t2_thread(void *arg)
{
	DB_TXN *txn;
	int e, junk, rc, x;

	(void)arg;
	if ((rc = env->txn_begin(env, NULL, &txn, DB_TXN_SERIALIZABLE)) != 0)
		die("t2 begin", rc);
	/* Extra reads first (spread across the key space). */
	for (e = 0; e < smr_extra; e++)
		if ((rc = rd(txn, (e * 7 + 3) % nkeys, &junk)) != 0 &&
		    rc != DB_LOCK_DEADLOCK && rc != DB_SNAPSHOT_CONFLICT &&
		    rc != DB_SNAPSHOT_UNSAFE && rc != DB_LOCK_NOTGRANTED)
			die("t2 extra read", rc);
	rc = rd(txn, kx, &x);			/* the decisive read */
	sync_pt();				/* both snapshots taken + read */
	sync_pt();				/* T1 write done */
	if (rc == 0)
		rc = wr(txn, ky, 1);
	if (rc == 0) {
		rc = txn->commit(txn, 0);
		if (rc == 0) atomic_store(&t2_committed, 1);
	} else
		(void)txn->abort(txn);
	if (rc != 0 && rc != DB_LOCK_DEADLOCK && rc != DB_SNAPSHOT_CONFLICT &&
	    rc != DB_SNAPSHOT_UNSAFE && rc != DB_LOCK_NOTGRANTED)
		die("t2 outcome", rc);
	return (NULL);
}

/* Returns 1 if this attempt committed BOTH (a write skew). */
static int
one_attempt(void)
{
	DB_TXN *txn;
	pthread_t t2;
	int e, junk, rc, y, xv, yv;

	rmtree(home);
	open_env(DB_CREATE);
	/* Load filler + the two decisive keys, all 0. */
	for (e = 0; e < nkeys; e++)
		if ((rc = wr(NULL, e, 0)) != 0)
			die("load", rc);
	{
		static int reported = 0;
		if (!reported) {
			DB_BTREE_STAT *bst;
			reported = 1;
			if (db->stat(db, NULL, &bst, 0) == 0) {
				printf("  tree: %lu leaf pages, %lu levels, "
				    "%lu ndata\n",
				    (u_long)bst->bt_leaf_pg,
				    (u_long)bst->bt_levels,
				    (u_long)bst->bt_ndata);
				free(bst);
			}
		}
	}

	atomic_store(&t1_committed, 0);
	atomic_store(&t2_committed, 0);
	if ((rc = pthread_barrier_init(&bar, NULL, 2)) != 0)
		die("barrier_init", rc);
	if ((rc = pthread_create(&t2, NULL, t2_thread, NULL)) != 0)
		die("pthread_create", rc);

	/* T1: read Y (+ extra), write X. */
	if ((rc = env->txn_begin(env, NULL, &txn, DB_TXN_SERIALIZABLE)) != 0)
		die("t1 begin", rc);
	for (e = 0; e < smr_extra; e++)
		if ((rc = rd(txn, (e * 5 + 11) % nkeys, &junk)) != 0 &&
		    rc != DB_LOCK_DEADLOCK && rc != DB_SNAPSHOT_CONFLICT &&
		    rc != DB_SNAPSHOT_UNSAFE && rc != DB_LOCK_NOTGRANTED)
			die("t1 extra read", rc);
	rc = rd(txn, ky, &y);
	sync_pt();				/* both snapshots taken + read */
	if (rc == 0)
		rc = wr(txn, kx, 1);
	if (smr_sequential) {
		/*
		 * Unambiguous "late" timing: T1 COMMITS before T2 writes, so no
		 * commit-window race (#136) is possible.  A skew here is purely a
		 * conflict-DETECTION gap: T2's write must still find T1's SIREAD
		 * marker on key Y and abort.
		 */
		if (rc == 0) {
			rc = txn->commit(txn, 0);
			if (rc == 0) atomic_store(&t1_committed, 1);
		} else
			(void)txn->abort(txn);
		sync_pt();			/* T1 fully committed -> T2 writes */
	} else {
		sync_pt();			/* T1 write done -> T2 may write */
		if (rc == 0) {
			rc = txn->commit(txn, 0);
			if (rc == 0) atomic_store(&t1_committed, 1);
		} else
			(void)txn->abort(txn);
	}
	if (rc != 0 && rc != DB_LOCK_DEADLOCK && rc != DB_SNAPSHOT_CONFLICT &&
	    rc != DB_SNAPSHOT_UNSAFE && rc != DB_LOCK_NOTGRANTED)
		die("t1 outcome", rc);

	(void)pthread_join(t2, NULL);
	(void)pthread_barrier_destroy(&bar);

	/* Read the durable state back after a reopen. */
	close_env();
	open_env(0);
	if ((rc = rd(NULL, kx, &xv)) != 0 || (rc = rd(NULL, ky, &yv)) != 0)
		die("read back", rc);
	close_env();

	/* A write skew is (X,Y)=(1,1) with BOTH committed. */
	return (atomic_load(&t1_committed) && atomic_load(&t2_committed) &&
	    xv == 1 && yv == 1);
}

int
main(void)
{
	int a, attempts, skews = 0;
	const char *s;

	if ((s = getenv("SMR_EXTRA")) != NULL) smr_extra = atoi(s);
	if ((s = getenv("SMR_SEQUENTIAL")) != NULL) smr_sequential = atoi(s);
	if ((s = getenv("SMR_PAGESIZE")) != NULL) smr_pagesize = atoi(s);
	if ((s = getenv("SMR_FILLER")) != NULL) smr_filler = atoi(s);
	attempts = (s = getenv("SMR_ATTEMPTS")) != NULL ? atoi(s) : 200;

	setvbuf(stdout, NULL, _IOLBF, 0);
	iso_watchdog("ssi_multiread_skew", 240);

	/*
	 * Keys: filler [0..smr_filler), then X and Y.  By default Y is at the far
	 * end (different leaf).  SMR_KYOFF overrides Y's key so a caller can place
	 * X and Y on the SAME leaf (small offset) to probe same-page behaviour:
	 * under page-level MVCC a read of X and a write of a *different* key Y on
	 * the same page is still a cross write-skew on {X,Y} if each txn reads one
	 * and writes the other, and SSI must still prevent it.
	 */
	kx = 0;
	{
		const char *o = getenv("SMR_KYOFF");
		ky = (o != NULL && *o != '\0') ? atoi(o) : smr_filler + 1;
	}
	nkeys = (ky > smr_filler ? ky : smr_filler) + 1;

	(void)snprintf(home, sizeof(home), "SMRDIR");
	(void)mkdir(home, 0755);

	printf("%s\n", db_version(NULL, NULL, NULL));
	printf("ssi_multiread_skew: attempts=%d extra_reads=%d pagesize=%d "
	    "filler=%d (X=key%d Y=key%d)\n",
	    attempts, smr_extra, smr_pagesize, smr_filler, kx, ky);

	for (a = 0; a < attempts; a++)
		if (one_attempt()) {
			skews++;
			printf("  attempt %d: WRITE SKEW -- both committed, "
			    "(X,Y)=(1,1) under DB_TXN_SERIALIZABLE\n", a);
		}

	rmtree(home);
	if (skews != 0) {
		printf("FAIL: %d/%d attempts committed a write skew under "
		    "DB_TXN_SERIALIZABLE (SSI admitted a non-serializable "
		    "history)\n", skews, attempts);
		return (1);
	}
	printf("PASS: 0/%d attempts -- SSI prevented the multi-read write "
	    "skew in every attempt\n", attempts);
	return (0);
}
