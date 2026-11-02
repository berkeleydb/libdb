/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 * SPDX-License-Identifier: Sleepycat
 *
 * meta_nta_conc.c --
 *	Concurrency coverage for RFC 0022.  Transactions that each allocate a
 *	page must no longer serialise to commit on the PGNO_BASE_MD meta lock.
 *
 *	To isolate the META-page convoy from unrelated shared-page contention
 *	(the B1 right-edge / shared-internal-page convoy), the test first
 *	PRE-POPULATES a deep btree, then gives each worker a DISJOINT, widely
 *	separated key band so its split/allocation touches leaves and parent
 *	internal pages that no other worker touches.  The only lock object all
 *	workers share is then the metadata page (page 0), which __db_new locks
 *	on every allocation.
 *
 *	Each worker opens a transaction, inserts into its band enough to force
 *	at least one page allocation, then HOLDS the transaction open for
 *	HOLD_MS before committing -- standing in for the durable-commit window
 *	during which the old code pinned the meta write lock (BTREE-LOCK-SCOPE
 *	measured 400/400 waiters on page 0 under exactly this pattern).
 *
 *	  - Without the NTA, __db_new holds the meta lock to commit, so the N
 *	    workers serialise: total wall time ~= N * HOLD_MS.
 *	  - With the NTA, the meta lock is released when the allocation
 *	    finishes, before the hold, so the holds overlap: ~= HOLD_MS.
 *
 *	The test asserts elapsed < half the serial bound: fails on an unpatched
 *	library, passes on a patched one.  It also checks every worker's keys
 *	are present -- correctness under the relaxed lock scope.
 *
 *	A SIGALRM guard bounds the run.
 *
 * Usage (from the build dir):
 *	BUILD=. sh ../test/db/run_meta_nta_conc.sh
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>

#include <errno.h>
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "db.h"

#define	HOME		"META_NTA_CONC_TESTDIR"
#define	TABLE		"ntaconc.db"
#define	PAGESIZE	512
#define	NWORKERS	8
#define	HOLD_MS		300		/* commit-window stand-in */
/*
 * Each worker owns a band of BAND_SIZE logical slots, BAND_STRIDE apart, so
 * the bands are far enough that their leaves and parent internal pages do not
 * overlap.  The pre-fill seeds every band sparsely (every PREFILL_STEP-th
 * slot) so the tree is already deep; the worker then fills the gaps in its
 * band, forcing splits/allocations in its own subtree only.
 */
#define	BAND_STRIDE	10000000u
#define	PREFILL_PER_BAND 400
#define	PREFILL_STEP	64
#define	KEYS_PER_TXN	200		/* fills gaps -> forces allocation */
#define	ALARM_SECS	120

static DB_ENV *dbenv;
static DB *db;
static int worker_fail[NWORKERS];

#define	CHK0(call) do {							\
	int _r = (call);						\
	if (_r != 0) {							\
		fprintf(stderr, "FAIL: %s:%d: %s => %d (%s)\n",		\
		    __FILE__, __LINE__, #call, _r, db_strerror(_r));	\
		return (EXIT_FAILURE);					\
	}								\
} while (0)

static void
msleep(long ms)
{
	struct timespec ts;
	ts.tv_sec = ms / 1000;
	ts.tv_nsec = (ms % 1000) * 1000000L;
	(void)nanosleep(&ts, NULL);
}

static int
put1(DB_TXN *txn, u_int32_t k)
{
	DBT key, data;
	char kbuf[48], vbuf[200];

	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	(void)snprintf(kbuf, sizeof(kbuf), "%010u", k);
	(void)snprintf(vbuf, sizeof(vbuf),
	    "v-%010u-%s", k, "padpadpadpadpadpadpadpadpadpadpadpadpad");
	key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
	data.data = vbuf; data.size = (u_int32_t)strlen(vbuf) + 1;
	return (db->put(db, txn, &key, &data, 0));
}

static void *
worker(void *arg)
{
	DB_TXN *txn;
	long id = (long)arg;
	u_int32_t i, base;
	int ret;

	base = (u_int32_t)id * BAND_STRIDE;
	if ((ret = dbenv->txn_begin(dbenv, NULL, &txn, 0)) != 0) {
		fprintf(stderr, "worker %ld: txn_begin => %s\n",
		    id, db_strerror(ret));
		worker_fail[id] = 1;
		return (NULL);
	}
	/* Fill the gaps between the pre-seeded keys -> splits + allocations. */
	for (i = 0; i < KEYS_PER_TXN; i++) {
		/* interleave into the pre-seeded skeleton (odd offsets). */
		if ((ret = put1(txn, base + i * 3 + 1)) != 0) {
			fprintf(stderr, "worker %ld: put => %s\n",
			    id, db_strerror(ret));
			(void)txn->abort(txn);
			worker_fail[id] = 1;
			return (NULL);
		}
	}

	/*
	 * Allocation is done; the meta lock must be released by now (NTA).
	 * Hold the txn open to model the durable-commit window.
	 */
	msleep(HOLD_MS);

	if ((ret = txn->commit(txn, 0)) != 0) {
		fprintf(stderr, "worker %ld: commit => %s\n",
		    id, db_strerror(ret));
		worker_fail[id] = 1;
	}
	return (NULL);
}

static long
elapsed_ms(struct timeval *a, struct timeval *b)
{
	return ((b->tv_sec - a->tv_sec) * 1000L +
	    (b->tv_usec - a->tv_usec) / 1000L);
}

int
main(void)
{
	pthread_t th[NWORKERS];
	struct timeval t0, t1;
	DB_TXN *txn;
	DBT key, data;
	long i, j, el, serial_bound;
	int ret, fails = 0;
	char kbuf[48];

	(void)signal(SIGALRM, SIG_DFL);
	(void)alarm(ALARM_SECS);

	(void)system("rm -f " HOME "/__db.* " HOME "/log.* " HOME "/*.db "
	    HOME "/DB_CONFIG 2>/dev/null");
	(void)mkdir(HOME, 0755);

	if (db_env_create(&dbenv, 0) != 0)
		return (EXIT_FAILURE);
	dbenv->set_errpfx(dbenv, "meta_nta_conc");
	if ((ret = dbenv->open(dbenv, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | DB_THREAD |
	    DB_RECOVER, 0644)) != 0) {
		fprintf(stderr, "env open: %s\n", db_strerror(ret));
		return (EXIT_FAILURE);
	}
	if (db_create(&db, dbenv, 0) != 0)
		return (EXIT_FAILURE);
	(void)db->set_pagesize(db, PAGESIZE);
	if ((ret = db->open(db, NULL, TABLE, NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT | DB_THREAD, 0644)) != 0) {
		fprintf(stderr, "db open: %s\n", db_strerror(ret));
		return (EXIT_FAILURE);
	}

	/*
	 * Pre-populate: seed every band sparsely so the tree is already deep
	 * and each worker's band sits under its own internal pages.  One big
	 * committed txn, before any worker starts.
	 */
	CHK0(dbenv->txn_begin(dbenv, NULL, &txn, 0));
	for (i = 0; i < NWORKERS; i++)
		for (j = 0; j < PREFILL_PER_BAND; j++)
			CHK0(put1(txn,
			    (u_int32_t)i * BAND_STRIDE +
			    (u_int32_t)j * PREFILL_STEP));
	CHK0(txn->commit(txn, 0));

	(void)gettimeofday(&t0, NULL);
	for (i = 0; i < NWORKERS; i++)
		if (pthread_create(&th[i], NULL, worker, (void *)i) != 0) {
			fprintf(stderr, "pthread_create %ld failed\n", i);
			return (EXIT_FAILURE);
		}
	for (i = 0; i < NWORKERS; i++)
		(void)pthread_join(th[i], NULL);
	(void)gettimeofday(&t1, NULL);

	for (i = 0; i < NWORKERS; i++)
		if (worker_fail[i])
			fails++;

	el = elapsed_ms(&t0, &t1);
	serial_bound = (long)NWORKERS * HOLD_MS;
	printf("meta_nta_conc: %d workers, hold=%dms, elapsed=%ldms "
	    "(serial bound=%ldms)\n", NWORKERS, HOLD_MS, el, serial_bound);

	/*
	 * The decisive assertion: with the meta lock released before the hold,
	 * the holds overlap and elapsed is far below the serial bound.  Use a
	 * generous threshold (half the serial bound) so scheduling noise does
	 * not flap the test, while still failing a library that serialises
	 * (which would take ~NWORKERS*HOLD_MS).
	 */
	if (el >= serial_bound / 2) {
		fprintf(stderr, "FAIL: allocators serialised: elapsed %ldms "
		    ">= %ldms (half the serial bound). The meta lock was held "
		    "across the commit window.\n", el, serial_bound / 2);
		fails++;
	}

	/* Correctness: a representative key from each worker band is present. */
	for (i = 0; i < NWORKERS; i++) {
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		data.flags = DB_DBT_MALLOC;	/* DB_THREAD env requires it */
		(void)snprintf(kbuf, sizeof(kbuf), "%010u",
		    (u_int32_t)i * BAND_STRIDE + 1);
		key.data = kbuf; key.size = (u_int32_t)strlen(kbuf) + 1;
		if ((ret = db->get(db, NULL, &key, &data, 0)) != 0) {
			fprintf(stderr, "FAIL: worker %ld key missing: %s\n",
			    i, db_strerror(ret));
			fails++;
		} else
			free(data.data);
	}

	(void)db->close(db, 0);
	(void)dbenv->close(dbenv, 0);

	if (fails != 0) {
		fprintf(stderr, "meta_nta_conc: %d checks FAILED\n", fails);
		return (EXIT_FAILURE);
	}
	printf("meta_nta_conc: PASS\n");
	return (EXIT_SUCCESS);
}
