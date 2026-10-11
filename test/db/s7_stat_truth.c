/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * s7_stat_truth.c -- RFC 0015 S7: db_stat must describe the latch it reads.
 *
 * Two defects, one check each.
 *
 * (b) c_locker sampled ONE stripe.  After P12 a locker's create/free takes
 *     only its own stripe of LOCK_LOCKER_STRIPES (64), but __lock_stat copied
 *     the wait/nowait counts of mtx_locker_stripe[0] alone (lock_stat.c:168),
 *     so "locker allocations that required waiting" was a 1/64 sample that
 *     every P12 measurement quoted as the whole.  The check is EXACT: the
 *     reported pair must equal the sum over all 64 stripe mutexes, read here
 *     independently through the mutex statistics.  It also has a public-API
 *     floor: N transactions create and free N lockers, so the total cannot be
 *     below N -- which a stripe-0 sample is, by ~63/64.
 *
 * (a) Three names for one mutex.  LOCK_REGION_LOCK, TXN_SYSTEM_LOCK and
 *     LOG_SYSTEM_LOCK all resolve to renv->mtx_regenv, so `db_stat -c`, `-t`
 *     and `-l` print one counter as "region locks" three times, and G13 and
 *     P12 each misread it once.  Each line must now say it is mtx_regenv and
 *     who shares it.  The label is derived from the mutex ids at runtime
 *     (__env_regenv_tag), so it cannot go stale when RFC 0015 M1 splits it.
 *
 * Single-threaded, so no stripe counter moves between the two reads compared.
 */
#include "db_config.h"

#include "db_int.h"
#include "dbinc/lock.h"

#define	HOME	"TESTDIR_s7"
#define	NTXN	1000

static int
grep_line(const char *path, const char *needle, const char *want)
{
	FILE *fp;
	char line[1024];
	int found;

	if ((fp = fopen(path, "r")) == NULL)
		return (-1);
	found = 0;
	while (fgets(line, sizeof(line), fp) != NULL)
		if (strstr(line, needle) != NULL) {
			found = strstr(line, want) != NULL ? 1 : 2;
			break;
		}
	(void)fclose(fp);
	return (found);
}

int
main(int argc, char *argv[])
{
#ifndef HAVE_STATISTICS
	COMPQUIET(argc, 0);
	COMPQUIET(argv, NULL);
	printf("VERDICT s7_stat_truth SKIP built without statistics\n");
	return (0);
#else
	DB_ENV *dbenv;
	DB_LOCKREGION *region;
	DB_LOCK_STAT *lsp;
	DB_LOG_STAT *gsp;
	DB_TXN *txn;
	DB_TXN_STAT *tsp;
	ENV *env;
	FILE *fp;
	uintmax_t w, nw, sw, snw, s0w, s0nw;
	u_int32_t i;
	int bad, r, ret;
	const char *f;
	static const char *files[] = { HOME "/c.out", HOME "/t.out",
	    HOME "/l.out" };

	COMPQUIET(argc, 0);
	COMPQUIET(argv, NULL);
	bad = 0;

	if ((ret = db_env_create(&dbenv, 0)) != 0 ||
	    (ret = dbenv->open(dbenv, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN | DB_THREAD, 0600)) != 0) {
		printf("VERDICT s7_stat_truth FAIL env open: %s\n",
		    db_strerror(ret));
		return (1);
	}
	env = dbenv->env;

	/* Each begin/commit creates and frees one locker on its own stripe. */
	for (i = 0; i < NTXN; i++) {
		if ((ret = dbenv->txn_begin(dbenv, NULL, &txn, 0)) != 0 ||
		    (ret = txn->commit(txn, 0)) != 0) {
			printf("VERDICT s7_stat_truth FAIL txn %u: %s\n",
			    i, db_strerror(ret));
			return (1);
		}
	}

	if ((ret = dbenv->lock_stat(dbenv, &lsp, 0)) != 0) {
		printf("VERDICT s7_stat_truth FAIL lock_stat: %s\n",
		    db_strerror(ret));
		return (1);
	}
	region = env->lk_handle->reginfo.primary;
	sw = snw = 0;
	for (i = 0; i < LOCK_LOCKER_STRIPES; i++) {
		__mutex_set_wait_info(env,
		    region->mtx_locker_stripe[i], &w, &nw);
		sw += w;
		snw += nw;
		if (i == 0) {
			s0w = w;
			s0nw = nw;
		}
	}
	printf("  locker stripes: reported %ju/%ju  sum(64) %ju/%ju  "
	    "stripe0 %ju/%ju  (wait/nowait, %d txns)\n",
	    lsp->st_lockers_wait, lsp->st_lockers_nowait, sw, snw,
	    s0w, s0nw, NTXN);
	if (lsp->st_lockers_wait != sw || lsp->st_lockers_nowait != snw) {
		printf("  FAIL (b): st_lockers_* is not the sum over all %d "
		    "locker stripes%s\n", LOCK_LOCKER_STRIPES,
		    lsp->st_lockers_wait == s0w &&
		    lsp->st_lockers_nowait == s0nw ?
		    " -- it equals stripe 0 alone" : "");
		bad = 1;
	}
	if (lsp->st_lockers_wait + lsp->st_lockers_nowait < NTXN) {
		printf("  FAIL (b): %d txns created and freed %d lockers, but "
		    "only %ju locker-latch acquisitions are reported\n",
		    NTXN, NTXN,
		    lsp->st_lockers_wait + lsp->st_lockers_nowait);
		bad = 1;
	}

	/* (a): the three subsystems' region counters are one mutex. */
	if ((ret = dbenv->txn_stat(dbenv, &tsp, 0)) != 0 ||
	    (ret = dbenv->log_stat(dbenv, &gsp, 0)) != 0) {
		printf("VERDICT s7_stat_truth FAIL txn/log stat: %s\n",
		    db_strerror(ret));
		return (1);
	}
	printf("  region latch: lock %ju/%ju  txn %ju/%ju  log %ju/%ju\n",
	    lsp->st_region_wait, lsp->st_region_nowait,
	    tsp->st_region_wait, tsp->st_region_nowait,
	    gsp->st_region_wait, gsp->st_region_nowait);
	/*
	 * Not compared for equality: each *_stat call takes mtx_regenv itself,
	 * so the three reads differ by exactly those acquisitions.  The labels
	 * below are what assert the aliasing, and they test it by mutex id.
	 */

	for (i = 0; i < 3; i++) {
		if ((fp = fopen(files[i], "w")) == NULL) {
			printf("VERDICT s7_stat_truth FAIL open %s\n",
			    files[i]);
			return (1);
		}
		dbenv->set_msgfile(dbenv, fp);
		ret = i == 0 ? dbenv->lock_stat_print(dbenv, 0) :
		    i == 1 ? dbenv->txn_stat_print(dbenv, 0) :
		    dbenv->log_stat_print(dbenv, 0);
		dbenv->set_msgfile(dbenv, NULL);
		(void)fclose(fp);
		if (ret != 0) {
			printf("VERDICT s7_stat_truth FAIL stat_print %d: %s\n",
			    (int)i, db_strerror(ret));
			return (1);
		}
	}
	for (i = 0; i < 3; i++) {
		f = files[i];
		r = grep_line(f, "region locks that required waiting",
		    "mtx_regenv, shared by lock txn log");
		printf("  %s region-locks line labelled: %s\n", f,
		    r == 1 ? "yes" : r == 2 ? "NO" : "line missing");
		if (r != 1)
			bad = 1;
	}
	r = grep_line(files[0], "locker allocations that required waiting",
	    "summed over all locker stripes");
	printf("  %s locker line labelled as a stripe sum: %s\n", files[0],
	    r == 1 ? "yes" : r == 2 ? "NO" : "line missing");
	if (r != 1)
		bad = 1;

	free(lsp);
	free(tsp);
	free(gsp);
	if ((ret = dbenv->close(dbenv, 0)) != 0) {
		printf("VERDICT s7_stat_truth FAIL env close: %s\n",
		    db_strerror(ret));
		return (1);
	}
	if (bad) {
		printf("VERDICT s7_stat_truth FAIL\n");
		return (1);
	}
	printf("VERDICT s7_stat_truth PASS c_locker sums all %d stripes; "
	    "-c/-t/-l name the shared mtx_regenv\n", LOCK_LOCKER_STRIPES);
	return (0);
#endif
}
