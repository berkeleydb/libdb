/*-
 * See the file LICENSE for redistribution information.
 *
 * leak_si_locker.c -- resource-accounting regression test for the SSI
 * committed-reader locker sawtooth (GitHub issue #137).
 *
 * Runs many *sequential* read-only DB_TXN_SERIALIZABLE (SSI) transactions in
 * one long-lived environment -- one transaction active at a time, no
 * checkpoint, no injected fault -- and asserts through the public statistics
 * APIs that the locker / mutex slot counts stay bounded (a function of the
 * lock-object region size, NOT of the transaction count), and that
 * DB_ENV->txn_begin never returns ENOMEM.
 *
 * The invariant, and why it is a sawtooth (not a flat plateau):
 *   A committed SSI reader leaves SIREAD markers on the objects it read;
 *   __lock_sicommit flags the reader's locker DB_LOCKER_FREED and defers
 *   freeing the locker (and its logical mutex and TXN_DETAIL) until the last
 *   marker is garbage-collected.  Reclamation is __lock_sicleanup, which for
 *   a write-free, checkpoint-free workload runs only from a pressure trigger
 *   at txn_begin: when live markers exceed st_objects / 8.  So the marker /
 *   locker / detail population climbs ~1 per transaction, then the sweep
 *   reclaims everything obsolete and it drops -- a sawtooth whose CEILING is
 *   st_objects / 8, reached and re-reached forever, independent of how many
 *   transactions run.  This is correct, bounded behavior, not a leak.
 *
 * A *true* per-transaction leak (a committed reader whose locker is never
 * reclaimed) has a different signature: the peak grows without bound with the
 * transaction count and eventually returns ENOMEM.  This test distinguishes
 * the two: it runs long enough to span many sawtooth periods and asserts both
 * (1) the peak never exceeds a small multiple of the sawtooth ceiling, and
 * (2) the peak of a late window does not exceed the peak of an early window --
 * for a bounded sawtooth both windows reach the same ceiling; a per-txn leak
 * makes the late window's peak strictly (and unboundedly) larger.
 *
 * Usage: leak_si_locker [snapshot|control]   (default: snapshot)
 *   snapshot -- DB_TXN_SERIALIZABLE (the trigger; exercises SIREAD markers)
 *   control  -- flags 0 / plain SI (no SIREAD markers; must stay flat)
 *
 * Self-bounded and deterministic: fixed transaction count, single thread.
 */
#include <sys/types.h>
#include <sys/stat.h>

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "db.h"

#define	HOME		"TESTDIR_leak_si_locker"
#define	ATTEMPTS	8000		/* sequential read-only txns */
#define	SAMPLE_EVERY	100

/*
 * Ceiling.  The sweep fires when live SIREAD markers pass st_objects / 8, so
 * the sawtooth peaks at ~st_objects / 8 plus the small in-flight working set.
 * We allow 4x that as the hard bound: comfortably above the true ceiling, but
 * far below the per-txn leak signature (which grows past st_objects / 2 toward
 * region exhaustion / ENOMEM at a few thousand transactions in the default
 * region).  Both this and the window-peak comparison below have teeth: neuter
 * __lock_sireap_lockers and this test fails.
 */
#define	SAWTOOTH_DIVISOR	8u	/* mirrors SI_CLEANUP_TRIGGER_DIV in txn.c */
#define	CEILING_SLACK		4u	/* headroom over the true ceiling */

static DB_ENV *env;
static DB *db;

static void
fail(const char *op, int ret)
{
	fprintf(stderr, "FAIL %s: %s (%d)\n", op, db_strerror(ret), ret);
	exit(1);
}

/* Read st_nlockers and, on request, st_objects (the region size). */
static u_int32_t
lockers(u_int32_t *nobjp)
{
	DB_LOCK_STAT *sp;
	u_int32_t n;
	int ret;

	if ((ret = env->lock_stat(env, &sp, 0)) != 0)
		fail("DB_ENV->lock_stat", ret);
	n = sp->st_nlockers;
	if (nobjp != NULL)
		*nobjp = sp->st_objects;
	free(sp);
	return (n);
}

static u_int32_t
mutexes(void)
{
	DB_MUTEX_STAT *sp;
	u_int32_t n;
	int ret;

	if ((ret = env->mutex_stat(env, &sp, 0)) != 0)
		fail("DB_ENV->mutex_stat", ret);
	n = sp->st_mutex_inuse;
	free(sp);
	return (n);
}

int
main(int argc, char *argv[])
{
	DB_TXN *txn;
	DBT key, data;
	u_int32_t base_lk, base_mtx, high_lk, high_mtx, lk, mtx, txn_flags;
	u_int32_t nobj, ceiling;
	u_int32_t early_lk, early_mtx, late_lk, late_mtx;
	char keybuf[] = "key", valbuf[64];
	const char *mode;
	int completed, enomem, i, ret;

	mode = argc > 1 ? argv[1] : "snapshot";
	if (strcmp(mode, "snapshot") != 0 && strcmp(mode, "control") != 0) {
		fprintf(stderr, "usage: %s [snapshot|control]\n", argv[0]);
		return (2);
	}
	txn_flags = strcmp(mode, "control") == 0 ? 0 : DB_TXN_SERIALIZABLE;

	/* The caller is expected to run this in a fresh scratch directory. */
	(void)mkdir(HOME, 0755);

	if ((ret = db_env_create(&env, 0)) != 0)
		fail("db_env_create", ret);
	env->set_errfile(env, stderr);
	env->set_errpfx(env, "leak_si_locker");
	if ((ret = env->set_lk_detect(env, DB_LOCK_DEFAULT)) != 0)
		fail("DB_ENV->set_lk_detect", ret);
	if ((ret = env->open(env, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN, 0600)) != 0)
		fail("DB_ENV->open", ret);

	if ((ret = db_create(&db, env, 0)) != 0)
		fail("db_create", ret);
	if ((ret = db->open(db, NULL, "data.db", NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT | DB_MULTIVERSION, 0600)) != 0)
		fail("DB->open", ret);

	/* Seed the one record every transaction reads. */
	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = keybuf;
	key.size = sizeof(keybuf);
	data.data = valbuf;
	data.size = sizeof(valbuf);
	memset(valbuf, 'v', sizeof(valbuf));
	if ((ret = db->put(db, NULL, &key, &data, 0)) != 0)
		fail("DB->put(seed)", ret);

	high_lk = base_lk = lockers(&nobj);
	high_mtx = base_mtx = mutexes();
	early_lk = early_mtx = late_lk = late_mtx = 0;

	/*
	 * The sawtooth ceiling is ~nobj / SAWTOOTH_DIVISOR; allow CEILING_SLACK
	 * times that (plus the mutex baseline) before we call it a leak.  A
	 * per-txn leak sails past this and eventually ENOMEMs; the bounded
	 * sawtooth never comes close.
	 */
	ceiling = (nobj / SAWTOOTH_DIVISOR) * CEILING_SLACK;
	if (ceiling < 256)			/* tiny regions: floor the bound */
		ceiling = 256;
	printf("baseline lockers=%lu mutexes=%lu nobj=%lu ceiling=%lu "
	    "mode=%s attempts=%d\n", (u_long)base_lk, (u_long)base_mtx,
	    (u_long)nobj, (u_long)ceiling, mode, ATTEMPTS);

	completed = enomem = 0;
	for (i = 0; i < ATTEMPTS; i++) {
		if ((ret = env->txn_begin(env, NULL, &txn, txn_flags)) != 0) {
			if (ret == ENOMEM) {
				printf("ENOMEM at txn_begin after %d txns\n",
				    completed);
				enomem = 1;
				break;
			}
			fail("DB_ENV->txn_begin", ret);
		}
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = keybuf;
		key.size = sizeof(keybuf);
		data.data = valbuf;
		data.ulen = sizeof(valbuf);
		data.flags = DB_DBT_USERMEM;
		if ((ret = db->get(db, txn, &key, &data, 0)) != 0) {
			(void)txn->abort(txn);
			if (ret == ENOMEM) {
				printf("ENOMEM at DB->get after %d txns\n",
				    completed);
				enomem = 1;
				break;
			}
			fail("DB->get", ret);
		}
		if ((ret = txn->commit(txn, 0)) != 0) {
			if (ret == ENOMEM) {
				printf("ENOMEM at commit after %d txns\n",
				    completed);
				enomem = 1;
				break;
			}
			fail("DB_TXN->commit", ret);
		}
		completed++;

		if (completed % SAMPLE_EVERY == 0) {
			lk = lockers(NULL);
			mtx = mutexes();
			if (lk > high_lk)
				high_lk = lk;
			if (mtx > high_mtx)
				high_mtx = mtx;
			/*
			 * Track the peak of an early window (first third) and a
			 * late window (last third).  Each window spans many
			 * sawtooth periods, so each reliably reaches the ceiling;
			 * comparing their peaks is therefore phase-independent.
			 * A bounded sawtooth: late peak == early peak.  A per-txn
			 * leak: late peak strictly (and unboundedly) larger.
			 */
			if (completed <= ATTEMPTS / 3) {
				if (lk > early_lk)
					early_lk = lk;
				if (mtx > early_mtx)
					early_mtx = mtx;
			} else if (completed > (ATTEMPTS * 2) / 3) {
				if (lk > late_lk)
					late_lk = lk;
				if (mtx > late_mtx)
					late_mtx = mtx;
			}
			printf("  after %5d txns: lockers=%lu mutexes=%lu\n",
			    completed, (u_long)lk, (u_long)mtx);
		}
	}

	lk = lockers(NULL);
	mtx = mutexes();
	if (lk > high_lk)
		high_lk = lk;
	if (mtx > high_mtx)
		high_mtx = mtx;
	printf("final completed=%d enomem=%d lockers=%lu->%lu (peak %lu) "
	    "mutexes=%lu->%lu (peak %lu) early/late lockers=%lu/%lu "
	    "mutexes=%lu/%lu\n", completed, enomem,
	    (u_long)base_lk, (u_long)lk, (u_long)high_lk,
	    (u_long)base_mtx, (u_long)mtx, (u_long)high_mtx,
	    (u_long)early_lk, (u_long)late_lk,
	    (u_long)early_mtx, (u_long)late_mtx);

	if ((ret = db->close(db, 0)) != 0)
		fail("DB->close", ret);
	if ((ret = env->close(env, 0)) != 0)
		fail("DB_ENV->close", ret);

	ret = 0;
	if (enomem || completed != ATTEMPTS) {
		fprintf(stderr,
		    "FAIL: %s mode did not complete %d transactions "
		    "(completed %d, enomem %d)\n",
		    mode, ATTEMPTS, completed, enomem);
		ret = 1;
	}
	/*
	 * Hard bound: the peak must stay within the sawtooth ceiling (a
	 * function of the region size).  A per-transaction locker leak grows
	 * past this toward region exhaustion.
	 */
	if (high_lk > ceiling) {
		fprintf(stderr, "FAIL: peak lockers %lu exceeded the sawtooth "
		    "ceiling %lu (nobj/%u x%u) -- committed-reader lockers are "
		    "not being reclaimed\n", (u_long)high_lk, (u_long)ceiling,
		    SAWTOOTH_DIVISOR, CEILING_SLACK);
		ret = 1;
	}
	if (high_mtx > base_mtx + ceiling) {
		fprintf(stderr, "FAIL: peak mutex slots %lu exceeded base %lu "
		    "+ ceiling %lu\n", (u_long)high_mtx, (u_long)base_mtx,
		    (u_long)ceiling);
		ret = 1;
	}
	/*
	 * The leak signature: a peak that grows with the transaction count.
	 * With windows that each span many sawtooth periods, a bounded sawtooth
	 * gives equal peaks; a per-transaction leak makes the late window's
	 * peak strictly larger.  (control mode leaves early/late at 0 -- no
	 * markers ever accumulate -- and this check is trivially satisfied.)
	 */
	if (early_lk != 0 && late_lk > early_lk) {
		fprintf(stderr, "FAIL: peak lockers rose from %lu (early "
		    "window) to %lu (late window) -- still leaking one per "
		    "txn\n", (u_long)early_lk, (u_long)late_lk);
		ret = 1;
	}
	if (early_mtx != 0 && late_mtx > early_mtx) {
		fprintf(stderr, "FAIL: peak mutex slots rose from %lu (early "
		    "window) to %lu (late window)\n",
		    (u_long)early_mtx, (u_long)late_mtx);
		ret = 1;
	}
	printf("%s: mode=%s\n", ret == 0 ? "PASS" : "FAIL", mode);
	return (ret);
}
