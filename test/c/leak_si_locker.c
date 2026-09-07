/*-
 * See the file LICENSE for redistribution information.
 *
 * leak_si_locker.c -- resource-accounting regression test for the SSI
 * committed-reader locker leak (GitHub issue #137).
 *
 * Runs many *sequential* read-only DB_TXN_SNAPSHOT transactions in one
 * long-lived environment -- one transaction active at a time, no checkpoint,
 * no injected fault -- and asserts through the public statistics APIs that
 * the locker / mutex slot counts stay bounded instead of growing once per
 * transaction, and that DB_ENV->txn_begin never returns ENOMEM.
 *
 * Before the fix: __lock_sicleanup reclaimed a committed reader's obsolete
 * SIREAD markers but never the DB_LOCKER_FREED locker (nor its logical
 * mutex) they were deferring, so st_nlockers grew ~1 per transaction until
 * the mutex region was exhausted.
 *
 * Usage: leak_si_locker [snapshot|control]   (default: snapshot)
 *   snapshot -- DB_TXN_SNAPSHOT (the trigger)
 *   control  -- flags 0 (must be flat both before and after the fix)
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
#define	ATTEMPTS	2500		/* sequential read-only txns */
#define	SAMPLE_EVERY	250

/*
 * Bounds.  The marker sweep is best-effort and triggers when live SIREAD
 * markers pass half the allocated lock objects, so the steady state is a
 * sawtooth, not the baseline -- but its height is a function of the lock
 * region size, never of ATTEMPTS.  Pre-fix the counts track ATTEMPTS (they
 * hit ENOMEM at ~1400); post-fix they plateau far below.  The sharp test is
 * the plateau check (late sample vs. an early one); these are backstops.
 */
#define	MAX_LOCKERS	1000
#define	MAX_MUTEXES_OVER_BASE	1000

static DB_ENV *env;
static DB *db;

static void
fail(const char *op, int ret)
{
	fprintf(stderr, "FAIL %s: %s (%d)\n", op, db_strerror(ret), ret);
	exit(1);
}

static u_int32_t
lockers(void)
{
	DB_LOCK_STAT *sp;
	u_int32_t n;
	int ret;

	if ((ret = env->lock_stat(env, &sp, 0)) != 0)
		fail("DB_ENV->lock_stat", ret);
	n = sp->st_nlockers;
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
	u_int32_t first_lk, first_mtx, second_lk, second_mtx;
	char keybuf[] = "key", valbuf[64];
	const char *mode;
	int completed, enomem, i, ret;

	mode = argc > 1 ? argv[1] : "snapshot";
	if (strcmp(mode, "snapshot") != 0 && strcmp(mode, "control") != 0) {
		fprintf(stderr, "usage: %s [snapshot|control]\n", argv[0]);
		return (2);
	}
	txn_flags = strcmp(mode, "control") == 0 ? 0 : DB_TXN_SNAPSHOT;

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

	high_lk = base_lk = lockers();
	high_mtx = base_mtx = mutexes();
	first_lk = first_mtx = second_lk = second_mtx = 0;
	printf("baseline lockers=%lu mutexes=%lu mode=%s attempts=%d\n",
	    (u_long)base_lk, (u_long)base_mtx, mode, ATTEMPTS);

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
			lk = lockers();
			mtx = mutexes();
			if (lk > high_lk)
				high_lk = lk;
			if (mtx > high_mtx)
				high_mtx = mtx;
			/*
			 * Peak per half of the run.  The sweep is best-effort, so
			 * the steady state is a sawtooth; comparing the two peaks
			 * is phase-independent, while a per-transaction leak makes
			 * the second-half peak strictly larger.
			 */
			if (completed <= ATTEMPTS / 2) {
				if (lk > first_lk)
					first_lk = lk;
				if (mtx > first_mtx)
					first_mtx = mtx;
			} else {
				if (lk > second_lk)
					second_lk = lk;
				if (mtx > second_mtx)
					second_mtx = mtx;
			}
			printf("  after %5d txns: lockers=%lu mutexes=%lu\n",
			    completed, (u_long)lk, (u_long)mtx);
		}
	}

	lk = lockers();
	mtx = mutexes();
	if (lk > high_lk)
		high_lk = lk;
	if (mtx > high_mtx)
		high_mtx = mtx;
	printf("final completed=%d enomem=%d lockers=%lu->%lu (peak %lu) "
	    "mutexes=%lu->%lu (peak %lu) halfpeak lockers=%lu/%lu "
	    "mutexes=%lu/%lu\n", completed, enomem,
	    (u_long)base_lk, (u_long)lk, (u_long)high_lk,
	    (u_long)base_mtx, (u_long)mtx, (u_long)high_mtx,
	    (u_long)first_lk, (u_long)second_lk,
	    (u_long)first_mtx, (u_long)second_mtx);

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
	if (high_lk > MAX_LOCKERS) {
		fprintf(stderr, "FAIL: locker count grew to %lu (limit %lu) "
		    "-- committed-reader lockers are not being reclaimed\n",
		    (u_long)high_lk, (u_long)MAX_LOCKERS);
		ret = 1;
	}
	if (high_mtx > base_mtx + MAX_MUTEXES_OVER_BASE) {
		fprintf(stderr, "FAIL: mutex slots grew to %lu from base %lu "
		    "(limit +%lu)\n", (u_long)high_mtx, (u_long)base_mtx,
		    (u_long)MAX_MUTEXES_OVER_BASE);
		ret = 1;
	}
	/*
	 * The leak signature: counts that track the transaction count.  A
	 * plateau or sawtooth has equal peaks in both halves of the run; a
	 * per-transaction leak makes the second half's peak strictly larger.
	 */
	if (first_lk != 0 && second_lk > first_lk) {
		fprintf(stderr, "FAIL: peak lockers rose from %lu (first half) "
		    "to %lu (second half) -- still leaking one per txn\n",
		    (u_long)first_lk, (u_long)second_lk);
		ret = 1;
	}
	if (first_mtx != 0 && second_mtx > first_mtx) {
		fprintf(stderr, "FAIL: peak mutex slots rose from %lu (first "
		    "half) to %lu (second half)\n",
		    (u_long)first_mtx, (u_long)second_mtx);
		ret = 1;
	}
	printf("%s: mode=%s\n", ret == 0 ? "PASS" : "FAIL", mode);
	return (ret);
}
