/*-
 * See the file LICENSE for redistribution information.
 *
 * health_stats.c -- proves the operator "deployment health" signal TRACKS
 * REALITY for the two historical resource-exhaustion shapes (GitHub issues
 * #137 and #138).
 *
 * The signal is useless if it merely exists.  Two things must hold, and this
 * test asserts both:
 *
 *   (1) IT MOVES.  When the workload retains the resource, the counter rises
 *       measurably above its baseline.  A counter stuck at its baseline (or
 *       wrapped, which is what st_nsnapshot did before this change -- it read
 *       ~4.29e9 because __txn_end's SIREAD park site never incremented what
 *       both reap paths decremented) is not an alarmable signal.
 *
 *   (2) IT COMES BACK DOWN, AND STAYS BOUNDED.  The healthy steady state for
 *       both shapes is a bounded SAWTOOTH, not a plateau and not a ramp: the
 *       reclaimers (__lock_sicleanup for #137, __memp_purge_obsolete at
 *       checkpoint for #138) fire on a trigger, so the population climbs, is
 *       swept, and climbs again.  The ceiling is a function of REGION SIZE,
 *       not of transaction count.  So a late window must reach the same peak
 *       as an early window -- and utilization against the configured maximum
 *       must stay clear of 1.0.
 *
 * Two shapes, selected by argv[1]:
 *   si137  -- sequential read-only DB_TXN_SERIALIZABLE txns.  Each committed
 *             reader leaves SIREAD markers that pin its locker AND its
 *             TXN_DETAIL (st_nlockers, st_nsnapshot, mutex slots).
 *   si138  -- snapshot txns that WRITE a multiversion db while a LONG-LIVED
 *             snapshot reader is held open.  The open reader holds back the
 *             oldest-reader frontier, so the committed writers' details stay
 *             parked on region->mvcc_txn with mvcc_ref > 0 and keep their
 *             mvcc_mtx.  The reader is closed and reopened periodically, so
 *             reclamation is allowed to catch up -- which is what makes the
 *             result a sawtooth.  (Without a held-open reader every version
 *             goes obsolete immediately and the retention never appears; a
 *             "#138 test" without one measures nothing, which is precisely the
 *             trap this comment exists to prevent.)
 *   control -- plain (non-snapshot) txns on a NON-multiversion database: no
 *             SIREAD markers and no MVCC versions, so there is nothing to
 *             retain and the counters must stay flat at their baseline.  This
 *             is what proves the movement seen in the other two modes is the
 *             SSI/MVCC retention under test and not transaction traffic as
 *             such.  (The database must drop DB_MULTIVERSION for this to be a
 *             real control: with it set, even a plain auto-commit writer
 *             creates versions and parks details, so the "control" would
 *             exhibit a muted form of the very thing under test.)
 *
 * The maxima are configured deliberately (set_lk_max_lockers etc.) so that
 * utilization = in-use/max is a real ratio rather than "unlimited", which is
 * exactly what an operator must do to be able to alarm at all -- and this
 * test therefore also documents that requirement.
 *
 * Teeth: neuter __lock_sireap_lockers (or __memp_purge_obsolete) and the
 * bounded-sawtooth assertions below fail; revert the __txn_end STAT_INC and
 * the "signal moved" assertion on st_nsnapshot fails (it wraps instead).
 *
 * Self-bounded and deterministic: fixed txn count, single thread, no faults.
 */
#include <sys/types.h>
#include <sys/stat.h>

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "db.h"

#define	HOME		"TESTDIR_health_stats"
#define	TXNS		6000		/* spans many sawtooth periods */
#define	SAMPLE_EVERY	100
#define	KEYS		256		/* #138 keyspace */
#define	VALUE_BYTES	128
#define	READER_WINDOW	250		/* #138: txns per held-open reader */

/*
 * Configured maxima.  Chosen large enough that a bounded sawtooth never comes
 * near them (so a passing run proves headroom, not luck) yet small enough that
 * a genuine per-transaction leak would drive utilization to 1.0 and ENOMEM
 * well inside TXNS.
 */
#define	MAX_LOCKERS	4000
#define	MAX_OBJECTS	4000
#define	MAX_LOCKS	4000
#define	MAX_TXNS	2000

/*
 * A bounded sawtooth must stay clear of the configured maximum.  0.8 is the
 * threshold the operator playbook tells people to alarm on, so assert the
 * healthy workload stays under it -- that is what makes the documented
 * threshold meaningful rather than arbitrary.
 */
#define	UTIL_ALARM_PCT	80

static DB_ENV *env;
static DB *db;

/* One sample of every resource the health signal covers. */
struct sample {
	u_int32_t nlockers, maxlockers;	/* lock region: locker slots */
	u_int32_t nobjects, maxobjects;	/* lock region: lock-object slots */
	u_int32_t nlocks, maxlocks;	/* lock region: lock slots */
	u_int32_t objects;		/* allocated lock objects (sweep bound) */
	u_int32_t mutex_inuse, mutex_max;/* mutex region */
	u_int32_t nactive, maxtxns;	/* txn region: active slots */
	u_int32_t nsnapshot;		/* retained MVCC/SSI details (#137/#138) */
};

static void
fail(const char *op, int ret)
{
	fprintf(stderr, "FAIL %s: %s (%d)\n", op, db_strerror(ret), ret);
	exit(1);
}

/*
 * Read every counter through the PUBLIC stat APIs -- the same ones an operator
 * scrapes.  Flags are 0: DB_STAT_CLEAR would reset the very peaks we are
 * measuring, and a health read must not perturb what it measures.
 */
static void
take(struct sample *s)
{
	DB_LOCK_STAT *lp;
	DB_MUTEX_STAT *mp;
	DB_TXN_STAT *tp;
	int ret;

	if ((ret = env->lock_stat(env, &lp, 0)) != 0)
		fail("DB_ENV->lock_stat", ret);
	s->nlockers = lp->st_nlockers;
	s->maxlockers = lp->st_maxlockers;
	s->nobjects = lp->st_nobjects;
	s->maxobjects = lp->st_maxobjects;
	s->nlocks = lp->st_nlocks;
	s->maxlocks = lp->st_maxlocks;
	s->objects = lp->st_objects;
	free(lp);

	if ((ret = env->mutex_stat(env, &mp, 0)) != 0)
		fail("DB_ENV->mutex_stat", ret);
	s->mutex_inuse = mp->st_mutex_inuse;
	s->mutex_max = mp->st_mutex_max;
	free(mp);

	if ((ret = env->txn_stat(env, &tp, 0)) != 0)
		fail("DB_ENV->txn_stat", ret);
	s->nactive = tp->st_nactive;
	s->maxtxns = tp->st_maxtxns;
	s->nsnapshot = tp->st_nsnapshot;
	free(tp);
}

/* Integer utilization percent; max == 0 means unlimited, report 0. */
static u_int32_t
util(u_int32_t inuse, u_int32_t max)
{
	return (max == 0 ? 0 : (u_int32_t)(((u_long)inuse * 100) / max));
}

int
main(int argc, char *argv[])
{
	DB_TXN *txn, *reader;
	DBT key, data;
	struct sample s, base, peak, early, late;
	const char *mode;
	u_int32_t txn_flags, dbflags;
	char keybuf[64], databuf[VALUE_BYTES];
	int completed, do_write, enomem, hold_reader, i, ret, sawtooth_down;

	mode = argc > 1 ? argv[1] : "si137";
	if (strcmp(mode, "si137") == 0) {
		txn_flags = DB_TXN_SERIALIZABLE;
		do_write = 0;
		hold_reader = 0;
	} else if (strcmp(mode, "si138") == 0) {
		txn_flags = DB_TXN_SNAPSHOT;
		do_write = 1;
		hold_reader = 1;
	} else if (strcmp(mode, "control") == 0) {
		txn_flags = 0;
		do_write = 1;
		hold_reader = 0;
	} else {
		fprintf(stderr,
		    "usage: %s [si137|si138|control]\n", argv[0]);
		return (2);
	}

	/* The caller is expected to run this in a fresh scratch directory. */
	(void)mkdir(HOME, 0755);

	if ((ret = db_env_create(&env, 0)) != 0)
		fail("db_env_create", ret);
	env->set_errfile(env, stderr);
	env->set_errpfx(env, "health_stats");
	/*
	 * Configure the maxima.  Without these the health lines correctly read
	 * "unlimited" (the resources grow until the region itself is exhausted)
	 * and no ratio exists to alarm on -- so setting them is step one of the
	 * operator playbook, and this test exercises exactly that configuration.
	 */
	if ((ret = env->set_lk_max_lockers(env, MAX_LOCKERS)) != 0)
		fail("DB_ENV->set_lk_max_lockers", ret);
	if ((ret = env->set_lk_max_objects(env, MAX_OBJECTS)) != 0)
		fail("DB_ENV->set_lk_max_objects", ret);
	if ((ret = env->set_lk_max_locks(env, MAX_LOCKS)) != 0)
		fail("DB_ENV->set_lk_max_locks", ret);
	if ((ret = env->set_tx_max(env, MAX_TXNS)) != 0)
		fail("DB_ENV->set_tx_max", ret);
	if ((ret = env->set_lk_detect(env, DB_LOCK_DEFAULT)) != 0)
		fail("DB_ENV->set_lk_detect", ret);
	if ((ret = env->open(env, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN, 0600)) != 0)
		fail("DB_ENV->open", ret);

	if ((ret = db_create(&db, env, 0)) != 0)
		fail("db_create", ret);
	dbflags = DB_CREATE | DB_AUTO_COMMIT;
	/*
	 * DB_MULTIVERSION is what makes writers create MVCC versions at all, so
	 * the control deliberately omits it (see the header comment).
	 */
	if (strcmp(mode, "control") != 0)
		dbflags |= DB_MULTIVERSION;
	if ((ret = db->open(db,
	    NULL, "data.db", NULL, DB_BTREE, dbflags, 0600)) != 0)
		fail("DB->open", ret);

	/* Seed the keyspace. */
	memset(databuf, 'v', sizeof(databuf));
	for (i = 0; i < KEYS; i++) {
		(void)snprintf(keybuf, sizeof(keybuf), "key%06d", i);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = keybuf;
		key.size = (u_int32_t)strlen(keybuf);
		data.data = databuf;
		data.size = sizeof(databuf);
		if ((ret = db->put(db, NULL, &key, &data, 0)) != 0)
			fail("DB->put(seed)", ret);
	}

	take(&base);
	peak = early = base;
	memset(&late, 0, sizeof(late));
	sawtooth_down = 0;

	printf("mode=%s txns=%d\n", mode, TXNS);
	printf("baseline: lockers=%lu/%lu objects=%lu/%lu locks=%lu/%lu "
	    "mutexes=%lu/%lu active=%lu/%lu retained=%lu allocobj=%lu\n",
	    (u_long)base.nlockers, (u_long)base.maxlockers,
	    (u_long)base.nobjects, (u_long)base.maxobjects,
	    (u_long)base.nlocks, (u_long)base.maxlocks,
	    (u_long)base.mutex_inuse, (u_long)base.mutex_max,
	    (u_long)base.nactive, (u_long)base.maxtxns,
	    (u_long)base.nsnapshot, (u_long)base.objects);

	completed = enomem = 0;
	reader = NULL;
	for (i = 0; i < TXNS; i++) {
		/*
		 * #138: hold a snapshot reader open across a window of writes.
		 * While it is open the oldest-reader frontier cannot advance past
		 * its snapshot, so the committed writers' versions are not yet
		 * obsolete and their details stay parked (retaining mvcc_mtx).
		 * Closing it lets the checkpoint purge reclaim them -- the down
		 * stroke of the sawtooth.
		 */
		if (hold_reader && completed % READER_WINDOW == 0) {
			if (reader != NULL &&
			    (ret = reader->commit(reader, 0)) != 0)
				fail("DB_TXN->commit(reader)", ret);
			reader = NULL;
			if ((ret = env->txn_begin(env,
			    NULL, &reader, DB_TXN_SNAPSHOT)) != 0)
				fail("DB_ENV->txn_begin(reader)", ret);
			(void)snprintf(keybuf, sizeof(keybuf), "key%06d", 0);
			memset(&key, 0, sizeof(key));
			memset(&data, 0, sizeof(data));
			key.data = keybuf;
			key.size = (u_int32_t)strlen(keybuf);
			data.data = databuf;
			data.ulen = sizeof(databuf);
			data.flags = DB_DBT_USERMEM;
			/* Touch a page so the snapshot is materialized. */
			if ((ret = db->get(db, reader, &key, &data, 0)) != 0)
				fail("DB->get(reader)", ret);
		}
		if ((ret = env->txn_begin(env, NULL, &txn, txn_flags)) != 0) {
			if (ret == ENOMEM) {
				printf("ENOMEM at txn_begin after %d txns\n",
				    completed);
				enomem = 1;
				break;
			}
			fail("DB_ENV->txn_begin", ret);
		}
		(void)snprintf(keybuf, sizeof(keybuf),
		    "key%06d", i % KEYS);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = keybuf;
		key.size = (u_int32_t)strlen(keybuf);
		if (do_write) {
			/*
			 * #138 shape: a snapshot txn that WRITES creates MVCC
			 * version buffers, so its committed detail is parked on
			 * mvcc_txn with mvcc_ref > 0 and keeps its mvcc_mtx.
			 */
			data.data = databuf;
			data.size = sizeof(databuf);
			ret = db->put(db, txn, &key, &data, 0);
		} else {
			/*
			 * #137 shape: a read under SSI leaves a SIREAD marker
			 * that outlives the commit and pins locker + detail.
			 */
			data.data = databuf;
			data.ulen = sizeof(databuf);
			data.flags = DB_DBT_USERMEM;
			ret = db->get(db, txn, &key, &data, 0);
		}
		if (ret != 0) {
			(void)txn->abort(txn);
			if (ret == ENOMEM) {
				printf("ENOMEM at op after %d txns\n",
				    completed);
				enomem = 1;
				break;
			}
			/* A conflict is legitimate; it just is not a sample. */
			if (ret == DB_LOCK_DEADLOCK ||
			    ret == DB_SNAPSHOT_CONFLICT ||
			    ret == DB_SNAPSHOT_UNSAFE)
				continue;
			fail(do_write ? "DB->put" : "DB->get", ret);
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

		/*
		 * #138's reclaimer runs at checkpoint (__memp_purge_obsolete),
		 * so a workload that never checkpoints is not the shape an
		 * operator runs.  Checkpoint periodically so the sawtooth we
		 * assert on is the real steady state.
		 */
		if (do_write && completed % 500 == 0 &&
		    (ret = env->txn_checkpoint(env, 0, 0, 0)) != 0)
			fail("DB_ENV->txn_checkpoint", ret);

		if (completed % SAMPLE_EVERY != 0)
			continue;

		take(&s);
		if (s.nlockers > peak.nlockers)
			peak.nlockers = s.nlockers;
		if (s.mutex_inuse > peak.mutex_inuse)
			peak.mutex_inuse = s.mutex_inuse;
		if (s.nsnapshot > peak.nsnapshot)
			peak.nsnapshot = s.nsnapshot;
		if (s.nobjects > peak.nobjects)
			peak.nobjects = s.nobjects;
		if (s.nlocks > peak.nlocks)
			peak.nlocks = s.nlocks;
		/*
		 * Sawtooth evidence: at least once, a sample must be LOWER than
		 * the running peak.  A monotone ramp (a real leak) never is.
		 */
		if (s.nsnapshot < peak.nsnapshot ||
		    s.nlockers < peak.nlockers)
			sawtooth_down = 1;

		/* Early / late window peaks -- each spans many periods. */
		if (completed <= TXNS / 3) {
			if (s.nlockers > early.nlockers)
				early.nlockers = s.nlockers;
			if (s.nsnapshot > early.nsnapshot)
				early.nsnapshot = s.nsnapshot;
			if (s.mutex_inuse > early.mutex_inuse)
				early.mutex_inuse = s.mutex_inuse;
		} else if (completed > (TXNS * 2) / 3) {
			if (s.nlockers > late.nlockers)
				late.nlockers = s.nlockers;
			if (s.nsnapshot > late.nsnapshot)
				late.nsnapshot = s.nsnapshot;
			if (s.mutex_inuse > late.mutex_inuse)
				late.mutex_inuse = s.mutex_inuse;
		}

		if (completed % (SAMPLE_EVERY * 5) == 0)
			printf("  after %5d txns: lockers=%lu (%lu%%) "
			    "objects=%lu (%lu%%) locks=%lu (%lu%%) "
			    "mutexes=%lu retained=%lu (%lu%%)\n",
			    completed,
			    (u_long)s.nlockers,
			    (u_long)util(s.nlockers, s.maxlockers),
			    (u_long)s.nobjects,
			    (u_long)util(s.nobjects, s.maxobjects),
			    (u_long)s.nlocks,
			    (u_long)util(s.nlocks, s.maxlocks),
			    (u_long)s.mutex_inuse,
			    (u_long)s.nsnapshot,
			    (u_long)util(s.nsnapshot, s.maxtxns));
	}

	take(&s);
	if (reader != NULL && (ret = reader->commit(reader, 0)) != 0)
		fail("DB_TXN->commit(reader, final)", ret);
	printf("final: completed=%d enomem=%d sawtooth_down=%d\n"
	    "  peak   lockers=%lu retained=%lu mutexes=%lu\n"
	    "  early  lockers=%lu retained=%lu mutexes=%lu\n"
	    "  late   lockers=%lu retained=%lu mutexes=%lu\n"
	    "  peak utilization: lockers=%lu%% objects=%lu%% locks=%lu%% "
	    "retained=%lu%%\n",
	    completed, enomem, sawtooth_down,
	    (u_long)peak.nlockers, (u_long)peak.nsnapshot,
	    (u_long)peak.mutex_inuse,
	    (u_long)early.nlockers, (u_long)early.nsnapshot,
	    (u_long)early.mutex_inuse,
	    (u_long)late.nlockers, (u_long)late.nsnapshot,
	    (u_long)late.mutex_inuse,
	    (u_long)util(peak.nlockers, s.maxlockers),
	    (u_long)util(peak.nobjects, s.maxobjects),
	    (u_long)util(peak.nlocks, s.maxlocks),
	    (u_long)util(peak.nsnapshot, s.maxtxns));

	/* Show the operator-facing lines for this environment. */
	printf("--- DB_ENV->lock_stat_print health lines:\n");
	if ((ret = env->lock_stat_print(env, 0)) != 0)
		fail("DB_ENV->lock_stat_print", ret);
	printf("--- DB_ENV->txn_stat_print health lines:\n");
	if ((ret = env->txn_stat_print(env, 0)) != 0)
		fail("DB_ENV->txn_stat_print", ret);
	printf("--- DB_ENV->mutex_stat_print health lines:\n");
	if ((ret = env->mutex_stat_print(env, 0)) != 0)
		fail("DB_ENV->mutex_stat_print", ret);

	if ((ret = db->close(db, 0)) != 0)
		fail("DB->close", ret);
	if ((ret = env->close(env, 0)) != 0)
		fail("DB_ENV->close", ret);

	ret = 0;
	if (enomem || completed == 0) {
		fprintf(stderr, "FAIL: %s did not run to completion "
		    "(completed %d of %d, enomem %d)\n",
		    mode, completed, TXNS, enomem);
		ret = 1;
	}

	/*
	 * The maxima must have been reported.  If they read 0 the utilization
	 * an operator computes is meaningless, so this is part of the signal.
	 */
	if (s.maxlockers == 0 || s.maxobjects == 0 ||
	    s.maxlocks == 0 || s.maxtxns == 0) {
		fprintf(stderr, "FAIL: a configured maximum read back as 0 "
		    "(lockers=%lu objects=%lu locks=%lu txns=%lu) -- "
		    "utilization cannot be computed\n",
		    (u_long)s.maxlockers, (u_long)s.maxobjects,
		    (u_long)s.maxlocks, (u_long)s.maxtxns);
		ret = 1;
	}

	if (strcmp(mode, "control") == 0) {
		/*
		 * Control: no multiversion database and no snapshot isolation, so
		 * no SIREAD marker and no MVCC version exists to retain.  The
		 * retention counters must therefore be flat -- identical to the
		 * baseline after 6000 transactions.  Any movement here would mean
		 * the signal counts transaction traffic rather than retention,
		 * i.e. that it is not the signal it claims to be.
		 */
		if (peak.nsnapshot != base.nsnapshot) {
			fprintf(stderr, "FAIL: control moved the retained-detail "
			    "counter from %lu (baseline) to %lu -- with neither "
			    "DB_MULTIVERSION nor snapshot isolation there is "
			    "nothing to retain, so the counter is not measuring "
			    "retention\n",
			    (u_long)base.nsnapshot, (u_long)peak.nsnapshot);
			ret = 1;
		}
		if (peak.nlockers > base.nlockers) {
			fprintf(stderr, "FAIL: control mode grew locker count from "
			    "%lu (baseline) to %lu\n",
			    (u_long)base.nlockers, (u_long)peak.nlockers);
			ret = 1;
		}
		printf("%s: mode=%s\n", ret == 0 ? "PASS" : "FAIL", mode);
		return (ret);
	}

	/* (1) The signal MOVED: it must rise measurably above baseline. */
	if (peak.nsnapshot <= base.nsnapshot) {
		fprintf(stderr, "FAIL: retained-detail counter did not move "
		    "(baseline %lu, peak %lu) -- the signal does not track the "
		    "retention it is supposed to report\n",
		    (u_long)base.nsnapshot, (u_long)peak.nsnapshot);
		ret = 1;
	}
	/*
	 * ...and it must be a plausible count, not a wrapped one.  Before the
	 * __txn_end STAT_INC fix this counter underflowed and read ~4.29e9.
	 */
	if (peak.nsnapshot > (u_int32_t)TXNS) {
		fprintf(stderr, "FAIL: retained-detail counter reads %lu, "
		    "more than the %d transactions run -- the counter is "
		    "wrapped or double-counted, not a usable signal\n",
		    (u_long)peak.nsnapshot, TXNS);
		ret = 1;
	}
	if (mode[4] == '7' && peak.nlockers <= base.nlockers) {
		fprintf(stderr, "FAIL: locker count did not move (baseline "
		    "%lu, peak %lu) -- #137 retention not observable\n",
		    (u_long)base.nlockers, (u_long)peak.nlockers);
		ret = 1;
	}

	/* (2a) It came back down: a sawtooth, not a plateau or a ramp. */
	if (!sawtooth_down) {
		fprintf(stderr, "FAIL: no sample ever fell below the running "
		    "peak -- the population is not being reclaimed (a ramp, "
		    "not the bounded sawtooth the signal should show)\n");
		ret = 1;
	}

	/* (2b) Bounded: the late window must not out-peak the early one. */
	if (early.nlockers != 0 && late.nlockers > early.nlockers) {
		fprintf(stderr, "FAIL: peak lockers rose from %lu (early) to "
		    "%lu (late) -- the ceiling grows with txn count, so the "
		    "signal is tracking a leak, not a bounded sawtooth\n",
		    (u_long)early.nlockers, (u_long)late.nlockers);
		ret = 1;
	}
	if (early.nsnapshot != 0 && late.nsnapshot > early.nsnapshot) {
		fprintf(stderr, "FAIL: peak retained details rose from %lu "
		    "(early) to %lu (late)\n",
		    (u_long)early.nsnapshot, (u_long)late.nsnapshot);
		ret = 1;
	}
	if (early.mutex_inuse != 0 && late.mutex_inuse > early.mutex_inuse) {
		fprintf(stderr, "FAIL: peak mutex slots rose from %lu (early) "
		    "to %lu (late)\n",
		    (u_long)early.mutex_inuse, (u_long)late.mutex_inuse);
		ret = 1;
	}

	/*
	 * (2c) Utilization stayed clear of the documented alarm threshold.
	 * This is the assertion that makes the playbook's 0.8 meaningful: a
	 * healthy bounded workload must sit well below it, so crossing it
	 * really does mean something changed.
	 */
	if (util(peak.nlockers, s.maxlockers) >= UTIL_ALARM_PCT ||
	    util(peak.nobjects, s.maxobjects) >= UTIL_ALARM_PCT ||
	    util(peak.nlocks, s.maxlocks) >= UTIL_ALARM_PCT ||
	    util(peak.nsnapshot, s.maxtxns) >= UTIL_ALARM_PCT) {
		fprintf(stderr, "FAIL: peak utilization reached the %d%% alarm "
		    "threshold (lockers=%lu%% objects=%lu%% locks=%lu%% "
		    "retained=%lu%%) on a workload that is supposed to be "
		    "bounded\n", UTIL_ALARM_PCT,
		    (u_long)util(peak.nlockers, s.maxlockers),
		    (u_long)util(peak.nobjects, s.maxobjects),
		    (u_long)util(peak.nlocks, s.maxlocks),
		    (u_long)util(peak.nsnapshot, s.maxtxns));
		ret = 1;
	}

	printf("%s: mode=%s\n", ret == 0 ? "PASS" : "FAIL", mode);
	return (ret);
}
