/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*
 * dd_stripes -- the deadlock detector must find every real cycle and abort
 * only real participants while lockers are created and freed concurrently on
 * every locker stripe.
 *
 * Two changes in src/lock/lock_deadlock.c are under test.  The detector
 * skips building the waits-for graph when every waiter is on one object and
 * no waiter behind the first also holds it (__dd_no_cycle_possible), and it
 * aborts a victim holding only the victim's locker stripe, where it used to
 * hold all 64, while transactions begin and end on the other stripes.  A
 * wrong short cut hides a real deadlock; a wrong abort corrupts the locker
 * table or aborts a bystander.  This test runs four kinds of thread at once:
 *
 *   cyclers  pairs of threads that build a guaranteed two-transaction cycle:
 *            A locks x then y, B locks y then x, with a barrier between the
 *            two steps.  One of each pair MUST get DB_LOCK_DEADLOCK, or the
 *            pair hangs; the test fails on a hang via a watchdog.
 *   churners begin/commit empty and one-lock transactions as fast as they can,
 *            so lockers appear and vanish on every stripe during each run.
 *   upgraders pairs of threads that both READ the same key, then both try to
 *            write it: a deadlock on ONE object (two READ holders each
 *            waiting to upgrade).  The detector's "all waiters on one
 *            object" short cut must not hide it.
 *   bystanders take one private key and commit.  They are never in a cycle,
 *            so the detector must never abort one ("false victim").
 *
 * Phase 1 runs one upgrade pair alone, so its deadlock is the only wait and
 * only the full graph can find it.  Phase 2 runs everything.  The detector
 * runs on every conflict (DB_LOCK_DEFAULT) and, in phase 2, also from a
 * dedicated thread in a tight loop, to maximise detector runs racing the
 * churn.  PASS requires: every cycle resolved (no watchdog fire), zero
 * bystander aborts, and at least one deadlock found per cycle round.
 *
 * usage: dd_stripes <home> <seconds>
 * Exit 0 = PASS.
 */
#include <sys/types.h>
#include <errno.h>
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <db.h>

#define	NPAIRS		4
#define	NCHURN		8
#define	NBYSTAND	4
#define	NUPGRADE	2

static DB_ENV *env;
static DB *db;			/* churners and bystanders */
static DB *dbx[NPAIRS], *dby[NPAIRS];	/* one file per cycle key */
static volatile int stop;
static volatile unsigned long cycles_done, cycle_deadlocks, false_victims;
static volatile unsigned long churn_txns, bystander_txns, progress;
static volatile unsigned long up_rounds, up_deadlocks, up_progress;

static int
put_key_db(DB *d, DB_TXN *txn, unsigned k)
{
	DBT key, data;
	char v[16];

	memset(&key, 0, sizeof(key));
	memset(&data, 0, sizeof(data));
	key.data = &k;
	key.size = sizeof(k);
	memset(v, 'v', sizeof(v));
	data.data = v;
	data.size = sizeof(v);
	return (d->put(d, txn, &key, &data, 0));
}

#define	put_key(txn, k)	put_key_db(db, txn, k)

/* A pair shares a barrier so both hold their first lock before either asks for its second. */
typedef struct {
	pthread_barrier_t bar;
	int id;
} pair_t;
typedef struct {
	pair_t *p;
	int side;
} cyc_arg;

static void *
cycler(void *a)
{
	cyc_arg *c = a;
	DB_TXN *txn;
	unsigned x, y;
	int ret;

	x = 1000000u + (unsigned)c->p->id * 2;
	y = x + 1;
	while (!stop) {
		if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
			exit(2);
		/* Step 1: each side write-locks its own key (its own file). */
		ret = put_key_db(c->side == 0 ? dbx[c->p->id] : dby[c->p->id],
		    txn, c->side == 0 ? x : y);
		/*
		 * Both sides hold their first key before either asks for the
		 * other's, so step 2 is a guaranteed cycle.  Wait here even if
		 * step 1 failed, so the partner is never left at the barrier.
		 */
		(void)pthread_barrier_wait(&c->p->bar);
		if (ret == 0)
			ret = put_key_db(c->side == 0 ?
			    dby[c->p->id] : dbx[c->p->id],
			    txn, c->side == 0 ? y : x);
		if (ret == 0)
			ret = txn->commit(txn, 0);
		else {
			(void)txn->abort(txn);
			if (ret == DB_LOCK_DEADLOCK)
				__sync_fetch_and_add(&cycle_deadlocks, 1);
			else {
				fprintf(stderr, "cycler: %s\n", db_strerror(ret));
				exit(2);
			}
		}
		/* Both sides finished this round (commit or abort). */
		(void)pthread_barrier_wait(&c->p->bar);
		if (c->side == 0) {
			__sync_fetch_and_add(&cycles_done, 1);
			__sync_fetch_and_add(&progress, 1);
		}
	}
	return (NULL);
}

static DB *dbu[NUPGRADE];	/* one file per upgrade pair */

static void *
upgrader(void *a)
{
	cyc_arg *c = a;
	DB_TXN *txn;
	DBT key, data;
	unsigned k;
	char buf[32];
	int ret;

	k = 4000000u + (unsigned)c->p->id;
	while (!stop) {
		if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
			exit(2);
		memset(&key, 0, sizeof(key));
		memset(&data, 0, sizeof(data));
		key.data = &k;
		key.size = sizeof(k);
		data.data = buf;
		data.ulen = sizeof(buf);
		data.flags = DB_DBT_USERMEM;
		/* Step 1: both sides READ-lock the same key (no DB_RMW). */
		ret = dbu[c->p->id]->get(dbu[c->p->id], txn, &key, &data, 0);
		if (ret == DB_NOTFOUND)
			ret = 0;
		(void)pthread_barrier_wait(&c->p->bar);
		/* Step 2: both try to write it: each waits for the other's READ. */
		if (ret == 0)
			ret = put_key_db(dbu[c->p->id], txn, k);
		if (ret == 0)
			ret = txn->commit(txn, 0);
		else {
			(void)txn->abort(txn);
			if (ret == DB_LOCK_DEADLOCK)
				__sync_fetch_and_add(&up_deadlocks, 1);
			else {
				fprintf(stderr, "upgrader: %s\n", db_strerror(ret));
				exit(2);
			}
		}
		(void)pthread_barrier_wait(&c->p->bar);
		if (c->side == 0) {
			__sync_fetch_and_add(&up_rounds, 1);
			__sync_fetch_and_add(&up_progress, 1);
		}
	}
	return (NULL);
}

static void *
churner(void *a)
{
	DB_TXN *txn;
	unsigned k;
	int ret, i = 0;

	k = 2000000u + (unsigned)(uintptr_t)a * 100000u;
	while (!stop) {
		if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
			exit(2);
		/* Every other txn takes a private lock, so some lockers hold locks mid-walk. */
		if ((i++ & 1) && (ret = put_key(txn, k + (unsigned)(i % 1000))) != 0) {
			(void)txn->abort(txn);
			if (ret != DB_LOCK_DEADLOCK)
				exit(2);
			continue;
		}
		if ((ret = txn->commit(txn, 0)) != 0)
			exit(2);
		__sync_fetch_and_add(&churn_txns, 1);
	}
	return (NULL);
}

static void *
bystander(void *a)
{
	DB_TXN *txn;
	unsigned k;
	int ret;

	k = 3000000u + (unsigned)(uintptr_t)a;
	while (!stop) {
		if ((ret = env->txn_begin(env, NULL, &txn, 0)) != 0)
			exit(2);
		if ((ret = put_key(txn, k)) != 0) {
			(void)txn->abort(txn);
			if (ret == DB_LOCK_DEADLOCK) {
				__sync_fetch_and_add(&false_victims, 1);
				continue;
			}
			exit(2);
		}
		if ((ret = txn->commit(txn, 0)) != 0)
			exit(2);
		__sync_fetch_and_add(&bystander_txns, 1);
	}
	return (NULL);
}

static void *
detector(void *a)
{
	(void)a;
	while (!stop)
		(void)env->lock_detect(env, 0, DB_LOCK_DEFAULT, NULL);
	return (NULL);
}

int
main(int argc, char *argv[])
{
	pthread_t th[NPAIRS * 2 + NUPGRADE * 2 + NCHURN + NBYSTAND + 1];
	pair_t pairs[NPAIRS], upairs[NUPGRADE];
	cyc_arg ca[NPAIRS * 2], ua[NUPGRADE * 2];
	unsigned long last, ulast;
	int i, n, secs, stalls, ret;

	if (argc != 3) {
		fprintf(stderr, "usage: dd_stripes <home> <seconds>\n");
		return (2);
	}
	secs = atoi(argv[2]);
	if ((ret = db_env_create(&env, 0)) != 0)
		return (2);
	env->set_errfile(env, stderr);
	(void)env->set_cachesize(env, 0, 64 * 1024 * 1024, 1);
	(void)env->set_lk_detect(env, DB_LOCK_DEFAULT);
	(void)env->set_lk_max_lockers(env, 20000);
	(void)env->set_lk_max_locks(env, 20000);
	(void)env->set_lk_max_objects(env, 20000);
	(void)env->set_flags(env, DB_TXN_NOSYNC, 1);
	if ((ret = env->open(env, argv[1], DB_CREATE | DB_INIT_MPOOL |
	    DB_INIT_LOCK | DB_INIT_LOG | DB_INIT_TXN | DB_THREAD, 0)) != 0) {
		fprintf(stderr, "env open: %s\n", db_strerror(ret));
		return (2);
	}
	/*
	 * Churners and bystanders share one database with keys far apart.  Each
	 * cycle key has a database of its own (below), so a cycle is exactly two
	 * page locks in two files and nobody else ever waits on them.
	 */
	if ((ret = db_create(&db, env, 0)) != 0 ||
	    (ret = db->set_pagesize(db, 512)) != 0 ||
	    (ret = db->open(db, NULL, "dd.db", NULL, DB_BTREE,
	    DB_CREATE | DB_AUTO_COMMIT | DB_THREAD, 0644)) != 0) {
		fprintf(stderr, "db open: %s\n", db_strerror(ret));
		return (2);
	}

	for (i = 0; i < NPAIRS; i++) {
		char nx[32], ny[32];

		snprintf(nx, sizeof(nx), "x%d.db", i);
		snprintf(ny, sizeof(ny), "y%d.db", i);
		if ((ret = db_create(&dbx[i], env, 0)) != 0 ||
		    (ret = dbx[i]->open(dbx[i], NULL, nx, NULL, DB_BTREE,
		    DB_CREATE | DB_AUTO_COMMIT | DB_THREAD, 0644)) != 0 ||
		    (ret = db_create(&dby[i], env, 0)) != 0 ||
		    (ret = dby[i]->open(dby[i], NULL, ny, NULL, DB_BTREE,
		    DB_CREATE | DB_AUTO_COMMIT | DB_THREAD, 0644)) != 0) {
			fprintf(stderr, "pair db open: %s\n", db_strerror(ret));
			return (2);
		}
	}

	for (i = 0; i < NUPGRADE; i++) {
		char nu[32];

		snprintf(nu, sizeof(nu), "u%d.db", i);
		if ((ret = db_create(&dbu[i], env, 0)) != 0 ||
		    (ret = dbu[i]->open(dbu[i], NULL, nu, NULL, DB_BTREE,
		    DB_CREATE | DB_AUTO_COMMIT | DB_THREAD, 0644)) != 0) {
			fprintf(stderr, "upgrade db open: %s\n", db_strerror(ret));
			return (2);
		}
	}

	n = 0;
	for (i = 0; i < NUPGRADE; i++) {
		upairs[i].id = i;
		pthread_barrier_init(&upairs[i].bar, NULL, 2);
		ua[i * 2].p = ua[i * 2 + 1].p = &upairs[i];
		ua[i * 2].side = 0;
		ua[i * 2 + 1].side = 1;
	}

	/*
	 * Phase 1: one upgrade pair alone.  Its deadlock is then the only
	 * object with waiters: exactly the case the detector's one-object
	 * short cut must still hand to the full graph.  With other waits
	 * about, a full build would find the cycle anyway and hide a broken
	 * short cut.
	 */
	pthread_create(&th[n++], NULL, upgrader, &ua[0]);
	pthread_create(&th[n++], NULL, upgrader, &ua[1]);
	for (i = 0; i < 100 && up_rounds < 100; i++)
		usleep(100000);
	if (up_rounds < 100) {
		printf("VERDICT dd_stripes FAIL: a lone upgrade pair completed "
		    "%lu of 100 rounds in 10 s (a one-object deadlock was not "
		    "detected)\n", up_rounds);
		fflush(stdout);
		_exit(1);
	}
	printf("phase 1: lone upgrade pair, %lu rounds, %lu deadlocks\n",
	    up_rounds, up_deadlocks);

	/* Phase 2: everything at once. */
	for (i = 1; i < NUPGRADE; i++) {
		pthread_create(&th[n++], NULL, upgrader, &ua[i * 2]);
		pthread_create(&th[n++], NULL, upgrader, &ua[i * 2 + 1]);
	}
	for (i = 0; i < NPAIRS; i++) {
		pairs[i].id = i;
		pthread_barrier_init(&pairs[i].bar, NULL, 2);
		ca[i * 2].p = ca[i * 2 + 1].p = &pairs[i];
		ca[i * 2].side = 0;
		ca[i * 2 + 1].side = 1;
		pthread_create(&th[n++], NULL, cycler, &ca[i * 2]);
		pthread_create(&th[n++], NULL, cycler, &ca[i * 2 + 1]);
	}
	for (i = 0; i < NCHURN; i++)
		pthread_create(&th[n++], NULL, churner, (void *)(uintptr_t)i);
	for (i = 0; i < NBYSTAND; i++)
		pthread_create(&th[n++], NULL, bystander, (void *)(uintptr_t)i);
	pthread_create(&th[n++], NULL, detector, NULL);

	/* Watchdog: the cyclers must keep completing rounds. */
	last = ulast = 0;
	stalls = 0;
	for (i = 0; i < secs; i++) {
		sleep(1);
		if ((progress == last || up_progress == ulast) && ++stalls >= 10) {
			DB_LOCK_STAT *ls;

			if (env->lock_stat(env, &ls, 0) == 0) {
				printf("lock stats: deadlocks=%lu waits=%lu "
				    "lockers=%lu\n", (u_long)ls->st_ndeadlocks,
				    (u_long)ls->st_lock_wait,
				    (u_long)ls->st_nlockers);
				free(ls);
			}
			printf("VERDICT dd_stripes FAIL: no cycle round completed in "
			    "10 s (a cycle was never resolved) cycles=%lu "
			    "upgrade_rounds=%lu\n", cycles_done, up_rounds);
			fflush(stdout);
			_exit(1);
		}
		if (progress != last && up_progress != ulast)
			stalls = 0;
		last = progress;
		ulast = up_progress;
	}
	stop = 1;
	/*
	 * A cycler may be waiting at a barrier for a partner that already saw
	 * stop: let the detector and the other side drain it, then exit without
	 * joining the cyclers (their state is checked from the counters).
	 */
	sleep(2);
	printf("cycles=%lu cycle_deadlocks=%lu upgrade_rounds=%lu "
	    "upgrade_deadlocks=%lu false_victims=%lu churn_txns=%lu "
	    "bystander_txns=%lu\n", cycles_done, cycle_deadlocks, up_rounds,
	    up_deadlocks, false_victims, churn_txns, bystander_txns);
	if (false_victims != 0) {
		printf("VERDICT dd_stripes FAIL: %lu bystander transaction(s) "
		    "chosen as deadlock victims\n", false_victims);
		fflush(stdout);
		_exit(1);
	}
	if (up_rounds < 10 || up_deadlocks < up_rounds) {
		printf("VERDICT dd_stripes FAIL: %lu upgrade rounds, %lu "
		    "deadlocks (every round needs one)\n", up_rounds, up_deadlocks);
		fflush(stdout);
		_exit(1);
	}
	if (cycles_done < 10 || cycle_deadlocks < cycles_done) {
		printf("VERDICT dd_stripes FAIL: %lu cycles, %lu deadlocks "
		    "(every cycle needs one)\n", cycles_done, cycle_deadlocks);
		fflush(stdout);
		_exit(1);
	}
	printf("VERDICT dd_stripes PASS %lu cycles and %lu upgrade deadlocks "
	    "resolved, 0 false victims, %lu churn txns\n", cycles_done,
	    up_rounds, churn_txns);
	fflush(stdout);
	_exit(0);
}
