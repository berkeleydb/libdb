/*-
 * Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * See the file LICENSE for redistribution information.
 */

/*-
 * A4 (REVIEW-2026-11 Sec 1.5): pin the atomicity contract of DB_ENV->lock_vec
 * now that the lock region is no longer a single mutex.
 *
 * Berkeley DB's lock-manager design note (sources/historical/lock-Design.txt,
 * Sleepycat bug #553): with fine-grained locking "lock_vec will no longer be
 * atomic.  We still execute the items in a lock_vec in order, so things like
 * lock-coupling still work, but you can't make any guarantees about atomicity."
 * libdb's default is fine-grained: lk_partitions = 10 * ncpu (lock_method.c),
 * so LOCK_SYSTEM_LOCK is a no-op (lock.h) and a vector holds no region mutex
 * across its elements; another locker can interpose in the inter-element gap.
 *
 * This test makes that a checkable property using a CONFIG_TEST-only hook
 * (DB_TEST_LOCKVEC_GAP) that yields ~50 ms between __lock_vec elements when the
 * caller has set test_abort = DB_TEST_LOCKVEC_GAP.  The hook is compiled out of
 * every non-test build and fires only for this test's env.
 *
 * Worker thread: test_abort = GAP, then submits a NON-BLOCKING vector
 *   [PUT A, GET C] (C is a free object, so GET C does not block).  The hook
 *   yields after PUT A, while the worker is still inside the one lock_vec call.
 * Probe thread: a DIFFERENT locker tries to acquire A, NOWAIT, during that gap,
 *   and records whether the worker's vector had returned yet (vec_done).
 *
 * The outcome isolates the inter-element window exactly:
 *   HOOK ARMED (the normal build): the gap-yield fires after PUT A, so the
 *     probe acquires A *while the worker is still inside lock_vec*
 *     (during_vec == 1) -> the vector is observably NOT atomic across its
 *     elements.  THIS IS THE EXPECTED PASS.
 *   HOOK NOT ARMED (-DLOCKVEC_NO_HOOK, the sabotage build): there is no
 *     yield, so the real inter-element window is microseconds and the probe
 *     cannot catch it -- it only gets A AFTER the vector returns
 *     (during_vec == 0).  Scenario 1 then FAILS.  That failure is the teeth:
 *     it proves the normal PASS depends on actually catching the inter-element
 *     gap (via the hook) and is not an artifact of timing -- a future change
 *     that held a lock across the whole vector would likewise be caught,
 *     because the probe could no longer acquire A mid-vector even with the
 *     hook armed.
 *
 * Scenario 2 (coupling, unconditional): the worker submits the [GET C, PUT A]
 * couple __db_lget uses and the probe confirms A is HELD until the vector ends,
 * regardless of partition count -- lock coupling keeps its order guarantee.
 */
#include "db_config.h"

#include "db_int.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

/* DB_TEST_LOCKVEC_GAP comes from db_int.h (src/dbinc/db_int.in). */
#define	LOCKVEC_GAP_SLOT	DB_TEST_LOCKVEC_GAP

static int failures = 0;
static int checks = 0;

#define CHECK(cond, ...) do {						\
	checks++;							\
	if (!(cond)) {							\
		failures++;						\
		printf("  FAIL: ");					\
		printf(__VA_ARGS__);					\
		printf("\n");						\
	}								\
} while (0)

#define OBJ_A	"OBJECT-A"
#define OBJ_C	"OBJECT-C"

static DB_ENV *dbenv;
static pthread_barrier_t bar;

struct work {
	u_int32_t	locker;		/* worker's own locker id */
	DB_LOCK		lock_a;		/* worker's lock on A (held on entry) */
	int		coupling;	/* 0 = scenario 1, 1 = scenario 2 */
	volatile int	vec_done;	/* set after lock_vec returns */
	int		vec_ret;
	DB_LOCK		got_c;
};

static void
mkobj(DBT *dbt, const char *s)
{
	memset(dbt, 0, sizeof(*dbt));
	dbt->data = (void *)s;
	dbt->size = (u_int32_t)strlen(s);
}

/*
 * The worker submits a non-blocking two-element vector.  C is a free object so
 * GET C never blocks; the only pause is the gap hook between PUT A and GET C
 * (scenario 1) or between GET C and PUT A (scenario 2).
 */
static void *
worker(void *arg)
{
	struct work *w = arg;
	DB_LOCKREQ req[2];
	DBT obj_a, obj_c;

	mkobj(&obj_a, OBJ_A);
	mkobj(&obj_c, OBJ_C);
	memset(req, 0, sizeof(req));

	if (w->coupling) {
		/* [GET C, PUT A]: lock coupling -- new lock before old drop. */
		req[0].op = DB_LOCK_GET;
		req[0].obj = &obj_c;
		req[0].mode = DB_LOCK_WRITE;
		req[1].op = DB_LOCK_PUT;
		req[1].lock = w->lock_a;
	} else {
		/* [PUT A, GET C]: old drop first -> A free during the gap. */
		req[0].op = DB_LOCK_PUT;
		req[0].lock = w->lock_a;
		req[1].op = DB_LOCK_GET;
		req[1].obj = &obj_c;
		req[1].mode = DB_LOCK_WRITE;
	}

	/* Arm the between-elements yield for this thread's calls. */
	(void)pthread_barrier_wait(&bar);

	w->vec_ret = dbenv->lock_vec(dbenv, w->locker, 0, req, 2, NULL);
	w->vec_done = 1;
	if (w->vec_ret == 0)
		w->got_c = req[w->coupling ? 0 : 1].lock;
	return (NULL);
}

/*
 * Probe A, NOWAIT, retrying until it succeeds or a deadline passes.  Returns
 * 0 if A was acquired (sets *during_vec = whether the worker's vector was still
 * running at the moment of success), DB_LOCK_NOTGRANTED if A stayed held the
 * whole time, or a real error.
 */
static int
probe_a(u_int32_t locker, DB_LOCK *lockp, const volatile int *vec_done,
    int *during_vec)
{
	DBT obj_a;
	struct timespec ts;
	int ret, i;

	mkobj(&obj_a, OBJ_A);
	*during_vec = 0;
	for (i = 0; i < 4000; i++) {		/* up to ~4s */
		ret = dbenv->lock_get(dbenv, locker, DB_LOCK_NOWAIT,
		    &obj_a, DB_LOCK_WRITE, lockp);
		if (ret == 0) {
			*during_vec = (*vec_done == 0);
			return (0);
		}
		if (ret != DB_LOCK_NOTGRANTED && ret != DB_LOCK_DEADLOCK)
			return (ret);
		ts.tv_sec = 0;
		ts.tv_nsec = 1000000;		/* 1 ms */
		(void)nanosleep(&ts, NULL);
	}
	return (DB_LOCK_NOTGRANTED);
}

static void
run_scenario(int coupling, int no_hook)
{
	struct work w;
	pthread_t tid;
	DBT obj_a;
	DB_LOCK probe;
	u_int32_t probe_locker;
	int ret, got_a, during_vec;

	memset(&w, 0, sizeof(w));
	w.coupling = coupling;
	mkobj(&obj_a, OBJ_A);

	if ((ret = dbenv->lock_id(dbenv, &w.locker)) != 0 ||
	    (ret = dbenv->lock_id(dbenv, &probe_locker)) != 0) {
		fprintf(stderr, "lock_id: %s\n", db_strerror(ret));
		failures++;
		return;
	}

	/* Worker holds A (write) on entry. */
	if ((ret = dbenv->lock_get(dbenv, w.locker, 0, &obj_a,
	    DB_LOCK_WRITE, &w.lock_a)) != 0) {
		fprintf(stderr, "worker GET A: %s\n", db_strerror(ret));
		failures++;
		return;
	}

	/*
	 * Arm the inter-element gap yield, env-wide, for the duration -- unless
	 * this is the sabotage build, which omits it to show the test depends on
	 * the hook to catch the real (microsecond) inter-element window.
	 */
#ifndef LOCKVEC_NO_HOOK
	dbenv->env->test_abort = LOCKVEC_GAP_SLOT;
#endif

	if ((ret = pthread_create(&tid, NULL, worker, &w)) != 0) {
		fprintf(stderr, "pthread_create: %s\n", strerror(ret));
		failures++;
		return;
	}

	(void)pthread_barrier_wait(&bar);	/* worker about to enter lock_vec */

	got_a = (probe_a(probe_locker, &probe, &w.vec_done, &during_vec) == 0);

	(void)pthread_join(tid, NULL);
	/* Disarm the hook now that the worker is done. */
	dbenv->env->test_abort = 0;

	if (coupling) {
		/*
		 * Lock coupling: A stays held until the vector completes, so
		 * the probe must NOT acquire A during the vector.
		 */
		CHECK(!(got_a && during_vec),
		    "coupling [GET C, PUT A]: A was grabbable mid-vector -- "
		    "the coupling locker was lock-less");
		printf("  scenario 2 (coupling [GET C,PUT A]): A %s during "
		    "vector%s\n",
		    (got_a && during_vec) ? "FREE" : "HELD",
		    got_a ? "" : " (A never acquired)");
	} else if (no_hook) {
		/*
		 * Sabotage: the gap hook is not armed, so the real inter-element
		 * window is microseconds and the probe cannot catch it; it gets A
		 * only AFTER the vector returns (during_vec == 0).  If during_vec
		 * were 1 here the "observation" would be luck, and the normal
		 * PASS would have no teeth.
		 */
		CHECK(got_a && !during_vec,
		    "NO_HOOK [PUT A, GET C]: without the gap hook the probe should"
		    " NOT catch A mid-vector, but A was %s during the vector"
		    " (got_a=%d during_vec=%d)",
		    during_vec ? "FREE" : "HELD", got_a, during_vec);
		printf("  scenario 1 (NO_HOOK): A %s during vector "
		    "(expected HELD -- gap not observable without the hook)\n",
		    during_vec ? "FREE" : "HELD");
	} else {
		/*
		 * Default: the gap hook yields after element 0 (PUT A), so the
		 * probe acquires A WHILE the worker is still inside lock_vec:
		 * during_vec == 1.  This is the documented non-atomicity made
		 * observable.
		 */
		CHECK(got_a && during_vec,
		    "[PUT A, GET C]: expected A free mid-vector (non-atomic) "
		    "but got_a=%d during_vec=%d", got_a, during_vec);
		printf("  scenario 1 (non-atomic [PUT A,GET C]): A %s during "
		    "vector (expected FREE)\n", during_vec ? "FREE" : "HELD");
	}

	if (got_a)
		(void)dbenv->lock_put(dbenv, &probe);
	CHECK(w.vec_ret == 0, "%s vector returned %d (%s)",
	    coupling ? "coupling" : "non-atomic", w.vec_ret,
	    db_strerror(w.vec_ret));
	if (w.vec_ret == 0)
		(void)dbenv->lock_put(dbenv, &w.got_c);
}

int
main(int argc, char *argv[])
{
	const char *home = "LOCKVEC_ATOMICITY_TESTDIR";
	u_int32_t parts;
	int no_hook, ret;

	(void)argc; (void)argv;

	if ((ret = db_env_create(&dbenv, 0)) != 0) {
		fprintf(stderr, "db_env_create: %s\n", db_strerror(ret));
		return (EXIT_FAILURE);
	}
	dbenv->set_errpfx(dbenv, "lockvec_atomicity");
	(void)mkdir(home, 0700);

	/*
	 * Always run at the default fine-grained partition count: that is the
	 * configuration the audit is about.  The sabotage build differs only in
	 * that it does not arm the inter-element gap hook (LOCKVEC_NO_HOOK).
	 */
	parts = 64;
#ifdef LOCKVEC_NO_HOOK
	no_hook = 1;
#else
	no_hook = 0;
#endif
	if ((ret = dbenv->set_lk_partitions(dbenv, parts)) != 0) {
		fprintf(stderr, "set_lk_partitions(%u): %s\n",
		    parts, db_strerror(ret));
		return (EXIT_FAILURE);
	}
	if ((ret = dbenv->open(dbenv, home, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_MPOOL, 0600)) != 0) {
		fprintf(stderr, "env open %s: %s\n", home, db_strerror(ret));
		return (EXIT_FAILURE);
	}
	printf("lockvec_atomicity: lk_partitions=%u%s\n", parts,
	    no_hook ? " (NO_HOOK sabotage)" : "");

	if (pthread_barrier_init(&bar, NULL, 2) != 0) {
		fprintf(stderr, "barrier_init failed\n");
		return (EXIT_FAILURE);
	}
	run_scenario(0, no_hook);		/* atomicity contract */
	if (pthread_barrier_destroy(&bar) != 0 ||
	    pthread_barrier_init(&bar, NULL, 2) != 0) {
		fprintf(stderr, "barrier reinit failed\n");
		return (EXIT_FAILURE);
	}
	run_scenario(1, no_hook);		/* coupling order */
	(void)pthread_barrier_destroy(&bar);
	(void)dbenv->close(dbenv, 0);

	printf("lockvec_atomicity: %d checks, %d failures\n", checks, failures);
	printf("lockvec_atomicity: %s\n", failures == 0 ? "PASS" : "FAIL");
	return (failures == 0 ? EXIT_SUCCESS : EXIT_FAILURE);
}
