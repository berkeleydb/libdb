/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_clockskew_backward.c --
 *	A pure BACKWARD clock jump mid-run: the dangerous case.
 *
 *	The clock jumps only backward (never forward) partway through a
 *	txn-timeout wait.  This is the exact FoundationDB clock-skew concern:
 *	`deadline = now + timeout` is set, then the clock jumps back below
 *	`now`, so later reads of `now2` stay < deadline for a while.  The jump
 *	is TRANSIENT (bounded via __db_sim_clock_settle_after): after the
 *	disturbance the clock resumes advancing.  A correct engine must fire
 *	the timeout once the recovered clock crosses the fixed deadline; a
 *	broken engine that RE-ARMS the deadline off the jumped-back clock, or
 *	that trusts a monotonic assumption, would lose the timeout FOREVER
 *	even after the clock recovers -- and the wall-clock alarm catches it.
 *
 *	(A PERSISTENT adversary clock that never advances past the deadline
 *	could never fire ANY deadline-based timeout -- that is physics, not a
 *	libdb bug -- so this scenario uses a bounded, recovering jump.)
 *
 *	Uses a TXN timeout (DB_SET_TXN_TIMEOUT) via a real DB_TXN + a lock
 *	contended between the txn and a helper-held lock, so the txn's wait is
 *	bounded by tx_expire (set from the skewed clock).  Also asserts
 *	determinism: two runs of the same seed apply the identical number of
 *	skews (same skew sequence) and reach the identical verdict.
 *
 *	Guarded by a hard wall-clock alarm: a LOST timeout hangs the waiter,
 *	and the alarm reports it as a REAL BUG with the seed, rather than
 *	wedging CI.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_clockskew_backward && ./test_sim_clockskew_backward [seed]
 */

#include <sys/types.h>

#include <errno.h>
#include <pthread.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include "db.h"
#include "sim_rng.h"
#include "sim_fault.h"
#include "sim_clock.h"

#define HOME       "TESTDIR_sim_clockskew_backward"
#define OBJNAME    "contended"
#define TXN_TMOUT  60000        /* 60 ms txn timeout (microseconds) */
#define WALL_LIMIT 20

static DB_ENV  *g_env;
static u_int32_t g_holder_id;
static DB_TXN  *g_waiter_txn;
static volatile int g_helper_ret = -12345;
static volatile int g_helper_done;
static uint64_t g_seed;

static void
on_alarm(sig)
	int sig;
{
	(void)sig;
	fprintf(stderr, "test_sim_clockskew_backward: FAIL -- HUNG: a txn "
	    "timeout was LOST after a BACKWARD clock jump (never woke within "
	    "%ds); REAL BUG: backward jump defeats the deadline.  Reproduce: "
	    "./test_sim_clockskew_backward 0x%llx\n",
	    WALL_LIMIT, (unsigned long long)g_seed);
	_exit(3);
}

static void *
helper(arg)
	void *arg;
{
	DB_LOCKREQ req;
	DBT obj;
	u_int32_t wid;
	int ret;

	(void)arg;
	/* The waiter txn's locker id drives the txn timeout. */
	wid = g_waiter_txn->id(g_waiter_txn);
	memset(&obj, 0, sizeof(obj));
	obj.data = (void *)OBJNAME;
	obj.size = (u_int32_t)strlen(OBJNAME) + 1;
	memset(&req, 0, sizeof(req));
	req.op = DB_LOCK_GET;
	req.mode = DB_LOCK_WRITE;
	req.obj = &obj;

	/* Blocks until the txn's tx_expire deadline expires it. */
	ret = g_env->lock_vec(g_env, wid, 0, &req, 1, NULL);
	if (ret == 0)
		(void)g_env->lock_put(g_env, &req.lock);
	g_helper_ret = ret;
	g_helper_done = 1;
	return (NULL);
}

/* One full run at a given seed; returns 0 on PASS, sets skews + verdict. */
static int
run_once(seed, skews, verdict)
	uint64_t seed;
	unsigned long *skews;
	int *verdict;
{
	DB_LOCK holder_lock;
	DBT obj;
	pthread_t tid;
	struct timespec nap;
	int ret, i, rejected;

	g_helper_ret = -12345;
	g_helper_done = 0;

	if ((ret = db_env_create(&g_env, 0)) != 0)
		return (ret);
	if ((ret = g_env->open(g_env, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_MPOOL | DB_INIT_TXN, 0664)) != 0)
		return (ret);
	(void)g_env->set_lk_detect(g_env, DB_LOCK_EXPIRE);

	if ((ret = g_env->lock_id(g_env, &g_holder_id)) != 0)
		return (ret);
	/* Waiter runs inside a real txn with a txn timeout. */
	if ((ret = g_env->txn_begin(g_env, NULL, &g_waiter_txn, 0)) != 0)
		return (ret);

	/* Main thread holds the write lock on the contended object. */
	memset(&obj, 0, sizeof(obj));
	obj.data = (void *)OBJNAME;
	obj.size = (u_int32_t)strlen(OBJNAME) + 1;
	memset(&holder_lock, 0, sizeof(holder_lock));
	if ((ret = g_env->lock_get(g_env, g_holder_id, 0, &obj,
	    DB_LOCK_WRITE, &holder_lock)) != 0)
		return (ret);

	__db_sim_activate(seed);
	/*
	 * Set the txn's deadline FIRST, on the TRUE clock: tx_expire =
	 * true_now + 60ms (a legitimate, correctly-computed deadline).
	 */
	if ((ret = g_waiter_txn->set_timeout(g_waiter_txn, TXN_TMOUT,
	    DB_SET_TXN_TIMEOUT)) != 0)
		return (ret);

	/*
	 * NOW knock the clock backward (the TRANSIENT FoundationDB model): a
	 * big negative offset plus large backward-leaning jumps, for a BOUNDED
	 * number of reads, after which the clock settles back to true time.
	 * The deadline is a fixed, already-set target; the backward jump pushes
	 * every fresh `now2` reading below it for a while.  A correct engine
	 * must fire the timeout once the recovered clock crosses the deadline;
	 * a broken engine that trusts a monotonic assumption, or re-arms the
	 * deadline off the jumped-back clock, would lose the timeout FOREVER --
	 * and the wall-clock alarm catches that as a REAL BUG.
	 *
	 * (A PERSISTENT adversary clock that never advances past the deadline
	 * could never fire ANY deadline-based timeout -- physics, not a bug --
	 * so the jump is bounded and recovering.)
	 */
	__db_sim_clock_enable(
	    /* offset */ -3LL * 1000 * 1000 * 1000,     /* -3s steady */
	    /* jitter */ 20LL * 1000 * 1000,            /* +/-20ms */
	    /* jump   */ 10LL * 1000 * 1000 * 1000,     /* up to 10s jumps */
	    /* jump%  */ 700);                           /* 70% of reads jump */
	/* Disturbance lasts ~60 clock reads, then the clock returns to true
	 * and the fixed deadline MUST be crossed. */
	__db_sim_clock_settle_after(60);

	/* The txn must own the tx_expire deadline: touch a lock under it so
	 * the locker exists, then start the waiter which contends. */
	if ((ret = pthread_create(&tid, NULL, helper, NULL)) != 0)
		return (ret);

	nap.tv_sec = 0;
	nap.tv_nsec = 5 * 1000 * 1000;
	for (i = 0; i < 4000 && !g_helper_done; i++) {
		rejected = 0;
		(void)g_env->lock_detect(g_env, 0, DB_LOCK_EXPIRE, &rejected);
		(void)nanosleep(&nap, NULL);
	}

	(void)pthread_join(tid, NULL);

	*skews = __db_sim_clock_fire_count();
	*verdict = g_helper_ret;
	__db_sim_deactivate();

	(void)g_env->lock_put(g_env, &holder_lock);
	(void)g_waiter_txn->abort(g_waiter_txn);
	(void)g_env->close(g_env, 0);

	if (!g_helper_done)
		return (-100);   /* never resolved */
	return (0);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	char cmd[512];
	unsigned long skews1, skews2;
	int ret, verdict1, verdict2;

	g_seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0xC10C0003;

	(void)signal(SIGALRM, on_alarm);
	(void)alarm(WALL_LIMIT);

	(void)snprintf(cmd, sizeof(cmd), "rm -rf %s && mkdir -p %s",
	    HOME, HOME);
	if (system(cmd) != 0)
		return (EXIT_FAILURE);

	if ((ret = run_once(g_seed, &skews1, &verdict1)) != 0) {
		if (ret == -100)
			fprintf(stderr, "test_sim_clockskew_backward: FAIL -- "
			    "waiter never resolved (lost timeout?) (seed "
			    "0x%llx)\n", (unsigned long long)g_seed);
		else
			fprintf(stderr, "test_sim_clockskew_backward: run "
			    "failed: %s\n", db_strerror(ret));
		return (EXIT_FAILURE);
	}

	/* Timeout must have fired cleanly, never granted. */
	if (verdict1 != DB_LOCK_NOTGRANTED && verdict1 != DB_LOCK_DEADLOCK) {
		fprintf(stderr, "test_sim_clockskew_backward: FAIL -- "
		    "unexpected verdict %d (%s) (seed 0x%llx)\n",
		    verdict1, db_strerror(verdict1),
		    (unsigned long long)g_seed);
		return (EXIT_FAILURE);
	}

	/* Determinism: rerun the same seed -> identical skew count + verdict. */
	(void)snprintf(cmd, sizeof(cmd), "rm -rf %s && mkdir -p %s",
	    HOME, HOME);
	(void)system(cmd);
	if (run_once(g_seed, &skews2, &verdict2) != 0) {
		fprintf(stderr, "test_sim_clockskew_backward: FAIL -- replay "
		    "run did not resolve (seed 0x%llx)\n",
		    (unsigned long long)g_seed);
		return (EXIT_FAILURE);
	}
	if (skews1 != skews2 || verdict1 != verdict2) {
		fprintf(stderr, "test_sim_clockskew_backward: FAIL -- NOT "
		    "DETERMINISTIC: skews %lu vs %lu, verdict %d vs %d "
		    "(seed 0x%llx)\n", skews1, skews2, verdict1, verdict2,
		    (unsigned long long)g_seed);
		return (EXIT_FAILURE);
	}

	(void)alarm(0);
	printf("test_sim_clockskew_backward: PASS -- txn timeout fired "
	    "(ret=%s) despite a backward-trending clock; no timeout lost, "
	    "deterministic (%lu skews, replay identical) (seed 0x%llx)\n",
	    verdict1 == DB_LOCK_NOTGRANTED ? "NOTGRANTED" : "DEADLOCK",
	    skews1, (unsigned long long)g_seed);
	return (EXIT_SUCCESS);
}
