/*-
 * Deterministic Simulation Testing (DST) for libdb.
 *
 * test_sim_clockskew_timeout.c --
 *	Lock timeout under a skewed / jumping / non-monotonic clock.
 *
 *	The dangerous pattern (FoundationDB clock-skew model): a timeout is
 *	a deadline computed as `deadline = now + timeout` (__clock_set_expires),
 *	compared later against `now2 >= deadline` (__clock_expired) by the
 *	deadlock detector's DB_LOCK_EXPIRE scan.  If the clock jumps BACKWARD
 *	between the two reads, now2 can be < deadline forever and the timeout
 *	never fires -- the waiter HANGS.
 *
 *	Scenario: the main thread holds a write lock on an object; a helper
 *	thread requests a conflicting write lock with a short lock timeout and
 *	blocks.  The main thread arms the clock-skew fault (fixed offset +
 *	jitter + occasional forward AND backward jumps) and then drives the
 *	deadlock detector's expiry scan repeatedly.  Invariant: the blocked
 *	lock request EVENTUALLY returns DB_LOCK_NOTGRANTED (the timeout fires)
 *	and the run resolves -- no hang, no premature abort-storm.
 *
 *	The whole run is guarded by a hard wall-clock alarm: a clock bug that
 *	loses the timeout would hang the helper, and the alarm turns that hang
 *	into a reported FAILURE + the exact seed to reproduce (rather than a
 *	CI process that wedges forever).  Same seed => same skew sequence =>
 *	deterministic outcome.
 *
 *	Build/run (from build_unix, after configure --enable-dst):
 *	    make test_sim_clockskew_timeout && ./test_sim_clockskew_timeout [seed]
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

#define HOME       "TESTDIR_sim_clockskew_timeout"
#define OBJNAME    "hot-object"
#define LK_TIMEOUT 50000        /* 50 ms lock timeout (microseconds) */
#define WALL_LIMIT 20           /* hard wall-clock guard: fail if we hang */

static DB_ENV  *g_env;
static u_int32_t g_holder_id, g_waiter_id;
static volatile int g_helper_ret = -12345;    /* sentinel: not set yet */
static volatile int g_helper_done;

/* Wall-clock watchdog: if a lost timeout hangs us, SIGALRM aborts with a
 * clear "hung" verdict and the seed, instead of wedging CI forever. */
static uint64_t g_seed;
static void
on_alarm(sig)
	int sig;
{
	(void)sig;
	fprintf(stderr, "test_sim_clockskew_timeout: FAIL -- HUNG: a lock "
	    "timeout was LOST under clock skew (waiter never woke within %ds); "
	    "REAL BUG: non-monotonic clock defeats the deadline.  Reproduce: "
	    "./test_sim_clockskew_timeout 0x%llx\n",
	    WALL_LIMIT, (unsigned long long)g_seed);
	_exit(3);
}

/*
 * Helper thread: request the same object with a conflicting write lock and
 * a lock timeout.  Blocks until the main thread's detector expires it.
 */
static void *
helper(arg)
	void *arg;
{
	DB_LOCKREQ req;
	DB_LOCK lock;
	DBT obj;
	int ret;

	(void)arg;
	memset(&obj, 0, sizeof(obj));
	obj.data = (void *)OBJNAME;
	obj.size = (u_int32_t)strlen(OBJNAME) + 1;
	memset(&req, 0, sizeof(req));
	req.op = DB_LOCK_GET_TIMEOUT;
	req.mode = DB_LOCK_WRITE;
	req.timeout = LK_TIMEOUT;
	req.obj = &obj;
	memset(&lock, 0, sizeof(lock));

	/* This blocks inside __lock_get_internal until the expiry scan wakes
	 * it; the return code tells us how it resolved. */
	ret = g_env->lock_vec(g_env, g_waiter_id, 0, &req, 1, NULL);
	if (ret == 0)      /* granted (should not happen: main holds it) */
		(void)g_env->lock_put(g_env, &req.lock);
	g_helper_ret = ret;
	g_helper_done = 1;
	return (NULL);
}

int
main(argc, argv)
	int argc;
	char *argv[];
{
	DB_LOCK holder_lock;
	DBT obj;
	pthread_t tid;
	char cmd[512];
	struct timespec nap;
	int ret, i, rejected;

	g_seed = argc > 1 ? strtoull(argv[1], NULL, 0) : 0xC10C0001;

	(void)snprintf(cmd, sizeof(cmd), "rm -rf %s && mkdir -p %s",
	    HOME, HOME);
	if (system(cmd) != 0)
		return (EXIT_FAILURE);

	/* Hard wall-clock guard against a hung (lost) timeout. */
	(void)signal(SIGALRM, on_alarm);
	(void)alarm(WALL_LIMIT);

	if ((ret = db_env_create(&g_env, 0)) != 0)
		goto envfail;
	/* We drive the expiry scan by hand, so no background detector. */
	if ((ret = g_env->open(g_env, HOME, DB_CREATE | DB_INIT_LOCK |
	    DB_INIT_MPOOL, 0664)) != 0)
		goto envfail;
	/* Auto-run the detector's expiry scan on each lock request too. */
	(void)g_env->set_lk_detect(g_env, DB_LOCK_EXPIRE);

	if ((ret = g_env->lock_id(g_env, &g_holder_id)) != 0)
		goto envfail;
	if ((ret = g_env->lock_id(g_env, &g_waiter_id)) != 0)
		goto envfail;

	/* Main thread grabs the write lock on the hot object. */
	memset(&obj, 0, sizeof(obj));
	obj.data = (void *)OBJNAME;
	obj.size = (u_int32_t)strlen(OBJNAME) + 1;
	memset(&holder_lock, 0, sizeof(holder_lock));
	if ((ret = g_env->lock_get(g_env, g_holder_id, 0, &obj,
	    DB_LOCK_WRITE, &holder_lock)) != 0)
		goto envfail;

	/*
	 * Arm the clock-skew fault: a seeded fixed offset, per-read jitter,
	 * and frequent forward+BACKWARD jumps of up to 5s -- an aggressively
	 * non-monotonic clock.  Drawn from the CLOCK stream, so arming it does
	 * not perturb anything else.  This is the exact condition that could
	 * defeat `deadline = now + timeout; ... now2 >= deadline`.
	 */
	__db_sim_activate(g_seed);
	__db_sim_clock_enable(
	    /* offset */ 250LL * 1000 * 1000,      /* +250ms steady skew */
	    /* jitter */ 10LL * 1000 * 1000,       /* +/-10ms per read */
	    /* jump   */ 5LL * 1000 * 1000 * 1000, /* up to 5s jumps */
	    /* jump%  */ 400);                      /* 40% of reads jump */

	/* Start the waiter; it blocks on the conflicting lock. */
	if ((ret = pthread_create(&tid, NULL, helper, NULL)) != 0) {
		fprintf(stderr, "pthread_create: %s\n", strerror(ret));
		goto envfail;
	}

	/*
	 * Drive the expiry scan.  Each __lock_detect(DB_LOCK_EXPIRE) reads
	 * the (skewed) clock via __clock_expired.  On a monotonic-enough
	 * reading the deadline passes and the waiter is expired + woken; a
	 * backward jump can push "now" back before the deadline, delaying it.
	 * We loop with a real nap between scans; the wall-clock alarm bounds
	 * the total.  A robust engine resolves within a bounded number of
	 * scans DESPITE the skew (the offset+jitter+jumps average out and the
	 * true wall clock keeps advancing between naps, so some scan reads a
	 * value past the deadline).
	 */
	nap.tv_sec = 0;
	nap.tv_nsec = 5 * 1000 * 1000;    /* 5 ms between scans */
	for (i = 0; i < 4000 && !g_helper_done; i++) {
		rejected = 0;
		(void)g_env->lock_detect(g_env, 0, DB_LOCK_EXPIRE, &rejected);
		(void)nanosleep(&nap, NULL);
	}

	(void)pthread_join(tid, NULL);
	(void)alarm(0);

	__db_sim_deactivate();

	/* Release the holder lock and shut down. */
	(void)g_env->lock_put(g_env, &holder_lock);
	(void)g_env->close(g_env, 0);

	/*
	 * Verdict.  The waiter MUST have resolved (not still blocked) and the
	 * resolution must be a clean timeout (DB_LOCK_NOTGRANTED) or a
	 * deadlock verdict (DB_LOCK_DEADLOCK) -- never granted (main held it),
	 * never a crash.  The point: under an aggressively non-monotonic clock
	 * the timeout still EVENTUALLY fires; no hang, no corruption.
	 */
	if (!g_helper_done) {
		fprintf(stderr, "test_sim_clockskew_timeout: FAIL -- waiter "
		    "never resolved under clock skew (seed 0x%llx)\n",
		    (unsigned long long)g_seed);
		return (EXIT_FAILURE);
	}
	if (g_helper_ret != DB_LOCK_NOTGRANTED &&
	    g_helper_ret != DB_LOCK_DEADLOCK) {
		fprintf(stderr, "test_sim_clockskew_timeout: FAIL -- waiter "
		    "resolved with unexpected ret %d (%s) (seed 0x%llx)\n",
		    g_helper_ret, db_strerror(g_helper_ret),
		    (unsigned long long)g_seed);
		return (EXIT_FAILURE);
	}

	printf("test_sim_clockskew_timeout: PASS -- lock timeout fired "
	    "(ret=%s) under a non-monotonic clock (%lu skews applied); "
	    "no hang, no corruption (seed 0x%llx)\n",
	    g_helper_ret == DB_LOCK_NOTGRANTED ? "NOTGRANTED" : "DEADLOCK",
	    __db_sim_clock_fire_count(), (unsigned long long)g_seed);
	return (EXIT_SUCCESS);

envfail:
	fprintf(stderr, "test_sim_clockskew_timeout: setup failed: %s\n",
	    db_strerror(ret));
	return (EXIT_FAILURE);
}
