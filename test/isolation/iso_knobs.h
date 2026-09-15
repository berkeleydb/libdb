/*-
 * See the file LICENSE for redistribution information.
 *
 * test/isolation/iso_knobs.h --
 *	Two knobs shared by the Tier B1 drivers.
 *
 * 1. ISO_LK_PARTITIONS -- run the whole tier with a chosen lock-partition
 *    count.  This is a REGRESSION GATE, not a tuning option.
 *
 *    The lock, txn and log "regions" all live in the environment region and
 *    each sets its own mtx_region to the SAME mutex, renv->mtx_regenv (see
 *    __lock_region_init, __txn_init, __log_init) -- they must, because that
 *    mutex is also the shared region's mtx_alloc.  LOCK_SYSTEM_LOCK acquires
 *    it in exactly one case: part_t_size == 1 (with more partitions it is a
 *    no-op and the per-partition mutex serializes instead).  So at
 *    lk_partitions == 1, and only there, LOCK_SYSTEM_LOCK and
 *    TXN_SYSTEM_LOCK are one non-recursive latch, and any code path that
 *    nests them hangs a thread against itself.
 *
 *    That is exactly what the SSI rw-conflict branch in __lock_get_internal
 *    did: __lock_get holds LOCK_SYSTEM_LOCK across the call, and the branch
 *    took TXN_SYSTEM_LOCK to make its pivot-flag read-modify-write atomic.
 *    Every SSI schedule in this tier drives that branch, so running the tier
 *    at lk_partitions=1 reproduces the self-deadlock and running it at the
 *    default does not -- which is why the tier must run BOTH.
 *
 *    lk_partitions=1 is a supported configuration (DB_ENV->set_lk_partitions)
 *    and is the DEFAULT on a single-CPU machine (__lock_env_create:
 *    lk_partitions = cpu > 1 ? 10 * cpu : 1), so this is user-reachable.
 *
 * 2. The watchdog.  A self-deadlock does not fail, it HANGS -- and a hung
 *    driver is indistinguishable from a slow one, so CI would report a job
 *    timeout naming no test.  iso_watchdog() turns the hang into a named,
 *    non-zero verdict on stdout, well inside the tier's own timeout, so the
 *    failing test identifies itself.
 */
#ifndef	_ISO_KNOBS_H_
#define	_ISO_KNOBS_H_

#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/*
 * iso_set_partitions --
 *	Apply ISO_LK_PARTITIONS to a not-yet-opened DB_ENV.  Unset or "0"
 *	leaves the library default alone.  Returns the DB_ENV method's rc.
 */
static int
iso_set_partitions(DB_ENV *dbenv)
{
	const char *s;
	int nparts;

	if ((s = getenv("ISO_LK_PARTITIONS")) == NULL || *s == '\0')
		return (0);
	if ((nparts = atoi(s)) <= 0)
		return (0);
	return (dbenv->set_lk_partitions(dbenv, (u_int32_t)nparts));
}

/*
 * Report the partition count actually in force, so a run is self-describing.
 * Marked used: the crash driver forks children and does not print it.
 */
static void iso_report_partitions(DB_ENV *) __attribute__((unused));
static void
iso_report_partitions(DB_ENV *dbenv)
{
	static int reported = 0;
	u_int32_t nparts;

	if (reported)			/* Drivers that reopen per scenario. */
		return;
	reported = 1;
	if (dbenv->get_lk_partitions(dbenv, &nparts) == 0)
		printf("lk_partitions: %lu\n", (u_long)nparts);
}

static const char *iso_watchdog_what = "test";

static void
iso_watchdog_fired(int sig)
{
	char buf[256];
	int n;

	(void)sig;
	/*
	 * Async-signal-safe: no printf, no exit (no atexit handlers, no stdio
	 * flush that could itself block on a lock held by the wedged thread).
	 */
	n = snprintf(buf, sizeof(buf),
	    "\nFAIL: %s made no progress before the watchdog fired -- a hang, "
	    "not a slow run.\n    A self-deadlock in the engine looks exactly "
	    "like this: check whether a region latch is being acquired twice "
	    "by one thread (see iso_knobs.h).\n", iso_watchdog_what);
	if (n > 0)
		(void)write(STDERR_FILENO, buf, (size_t)n);
	_exit(1);
}

/*
 * iso_watchdog --
 *	Fail with a named verdict after `seconds' rather than hanging.  ISO_WATCHDOG
 *	overrides; 0 disables (for interactive debugging under a debugger).
 */
static void
iso_watchdog(const char *what, unsigned seconds)
{
	const char *s;

	if ((s = getenv("ISO_WATCHDOG")) != NULL && *s != '\0')
		seconds = (unsigned)atoi(s);
	if (seconds == 0)
		return;
	iso_watchdog_what = what;
	(void)signal(SIGALRM, iso_watchdog_fired);
	(void)alarm(seconds);
}

#endif /* !_ISO_KNOBS_H_ */
