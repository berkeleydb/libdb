/*-
 * SQLite-style malloc-failure injection for libdb.
 *
 * fi_alloc.c --
 *	Process-global allocation fault-injection state.  Compiled into the
 *	library ONLY under --enable-faultinject (HAVE_FAULT_INJECT); absent
 *	from a production build.  See fi_alloc.h for the model.
 *
 *	The state is a single process-global counter plus an armed target.
 *	__db_fi_fail() is called once per allocation from the __os_* seam:
 *	it increments the counter and, if the counter reaches the armed K,
 *	returns 1 exactly once per run so the caller returns NULL/ENOMEM
 *	just as a real OOM would.
 */

#include "db_config.h"
#include "db_int.h"

#include "fi_alloc.h"

#include <stdlib.h>
#include <stdio.h>

/*
 * ponytail: process-global, single-threaded sweep. The driver runs a
 * DB_PRIVATE single-process workload, so plain (non-atomic) globals are
 * fine and keep the hook a couple of instructions. If a future
 * multi-threaded sweep needs it, make fi_count atomic and key the arm
 * per-thread.
 */
static long fi_count = 0;	/* allocations seen since last reset/arm */
static long fi_fail_at = 0;	/* fail this allocation number; 0 = disarmed */
static int  fi_fired = 0;	/* the armed failure has fired this run */
static int  fi_env_checked = 0;	/* DB_FI_FAIL_AT consulted yet? */
static int  fi_verbose = 0;	/* DB_FI_VERBOSE: trace fired failures */

/*
 * fi_check_env --
 *	Lazily honor DB_FI_FAIL_AT / DB_FI_VERBOSE on the first allocation,
 *	so setting the env var before running any libdb code arms the
 *	sweep without an API call.  An explicit __db_fi_arm() overrides it.
 */
static void
fi_check_env()
{
	char *p;

	fi_env_checked = 1;
	if ((p = getenv("DB_FI_VERBOSE")) != NULL && atoi(p) != 0)
		fi_verbose = 1;
	if (fi_fail_at == 0 &&
	    (p = getenv("DB_FI_FAIL_AT")) != NULL) {
		long k = atol(p);
		if (k > 0)
			fi_fail_at = k;
	}
}

int
__db_fi_fail()
{
	if (!fi_env_checked)
		fi_check_env();

	++fi_count;

	if (fi_fail_at != 0 && !fi_fired && fi_count == fi_fail_at) {
		fi_fired = 1;
		if (fi_verbose)
			fprintf(stderr,
			    "fi: injecting OOM at allocation #%ld\n",
			    fi_count);
		return (1);
	}
	return (0);
}

void
__db_fi_arm(k)
	long k;
{
	fi_env_checked = 1;	/* explicit arm overrides the env var */
	fi_count = 0;
	fi_fired = 0;
	fi_fail_at = k > 0 ? k : 0;
}

void
__db_fi_disarm()
{
	fi_fail_at = 0;
	fi_fired = 0;
}

void
__db_fi_reset()
{
	fi_count = 0;
	fi_fired = 0;
}

long
__db_fi_count()
{
	return (fi_count);
}

int
__db_fi_fired()
{
	return (fi_fired);
}
