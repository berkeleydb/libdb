/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)mut_tas.c	11.4 (Sleepycat) 10/1/99";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#endif

#include "db_int.h"

#ifdef HAVE_MUTEX_68K_GCC_ASSEMBLY
#include "68K.gcc"
#endif
#ifdef HAVE_MUTEX_ALPHA_GCC_ASSEMBLY
#include "alpha.gcc"
#endif
#ifdef HAVE_MUTEX_HPPA_GCC_ASSEMBLY
#include "parisc.gcc"
#endif
#ifdef HAVE_MUTEX_SCO_X86_CC_ASSEMBLY
#include "sco.cc"
#endif
#ifdef HAVE_MUTEX_SPARC_GCC_ASSEMBLY
#include "sparc.gcc"
#endif
#ifdef HAVE_MUTEX_X86_GCC_ASSEMBLY
#include "x86.gcc"
#endif

#ifdef DIAGNOSTIC
#undef	MSG1
#define	MSG1		"mutex_lock: ERROR: lock currently in use: pid: %lu.\n"
#undef	MSG2
#define	MSG2		"mutex_unlock: ERROR: lock already unlocked\n"
#ifndef	STDERR_FILENO
#define	STDERR_FILENO	2
#endif
#endif

/*
 * __db_tas_mutex_init --
 *	Initialize a MUTEX.
 *
 * PUBLIC: int __db_tas_mutex_init __P((DB_ENV *, MUTEX *, u_int32_t));
 */
int
__db_tas_mutex_init(dbenv, mutexp, flags)
	DB_ENV *dbenv;
	MUTEX *mutexp;
	u_int32_t flags;
{
	memset(mutexp, 0, sizeof(*mutexp));

	/*
	 * If this is a thread lock or the process has told us that there are
	 * no other processes in the environment, use thread-only locks, they
	 * are faster in some cases.
	 *
	 * This is where we decide to ignore locks we don't need to set -- if
	 * the application isn't threaded, there aren't any threads to block.
	 */
	if (LF_ISSET(MUTEX_THREAD) || F_ISSET(dbenv, DB_ENV_PRIVATE)) {
		if (!F_ISSET(dbenv, DB_ENV_THREAD)) {
			F_SET(mutexp, MUTEX_IGNORE);
			return (0);
		}
		F_SET(mutexp, MUTEX_THREAD);
	}

	/* Initialize the lock. */
	if (MUTEX_INIT(&mutexp->tas))
		return (__os_get_errno());

	mutexp->spins = __os_spin();

	return (0);
}

/*
 * __db_tas_mutex_lock
 *	Lock on a mutex, logically blocking if necessary.
 *
 * PUBLIC: int __db_tas_mutex_lock __P((MUTEX *));
 */
int
__db_tas_mutex_lock(mutexp)
	MUTEX *mutexp;
{
	u_long ms;
	int nspins;

	if (!DB_GLOBAL(db_mutexlocks) || F_ISSET(mutexp, MUTEX_IGNORE))
		return (0);

	ms = 1;

loop:	/* Attempt to acquire the resource for N spins. */
	for (nspins = mutexp->spins; nspins > 0; --nspins) {
		if (!MUTEX_SET(&mutexp->tas))
			continue;
#ifdef DIAGNOSTIC
		if (mutexp->locked != 0) {
			char msgbuf[128];
			(void)snprintf(msgbuf,
			    sizeof(msgbuf), MSG1, (u_long)mutexp->locked);
			(void)write(STDERR_FILENO, msgbuf, strlen(msgbuf));
		}
		mutexp->locked = (u_int32_t)getpid();
#endif
		if (ms == 1)
			++mutexp->mutex_set_nowait;
		else
			++mutexp->mutex_set_wait;
		return (0);
	}

	/* Yield the processor; wait 1ms initially, up to 1 second. */
	__os_yield(ms * USEC_PER_MS);
	if ((ms <<= 1) > MS_PER_SEC)
		ms = MS_PER_SEC;

	goto loop;
}

/*
 * __db_tas_mutex_unlock --
 *	Release a lock.
 *
 * PUBLIC: int __db_tas_mutex_unlock __P((MUTEX *));
 */
int
__db_tas_mutex_unlock(mutexp)
	MUTEX *mutexp;
{
	if (!DB_GLOBAL(db_mutexlocks) || F_ISSET(mutexp, MUTEX_IGNORE))
		return (0);

#ifdef DIAGNOSTIC
	if (!mutexp->locked)
		(void)write(STDERR_FILENO, MSG2, sizeof(MSG2) - 1);
	mutexp->locked = 0;
#endif

	MUTEX_UNSET(&mutexp->tas);

	return (0);
}
