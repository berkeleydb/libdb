/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1999
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)mutex.c	11.1 (Sleepycat) 7/25/99";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>
#endif

#include "db_int.h"

/*
 * __db_mutex_alloc --
 *	Allocate and initialize a mutex.
 *
 * PUBLIC: int __db_mutex_alloc __P((DB_ENV *, REGINFO *, MUTEX **));
 */
int
__db_mutex_alloc(dbenv, infop, storep)
	DB_ENV *dbenv;
	REGINFO *infop;
	MUTEX **storep;
{
	int ret;

	/*
	 * If the architecture supports mutex in heap memory, simply
	 * use that memory.  If it doesn't, we have to allocate space
	 * in the region.
	 */
#ifdef MUTEX_NO_MALLOC_LOCKS
	R_LOCK(dbenv, infop);
	ret = __db_shalloc(infop->addr, sizeof(MUTEX), MUTEX_ALIGN, storep);
	R_UNLOCK(dbenv, infop);
#else
	COMPQUIET(dbenv, NULL);
	COMPQUIET(infop, NULL);
	ret = __os_calloc(1, sizeof(MUTEX), storep);
#endif
	return (ret);
}

/*
 * __db_mutex_free --
 *	Free a mutex.
 *
 * PUBLIC: void __db_mutex_free __P((DB_ENV *, REGINFO *, MUTEX *));
 */
void
__db_mutex_free(dbenv, infop, mutexp)
	DB_ENV *dbenv;
	REGINFO *infop;
	MUTEX *mutexp;
{
#ifdef MUTEX_NO_MALLOC_LOCKS
	R_LOCK(dbenv, infop);
	__db_shalloc_free(infop->addr, mutexp);
	R_UNLOCK(dbenv, infop);
#else
	COMPQUIET(dbenv, NULL);
	COMPQUIET(infop, NULL);
	__os_free(mutexp, sizeof(*mutexp));
#endif
}
