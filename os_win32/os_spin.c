/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998
 *	Sleepycat Software.  All rights reserved.
 */

#include "config.h"

#ifndef lint
static const char sccsid[] = "@(#)os_spin.c	10.8 (Sleepycat) 10/28/98";
#endif /* not lint */

#include "db_int.h"
#include "os_jump.h"

/*
 * __os_yield --
 *	Yield the processor.
 *
 * PUBLIC: void __os_yield __P((u_long));
 */
void
__os_yield(usecs)
	u_long usecs;
{
	if (__db_jump.j_yield != NULL && __db_jump.j_yield() == 0)
		return;
	__os_sleep(0, usecs);
}

/*
 * __os_spin --
 *	Return the number of default spins before blocking.
 *
 * PUBLIC: int __os_spin __P((void));
 */
int
__os_spin()
{
	static long sys_val;
        SYSTEM_INFO SystemInfo;

	/* If the application specified the spins, use its value. */
	if (DB_GLOBAL(db_tsl_spins) != 0)
		return (DB_GLOBAL(db_tsl_spins));

	/* If we've already figured this out, return the value. */
	if (sys_val != 0)
		return (sys_val);

	/* Get the number of processors */
        GetSystemInfo(&SystemInfo);

	/*
	 * Spin 50 times per processor -- we have anecdotal evidence that this
	 * is a reasonable value.
	 */
	if (SystemInfo.dwNumberOfProcessors > 1)
		sys_val = 50 * SystemInfo.dwNumberOfProcessors;
	else
		sys_val = 1;
	return (sys_val);
}
