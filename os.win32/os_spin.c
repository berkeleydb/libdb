/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997
 *	Sleepycat Software.  All rights reserved.
 */

#include "config.h"

#ifndef lint
static const char sccsid[] = "@(#)os_spin.c	10.2 (Sleepycat) 11/25/97";
#endif /* not lint */

/*
 * __os_spin --
 *	Return the number of default spins before blocking.
 *
 * PUBLIC: int __os_spin __P((void));
 */
int
__os_spin()
{
	extern int __db_tsl_spins;
        SYSTEM_INFO SystemInfo;

	/* If the application specified the spins, use its value. */
	if (__db_tsl_spins != 0)
		return (__db_tsl_spins);

	/* Get the number of processors */
        GetSystemInfo(&SystemInfo);

	/* If SMP, spins = 50 * # cpus, otherwise spins = 1. */
        if (SystemInfo.dwNumberOfProcessors > 1)
		return (50 * SystemInfo.dwNumberOfProcessors);

	return (1);
}
