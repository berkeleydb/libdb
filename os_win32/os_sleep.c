/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998
 *	Sleepycat Software.  All rights reserved.
 */

#include "config.h"

#ifndef lint
static const char sccsid[] = "@(#)os_sleep.c	10.12 (Sleepycat) 10/28/98";
#endif /* not lint */

#include "db_int.h"
#include "os_jump.h"

/*
 * __os_sleep --
 *	Yield the processor for a period of time.
 */
int
__os_sleep(secs, usecs)
	u_long secs, usecs;		/* Seconds and microseconds. */
{
	/* Don't require that the values be normalized. */
	for (; usecs >= 1000000; ++secs, usecs -= 1000000)
		;

	if (__db_jump.j_sleep != NULL)
		return (__db_jump.j_sleep(secs, usecs));

	/*
	 * It's important that we yield the processor here so that other
	 * processes or threads are permitted to run.
	 */
	Sleep(secs * 1000 + usecs / 1000);
	return (0);
}
