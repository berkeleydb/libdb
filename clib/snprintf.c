/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1996, 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)snprintf.c	11.1 (Sleepycat) 7/24/99";
#endif /* not lint */

#ifndef NO_SYSTEM_INCLUDES
#include <sys/types.h>

#include <stdio.h>
#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif
#endif

#include "db_int.h"

/*
 * snprintf --
 *	Bounded version of sprintf.
 *
 * PUBLIC: #ifndef HAVE_SNPRINTF
 * PUBLIC: #ifdef __STDC__
 * PUBLIC: int snprintf __P((char *, size_t, const char *, ...));
 * PUBLIC: #else
 * PUBLIC: int snprintf();
 * PUBLIC: #endif
 * PUBLIC: #endif
 */
#ifndef HAVE_SNPRINTF
int
#ifdef __STDC__
snprintf(char *str, size_t n, const char *fmt, ...)
#else
snprintf(str, n, fmt, va_alist)
	char *str;
	size_t n;
	const char *fmt;
	va_dcl
#endif
{
	va_list ap;
	int rval;

	COMPQUIET(n, 0);
#ifdef __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
#ifdef SPRINTF_RET_CHARPNT
	(void)vsprintf(str, fmt, ap);
	va_end(ap);
	return (strlen(str));
#else
	rval = vsprintf(str, fmt, ap);
	va_end(ap);
	return (rval);
#endif
}
#endif
