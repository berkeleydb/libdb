/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2006,2008 Oracle.  All rights reserved.
 *
 * $Id: time.c,v 12.7 2008/04/17 01:25:33 alexg Exp $
 */

#include "db_config.h"

#include "db_int.h"

/*
 * time --
 *
 * PUBLIC: #ifndef HAVE_TIME
 * PUBLIC: time_t time __P((time_t *));
 * PUBLIC: #endif
 */
time_t
time(nowp)
	time_t *nowp;
{
	db_timespec t;
	time_t res;

	__os_gettime(NULL, &t, 0);

	res = t.tv_sec + t.tv_nsec / NS_PER_SEC;

	if (nowp != NULL)
		*nowp = res;
	return (res);
}
