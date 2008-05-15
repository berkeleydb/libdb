/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1999,2008 Oracle.  All rights reserved.
 *
 * $Id: os_root.c,v 12.8 2008/01/08 20:58:43 bostic Exp $
 */

#include "db_config.h"

#include "db_int.h"

/*
 * __os_isroot --
 *	Return if user has special permissions.
 *
 * PUBLIC: int __os_isroot __P((void));
 */
int
__os_isroot()
{
#ifdef HAVE_GETUID
	return (getuid() == 0);
#else
	return (0);
#endif
}
