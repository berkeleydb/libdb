/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2006,2008 Oracle.  All rights reserved.
 *
 * $Id: os_abs.c,v 1.5 2008/01/08 20:58:44 bostic Exp $
 */

#include "db_config.h"

#include "db_int.h"

/*
 * __os_abspath --
 *	Return if a path is an absolute path.
 */
int
__os_abspath(path)
	const char *path;
{
	return (path[0] == 'f' && path[1] == 's' && path[2] == ':');
}
