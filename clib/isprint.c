/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2005,2008 Oracle.  All rights reserved.
 *
 * $Id: isprint.c,v 1.5 2008/01/08 20:58:08 bostic Exp $
 */

#include "db_config.h"

#include "db_int.h"

/*
 * isprint --
 *
 * PUBLIC: #ifndef HAVE_ISPRINT
 * PUBLIC: int isprint __P((int));
 * PUBLIC: #endif
 */
int
isprint(c)
	int c;
{
	/*
	 * Depends on ASCII character values.
	 */
	return ((c >= ' ' && c <= '~') ? 1 : 0);
}
