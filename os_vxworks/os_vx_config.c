/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1999-2005
 *	Sleepycat Software.  All rights reserved.
 *
 * $Id: os_vx_config.c,v 12.2 2005/07/19 18:51:13 sue Exp $
 */

#include "db_config.h"

#include "db_int.h"

/*
 * __os_fs_notzero --
 *	Return 1 if allocated filesystem blocks are not zeroed.
 *
 * PUBLIC: int __os_fs_notzero __P((void));
 */
int
__os_fs_notzero()
{
	/*
	 * Some VxWorks FS drivers do not zero-fill pages that were never
	 * explicitly written to the file, they give you random garbage,
	 * and that breaks Berkeley DB.
	 */
	return (1);
}

/*
 * __os_support_db_register --
 *	Return 1 if the system supports DB_REGISTER.
 *
 * PUBLIC: int __os_support_db_register __P((void));
 */
int
__os_support_db_register()
{
	return (0);
}

/*
 * __os_support_replication --
 *	Return 1 if the system supports replication.
 *
 * PUBLIC: int __os_support_replication __P((void));
 */
int
__os_support_replication()
{
	return (1);
}
