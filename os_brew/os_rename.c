/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997,2008 Oracle.  All rights reserved.
 *
 * $Id: os_rename.c,v 1.7 2008/02/18 19:34:22 bostic Exp $
 */

#include "db_config.h"

#include "db_int.h"

/*
 * __os_rename --
 *	Rename a file.
 */
int
__os_rename(env, old, new, silent)
	ENV *env;
	const char *old, *new;
	u_int32_t silent;
{
	IFileMgr *pIFileMgr;
	int ret;

	FILE_MANAGER_CREATE(env, pIFileMgr, ret);
	if (ret != 0)
		return (ret);

	LAST_PANIC_CHECK_BEFORE_IO(env);

	if (IFILEMGR_Rename(pIFileMgr, old, new) == SUCCESS)
		ret = 0;
	else
		if (!silent)
			FILE_MANAGER_ERR(env,
			    pIFileMgr, old, "IFILEMGR_Rename", ret);
		else
			ret = __os_posix_err(IFILEMGR_GetLastError(pIFileMgr));

	IFILEMGR_Release(pIFileMgr);
	return (ret);
}
