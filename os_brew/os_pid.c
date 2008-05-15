/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2001,2008 Oracle.  All rights reserved.
 *
 * $Id: os_pid.c,v 1.8 2008/01/08 20:58:44 bostic Exp $
 */

#include "db_config.h"

#include "db_int.h"

/*
 * __os_id --
 *	Return the current process ID.
 */
void
__os_id(dbenv, pidp, tidp)
	DB_ENV *dbenv;
	pid_t *pidp;
	db_threadid_t *tidp;
{
	AEEApplet *app;

	COMPQUIET(dbenv, NULL);

	if (pidp != NULL) {
		app = (AEEApplet *)GETAPPINSTANCE();
		*pidp = (pid_t)ISHELL_ActiveApplet(app->m_pIShell);
	}
	if (tidp != NULL)
		*tidp = 0;
}
