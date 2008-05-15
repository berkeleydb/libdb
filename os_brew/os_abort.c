/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2005,2008 Oracle.  All rights reserved.
 *
 * $Id: os_abort.c,v 1.9 2008/01/08 20:58:44 bostic Exp $
 */

#include "db_config.h"

#include "db_int.h"

/*
 * __os_abort --
 */
void
__os_abort(env)
	ENV *env;
{
	AEEApplet *app;

	COMPQUIET(env, NULL);

	app = (AEEApplet *)GETAPPINSTANCE();
	ISHELL_CloseApplet(app->m_pIShell, FALSE);

	/* NOTREACHED */
}
