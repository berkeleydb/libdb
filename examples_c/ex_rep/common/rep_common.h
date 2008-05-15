/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 2006,2008 Oracle.  All rights reserved.
 *
 * $Id: rep_common.h,v 12.6 2008/01/08 20:58:25 bostic Exp $
 */

/* Data shared by both repmgr and base versions of this program. */
typedef struct {
	int is_master;
} SHARED_DATA;

int create_env __P((const char *progname, DB_ENV **));
int doloop __P((DB_ENV *, SHARED_DATA *));
int env_init __P((DB_ENV *, const char *));
void usage __P((const char *));
