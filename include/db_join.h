/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 *
 *	@(#)db_join.h	11.1 (Sleepycat) 7/25/99
 */

#ifndef _DB_JOIN_H
#define _DB_JOIN_H

/*
 * Joins use a join cursor that is similar to a regular DB cursor except
 * that it only supports c_get and c_close functionality.  Also, it does
 * not support the full range of flags for get.
 */
typedef struct __join_cursor {
	u_int8_t *j_exhausted;	/* Array of flags; is cursor i exhausted? */
	DBC 	**j_curslist;	/* Array of cursors in the join: constant. */
	DBC	**j_fdupcurs;	/* Cursors w/ first intances of current dup. */
	DBC	**j_workcurs;	/* Scratch cursor copies to muck with. */
	DB	 *j_primary;	/* Primary dbp. */
	DBT	  j_key;	/* Used to do lookups. */
} JOIN_CURSOR;

#endif
