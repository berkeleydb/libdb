/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998, 1999, 2000
 *	Sleepycat Software.  All rights reserved.
 */
#include "db_config.h"

#ifndef lint
static const char revid[] = "$Id: java_DbLsn.c,v 11.3 2000/05/25 04:18:11 dda Exp $";
#endif /* not lint */

#include <jni.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>              /* needed for FILENAME_MAX */

#include "db.h"
#include "java_util.h"
#include "com_sleepycat_db_DbLsn.h"

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbLsn_init_1lsn
  (JNIEnv *jnienv, /*DbLsn*/ jobject jthis)
{
	DB_LSN *lsn;

	lsn = (DB_LSN *)malloc(sizeof(DB_LSN));
	memset(lsn, 0, sizeof(DB_LSN));
	set_private_dbobj(jnienv, name_DB_LSN, jthis, lsn);
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbLsn_finalize
  (JNIEnv *jnienv, jobject jthis)
{
	DB_LSN *dblsn;

	dblsn = get_DB_LSN(jnienv, jthis);
	if (dblsn) {
		free(dblsn);
	}
}
