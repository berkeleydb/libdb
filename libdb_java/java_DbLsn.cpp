/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */
#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)java_DbLsn.cpp	11.1 (Sleepycat) 7/25/99";
#endif /* not lint */

#include <jni.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>              // needed for FILENAME_MAX

#include "db.h"
#include "java_util.h"
#include "com_sleepycat_db_DbLsn.h"

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbLsn_init_1lsn
  (JNIEnv *jnienv, /*DbLsn*/ jobject jthis)
{
	DB_LSN *lsn = NEW(DB_LSN);
	memset(lsn, 0, sizeof(DB_LSN));
	set_private_info(jnienv, name_DB_LSN, jthis, lsn);
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbLsn_finalize
  (JNIEnv *jnienv, jobject jthis)
{
	DB_LSN *dblsn = get_DB_LSN(jnienv, jthis);
	if (dblsn) {
		DELETE(dblsn);
	}
}
