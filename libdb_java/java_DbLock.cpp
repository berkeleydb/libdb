/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998
 *	Sleepycat Software.  All rights reserved.
 */
#include "config.h"

#ifndef lint
static const char sccsid[] = "@(#)java_DbLock.cpp	10.3 (Sleepycat) 10/18/98";
#endif /* not lint */

#include <jni.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "db.h"
#include "java_util.h"
#include "com_sleepycat_db_DbLock.h"

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbLock_put
  (JNIEnv *jnienv, jobject jthis, /*DbLockTab*/ jobject locktab)
{
    int err;
    DB_LOCK dblock = get_DB_LOCK(jnienv, jthis);
    DB_LOCKTAB *dblocktab = get_DB_LOCKTAB(jnienv, locktab);

    if (!verify_non_null(jnienv, dblocktab))
        return;

    err = lock_put(dblocktab, dblock);
    if (verify_return(jnienv, err))
    {
        set_private_info(jnienv, name_DB_LOCK, jthis, 0);
    }
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbLock_finalize
  (JNIEnv *jnienv, jobject jthis)
{
    // no data related to DB_LOCK needs to be freed.
    set_private_info(jnienv, name_DB_LOCK, jthis, 0);
}
