/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997
 *	Sleepycat Software.  All rights reserved.
 */
#include "config.h"

#ifndef lint
static const char sccsid[] = "@(#)java_DbMpool.cpp	10.2 (Sleepycat) 11/20/97";
#endif /* not lint */

#include <jni.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "db.h"
#include "java_util.h"
#include "com_sleepycat_db_DbMpool.h"

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbMpool_close
  (JNIEnv *jnienv, jobject jthis)
{
    int err;
    DB_MPOOL *dbmpool = get_DB_MPOOL(jnienv, jthis);

    if (!verify_non_null(jnienv, dbmpool))
        return;
    err = memp_close(dbmpool);
    if (verify_return(jnienv, err))
    {
        set_private_info(jnienv, name_DB_MPOOL, jthis, 0);
    }
}

JNIEXPORT jobject JNICALL Java_com_sleepycat_db_DbMpool_stat
  (JNIEnv *jnienv, jobject jthis)
{
    int err;
    DB_MPOOL *dbmpool = get_DB_MPOOL(jnienv, jthis);
    DB_MPOOL_STAT *statp = 0;

    if (!verify_non_null(jnienv, dbmpool))
        return 0;

    err = memp_stat(dbmpool, &statp, 0, 0);
    verify_return(jnienv, err);
    return get_DbMpoolStat(jnienv, statp);
}

JNIEXPORT jobjectArray JNICALL Java_com_sleepycat_db_DbMpool_fstat
  (JNIEnv *jnienv, jobject jthis)
{
    int err;
    DB_MPOOL *dbmpool = get_DB_MPOOL(jnienv, jthis);
    DB_MPOOL_FSTAT **fstatp = 0;

    if (!verify_non_null(jnienv, dbmpool))
        return 0;

    err = memp_stat(dbmpool, 0, &fstatp, 0);
    if (!verify_return(jnienv, err))
        return 0;

    int len = 0;
    while (fstatp[len])
        len++;
    jclass fstat_class = get_class(jnienv, name_DB_MPOOL_FSTAT);
    jobjectArray array = jnienv->NewObjectArray(len, fstat_class, 0);
    for (int i=0; i<len; i++) {
        jobject obj = get_DbMpoolFStat(jnienv, fstatp[i]);
        jnienv->SetObjectArrayElement(array, i, obj);
    }

    // Free the array of pointers now.  The individual
    // DB_MPOOL_FSTAT pointers will be be freed when
    // each DbMpoolFStat object is GC'd.
    //
    free (fstatp);
    return array;
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbMpool_sync
  (JNIEnv *jnienv, jobject jthis, /*DbLsn*/ jobject lsn)
{
    int err;
    DB_MPOOL *dbmpool = get_DB_MPOOL(jnienv, jthis);
    DB_MPOOL_STAT *statp = 0;
    DB_LSN *dblsn = get_DB_LSN(jnienv, lsn);

    if (!verify_non_null(jnienv, dbmpool))
        return;

    err = memp_sync(dbmpool, dblsn);
    verify_return(jnienv, err);
}

JNIEXPORT jobject JNICALL Java_com_sleepycat_db_DbMpool_open
  (JNIEnv *jnienv, jclass jthis_class, jstring dir,
   jint flags, jint mode, /*DbEnv*/ jobject dbenv)
{
    int err;
    jobject retval = NULL;
    DB_MPOOL *dbmpool;
    DB_ENV *db_dbenv = get_DB_ENV(jnienv, dbenv);
    LockedString dbdir(jnienv, dir);

    if (verify_non_null(jnienv, db_dbenv)) {
        err = memp_open(dbdir.string, flags, mode,
                      db_dbenv, &dbmpool);
        if (verify_return(jnienv, err)) {
            retval = create_default_object(jnienv, name_DB_MPOOL);
            set_private_info(jnienv, name_DB_MPOOL, retval, dbmpool);
        }
    }
    return retval;
}

JNIEXPORT jint JNICALL Java_com_sleepycat_db_DbMpool_trickle
  (JNIEnv *jnienv, jobject jthis, jint pct)
{
    int err;
    DB_MPOOL *dbmpool = get_DB_MPOOL(jnienv, jthis);
    int result = 0;

    if (verify_non_null(jnienv, dbmpool)) {
        err = memp_trickle(dbmpool, pct, &result);
        verify_return(jnienv, err);
    }
    return result;
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbMpool_unlink
  (JNIEnv *jnienv, jclass jthis_class, jstring dir, jint force,
   /*DbEnv*/ jobject dbenv)
{
    int err;
    DB_ENV *db_dbenv = get_DB_ENV(jnienv, dbenv);
    LockedString dbdir(jnienv, dir);

    if (verify_non_null(jnienv, db_dbenv)) {
        err = memp_unlink(dbdir.string, force, db_dbenv);
        verify_return(jnienv, err);
    }
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbMpool_finalize
  (JNIEnv *jnienv, jobject jthis)
{
    DB_MPOOL *dbmpool = get_DB_MPOOL(jnienv, jthis);
    if (dbmpool) {
        // Free any data related to DB_MPOOL here
    }
}
