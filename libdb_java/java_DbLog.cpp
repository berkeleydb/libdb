/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998
 *	Sleepycat Software.  All rights reserved.
 */
#include "config.h"

#ifndef lint
static const char sccsid[] = "@(#)java_DbLog.cpp	10.7 (Sleepycat) 10/24/98";
#endif /* not lint */

#include <jni.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>              // needed for FILENAME_MAX

#include "db.h"
#include "java_util.h"
#include "com_sleepycat_db_DbLog.h"

JNIEXPORT jobjectArray JNICALL Java_com_sleepycat_db_DbLog_archive
  (JNIEnv *jnienv, /*DbLog*/ jobject jthis, jint flags)
{
    int err;
    char** ret;
    DB_LOG *dblog = get_DB_LOG(jnienv, jthis);

    if (!verify_non_null(jnienv, dblog))
        return 0;
    err = log_archive(dblog, &ret, flags, 0);
    if (!verify_return(jnienv, err))
        return 0;

    int len = 0;
    while (ret[len] != NULL)
        len++;
    jclass stringClass = jnienv->FindClass("java/lang/String");
    jobjectArray strarray = jnienv->NewObjectArray(len, stringClass, 0);
    for (int i=0; i<len; i++) {
        jstring str = jnienv->NewStringUTF(ret[i]);
        jnienv->SetObjectArrayElement(strarray, i, str);
    }
    return strarray;
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbLog_close
  (JNIEnv *jnienv, /*DbLog*/ jobject jthis)
{
    int err;
    DB_LOG *dblog = get_DB_LOG(jnienv, jthis);

    if (!verify_non_null(jnienv, dblog))
        return;
    err = log_close(dblog);
    if (verify_return(jnienv, err))
    {
        set_private_info(jnienv, name_DB_LOG, jthis, 0);
    }
}

JNIEXPORT jint JNICALL Java_com_sleepycat_db_DbLog_compare
  (JNIEnv *jnienv, jclass jthis_class,
   /*DbLsn*/ jobject lsn0, /*DbLsn*/ jobject lsn1)
{
    DB_LSN *dblsn0 = get_DB_LSN(jnienv, lsn0);
    DB_LSN *dblsn1 = get_DB_LSN(jnienv, lsn1);

    return log_compare(dblsn0, dblsn1);
}

JNIEXPORT jstring JNICALL Java_com_sleepycat_db_DbLog_file
  (JNIEnv *jnienv, jobject jthis, /*DbLsn*/ jobject lsn)
{
    int err;
    DB_LOG *dblog = get_DB_LOG(jnienv, jthis);
    DB_LSN *dblsn = get_DB_LSN(jnienv, lsn);
    char filename[FILENAME_MAX+1] = "";

    err = log_file(dblog, dblsn, filename, FILENAME_MAX);
    verify_return(jnienv, err);
    filename[FILENAME_MAX] = '\0'; // just to be sure
    return get_java_string(jnienv, filename);
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbLog_flush
  (JNIEnv *jnienv, jobject jthis, /*DbLsn*/ jobject lsn)
{
    int err;
    DB_LOG *dblog = get_DB_LOG(jnienv, jthis);
    DB_LSN *dblsn = get_DB_LSN(jnienv, lsn);

    err = log_flush(dblog, dblsn);
    verify_return(jnienv, err);
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbLog_get
  (JNIEnv *jnienv, jobject jthis, /*DbLsn*/ jobject lsn,
   /*DbDbt*/ jobject data, jint flags)
{
    int err;
    DB_LOG *dblog = get_DB_LOG(jnienv, jthis);
    DB_LSN *dblsn = get_DB_LSN(jnienv, lsn);
    LockedDBT dbdata(jnienv, data, outOp);
    if (dbdata.has_error())
        return;

    err = log_get(dblog, dblsn, dbdata.dbt, flags);
    verify_return(jnienv, err);
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbLog_put
  (JNIEnv *jnienv, jobject jthis, /*DbLsn*/ jobject lsn,
   /*DbDbt*/ jobject data, jint flags)
{
    int err;
    DB_LOG *dblog = get_DB_LOG(jnienv, jthis);
    DB_LSN *dblsn = get_DB_LSN(jnienv, lsn);
    LockedDBT dbdata(jnienv, data, inOp);
    if (dbdata.has_error())
        return;

    err = log_put(dblog, dblsn, dbdata.dbt, flags);
    verify_return(jnienv, err);
}

JNIEXPORT jint JNICALL Java_com_sleepycat_db_DbLog_db_1register
  (JNIEnv *jnienv, jobject jthis, /*Db*/ jobject dbp,
   jstring name, jint dbtype)
{
    int err;
    DB_LOG *dblog = get_DB_LOG(jnienv, jthis);
    DB *dbdb = get_DB(jnienv, dbp);
    LockedString dbname(jnienv, name);
    u_int32_t result;

    err = log_register(dblog, dbdb, dbname.string, (DBTYPE)dbtype, &result);
    verify_return(jnienv, err);
    return result;
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbLog_db_1unregister
  (JNIEnv *jnienv, jobject jthis, jint fid)
{
    int err;
    DB_LOG *dblog = get_DB_LOG(jnienv, jthis);

    err = log_unregister(dblog, fid);
    verify_return(jnienv, err);
}

JNIEXPORT jobject JNICALL Java_com_sleepycat_db_DbLog_stat
  (JNIEnv *jnienv, jobject jthis)
{
    int err;
    DB_LOG *dblog = get_DB_LOG(jnienv, jthis);
    DB_LOG_STAT *statp = NULL;
    jobject retval = NULL;

    if (!verify_non_null(jnienv, dblog))
        return NULL;

    // We cannot use the default allocator (on Win* platforms anyway)
    // because it often causes problems when we free storage
    // in a DLL that was allocated in another DLL.  Using
    // our own allocator (ours just calls malloc!) ensures
    // that there is no mismatch.
    //
    err = log_stat(dblog, &statp, allocMemory);
    if (verify_return(jnienv, err)) {
        retval = create_default_object(jnienv, name_DB_LOG_STAT);
        jclass dbclass = get_class(jnienv, name_DB_LOG_STAT);

        // Set the individual fields
        set_int_field(jnienv, dbclass, retval,
                      "st_magic", statp->st_magic);
        set_int_field(jnienv, dbclass, retval,
                      "st_version", statp->st_version);
        set_int_field(jnienv, dbclass, retval,
                      "st_mode", statp->st_mode);
        set_int_field(jnienv, dbclass, retval,
                      "st_lg_max", statp->st_lg_max);
        set_int_field(jnienv, dbclass, retval,
                      "st_w_bytes", statp->st_w_bytes);
        set_int_field(jnienv, dbclass, retval,
                      "st_w_mbytes", statp->st_w_mbytes);
        set_int_field(jnienv, dbclass, retval,
                      "st_wc_bytes", statp->st_wc_bytes);
        set_int_field(jnienv, dbclass, retval,
                      "st_wc_mbytes", statp->st_wc_mbytes);
        set_int_field(jnienv, dbclass, retval,
                      "st_wcount", statp->st_wcount);
        set_int_field(jnienv, dbclass, retval,
                      "st_scount", statp->st_scount);
        set_int_field(jnienv, dbclass, retval,
                      "st_region_wait", statp->st_region_wait);
        set_int_field(jnienv, dbclass, retval,
                      "st_region_nowait", statp->st_region_nowait);
        set_int_field(jnienv, dbclass, retval,
                      "st_cur_file", statp->st_cur_file);
        set_int_field(jnienv, dbclass, retval,
                      "st_cur_offset", statp->st_cur_offset);
        set_int_field(jnienv, dbclass, retval,
                      "st_refcnt", statp->st_refcnt);
        set_int_field(jnienv, dbclass, retval,
                      "st_regsize", statp->st_regsize);

        freeMemory(statp);
    }
    return retval;
}

JNIEXPORT /*DbLog*/ jobject JNICALL Java_com_sleepycat_db_DbLog_open
  (JNIEnv *jnienv, jclass jthis_class, jstring dir, jint flags,
   jint mode, /*DbEnv*/ jobject dbenv)
{
    int err;
    jobject retval = NULL;
    DB_LOG *dblog;
    DB_ENV *db_dbenv = get_DB_ENV(jnienv, dbenv);
    LockedString dbdir(jnienv, dir);

    if (verify_non_null(jnienv, db_dbenv)) {
        err = log_open(dbdir.string, flags, mode,
                      db_dbenv, &dblog);
        if (verify_return(jnienv, err)) {
            retval = create_default_object(jnienv, name_DB_LOG);
            set_private_info(jnienv, name_DB_LOG, retval, dblog);
        }
    }
    return retval;
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbLog_unlink
  (JNIEnv *jnienv, jclass jthis_class, jstring dir, jint force,
   /*DbEnv*/ jobject dbenv)
{
    int err;
    DB_ENV *db_dbenv = get_DB_ENV(jnienv, dbenv);
    LockedString dbdir(jnienv, dir);

    if (verify_non_null(jnienv, db_dbenv)) {
        err = log_unlink(dbdir.string, force, db_dbenv);
        verify_return(jnienv, err);
    }
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbLog_finalize
  (JNIEnv *jnienv, jobject jthis)
{
    DB_LOG *dblog = get_DB_LOG(jnienv, jthis);
    if (dblog) {
        // Free any data related to DB_LOG here
    }
}
