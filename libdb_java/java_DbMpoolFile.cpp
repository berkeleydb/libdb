/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997
 *	Sleepycat Software.  All rights reserved.
 */
#include "config.h"

#ifndef lint
static const char sccsid[] = "@(#)java_DbMpoolFile.cpp	10.2 (Sleepycat) 1/18/98";
#endif /* not lint */

#include <jni.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "db.h"
#include "java_util.h"
#include "com_sleepycat_db_DbMpoolFile.h"

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbMpoolFile_close
  (JNIEnv *jnienv, jobject jthis)
{
    int err;
    DB_MPOOLFILE *dbmpoolfile = get_DB_MPOOLFILE(jnienv, jthis);

    if (!verify_non_null(jnienv, dbmpoolfile))
        return;
    err = memp_fclose(dbmpoolfile);
    if (verify_return(jnienv, err))
    {
        set_private_info(jnienv, name_DB_MPOOLFILE, jthis, 0);
    }
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbMpoolFile_sync
  (JNIEnv *jnienv, jobject jthis)
{
    int err;
    DB_MPOOLFILE *dbmpoolfile = get_DB_MPOOLFILE(jnienv, jthis);

    if (!verify_non_null(jnienv, dbmpoolfile))
        return;
    err = memp_fsync(dbmpoolfile);
    verify_return(jnienv, err);
}

JNIEXPORT jobject JNICALL Java_com_sleepycat_db_DbMpoolFile_open
  (JNIEnv *jnienv, jclass jthis_class, /*DbMpool*/ jobject mp,
   jstring dir, jint ftype, jint flags, jint mode, jlong pagesize,
   jint lsn_offset, /*Dbt*/ jobject pgcookie, jbyteArray uid)
{
    int err;
    jobject retval = NULL;
    DB_MPOOLFILE *dbmpf = 0;
    DB_MPOOL *dbmp = get_DB_MPOOL(jnienv, mp);
    LockedDBT dbpgcookie(jnienv, pgcookie, 0);
    if (dbpgcookie.has_error())
        return 0;
    LockedString dbdir(jnienv, dir);
    jbyte *dbuid = 0;

    if (verify_non_null(jnienv, dbmp)) {
        if (uid != 0) {
            dbuid = jnienv->GetByteArrayElements(uid, 0);
        }
        err = memp_fopen(dbmp, dbdir.string, ftype, flags, mode, pagesize,
                         lsn_offset, dbpgcookie.dbt,
                         (unsigned char*)dbuid, &dbmpf);
        if (verify_return(jnienv, err)) {
            retval = create_default_object(jnienv, name_DB_MPOOLFILE);
            set_private_info(jnienv, name_DB_MPOOLFILE, retval, dbmpf);
        }
    }
    if (uid != 0) {
        jnienv->ReleaseByteArrayElements(uid, dbuid, 0);
    }
    return retval;
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbMpoolFile_finalize
  (JNIEnv *jnienv, jobject jthis)
{
    DB_MPOOLFILE *dbmpoolfile = get_DB_MPOOLFILE(jnienv, jthis);
    if (dbmpoolfile) {
        // Free any data related to DB_MPOOLFILE here
        // Unfortunately, we cannot really autoclose because
        // finalize is not called in any particular order
    }
}
