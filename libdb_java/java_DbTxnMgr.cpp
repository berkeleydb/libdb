/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998
 *	Sleepycat Software.  All rights reserved.
 */
#include "config.h"

#ifndef lint
static const char sccsid[] = "@(#)java_DbTxnMgr.cpp	10.3 (Sleepycat) 10/24/98";
#endif /* not lint */

#include <jni.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "db.h"
#include "java_util.h"
#include "com_sleepycat_db_DbTxnMgr.h"

JNIEXPORT jobject JNICALL Java_com_sleepycat_db_DbTxnMgr_begin
  (JNIEnv *jnienv, jobject jthis, /*DbTxn*/ jobject pid)
{
    int err;
    DB_TXNMGR *dbtxnmgr = get_DB_TXNMGR(jnienv, jthis);
    if (!verify_non_null(jnienv, dbtxnmgr))
        return 0;
    DB_TXN *dbpid = get_DB_TXN(jnienv, pid);
    DB_TXN *result = 0;

    err = txn_begin(dbtxnmgr, dbpid, &result);
    if (!verify_return(jnienv, err))
        return 0;
    return get_DbTxn(jnienv, result);
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbTxnMgr_checkpoint
  (JNIEnv *jnienv, jobject jthis, jint kbyte, jint min)
{
    int err;
    DB_TXNMGR *dbtxnmgr = get_DB_TXNMGR(jnienv, jthis);

    if (!verify_non_null(jnienv, dbtxnmgr))
        return;
    err = txn_checkpoint(dbtxnmgr, kbyte, min);
    verify_return(jnienv, err);
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbTxnMgr_close
  (JNIEnv *jnienv, jobject jthis)
{
    int err;
    DB_TXNMGR *dbtxnmgr = get_DB_TXNMGR(jnienv, jthis);

    if (!verify_non_null(jnienv, dbtxnmgr))
        return;
    err = txn_close(dbtxnmgr);
    if (verify_return(jnienv, err))
    {
        set_private_info(jnienv, name_DB_TXNMGR, jthis, 0);
    }
}

JNIEXPORT jobject JNICALL Java_com_sleepycat_db_DbTxnMgr_stat
  (JNIEnv *jnienv, jobject jthis)
{
    int err;
    DB_TXNMGR *dbtxnmgr = get_DB_TXNMGR(jnienv, jthis);
    DB_TXN_STAT *statp = NULL;
    jobject retval = NULL;

    if (!verify_non_null(jnienv, dbtxnmgr))
        return NULL;

    // We cannot use the default allocator (on Win* platforms anyway)
    // because it often causes problems when we free storage
    // in a DLL that was allocated in another DLL.  Using
    // our own allocator (ours just calls malloc!) ensures
    // that there is no mismatch.
    //
    err = txn_stat(dbtxnmgr, &statp, allocMemory);
    if (verify_return(jnienv, err)) {
        retval = create_default_object(jnienv, name_DB_TXN_STAT);
        jclass dbclass = get_class(jnienv, name_DB_TXN_STAT);

        // Set the individual fields
//TODO!
/*    public static class Active {
        public int txnid;               // Transaction ID
        public DbLsn lsn;               // Lsn of the begin record
    };
*/
        set_lsn_field(jnienv, dbclass, retval,
                      "st_last_ckp", statp->st_last_ckp);
        set_lsn_field(jnienv, dbclass, retval,
                      "st_pending_ckp", statp->st_pending_ckp);
        set_long_field(jnienv, dbclass, retval,
                       "st_time_ckp", statp->st_time_ckp);
        set_int_field(jnienv, dbclass, retval,
                      "st_last_txnid", statp->st_last_txnid);
        set_int_field(jnienv, dbclass, retval,
                      "st_maxtxns", statp->st_maxtxns);
        set_int_field(jnienv, dbclass, retval,
                      "st_naborts", statp->st_naborts);
        set_int_field(jnienv, dbclass, retval,
                      "st_nbegins", statp->st_nbegins);
        set_int_field(jnienv, dbclass, retval,
                      "st_ncommits", statp->st_ncommits);
        set_int_field(jnienv, dbclass, retval,
                      "st_nactive", statp->st_nactive);

        jclass active_class = get_class(jnienv, name_DB_TXN_STAT_ACTIVE);
        jobjectArray actives = jnienv->NewObjectArray(statp->st_nactive,
                                                      active_class, 0);

        // Set the st_txnarray field.  This is a little involved
        // than other fields, since the type is an array, so none
        // of our utility functions help.
        //
        //   [Lcom/sleepycat/db/DbTxnStat$Active;
        //
        char active_signature[512];

        strcpy(active_signature, "[L");
        strcat(active_signature, DB_PACKAGE_NAME);
        strcat(active_signature, name_DB_TXN_STAT_ACTIVE);
        strcat(active_signature, ";");

        jfieldID arrid  = jnienv->GetFieldID(dbclass, "st_txnarray",
                                             active_signature);
        jnienv->SetObjectField(retval, arrid, actives);

        // Now fill the in the elements of st_txnarray.
        //
        for (int i=0; i<statp->st_nactive; i++) {
            jobject obj = create_default_object(jnienv, name_DB_TXN_STAT_ACTIVE);
            jnienv->SetObjectArrayElement(actives, i, obj);

            set_int_field(jnienv, active_class, obj,
                          "txnid", statp->st_txnarray[i].txnid);
            set_lsn_field(jnienv, active_class, obj,
                          "lsn", statp->st_txnarray[i].lsn);
        }
        set_int_field(jnienv, dbclass, retval,
                      "st_region_wait", statp->st_region_wait);
        set_int_field(jnienv, dbclass, retval,
                      "st_region_nowait", statp->st_region_nowait);
        set_int_field(jnienv, dbclass, retval,
                      "st_refcnt", statp->st_refcnt);
        set_int_field(jnienv, dbclass, retval,
                      "st_regsize", statp->st_regsize);

        freeMemory(statp);
    }
    return retval;
}

JNIEXPORT jobject JNICALL Java_com_sleepycat_db_DbTxnMgr_open
  (JNIEnv *jnienv, jclass jthis_class, jstring dir,
   jint flags, jint mode, jobject dbenv)
{
    int err;
    jobject retval = NULL;
    DB_TXNMGR *dbtxnmgr;
    DB_ENV *db_dbenv = get_DB_ENV(jnienv, dbenv);
    LockedString dbdir(jnienv, dir);

    if (verify_non_null(jnienv, db_dbenv)) {
        err = txn_open(dbdir.string, flags, mode, db_dbenv, &dbtxnmgr);
        if (verify_return(jnienv, err)) {
            retval = create_default_object(jnienv, name_DB_TXNMGR);
            set_private_info(jnienv, name_DB_TXNMGR, retval, dbtxnmgr);
        }
    }
    return retval;
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbTxnMgr_unlink
  (JNIEnv *jnienv, jclass jthis_class, jstring dir, jint force,
   /*DbEnv*/ jobject dbenv)
{
    int err;
    DB_ENV *db_dbenv = get_DB_ENV(jnienv, dbenv);
    LockedString dbdir(jnienv, dir);

    if (verify_non_null(jnienv, db_dbenv)) {
        err = txn_unlink(dbdir.string, force, db_dbenv);
        verify_return(jnienv, err);
    }
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbTxnMgr_finalize
  (JNIEnv *jnienv, jobject jthis)
{
    DB_TXNMGR *dbtxnmgr = get_DB_TXNMGR(jnienv, jthis);
    if (dbtxnmgr) {
        // Free any data related to DB_TXNMGR here
    }
}
