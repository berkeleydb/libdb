/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998
 *	Sleepycat Software.  All rights reserved.
 */
#include "config.h"

#ifndef lint
static const char sccsid[] = "@(#)java_Db.cpp	10.11 (Sleepycat) 12/7/98";
#endif /* not lint */

#include <jni.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "db.h"
#include "java_util.h"
#include "com_sleepycat_db_Db.h"

JNIEXPORT void JNICALL Java_com_sleepycat_db_Db_close
  (JNIEnv *jnienv, /*Db*/ jobject jthis, jint flags)
{
    int err;
    DB *db = get_DB(jnienv, jthis);

    if (!verify_non_null(jnienv, db))
        return;
    err = db->close(db, flags);
    if (verify_return(jnienv, err))
    {
        set_private_info(jnienv, name_DB, jthis, 0);
    }
}

JNIEXPORT jobject JNICALL Java_com_sleepycat_db_Db_cursor
  (JNIEnv *jnienv, /*Db*/ jobject jthis, /*DbTxn*/ jobject txnid, jint flags)
{
    int err;
    DB *db = get_DB(jnienv, jthis);
    DB_TXN *dbtxnid = get_DB_TXN(jnienv, txnid);

    if (!verify_non_null(jnienv, db))
        return NULL;
    DBC *dbc;
    err = db->cursor(db, dbtxnid, &dbc, flags);
    verify_return(jnienv, err);
    return get_Dbc(jnienv, dbc);
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_Db_del
  (JNIEnv *jnienv, /*Db*/ jobject jthis, /*DbTxn*/ jobject txnid,
   /*Dbt*/ jobject key, jint dbflags)
{
    int err;
    DB *db = get_DB(jnienv, jthis);
    if (!verify_non_null(jnienv, db))
        return;
    DB_TXN *dbtxnid = get_DB_TXN(jnienv, txnid);
    LockedDBT dbkey(jnienv, key, inOp);
    if (dbkey.has_error())
        return;

    err = db->del(db, dbtxnid, dbkey.dbt, dbflags);
    if (err != DB_NOTFOUND) {
        verify_return(jnienv, err);
    }
}

JNIEXPORT jint JNICALL Java_com_sleepycat_db_Db_fd
  (JNIEnv *jnienv, /*Db*/ jobject jthis)
{
    int err;
    int return_value = 0;
    DB *db = get_DB(jnienv, jthis);

    if (!verify_non_null(jnienv, db))
        return 0;
    err = db->fd(db, &return_value);
    verify_return(jnienv, err);
    return return_value;
}

JNIEXPORT jint JNICALL Java_com_sleepycat_db_Db_get
  (JNIEnv *jnienv, /*Db*/ jobject jthis, /*DbTxn*/ jobject txnid,
   /*Dbt*/ jobject key, /*Dbt*/ jobject data, jint flags)
{
    int err;
    DB *db = get_DB(jnienv, jthis);
    DB_TXN *dbtxnid = get_DB_TXN(jnienv, txnid);
    LockedDBT dbkey(jnienv, key, inOp);
    if (dbkey.has_error())
        return 0;
    LockedDBT dbdata(jnienv, data, outOp);
    if (dbdata.has_error())
        return 0;

    if (!verify_non_null(jnienv, db))
        return 0;
    err = db->get(db, dbtxnid, dbkey.dbt, dbdata.dbt, flags);
    if (err != DB_NOTFOUND) {
        verify_return(jnienv, err);
    }
    return err;
}

JNIEXPORT jobject JNICALL Java_com_sleepycat_db_Db_join
  (JNIEnv *jnienv, /*Db*/ jobject jthis, /*Dbc[]*/ jobjectArray curslist,
   jint flags)
{
    int err;
    DB *db = get_DB(jnienv, jthis);
    int count = jnienv->GetArrayLength(curslist);
    DBC **newlist = NEW_ARRAY(DBC *, count+1);
    DBC *dbc;

    // Convert the java array of Dbc's to a C array of DBC's.
    //
    for (int i=0; i<count; i++) {
        jobject jobj = jnienv->GetObjectArrayElement(curslist, i);
        if (jobj == 0) {
            //
            // An embedded null in the array is treated as an endpoint.
            //
            newlist[i] = 0;
            break;
        }
        else {
            newlist[i] = get_DBC(jnienv, jobj);
        }
    }
    newlist[count] = 0;

    if (!verify_non_null(jnienv, db))
        return NULL;
    err = db->join(db, newlist, flags, &dbc);
    DELETE(newlist);
    verify_return(jnienv, err);
    return get_Dbc(jnienv, dbc);
}

JNIEXPORT jint JNICALL Java_com_sleepycat_db_Db_put
  (JNIEnv *jnienv, /*Db*/ jobject jthis, /*DbTxn*/ jobject txnid,
   /*Dbt*/ jobject key, /*Dbt*/ jobject data, jint flags)
{
    int err;
    DB *db = get_DB(jnienv, jthis);
    DB_TXN *dbtxnid = get_DB_TXN(jnienv, txnid);
    LockedDBT dbkey(jnienv, key, inOp);
    if (dbkey.has_error())
        return 0;
    LockedDBT dbdata(jnienv, data, inOp);
    if (dbdata.has_error())
        return 0;

    if (!verify_non_null(jnienv, db))
        return 0;
    err = db->put(db, dbtxnid, dbkey.dbt, dbdata.dbt, flags);
    if (err != DB_KEYEXIST) {
        verify_return(jnienv, err);
    }
    return err;
}

JNIEXPORT jobject JNICALL Java_com_sleepycat_db_Db_stat
  (JNIEnv *jnienv, jobject jthis, jint flags)
{
    int err;
    DB *db = get_DB(jnienv, jthis);
    DB_BTREE_STAT *statp = 0;
    jobject retval = NULL;

    if (!verify_non_null(jnienv, db))
        return NULL;

    // We cannot use the default allocator (on Win* platforms anyway)
    // because it often causes problems when we free storage
    // in a DLL that was allocated in another DLL.  Using
    // our own allocator (ours just calls malloc!) ensures
    // that there is no mismatch.
    //
    err = db->stat(db, &statp, allocMemory, flags);
    if (verify_return(jnienv, err)) {
        retval = create_default_object(jnienv, name_DB_BTREE_STAT);
        jclass dbclass = get_class(jnienv, name_DB_BTREE_STAT);

        // Set the individual fields
        set_int_field(jnienv, dbclass, retval,
                      "bt_flags", statp->bt_flags);
        set_int_field(jnienv, dbclass, retval,
                      "bt_maxkey", statp->bt_maxkey);
        set_int_field(jnienv, dbclass, retval,
                      "bt_minkey", statp->bt_minkey);
        set_int_field(jnienv, dbclass, retval,
                      "bt_re_len", statp->bt_re_len);
        set_int_field(jnienv, dbclass, retval,
                      "bt_re_pad", statp->bt_re_pad);
        set_int_field(jnienv, dbclass, retval,
                      "bt_pagesize", statp->bt_pagesize);
        set_int_field(jnienv, dbclass, retval,
                      "bt_levels", statp->bt_levels);
        set_int_field(jnienv, dbclass, retval,
                      "bt_nrecs", statp->bt_nrecs);
        set_int_field(jnienv, dbclass, retval,
                      "bt_int_pg", statp->bt_int_pg);
        set_int_field(jnienv, dbclass, retval,
                      "bt_leaf_pg", statp->bt_leaf_pg);
        set_int_field(jnienv, dbclass, retval,
                      "bt_dup_pg", statp->bt_dup_pg);
        set_int_field(jnienv, dbclass, retval,
                      "bt_over_pg", statp->bt_over_pg);
        set_int_field(jnienv, dbclass, retval,
                      "bt_free", statp->bt_free);
        set_int_field(jnienv, dbclass, retval,
                      "bt_int_pgfree", statp->bt_int_pgfree);
        set_int_field(jnienv, dbclass, retval,
                      "bt_leaf_pgfree", statp->bt_leaf_pgfree);
        set_int_field(jnienv, dbclass, retval,
                      "bt_dup_pgfree", statp->bt_dup_pgfree);
        set_int_field(jnienv, dbclass, retval,
                      "bt_over_pgfree", statp->bt_over_pgfree);
        set_int_field(jnienv, dbclass, retval,
                      "bt_magic", statp->bt_magic);
        set_int_field(jnienv, dbclass, retval,
                      "bt_version", statp->bt_version);

        freeMemory(statp);
    }
    return retval;
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_Db_sync
  (JNIEnv *jnienv, /*Db*/ jobject jthis, jint flags)
{
    int err;
    DB *db = get_DB(jnienv, jthis);

    if (!verify_non_null(jnienv, db))
        return;
    err = db->sync(db, flags);
    verify_return(jnienv, err);
}

JNIEXPORT jboolean JNICALL Java_com_sleepycat_db_Db_get_1byteswapped
  (JNIEnv *jnienv, /*Db*/ jobject jthis)
{
    DB *db = get_DB(jnienv, jthis);
    if (!verify_non_null(jnienv, db))
        return 0;

    return db->byteswapped ? 1 : 0;
}

JNIEXPORT jint JNICALL Java_com_sleepycat_db_Db_get_1type
  (JNIEnv *jnienv, /*Db*/ jobject jthis)
{
    DB *db = get_DB(jnienv, jthis);
    if (!verify_non_null(jnienv, db))
        return 0;

    return (jint)db->type;
}

JNIEXPORT jobject JNICALL Java_com_sleepycat_db_Db_open
  (JNIEnv *jnienv, /*static Db*/ jclass /*jthis_class*/,
   jstring fname, jint type, jint flags, jint mode,
   /*DbEnv*/ jobject dbenv, /*DbInfo*/ jobject info)
{
    int err;
    jobject retval = NULL;
    DB *db;
    DB_ENV *db_dbenv = get_DB_ENV(jnienv, dbenv);
    DB_INFO *dbinfo = get_DB_INFO(jnienv, info);
    LockedString dbfname(jnienv, fname);

    if (verify_non_null(jnienv, db_dbenv)) {
        flags |= DB_THREAD;
        err = db_open(dbfname.string, (DBTYPE)type, flags, mode,
                      db_dbenv, dbinfo, &db);
        if (verify_return(jnienv, err)) {
            db->db_malloc = java_db_malloc;
            retval = create_default_object(jnienv, name_DB);
            set_private_info(jnienv, name_DB, retval, db);
        }
    }
    return retval;
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_Db_finalize
  (JNIEnv *jnienv, jobject jthis)
{
    DB *db = get_DB(jnienv, jthis);
    if (db) {
        // Free any info related to DB here.
        // Unfortunately, we cannot really autoclose because
        // finalize is not called in any particular order
    }
}
