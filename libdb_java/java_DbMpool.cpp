/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998
 *	Sleepycat Software.  All rights reserved.
 */
#include "config.h"

#ifndef lint
static const char sccsid[] = "@(#)java_DbMpool.cpp	10.5 (Sleepycat) 10/24/98";
#endif /* not lint */

#include <jni.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "db.h"
#include "java_util.h"
#include "com_sleepycat_db_DbMpool.h"

JNIEXPORT jobject JNICALL Java_com_sleepycat_db_DbMpool_stat
  (JNIEnv *jnienv, jobject jthis)
{
    int err;
    DB_MPOOL *dbmpool = get_DB_MPOOL(jnienv, jthis);
    DB_MPOOL_STAT *statp = NULL;
    jobject retval = NULL;

    if (!verify_non_null(jnienv, dbmpool))
        return NULL;

    // We cannot use the default allocator (on Win* platforms anyway)
    // because it often causes problems when we free storage
    // in a DLL that was allocated in another DLL.  Using
    // our own allocator (ours just calls malloc!) ensures
    // that there is no mismatch.
    //
    err = memp_stat(dbmpool, &statp, 0, allocMemory);
    if (verify_return(jnienv, err)) {
        retval = create_default_object(jnienv, name_DB_MPOOL_STAT);
        jclass dbclass = get_class(jnienv, name_DB_MPOOL_STAT);

        set_int_field(jnienv, dbclass, retval,
                      "st_cachesize", statp->st_cachesize);
        set_int_field(jnienv, dbclass, retval,
                      "st_cache_hit", statp->st_cache_hit);
        set_int_field(jnienv, dbclass, retval,
                      "st_cache_miss", statp->st_cache_miss);
        set_int_field(jnienv, dbclass, retval,
                      "st_map", statp->st_map);
        set_int_field(jnienv, dbclass, retval,
                      "st_page_create", statp->st_page_create);
        set_int_field(jnienv, dbclass, retval,
                      "st_page_in", statp->st_page_in);
        set_int_field(jnienv, dbclass, retval,
                      "st_page_out", statp->st_page_out);
        set_int_field(jnienv, dbclass, retval,
                      "st_ro_evict", statp->st_ro_evict);
        set_int_field(jnienv, dbclass, retval,
                      "st_rw_evict", statp->st_rw_evict);
        set_int_field(jnienv, dbclass, retval,
                      "st_hash_buckets", statp->st_hash_buckets);
        set_int_field(jnienv, dbclass, retval,
                      "st_hash_searches", statp->st_hash_searches);
        set_int_field(jnienv, dbclass, retval,
                      "st_hash_longest", statp->st_hash_longest);
        set_int_field(jnienv, dbclass, retval,
                      "st_hash_examined", statp->st_hash_examined);
        set_int_field(jnienv, dbclass, retval,
                      "st_page_clean", statp->st_page_clean);
        set_int_field(jnienv, dbclass, retval,
                      "st_page_dirty", statp->st_page_dirty);
        set_int_field(jnienv, dbclass, retval,
                      "st_page_trickle", statp->st_page_trickle);
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

JNIEXPORT jobjectArray JNICALL Java_com_sleepycat_db_DbMpool_fstat
  (JNIEnv *jnienv, jobject jthis)
{
    int err;
    DB_MPOOL *dbmpool = get_DB_MPOOL(jnienv, jthis);
    DB_MPOOL_FSTAT **fstatp = NULL;
    jobjectArray retval = NULL;

    if (!verify_non_null(jnienv, dbmpool))
        return NULL;

    // We cannot use the default allocator (on Win* platforms anyway)
    // because it often causes problems when we free storage
    // in a DLL that was allocated in another DLL.  Using
    // our own allocator (ours just calls malloc!) ensures
    // that there is no mismatch.
    //
    err = memp_stat(dbmpool, 0, &fstatp, allocMemory);
    if (verify_return(jnienv, err)) {
        int len = 0;
        while (fstatp[len])
            len++;
        jclass fstat_class = get_class(jnienv, name_DB_MPOOL_FSTAT);
        retval = jnienv->NewObjectArray(len, fstat_class, 0);
        for (int i=0; i<len; i++) {
            jobject obj = create_default_object(jnienv, name_DB_MPOOL_FSTAT);
            jnienv->SetObjectArrayElement(retval, i, obj);

            // Set the string field.
            jfieldID filename_id  = jnienv->GetFieldID(fstat_class,
                                                       "file_name",
                                                       string_signature);
            jnienv->SetObjectField(obj, filename_id,
                                   get_java_string(jnienv, fstatp[i]->file_name));

            set_int_field(jnienv, fstat_class, obj,
                          "st_pagesize", fstatp[i]->st_pagesize);
            set_int_field(jnienv, fstat_class, obj,
                          "st_cache_hit", fstatp[i]->st_cache_hit);
            set_int_field(jnienv, fstat_class, obj,
                          "st_cache_miss", fstatp[i]->st_cache_miss);
            set_int_field(jnienv, fstat_class, obj,
                          "st_map", fstatp[i]->st_map);
            set_int_field(jnienv, fstat_class, obj,
                          "st_page_create", fstatp[i]->st_page_create);
            set_int_field(jnienv, fstat_class, obj,
                          "st_page_in", fstatp[i]->st_page_in);
            set_int_field(jnienv, fstat_class, obj,
                          "st_page_out", fstatp[i]->st_page_out);
            freeMemory(fstatp[i]);
        }
        freeMemory(fstatp);
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

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbMpool_finalize
  (JNIEnv *jnienv, jobject jthis)
{
    DB_MPOOL *dbmpool = get_DB_MPOOL(jnienv, jthis);
    if (dbmpool) {
        // Free any data related to DB_MPOOL here
    }
}
