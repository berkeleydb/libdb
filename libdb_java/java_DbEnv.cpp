/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */
#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)java_DbEnv.cpp	11.5 (Sleepycat) 9/30/99";
#endif /* not lint */

#include <jni.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "db.h"
#include "java_util.h"
#include "com_sleepycat_db_DbEnv.h"

JAVADB_WO_ACCESS_METHOD(DbEnv, jint, lk_1max, DB_ENV, lk_max)
JAVADB_WO_ACCESS_METHOD(DbEnv, jint, lk_1detect, DB_ENV, lk_detect)
JAVADB_WO_ACCESS_METHOD(DbEnv, jint, lg_1bsize, DB_ENV, lg_bsize)
JAVADB_WO_ACCESS_METHOD(DbEnv, jint, lg_1max, DB_ENV, lg_max)
JAVADB_WO_ACCESS_METHOD(DbEnv, jlong, mp_1mmapsize, DB_ENV, mp_mmapsize)
JAVADB_WO_ACCESS_METHOD(DbEnv, jint, mutexlocks, DB_ENV, mutexlocks)
JAVADB_WO_ACCESS_METHOD(DbEnv, jint, pageyield, DB_ENV, pageyield)
JAVADB_WO_ACCESS_METHOD(DbEnv, jint, region_1init, DB_ENV, region_init)
JAVADB_WO_ACCESS_METHOD(DbEnv, jint, tas_1spins, DB_ENV, tas_spins)
JAVADB_WO_ACCESS_METHOD(DbEnv, jint, tx_1max, DB_ENV, tx_max)


static void DbEnv_errcall_callback(const char *prefix, char *message)
{
	DB_ENV_javainfo *envinfo = (DB_ENV_javainfo *)prefix;

	// Note: these error cases are "impossible", and would
	// normally warrant an exception.  However, without
	// a jnienv, we cannot throw an exception...
	// We don't want to trap or exit, since the point of
	// this facility is for the user to completely control
	// error situations.
	//
	if (envinfo == NULL) {
		// Something is *really* wrong here, the
		// prefix is set in every environment created.
		//
		fprintf(stderr, "Error callback failed!\n");
		fprintf(stderr, "error: %s\n", message);
		return;
	}

	// Should always succeed...
	JNIEnv *jnienv = envinfo->get_jnienv();

	if (jnienv == NULL) {

		// But just in case...
		fprintf(stderr, "Cannot attach to current thread!\n");
		fprintf(stderr, "error: %s\n", message);
		return;
	}

	jstring pre = envinfo->get_errpfx(jnienv);
	jstring msg = get_java_string(jnienv, message);
	jclass errcall_class = get_class(jnienv, name_DbErrcall);
	jmethodID id = jnienv->GetMethodID(errcall_class,
					   "errcall",
					   "(Ljava/lang/String;Ljava/lang/String;)V");
	if (!id)
		return;

	jnienv->CallVoidMethod(envinfo->get_errcall(), id, pre, msg);
}

static void DbEnv_initialize(JNIEnv *jnienv, DB_ENV *dbenv,
			     /*DbEnv*/ jobject jenv,
			     /*DbErrcall*/ jobject jerrcall,
			     int is_dbopen)
{
	jclass dbenvClass = get_class(jnienv, name_DB_ENV);

	DB_ENV_javainfo *envinfo =
		new DB_ENV_javainfo(jnienv, jenv, jerrcall, is_dbopen);
	dbenv->cj_internal = envinfo;
	dbenv->set_errpfx(dbenv, (const char*)envinfo);
	dbenv->set_errcall(dbenv, DbEnv_errcall_callback);

	set_private_info(jnienv, name_DB_ENV, jenv, dbenv);
}

static int DbEnv_internal_close(JNIEnv *jnienv, DB_ENV *dbenv, jint flags)
{
	// If an error occurs during the close, we want the
	// errcall mechanism to be in place to report any error.
	// But after the close, we can't peek inside the DB_ENV,
	// so we'll save a pointer to the internal info in the
	// meantime.
	//
	DB_ENV_javainfo *envinfo =
		(DB_ENV_javainfo*)(dbenv->cj_internal);
	int err = dbenv->close(dbenv, flags);

	envinfo->free_references(jnienv);
	delete envinfo;
	dbenv->cj_internal = NULL;
	return err;
}


JNIEXPORT void JNICALL Java_com_sleepycat_db_DbEnv_feedback_1changed
  (JNIEnv *jnienv, /*DbEnv*/ jobject jthis, /*DbFeedback*/ jobject jfeedback)
{
	DB_ENV *dbenv;
	DB_ENV_javainfo *dbenvinfo;

	dbenv = get_DB_ENV(jnienv, jthis);
	if (!verify_non_null(jnienv, dbenv))
		return;

	dbenvinfo = (DB_ENV_javainfo *)dbenv->cj_internal;
	dbenvinfo->set_feedback_object(jnienv, dbenv, jfeedback);
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbEnv__1init
  (JNIEnv *jnienv, /*DbEnv*/ jobject jthis, jobject /*DbErrcall*/ jerrcall,
   jint flags)
{
	int err;
	DB_ENV *dbenv;

	err = db_env_create(&dbenv, flags);
	if (verify_return(jnienv, err)) {
		DbEnv_initialize(jnienv, dbenv, jthis, jerrcall, 0);
	}
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbEnv__1init_1using_1db
  (JNIEnv *jnienv, /*DbEnv*/ jobject jthis, jobject /*DbErrcall*/ jerrcall,
   /*Db*/ jobject jdb)
{
	DB_ENV *dbenv;
	DB *db;

	db = get_DB(jnienv, jdb);
	dbenv = db->dbenv;
	DbEnv_initialize(jnienv, dbenv, jthis, jerrcall, 1);
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbEnv_open
  (JNIEnv *jnienv, /*DbEnv*/ jobject jthis, jstring db_home,
   jobjectArray db_config, jint flags, jint mode)
{
	int err;
	DB_ENV *dbenv = get_DB_ENV(jnienv, jthis);
	LockedString j_home(jnienv, db_home);
	LockedStringArray j_config(jnienv, db_config);

	// XXX: cast currently required because of const issues
	char *const *config_array = (char *const *)j_config.string_array;
	err = dbenv->open(dbenv, j_home.string, config_array, flags, mode);
	verify_return(jnienv, err, EXCEPTION_FILE_NOT_FOUND);
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbEnv_remove
  (JNIEnv *jnienv, /*DbEnv*/ jobject jthis, jstring db_home,
   /*String[]*/ jobjectArray db_config, jint flags)
{
	int err;
	DB_ENV *dbenv = get_DB_ENV(jnienv, jthis);
	LockedString j_home(jnienv, db_home);
	LockedStringArray j_config(jnienv, db_config);

	// XXX: cast currently required because of const issues
	char *const *config_array = (char *const *)j_config.string_array;
	err = dbenv->remove(dbenv, j_home.string, config_array, flags);
	verify_return(jnienv, err);
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbEnv_close
  (JNIEnv *jnienv, /*DbEnv*/ jobject jthis, jint flags)
{
	int err;
	DB_ENV *dbenv = get_DB_ENV(jnienv, jthis);
	if (!verify_non_null(jnienv, dbenv))
		return;

	err = DbEnv_internal_close(jnienv, dbenv, flags);

	// Null out the private data to indicate the DB_ENV is invalid.
	set_private_info(jnienv, name_DB_ENV, jthis, 0);

	// Throw an exception if the close failed.
	verify_return(jnienv, err);
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbEnv_err
  (JNIEnv *jnienv, /*DbEnv*/ jobject jthis, jint ecode, jstring msg)
{
	LockedString msg_string(jnienv, msg);
	DB_ENV *dbenv = get_DB_ENV(jnienv, jthis);
	if (!verify_non_null(jnienv, dbenv))
		return;

	dbenv->err(dbenv, ecode, msg_string.string);
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbEnv_errx
  (JNIEnv *jnienv, /*DbEnv*/ jobject jthis, jstring msg)
{
	LockedString msg_string(jnienv, msg);
	DB_ENV *dbenv = get_DB_ENV(jnienv, jthis);
	if (!verify_non_null(jnienv, dbenv))
		return;

	dbenv->errx(dbenv, msg_string.string);
}

JNIEXPORT jstring JNICALL Java_com_sleepycat_db_DbEnv_strerror
  (JNIEnv *jnienv, jclass /*jthis_class*/, jint ecode)
{
	const char *message = db_strerror(ecode);
	return get_java_string(jnienv, message);
}

JNIEXPORT void JNICALL
  Java_com_sleepycat_db_DbEnv_set_1cachesize
  (JNIEnv *jnienv, /*DbEnv*/ jobject jthis, jint gbytes, jint bytes,
   jint ncaches)
{
	DB_ENV *dbenv = get_DB_ENV(jnienv, jthis);

	if (verify_non_null(jnienv, dbenv)) {
		dbenv->set_cachesize(dbenv, gbytes, bytes, ncaches);
	}
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbEnv_recovery_1init_1changed
  (JNIEnv *jnienv, /*DbEnv*/ jobject jthis, /*DbRecoveryInit*/ jobject jrecoveryinit)
{
	DB_ENV *dbenv;
	DB_ENV_javainfo *dbenvinfo;

	dbenv = get_DB_ENV(jnienv, jthis);
	if (!verify_non_null(jnienv, dbenv))
		return;

	dbenvinfo = (DB_ENV_javainfo *)dbenv->cj_internal;
	dbenvinfo->set_recovery_init_object(jnienv, dbenv, jrecoveryinit);
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbEnv_set_1lk_1conflicts
  (JNIEnv *jnienv, /*DbEnv*/ jobject jthis, jobjectArray array)
{
	DB_ENV *dbenv = get_DB_ENV(jnienv, jthis);
	static const char * const array_length_msg =
		"array length does not match lk_modes";

	if (!verify_non_null(jnienv, dbenv))
		return;

	jsize len = jnienv->GetArrayLength(array);

	unsigned char *newarr = NEW_ARRAY(unsigned char, len * len);

	for (int i=0; i<len; i++) {
		jobject subArray = jnienv->GetObjectArrayElement(array, i);
		jnienv->GetByteArrayRegion((jbyteArray)subArray, 0, len,
					   (jbyte *)&newarr[i*len]);
	}
	DB_ENV_javainfo *envinfo =
		(DB_ENV_javainfo *)dbenv->cj_internal;
	envinfo->set_conflict(newarr);
	dbenv->set_lk_conflicts(dbenv, newarr, len);
}

JNIEXPORT void JNICALL
  Java_com_sleepycat_db_DbEnv_set_1verbose
  (JNIEnv *jnienv, /*DbEnv*/ jobject jthis, jint which, jint onoff)
{
	int err;
	DB_ENV *dbenv = get_DB_ENV(jnienv, jthis);

	if (verify_non_null(jnienv, dbenv)) {
		err = dbenv->set_verbose(dbenv, which, onoff);
	}

	// Throw an exception if the call failed.
	verify_return(jnienv, err);
}

JNIEXPORT jint JNICALL Java_com_sleepycat_db_DbEnv_get_1version_1major
  (JNIEnv * /*jnienv*/, jclass /*this_class*/)
{
	return DB_VERSION_MAJOR;
}

JNIEXPORT jint JNICALL Java_com_sleepycat_db_DbEnv_get_1version_1minor
  (JNIEnv * /*jnienv*/, jclass /*this_class*/)
{
	return DB_VERSION_MINOR;
}

JNIEXPORT jint JNICALL Java_com_sleepycat_db_DbEnv_get_1version_1patch
  (JNIEnv * /*jnienv*/, jclass /*this_class*/)
{
	return DB_VERSION_PATCH;
}

JNIEXPORT jstring JNICALL Java_com_sleepycat_db_DbEnv_get_1version_1string
  (JNIEnv *jnienv, jclass /*this_class*/)
{
	return jnienv->NewStringUTF(DB_VERSION_STRING);
}

JNIEXPORT jint JNICALL Java_com_sleepycat_db_DbEnv_lock_1id
  (JNIEnv *jnienv, /*DbEnv*/ jobject jthis)
{
	int err;
	DB_ENV *dbenv = get_DB_ENV(jnienv, jthis);

	if (!verify_non_null(jnienv, dbenv))
		return -1;
	u_int32_t id;
	err = lock_id(dbenv, &id);
	verify_return(jnienv, err);
	return id;
}

JNIEXPORT jobject JNICALL Java_com_sleepycat_db_DbEnv_lock_1stat
  (JNIEnv *jnienv, /*DbEnv*/ jobject jthis)
{
	int err;
	DB_ENV *dbenv = get_DB_ENV(jnienv, jthis);
	DB_LOCK_STAT *statp = NULL;
	jobject retval = NULL;

	if (!verify_non_null(jnienv, dbenv))
		return NULL;

	// We cannot use the default allocator (on Win* platforms anyway)
	// because it often causes problems when we free storage
	// in a DLL that was allocated in another DLL.  Using
	// our own allocator (ours just calls malloc!) ensures
	// that there is no mismatch.
	//
	err = lock_stat(dbenv, &statp, java_alloc_memory);
	if (verify_return(jnienv, err)) {
		retval = create_default_object(jnienv, name_DB_LOCK_STAT);
		jclass dbclass = get_class(jnienv, name_DB_LOCK_STAT);

		// Set the individual fields
		set_int_field(jnienv, dbclass, retval,
			      "st_maxlocks", statp->st_maxlocks);
		set_int_field(jnienv, dbclass, retval,
			      "st_nmodes", statp->st_nmodes);
		set_int_field(jnienv, dbclass, retval,
			      "st_nlockers", statp->st_nlockers);
		set_int_field(jnienv, dbclass, retval,
			      "st_nconflicts", statp->st_nconflicts);
		set_int_field(jnienv, dbclass, retval,
			      "st_nrequests", statp->st_nrequests);
		set_int_field(jnienv, dbclass, retval,
			      "st_nreleases", statp->st_nreleases);
		set_int_field(jnienv, dbclass, retval,
			      "st_ndeadlocks", statp->st_ndeadlocks);
		set_int_field(jnienv, dbclass, retval,
			      "st_region_wait", statp->st_region_wait);
		set_int_field(jnienv, dbclass, retval,
			      "st_region_nowait", statp->st_region_nowait);
		set_int_field(jnienv, dbclass, retval,
			      "st_regsize", statp->st_regsize);

		java_free_memory(statp);
	}
	return retval;
}

JNIEXPORT jint JNICALL Java_com_sleepycat_db_DbEnv_lock_1detect
  (JNIEnv *jnienv, /*DbEnv*/ jobject jthis, jint atype, jint flags)
{
	int err;
	DB_ENV *dbenv = get_DB_ENV(jnienv, jthis);
	int aborted;

	if (!verify_non_null(jnienv, dbenv))
		return false;
	err = lock_detect(dbenv, atype, flags, &aborted);
	verify_return(jnienv, err);
	return aborted;
}

JNIEXPORT /*DbLock*/ jobject JNICALL Java_com_sleepycat_db_DbEnv_lock_1get
  (JNIEnv *jnienv, /*DbEnv*/ jobject jthis, /*u_int32_t*/ jint locker,
   jint flags, /*const Dbt*/ jobject obj, /*db_lockmode_t*/ jint lock_mode)
{
	int err;
	DB_ENV *dbenv = get_DB_ENV(jnienv, jthis);
	DB_LOCK *dblock = new DB_LOCK;
	LockedDBT dbobj(jnienv, obj, inOp);
	if (dbobj.has_error())
		return 0;
	/*DbLock*/ jobject retval = NULL;

	if (!verify_non_null(jnienv, dbenv))
		return retval;
	err = lock_get(dbenv, locker, flags, dbobj.dbt,
		       (db_lockmode_t)lock_mode, dblock);
	if (verify_return(jnienv, err)) {
		retval = create_default_object(jnienv, name_DB_LOCK);
		set_private_info(jnienv, name_DB_LOCK, retval, dblock);
	}
	return retval;
}

JNIEXPORT jobjectArray JNICALL Java_com_sleepycat_db_DbEnv_log_1archive
  (JNIEnv *jnienv, /*DbEnv*/ jobject jthis, jint flags)
{
	int err;
	char** ret;
	DB_ENV *dbenv = get_DB_ENV(jnienv, jthis);

	if (!verify_non_null(jnienv, dbenv))
		return 0;
	err = log_archive(dbenv, &ret, flags, 0);
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

JNIEXPORT jint JNICALL Java_com_sleepycat_db_DbEnv_log_1compare
  (JNIEnv *jnienv, jclass /*jthis_class*/,
   /*DbLsn*/ jobject lsn0, /*DbLsn*/ jobject lsn1)
{
	DB_LSN *dblsn0 = get_DB_LSN(jnienv, lsn0);
	DB_LSN *dblsn1 = get_DB_LSN(jnienv, lsn1);

	return log_compare(dblsn0, dblsn1);
}

JNIEXPORT jstring JNICALL Java_com_sleepycat_db_DbEnv_log_1file
  (JNIEnv *jnienv, /*DbEnv*/ jobject jthis, /*DbLsn*/ jobject lsn)
{
	int err;
	DB_ENV *dbenv = get_DB_ENV(jnienv, jthis);
	DB_LSN *dblsn = get_DB_LSN(jnienv, lsn);
	char filename[FILENAME_MAX+1] = "";

	err = log_file(dbenv, dblsn, filename, FILENAME_MAX);
	verify_return(jnienv, err);
	filename[FILENAME_MAX] = '\0'; // just to be sure
	return get_java_string(jnienv, filename);
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbEnv_log_1flush
  (JNIEnv *jnienv, /*DbEnv*/ jobject jthis, /*DbLsn*/ jobject lsn)
{
	int err;
	DB_ENV *dbenv = get_DB_ENV(jnienv, jthis);
	DB_LSN *dblsn = get_DB_LSN(jnienv, lsn);

	err = log_flush(dbenv, dblsn);
	verify_return(jnienv, err);
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbEnv_log_1get
  (JNIEnv *jnienv, /*DbEnv*/ jobject jthis, /*DbLsn*/ jobject lsn,
   /*DbDbt*/ jobject data, jint flags)
{
	int err;
	DB_ENV *dbenv = get_DB_ENV(jnienv, jthis);
	DB_LSN *dblsn = get_DB_LSN(jnienv, lsn);
	LockedDBT dbdata(jnienv, data, outOp);
	if (dbdata.has_error())
		return;

	for (int retry = 0; retry < 3; retry++) {
		err = log_get(dbenv, dblsn, dbdata.dbt, flags);
		// If we failed due to lack of memory in our DBT arrays,
		// retry.
		//
		if (err != ENOMEM)
			break;
		if (!dbdata.realloc())
			break;
	}

	verify_return(jnienv, err);
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbEnv_log_1put
  (JNIEnv *jnienv, /*DbEnv*/ jobject jthis, /*DbLsn*/ jobject lsn,
   /*DbDbt*/ jobject data, jint flags)
{
	int err;
	DB_ENV *dbenv = get_DB_ENV(jnienv, jthis);
	DB_LSN *dblsn = get_DB_LSN(jnienv, lsn);
	LockedDBT dbdata(jnienv, data, inOp);
	if (dbdata.has_error())
		return;

	err = log_put(dbenv, dblsn, dbdata.dbt, flags);
	verify_return(jnienv, err);
}

JNIEXPORT jint JNICALL Java_com_sleepycat_db_DbEnv_log_1register
  (JNIEnv *jnienv, /*DbEnv*/ jobject jthis, /*Db*/ jobject dbp,
   jstring name)
{
	int err;
	DB_ENV *dbenv = get_DB_ENV(jnienv, jthis);
	DB *dbdb = get_DB(jnienv, dbp);
	LockedString dbname(jnienv, name);
	int32_t result;

	err = log_register(dbenv, dbdb, dbname.string, &result);
	verify_return(jnienv, err);
	return result;
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbEnv_log_1unregister
  (JNIEnv *jnienv, /*DbEnv*/ jobject jthis, jint fid)
{
	int err;
	DB_ENV *dbenv = get_DB_ENV(jnienv, jthis);

	err = log_unregister(dbenv, fid);
	verify_return(jnienv, err);
}

JNIEXPORT jobject JNICALL Java_com_sleepycat_db_DbEnv_log_1stat
  (JNIEnv *jnienv, /*DbEnv*/ jobject jthis)
{
	int err;
	DB_ENV *dbenv = get_DB_ENV(jnienv, jthis);
	DB_LOG_STAT *statp = NULL;
	jobject retval = NULL;

	if (!verify_non_null(jnienv, dbenv))
		return NULL;

	// We cannot use the default allocator (on Win* platforms anyway)
	// because it often causes problems when we free storage
	// in a DLL that was allocated in another DLL.  Using
	// our own allocator (ours just calls malloc!) ensures
	// that there is no mismatch.
	//
	err = log_stat(dbenv, &statp, java_alloc_memory);
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
			      "st_regsize", statp->st_regsize);

		java_free_memory(statp);
	}
	return retval;
}

JNIEXPORT jobject JNICALL Java_com_sleepycat_db_DbEnv_memp_1stat
  (JNIEnv *jnienv, /*DbEnv*/ jobject jthis)
{
	int err;
	DB_ENV *dbenv = get_DB_ENV(jnienv, jthis);
	DB_MPOOL_STAT *statp = NULL;
	jobject retval = NULL;

	if (!verify_non_null(jnienv, dbenv))
		return NULL;

	// We cannot use the default allocator (on Win* platforms anyway)
	// because it often causes problems when we free storage
	// in a DLL that was allocated in another DLL.  Using
	// our own allocator (ours just calls malloc!) ensures
	// that there is no mismatch.
	//
	err = memp_stat(dbenv, &statp, 0, java_alloc_memory);
	if (verify_return(jnienv, err)) {
		retval = create_default_object(jnienv, name_DB_MPOOL_STAT);
		jclass dbclass = get_class(jnienv, name_DB_MPOOL_STAT);

		set_int_field(jnienv, dbclass, retval, "st_cachesize", 0);
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
			      "st_regsize", statp->st_regsize);

		java_free_memory(statp);
	}
	return retval;
}

JNIEXPORT jobjectArray JNICALL Java_com_sleepycat_db_DbEnv_memp_1fstat
  (JNIEnv *jnienv, /*DbEnv*/ jobject jthis)
{
	int err;
	DB_ENV *dbenv = get_DB_ENV(jnienv, jthis);
	DB_MPOOL_FSTAT **fstatp = NULL;
	jobjectArray retval = NULL;

	if (!verify_non_null(jnienv, dbenv))
		return NULL;

	// We cannot use the default allocator (on Win* platforms anyway)
	// because it often causes problems when we free storage
	// in a DLL that was allocated in another DLL.  Using
	// our own allocator (ours just calls malloc!) ensures
	// that there is no mismatch.
	//
	err = memp_stat(dbenv, 0, &fstatp, java_alloc_memory);
	if (verify_return(jnienv, err)) {
		int len = 0;
		while (fstatp[len])
			len++;
		jclass fstat_class = get_class(jnienv, name_DB_MPOOL_FSTAT);
		retval = jnienv->NewObjectArray(len, fstat_class, 0);
		for (int i=0; i<len; i++) {
			jobject obj = create_default_object(jnienv,
							    name_DB_MPOOL_FSTAT);
			jnienv->SetObjectArrayElement(retval, i, obj);

			// Set the string field.
			jfieldID filename_id =
				jnienv->GetFieldID(fstat_class,
						   "file_name",
						   string_signature);
			jstring jfilename =
				get_java_string(jnienv, fstatp[i]->file_name);
			jnienv->SetObjectField(obj, filename_id, jfilename);

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
			java_free_memory(fstatp[i]);
		}
		java_free_memory(fstatp);
	}
	return retval;
}

JNIEXPORT jint JNICALL Java_com_sleepycat_db_DbEnv_memp_1trickle
  (JNIEnv *jnienv, /*DbEnv*/ jobject jthis, jint pct)
{
	int err;
	DB_ENV *dbenv = get_DB_ENV(jnienv, jthis);
	int result = 0;

	if (verify_non_null(jnienv, dbenv)) {
		err = memp_trickle(dbenv, pct, &result);
		verify_return(jnienv, err);
	}
	return result;
}

JNIEXPORT jobject JNICALL Java_com_sleepycat_db_DbEnv_txn_1begin
  (JNIEnv *jnienv, /*DbEnv*/ jobject jthis, /*DbTxn*/ jobject pid, jint flags)
{
	int err;
	DB_ENV *dbenv = get_DB_ENV(jnienv, jthis);
	if (!verify_non_null(jnienv, dbenv))
		return 0;
	DB_TXN *dbpid = get_DB_TXN(jnienv, pid);
	DB_TXN *result = 0;

	err = txn_begin(dbenv, dbpid, &result, flags);
	if (!verify_return(jnienv, err))
		return 0;
	return get_DbTxn(jnienv, result);
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbEnv_txn_1checkpoint
  (JNIEnv *jnienv, /*DbEnv*/ jobject jthis, jint kbyte, jint min)
{
	int err;
	DB_ENV *dbenv = get_DB_ENV(jnienv, jthis);

	if (!verify_non_null(jnienv, dbenv))
		return;
	err = txn_checkpoint(dbenv, kbyte, min);
	verify_return(jnienv, err);
}

JNIEXPORT jobject JNICALL Java_com_sleepycat_db_DbEnv_txn_1stat
  (JNIEnv *jnienv, /*DbEnv*/ jobject jthis)
{
	int err;
	DB_ENV *dbenv = get_DB_ENV(jnienv, jthis);
	DB_TXN_STAT *statp = NULL;
	jobject retval = NULL;

	if (!verify_non_null(jnienv, dbenv))
		return NULL;

	// We cannot use the default allocator (on Win* platforms anyway)
	// because it often causes problems when we free storage
	// in a DLL that was allocated in another DLL.  Using
	// our own allocator (ours just calls malloc!) ensures
	// that there is no mismatch.
	//
	err = txn_stat(dbenv, &statp, java_alloc_memory);
	if (verify_return(jnienv, err)) {
		retval = create_default_object(jnienv, name_DB_TXN_STAT);
		jclass dbclass = get_class(jnienv, name_DB_TXN_STAT);

		// Set the individual fields

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
		set_int_field(jnienv, dbclass, retval,
			      "st_maxnactive", statp->st_maxnactive);

		jclass active_class = get_class(jnienv, name_DB_TXN_STAT_ACTIVE);
		jobjectArray actives = jnienv->NewObjectArray(statp->st_nactive,
							      active_class, 0);

		// Set the st_txnarray field.  This is a little more involved
		// than other fields, since the type is an array, so none
		// of our utility functions help.
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
		for (unsigned int i=0; i<statp->st_nactive; i++) {
			jobject obj = create_default_object(jnienv, name_DB_TXN_STAT_ACTIVE);
			jnienv->SetObjectArrayElement(actives, i, obj);

			set_int_field(jnienv, active_class, obj,
				      "txnid", statp->st_txnarray[i].txnid);
			set_int_field(jnienv, active_class, obj,
				      "parentid", statp->st_txnarray[i].parentid);
			set_lsn_field(jnienv, active_class, obj,
				      "lsn", statp->st_txnarray[i].lsn);
		}
		set_int_field(jnienv, dbclass, retval,
			      "st_region_wait", statp->st_region_wait);
		set_int_field(jnienv, dbclass, retval,
			      "st_region_nowait", statp->st_region_nowait);
		set_int_field(jnienv, dbclass, retval,
			      "st_regsize", statp->st_regsize);

		java_free_memory(statp);
	}
	return retval;
}



// See discussion on errpfx, errcall in DB_ENV_javainfo
JNIEXPORT void JNICALL Java_com_sleepycat_db_DbEnv_set_1errcall
  (JNIEnv *jnienv, /*DbEnv*/ jobject jthis, jobject errcall)
{
	DB_ENV *dbenv = get_DB_ENV(jnienv, jthis);

	if (verify_non_null(jnienv, dbenv)) {
		DB_ENV_javainfo *envinfo =
			(DB_ENV_javainfo *)dbenv->cj_internal;
		envinfo->set_errcall(jnienv, errcall);
	}
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbEnv_set_1errpfx
  (JNIEnv *jnienv, /*DbEnv*/ jobject jthis, jstring str)
{
	DB_ENV *dbenv = get_DB_ENV(jnienv, jthis);

	if (verify_non_null(jnienv, dbenv)) {
		DB_ENV_javainfo *envinfo =
			(DB_ENV_javainfo *)dbenv->cj_internal;
		envinfo->set_errpfx(jnienv, str);
	}
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbEnv_finalize
  (JNIEnv *jnienv, /*DbEnv*/ jobject jthis)
{
	// Note: if there's still a DB_ENV in the private data
	// of this object, it means the user never called close.
	// We'll do it for them, and ignore any error.
	// It's really the only way to release the data
	// and avoid a memory leak.
	//
	DB_ENV *dbenv = get_DB_ENV(jnienv, jthis);

	if (dbenv != NULL)
		(void)DbEnv_internal_close(jnienv, dbenv, 0);

	// Shouldn't see this object again, but just in case
	set_private_info(jnienv, name_DB_ENV, jthis, 0);
}
