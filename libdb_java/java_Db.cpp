/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */
#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)java_Db.cpp	11.5 (Sleepycat) 11/9/99";
#endif /* not lint */

#include <jni.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "db.h"
#include "java_util.h"
#include "com_sleepycat_db_Db.h"


JAVADB_WO_ACCESS_METHOD(Db, jint, flags, DB, flags)
JAVADB_WO_ACCESS_METHOD(Db, jint, h_1ffactor, DB, h_ffactor)
JAVADB_WO_ACCESS_METHOD(Db, jint, h_1nelem, DB, h_nelem)
JAVADB_WO_ACCESS_METHOD(Db, jint, lorder, DB, lorder)
JAVADB_WO_ACCESS_METHOD(Db, jlong, pagesize, DB, pagesize)
JAVADB_WO_ACCESS_METHOD(Db, jint, re_1delim, DB, re_delim)
JAVADB_WO_ACCESS_METHOD(Db, jint, re_1len, DB, re_len)
JAVADB_WO_ACCESS_METHOD(Db, jint, re_1pad, DB, re_pad)
JAVADB_WO_ACCESS_METHOD(Db, jint, bt_1maxkey, DB, bt_maxkey)
JAVADB_WO_ACCESS_METHOD(Db, jint, bt_1minkey, DB, bt_minkey)

static int Db_internal_close(JNIEnv *jnienv, DB *db, jint flags, int finalize)
{
	// finalize is not called in any particular order,
	// and the associated DB_ENV may already be closed
	// (via finalize), so we can't use that.

	int err = 0;

	// Free any info related to DB here.
	DB_javainfo *dbinfo = (DB_javainfo*)(db->cj_internal);

	dbinfo->set_feedback_object(jnienv, db, NULL);

	dbinfo->free_references(jnienv);
	delete dbinfo;
	db->cj_internal = NULL;

	if (!finalize) {
		DB_ENV_javainfo *envinfo =
			(DB_ENV_javainfo*)(db->dbenv->cj_internal);
		err = db->close(db, flags);

		// If the DB_ENV was created via a Db open, then
		// this close call is responsible for freeing at
		// least the references.  The memory will be
		// freed via finalize or close of the DbEnv object.
		//
		if (envinfo->is_dbopen()) {
			envinfo->free_references(jnienv);
		}
	}
	return err;
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_Db__1init
  (JNIEnv *jnienv, /*Db*/ jobject jthis, /*DbEnv*/ jobject jdbenv, jint flags)
{
	int err;
	DB *db;
	DB_ENV *dbenv = get_DB_ENV(jnienv, jdbenv);

	err = db_create(&db, dbenv, flags);
	if (verify_return(jnienv, err)) {
		set_private_info(jnienv, name_DB, jthis, db);
	}
	DB_javainfo *dbinfo = new DB_javainfo(jnienv, jthis);
	db->cj_internal = dbinfo;

	// This seems to be needed to avoid free/malloc mismatches on NT.
	// One shared library cannot free memory that another shared library
	// has alloced.  But that is exactly what we need to do when e.g. we
	// use the DB_MALLOC DBT option to implement Dbc.get().  This works
	// around it by asking DB to use the allocator in this library.
	//
	db->set_malloc(db, java_alloc_memory);
	db->set_realloc(db, java_realloc_memory);
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_Db_close
  (JNIEnv *jnienv, /*Db*/ jobject jthis, jint flags)
{
	int err;
	DB *db = get_DB(jnienv, jthis);

	if (!verify_non_null(jnienv, db))
		return;
	err = Db_internal_close(jnienv, db, flags, 0);

	// Null out the private data to indicate the DB is invalid.
	set_private_info(jnienv, name_DB, jthis, 0);

	// Throw an exception if the close failed.
	verify_return(jnienv, err);
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

JNIEXPORT void JNICALL Java_com_sleepycat_db_Db_err
  (JNIEnv *jnienv, /*Db*/ jobject jthis, jint ecode, jstring msg)
{
	LockedString msg_string(jnienv, msg);
	DB *db = get_DB(jnienv, jthis);
	if (!verify_non_null(jnienv, db))
		return;

	db->err(db, ecode, msg_string.string);
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_Db_errx
  (JNIEnv *jnienv, /*Db*/ jobject jthis, jstring msg)
{
	LockedString msg_string(jnienv, msg);
	DB *db = get_DB(jnienv, jthis);
	if (!verify_non_null(jnienv, db))
		return;

	db->errx(db, msg_string.string);
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

JNIEXPORT void JNICALL Java_com_sleepycat_db_Db_feedback_1changed
  (JNIEnv *jnienv, /*Db*/ jobject jthis, /*DbFeedback*/ jobject jfeedback)
{
	DB *db;
	DB_javainfo *dbinfo;

	db = get_DB(jnienv, jthis);
	if (!verify_non_null(jnienv, db))
		return;

	dbinfo = (DB_javainfo *)db->cj_internal;
	dbinfo->set_feedback_object(jnienv, db, jfeedback);
}

JNIEXPORT jint JNICALL Java_com_sleepycat_db_Db_get
  (JNIEnv *jnienv, /*Db*/ jobject jthis, /*DbTxn*/ jobject txnid,
   /*Dbt*/ jobject key, /*Dbt*/ jobject data, jint flags)
{
	// Depending on flags, the key may be input/output.
	OpKind keyop = inOp;
	OpKind dataop = outOp;
	int op_flags = flags & DB_OPFLAGS_MASK;
	if (op_flags == DB_SET_RECNO) {
		keyop = inOutOp;
	}
	else if (op_flags == DB_GET_BOTH) {
		keyop = inOutOp;
		dataop = inOutOp;
	}

	int err;
	DB *db = get_DB(jnienv, jthis);
	DB_TXN *dbtxnid = get_DB_TXN(jnienv, txnid);
	LockedDBT dbkey(jnienv, key, keyop);
	if (dbkey.has_error())
		return 0;
	LockedDBT dbdata(jnienv, data, dataop);
	if (dbdata.has_error())
		return 0;

	if (!verify_non_null(jnienv, db))
		return 0;

	for (int retry = 0; retry < 3; retry++) {
		err = db->get(db, dbtxnid, dbkey.dbt, dbdata.dbt, flags);

		// If we failed due to lack of memory in our DBT arrays,
		// retry.
		//
		if (err != ENOMEM)
			break;
		if (!dbdata.realloc())
			break;
	}
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
			// An embedded null in the array is treated
			// as an endpoint.
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
	err = db->join(db, newlist, &dbc, flags);
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

JNIEXPORT void JNICALL Java_com_sleepycat_db_Db_remove
  (JNIEnv *jnienv, /*Db*/ jobject jthis, jstring name,
   jstring subname, jint flags)
{
	int err;
	DB *db = get_DB(jnienv, jthis);
	LockedString j_name(jnienv, name);
	LockedString j_subname(jnienv, subname);

	err = db->remove(db, j_name.string, j_subname.string, flags);
	verify_return(jnienv, err);
}

// See discussion on errpfx, errcall in DB_ENV_javainfo
JNIEXPORT void JNICALL Java_com_sleepycat_db_Db_set_1errcall
  (JNIEnv *jnienv, /*Db*/ jobject jthis, jobject errcall)
{
	DB *db = get_DB(jnienv, jthis);
	if (verify_non_null(jnienv, db)) {
		DB_ENV_javainfo *envinfo =
			(DB_ENV_javainfo *)db->dbenv->cj_internal;
		envinfo->set_errcall(jnienv, errcall);
	}
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_Db_set_1errpfx
  (JNIEnv *jnienv, /*DbEnv*/ jobject jthis, jstring str)
{
	DB *db = get_DB(jnienv, jthis);
	if (verify_non_null(jnienv, db)) {
		DB_ENV_javainfo *envinfo =
			(DB_ENV_javainfo *)db->dbenv->cj_internal;
		envinfo->set_errpfx(jnienv, str);
	}
}

JNIEXPORT void JNICALL
  Java_com_sleepycat_db_Db_set_1cachesize
  (JNIEnv *jnienv, /*Db*/ jobject jthis, jint gbytes, jint bytes,
   jint ncaches)
{
	DB *db = get_DB(jnienv, jthis);

	if (verify_non_null(jnienv, db)) {
		db->set_cachesize(db, gbytes, bytes, ncaches);
	}
}

JNIEXPORT void JNICALL
  Java_com_sleepycat_db_Db_set_1re_1source
  (JNIEnv *jnienv, /*Db*/ jobject jthis, jstring re_source)
{
	DB *db = get_DB(jnienv, jthis);

	if (verify_non_null(jnienv, db)) {
		if (re_source) {
			char *str = dup_string(jnienv->GetStringUTFChars(re_source, NULL));
			db->set_re_source(db, str);
		}
		else {
			db->set_re_source(db, 0);
		}
	}
}

JNIEXPORT jobject JNICALL Java_com_sleepycat_db_Db_stat
  (JNIEnv *jnienv, jobject jthis, jint flags)
{
	int err;
	DB *db = get_DB(jnienv, jthis);
	jobject retval = NULL;
	jclass dbclass;
	void *statp = 0;
	DB_BTREE_STAT *bstp;
	DB_HASH_STAT *hstp;
	DB_QUEUE_STAT *qstp;

	if (!verify_non_null(jnienv, db))
		return NULL;

	// We cannot use the default allocator (on Win* platforms anyway)
	// because it often causes problems when we free storage
	// in a DLL that was allocated in another DLL.  Using
	// our own allocator (ours just calls malloc!) ensures
	// that there is no mismatch.
	//
	err = db->stat(db, &statp, java_alloc_memory, flags);
	if (verify_return(jnienv, err)) {
		DBTYPE dbtype = db->get_type(db);
		switch (dbtype) {

			// Btree and recno share the same stat structure
		case DB_BTREE:
		case DB_RECNO:
			bstp = (DB_BTREE_STAT *)statp;
			retval = create_default_object(jnienv,
						       name_DB_BTREE_STAT);
			dbclass = get_class(jnienv, name_DB_BTREE_STAT);

			// Set the individual fields
			set_int_field(jnienv, dbclass, retval,
				      "bt_metaflags", bstp->bt_metaflags);
			set_int_field(jnienv, dbclass, retval,
				      "bt_maxkey", bstp->bt_maxkey);
			set_int_field(jnienv, dbclass, retval,
				      "bt_minkey", bstp->bt_minkey);
			set_int_field(jnienv, dbclass, retval,
				      "bt_re_len", bstp->bt_re_len);
			set_int_field(jnienv, dbclass, retval,
				      "bt_re_pad", bstp->bt_re_pad);
			set_int_field(jnienv, dbclass, retval,
				      "bt_pagesize", bstp->bt_pagesize);
			set_int_field(jnienv, dbclass, retval,
				      "bt_levels", bstp->bt_levels);
			set_int_field(jnienv, dbclass, retval,
				      "bt_nrecs", bstp->bt_nrecs);
			set_int_field(jnienv, dbclass, retval,
				      "bt_int_pg", bstp->bt_int_pg);
			set_int_field(jnienv, dbclass, retval,
				      "bt_leaf_pg", bstp->bt_leaf_pg);
			set_int_field(jnienv, dbclass, retval,
				      "bt_dup_pg", bstp->bt_dup_pg);
			set_int_field(jnienv, dbclass, retval,
				      "bt_over_pg", bstp->bt_over_pg);
			set_int_field(jnienv, dbclass, retval,
				      "bt_free", bstp->bt_free);
			set_int_field(jnienv, dbclass, retval,
				      "bt_int_pgfree", bstp->bt_int_pgfree);
			set_int_field(jnienv, dbclass, retval,
				      "bt_leaf_pgfree", bstp->bt_leaf_pgfree);
			set_int_field(jnienv, dbclass, retval,
				      "bt_dup_pgfree", bstp->bt_dup_pgfree);
			set_int_field(jnienv, dbclass, retval,
				      "bt_over_pgfree", bstp->bt_over_pgfree);
			set_int_field(jnienv, dbclass, retval,
				      "bt_magic", bstp->bt_magic);
			set_int_field(jnienv, dbclass, retval,
			      "bt_version", bstp->bt_version);

			break;

			// Hash stat structure
		case DB_HASH:
			hstp = (DB_HASH_STAT *)statp;
			retval = create_default_object(jnienv,
						       name_DB_HASH_STAT);
			dbclass = get_class(jnienv, name_DB_HASH_STAT);

			// Set the individual fields
			set_int_field(jnienv, dbclass, retval,
				      "hash_magic", hstp->hash_magic);
			set_int_field(jnienv, dbclass, retval,
				      "hash_version", hstp->hash_version);
			set_int_field(jnienv, dbclass, retval,
				      "hash_metaflags", hstp->hash_metaflags);
			set_int_field(jnienv, dbclass, retval,
				      "hash_pagesize", hstp->hash_pagesize);
			set_int_field(jnienv, dbclass, retval,
				      "hash_nelem", hstp->hash_nelem);
			set_int_field(jnienv, dbclass, retval,
				      "hash_ffactor", hstp->hash_ffactor);
			set_int_field(jnienv, dbclass, retval,
				      "hash_nrecs", hstp->hash_nrecs);
			set_int_field(jnienv, dbclass, retval,
				      "hash_buckets", hstp->hash_buckets);
			set_int_field(jnienv, dbclass, retval,
				      "hash_free", hstp->hash_free);
			set_int_field(jnienv, dbclass, retval,
				      "hash_bfree", hstp->hash_bfree);
			set_int_field(jnienv, dbclass, retval,
				      "hash_bigpages", hstp->hash_bigpages);
			set_int_field(jnienv, dbclass, retval,
				      "hash_big_bfree", hstp->hash_big_bfree);
			set_int_field(jnienv, dbclass, retval,
				      "hash_overflows", hstp->hash_overflows);
			set_int_field(jnienv, dbclass, retval,
				      "hash_ovfl_free", hstp->hash_ovfl_free);
			set_int_field(jnienv, dbclass, retval,
				      "hash_dup", hstp->hash_dup);
			set_int_field(jnienv, dbclass, retval,
				      "hash_dup_free", hstp->hash_dup_free);

			break;

		case DB_QUEUE:
			qstp = (DB_QUEUE_STAT *)statp;
			retval = create_default_object(jnienv,
						       name_DB_QUEUE_STAT);
			dbclass = get_class(jnienv, name_DB_QUEUE_STAT);

			// Set the individual fields
			set_int_field(jnienv, dbclass, retval,
				      "qs_magic", qstp->qs_magic);
			set_int_field(jnienv, dbclass, retval,
				      "qs_version", qstp->qs_version);
			set_int_field(jnienv, dbclass, retval,
				      "qs_metaflags", qstp->qs_metaflags);
			set_int_field(jnienv, dbclass, retval,
				      "qs_nrecs", qstp->qs_nrecs);
			set_int_field(jnienv, dbclass, retval,
				      "qs_pages", qstp->qs_pages);
			set_int_field(jnienv, dbclass, retval,
				      "qs_pagesize", qstp->qs_pagesize);
			set_int_field(jnienv, dbclass, retval,
				      "qs_pgfree", qstp->qs_pgfree);
			set_int_field(jnienv, dbclass, retval,
				      "qs_re_len", qstp->qs_re_len);
			set_int_field(jnienv, dbclass, retval,
				      "qs_re_pad", qstp->qs_re_pad);
			set_int_field(jnienv, dbclass, retval,
				      "qs_start", qstp->qs_start);
			set_int_field(jnienv, dbclass, retval,
				      "qs_first_recno", qstp->qs_first_recno);
			set_int_field(jnienv, dbclass, retval,
				      "qs_cur_recno", qstp->qs_cur_recno);
			break;



			// That's all the database types we're aware of!
		default:
			report_exception(jnienv,
					 "Db.stat not implemented for types"
					 "other than HASH, BTREE and RECNO",
					 0);
			break;
		}
		java_free_memory(statp);
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

	return db->get_byteswapped(db) ? 1 : 0;
}

JNIEXPORT jint JNICALL Java_com_sleepycat_db_Db_get_1type
  (JNIEnv *jnienv, /*Db*/ jobject jthis)
{
	DB *db = get_DB(jnienv, jthis);
	if (!verify_non_null(jnienv, db))
		return 0;

	return (jint)db->type;
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_Db_open
  (JNIEnv *jnienv, /*Db*/ jobject jthis, jstring name, jstring subname,
   jint type, jint flags, jint mode)
{
	int err;
	jobject retval = NULL;
	DB *db = get_DB(jnienv, jthis);
	LockedString dbname(jnienv, name);
	LockedString dbsubname(jnienv, subname);

	if (verify_non_null(jnienv, db)) {
		err = db->open(db, dbname.string, dbsubname.string,
			       (DBTYPE)type, flags, mode);
		verify_return(jnienv, err, EXCEPTION_FILE_NOT_FOUND);
	}
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_Db_upgrade
  (JNIEnv *jnienv, /*Db*/ jobject jthis, jstring name,
   jint flags)
{
	int err;
	DB *db = get_DB(jnienv, jthis);
	LockedString j_name(jnienv, name);

	err = db->upgrade(db, j_name.string, flags);
	verify_return(jnienv, err);
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_Db_finalize
  (JNIEnv *jnienv, jobject jthis)
{
	DB *db = get_DB(jnienv, jthis);
	if (db != NULL)
		(void)Db_internal_close(jnienv, db, 0, 1);

	// Shouldn't see this object again, but just in case
	set_private_info(jnienv, name_DB, jthis, 0);
}
