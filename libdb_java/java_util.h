/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 *
 *	@(#)java_util.h	11.4 (Sleepycat) 8/26/99
 */

#ifndef _JAVA_UTIL_H_
#define _JAVA_UTIL_H_

#ifdef _MSC_VER

// These are level 4 warnings that are explicitly disabled.
// With Visual C++, by default you do not see above level 3 unless
// you use /W4.  But we like to compile with the highest level
// warnings to catch other errors.
//
// 4201: nameless struct/union
//       triggered by standard include file <winnt.h>
//
// 4244: '=' : convert from '__int64' to 'unsigned int', possible loss of data
//       results from making size_t data members correspond to jlongs
//
// 4514: unreferenced inline function has been removed
//       jni.h defines methods that are not called
//
// 4127: conditional expression is constant
//       occurs because of arg in JAVADB_RW_ACCESS_STRING macro
//
#pragma warning(disable: 4244 4201 4514 4127)

#endif

#include "db_config.h"
#include "db.h"
#include "java_info.h"
#include "java_locked.h"
#include <jni.h>
#include <string.h>             // needed for memset

#define DB_PACKAGE_NAME "com/sleepycat/db/"

/* Union to convert longs to pointers (see {get,set}_private_info).
 */
typedef union {
    jlong java_long;
    void *ptr;
} long_to_ptr;

/****************************************************************
 *
 * Utility functions and definitions used by "glue" functions.
 *
 */

#define NOT_IMPLEMENTED(str) \
	report_exception(jnienv, str /*concatenate*/ ": not implemented", 0)

/* Get the private data from a Db* object as a (64 bit) java long.
 */
jlong get_private_long_info(JNIEnv *jnienv, const char *classname,
			    jobject obj);

/* Get the private data from a Db* object.
 * The private data is stored in the object as a Java long (64 bits),
 * which is long enough to store a pointer on current architectures.
 */
void *get_private_info(JNIEnv *jnienv, const char *classname,
		       jobject obj);

/* Set the private data in a Db* object as a (64 bit) java long.
 */
void set_private_long_info(JNIEnv *jnienv, const char *classname,
			   jobject obj, jlong value);

/* Set the private data in a Db* object.
 * The private data is stored in the object as a Java long (64 bits),
 * which is long enough to store a pointer on current architectures.
 */
void set_private_info(JNIEnv *jnienv, const char *classname,
		      jobject obj, void *value);

/*
 * Given a non-qualified name (e.g. "foo"), get the class handl
 * for the fully qualified name (e.g. "com.sleepycat.db.foo")
 */
jclass get_class(JNIEnv *jnienv, const char *classname);

/* Set an individual field in a Db* object.
 * The field must be a DB object type.
 */
void set_object_field(JNIEnv *jnienv, jclass class_of_this,
		      jobject jthis, const char *object_classname,
		      const char *name_of_field, jobject obj);

/* Set an individual field in a Db* object.
 * The field must be an integer type.
 */
void set_int_field(JNIEnv *jnienv, jclass class_of_this,
		   jobject jthis, const char *name_of_field, jint value);

/* Set an individual field in a Db* object.
 * The field must be an integer type.
 */
void set_long_field(JNIEnv *jnienv, jclass class_of_this,
			jobject jthis, const char *name_of_field, jlong value);

/* Set an individual field in a Db* object.
 * The field must be an DbLsn type.
 */
void set_lsn_field(JNIEnv *jnienv, jclass class_of_this,
		   jobject jthis, const char *name_of_field, DB_LSN value);

/* Values of expect_mask
 */
static const int EXCEPTION_FILE_NOT_FOUND = 0x0001;

/* Report an exception back to the java side.
 */
void report_exception(JNIEnv *jnienv, const char *text, int err,
		      unsigned long expect_mask = 0);

/* If the object is null, report an exception and return false (0),
 * otherwise return true (1).
 */
int verify_non_null(JNIEnv *jnienv, void *obj);

/* If the error code is non-zero, report an exception and return false (0),
 * otherwise return true (1).
 */
int verify_return(JNIEnv *jnienv, int err, unsigned long expect_mask = 0);

/* Create an object of the given class, calling its default constructor.
 */
jobject create_default_object(JNIEnv *jnienv, const char *class_name);

/* Convert an DB object to a Java encapsulation of that object.
 * Note: This implementation creates a new Java object on each call,
 * so it is generally useful when a new DB object has just been created.
 */
jobject convert_object(JNIEnv *jnienv, const char *class_name, void *dbobj);

/* Create a copy of the string
 */
char *dup_string(const char *str);

/* Create a java string from the given string
 */
jstring get_java_string(JNIEnv *jnienv, const char* string);

/* Storage allocator
 */
void *java_alloc_memory(size_t n);
void java_free_memory(void *p);
void *java_realloc_memory(void *p, size_t n);


/* Convert a java object to the various C pointers they represent.
 */
DB             *get_DB            (JNIEnv *jnienv, jobject obj);
DB_BTREE_STAT  *get_DB_BTREE_STAT (JNIEnv *jnienv, jobject obj);
DBC            *get_DBC           (JNIEnv *jnienv, jobject obj);
DB_ENV         *get_DB_ENV        (JNIEnv *jnienv, jobject obj);
DB_HASH_STAT   *get_DB_HASH_STAT  (JNIEnv *jnienv, jobject obj);
DB_LOCK        *get_DB_LOCK       (JNIEnv *jnienv, jobject obj);
DB_LOG_STAT    *get_DB_LOG_STAT   (JNIEnv *jnienv, jobject obj);
DB_LSN         *get_DB_LSN        (JNIEnv *jnienv, jobject obj);
DB_MPOOL_FSTAT *get_DB_MPOOL_FSTAT(JNIEnv *jnienv, jobject obj);
DB_MPOOL_STAT  *get_DB_MPOOL_STAT (JNIEnv *jnienv, jobject obj);
DB_QUEUE_STAT  *get_DB_QUEUE_STAT (JNIEnv *jnienv, jobject obj);
DB_TXN         *get_DB_TXN        (JNIEnv *jnienv, jobject obj);
DB_TXN_STAT    *get_DB_TXN_STAT   (JNIEnv *jnienv, jobject obj);
DBT_javainfo   *get_DBT           (JNIEnv *jnienv, jobject obj);

/* Convert a C object to the various java pointers they represent.
 */
jobject get_DbBtreeStat  (JNIEnv *jnienv, DB_BTREE_STAT *dbobj);
jobject get_Dbc          (JNIEnv *jnienv, DBC *dbobj);
jobject get_DbHashStat   (JNIEnv *jnienv, DB_HASH_STAT *dbobj);
jobject get_DbLogStat    (JNIEnv *jnienv, DB_LOG_STAT *dbobj);
jobject get_DbLsn        (JNIEnv *jnienv, DB_LSN dbobj);
jobject get_DbMpoolStat  (JNIEnv *jnienv, DB_MPOOL_STAT *dbobj);
jobject get_DbMpoolFStat (JNIEnv *jnienv, DB_MPOOL_FSTAT *dbobj);
jobject get_DbQueueStat  (JNIEnv *jnienv, DB_QUEUE_STAT *dbobj);
jobject get_DbTxn        (JNIEnv *jnienv, DB_TXN *dbobj);
jobject get_DbTxnStat    (JNIEnv *jnienv, DB_TXN_STAT *dbobj);

// The java names of DB classes
const char * const name_DB                 = "Db";
const char * const name_DB_BTREE_STAT      = "DbBtreeStat";
const char * const name_DBC                = "Dbc";
const char * const name_DB_DEADLOCK_EX     = "DbDeadlockException";
const char * const name_DB_ENV             = "DbEnv";
const char * const name_DB_EXCEPTION       = "DbException";
const char * const name_DB_HASH_STAT       = "DbHashStat";
const char * const name_DB_LOCK            = "DbLock";
const char * const name_DB_LOCK_STAT       = "DbLockStat";
const char * const name_DB_LOG_STAT        = "DbLogStat";
const char * const name_DB_LSN             = "DbLsn";
const char * const name_DB_MEMORY_EX       = "DbMemoryException";
const char * const name_DB_MPOOL_FSTAT     = "DbMpoolFStat";
const char * const name_DB_MPOOL_STAT      = "DbMpoolStat";
const char * const name_DB_QUEUE_STAT      = "DbQueueStat";
const char * const name_DB_RUNRECOVERY_EX  = "DbRunRecoveryException";
const char * const name_DBT                = "Dbt";
const char * const name_DB_TXN             = "DbTxn";
const char * const name_DB_TXN_STAT        = "DbTxnStat";
const char * const name_DB_TXN_STAT_ACTIVE = "DbTxnStat$Active";
const char * const name_DbEnvFeedback      = "DbEnvFeedback";
const char * const name_DbErrcall          = "DbErrcall";
const char * const name_DbFeedback         = "DbFeedback";
const char * const name_DbRecoveryInit     = "DbRecoveryInit";

const char * const string_signature    = "Ljava/lang/String;";


#define JAVADB_RO_ACCESS(j_class, j_fieldtype, j_field, c_type, c_field)    \
extern "C" JNIEXPORT j_fieldtype JNICALL                                    \
  Java_com_sleepycat_db_##j_class##_get_1##j_field                          \
  (JNIEnv *jnienv, jobject jthis)                                           \
{                                                                           \
	c_type *db_this = get_##c_type(jnienv, jthis);                      \
				                                            \
	if (verify_non_null(jnienv, db_this)) {                             \
		return db_this->c_field;                                    \
	}                                                                   \
	return 0;                                                           \
}

#define JAVADB_WO_ACCESS(j_class, j_fieldtype, j_field, c_type, c_field)    \
extern "C" JNIEXPORT void JNICALL                                           \
  Java_com_sleepycat_db_##j_class##_set_1##j_field                          \
  (JNIEnv *jnienv, jobject jthis, j_fieldtype value)                        \
{                                                                           \
	c_type *db_this = get_##c_type(jnienv, jthis);                      \
								            \
	if (verify_non_null(jnienv, db_this)) {                             \
		db_this->c_field = value;                                   \
	}                                                                   \
}

// This is a variant of the JAVADB_WO_ACCESS macro to define a simple set_
// method using a C "method" call.
//
#define JAVADB_WO_ACCESS_METHOD(j_class, j_fieldtype,                       \
			        j_field, c_type, c_field)		    \
extern "C" JNIEXPORT void JNICALL                                           \
  Java_com_sleepycat_db_##j_class##_set_1##j_field                          \
  (JNIEnv *jnienv, jobject jthis, j_fieldtype value)                        \
{                                                                           \
	c_type *db_this = get_##c_type(jnienv, jthis);                      \
								            \
	if (verify_non_null(jnienv, db_this)) {                             \
		db_this->set_##c_field(db_this, value);                     \
	}                                                                   \
}

#define JAVADB_RW_ACCESS(j_class, j_fieldtype, j_field, c_type, c_field)    \
	JAVADB_RO_ACCESS(j_class, j_fieldtype, j_field, c_type, c_field)    \
	JAVADB_WO_ACCESS(j_class, j_fieldtype, j_field, c_type, c_field)

#define JAVADB_RO_ACCESS_STRING(j_class, j_field, c_type, c_field, dealloc) \
extern "C" JNIEXPORT jstring JNICALL                                        \
  Java_com_sleepycat_db_##j_class##_get_1##j_field                          \
  (JNIEnv *jnienv, jobject jthis)                                           \
{                                                                           \
	c_type *db_this = get_##c_type(jnienv, jthis);                      \
							                    \
	if (verify_non_null(jnienv, db_this)) {                             \
		return get_java_string(jnienv, db_this->c_field);           \
	}                                                                   \
	return 0;                                                           \
}

#define JAVADB_WO_ACCESS_STRING(j_class, j_field, c_type, c_field, dealloc) \
extern "C" JNIEXPORT void JNICALL                                           \
  Java_com_sleepycat_db_##j_class##_set_1##j_field                          \
  (JNIEnv *jnienv, jobject jthis, jstring value)                            \
{                                                                           \
	c_type *db_this = get_##c_type(jnienv, jthis);                      \
							                    \
	if (verify_non_null(jnienv, db_this)) {                             \
		if (dealloc && db_this->c_field)                            \
			delete (char*)db_this->c_field;                     \
		if (value)                                                  \
			db_this->c_field =                                  \
			  dup_string(jnienv->GetStringUTFChars(value, NULL)); \
		else                                                        \
			db_this->c_field = 0;                               \
	}                                                                   \
}

#define JAVADB_RW_ACCESS_STRING(j_class, j_field, c_type, c_field, dealloc) \
	JAVADB_RO_ACCESS_STRING(j_class, j_field, c_type, c_field, dealloc) \
	JAVADB_WO_ACCESS_STRING(j_class, j_field, c_type, c_field, dealloc)


// Replacements for C++ new and delete.
// Our experience with Sparc/gcc shared libraries is that new/delete
// does not work correctly, so we use malloc.
// Obviously, these macros are quite limited and do not accept
// constructor args.  But our use of new/delete is also quite
// limited in this library.
//
#undef NEW
#undef NEW_ARRAY
#undef DELETE
#undef DELETE_ARRAY

#ifdef unix
//
// currently NEW zeros (to prevent common bugs), NEW_ARRAY does not,
// since it is almost always used for string creation.
//
#define NEW(type)		((type*)memset(malloc(sizeof(type)), 0, sizeof(type)))
#define NEW_ARRAY(type,n)	((type*)malloc(sizeof(type)*n))
#define DELETE(p)		(free(p))
#define DELETE_ARRAY(p)		(free(p))
#else
#define NEW(type)		new type
#define NEW_ARRAY(type,n)	new type[n]
#define DELETE(p)		delete p
#define DELETE_ARRAY(p)		delete [] p
#endif

#endif /* !_JAVA_UTIL_H_ */
