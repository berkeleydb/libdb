/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */
#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)java_info.cpp	11.3 (Sleepycat) 8/26/99";
#endif /* not lint */

#include <jni.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "db.h"
#include "java_util.h"

/****************************************************************
 *
 * Callback functions
 *
 */

static void Db_feedback_callback(DB *db, int opcode, int percent)
{
	if (db == NULL) {
		// Something is *really* wrong here...
		//
		fprintf(stderr, "feedback callback failed!\n");
		return;
	}
	DB_javainfo *dbinfo = (DB_javainfo *)db->cj_internal;
	dbinfo->call_feedback(db, opcode, percent);
}

static void DbEnv_feedback_callback(DB_ENV *dbenv, int opcode, int percent)
{
	if (dbenv == NULL) {
		// Something is *really* wrong here...
		//
		fprintf(stderr, "feedback callback failed!\n");
		return;
	}
	DB_ENV_javainfo *dbinfo = (DB_ENV_javainfo *)dbenv->cj_internal;
	dbinfo->call_feedback(dbenv, opcode, percent);
}

static int DbEnv_recovery_init_callback(DB_ENV *dbenv)
{
	if (dbenv == NULL) {
		// Something is *really* wrong here...
		//
		fprintf(stderr, "recovery_init callback failed!\n");
		return EINVAL;
	}
	DB_ENV_javainfo *dbinfo = (DB_ENV_javainfo *)dbenv->cj_internal;
	return dbinfo->call_recovery_init(dbenv);
}

/****************************************************************
 *
 * context_free_callback: utility class to de/allocate a callback slot,
 * used internally.
 *
 */
context_free_callback::context_free_callback(int size)
: callback_object_(NULL)
, callback_slot_(-1)
, size_(size)
{
}

context_free_callback::~context_free_callback()
{
	// Sanity check:
	// We cannot free the object here because we don't have a JNIEnv.
	// The user of this class must call release().
	//
	if (callback_object_ != NULL) {
		fprintf(stderr, "object is not freed\n");
	}
}


int context_free_callback::get_new_slot(jobject new_object,
					void **array_of_containers,
					void *this_container)
{
	// Allocate/Deallocate a slot to take care of the callback.
	//
	// There are four cases:
	//  1) don't already have callback set, setting callback to null:
	//     nothing to do.
	//
	//  2) have callback set, setting callback to non-null.
	//     There is no need to change the C callback function,
	//     so nothing to do.
	//
	//  3) have callback set, setting callback to null
	//     must deallocate, code follows:
	//
	if (callback_slot_ >= 0 && new_object == NULL) {
		array_of_containers[callback_slot_] = 0;
		callback_slot_ = -1;
	}
	//
	//  4) don't already have callback set, setting callback to non-null.
	//     must allocate, code follows:
	//
	else if (callback_slot_ < 0 && new_object != NULL) {
		for (int i=0; i<size_; i++) {
			if (array_of_containers[i] == NULL) {

				// Note: Thread locking issues
				// are guarded against by having the
				// *_changed() native function called within
				// a synchronized section in Java.
				//
				array_of_containers[i] = this_container;
				callback_slot_ = i;
				break;
			}
		}
	}
	return callback_slot_;
}

void context_free_callback::set_callback_object(JNIEnv *jnienv, jobject val)
{
	release(jnienv);
	callback_object_ = jnienv->NewGlobalRef(val);
}

void context_free_callback::release(JNIEnv *jnienv)
{
	if (callback_object_ != NULL) {
		jnienv->DeleteGlobalRef(callback_object_);
	}
}


/****************************************************************
 *
 * Implementation of class DBT_javainfo
 *
 */
DBT_javainfo::DBT_javainfo()
:	array_(NULL)
,	offset_(0)
{
	memset((DBT*)this, 0, sizeof(DBT));
}

void DBT_javainfo::release(JNIEnv *jnienv)
{
	if (array_ != NULL) {
		jnienv->DeleteGlobalRef(array_);
	}
}

DBT_javainfo::~DBT_javainfo()
{
}

/****************************************************************
 *
 * Implementation of class DB_ENV_javainfo
 *
 */
DB_ENV_javainfo::DB_ENV_javainfo(JNIEnv *jnienv, jobject jenv,
				 jobject default_errcall,
				 int is_dbopen)
:	javavm_(0)
,	is_dbopen_(is_dbopen)
,	errpfx_(0)
,	jenv_(0)
,	default_errcall_(0)
,	errcall_(0)
,	conflict_(0)
,	feedback_(0)
,	recovery_init_(0)
{
	if (jnienv->GetJavaVM(&javavm_) != 0) {
		report_exception(jnienv, "cannot get Java VM", 0);
		return;
	}

	// The default error call just prints to the 'System.err'
	// stream.  If the user does set_errcall to null, we'll
	// want to have a reference to set it back to.
	//
	// Why do we have always set db_errcall to our own callback?
	// Because it makes the interaction between setting the
	// error prefix, error stream, and user's error callback
	// that much easier.
	//
	default_errcall_ = jnienv->NewGlobalRef(default_errcall);
	errcall_ = jnienv->NewGlobalRef(default_errcall);
	jenv_ = jnienv->NewGlobalRef(jenv);
}

DB_ENV_javainfo::~DB_ENV_javainfo()
{
	if (conflict_)
		DELETE(conflict_);
}

void
DB_ENV_javainfo::free_references(JNIEnv *jnienv)
{
	if (errcall_ != NULL) {
		jnienv->DeleteGlobalRef(errcall_);
		errcall_ = NULL;
	}
	if (default_errcall_ != NULL) {
		jnienv->DeleteGlobalRef(default_errcall_);
		default_errcall_ = NULL;
	}
	if (jenv_ != NULL) {
		jnienv->DeleteGlobalRef(jenv_);
		jenv_ = NULL;
	}
}

// Attach to the current thread that is running and
// return that.  We use the java virtual machine
// that we saved in the constructor.
//
JNIEnv *
DB_ENV_javainfo::get_jnienv()
{
	// Note:
	// Different versions of the JNI disagree on the signature
	// for AttachCurrentThread.  The most recent documentation
	// seems to say that (JNIEnv **) is correct, but newer
	// JNIs seem to use (void **), oddly enough.
	//
#ifdef JNI_VERSION_1_2
	void *attachret = 0;
#else
	JNIEnv *attachret = 0;
#endif

	// This should always succeed, as we are called via
	// some Java activity.  I think therefore I am (a thread).
	//
	if (javavm_->AttachCurrentThread(&attachret, 0) != 0)
		return 0;

	return (JNIEnv *)attachret;
}

jstring
DB_ENV_javainfo::get_errpfx(JNIEnv *jnienv)
{
	return get_java_string(jnienv, errpfx_);
}

void
DB_ENV_javainfo::set_errcall(JNIEnv *jnienv, jobject new_errcall)
{
	// If the new_errcall is null, we'll set the error call
	// to the default one.
	//
	if (new_errcall == NULL)
		new_errcall = default_errcall_;

	jnienv->DeleteGlobalRef(errcall_);
	errcall_ = jnienv->NewGlobalRef(new_errcall);
}

void
DB_ENV_javainfo::set_errpfx(JNIEnv *jnienv, jstring errpfx)
{
	if (errpfx_)
		DELETE(errpfx_);

	if (errpfx)
		errpfx_ = dup_string(jnienv->GetStringUTFChars(errpfx, NULL));
	else
		errpfx_ = 0;
}

void
DB_ENV_javainfo::set_conflict(unsigned char *newarr)
{
	if (conflict_)
		DELETE(conflict_);
	conflict_ = newarr;
}

void DB_ENV_javainfo::set_feedback_object(JNIEnv *jnienv, DB_ENV *dbenv,
					  jobject jfeedback)
{
	if (jfeedback == NULL) {
		dbenv->set_feedback(dbenv, NULL);
	}
	else {
		dbenv->set_feedback(dbenv, DbEnv_feedback_callback);
	}

	// Note: we don't have to grab a global ref for the feedback
	// object since it is also referenced by the DbEnv java.
	//
	feedback_ = jfeedback;
}

void DB_ENV_javainfo::call_feedback(DB_ENV *dbenv, int opcode, int percent)
{
	JNIEnv *jnienv = get_jnienv();

	if (jnienv == NULL) {
		fprintf(stderr, "Cannot attach to current thread!\n");
		return;
	}

	jclass feedback_class = get_class(jnienv, name_DbEnvFeedback);
	jmethodID id = jnienv->GetMethodID(feedback_class,
					   "feedback",
					   "(Lcom/sleepycat/db/DbEnv;II)V");
	if (!id) {
		fprintf(stderr, "Cannot find callback class\n");
		return;
	}

	jnienv->CallVoidMethod(feedback_, id,
			       jenv_, (jint)opcode, (jint)percent);
}

void DB_ENV_javainfo::set_recovery_init_object(JNIEnv *jnienv, DB_ENV *dbenv,
					  jobject jrecovery_init)
{
	if (jrecovery_init == NULL) {
		dbenv->set_recovery_init(dbenv, NULL);
	}
	else {
		dbenv->set_recovery_init(dbenv, DbEnv_recovery_init_callback);
	}

	// Note: we don't have to grab a global ref for the recovery_init
	// object since it is also referenced by the DbEnv java.
	//
	recovery_init_ = jrecovery_init;
}

int DB_ENV_javainfo::call_recovery_init(DB_ENV *dbenv)
{
	JNIEnv *jnienv = get_jnienv();

	if (jnienv == NULL) {
		fprintf(stderr, "Cannot attach to current thread!\n");
		return EINVAL;
	}

	jclass recovery_init_class = get_class(jnienv, name_DbRecoveryInit);
	jmethodID id = jnienv->GetMethodID(recovery_init_class,
					   "recovery_init",
					   "(Lcom/sleepycat/db/DbEnv;)V");
	if (!id) {
		fprintf(stderr, "Cannot find callback class\n");
		return EINVAL;
	}

	return jnienv->CallIntMethod(recovery_init_, id, jenv_);
}

/****************************************************************
 *
 * Implementation of class DB_javainfo
 *
 */
DB_javainfo::DB_javainfo(JNIEnv *jnienv, jobject jdb)
:	javavm_(0)
,	jdb_(0)
,	feedback_(0)
{
	if (jnienv->GetJavaVM(&javavm_) != 0) {
		report_exception(jnienv, "cannot get Java VM", 0);
		return;
	}
	jdb_ = jnienv->NewGlobalRef(jdb);
}

DB_javainfo::~DB_javainfo()
{
}

void
DB_javainfo::free_references(JNIEnv *jnienv)
{
	if (jdb_ != NULL) {
		jnienv->DeleteGlobalRef(jdb_);
		jdb_ = NULL;
	}
}

JNIEnv *DB_javainfo::get_jnienv()
{
	// Note:
	// Different versions of the JNI disagree on the signature
	// for AttachCurrentThread.  The most recent documentation
	// seems to say that (JNIEnv **) is correct, but newer
	// JNIs seem to use (void **), oddly enough.
	//
#ifdef JNI_VERSION_1_2
	void *attachret = 0;
#else
	JNIEnv *attachret = 0;
#endif

	// This should always succeed, as we are called via
	// some Java activity.  I think therefore I am (a thread).
	//
	if (javavm_->AttachCurrentThread(&attachret, 0) != 0)
		return 0;

	return (JNIEnv *)attachret;
}

void DB_javainfo::set_feedback_object(JNIEnv *jnienv, DB *db, jobject jfeedback)
{
	if (jfeedback == NULL) {
		db->set_feedback(db, NULL);
	}
	else {
		db->set_feedback(db, Db_feedback_callback);
	}

	// Note: we don't have to grab a global ref for the feedback
	// object since it is also referenced by the DbEnv java.
	//
	feedback_ = jfeedback;
}

void DB_javainfo::call_feedback(DB *db, int opcode, int percent)
{
	JNIEnv *jnienv = get_jnienv();

	if (jnienv == NULL) {
		fprintf(stderr, "Cannot attach to current thread!\n");
		return;
	}

	jclass feedback_class = get_class(jnienv, name_DbFeedback);
	jmethodID id = jnienv->GetMethodID(feedback_class,
					   "feedback",
					   "(Lcom/sleepycat/db/Db;II)V");
	if (!id) {
		fprintf(stderr, "Cannot find callback class\n");
		return;
	}

	jnienv->CallVoidMethod(feedback_, id,
			       jdb_, (jint)opcode, (jint)percent);
}

