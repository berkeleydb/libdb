/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */
#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)java_locked.cpp	11.1 (Sleepycat) 7/25/99";
#endif /* not lint */

#include <jni.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "db.h"
#include "java_util.h"

/****************************************************************
 *
 * Implementation of class LockedDBT
 *
 */
LockedDBT::LockedDBT(JNIEnv *jnienv, jobject obj, OpKind kind)
:	env_(jnienv)
,	obj_(obj)
,	has_error_(0)
,	do_realloc_(0)
,	kind_(kind)
,	java_array_len_(0)
,	java_data_(0)
,	before_data_(0)
	/* dbt initialized below */
{
	dbt = (DBT_javainfo *)get_private_info(jnienv, name_DBT, obj);
	if (!verify_non_null(jnienv, dbt)) {
		has_error_ = 1;
		return;
	}

	if (kind == outOp &&
	    (dbt->flags & (DB_DBT_USERMEM | DB_DBT_MALLOC | DB_DBT_REALLOC)) == 0) {
		report_exception(jnienv,
				 "Dbt.flags must set to Db.DB_DBT_USERMEM, "
				 "Db.DB_DBT_MALLOC or Db.DB_DBT_REALLOC",
				 0);
		has_error_ = 1;
		return;
	}

	// If this is requested to be realloc, we cannot use the
	// underlying realloc, because the array we will pass in
	// is not allocated by us, but the Java VM, so it cannot
	// be successfully realloced.  We simulate the reallocation,
	// by using USERMEM and reallocating the java array when a
	// ENOMEM error occurs.  We change the flags during the operation,
	// and they are reset when the operation completes (in the
	// LockedDBT destructor.
	//
	if ((dbt->flags & DB_DBT_REALLOC) != 0) {
		dbt->flags &= ~DB_DBT_REALLOC;
		dbt->flags |= DB_DBT_USERMEM;
		do_realloc_ = 1;
	}

	if ((dbt->flags & DB_DBT_USERMEM) || kind != outOp) {

		// If writing with DB_DBT_USERMEM/REALLOC
		// or it's a set (or get/set) operation,
		// then the data should point to a java array.
		// Note that outOp means data is coming out of the database
		// (it's a get).  inOp means data is going into the database
		// (either a put, or a key input).
		//
		if (!dbt->array_) {
			report_exception(jnienv, "Dbt.data is null", 0);
			has_error_ = 1;
			return;
		}

		// Verify other parameters
		//
		java_array_len_ = jnienv->GetArrayLength(dbt->array_);
		if (dbt->offset_ < 0 ) {
			report_exception(jnienv, "Dbt.offset illegal", 0);
			has_error_ = 1;
			return;
		}
		if (dbt->ulen + dbt->offset_ > java_array_len_) {
			report_exception(jnienv,
			 "Dbt.ulen + Dbt.offset greater than array length", 0);
			has_error_ = 1;
			return;
		}

		java_data_ = jnienv->GetByteArrayElements(dbt->array_,
							  (jboolean *)0);
		dbt->data = before_data_ = java_data_ + dbt->offset_;
	}
	else {

		// If writing with DB_DBT_MALLOC, then the data is
		// allocated by DB.
		//
		dbt->data = before_data_ = 0;
	}
}

// The LockedDBT destructor is called when the java handler returns
// to the user, since that's when the LockedDBT objects go out of scope.
// Since it is thus called after any call to the underlying database,
// it copies any information from temporary structures back to user
// accessible arrays, and of course must free memory and remove references.
//
LockedDBT::~LockedDBT()
{
	// Fix up the flags if we changed them.
	//
	if (do_realloc_) {
		dbt->flags &= ~DB_DBT_USERMEM;
		dbt->flags |= DB_DBT_REALLOC;
	}

	// If there was an error in the constructor,
	// everything is already cleaned up.
	//
	if (has_error_)
		return;

	if ((dbt->flags & (DB_DBT_USERMEM | DB_DBT_REALLOC)) ||
	    kind_ == inOp) {

		// If writing with DB_DBT_USERMEM/REALLOC or it's a set
		// (or get/set) operation, then the data may be already in
		// the java array, in which case, we just need to release it.
		// If DB didn't put it in the array (indicated by the
		// dbt->data changing), we need to do that
		//
		if (before_data_ != java_data_) {
			env_->SetByteArrayRegion(dbt->array_, dbt->offset_,
						 dbt->ulen, before_data_);
		}
		env_->ReleaseByteArrayElements(dbt->array_, java_data_, 0);
		dbt->data = 0;
	}
	if ((dbt->flags & DB_DBT_MALLOC) && kind_ != inOp) {

		// If writing with DB_DBT_MALLOC, then the data was allocated
		// by DB.  If dbt->data is zero, it means an error occurred
		// (and should have been already reported).
		//
		if (dbt->data) {

			// Release any old references.
			//
			dbt->release(env_);

			dbt->array_ = (jbyteArray)
				env_->NewGlobalRef(env_->NewByteArray(dbt->size));
			dbt->offset_ = 0;
			env_->SetByteArrayRegion(dbt->array_, 0, dbt->size,
						 (jbyte *)dbt->data);
			free(dbt->data);
			dbt->data = 0;
		}
	}
}

// Realloc the java array to receive data if the DBT was marked
// for realloc, and the last operation set the size field to an
// amount greater than ulen.
//
int LockedDBT::realloc()
{
	if (!do_realloc_ || has_error_ || dbt->size <= dbt->ulen)
		return 0;

	env_->ReleaseByteArrayElements(dbt->array_, java_data_, 0);
	dbt->release(env_);

	// We allocate a new array of the needed size.
	// We'll set the offset to 0, as the old offset
	// really doesn't make any sense.
	//
	java_array_len_ = dbt->ulen = dbt->size;
	dbt->offset_ = 0;
	dbt->array_ = (jbyteArray)
		env_->NewGlobalRef(env_->NewByteArray(dbt->size));

	java_data_ = env_->GetByteArrayElements(dbt->array_, (jboolean *)0);
	dbt->data = before_data_ = java_data_;
	return 1;
}


/****************************************************************
 *
 * Implementation of class LockedString
 *
 */
LockedString::LockedString(JNIEnv *jnienv, jstring jstr)
:	env_(jnienv)
,	jstr_(jstr)
{
	if (jstr == 0)
		string = 0;
	else
		string = jnienv->GetStringUTFChars(jstr, (jboolean *)0);
}

LockedString::~LockedString()
{
	if (jstr_)
		env_->ReleaseStringUTFChars(jstr_, string);
}


/****************************************************************
 *
 * Implementation of class LockedStringArray
 *
 */
LockedStringArray::LockedStringArray(JNIEnv *jnienv, jobjectArray arr)
:	env_(jnienv)
,	arr_(arr)
,	string_array(0)
{
	typedef const char *conststr;

	if (arr != 0) {
		int count = jnienv->GetArrayLength(arr);
		const char **new_string_array = NEW_ARRAY(conststr, count+1);
		for (int i=0; i<count; i++) {
			jstring jstr = (jstring)jnienv->GetObjectArrayElement(arr, i);
			if (jstr == 0) {
				//
				// An embedded null in the string array
				// is treated as an endpoint.
				//
				new_string_array[i] = 0;
				break;
			}
			else {
				new_string_array[i] =
					jnienv->GetStringUTFChars(jstr, (jboolean *)0);
			}
		}
		new_string_array[count] = 0;
		string_array = new_string_array;
	}
}

LockedStringArray::~LockedStringArray()
{
	if (arr_) {
		int count = env_->GetArrayLength(arr_);
		for (int i=0; i<count; i++) {
			if (string_array[i] == 0)
				break;
			jstring jstr = (jstring)env_->GetObjectArrayElement(arr_, i);
			env_->ReleaseStringUTFChars(jstr, string_array[i]);
		}
		DELETE((void*)string_array);
	}
}
