/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 *
 *	@(#)java_locked.h	11.2 (Sleepycat) 9/10/99
 */

#ifndef _JAVA_LOCKED_H_
#define _JAVA_LOCKED_H_

/*
 * Used internally by LockedDBT constructor.
 */
enum OpKind {
		inOp,     // setting data in database (passing data in)
		outOp,    // getting data from database to user memory
		inOutOp   // both getting/setting data
};

/*
 *
 * Declaration of class LockedDBT
 *
 * A LockedDBT class is only in existence during a
 * single native call to the DB API.  Its constructor's job is
 * to temporarily convert any java array found in the DBT_javainfo
 * to actual bytes in memory that remain locked in place.  These
 * bytes are used during the call to the underlying DB C layer,
 * and are released and/or copied back by the destructor.
 * Thus, a LockedDBT must be declared as a stack object to
 * function properly.
 */
class LockedDBT
{
public:
	// After the constructor returns, if has_error() is false,
	// then dbt must be initialized.
	//
	LockedDBT(JNIEnv *jnienv, jobject obj, OpKind kind);
	~LockedDBT();
	int realloc();	    /* returns true if reallocation took place */
	int has_error()     { return has_error_; }

public:
	DBT_javainfo *dbt;
	unsigned int java_array_len_;

private:
	JNIEnv *env_;
	jobject obj_;
	jbyte *java_data_;
	jbyte *before_data_;
	int has_error_;
	int do_realloc_;
	OpKind kind_;
};

/****************************************************************
 *
 * Declaration of class LockedString
 *
 * Given a java jstring object, this gets an encapsulated
 * const char *.  When the LockedString object is destroyed, the
 * char * array is released.
 */
class LockedString
{
public:
	LockedString(JNIEnv *jnienv, jstring jstr);
	~LockedString();

public:
	const char *string;
private:
	JNIEnv *env_;
	jstring jstr_;
};

/****************************************************************
 *
 * Declaration of class LockedStringArray
 *
 * Given a java jobjectArray object (that must be a String[]),
 * we extract the individual strings and build a const char **
 * When the LockedStringArray object is destroyed, the individual
 * strings are released.
 */
class LockedStringArray
{
public:
	LockedStringArray(JNIEnv *jnienv, jobjectArray arr);
	~LockedStringArray();

public:
	const char * const *string_array;
private:
	JNIEnv *env_;
	jobjectArray arr_;
};

#endif /* !_JAVA_LOCKED_H_ */
