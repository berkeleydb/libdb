/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 *
 *	@(#)java_info.h	11.3 (Sleepycat) 9/10/99
 */

#ifndef _JAVA_INFO_H_
#define _JAVA_INFO_H_

/*
 * "Info" classes for Java implementation of Berkeley DB API.
 * These classes hold extra information for which there is
 * no room or counterpart in the base classes used in the C API.
 * In the case of a DBT, the DBT_javainfo class is stored in the
 * 'private' variable of the java Dbt, and the DBT_javainfo is subclassed
 * from a DBT.  In the case of DB and DB_ENV, the appropriate
 * info objects are pointed to by the DB and DB_ENV objects.
 * This is convenient to implement callbacks.
 */


/****************************************************************
 *
 * This class is used internally to help manage callbacks that
 * must go from C back into Java that have no arguments from
 * which we can extract context.  These are handled via
 * a set of fixed C callback functions for each callback type.
 * The context_free_callback manages the allocation/deallocation
 * of the functions.
 *
 */
class context_free_callback
{
public:
	context_free_callback(int size);
	~context_free_callback();

	// Allocate a new slot, or if appropriate, just return the old slot.
	int get_new_slot(jobject new_object,
			 void **array_of_containers,
			 void *this_container);
	int get_slot()                            { return callback_slot_; }
	void set_callback_object(JNIEnv *jnienv, jobject val);
	jobject get_callback_object()             { return callback_object_; }
	void release(JNIEnv *jnienv);

private:
	jobject callback_object_;
	int callback_slot_;
	int size_;
};

/****************************************************************
 *
 * Declaration of class DBT_javainfo
 *
 * A DBT_javainfo is created whenever a Dbt (java) object is created,
 * and a pointer to it is stored in its private info storage.
 * It is subclassed from DBT, because we must retain some extra
 * information in it while it is in use.  In particular, when
 * a java array is associated with it, we need to keep a Globally
 * Locked reference to it so it is not GC'd.  This reference is
 * released when the Dbt is GC'd.
 */
class DBT_javainfo : public DBT
{
public:
	DBT_javainfo();
	~DBT_javainfo();
	void release(JNIEnv *jnienv);

	jbyteArray array_;
	int offset_;
};

/****************************************************************
 *
 * Declaration of class DB_ENV_javainfo
 *
 * A DB_ENV_javainfo is allocated and stuffed into the cj_internal
 * and the db_errpfx for every DB_ENV created.  It holds a
 * little extra info that is needed to support callbacks.
 *
 * There's a bit of trickery here, because we have built this
 * above a layer that has a C function callback that gets
 * invoked when an error occurs.  One of the C callback's arguments
 * is the prefix from the DB_ENV, but since we stuffed a pointer
 * to our own DB_ENV_javainfo into the prefix, we get that object as an
 * argument to the C callback.  Thus, the C callback can have
 * access to much more than just the prefix, and it needs that
 * to call back into the Java enviroment.
 *
 * The DB_ENV_javainfo object holds a copy of the Java Virtual Machine,
 * which is needed to attach to the current running thread
 * whenever we need to make a callback.  (This is more reliable
 * than our previous approach, which was to save the thread
 * that created the DbEnv).  It also has the Java callback object,
 * as well as a 'default' callback object that is used when the
 * caller sets the callback to null.  It also has the original
 * error prefix, since we overwrote the one in the DB_ENV.
 * There are also fields that are unrelated to the handling
 * of callbacks, but are convenient to attach to a DB_ENV.
 *
 * Note: We assume that the Java layer is the only one
 * fiddling with the contents of db_errpfx, db_errcall, cj_internal
 * for a DB_ENV that was created via Java.  Since the Java layer should
 * have the only pointer to such a DB_ENV, this should be true.
 */
class DB_ENV_javainfo
{
public:
	DB_ENV_javainfo(JNIEnv *jnienv, jobject jenv,
			jobject default_errcall,
			int is_dbopen);
	~DB_ENV_javainfo();

	// This gets the environment for the current thread
	JNIEnv *get_jnienv();

	void free_references(JNIEnv *jnienv);
	void set_errpfx(JNIEnv *jnienv, jstring errpfx);
	jstring get_errpfx(JNIEnv *jnienv);
	void set_errcall(JNIEnv *jnienv, jobject new_errcall);
	void set_conflict(unsigned char *v);
	void set_feedback_object(JNIEnv *jnienv, DB_ENV *dbenv, jobject value);
	void call_feedback(DB_ENV *dbenv, int opcode, int percent);
	void set_recovery_init_object(JNIEnv *jnienv, DB_ENV *dbenv,
				      jobject value);
	int call_recovery_init(DB_ENV *dbenv);

	jobject get_errcall()                       { return errcall_; }
	int is_dbopen()                             { return is_dbopen_; }

private:
	JavaVM *javavm_;
	int is_dbopen_;
	char *errpfx_;
	jobject jenv_;
	jobject default_errcall_;
	jobject errcall_;
	unsigned char *conflict_;
	jobject feedback_;
	jobject recovery_init_;
};

/****************************************************************
 *
 * Declaration of class DB_javainfo
 *
 * A DB_javainfo is allocated and stuffed into the cj_internal field
 * for every DB created.  It holds a little extra info that is needed
 * to support callbacks.
 *
 * Note: We assume that the Java layer is the only one
 * fiddling with the contents of cj_internal
 * for a DB that was created via Java.  Since the Java layer should
 * have the only pointer to such a DB, this should be true.
 */
class DB_javainfo
{
public:
	DB_javainfo(JNIEnv *jnienv, jobject jdb);
	~DB_javainfo();

	// This gets the environment for the current thread
	JNIEnv *get_jnienv();

	void free_references(JNIEnv *jnienv);
	void set_feedback_object(JNIEnv *jnienv, DB *db, jobject value);
	void call_feedback(DB *db, int opcode, int percent);

private:
	JavaVM *javavm_;
	jobject jdb_;
	jobject feedback_;
};

#endif /* !_JAVA_INFO_H_ */
