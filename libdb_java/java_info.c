/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998, 1999, 2000
 *	Sleepycat Software.  All rights reserved.
 */
#include "db_config.h"

#ifndef lint
static const char revid[] = "$Id: java_info.c,v 11.8 2000/06/01 14:17:58 dda Exp $";
#endif /* not lint */

#include <jni.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "db.h"
#include "db_int.h"
#include "java_util.h"

/****************************************************************
 *
 * Callback functions
 *
 */

static void Db_feedback_callback(DB *db, int opcode, int percent)
{
	DB_JAVAINFO *dbinfo;

	if (db == NULL) {
		/* Something is *really* wrong here... */
		fprintf(stderr, "feedback callback failed!\n");
		return;
	}
	dbinfo = (DB_JAVAINFO *)db->cj_internal;
	dbji_call_feedback(dbinfo, db, dbinfo->jdbref_, opcode, percent);
}

static void DbEnv_feedback_callback(DB_ENV *dbenv, int opcode, int percent)
{
	DB_ENV_JAVAINFO *dbinfo;

	if (dbenv == NULL) {
		/* Something is *really* wrong here... */
		fprintf(stderr, "feedback callback failed!\n");
		return;
	}
	dbinfo = (DB_ENV_JAVAINFO *)dbenv->cj_internal;
	dbjie_call_feedback(dbinfo, dbenv, dbinfo->jenvref_, opcode, percent);
}

static int DbEnv_recovery_init_callback(DB_ENV *dbenv)
{
	DB_ENV_JAVAINFO *dbinfo;

	if (dbenv == NULL) {
		/* Something is *really* wrong here... */
		fprintf(stderr, "recovery_init callback failed!\n");
		return EINVAL;
	}
	dbinfo = (DB_ENV_JAVAINFO *)dbenv->cj_internal;
	return dbjie_call_recovery_init(dbinfo, dbenv, dbinfo->jenvref_);
}

/****************************************************************
 *
 * context_free_callback: utility class to de/allocate a callback slot,
 * used internally.
 *
 */

static void cfc_internal_release(CONTEXT_FREE_CALLBACK *cfc, JNIEnv *jnienv)
{
	if (cfc->callback_object_ != NULL) {
		DELETE_GLOBAL_REF(jnienv, cfc->callback_object_);
	}
}

int cfc_cfcget_slot(CONTEXT_FREE_CALLBACK *cfc)
{
	return cfc->callback_slot_;
}

jobject cfc_get_callback_object(CONTEXT_FREE_CALLBACK *cfc)
{
	return cfc->callback_object_;
}

CONTEXT_FREE_CALLBACK *cfc_construct(int size)
{
	CONTEXT_FREE_CALLBACK *cfc;

	cfc = (CONTEXT_FREE_CALLBACK *)malloc(sizeof(CONTEXT_FREE_CALLBACK));
	cfc->callback_object_ = NULL;
	cfc->callback_slot_ = -1;
	cfc->size_ = size;
	return cfc;
}

void context_free_callback_destroy(CONTEXT_FREE_CALLBACK *cfc, JNIEnv *jnienv)
{
	/* Sanity check:
	 *  We cannot free the object here because we don't have a JNIEnv.
	 * The user of this class must call destroy() to dispose of it.
	 */
	if (cfc->callback_object_ != NULL) {
		fprintf(stderr, "object is not freed\n");
	}
	cfc_internal_release(cfc, jnienv);
	free(cfc);
}

int cfc_get_new_slot(CONTEXT_FREE_CALLBACK *cfc, jobject new_object,
		     void **array_of_containers,
		     void *this_container)
{
	int i;

	/* Allocate/Deallocate a slot to take care of the callback.
	 *
	 * There are four cases:
	 *  1) don't already have callback set, setting callback to null:
	 *     nothing to do.
	 *
	 *  2) have callback set, setting callback to non-null.
	 *     There is no need to change the C callback function,
	 *     so nothing to do.
	 *
	 *  3) have callback set, setting callback to null
	 *     must deallocate, code follows:
	 */
	if (cfc->callback_slot_ >= 0 && new_object == NULL) {
		array_of_containers[cfc->callback_slot_] = 0;
		cfc->callback_slot_ = -1;
	}
	/*
	 *  4) don't already have callback set, setting callback to non-null.
	 *     must allocate, code follows:
	 */
	else if (cfc->callback_slot_ < 0 && new_object != NULL) {
		for (i=0; i<cfc->size_; i++) {
			if (array_of_containers[i] == NULL) {

				/* Note: Thread locking issues
				 * are guarded against by having the
				 * *_changed() native function called within
				 * a synchronized section in Java.
				 */
				array_of_containers[i] = this_container;
				cfc->callback_slot_ = i;
				break;
			}
		}
	}
	return cfc->callback_slot_;
}

void cfc_set_callback_object(CONTEXT_FREE_CALLBACK *cfc,
			     JNIEnv *jnienv, jobject val)
{
	cfc_internal_release(cfc, jnienv);
	cfc->callback_object_ = NEW_GLOBAL_REF(jnienv, val);
}

/****************************************************************
 *
 * Implementation of class DBT_javainfo
 *
 */
DBT_JAVAINFO *
dbjit_construct()
{
	DBT_JAVAINFO *dbjit;

	dbjit = (DBT_JAVAINFO *)malloc(sizeof(DBT_JAVAINFO));
	memset(dbjit, 0, sizeof(DBT_JAVAINFO));
	return dbjit;
}

void dbjit_destroy(DBT_JAVAINFO *dbjit)
{
	/* Sanity check:
	 * We cannot delete the global ref because we don't have a JNIEnv.
	 */
	if (dbjit->array_ != NULL) {
		fprintf(stderr, "object is not freed\n");
	}
	free(dbjit);
}

void dbjit_release(DBT_JAVAINFO *dbjit, JNIEnv *jnienv)
{
	if (dbjit->array_ != NULL) {
		DELETE_GLOBAL_REF(jnienv, dbjit->array_);
		dbjit->array_ = NULL;
	}
}

/****************************************************************
 *
 * Implementation of class DB_ENV_JAVAINFO
 *
 */

/* create/initialize an object */
DB_ENV_JAVAINFO *
dbjie_construct(JNIEnv *jnienv,
		jobject default_errcall,
		int is_dbopen)
{
	DB_ENV_JAVAINFO *dbjie;

	dbjie = (DB_ENV_JAVAINFO *)malloc(sizeof(DB_ENV_JAVAINFO));
	memset(dbjie, 0, sizeof(DB_ENV_JAVAINFO));
	dbjie->is_dbopen_ = is_dbopen;

	if ((*jnienv)->GetJavaVM(jnienv, &dbjie->javavm_) != 0) {
		free(dbjie);
		report_exception(jnienv, "cannot get Java VM", 0, 0);
		return NULL;
	}

	/* The default error call just prints to the 'System.err'
	 * stream.  If the user does set_errcall to null, we'll
	 * want to have a reference to set it back to.
	 *
	 * Why do we have always set db_errcall to our own callback?
	 * Because it makes the interaction between setting the
	 * error prefix, error stream, and user's error callback
	 * that much easier.
	 */
	dbjie->default_errcall_ = NEW_GLOBAL_REF(jnienv, default_errcall);
	dbjie->errcall_ = NEW_GLOBAL_REF(jnienv, default_errcall);
	return dbjie;
}

/* release all objects held by this this one */
void dbjie_dealloc(DB_ENV_JAVAINFO *dbjie, JNIEnv *jnienv)
{
	if (dbjie->errcall_ != NULL) {
		DELETE_GLOBAL_REF(jnienv, dbjie->errcall_);
	}
	if (dbjie->default_errcall_ != NULL) {
		DELETE_GLOBAL_REF(jnienv, dbjie->default_errcall_);
	}
	if (dbjie->conflict_ != NULL) {
		free(dbjie->conflict_);
	}
}

/* free this object, releasing anything allocated on its behalf */
void dbjie_destroy(DB_ENV_JAVAINFO *dbjie, JNIEnv *jnienv)
{
	dbjie_dealloc(dbjie, jnienv);
	free(dbjie);
}

/* Attach to the current thread that is running and
 * return that.  We use the java virtual machine
 * that we saved in the constructor.
 */
JNIEnv *
dbjie_get_jnienv(DB_ENV_JAVAINFO *dbjie)
{
	/* Note:
	 * Different versions of the JNI disagree on the signature
	 * for AttachCurrentThread.  The most recent documentation
	 * seems to say that (JNIEnv **) is correct, but newer
	 * JNIs seem to use (void **), oddly enough.
	 */
#ifdef JNI_VERSION_1_2
	void *attachret = 0;
#else
	JNIEnv *attachret = 0;
#endif

	/* This should always succeed, as we are called via
	 * some Java activity.  I think therefore I am (a thread).
	 */
	if ((*dbjie->javavm_)->AttachCurrentThread(dbjie->javavm_, &attachret, 0) != 0)
		return 0;

	return (JNIEnv *)attachret;
}

jstring
dbjie_get_errpfx(DB_ENV_JAVAINFO *dbjie, JNIEnv *jnienv)
{
	return get_java_string(jnienv, dbjie->errpfx_);
}

void
dbjie_set_errcall(DB_ENV_JAVAINFO *dbjie, JNIEnv *jnienv, jobject new_errcall)
{
	/* If the new_errcall is null, we'll set the error call
	 * to the default one.
	 */
	if (new_errcall == NULL)
		new_errcall = dbjie->default_errcall_;

	DELETE_GLOBAL_REF(jnienv, dbjie->errcall_);
	dbjie->errcall_ = NEW_GLOBAL_REF(jnienv, new_errcall);
}

void
dbjie_set_errpfx(DB_ENV_JAVAINFO *dbjie, JNIEnv *jnienv, jstring errpfx)
{
	if (dbjie->errpfx_)
		free(dbjie->errpfx_);

	if (errpfx)
		dbjie->errpfx_ = dup_string((*jnienv)->GetStringUTFChars(jnienv, errpfx, NULL));
	else
		dbjie->errpfx_ = 0;
}

void
dbjie_set_conflict(DB_ENV_JAVAINFO *dbjie, unsigned char *newarr)
{
	if (dbjie->conflict_)
		free(dbjie->conflict_);
	dbjie->conflict_ = newarr;
}

void dbjie_set_feedback_object(DB_ENV_JAVAINFO *dbjie, JNIEnv *jnienv,
			       DB_ENV *dbenv, jobject jfeedback)
{
	int err;

	COMPQUIET(jnienv, NULL);

	if (jfeedback == NULL) {
		if ((err = dbenv->set_feedback(dbenv, NULL)) != 0)
			report_exception(jnienv, "set_feedback failed",
					 err, 0);
	}
	else {
		if ((err = dbenv->set_feedback(dbenv,
					       DbEnv_feedback_callback)) != 0)
			report_exception(jnienv, "set_feedback failed",
					 err, 0);
	}

	/* Note: we don't have to grab a global ref for the feedback
	 * object since it is also referenced by the DbEnv object in java.
	 */
	dbjie->feedback_ = jfeedback;
}

void dbjie_call_feedback(DB_ENV_JAVAINFO *dbjie, DB_ENV *dbenv, jobject jenv,
			 int opcode, int percent)
{
	JNIEnv *jnienv;
	jclass feedback_class;
	jmethodID id;

	COMPQUIET(dbenv, NULL);
	jnienv = dbjie_get_jnienv(dbjie);
	if (jnienv == NULL) {
		fprintf(stderr, "Cannot attach to current thread!\n");
		return;
	}

	feedback_class = get_class(jnienv, name_DbEnvFeedback);
	id = (*jnienv)->GetMethodID(jnienv, feedback_class,
				    "feedback",
				    "(Lcom/sleepycat/db/DbEnv;II)V");
	if (!id) {
		fprintf(stderr, "Cannot find callback class\n");
		return;
	}

	(*jnienv)->CallVoidMethod(jnienv, dbjie->feedback_, id,
				  jenv, (jint)opcode, (jint)percent);
}

void dbjie_set_recovery_init_object(DB_ENV_JAVAINFO *dbjie,
				    JNIEnv *jnienv, DB_ENV *dbenv,
				    jobject jrecovery_init)
{
	int err;

	COMPQUIET(jnienv, NULL);

	if (jrecovery_init == NULL) {
		if ((err = dbenv->set_recovery_init(dbenv, NULL)) != 0)
			report_exception(jnienv, "set_recover_init failed",
					 err, 0);
	}
	else {
		if ((err = dbenv->set_recovery_init(dbenv,
					DbEnv_recovery_init_callback)) != 0)
			report_exception(jnienv, "set_recover_init failed",
					 err, 0);
	}

	/* Note: we don't have to grab a global ref for the recovery_init
	 * object since it is also referenced by the DbEnv java.
	 */
	dbjie->recovery_init_ = jrecovery_init;
}

int dbjie_call_recovery_init(DB_ENV_JAVAINFO *dbjie, DB_ENV *dbenv,
			     jobject jenv)
{
	JNIEnv *jnienv;
	jclass recovery_init_class;
	jmethodID id;

	COMPQUIET(dbenv, NULL);
	jnienv = dbjie_get_jnienv(dbjie);
	if (jnienv == NULL) {
		fprintf(stderr, "Cannot attach to current thread!\n");
		return EINVAL;
	}

	recovery_init_class = get_class(jnienv, name_DbRecoveryInit);
	id = (*jnienv)->GetMethodID(jnienv, recovery_init_class,
				    "recovery_init",
				    "(Lcom/sleepycat/db/DbEnv;)V");
	if (!id) {
		fprintf(stderr, "Cannot find callback class\n");
		return EINVAL;
	}
	return (*jnienv)->CallIntMethod(jnienv, dbjie->recovery_init_,
					id, jenv);
}

jobject dbjie_get_errcall(DB_ENV_JAVAINFO *dbjie)
{
	return dbjie->errcall_;
}

int dbjie_is_dbopen(DB_ENV_JAVAINFO *dbjie)
{
	return dbjie->is_dbopen_;
}

/****************************************************************
 *
 * Implementation of class DB_JAVAINFO
 *
 */

DB_JAVAINFO *dbji_construct(JNIEnv *jnienv, jint flags)
{
	DB_JAVAINFO *dbji;

	dbji = (DB_JAVAINFO *)malloc(sizeof(DB_JAVAINFO));
	memset(dbji, 0, sizeof(DB_JAVAINFO));

	if ((*jnienv)->GetJavaVM(jnienv, &dbji->javavm_) != 0) {
		report_exception(jnienv, "cannot get Java VM", 0, 0);
		free(dbji);
		return NULL;
	}
	dbji->flags_ = flags;
	return dbji;
}

void
dbji_dealloc(DB_JAVAINFO *dbji, JNIEnv *jnienv)
{
	COMPQUIET(dbji, NULL);
	COMPQUIET(jnienv, NULL);
}

void
dbji_destroy(DB_JAVAINFO *dbji, JNIEnv *jnienv)
{
	dbji_dealloc(dbji, jnienv);
	free(dbji);
}

JNIEnv *dbji_get_jnienv(DB_JAVAINFO *dbji)
{
	/* Note:
	 * Different versions of the JNI disagree on the signature
	 * for AttachCurrentThread.  The most recent documentation
	 * seems to say that (JNIEnv **) is correct, but newer
	 * JNIs seem to use (void **), oddly enough.
	 */
#ifdef JNI_VERSION_1_2
	void *attachret = 0;
#else
	JNIEnv *attachret = 0;
#endif

	/* This should always succeed, as we are called via
	 * some Java activity.  I think therefore I am (a thread).
	 */
	if ((*dbji->javavm_)->AttachCurrentThread(dbji->javavm_, &attachret, 0) != 0)
		return 0;

	return (JNIEnv *)attachret;
}

jint dbji_get_flags(DB_JAVAINFO *dbji)
{
	return dbji->flags_;
}

void dbji_set_feedback_object(DB_JAVAINFO *dbji, JNIEnv *jnienv,
			      DB *db, jobject jfeedback)
{
	COMPQUIET(jnienv, NULL);

	if (jfeedback == NULL) {
		db->set_feedback(db, NULL);
	}
	else {
		db->set_feedback(db, Db_feedback_callback);
	}

	/* Note: we don't have to grab a global ref for the feedback
	 *  object since it is also referenced by the Db object in java.
	 */
	dbji->feedback_ = jfeedback;
}

void dbji_call_feedback(DB_JAVAINFO *dbji, DB *db, jobject jdb,
			int opcode, int percent)
{
	jclass feedback_class;
	jmethodID id;
	JNIEnv *jnienv;

	COMPQUIET(db, NULL);
	jnienv = dbji_get_jnienv(dbji);
	if (jnienv == NULL) {
		fprintf(stderr, "Cannot attach to current thread!\n");
		return;
	}

	feedback_class = get_class(jnienv, name_DbFeedback);
	id = (*jnienv)->GetMethodID(jnienv, feedback_class,
				    "feedback",
				    "(Lcom/sleepycat/db/Db;II)V");
	if (!id) {
		fprintf(stderr, "Cannot find callback class\n");
		return;
	}

	(*jnienv)->CallVoidMethod(jnienv, dbji->feedback_, id,
				  jdb, (jint)opcode, (jint)percent);
}
