/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997-2001
 *	Sleepycat Software.  All rights reserved.
 */
#include "db_config.h"

#ifndef lint
static const char revid[] = "$Id: java_DbTxn.c,v 11.11 2001/09/13 16:11:08 dda Exp $";
#endif /* not lint */

#include <jni.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "db_int.h"
#include "java_util.h"
#include "com_sleepycat_db_DbTxn.h"

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbTxn_abort
  (JNIEnv *jnienv, jobject jthis)
{
	int err;
	DB_TXN *dbtxn = get_DB_TXN(jnienv, jthis);
	if (!verify_non_null(jnienv, dbtxn))
		return;

	err = dbtxn->abort(dbtxn);
	verify_return(jnienv, err, 0);
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbTxn_commit
  (JNIEnv *jnienv, jobject jthis, jint flags)
{
	int err;
	DB_TXN *dbtxn = get_DB_TXN(jnienv, jthis);
	if (!verify_non_null(jnienv, dbtxn))
		return;

	err = dbtxn->commit(dbtxn, flags);
	verify_return(jnienv, err, 0);
}

JNIEXPORT jint JNICALL Java_com_sleepycat_db_DbTxn_id
  (JNIEnv *jnienv, jobject jthis)
{
	int retval = 0;
	DB_TXN *dbtxn = get_DB_TXN(jnienv, jthis);
	if (!verify_non_null(jnienv, dbtxn))
		return (-1);

	/* No error to check for from DB_TXN->id */
	retval = dbtxn->id(dbtxn);
	return (retval);
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbTxn_prepare
  (JNIEnv *jnienv, jobject jthis, jbyteArray gid)
{
	int err;
	DB_TXN *dbtxn;
	jbyte *c_array;

	dbtxn = get_DB_TXN(jnienv, jthis);
	if (!verify_non_null(jnienv, dbtxn))
		return;

	if (gid == NULL ||
	    (*jnienv)->GetArrayLength(jnienv, gid) < DB_XIDDATASIZE) {
		report_exception(jnienv, "DbTxn.prepare gid array "
				 "must be >= 128 bytes", EINVAL, 0);
		return;
	}
	c_array = (*jnienv)->GetByteArrayElements(jnienv, gid, NULL);
	err = dbtxn->prepare(dbtxn, (u_int8_t *)c_array);
	(*jnienv)->ReleaseByteArrayElements(jnienv, gid, c_array, 0);
	verify_return(jnienv, err, 0);
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbTxn_set_1timeout
  (JNIEnv *jnienv, jobject jthis, jlong timeout, jint flags)
{
	int err;
	DB_TXN *dbtxn;

	dbtxn = get_DB_TXN(jnienv, jthis);
	if (!verify_non_null(jnienv, dbtxn))
		return;

	err = dbtxn->set_timeout(dbtxn, (u_int32_t)timeout, flags);
	verify_return(jnienv, err, 0);
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_DbTxn_finalize
  (JNIEnv *jnienv, jobject jthis)
{
	DB_TXN *dbtxn;

	dbtxn = get_DB_TXN(jnienv, jthis);
	if (dbtxn) {
		/* Free any data related to DB_TXN here
		 * Note: we don't make a policy of doing
		 * a commit or abort here.  The txnmgr
		 * should be closed, and DB will clean up.
		 */
	}
}
