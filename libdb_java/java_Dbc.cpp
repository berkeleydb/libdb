/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */
#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)java_Dbc.cpp	11.3 (Sleepycat) 11/9/99";
#endif /* not lint */

#include <jni.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "db.h"
#include "java_util.h"
#include "com_sleepycat_db_Dbc.h"

JNIEXPORT void JNICALL Java_com_sleepycat_db_Dbc_close
  (JNIEnv *jnienv, jobject jthis)
{
	int err;
	DBC *dbc = get_DBC(jnienv, jthis);

	if (!verify_non_null(jnienv, dbc))
		return;
	err = dbc->c_close(dbc);
	if (verify_return(jnienv, err)) {
		set_private_info(jnienv, name_DBC, jthis, 0);
	}
}

JNIEXPORT jint JNICALL Java_com_sleepycat_db_Dbc_del
  (JNIEnv *jnienv, jobject jthis, jint flags)
{
	int err;
	DBC *dbc = get_DBC(jnienv, jthis);

	if (!verify_non_null(jnienv, dbc))
		return 0;
	err = dbc->c_del(dbc, flags);
	if (err != DB_KEYEMPTY) {
		verify_return(jnienv, err);
	}
	return err;
}

JNIEXPORT jobject JNICALL Java_com_sleepycat_db_Dbc_dup
  (JNIEnv *jnienv, jobject jthis, jint flags)
{
	int err;
	DBC *dbc = get_DBC(jnienv, jthis);
	DBC *dbc_ret = NULL;

	if (!verify_non_null(jnienv, dbc))
		return 0;
	err = dbc->c_dup(dbc, &dbc_ret, flags);
	if (!verify_return(jnienv, err))
		return 0;

	return get_Dbc(jnienv, dbc_ret);
}

JNIEXPORT jint JNICALL Java_com_sleepycat_db_Dbc_get
  (JNIEnv *jnienv, jobject jthis,
   /*Dbt*/ jobject key, /*Dbt*/ jobject data, jint flags)
{
	// Depending on flags, the user may be supplying the key,
	// or else we may have to retrieve it.
	OpKind keyop = outOp;
	OpKind dataop = outOp;

	int op_flags = flags & DB_OPFLAGS_MASK;
	if (op_flags == DB_SET) {
		keyop = inOp;
	}
	else if (op_flags == DB_SET_RANGE ||
		 op_flags == DB_SET_RECNO) {
		keyop = inOutOp;
	}
	else if (op_flags == DB_GET_BOTH) {
		keyop = inOutOp;
		dataop = inOutOp;
	}

	int err;
	DBC *dbc = get_DBC(jnienv, jthis);
	LockedDBT dbkey(jnienv, key, keyop);

	if (dbkey.has_error())
		return 0;
	LockedDBT dbdata(jnienv, data, dataop);
	if (dbdata.has_error())
		return 0;

	if (!verify_non_null(jnienv, dbc))
		return -1;
	for (int retry = 0; retry < 3; retry++) {
		err = dbc->c_get(dbc, dbkey.dbt, dbdata.dbt, flags);

		// If we failed due to lack of memory in our DBT arrays,
		// retry.
		//
		if (err != ENOMEM)
			break;
		if (!dbkey.realloc() && !dbdata.realloc())
			break;
	}
	if (err != DB_NOTFOUND) {
		verify_return(jnienv, err);
	}
	return err;
}

JNIEXPORT jint JNICALL Java_com_sleepycat_db_Dbc_put
  (JNIEnv *jnienv, jobject jthis,
   /*Dbt*/ jobject key, /*Dbt*/ jobject data, jint flags)
{
	int err;
	DBC *dbc = get_DBC(jnienv, jthis);
	LockedDBT dbkey(jnienv, key, inOp);
	if (dbkey.has_error())
		return 0;
	LockedDBT dbdata(jnienv, data, inOp);
	if (dbdata.has_error())
		return 0;

	if (!verify_non_null(jnienv, dbc))
		return 0;
	err = dbc->c_put(dbc, dbkey.dbt, dbdata.dbt, flags);
	if (err != DB_KEYEXIST) {
		verify_return(jnienv, err);
	}
	return err;
}

JNIEXPORT void JNICALL Java_com_sleepycat_db_Dbc_finalize
  (JNIEnv *jnienv, jobject jthis)
{
	DBC *dbc = get_DBC(jnienv, jthis);
	if (dbc) {
		// Free any data related to DBC here
	}
}
