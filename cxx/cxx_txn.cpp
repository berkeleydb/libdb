/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)cxx_txn.cpp	11.3 (Sleepycat) 9/10/99";
#endif /* not lint */

#include "db_cxx.h"
#include "cxx_int.h"
#include <errno.h>

////////////////////////////////////////////////////////////////////////
//                                                                    //
//                            DbTxnMgr                                //
//                                                                    //
////////////////////////////////////////////////////////////////////////

int DbEnv::txn_begin(DbTxn *pid, DbTxn **tid, u_int32_t flags)
{
	int err;
	DB_ENV *env = unwrap(this);
	DB_TXN *txn;

	if ((err = ::txn_begin(env, unwrap(pid), &txn, flags)) != 0) {
		DB_ERROR("DbEnv::txn_begin", err, this);
		return err;
	}
	DbTxn *result = new DbTxn();
	result->imp_ = wrap(txn);
	*tid = result;
	return err;
}

int DbEnv::txn_checkpoint(u_int32_t kbyte, u_int32_t min)
{
	int err;
	DB_ENV *env = unwrap(this);
	if ((err = ::txn_checkpoint(env, kbyte, min)) != 0) {
		DB_ERROR("DbEnv::txn_checkpoint", err, this);
		return err;
	}
	return 0;
}

int DbEnv::txn_stat(DB_TXN_STAT **statp, void *(*db_malloc)(size_t))
{
	int err;
	DB_ENV *env = unwrap(this);
	if ((err = ::txn_stat(env, statp, db_malloc)) != 0) {
		DB_ERROR("DbEnv::txn_stat", err, this);
		return err;
	}
	return 0;
}

////////////////////////////////////////////////////////////////////////
//                                                                    //
//                            DbTxn                                   //
//                                                                    //
////////////////////////////////////////////////////////////////////////

DbTxn::DbTxn()
:	imp_(0)
{
}

DbTxn::~DbTxn()
{
}

int DbTxn::abort()
{
	int err;
	DB_TXN *txn = unwrap(this);
	if ((err = txn_abort(txn)) != 0) {
		DB_ERROR("DbTxn::abort", err, (DbEnv*)0);
		return err;
	}

	// This may seem weird, but is legal as long as we don't access
	// any data before returning.
	//
	delete this;
	return 0;
}

int DbTxn::commit(u_int32_t flags)
{
	int err;
	DB_TXN *txn = unwrap(this);
	if ((err = txn_commit(txn, flags)) != 0) {
		DB_ERROR("DbTxn::commit", err, (DbEnv*)0);
		return err;
	}

	// This may seem weird, but is legal as long as we don't access
	// any data before returning.
	//
	delete this;
	return 0;
}

u_int32_t DbTxn::id()
{
	DB_TXN *txn = unwrap(this);
	return txn_id(txn);         // no error
}

int DbTxn::prepare()
{
	int err;
	DB_TXN *txn = unwrap(this);
	if ((err = txn_prepare(txn)) != 0) {
		DB_ERROR("DbTxn::prepare", err, (DbEnv*)0);
		return err;
	}
	return 0;
}
