/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997-2001
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char revid[] = "$Id: cxx_txn.cpp,v 11.20 2001/09/08 14:21:18 dda Exp $";
#endif /* not lint */

#include <errno.h>

#include "db_cxx.h"
#include "cxx_int.h"

/*
 * This file contains the DbEnv::txn_* methods.
 */

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
	DB_TXN *txn;

	txn = unwrap(this);
	err = txn->abort(txn);

	// It may seem weird to delete this, but is legal as long
	// as we don't access any of its data before returning.
	//
	delete this;

	if (err != 0)
		DB_ERROR("DbTxn::abort", err, ON_ERROR_UNKNOWN);

	return (err);
}

int DbTxn::commit(u_int32_t flags)
{
	int err;
	DB_TXN *txn;

	txn = unwrap(this);
	err = txn->commit(txn, flags);

	// It may seem weird to delete this, but is legal as long
	// as we don't access any of its data before returning.
	//
	delete this;

	if (err != 0)
		DB_ERROR("DbTxn::commit", err, ON_ERROR_UNKNOWN);

	return (err);
}

u_int32_t DbTxn::id()
{
	DB_TXN *txn;

	txn = unwrap(this);
	return (txn->id(txn));		// no error
}

int DbTxn::prepare(u_int8_t *gid)
{
	int err;
	DB_TXN *txn;

	txn = unwrap(this);
	if ((err = txn->prepare(txn, gid)) != 0) {
		DB_ERROR("DbTxn::prepare", err, ON_ERROR_UNKNOWN);
		return (err);
	}
	return (0);
}

int DbTxn::set_timeout(db_timeout_t timeout, u_int32_t flags)
{
	int err;
	DB_TXN *txn;

	txn = unwrap(this);
	if ((err = txn->set_timeout(txn, timeout, flags)) != 0) {
		DB_ERROR("DbTxn::set_timeout", err, ON_ERROR_UNKNOWN);
		return (err);
	}
	return (0);
}

