/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)cxx_lock.cpp	11.3 (Sleepycat) 9/30/99";
#endif /* not lint */

#include "db_cxx.h"
#include "cxx_int.h"
#include <errno.h>
#include <string.h>

int DbEnv::lock_detect(u_int32_t flags, u_int32_t atype, int *aborted)
{
	DB_ENV *env = unwrap(this);
	int err;

	if ((err = ::lock_detect(env, flags, atype, aborted)) != 0) {
		DB_ERROR("DbEnv::lock_detect", err, this);
		return err;
	}
	return err;
}

int DbEnv::lock_get(u_int32_t locker, u_int32_t flags, const Dbt *obj,
		    db_lockmode_t lock_mode, DbLock *lock)
{
	DB_ENV *env = unwrap(this);
	int err;

	if ((err = ::lock_get(env, locker, flags, obj,
			      lock_mode, &lock->lock_)) != 0) {
		DB_ERROR("DbEnv::lock_get", err, this);
		return err;
	}
	return err;
}

int DbEnv::lock_id(u_int32_t *idp)
{
	DB_ENV *env = unwrap(this);
	int err;

	if ((err = ::lock_id(env, idp)) != 0) {
		DB_ERROR("DbEnv::lock_id", err, this);
	}
	return err;
}

int DbEnv::lock_stat(DB_LOCK_STAT **statp, void *(*db_malloc)(size_t))
{
	DB_ENV *env = unwrap(this);
	int err;

	if ((err = ::lock_stat(env, statp, db_malloc)) != 0) {
		DB_ERROR("DbEnv::lock_stat", err, this);
		return err;
	}
	return 0;
}

int DbEnv::lock_vec(u_int32_t locker, u_int32_t flags,
		    DB_LOCKREQ list[],
		    int nlist, DB_LOCKREQ **elist_returned)
{
	DB_ENV *env = unwrap(this);
	int err;

	if ((err = ::lock_vec(env, locker, flags, list,
			      nlist, elist_returned)) != 0) {
		DB_ERROR("DbEnv::lock_vec", err, this);
		return err;
	}
	return err;
}

////////////////////////////////////////////////////////////////////////
//                                                                    //
//                            DbLock                                  //
//                                                                    //
////////////////////////////////////////////////////////////////////////

DbLock::DbLock(DB_LOCK value)
:	lock_(value)
{
}

DbLock::DbLock()
{
	memset(&lock_, 0, sizeof(DB_LOCK));
}

DbLock::DbLock(const DbLock &that)
:	lock_(that.lock_)
{
}

DbLock &DbLock::operator = (const DbLock &that)
{
	lock_ = that.lock_;
	return *this;
}

int DbLock::put(DbEnv *env)
{
	DB_ENV *envp = unwrap(env);

	if (!env) {
		return EINVAL;        // handle never assigned
	}

	int err;
	if ((err = lock_put(envp, &lock_)) != 0) {
		DB_ERROR("DbLock::put", err, env);
	}
	return err;
}
