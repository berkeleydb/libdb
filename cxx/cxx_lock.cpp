/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997-2001
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char revid[] = "$Id: cxx_lock.cpp,v 11.14 2001/07/28 20:01:19 dda Exp $";
#endif /* not lint */

#include <errno.h>
#include <string.h>

#include "db_cxx.h"
#include "cxx_int.h"

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
	return (*this);
}

int DbLock::put(DbEnv *env)
{
	DB_ENV *envp = unwrap(env);

	if (!env) {
		return (EINVAL);        // handle never assigned
	}

	int err;
	if ((err = envp->lock_put(envp, &lock_)) != 0) {
		DB_ERROR("DbLock::put", err, env->error_policy());
	}
	return (err);
}
