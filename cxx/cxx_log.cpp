/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)cxx_log.cpp	11.2 (Sleepycat) 9/10/99";
#endif /* not lint */

#include "db_cxx.h"
#include "cxx_int.h"
#include <errno.h>

////////////////////////////////////////////////////////////////////////
//                                                                    //
//                            DbLog                                   //
//                                                                    //
////////////////////////////////////////////////////////////////////////

int DbEnv::log_archive(char **list[], u_int32_t flags, void *(*db_malloc)(size_t))
{
	int err;
	DB_ENV *env = unwrap(this);

	if ((err = ::log_archive(env, list, flags, db_malloc)) != 0) {
		DB_ERROR("DbEnv::log_archive", err, this);
		return err;
	}
	return 0;
}

int DbEnv::log_compare(const DbLsn *lsn0, const DbLsn *lsn1)
{
	return ::log_compare(lsn0, lsn1);
}

int DbEnv::log_file(DbLsn *lsn, char *namep, size_t len)
{
	int err;
	DB_ENV *env = unwrap(this);

	if ((err = ::log_file(env, lsn, namep, len)) != 0) {
		DB_ERROR("DbEnv::log_file", err, this);
		return err;
	}
	return 0;
}

int DbEnv::log_flush(const DbLsn *lsn)
{
	int err;
	DB_ENV *env = unwrap(this);

	if ((err = ::log_flush(env, lsn)) != 0) {
		DB_ERROR("DbEnv::log_flush", err, this);
		return err;
	}
	return 0;
}

int DbEnv::log_get(DbLsn *lsn, Dbt *data, u_int32_t flags)
{
	int err;
	DB_ENV *env = unwrap(this);

	if ((err = ::log_get(env, lsn, data, flags)) != 0) {
		DB_ERROR("DbEnv::log_get", err, this);
		return err;
	}
	return 0;
}

int DbEnv::log_put(DbLsn *lsn, const Dbt *data, u_int32_t flags)
{
	int err = 0;
	DB_ENV *env = unwrap(this);

	if ((err = ::log_put(env, lsn, data, flags)) != 0) {
		DB_ERROR("DbEnv::log_put", err, this);
		return err;
	}
	return 0;
}

int DbEnv::log_register(Db *dbp, const char *name, int32_t *fidp)
{
	int err = 0;
	DB_ENV *env = unwrap(this);

	if ((err = ::log_register(env, unwrap(dbp), name, fidp)) != 0) {
		DB_ERROR("DbEnv::log_register", err, this);
		return err;
	}
	return 0;
}

int DbEnv::log_stat(DB_LOG_STAT **spp, void *(*db_malloc)(size_t))
{
	int err = 0;
	DB_ENV *env = unwrap(this);

	if ((err = ::log_stat(env, spp, db_malloc)) != 0) {
		DB_ERROR("DbEnv::log_stat", err, this);
		return err;
	}
	return 0;
}

int DbEnv::log_unregister(int32_t fid)
{
	int err;
	DB_ENV *env = unwrap(this);

	if ((err = ::log_unregister(env, fid)) != 0) {
		DB_ERROR("DbEnv::log_unregister", err, this);
		return err;
	}
	return 0;
}
