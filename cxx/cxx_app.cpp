/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)cxx_app.cpp	11.10 (Sleepycat) 11/12/99";
#endif /* not lint */

#include "db_cxx.h"
#include "cxx_int.h"

extern "C" {          // needed for __db_errcall
#include "db_int.h"
#include "common_ext.h"
}

#include <errno.h>
#include <stdio.h>              // needed for set_error_stream
#include <string.h>

////////////////////////////////////////////////////////////////////////
//                                                                    //
//                            DbEnv                                   //
//                                                                    //
////////////////////////////////////////////////////////////////////////

static DbEnv *currentApp = 0;

ostream *DbEnv::error_stream_ = 0;

DbEnv::DbEnv(u_int32_t flags)
:	imp_(0)
,	no_exceptions_(0)
,	tx_recover_callback_(0)
,	paniccall_callback_(0)
{
	DB_ENV *env;
	int ret;

	if ((flags & DB_CXX_NO_EXCEPTIONS) != 0) {
		no_exceptions_ = 1;
		flags &= ~DB_CXX_NO_EXCEPTIONS;
	}

	if ((ret = ::db_env_create(&env, flags)) != 0) {
		DB_ERROR("DbEnv::DbEnv", ret, this);
		return;
	}
	imp_ = wrap(env);
	env->cj_internal = this;    // for DB_ENV* to DbEnv* conversion
	currentApp = this;
}

DbEnv::DbEnv(DB_ENV *env, u_int32_t flags)
:	imp_(0)
,	no_exceptions_(0)
,	tx_recover_callback_(0)
,	paniccall_callback_(0)
{
	if ((flags & DB_CXX_NO_EXCEPTIONS) != 0) {
		no_exceptions_ = 1;
		flags &= ~DB_CXX_NO_EXCEPTIONS;
	}

	imp_ = wrap(env);
	env->cj_internal = this;    // for DB_ENV* to DbEnv* conversion
	currentApp = this;
}

DbEnv::~DbEnv()
{
	DB_ENV *env = unwrap(this);
	env->cj_internal = 0;

	if (currentApp == this)
		currentApp = 0;
	imp_ = 0;                   // extra safety
}

int DbEnv::close(u_int32_t flags)
{
	DB_ENV *env = unwrap(this);
	int ret;

	if ((ret = env->close(env, flags)) != 0) {
		DB_ERROR("DbEnv::close", ret, this);
	}
	return ret;
}

void DbEnv::err(int error, const char *format, ...)
{
	va_list args;
	DB_ENV *env = unwrap(this);

	va_start(args, format);
	__db_real_err(env, error, 1, 1, format, args);
	va_end(args);
}

void DbEnv::errx(const char *format, ...)
{
	va_list args;
	DB_ENV *env = unwrap(this);

	va_start(args, format);
	__db_real_err(env, 0, 0, 1, format, args);
	va_end(args);
}

int DbEnv::open(const char *db_home, char * const *db_config,
		u_int32_t flags, int mode)
{
	DB_ENV *env = unwrap(this);
	int ret;

	if ((ret = env->open(env, db_home, db_config, flags, mode)) != 0) {
		DB_ERROR("DbEnv::open", ret, this);
	}
	return ret;
}

int DbEnv::remove(const char *db_home, char *const *db_config,
		  u_int32_t flags)
{
	DB_ENV *env = unwrap(this);
	int ret;

	if ((ret = env->remove(env, db_home, db_config, flags)) != 0) {
		DB_ERROR("DbEnv::remove", ret, this);
	}
	return ret;
}

int DbEnv::runtime_error(const char *caller, int error, DbEnv *env,
			 int in_destructor, int force_throw)
{
	if (env == 0) {
		env = currentApp;
	}
	int throwit = (env == NULL ||
		       (env != NULL && env->no_exceptions_ == 0));

	if ((throwit && !in_destructor) || force_throw) {
		throw DbException(caller, error);
	}
	return error;
}

// This interface, reporting a runtime error via a Db,
// is currently just a convenience for calling functions.
// Someday we may maintain a back pointer to a DbEnv, but the
// pointer management issues seem too large right now.
// (Like, what happens if the associated DbEnv is closed/deleted
// before the last use of the Db?).
//
int DbEnv::runtime_error(const char *caller, int error, Db *db,
			 int in_destructor, int force_throw)
{
	DbEnv *dbenv = 0;
	if (db != 0)
		dbenv = db->get_env(0);
	return runtime_error(caller, error, dbenv, in_destructor, force_throw);
}

// static method
char *DbEnv::strerror(int error)
{
	return db_strerror(error);
}

// Note: This actually behaves a bit like a static function,
// since DB_ENV.db_errcall has no information about which
// db_env triggered the call.  A user that has multiple DB_ENVs
// will simply not be able to have different streams for each one.
//
void DbEnv::set_error_stream(ostream *stream)
{
	DB_ENV *dbenv = unwrap(this);

	error_stream_ = stream;
	dbenv->set_errcall(dbenv, (stream == 0) ? 0 : stream_error_function);
}

void DbEnv::stream_error_function(const char *prefix, char *message)
{
	if (error_stream_) {
		if (prefix) {
			(*error_stream_) << prefix << ": ";
		}
		if (message) {
			(*error_stream_) << message;
		}
		(*error_stream_) << "\n";
	}
}

// static method
char *DbEnv::version(int *major, int *minor, int *patch)
{
	return db_version(major, minor, patch);
}

// This is a variant of the DB_WO_ACCESS macro to define a simple set_
// method calling the underlying C method, but unlike a simple
// set method, it may return an error or raise an exception.
// Note this macro expects that input _argspec is an argument
// list element (e.g. "char *arg") defined in terms of "arg".
//
#define DB_DBENV_ACCESS(_name, _argspec)                       \
                                                               \
int DbEnv::set_##_name(_argspec)                               \
{                                                              \
	int ret;                                               \
	DB_ENV *dbenv = unwrap(this);                          \
                                                               \
	if ((ret = (*(dbenv->set_##_name))(dbenv, arg)) != 0) {\
		DB_ERROR("DbEnv::set_" # _name, ret, this);    \
	}                                                      \
	return ret;                                            \
}

#define DB_DBENV_ACCESS_NORET(_name, _argspec)                 \
                                                               \
void DbEnv::set_##_name(_argspec)                              \
{                                                              \
	DB_ENV *dbenv = unwrap(this);                          \
                                                               \
	(*(dbenv->set_##_name))(dbenv, arg);                   \
	return;                                                \
}

DB_DBENV_ACCESS_NORET(errcall, void (*arg)(const char *, char *))
DB_DBENV_ACCESS_NORET(errfile, FILE *arg)
DB_DBENV_ACCESS_NORET(errpfx, const char *arg)
DB_DBENV_ACCESS(func_close, int (*arg)(int))
DB_DBENV_ACCESS(func_dirfree, void (*arg)(char **, int))
DB_DBENV_ACCESS(func_dirlist, int (*arg)(const char *, char ***, int *))
DB_DBENV_ACCESS(func_exists, int (*arg)(const char *, int *))
DB_DBENV_ACCESS(func_free, void (*arg)(void *))
DB_DBENV_ACCESS(func_fsync, int (*arg)(int))
DB_DBENV_ACCESS(func_ioinfo, int (*arg)(const char *,
		int, u_int32_t *, u_int32_t *, u_int32_t *))
DB_DBENV_ACCESS(func_malloc, void *(*arg)(size_t))
DB_DBENV_ACCESS(func_map, int (*arg)(char *, size_t, int, int, void **))
DB_DBENV_ACCESS(func_open, int (*arg)(const char *, int, ...))
DB_DBENV_ACCESS(func_read, ssize_t (*arg)(int, void *, size_t))
DB_DBENV_ACCESS(func_realloc, void *(*arg)(void *, size_t))
DB_DBENV_ACCESS(func_seek,
		int (*arg)(int, size_t, db_pgno_t, u_int32_t, int, int))
DB_DBENV_ACCESS(func_sleep, int (*arg)(u_long, u_long))
DB_DBENV_ACCESS(func_unlink, int (*arg)(const char *))
DB_DBENV_ACCESS(func_unmap, int (*arg)(void *, size_t))
DB_DBENV_ACCESS(func_write, ssize_t (*arg)(int, const void *, size_t))
DB_DBENV_ACCESS(func_yield, int (*arg)(void))
DB_DBENV_ACCESS(lg_bsize, u_int32_t arg)
DB_DBENV_ACCESS(lg_max, u_int32_t arg)
DB_DBENV_ACCESS(lk_detect, u_int32_t arg)
DB_DBENV_ACCESS(lk_max, u_int32_t arg)
DB_DBENV_ACCESS(mp_mmapsize, size_t arg)
DB_DBENV_ACCESS(mutexlocks, int arg)
DB_DBENV_ACCESS(pageyield, int arg)
DB_DBENV_ACCESS(region_init, int arg)
DB_DBENV_ACCESS(tas_spins, u_int32_t arg)
DB_DBENV_ACCESS(tx_max, u_int32_t arg)

// Here are the set methods that don't fit the above mold.
//

int DbEnv::set_cachesize(u_int32_t gbytes, u_int32_t bytes, int ncache)
{
	int ret;
	DB_ENV *dbenv = unwrap(this);

	if ((ret = (*(dbenv->set_cachesize))(dbenv, gbytes, bytes, ncache)) != 0) {
		DB_ERROR("DbEnv::set_cachesize", ret, this);
	}
	return ret;
}

int DbEnv::set_lk_conflicts(u_int8_t *lk_conflicts, int lk_max)
{
	int ret;
	DB_ENV *dbenv = unwrap(this);

	if ((ret = (*(dbenv->set_lk_conflicts))
	     (dbenv, lk_conflicts, lk_max)) != 0) {
		DB_ERROR("DbEnv::set_lk_conflicts", ret, this);
	}
	return ret;
}

int DbEnv::set_verbose(u_int32_t which, int onoff)
{
	int ret;
	DB_ENV *dbenv = unwrap(this);

	if ((ret = (*(dbenv->set_verbose))(dbenv, which, onoff)) != 0) {
		DB_ERROR("DbEnv::set_verbose", ret, this);
	}
	return ret;
}

int DbEnv::tx_recover_intercept(DB_ENV *env, DBT *dbt,
				DB_LSN *lsn, int redo, void *info)
{
	if (env == 0) {
		DB_ERROR("DbEnv::tx_recover_callback", EINVAL, (DbEnv*)0);
		return EINVAL;
	}
	DbEnv *cxxenv = (DbEnv *)env->cj_internal;
	if (cxxenv == 0) {
		DB_ERROR("DbEnv::tx_recover_callback", EINVAL, cxxenv);
		return EINVAL;
	}
	if (cxxenv->tx_recover_callback_ == 0) {
		DB_ERROR("DbEnv::tx_recover_callback", EINVAL, cxxenv);
		return EINVAL;
	}
	Dbt *cxxdbt = (Dbt *)dbt;
	DbLsn *cxxlsn = (DbLsn *)lsn;
	return (*cxxenv->tx_recover_callback_)(cxxenv, cxxdbt, cxxlsn, redo, info);
}

int DbEnv::set_tx_recover(int (*arg)(DbEnv *, Dbt *, DbLsn *, int, void *))
{
	int ret;
	DB_ENV *dbenv = unwrap(this);

	tx_recover_callback_ = arg;
	if ((ret = (*(dbenv->set_tx_recover))(dbenv, tx_recover_intercept)) != 0) {
		DB_ERROR("DbEnv::set_tx_recover", ret, this);
	}
	return ret;
}

void DbEnv::paniccall_intercept(DB_ENV *env, int errval)
{
	if (env == 0) {
		DB_ERROR("DbEnv::paniccall_callback", EINVAL, (DbEnv*)0);
	}
	DbEnv *cxxenv = (DbEnv *)env->cj_internal;
	if (cxxenv == 0) {
		DB_ERROR("DbEnv::paniccall_callback", EINVAL, cxxenv);
	}
	if (cxxenv->paniccall_callback_ == 0) {
		DB_ERROR("DbEnv::paniccall_callback", EINVAL, cxxenv);
	}
	(*cxxenv->paniccall_callback_)(cxxenv, errval);
}

void DbEnv::set_paniccall(void (*arg)(DbEnv *, int))
{
	DB_ENV *dbenv = unwrap(this);

	paniccall_callback_ = arg;

	(*(dbenv->set_paniccall))(dbenv, paniccall_intercept);
}

int DbEnv::recovery_init_intercept(DB_ENV *env)
{
	if (env == 0) {
		DB_ERROR("DbEnv::recovery_init_callback", EINVAL, (DbEnv*)0);
	}
	DbEnv *cxxenv = (DbEnv *)env->cj_internal;
	if (cxxenv == 0) {
		DB_ERROR("DbEnv::recovery_init_callback", EINVAL, cxxenv);
	}
	if (cxxenv->recovery_init_callback_ == 0) {
		DB_ERROR("DbEnv::recovery_init_callback", EINVAL, cxxenv);
	}
	return (*cxxenv->recovery_init_callback_)(cxxenv);
}

void DbEnv::set_recovery_init(int (*arg)(DbEnv *))
{
	DB_ENV *dbenv = unwrap(this);

	recovery_init_callback_ = arg;

	(*(dbenv->set_recovery_init))(dbenv, recovery_init_intercept);
}

void DbEnv::feedback_intercept(DB_ENV *env, int opcode, int pct)
{
	if (env == 0) {
		DB_ERROR("DbEnv::feedback_callback", EINVAL, (DbEnv*)0);
		return;
	}
	DbEnv *cxxenv = (DbEnv *)env->cj_internal;
	if (cxxenv == 0) {
		DB_ERROR("DbEnv::feedback_callback", EINVAL, cxxenv);
		return;
	}
	if (cxxenv->feedback_callback_ == 0) {
		DB_ERROR("DbEnv::feedback_callback", EINVAL, cxxenv);
		return;
	}
	(*cxxenv->feedback_callback_)(cxxenv, opcode, pct);
}

void DbEnv::set_feedback(void (*arg)(DbEnv *, int, int))
{
	DB_ENV *dbenv = unwrap(this);

	feedback_callback_ = arg;

	(*(dbenv->set_feedback))(dbenv, feedback_intercept);
}
