/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998
 *	Sleepycat Software.  All rights reserved.
 */

#include "config.h"

#ifndef lint
static const char sccsid[] = "@(#)cxx_app.cpp	10.23 (Sleepycat) 12/16/98";
#endif /* not lint */

#include "db_cxx.h"
#include "cxx_int.h"

#include <errno.h>
#include <fstream.h>
#include <iostream.h>
#include <stdio.h>              // needed for setErrorStream
#include <string.h>

////////////////////////////////////////////////////////////////////////
//                                                                    //
//                            DbEnv                                   //
//                                                                    //
////////////////////////////////////////////////////////////////////////

static DbEnv *currentApp = 0;

ostream *DbEnv::error_stream_ = 0;

DbEnv::DbEnv(const char *homeDir, char *const *db_config, u_int32_t flags_arg)
:   error_model_(Exception)
{
    DB_ENV *env = this;
    memset(env, 0, sizeof(DB_ENV));

    int err;

    if ((err = db_appinit(homeDir, db_config, env, flags_arg)) != 0) {
        DB_ERROR("DbEnv::DbEnv", err);
    }
    currentApp = this;
}

DbEnv::DbEnv()
:   error_model_(Exception)
{
    DB_ENV *env = this;
    memset(env, 0, sizeof(DB_ENV));
}

DbEnv::~DbEnv()
{
    if (currentApp == this)
        currentApp = 0;
    DB_ENV *env = this;

    // We want to call appexit() to enforce proper cleanup when
    // a DbEnv goes out of scope or is deleted.  At the same
    // time, we want to permit the user to call appexit() at
    // any time, so they can check the error return and/or
    // shut down DB at any time.  DB normally does not allow
    // multiple appexit() calls, but to satisfy the above needs,
    // we'll look at the (normally internal) flag DB_ENV_APPINIT,
    // that is set on appinit(), and cleared on appexit().
    //
    if ((env->flags & DB_ENV_APPINIT) != 0) {
        (void)appexit();        // ignore error return
    }
}

int DbEnv::appinit(const char *homeDir, char *const *db_config, u_int32_t flags_arg)
{
    DB_ENV *env = this;

    int err;

    if ((err = db_appinit(homeDir, db_config, env, flags_arg)) != 0) {
        DB_ERROR("DbEnv::appinit", err);
    }
    currentApp = this;
    return err;
}

int DbEnv::appexit()
{
    DB_ENV *env = this;

    int err;

    if ((err = db_appexit(env)) != 0) {
        DB_ERROR("DbEnv::appexit", err);
    }
    currentApp = 0;
    return err;
}

// static method
char *DbEnv::version(int *major, int *minor, int *patch)
{
    return db_version(major, minor, patch);
}

void DbEnv::set_error_model(ErrorModel model)
{
    error_model_ = model;
}

int DbEnv::runtime_error(const char *caller, int err,
                         int in_destructor, int force_throw)
{
    int throwit = (!currentApp ||
                   (currentApp && currentApp->error_model_ == Exception));

    if ((throwit && !in_destructor) || force_throw) {
        throw DbException(caller, err);
    }
    return err;
}

// Note: This actually behaves a bit like a static function,
// since DB_ENV.db_errcall has no information about which
// db_env triggered the call.  A user that has multiple DB_ENVs
// will simply not be able to have different streams for each one.
//
void DbEnv::set_error_stream(class ostream *stream)
{
    error_stream_ = stream;

    db_errcall = stream_error_function;
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

void DbEnv::set_paniccall(DbEnv::db_paniccall_fcn fcn)
{
    typedef void (*c_db_paniccall_fcn)(DB_ENV *, int);

    DB_ENV *env = this;
    env->db_paniccall = (c_db_paniccall_fcn)fcn;
}

// This is a variant of the DB_WO_ACCESS macro to define a simple set_
// method, but it raises an exception if the environment has already been
// initialized.  This is considered a configuration error (and thus
// serious enough for an unconditional exception) because user changes
// to the environment structure after appinit will have no effect.
//
#define DB_WO_ACCESS_BEFORE_APPINIT(_class, _type, _cxx_name, _field) \
                                                               \
void _class::set_##_cxx_name(_type value)                      \
{                                                              \
    if ((flags & DB_ENV_APPINIT) != 0) {                       \
        runtime_error("DbEnv::set_" #_cxx_name, EINVAL, 0, 1); \
    }                                                          \
    _field = value;                                            \
}                                                              \


DB_WO_ACCESS_BEFORE_APPINIT(DbEnv, int, lorder, db_lorder)
DB_WO_ACCESS_BEFORE_APPINIT(DbEnv, DbEnv::db_errcall_fcn, errcall, db_errcall)
DB_WO_ACCESS_BEFORE_APPINIT(DbEnv, FILE *, errfile, db_errfile)
DB_WO_ACCESS_BEFORE_APPINIT(DbEnv, int, verbose, db_verbose)
DB_WO_ACCESS_BEFORE_APPINIT(DbEnv, u_int8_t *, lk_conflicts, lk_conflicts)
DB_WO_ACCESS_BEFORE_APPINIT(DbEnv, int, lk_modes, lk_modes)
DB_WO_ACCESS_BEFORE_APPINIT(DbEnv, unsigned int, lk_max, lk_max)
DB_WO_ACCESS_BEFORE_APPINIT(DbEnv, u_int32_t, lk_detect, lk_detect)
DB_WO_ACCESS_BEFORE_APPINIT(DbEnv, u_int32_t, lg_max, lg_max)
DB_WO_ACCESS_BEFORE_APPINIT(DbEnv, size_t, mp_mmapsize, mp_mmapsize)
DB_WO_ACCESS_BEFORE_APPINIT(DbEnv, size_t, mp_size, mp_size)
DB_WO_ACCESS_BEFORE_APPINIT(DbEnv, unsigned int, tx_max, tx_max)
DB_WO_ACCESS_BEFORE_APPINIT(DbEnv, DbEnv::tx_recover_fcn, tx_recover, tx_recover)
DB_WO_ACCESS_BEFORE_APPINIT(DbEnv, u_int32_t, flags, flags)

// These fields can be changed after appinit().
//
DB_WO_ACCESS(DbEnv, const char *, errpfx, db_errpfx)


// These access methods require construction of
// wrapper options DB_FOO* to DbFoo* .
//

DbLockTab *DbEnv::get_lk_info() const
{
    if (!lk_info)
        return 0;
    DbLockTab *result = new DbLockTab();
    result->imp_ = wrap(lk_info);
    return result;
}

DbLog *DbEnv::get_lg_info() const
{
    if (!lg_info)
        return 0;
    DbLog *result = new DbLog();
    result->imp_ = wrap(lg_info);
    return result;
}

DbMpool *DbEnv::get_mp_info() const
{
    if (!mp_info)
        return 0;
    DbMpool *result = new DbMpool();
    result->imp_ = wrap(mp_info);
    return result;
}

DbTxnMgr *DbEnv::get_tx_info() const
{
    if (!tx_info)
        return 0;
    DbTxnMgr *result = new DbTxnMgr();
    result->imp_ = wrap(tx_info);
    return result;
}

////////////////////////////////////////////////////////////////////////
//                                                                    //
//                            DbInfo                                  //
//                                                                    //
////////////////////////////////////////////////////////////////////////

// Note: in theory, the db_home and db_*_dir fields will always be zero
// when managed by DbInfo.  That's because they are set by
// db_appinit, not by the user, and we make a copy of the db_env used by
// the application.
//

DbInfo::DbInfo()
{
    DB_INFO *info = this;
    memset(info, 0, sizeof(DB_INFO));
}

DbInfo::~DbInfo()
{
}

DbInfo::DbInfo(const DbInfo &that)
{
    DB_INFO *to = this;
    const DB_INFO *from = &that;
    memcpy(to, from, sizeof(DB_INFO));
}

DbInfo &DbInfo::operator = (const DbInfo &that)
{
    if (this != &that) {
        DB_INFO *to = this;
        const DB_INFO *from = &that;
        memcpy(to, from, sizeof(DB_INFO));
    }
    return *this;
}

DB_WO_ACCESS(DbInfo, int, lorder, db_lorder)
DB_WO_ACCESS(DbInfo, size_t, cachesize, db_cachesize)
DB_WO_ACCESS(DbInfo, size_t, pagesize, db_pagesize)
DB_WO_ACCESS(DbInfo, DbInfo::db_malloc_fcn, malloc, db_malloc)
DB_WO_ACCESS(DbInfo, DbInfo::dup_compare_fcn, dup_compare, dup_compare)
DB_WO_ACCESS(DbInfo, int, bt_maxkey, bt_maxkey)
DB_WO_ACCESS(DbInfo, int, bt_minkey, bt_minkey)
DB_WO_ACCESS(DbInfo, DbInfo::bt_compare_fcn, bt_compare, bt_compare)
DB_WO_ACCESS(DbInfo, DbInfo::bt_prefix_fcn, bt_prefix, bt_prefix)
DB_WO_ACCESS(DbInfo, unsigned int, h_ffactor, h_ffactor)
DB_WO_ACCESS(DbInfo, unsigned int, h_nelem, h_nelem)
DB_WO_ACCESS(DbInfo, DbInfo::h_hash_fcn, h_hash, h_hash)
DB_WO_ACCESS(DbInfo, int, re_pad, re_pad)
DB_WO_ACCESS(DbInfo, int, re_delim, re_delim)
DB_WO_ACCESS(DbInfo, u_int32_t, re_len, re_len)
DB_WO_ACCESS(DbInfo, char *, re_source, re_source)
DB_WO_ACCESS(DbInfo, u_int32_t, flags, flags)
