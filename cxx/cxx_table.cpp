/*-
 * See the file LICENSE for redistribution information.
 *
 * Copyright (c) 1997, 1998, 1999
 *	Sleepycat Software.  All rights reserved.
 */

#include "db_config.h"

#ifndef lint
static const char sccsid[] = "@(#)cxx_table.cpp	11.6 (Sleepycat) 11/2/99";
#endif /* not lint */

#include "db_cxx.h"
#include "cxx_int.h"
#include <errno.h>
#include <string.h>

extern "C" {
#include "common_ext.h"
};

////////////////////////////////////////////////////////////////////////
//                                                                    //
//                            Db                                      //
//                                                                    //
////////////////////////////////////////////////////////////////////////

Db::Db(DbEnv *env, u_int32_t flags)
:	imp_(0)
,	env_(env)
{
	DB_ENV *c_env = unwrap(env);
	DB *c_db;
	int ret;

	if ((ret = db_create(&c_db, c_env, flags)) != 0) {
		DB_ERROR("Db::Db", ret, this);
		return;
	}
	imp_ = wrap(c_db);
	c_db->cj_internal = this;
	(void)get_env(flags & DB_CXX_NO_EXCEPTIONS);
}

Db::Db()
:	imp_(0)
,	env_(0)
{
	// private constructor only used internally
}

Db::~Db()
{
	DB *db = unwrap(this);

	db->cj_internal = 0;
	imp_ = 0;                   // extra safety
}

int Db::close(u_int32_t flags)
{
	DB *db = unwrap(this);
	int err;

	if ((err = db->close(db, flags)) != 0) {
		DB_ERROR("Db::close", err, this);
		return err;
	}
	return 0;
}

int Db::cursor(DbTxn *txnid, Dbc **cursorp, u_int32_t flags)
{
	DB *db = unwrap(this);
	DBC *dbc = 0;
	int err;

	if ((err = db->cursor(db, unwrap(txnid), &dbc, flags)) != 0) {
		DB_ERROR("Db::cursor", err, this);
		return err;
	}

	// The following cast implies that Dbc can be no larger than DBC
	*cursorp = (Dbc*)dbc;
	return 0;
}

int Db::del(DbTxn *txnid, Dbt *key, u_int32_t flags)
{
	DB *db = unwrap(this);
	int err;

	if ((err = db->del(db, unwrap(txnid), key, flags)) != 0) {
		// DB_NOTFOUND is a "normal" return, so should not be
		// thrown as an error
		//
		if (err != DB_NOTFOUND) {
			DB_ERROR("Db::del", err, this);
			return err;
		}
	}
	return err;
}

void Db::err(int error, const char *format, ...)
{
	va_list args;
	DB *db = unwrap(this);

	va_start(args, format);
	__db_real_err(db->dbenv, error, 1, 1, format, args);
	va_end(args);
}

void Db::errx(const char *format, ...)
{
	va_list args;
	DB *db = unwrap(this);

	va_start(args, format);
	__db_real_err(db->dbenv, 0, 0, 1, format, args);
	va_end(args);
}

int Db::fd(int *fdp)
{
	DB *db = unwrap(this);
	int err;

	if ((err = db->fd(db, fdp)) != 0) {
		DB_ERROR("Db::fd", err, this);
		return err;
	}
	return 0;
}

//static
void Db::feedback_intercept(DB *db, int opcode, int pct)
{
	if (db == 0) {
		DB_ERROR("Db::feedback_callback", EINVAL, (Db*)0);
		return;
	}
	Db *cxxdb = (Db *)db->cj_internal;
	if (cxxdb == 0) {
		DB_ERROR("Db::feedback_callback", EINVAL, cxxdb);
		return;
	}
	if (cxxdb->feedback_callback_ == 0) {
		DB_ERROR("Db::feedback_callback", EINVAL, cxxdb);
		return;
	}
	(*cxxdb->feedback_callback_)(cxxdb, opcode, pct);
}

void Db::set_feedback(void (*arg)(Db *, int, int))
{
	DB *db = unwrap(this);

	feedback_callback_ = arg;

	(*(db->set_feedback))(db, feedback_intercept);
}


int Db::get(DbTxn *txnid, Dbt *key, Dbt *value, u_int32_t flags)
{

	DB *db = unwrap(this);
	int err;

	if ((err = db->get(db, unwrap(txnid), key, value, flags)) != 0) {
		// DB_NOTFOUND is a "normal" return, so should not be
		// thrown as an error
		//
		if (err != DB_NOTFOUND) {
			DB_ERROR("Db::get", err, this);
			return err;
		}
	}
	return err;
}

int Db::get_byteswapped() const
{
	DB *db = (DB *)unwrapConst(this);
	return db->get_byteswapped(db);
}

DBTYPE Db::get_type() const
{
	DB *db = (DB *)unwrapConst(this);
	return (DBTYPE)db->get_type(db);
}

int Db::join(Dbc **curslist, Dbc **cursorp, u_int32_t flags)
{
	// Dbc is a "compatible" subclass of DBC -
	// that is, no virtual functions or even extra data members,
	// so this cast, although technically non-portable,
	// "should" always be okay.
	//
	DBC **list = (DBC **)(curslist);
	DB *db = unwrap(this);
	DBC *dbc = 0;
	int err;

	if ((err = db->join(db, list, &dbc, flags)) != 0) {
		DB_ERROR("Db::join_cursor", err, this);
		return err;
	}
	*cursorp = (Dbc*)dbc;
	return 0;
}

int Db::open(const char *name, const char *subname,
	     DBTYPE type, u_int32_t flags, int mode)
{
	int err;
	DB *db = unwrap(this);

	if ((err = db->open(db, name, subname, type, flags, mode)) != 0) {
		DB_ERROR("Db::open", err, this);
	}
	return err;
}

int Db::put(DbTxn *txnid, Dbt *key, Dbt *value, u_int32_t flags)
{
	int err;
	DB *db = unwrap(this);

	if ((err = db->put(db, unwrap(txnid), key, value, flags)) != 0) {

		// DB_KEYEXIST is a "normal" return, so should not be
		// thrown as an error
		//
		if (err != DB_KEYEXIST) {
			DB_ERROR("Db::put", err, this);
			return err;
		}
	}
	return err;
}

int Db::remove(const char *name, const char *subname, u_int32_t flags)
{
	int err;
	DB *db = unwrap(this);

	if (!db) {
		DB_ERROR("Db::remove", EINVAL, this);
		return EINVAL;
	}
	if ((err = db->remove(db, name, subname, flags)) != 0) {
		DB_ERROR("Db::remove", err, this);
		return err;
	}
	return 0;
}

int Db::stat(void *sp, void *(*db_malloc)(size_t), u_int32_t flags)
{
	int err;
	DB *db = unwrap(this);

	if (!db) {
		DB_ERROR("Db::stat", EINVAL, this);
		return EINVAL;
	}
	if ((err = db->stat(db, sp, db_malloc, flags)) != 0) {
		DB_ERROR("Db::stat", err, this);
		return err;
	}
	return 0;
}

int Db::sync(u_int32_t flags)
{
	int err;
	DB *db = unwrap(this);

	if (!db) {
		DB_ERROR("Db::sync", EINVAL, this);
		return EINVAL;
	}
	if ((err = db->sync(db, flags)) != 0) {
		DB_ERROR("Db::sync", err, this);
		return err;
	}
	return 0;
}

int Db::upgrade(const char *name, u_int32_t flags)
{
	int err;
	DB *db = unwrap(this);

	if (!db) {
		DB_ERROR("Db::upgrade", EINVAL, this);
		return EINVAL;
	}
	if ((err = db->upgrade(db, name, flags)) != 0) {
		DB_ERROR("Db::upgrade", err, this);
		return err;
	}
	return 0;
}

// This is a variant of the DB_WO_ACCESS macro to define a simple set_
// method calling the underlying C method, but unlike a simple
// set method, it may return an error or raise an exception.
// Note this macro expects that input _argspec is an argument
// list element (e.g. "char *arg") defined in terms of "arg".
//
#define DB_DB_ACCESS(_name, _argspec)                          \
\
int Db::set_##_name(_argspec)                                  \
{                                                              \
	int ret;                                               \
	DB *db = unwrap(this);                                 \
                                                               \
	if ((ret = (*(db->set_##_name))(db, arg)) != 0) {      \
		DB_ERROR("Db::set_" # _name, ret, this);       \
	}                                                      \
	return ret;                                            \
}

#define DB_DB_ACCESS_NORET(_name, _argspec)                    \
                                                               \
void Db::set_##_name(_argspec)                                 \
{                                                              \
	DB *db = unwrap(this);                                 \
                                                               \
	(*(db->set_##_name))(db, arg);                         \
	return;                                                \
}

DB_DB_ACCESS(bt_compare, int (*arg)(const DBT *, const DBT *))
DB_DB_ACCESS(bt_maxkey, u_int32_t arg)
DB_DB_ACCESS(bt_minkey, u_int32_t arg)
DB_DB_ACCESS(bt_prefix, size_t (*arg)(const DBT *, const DBT *))
DB_DB_ACCESS(dup_compare, int (*arg)(const DBT *, const DBT *))
DB_DB_ACCESS_NORET(errcall, void (*arg)(const char *, char *))
DB_DB_ACCESS_NORET(errfile, FILE *arg)
DB_DB_ACCESS_NORET(errpfx, const char *arg)
DB_DB_ACCESS(flags, u_int32_t arg)
DB_DB_ACCESS(h_ffactor, u_int32_t arg)
DB_DB_ACCESS(h_hash, u_int32_t (*arg)(const void *, u_int32_t))
DB_DB_ACCESS(h_nelem, u_int32_t arg)
DB_DB_ACCESS(lorder, int arg)
DB_DB_ACCESS(malloc, void *(*arg)(size_t))
DB_DB_ACCESS(pagesize, u_int32_t arg)
DB_DB_ACCESS(re_delim, int arg)
DB_DB_ACCESS(re_len, u_int32_t arg)
DB_DB_ACCESS(re_pad, int arg)
DB_DB_ACCESS(re_source, char *arg)

// Here are the set methods that don't fit the above mold.
//

int Db::set_cachesize(u_int32_t gbytes, u_int32_t bytes, int ncache)
{
	int ret;
	DB *db = unwrap(this);

	if ((ret = (*(db->set_cachesize))(db, gbytes, bytes, ncache)) != 0) {
		DB_ERROR("Db::set_cachesize", ret, this);
	}
	return ret;
}

void Db::set_paniccall(void (*callback)(DbEnv *, int))
{
	get_env(0)->set_paniccall(callback);
}

void Db::set_error_stream(class ostream *error_stream)
{
	get_env(0)->set_error_stream(error_stream);
}

DbEnv *Db::get_env(u_int32_t flags)
{
	if (env_ == 0) {
		DB *db = unwrap(this);

		DB_ENV *dbenv = db->dbenv;

		// This 'cannot' happen, since the dbenv is set when
		// the DB object is created.
		//
		if (dbenv == 0) {
			DB_ERROR("Db::get_env (internal): no environment", 0, this);
			return 0;
		}
		env_ = new DbEnv(dbenv, flags);
	}
	return env_;
}

////////////////////////////////////////////////////////////////////////
//                                                                    //
//                            Dbc                                     //
//                                                                    //
////////////////////////////////////////////////////////////////////////

// It's private, and should never be called, but VC4.0 needs it resolved
//
Dbc::~Dbc()
{
}

int Dbc::close()
{
	DBC *cursor = this;
	int err;

	if ((err = cursor->c_close(cursor)) != 0) {
		DB_ERROR("Db::close", err, (DbEnv*)0);
		return err;
	}
	return 0;
}

int Dbc::del(u_int32_t flags_arg)
{
	DBC *cursor = this;
	int err;

	if ((err = cursor->c_del(cursor, flags_arg)) != 0) {

		// DB_KEYEMPTY is a "normal" return, so should not be
		// thrown as an error
		//
		if (err != DB_KEYEMPTY) {
			DB_ERROR("Db::del", err, (Db*)0);
			return err;
		}
	}
	return err;
}

int Dbc::dup(Dbc** cursorp, u_int32_t flags_arg)
{
	DBC *cursor = this;
	DBC *new_cursor = NULL;
	int err;

	if ((err = cursor->c_dup(cursor, &new_cursor, flags_arg)) != 0) {
		DB_ERROR("Db::dup", err, (Db*)0);
		return err;
	}

	// The following cast implies that Dbc can be no larger than DBC
	*cursorp = (Dbc*)new_cursor;
	return 0;
}

int Dbc::get(Dbt* key, Dbt *data, u_int32_t flags_arg)
{
	DBC *cursor = this;
	int err;

	if ((err = cursor->c_get(cursor, key, data, flags_arg)) != 0) {

		// DB_NOTFOUND is a "normal" return, so should not be
		// thrown as an error
		//
		if (err != DB_NOTFOUND) {
			DB_ERROR("Db::get", err, (Db*)0);
			return err;
		}
	}
	return err;
}

int Dbc::put(Dbt* key, Dbt *data, u_int32_t flags_arg)
{
	DBC *cursor = this;
	int err;

	if ((err = cursor->c_put(cursor, key, data, flags_arg)) != 0) {

		// DB_KEYEXIST is a "normal" return, so should not be
		// thrown as an error
		//
		if (err != DB_KEYEXIST) {
			DB_ERROR("Db::put", err, (Db*)0);
			return err;
		}
	}
	return err;
}

////////////////////////////////////////////////////////////////////////
//                                                                    //
//                            Dbt                                 //
//                                                                    //
////////////////////////////////////////////////////////////////////////

Dbt::Dbt()
{
	DBT *dbt = this;
	memset(dbt, 0, sizeof(DBT));
}

Dbt::Dbt(void *data_arg, size_t size_arg)
{
	DBT *dbt = this;
	memset(dbt, 0, sizeof(DBT));
	set_data(data_arg);
	set_size(size_arg);
}

Dbt::~Dbt()
{
}

Dbt::Dbt(const Dbt &that)
{
	const DBT *from = &that;
	DBT *to = this;
	memcpy(to, from, sizeof(DBT));
}

Dbt &Dbt::operator = (const Dbt &that)
{
	if (this != &that) {
		const DBT *from = &that;
		DBT *to = this;
		memcpy(to, from, sizeof(DBT));
	}
	return *this;
}

DB_RW_ACCESS(Dbt, void *, data, data)
DB_RW_ACCESS(Dbt, u_int32_t, size, size)
DB_RW_ACCESS(Dbt, u_int32_t, ulen, ulen)
DB_RW_ACCESS(Dbt, u_int32_t, dlen, dlen)
DB_RW_ACCESS(Dbt, u_int32_t, doff, doff)
DB_RW_ACCESS(Dbt, u_int32_t, flags, flags)
