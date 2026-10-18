/* Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * shim.c -- flat C entry points for the libdb handle methods a binding calls.
 *
 * libdb's methods are function pointers at varying offsets inside opaque
 * handle structs. Rather than make Rust/Zig reproduce those offsets (fragile
 * against a layout change under the stable soname), this shim lets the C
 * compiler -- which reads the real db.h -- resolve each `handle->method`. The
 * binding then calls a flat `libdb_<handle>_<method>` symbol and never knows a
 * struct offset. This is the same seam the C++/Java bindings use.
 *
 * The api_internal / api1_internal accessors stash the host callback context
 * behind the handle (the C++/Java trampoline trick, survey cxx/java): C++ uses
 * dbenv->api1_internal, Java api2_internal, both use db->api_internal -- this
 * binding takes api1_internal for DB_ENV and api_internal for DB.
 *
 * One line per wrapped method; keep in step with shim.h. A missing symbol is a
 * link error, and the drift gate keeps the constants honest. */
#include <sys/types.h>
#include <db.h>
#include "shim.h"

/* ---- DB_ENV ---- */
int libdb_env_open(DB_ENV *e, const char *home, u_int32_t flags, int mode)
{ return e->open(e, home, flags, mode); }
int libdb_env_close(DB_ENV *e, u_int32_t flags)
{ return e->close(e, flags); }
int libdb_env_set_cachesize(DB_ENV *e, u_int32_t gb, u_int32_t b, int nc)
{ return e->set_cachesize(e, gb, b, nc); }
int libdb_env_set_flags(DB_ENV *e, u_int32_t flags, int onoff)
{ return e->set_flags(e, flags, onoff); }
int libdb_env_set_errpfx(DB_ENV *e, const char *pfx)
{ e->set_errpfx(e, pfx); return 0; }
int libdb_env_set_errcall(DB_ENV *e,
    void (*cb)(const DB_ENV *, const char *, const char *))
{ e->set_errcall(e, cb); return 0; }
int libdb_env_set_msgcall(DB_ENV *e, void (*cb)(const DB_ENV *, const char *))
{ e->set_msgcall(e, cb); return 0; }
int libdb_env_txn_begin(DB_ENV *e, DB_TXN *parent, DB_TXN **tid, u_int32_t flags)
{ return e->txn_begin(e, parent, tid, flags); }
int libdb_env_set_api1_internal(DB_ENV *e, void *p)
{ e->api1_internal = p; return 0; }
void *libdb_env_get_api1_internal(DB_ENV *e)
{ return e->api1_internal; }

/* ---- DB ---- */
int libdb_db_open(DB *d, DB_TXN *txn, const char *file, const char *database,
    int dbtype, u_int32_t flags, int mode)
{ return d->open(d, txn, file, database, (DBTYPE)dbtype, flags, mode); }
int libdb_db_close(DB *d, u_int32_t flags)
{ return d->close(d, flags); }
int libdb_db_put(DB *d, DB_TXN *txn, DBT *key, DBT *data, u_int32_t flags)
{ return d->put(d, txn, key, data, flags); }
int libdb_db_get(DB *d, DB_TXN *txn, DBT *key, DBT *data, u_int32_t flags)
{ return d->get(d, txn, key, data, flags); }
int libdb_db_del(DB *d, DB_TXN *txn, DBT *key, u_int32_t flags)
{ return d->del(d, txn, key, flags); }
int libdb_db_exists(DB *d, DB_TXN *txn, DBT *key, u_int32_t flags)
{ return d->exists(d, txn, key, flags); }
int libdb_db_pget(DB *d, DB_TXN *txn, DBT *key, DBT *pkey, DBT *data,
    u_int32_t flags)
{ return d->pget(d, txn, key, pkey, data, flags); }
int libdb_db_cursor(DB *d, DB_TXN *txn, DBC **cur, u_int32_t flags)
{ return d->cursor(d, txn, cur, flags); }
int libdb_db_set_pagesize(DB *d, u_int32_t ps)
{ return d->set_pagesize(d, ps); }
int libdb_db_set_flags(DB *d, u_int32_t flags)
{ return d->set_flags(d, flags); }
int libdb_db_associate(DB *primary, DB_TXN *txn, DB *secondary,
    int (*cb)(DB *, const DBT *, const DBT *, DBT *), u_int32_t flags)
{ return primary->associate(primary, txn, secondary, cb, flags); }
int libdb_db_set_bt_compare(DB *d, int (*cb)(DB *, const DBT *, const DBT *))
{ return d->set_bt_compare(d, cb); }
int libdb_db_set_dup_compare(DB *d, int (*cb)(DB *, const DBT *, const DBT *))
{ return d->set_dup_compare(d, cb); }
int libdb_db_set_h_hash(DB *d, u_int32_t (*cb)(DB *, const void *, u_int32_t))
{ return d->set_h_hash(d, cb); }
int libdb_db_set_api_internal(DB *d, void *p)
{ d->api_internal = p; return 0; }
void *libdb_db_get_api_internal(DB *d)
{ return d->api_internal; }

/* ---- DBC (cursor) ---- */
int libdb_dbc_close(DBC *c)
{ return c->close(c); }
int libdb_dbc_get(DBC *c, DBT *key, DBT *data, u_int32_t flags)
{ return c->get(c, key, data, flags); }
int libdb_dbc_put(DBC *c, DBT *key, DBT *data, u_int32_t flags)
{ return c->put(c, key, data, flags); }
int libdb_dbc_del(DBC *c, u_int32_t flags)
{ return c->del(c, flags); }
int libdb_dbc_count(DBC *c, db_recno_t *countp, u_int32_t flags)
{ return c->count(c, countp, flags); }
int libdb_dbc_dup(DBC *c, DBC **newc, u_int32_t flags)
{ return c->dup(c, newc, flags); }
int libdb_dbc_pget(DBC *c, DBT *key, DBT *pkey, DBT *data, u_int32_t flags)
{ return c->pget(c, key, pkey, data, flags); }

/* ---- DB_TXN ---- */
int libdb_txn_commit(DB_TXN *t, u_int32_t flags)
{ return t->commit(t, flags); }
int libdb_txn_abort(DB_TXN *t)
{ return t->abort(t); }
int libdb_txn_discard(DB_TXN *t, u_int32_t flags)
{ return t->discard(t, flags); }
int libdb_txn_prepare(DB_TXN *t, u_int8_t *gid)
{ return t->prepare(t, gid); }
u_int32_t libdb_txn_id(DB_TXN *t)
{ return t->id(t); }
int libdb_txn_set_name(DB_TXN *t, const char *name)
{ return t->set_name(t, name); }
int libdb_txn_set_timeout(DB_TXN *t, db_timeout_t to, u_int32_t flags)
{ return t->set_timeout(t, to, flags); }

/* ---- DB_SEQUENCE ---- */
int libdb_seq_open(DB_SEQUENCE *s, DB_TXN *txn, DBT *key, u_int32_t flags)
{ return s->open(s, txn, key, flags); }
int libdb_seq_get(DB_SEQUENCE *s, DB_TXN *txn, int32_t delta, db_seq_t *retp,
    u_int32_t flags)
{ return s->get(s, txn, delta, retp, flags); }
int libdb_seq_close(DB_SEQUENCE *s, u_int32_t flags)
{ return s->close(s, flags); }
int libdb_seq_remove(DB_SEQUENCE *s, DB_TXN *txn, u_int32_t flags)
{ return s->remove(s, txn, flags); }
int libdb_seq_initial_value(DB_SEQUENCE *s, db_seq_t v)
{ return s->initial_value(s, v); }
int libdb_seq_set_cachesize(DB_SEQUENCE *s, int32_t sz)
{ return s->set_cachesize(s, sz); }
int libdb_seq_set_flags(DB_SEQUENCE *s, u_int32_t flags)
{ return s->set_flags(s, flags); }
int libdb_seq_set_range(DB_SEQUENCE *s, db_seq_t min, db_seq_t max)
{ return s->set_range(s, min, max); }
