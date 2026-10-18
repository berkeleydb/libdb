/* SPDX-License-Identifier: Sleepycat
 *
 * shim.c -- flat C entry points for the libdb handle methods a binding calls.
 *
 * libdb's methods are function pointers at varying offsets inside opaque
 * handle structs. Rather than make Rust reproduce those offsets (fragile
 * against a layout change under the stable soname), this shim lets the C
 * compiler -- which reads the real db.h -- resolve each `handle->method`. The
 * binding then calls a flat `libdb_<handle>_<method>` symbol and never knows a
 * struct offset. This is the same seam the C++/Java bindings use.
 *
 * The app_private accessors give Layer 2 a documented public slot (db.in:1451
 * DB.app_private, db.in:2364 DB_ENV.app_private) to stash the host callback
 * object behind, so the extern-C comparator/hash/associate thunks can recover
 * it. We take app_private rather than api_internal: both are public, but
 * app_private is the slot db.h documents as the application's.
 *
 * One line per wrapped method. Add here when Layer 2 grows; a missing symbol
 * is a link error, and the drift gate keeps the constants in step. */
#include <db.h>

/* ----- DB_ENV ----- */
int libdb_env_open(DB_ENV *e, const char *home, u_int32_t flags, int mode)
{ return e->open(e, home, flags, mode); }
int libdb_env_close(DB_ENV *e, u_int32_t flags)
{ return e->close(e, flags); }
int libdb_env_set_cachesize(DB_ENV *e, u_int32_t gb, u_int32_t b, int nc)
{ return e->set_cachesize(e, gb, b, nc); }
int libdb_env_set_flags(DB_ENV *e, u_int32_t flags, int onoff)
{ return e->set_flags(e, flags, onoff); }
void libdb_env_set_errpfx(DB_ENV *e, const char *pfx)
{ e->set_errpfx(e, pfx); }
void libdb_env_set_errcall(DB_ENV *e,
    void (*cb)(const DB_ENV *, const char *, const char *))
{ e->set_errcall(e, cb); }
void libdb_env_set_msgcall(DB_ENV *e, void (*cb)(const DB_ENV *, const char *))
{ e->set_msgcall(e, cb); }
int libdb_env_txn_begin(DB_ENV *e, DB_TXN *parent, DB_TXN **out, u_int32_t flags)
{ return e->txn_begin(e, parent, out, flags); }
int libdb_env_txn_checkpoint(DB_ENV *e, u_int32_t kb, u_int32_t min, u_int32_t flags)
{ return e->txn_checkpoint(e, kb, min, flags); }
void libdb_env_set_app_private(DB_ENV *e, void *p) { e->app_private = p; }
void *libdb_env_get_app_private(DB_ENV *e) { return e->app_private; }

/* ----- DB ----- */
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
int libdb_db_pget(DB *d, DB_TXN *txn, DBT *key, DBT *pkey, DBT *data, u_int32_t flags)
{ return d->pget(d, txn, key, pkey, data, flags); }
int libdb_db_set_pagesize(DB *d, u_int32_t bytes)
{ return d->set_pagesize(d, bytes); }
int libdb_db_set_flags(DB *d, u_int32_t flags)
{ return d->set_flags(d, flags); }
int libdb_db_cursor(DB *d, DB_TXN *txn, DBC **out, u_int32_t flags)
{ return d->cursor(d, txn, out, flags); }
int libdb_db_associate(DB *d, DB_TXN *txn, DB *secondary,
    int (*cb)(DB *, const DBT *, const DBT *, DBT *), u_int32_t flags)
{ return d->associate(d, txn, secondary, cb, flags); }
int libdb_db_set_bt_compare(DB *d, int (*cb)(DB *, const DBT *, const DBT *))
{ return d->set_bt_compare(d, cb); }
int libdb_db_set_dup_compare(DB *d, int (*cb)(DB *, const DBT *, const DBT *))
{ return d->set_dup_compare(d, cb); }
int libdb_db_set_h_hash(DB *d, u_int32_t (*cb)(DB *, const void *, u_int32_t))
{ return d->set_h_hash(d, cb); }
void libdb_db_set_app_private(DB *d, void *p) { d->app_private = p; }
void *libdb_db_get_app_private(DB *d) { return d->app_private; }
int libdb_db_truncate(DB *d, DB_TXN *txn, u_int32_t *count, u_int32_t flags)
{ return d->truncate(d, txn, count, flags); }
int libdb_db_sync(DB *d, u_int32_t flags)
{ return d->sync(d, flags); }

/* ----- DBC (cursor) ----- */
int libdb_dbc_close(DBC *c) { return c->close(c); }
int libdb_dbc_get(DBC *c, DBT *key, DBT *data, u_int32_t flags)
{ return c->get(c, key, data, flags); }
int libdb_dbc_pget(DBC *c, DBT *key, DBT *pkey, DBT *data, u_int32_t flags)
{ return c->pget(c, key, pkey, data, flags); }
int libdb_dbc_put(DBC *c, DBT *key, DBT *data, u_int32_t flags)
{ return c->put(c, key, data, flags); }
int libdb_dbc_del(DBC *c, u_int32_t flags) { return c->del(c, flags); }
int libdb_dbc_count(DBC *c, db_recno_t *countp, u_int32_t flags)
{ return c->count(c, countp, flags); }
int libdb_dbc_dup(DBC *c, DBC **out, u_int32_t flags)
{ return c->dup(c, out, flags); }

/* ----- DB_TXN ----- */
int libdb_txn_commit(DB_TXN *t, u_int32_t flags) { return t->commit(t, flags); }
int libdb_txn_abort(DB_TXN *t) { return t->abort(t); }
int libdb_txn_discard(DB_TXN *t, u_int32_t flags) { return t->discard(t, flags); }
int libdb_txn_prepare(DB_TXN *t, u_int8_t *gid) { return t->prepare(t, gid); }
u_int32_t libdb_txn_id(DB_TXN *t) { return t->id(t); }
int libdb_txn_set_name(DB_TXN *t, const char *name) { return t->set_name(t, name); }
int libdb_txn_set_timeout(DB_TXN *t, db_timeout_t timeout, u_int32_t flags)
{ return t->set_timeout(t, timeout, flags); }

/* ----- DB_SEQUENCE ----- */
int libdb_seq_open(DB_SEQUENCE *s, DB_TXN *txn, DBT *key, u_int32_t flags)
{ return s->open(s, txn, key, flags); }
int libdb_seq_get(DB_SEQUENCE *s, DB_TXN *txn, int32_t delta, db_seq_t *retp, u_int32_t flags)
{ return s->get(s, txn, delta, retp, flags); }
int libdb_seq_close(DB_SEQUENCE *s, u_int32_t flags) { return s->close(s, flags); }
int libdb_seq_remove(DB_SEQUENCE *s, DB_TXN *txn, u_int32_t flags)
{ return s->remove(s, txn, flags); }
int libdb_seq_initial_value(DB_SEQUENCE *s, db_seq_t value)
{ return s->initial_value(s, value); }
int libdb_seq_set_cachesize(DB_SEQUENCE *s, int32_t size)
{ return s->set_cachesize(s, size); }
int libdb_seq_set_range(DB_SEQUENCE *s, db_seq_t min, db_seq_t max)
{ return s->set_range(s, min, max); }
