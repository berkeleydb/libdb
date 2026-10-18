/* SPDX-License-Identifier: Sleepycat
 *
 * shim.c -- flat C entry points for the libdb handle methods a binding calls.
 *
 * libdb's methods are function pointers at varying offsets inside opaque
 * handle structs. Rather than make Rust/Zig reproduce those offsets (fragile
 * against a layout change under the stable soname), this shim lets the C
 * compiler -- which reads the real db.h -- resolve each `handle->method`. The
 * binding then calls a flat `libdb_<handle>_<method>` symbol and never knows a
 * struct offset. This is the same seam the C++/Java bindings use (they call
 * db->put through db_cxx.h).
 *
 * One line per wrapped method. Add here when Layer 2 grows; the drift gate
 * ensures the constants stay in step, and a missing symbol is a link error. */
#include <db.h>

int libdb_env_open(DB_ENV *e, const char *home, u_int32_t flags, int mode)
{ return e->open(e, home, flags, mode); }
int libdb_env_close(DB_ENV *e, u_int32_t flags)
{ return e->close(e, flags); }
int libdb_env_set_cachesize(DB_ENV *e, u_int32_t gb, u_int32_t b, int nc)
{ return e->set_cachesize(e, gb, b, nc); }

int libdb_db_open(DB *d, DB_TXN *txn, const char *file, const char *database,
    int dbtype, u_int32_t flags, int mode)
{ return d->open(d, txn, file, database, (DBTYPE)dbtype, flags, mode); }
int libdb_db_close(DB *d, u_int32_t flags)
{ return d->close(d, flags); }
int libdb_db_put(DB *d, DB_TXN *txn, DBT *key, DBT *data, u_int32_t flags)
{ return d->put(d, txn, key, data, flags); }
int libdb_db_get(DB *d, DB_TXN *txn, DBT *key, DBT *data, u_int32_t flags)
{ return d->get(d, txn, key, data, flags); }
