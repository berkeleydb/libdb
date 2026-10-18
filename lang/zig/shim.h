/* SPDX-License-Identifier: Sleepycat
 * shim.h -- prototypes for the flat method entry points in shim.c, so a
 * binding's @cImport/@cInclude (Zig) or a hand-decl (Rust) sees them. */
#ifndef LIBDB_SHIM_H
#define LIBDB_SHIM_H
#include <db.h>
int libdb_env_open(DB_ENV *, const char *, u_int32_t, int);
int libdb_env_close(DB_ENV *, u_int32_t);
int libdb_env_set_cachesize(DB_ENV *, u_int32_t, u_int32_t, int);
int libdb_db_open(DB *, DB_TXN *, const char *, const char *, int, u_int32_t, int);
int libdb_db_close(DB *, u_int32_t);
int libdb_db_put(DB *, DB_TXN *, DBT *, DBT *, u_int32_t);
int libdb_db_get(DB *, DB_TXN *, DBT *, DBT *, u_int32_t);
#endif
