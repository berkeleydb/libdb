/* Copyright (c) 2026 libdb contributors.  All rights reserved.
 *
 * SPDX-License-Identifier: Sleepycat
 *
 * shim.h -- prototypes for the flat method entry points in shim.c, so a
 * binding's @cImport/@cInclude (Zig) or a hand-decl (Rust) sees them. The
 * flat `libdb_<handle>_<method>` symbols let the C compiler -- which reads the
 * real db.h -- resolve each handle's method-pointer offset, so the binding
 * never assumes struct layout. Add one line here and in shim.c per wrapped
 * method. */
#ifndef LIBDB_SHIM_H
#define LIBDB_SHIM_H
#include <sys/types.h>
#include <db.h>

/* ---- DB_ENV ---- */
int libdb_env_open(DB_ENV *, const char *, u_int32_t, int);
int libdb_env_close(DB_ENV *, u_int32_t);
int libdb_env_set_cachesize(DB_ENV *, u_int32_t, u_int32_t, int);
int libdb_env_set_flags(DB_ENV *, u_int32_t, int);
int libdb_env_set_errpfx(DB_ENV *, const char *);
int libdb_env_set_errcall(DB_ENV *,
    void (*)(const DB_ENV *, const char *, const char *));
int libdb_env_set_msgcall(DB_ENV *, void (*)(const DB_ENV *, const char *));
int libdb_env_txn_begin(DB_ENV *, DB_TXN *, DB_TXN **, u_int32_t);
int libdb_env_set_api1_internal(DB_ENV *, void *);
void *libdb_env_get_api1_internal(DB_ENV *);

/* ---- DB ---- */
int libdb_db_open(DB *, DB_TXN *, const char *, const char *, int, u_int32_t, int);
int libdb_db_close(DB *, u_int32_t);
int libdb_db_put(DB *, DB_TXN *, DBT *, DBT *, u_int32_t);
int libdb_db_get(DB *, DB_TXN *, DBT *, DBT *, u_int32_t);
int libdb_db_del(DB *, DB_TXN *, DBT *, u_int32_t);
int libdb_db_exists(DB *, DB_TXN *, DBT *, u_int32_t);
int libdb_db_pget(DB *, DB_TXN *, DBT *, DBT *, DBT *, u_int32_t);
int libdb_db_cursor(DB *, DB_TXN *, DBC **, u_int32_t);
int libdb_db_set_pagesize(DB *, u_int32_t);
int libdb_db_set_flags(DB *, u_int32_t);
int libdb_db_associate(DB *, DB_TXN *, DB *,
    int (*)(DB *, const DBT *, const DBT *, DBT *), u_int32_t);
int libdb_db_set_bt_compare(DB *, int (*)(DB *, const DBT *, const DBT *));
int libdb_db_set_dup_compare(DB *, int (*)(DB *, const DBT *, const DBT *));
int libdb_db_set_h_hash(DB *, u_int32_t (*)(DB *, const void *, u_int32_t));
int libdb_db_set_api_internal(DB *, void *);
void *libdb_db_get_api_internal(DB *);

/* ---- DBC (cursor) ---- */
int libdb_dbc_close(DBC *);
int libdb_dbc_get(DBC *, DBT *, DBT *, u_int32_t);
int libdb_dbc_put(DBC *, DBT *, DBT *, u_int32_t);
int libdb_dbc_del(DBC *, u_int32_t);
int libdb_dbc_count(DBC *, db_recno_t *, u_int32_t);
int libdb_dbc_dup(DBC *, DBC **, u_int32_t);
int libdb_dbc_pget(DBC *, DBT *, DBT *, DBT *, u_int32_t);

/* ---- DB_TXN ---- */
int libdb_txn_commit(DB_TXN *, u_int32_t);
int libdb_txn_abort(DB_TXN *);
int libdb_txn_discard(DB_TXN *, u_int32_t);
int libdb_txn_prepare(DB_TXN *, u_int8_t *);
u_int32_t libdb_txn_id(DB_TXN *);
int libdb_txn_set_name(DB_TXN *, const char *);
int libdb_txn_set_timeout(DB_TXN *, db_timeout_t, u_int32_t);

/* ---- DB_SEQUENCE ---- */
int libdb_seq_open(DB_SEQUENCE *, DB_TXN *, DBT *, u_int32_t);
int libdb_seq_get(DB_SEQUENCE *, DB_TXN *, int32_t, db_seq_t *, u_int32_t);
int libdb_seq_close(DB_SEQUENCE *, u_int32_t);
int libdb_seq_remove(DB_SEQUENCE *, DB_TXN *, u_int32_t);
int libdb_seq_initial_value(DB_SEQUENCE *, db_seq_t);
int libdb_seq_set_cachesize(DB_SEQUENCE *, int32_t);
int libdb_seq_set_flags(DB_SEQUENCE *, u_int32_t);
int libdb_seq_set_range(DB_SEQUENCE *, db_seq_t, db_seq_t);
#endif
