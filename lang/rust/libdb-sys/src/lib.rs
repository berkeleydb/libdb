// SPDX-License-Identifier: Sleepycat
//! Layer 1 — raw FFI for libdb. Opaque handles, the `DBT` carrier, the
//! constants a binding bakes in, and the flat C method shim Layer 2 calls
//! through. Everything here is `unsafe` to use directly.
//!
//! Handles (`DB_ENV`, `DB`, `DBC`, `DB_TXN`, `DB_SEQUENCE`, …) are OPAQUE: the
//! C library allocates and owns them, and a binding must only call through the
//! method pointers, never read struct fields. We model each as an opaque
//! struct and use `*mut` to it. The one exception is `DBT`, which the
//! application fills in, so we mirror it as `#[repr(C)]`.
//!
//! The constants are RE-VERIFIED against the installed `db.h` by the drift
//! gate (`drift_gate.rs`); if the C library changes a value, the gate FAILs
//! rather than letting the binding silently use a stale number.
#![allow(non_camel_case_types)]

use core::ffi::{c_char, c_int, c_long, c_uint, c_void};

pub type u_int32_t = c_uint;
pub type u_int8_t = u8;
pub type db_recno_t = c_uint;
pub type db_seq_t = i64;
pub type db_timeout_t = c_uint;

// --- opaque handles ---------------------------------------------------------
// We never spell the struct bodies: the layout is the library's, and it is
// free to change under a stable soname. A binding depends on the method
// signatures, not the layout.
#[repr(C)]
pub struct DB_ENV {
    _p: [u8; 0],
}
#[repr(C)]
pub struct DB {
    _p: [u8; 0],
}
#[repr(C)]
pub struct DBC {
    _p: [u8; 0],
}
#[repr(C)]
pub struct DB_TXN {
    _p: [u8; 0],
}
#[repr(C)]
pub struct DB_SEQUENCE {
    _p: [u8; 0],
}

// --- DBT: the one struct the app fills in -----------------------------------
// Mirrors struct __db_dbt (db.in:225). The C struct is 40 bytes on LP64
// (verified by handle_sizes.c), and #[repr(C)] with the exact field
// order/types reproduces it. We keep every field so the size and offsets match
// the C layout exactly.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct DBT {
    pub data: *mut c_void,
    pub size: u_int32_t,
    pub ulen: u_int32_t,
    pub dlen: u_int32_t,
    pub doff: u_int32_t,
    pub app_data: *mut c_void,
    pub flags: u_int32_t,
}
impl Default for DBT {
    fn default() -> Self {
        // A zeroed DBT is the canonical empty carrier.
        unsafe { core::mem::zeroed() }
    }
}

// ===========================================================================
// Constants. EVERY value here is re-checked against db.h by the drift gate.
// To add one: print it from a C program against the installed header, add it
// here AND to `CONSTANTS` below so the gate guards it.
// ===========================================================================

// --- DBTYPE -----------------------------------------------------------------
pub const DB_BTREE: c_int = 1;
pub const DB_HASH: c_int = 2;
pub const DB_RECNO: c_int = 3;
pub const DB_QUEUE: c_int = 4;
pub const DB_UNKNOWN: c_int = 5;
pub const DB_HEAP: c_int = 6;

// --- open / env flags -------------------------------------------------------
pub const DB_CREATE: u_int32_t = 0x0000_0001;
pub const DB_RDONLY: u_int32_t = 0x0000_0400;
pub const DB_EXCL: u_int32_t = 0x0000_0004;
pub const DB_THREAD: u_int32_t = 0x0000_0020;
pub const DB_AUTO_COMMIT: u_int32_t = 0x0000_0100;
pub const DB_TRUNCATE: u_int32_t = 0x0002_0000;
pub const DB_INIT_LOCK: u_int32_t = 0x0000_0100;
pub const DB_INIT_LOG: u_int32_t = 0x0000_0200;
pub const DB_INIT_MPOOL: u_int32_t = 0x0000_0400;
pub const DB_INIT_TXN: u_int32_t = 0x0000_2000;
pub const DB_RECOVER: u_int32_t = 0x0000_0002;
pub const DB_PRIVATE: u_int32_t = 0x0001_0000;
pub const DB_MULTIVERSION: u_int32_t = 0x0000_0008;

// --- set_flags access-method flags ------------------------------------------
pub const DB_DUP: u_int32_t = 0x0000_0010;
pub const DB_DUPSORT: u_int32_t = 0x0000_0002;
pub const DB_RECNUM: u_int32_t = 0x0000_0040;
pub const DB_RENUMBER: u_int32_t = 0x0000_0080;

// --- DBT flags --------------------------------------------------------------
pub const DB_DBT_MALLOC: u_int32_t = 0x0000_0010;
pub const DB_DBT_USERMEM: u_int32_t = 0x0000_0800;
pub const DB_DBT_PARTIAL: u_int32_t = 0x0000_0040;
pub const DB_DBT_MULTIPLE: u_int32_t = 0x0000_0020;

// --- cursor get / put opcodes ----------------------------------------------
pub const DB_FIRST: u_int32_t = 7;
pub const DB_LAST: u_int32_t = 15;
pub const DB_NEXT: u_int32_t = 16;
pub const DB_PREV: u_int32_t = 23;
pub const DB_NEXT_DUP: u_int32_t = 17;
pub const DB_NEXT_NODUP: u_int32_t = 18;
pub const DB_PREV_DUP: u_int32_t = 24;
pub const DB_PREV_NODUP: u_int32_t = 25;
pub const DB_CURRENT: u_int32_t = 6;
pub const DB_SET: u_int32_t = 26;
pub const DB_SET_RANGE: u_int32_t = 27;
pub const DB_SET_RECNO: u_int32_t = 28;
pub const DB_GET_RECNO: u_int32_t = 11;
pub const DB_GET_BOTH: u_int32_t = 8;
pub const DB_GET_BOTH_RANGE: u_int32_t = 10;

// --- put flags/opcodes ------------------------------------------------------
pub const DB_NOOVERWRITE: u_int32_t = 20;
pub const DB_APPEND: u_int32_t = 2;
pub const DB_NODUPDATA: u_int32_t = 19;
pub const DB_KEYFIRST: u_int32_t = 13;
pub const DB_KEYLAST: u_int32_t = 14;
pub const DB_AFTER: u_int32_t = 1;
pub const DB_BEFORE: u_int32_t = 3;

// --- opcode modifiers (OR'd above DB_OPFLAGS_MASK=0xff) ----------------------
pub const DB_RMW: u_int32_t = 0x0000_2000;
pub const DB_MULTIPLE: u_int32_t = 0x0000_0800;
pub const DB_MULTIPLE_KEY: u_int32_t = 0x0000_4000;

// --- txn flags --------------------------------------------------------------
pub const DB_TXN_SNAPSHOT: u_int32_t = 0x0000_0004;
pub const DB_TXN_NOWAIT: u_int32_t = 0x0000_0002;
pub const DB_TXN_NOSYNC: u_int32_t = 0x0000_0001;
pub const DB_TXN_WRITE_NOSYNC: u_int32_t = 0x0000_0020;
pub const DB_TXN_SYNC: u_int32_t = 0x0000_0008;
pub const DB_READ_COMMITTED: u_int32_t = 0x0000_0400;
pub const DB_READ_UNCOMMITTED: u_int32_t = 0x0000_0200;

// --- set_timeout which-flags ------------------------------------------------
pub const DB_SET_LOCK_TIMEOUT: u_int32_t = 1;
pub const DB_SET_TXN_TIMEOUT: u_int32_t = 2;

// --- deadlock-detector policies (set_lk_detect) -----------------------------
pub const DB_LOCK_DEFAULT: u_int32_t = 1;
pub const DB_LOCK_MAXLOCKS: u_int32_t = 3;
pub const DB_LOCK_MINWRITE: u_int32_t = 6;
pub const DB_LOCK_OLDEST: u_int32_t = 7;
pub const DB_LOCK_RANDOM: u_int32_t = 8;
pub const DB_LOCK_YOUNGEST: u_int32_t = 9;

// --- return codes -----------------------------------------------------------
pub const DB_NOTFOUND: c_int = -30988;
pub const DB_KEYEXIST: c_int = -30994;
pub const DB_LOCK_DEADLOCK: c_int = -30993;
pub const DB_LOCK_NOTGRANTED: c_int = -30992;
pub const DB_BUFFER_SMALL: c_int = -30999;
pub const DB_RUNRECOVERY: c_int = -30973;
pub const DB_SECONDARY_BAD: c_int = -30972;
pub const DB_DONOTINDEX: c_int = -30998;
pub const DB_FOREIGN_CONFLICT: c_int = -30997;
pub const DB_KEYEMPTY: c_int = -30995;
pub const DB_SNAPSHOT_CONFLICT: c_int = -30968;
pub const DB_SNAPSHOT_UNSAFE: c_int = -30967;

// --- compile-time version (from db.h at build; re-checked at run time) ------
pub const DB_VERSION_MAJOR: c_int = 2026;
pub const DB_VERSION_MINOR: c_int = 0;

/// The binding's constant table, as `(name, value)`, for the drift gate: it
/// emits a C program that `#error`s if any entry disagrees with `db.h`.
/// Signed codes are stored by their bit pattern cast to `i64`.
pub const CONSTANTS: &[(&str, i64)] = &[
    ("DB_BTREE", DB_BTREE as i64),
    ("DB_HASH", DB_HASH as i64),
    ("DB_RECNO", DB_RECNO as i64),
    ("DB_QUEUE", DB_QUEUE as i64),
    ("DB_UNKNOWN", DB_UNKNOWN as i64),
    ("DB_HEAP", DB_HEAP as i64),
    ("DB_CREATE", DB_CREATE as i64),
    ("DB_RDONLY", DB_RDONLY as i64),
    ("DB_EXCL", DB_EXCL as i64),
    ("DB_THREAD", DB_THREAD as i64),
    ("DB_AUTO_COMMIT", DB_AUTO_COMMIT as i64),
    ("DB_TRUNCATE", DB_TRUNCATE as i64),
    ("DB_INIT_LOCK", DB_INIT_LOCK as i64),
    ("DB_INIT_LOG", DB_INIT_LOG as i64),
    ("DB_INIT_MPOOL", DB_INIT_MPOOL as i64),
    ("DB_INIT_TXN", DB_INIT_TXN as i64),
    ("DB_RECOVER", DB_RECOVER as i64),
    ("DB_PRIVATE", DB_PRIVATE as i64),
    ("DB_MULTIVERSION", DB_MULTIVERSION as i64),
    ("DB_DUP", DB_DUP as i64),
    ("DB_DUPSORT", DB_DUPSORT as i64),
    ("DB_RECNUM", DB_RECNUM as i64),
    ("DB_RENUMBER", DB_RENUMBER as i64),
    ("DB_DBT_MALLOC", DB_DBT_MALLOC as i64),
    ("DB_DBT_USERMEM", DB_DBT_USERMEM as i64),
    ("DB_DBT_PARTIAL", DB_DBT_PARTIAL as i64),
    ("DB_DBT_MULTIPLE", DB_DBT_MULTIPLE as i64),
    ("DB_FIRST", DB_FIRST as i64),
    ("DB_LAST", DB_LAST as i64),
    ("DB_NEXT", DB_NEXT as i64),
    ("DB_PREV", DB_PREV as i64),
    ("DB_NEXT_DUP", DB_NEXT_DUP as i64),
    ("DB_NEXT_NODUP", DB_NEXT_NODUP as i64),
    ("DB_PREV_DUP", DB_PREV_DUP as i64),
    ("DB_PREV_NODUP", DB_PREV_NODUP as i64),
    ("DB_CURRENT", DB_CURRENT as i64),
    ("DB_SET", DB_SET as i64),
    ("DB_SET_RANGE", DB_SET_RANGE as i64),
    ("DB_SET_RECNO", DB_SET_RECNO as i64),
    ("DB_GET_RECNO", DB_GET_RECNO as i64),
    ("DB_GET_BOTH", DB_GET_BOTH as i64),
    ("DB_GET_BOTH_RANGE", DB_GET_BOTH_RANGE as i64),
    ("DB_NOOVERWRITE", DB_NOOVERWRITE as i64),
    ("DB_APPEND", DB_APPEND as i64),
    ("DB_NODUPDATA", DB_NODUPDATA as i64),
    ("DB_KEYFIRST", DB_KEYFIRST as i64),
    ("DB_KEYLAST", DB_KEYLAST as i64),
    ("DB_AFTER", DB_AFTER as i64),
    ("DB_BEFORE", DB_BEFORE as i64),
    ("DB_RMW", DB_RMW as i64),
    ("DB_MULTIPLE", DB_MULTIPLE as i64),
    ("DB_MULTIPLE_KEY", DB_MULTIPLE_KEY as i64),
    ("DB_TXN_SNAPSHOT", DB_TXN_SNAPSHOT as i64),
    ("DB_TXN_NOWAIT", DB_TXN_NOWAIT as i64),
    ("DB_TXN_NOSYNC", DB_TXN_NOSYNC as i64),
    ("DB_TXN_WRITE_NOSYNC", DB_TXN_WRITE_NOSYNC as i64),
    ("DB_TXN_SYNC", DB_TXN_SYNC as i64),
    ("DB_READ_COMMITTED", DB_READ_COMMITTED as i64),
    ("DB_READ_UNCOMMITTED", DB_READ_UNCOMMITTED as i64),
    ("DB_SET_LOCK_TIMEOUT", DB_SET_LOCK_TIMEOUT as i64),
    ("DB_SET_TXN_TIMEOUT", DB_SET_TXN_TIMEOUT as i64),
    ("DB_LOCK_DEFAULT", DB_LOCK_DEFAULT as i64),
    ("DB_LOCK_MAXLOCKS", DB_LOCK_MAXLOCKS as i64),
    ("DB_LOCK_MINWRITE", DB_LOCK_MINWRITE as i64),
    ("DB_LOCK_OLDEST", DB_LOCK_OLDEST as i64),
    ("DB_LOCK_RANDOM", DB_LOCK_RANDOM as i64),
    ("DB_LOCK_YOUNGEST", DB_LOCK_YOUNGEST as i64),
    ("DB_NOTFOUND", DB_NOTFOUND as i64),
    ("DB_KEYEXIST", DB_KEYEXIST as i64),
    ("DB_LOCK_DEADLOCK", DB_LOCK_DEADLOCK as i64),
    ("DB_LOCK_NOTGRANTED", DB_LOCK_NOTGRANTED as i64),
    ("DB_BUFFER_SMALL", DB_BUFFER_SMALL as i64),
    ("DB_RUNRECOVERY", DB_RUNRECOVERY as i64),
    ("DB_SECONDARY_BAD", DB_SECONDARY_BAD as i64),
    ("DB_DONOTINDEX", DB_DONOTINDEX as i64),
    ("DB_FOREIGN_CONFLICT", DB_FOREIGN_CONFLICT as i64),
    ("DB_KEYEMPTY", DB_KEYEMPTY as i64),
    ("DB_SNAPSHOT_CONFLICT", DB_SNAPSHOT_CONFLICT as i64),
    ("DB_SNAPSHOT_UNSAFE", DB_SNAPSHOT_UNSAFE as i64),
    ("DB_VERSION_MAJOR", DB_VERSION_MAJOR as i64),
    ("DB_VERSION_MINOR", DB_VERSION_MINOR as i64),
];

// --- the C callback type aliases Layer 2 installs through the shim ----------
/// A comparator thunk: `int (*)(DB *, const DBT *, const DBT *)`.
pub type CompareFn = extern "C" fn(*mut DB, *const DBT, *const DBT) -> c_int;
/// A hash thunk: `u_int32_t (*)(DB *, const void *, u_int32_t)`.
pub type HashFn = extern "C" fn(*mut DB, *const c_void, u_int32_t) -> u_int32_t;
/// A secondary-index extractor: `int (*)(DB *, const DBT *, const DBT *, DBT *)`.
pub type AssociateFn = extern "C" fn(*mut DB, *const DBT, *const DBT, *mut DBT) -> c_int;
/// An error callback: `void (*)(const DB_ENV *, const char *errpfx, const char *msg)`.
pub type ErrcallFn = extern "C" fn(*const DB_ENV, *const c_char, *const c_char);
/// A message callback: `void (*)(const DB_ENV *, const char *msg)`.
pub type MsgcallFn = extern "C" fn(*const DB_ENV, *const c_char);

// --- top-level C entry points -----------------------------------------------
extern "C" {
    pub fn db_env_create(dbenvp: *mut *mut DB_ENV, flags: u_int32_t) -> c_int;
    pub fn db_create(dbp: *mut *mut DB, dbenv: *mut DB_ENV, flags: u_int32_t) -> c_int;
    pub fn db_sequence_create(seqp: *mut *mut DB_SEQUENCE, db: *mut DB, flags: u_int32_t) -> c_int;
    pub fn db_strerror(error: c_int) -> *const c_char;
    pub fn db_version(major: *mut c_int, minor: *mut c_int, patch: *mut c_int) -> *const c_char;
}

// --- flat method shim (shim.c) ----------------------------------------------
// Every wrapped handle method is a flat extern-C symbol `libdb_<handle>_<m>`
// that does `return h->m(h, ...)`. The C compiler, reading the real db.h,
// resolves the method-pointer offset, so Rust never assumes a struct layout.
// This is the same seam the C++/Java bindings use. One prototype here per
// shim.c entry; a missing symbol is a link error.
extern "C" {
    // --- DB_ENV ---
    pub fn libdb_env_open(
        e: *mut DB_ENV,
        home: *const c_char,
        flags: u_int32_t,
        mode: c_int,
    ) -> c_int;
    pub fn libdb_env_close(e: *mut DB_ENV, flags: u_int32_t) -> c_int;
    pub fn libdb_env_set_cachesize(e: *mut DB_ENV, gb: u_int32_t, b: u_int32_t, nc: c_int)
        -> c_int;
    pub fn libdb_env_set_lk_detect(e: *mut DB_ENV, mode: u_int32_t) -> c_int;
    pub fn libdb_env_set_flags(e: *mut DB_ENV, flags: u_int32_t, onoff: c_int) -> c_int;
    pub fn libdb_env_set_errpfx(e: *mut DB_ENV, pfx: *const c_char);
    pub fn libdb_env_set_errcall(e: *mut DB_ENV, cb: Option<ErrcallFn>);
    pub fn libdb_env_set_msgcall(e: *mut DB_ENV, cb: Option<MsgcallFn>);
    pub fn libdb_env_txn_begin(
        e: *mut DB_ENV,
        parent: *mut DB_TXN,
        out: *mut *mut DB_TXN,
        flags: u_int32_t,
    ) -> c_int;
    pub fn libdb_env_txn_checkpoint(
        e: *mut DB_ENV,
        kbyte: u_int32_t,
        min: u_int32_t,
        flags: u_int32_t,
    ) -> c_int;
    pub fn libdb_env_set_app_private(e: *mut DB_ENV, p: *mut c_void);
    pub fn libdb_env_get_app_private(e: *mut DB_ENV) -> *mut c_void;

    // --- allocator (libdb's own, via os/os_alloc.c) ---
    pub fn libdb_umalloc(size: usize) -> *mut c_void;
    pub fn libdb_ufree(p: *mut c_void);

    // --- DB ---
    pub fn libdb_db_open(
        d: *mut DB,
        txn: *mut DB_TXN,
        file: *const c_char,
        database: *const c_char,
        dbtype: c_int,
        flags: u_int32_t,
        mode: c_int,
    ) -> c_int;
    pub fn libdb_db_close(d: *mut DB, flags: u_int32_t) -> c_int;
    pub fn libdb_db_put(
        d: *mut DB,
        txn: *mut DB_TXN,
        key: *mut DBT,
        data: *mut DBT,
        flags: u_int32_t,
    ) -> c_int;
    pub fn libdb_db_get(
        d: *mut DB,
        txn: *mut DB_TXN,
        key: *mut DBT,
        data: *mut DBT,
        flags: u_int32_t,
    ) -> c_int;
    pub fn libdb_db_del(d: *mut DB, txn: *mut DB_TXN, key: *mut DBT, flags: u_int32_t) -> c_int;
    pub fn libdb_db_exists(d: *mut DB, txn: *mut DB_TXN, key: *mut DBT, flags: u_int32_t) -> c_int;
    pub fn libdb_db_pget(
        d: *mut DB,
        txn: *mut DB_TXN,
        key: *mut DBT,
        pkey: *mut DBT,
        data: *mut DBT,
        flags: u_int32_t,
    ) -> c_int;
    pub fn libdb_db_set_pagesize(d: *mut DB, bytes: u_int32_t) -> c_int;
    pub fn libdb_db_set_flags(d: *mut DB, flags: u_int32_t) -> c_int;
    pub fn libdb_db_cursor(
        d: *mut DB,
        txn: *mut DB_TXN,
        out: *mut *mut DBC,
        flags: u_int32_t,
    ) -> c_int;
    pub fn libdb_db_associate(
        d: *mut DB,
        txn: *mut DB_TXN,
        secondary: *mut DB,
        cb: Option<AssociateFn>,
        flags: u_int32_t,
    ) -> c_int;
    pub fn libdb_db_set_bt_compare(d: *mut DB, cb: Option<CompareFn>) -> c_int;
    pub fn libdb_db_set_dup_compare(d: *mut DB, cb: Option<CompareFn>) -> c_int;
    pub fn libdb_db_set_h_hash(d: *mut DB, cb: Option<HashFn>) -> c_int;
    pub fn libdb_db_set_app_private(d: *mut DB, p: *mut c_void);
    pub fn libdb_db_get_app_private(d: *mut DB) -> *mut c_void;
    pub fn libdb_db_truncate(
        d: *mut DB,
        txn: *mut DB_TXN,
        count: *mut u_int32_t,
        flags: u_int32_t,
    ) -> c_int;
    pub fn libdb_db_sync(d: *mut DB, flags: u_int32_t) -> c_int;

    // --- DBC (cursor) ---
    pub fn libdb_dbc_close(c: *mut DBC) -> c_int;
    pub fn libdb_dbc_get(c: *mut DBC, key: *mut DBT, data: *mut DBT, flags: u_int32_t) -> c_int;
    pub fn libdb_dbc_pget(
        c: *mut DBC,
        key: *mut DBT,
        pkey: *mut DBT,
        data: *mut DBT,
        flags: u_int32_t,
    ) -> c_int;
    pub fn libdb_dbc_put(c: *mut DBC, key: *mut DBT, data: *mut DBT, flags: u_int32_t) -> c_int;
    pub fn libdb_dbc_del(c: *mut DBC, flags: u_int32_t) -> c_int;
    pub fn libdb_dbc_count(c: *mut DBC, countp: *mut db_recno_t, flags: u_int32_t) -> c_int;
    pub fn libdb_dbc_dup(c: *mut DBC, out: *mut *mut DBC, flags: u_int32_t) -> c_int;

    // --- DB_TXN ---
    pub fn libdb_txn_commit(t: *mut DB_TXN, flags: u_int32_t) -> c_int;
    pub fn libdb_txn_abort(t: *mut DB_TXN) -> c_int;
    pub fn libdb_txn_discard(t: *mut DB_TXN, flags: u_int32_t) -> c_int;
    pub fn libdb_txn_prepare(t: *mut DB_TXN, gid: *mut u_int8_t) -> c_int;
    pub fn libdb_txn_id(t: *mut DB_TXN) -> u_int32_t;
    pub fn libdb_txn_set_name(t: *mut DB_TXN, name: *const c_char) -> c_int;
    pub fn libdb_txn_set_timeout(t: *mut DB_TXN, timeout: db_timeout_t, flags: u_int32_t) -> c_int;

    // --- DB_SEQUENCE ---
    pub fn libdb_seq_open(
        s: *mut DB_SEQUENCE,
        txn: *mut DB_TXN,
        key: *mut DBT,
        flags: u_int32_t,
    ) -> c_int;
    pub fn libdb_seq_get(
        s: *mut DB_SEQUENCE,
        txn: *mut DB_TXN,
        delta: c_int,
        retp: *mut db_seq_t,
        flags: u_int32_t,
    ) -> c_int;
    pub fn libdb_seq_close(s: *mut DB_SEQUENCE, flags: u_int32_t) -> c_int;
    pub fn libdb_seq_remove(s: *mut DB_SEQUENCE, txn: *mut DB_TXN, flags: u_int32_t) -> c_int;
    pub fn libdb_seq_initial_value(s: *mut DB_SEQUENCE, value: db_seq_t) -> c_int;
    pub fn libdb_seq_set_cachesize(s: *mut DB_SEQUENCE, size: c_int) -> c_int;
    pub fn libdb_seq_set_range(s: *mut DB_SEQUENCE, min: db_seq_t, max: db_seq_t) -> c_int;
}

// keep c_long referenced so a future recover() signature can use it without a
// warning churn; silences unused import on toolchains that notice.
#[doc(hidden)]
pub type ReservedLong = c_long;
