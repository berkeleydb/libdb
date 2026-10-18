// SPDX-License-Identifier: Sleepycat
//! Layer 1 — raw FFI for libdb. Opaque handles, the `DBT` carrier, the
//! constants a binding bakes in, and the method-pointer signatures Layer 2
//! calls through. Everything here is `unsafe` to use directly.
//!
//! Handles (`DB_ENV`, `DB`, `DBC`, `DB_TXN`, …) are OPAQUE: the C library
//! allocates and owns them, and a binding must only call through the method
//! pointers, never read struct fields. We model each as an opaque struct and
//! use `*mut` to it. The one exception is `DBT`, which the application fills
//! in, so we mirror it as `#[repr(C)]`.
//!
//! The constants are RE-VERIFIED against the installed `db.h` by the drift
//! gate (`libdb-sys`'s own test); if the C library changes a value, the gate
//! FAILs rather than letting the binding silently use a stale number.
#![allow(non_camel_case_types)]

use core::ffi::{c_char, c_int, c_uint, c_void};

pub type u_int32_t = c_uint;
pub type db_recno_t = c_uint;

// --- opaque handles ---------------------------------------------------------
// We never spell the struct bodies: the layout is the library's, and it is
// free to change under a stable soname. A binding depends on the method
// signatures, not the layout.
#[repr(C)] pub struct DB_ENV { _p: [u8; 0] }
#[repr(C)] pub struct DB { _p: [u8; 0] }
#[repr(C)] pub struct DBC { _p: [u8; 0] }
#[repr(C)] pub struct DB_TXN { _p: [u8; 0] }

// --- DBT: the one struct the app fills in -----------------------------------
// Mirrors struct __db_dbt (db.in:225). Only the fields a binding touches are
// named; the C struct is 40 bytes on LP64 (verified), and #[repr(C)] with the
// exact field order/types reproduces it. We keep every field so the size and
// offsets match the C layout exactly.
#[repr(C)]
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

// --- DBTYPE -----------------------------------------------------------------
pub const DB_BTREE: c_int = 1;
pub const DB_HASH: c_int = 2;
pub const DB_RECNO: c_int = 3;
pub const DB_QUEUE: c_int = 4;
pub const DB_UNKNOWN: c_int = 5;
pub const DB_HEAP: c_int = 6;

// --- open / env flags (values verified on the HEAD build) -------------------
pub const DB_CREATE: u_int32_t = 0x0000_0001;
pub const DB_RDONLY: u_int32_t = 0x0000_0400;
pub const DB_EXCL: u_int32_t = 0x0000_0004;
pub const DB_THREAD: u_int32_t = 0x0000_0020;
pub const DB_AUTO_COMMIT: u_int32_t = 0x0000_0100;
pub const DB_INIT_LOCK: u_int32_t = 0x0000_0100;
pub const DB_INIT_LOG: u_int32_t = 0x0000_0200;
pub const DB_INIT_MPOOL: u_int32_t = 0x0000_0400;
pub const DB_INIT_TXN: u_int32_t = 0x0000_2000;
pub const DB_RECOVER: u_int32_t = 0x0000_0002;

// --- DBT flags --------------------------------------------------------------
pub const DB_DBT_MALLOC: u_int32_t = 0x010;
pub const DB_DBT_USERMEM: u_int32_t = 0x800;
pub const DB_DBT_PARTIAL: u_int32_t = 0x040;

// --- cursor get / put opcodes ----------------------------------------------
pub const DB_FIRST: u_int32_t = 7;
pub const DB_NEXT: u_int32_t = 16;
pub const DB_SET: u_int32_t = 26;
pub const DB_SET_RANGE: u_int32_t = 27;
pub const DB_NOOVERWRITE: u_int32_t = 20;

// --- return codes -----------------------------------------------------------
pub const DB_NOTFOUND: c_int = -30988;
pub const DB_KEYEXIST: c_int = -30994;
pub const DB_LOCK_DEADLOCK: c_int = -30993;
pub const DB_BUFFER_SMALL: c_int = -30999;
pub const DB_RUNRECOVERY: c_int = -30973;

// --- the method tables ------------------------------------------------------
// We spell ONLY the method pointers Layer 2 calls, at their real offsets. The
// trick: a handle is opaque, but its first fields through the method block are
// an ABI contract. Rather than reproduce the whole struct, we call through
// typed wrappers that fetch the method pointer by reading the function-pointer
// field. The simplest correct approach that stays within "opaque" is to NOT
// reach into the struct at all and instead use the top-level C entry points
// plus the fact that every method takes its handle first — but libdb has no
// top-level put/get. So Layer 1 declares the method-pointer STRUCT PREFIXES it
// needs. For the Phase-0 proof we use the documented, stable prefix of each
// handle's public method block.
//
// For correctness and simplicity in this proof we instead bind the handful of
// methods via the C-visible function-pointer ABI using a minimal shim: see
// `methods` below, which mirrors just the leading method pointers we use. This
// is the one place a real binding chooses "generate the full table" vs "mirror
// the prefix"; the drift gate guards either.

extern "C" {
    pub fn db_env_create(dbenvp: *mut *mut DB_ENV, flags: u_int32_t) -> c_int;
    pub fn db_create(dbp: *mut *mut DB, dbenv: *mut DB_ENV, flags: u_int32_t) -> c_int;
    pub fn db_strerror(error: c_int) -> *const c_char;
    pub fn db_version(major: *mut c_int, minor: *mut c_int, patch: *mut c_int) -> *const c_char;
}

// The method-pointer prefixes. These MUST match db.in's public handle list
// order up to the last method we name; the drift gate asserts the offsets by
// calling each through C. For the proof we expose accessors that read the
// pointer at its known position via a generated C helper (see methods.c linked
// by build.rs in the full crate). Here, Phase 0 calls env/db methods through a
// tiny set of C trampolines compiled in `shim.c`.
extern "C" {
    // Thin C trampolines (shim.c) that call the method pointers, so Layer 1
    // need not reproduce the opaque struct layout at all. Each takes the
    // handle and the method's args. This keeps Rust free of any struct-offset
    // assumption — the C compiler, reading the real db.h, resolves the method.
    pub fn libdb_env_open(e: *mut DB_ENV, home: *const c_char, flags: u_int32_t, mode: c_int) -> c_int;
    pub fn libdb_env_close(e: *mut DB_ENV, flags: u_int32_t) -> c_int;
    pub fn libdb_env_set_cachesize(e: *mut DB_ENV, gb: u_int32_t, b: u_int32_t, nc: c_int) -> c_int;
    pub fn libdb_db_open(d: *mut DB, txn: *mut DB_TXN, file: *const c_char, database: *const c_char, dbtype: c_int, flags: u_int32_t, mode: c_int) -> c_int;
    pub fn libdb_db_close(d: *mut DB, flags: u_int32_t) -> c_int;
    pub fn libdb_db_put(d: *mut DB, txn: *mut DB_TXN, key: *mut DBT, data: *mut DBT, flags: u_int32_t) -> c_int;
    pub fn libdb_db_get(d: *mut DB, txn: *mut DB_TXN, key: *mut DBT, data: *mut DBT, flags: u_int32_t) -> c_int;
}
