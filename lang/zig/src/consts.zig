// SPDX-License-Identifier: Sleepycat
//! The binding's frozen copy of every libdb constant it depends on.
//!
//! These values are transcribed from `db.h` and VERIFIED against the installed
//! header on a FreeBSD build (DB_CALVER 2026.10.7). The idiomatic layer uses
//! THESE values -- not the `@cImport`ed macros -- so the list is load-bearing:
//! the drift gate (`gen-drift-gate.zig` -> a C program that `#error`s if any
//! disagrees with `db.h`) keeps them honest. Change a value here and the gate
//! fails against the real header.
//!
//! Every entry also appears in the `all` table so the drift gate can emit a
//! `#if NAME != VALUE` check for each without a second hand-maintained list.
const std = @import("std");

// Open / env flags
pub const DB_CREATE: u32 = 0x00000001;
pub const DB_EXCL: u32 = 0x00000004;
pub const DB_RDONLY: u32 = 0x00000400;
pub const DB_THREAD: u32 = 0x00000020;
pub const DB_AUTO_COMMIT: u32 = 0x00000100;
pub const DB_INIT_LOCK: u32 = 0x00000100;
pub const DB_INIT_LOG: u32 = 0x00000200;
pub const DB_INIT_MPOOL: u32 = 0x00000400;
pub const DB_INIT_TXN: u32 = 0x00002000;
pub const DB_PRIVATE: u32 = 0x00010000;
pub const DB_MULTIVERSION: u32 = 0x00000008;

// DBTYPE
pub const DB_BTREE: c_int = 1;
pub const DB_HASH: c_int = 2;
pub const DB_RECNO: c_int = 3;
pub const DB_QUEUE: c_int = 4;
pub const DB_HEAP: c_int = 6;

// Per-DB access-method set_flags
pub const DB_DUP: u32 = 0x00000010;
pub const DB_DUPSORT: u32 = 0x00000002;
pub const DB_RECNUM: u32 = 0x00000040;
pub const DB_RENUMBER: u32 = 0x00000080;

// Transaction begin / commit flags
pub const DB_TXN_NOSYNC: u32 = 0x00000001;
pub const DB_TXN_NOWAIT: u32 = 0x00000002;
pub const DB_TXN_SNAPSHOT: u32 = 0x00000004;
pub const DB_TXN_SYNC: u32 = 0x00000008;
pub const DB_TXN_WRITE_NOSYNC: u32 = 0x00000020;
pub const DB_READ_UNCOMMITTED: u32 = 0x00000200;
pub const DB_READ_COMMITTED: u32 = 0x00000400;

// txn->set_timeout which-flag
pub const DB_SET_TXN_TIMEOUT: u32 = 0x00000002;
pub const DB_SET_LOCK_TIMEOUT: u32 = 0x00000001;

// Deadlock-detector policy for DB_ENV->set_lk_detect (DB_LOCK_DEFAULT runs the
// detector on every lock conflict and victimises a party -- libdb does NOT
// auto-detect without this, so `Env.open` sets it; see `Env.transact`).
pub const DB_LOCK_DEFAULT: u32 = 1;

// Cursor->dup flag: duplicate the cursor at the same position.
pub const DB_POSITION: u32 = 22;

// Cursor get op codes
pub const DB_CURRENT: u32 = 6;
pub const DB_FIRST: u32 = 7;
pub const DB_LAST: u32 = 15;
pub const DB_NEXT: u32 = 16;
pub const DB_PREV: u32 = 23;
pub const DB_NEXT_DUP: u32 = 17;
pub const DB_NEXT_NODUP: u32 = 18;
pub const DB_PREV_DUP: u32 = 24;
pub const DB_PREV_NODUP: u32 = 25;
pub const DB_SET: u32 = 26;
pub const DB_SET_RANGE: u32 = 27;
pub const DB_SET_RECNO: u32 = 28;
pub const DB_GET_RECNO: u32 = 11;
pub const DB_GET_BOTH: u32 = 8;
pub const DB_GET_BOTH_RANGE: u32 = 10;

// put / cursor-put op codes
pub const DB_NOOVERWRITE: u32 = 20;
pub const DB_APPEND: u32 = 2;
pub const DB_NODUPDATA: u32 = 19;
pub const DB_KEYFIRST: u32 = 13;
pub const DB_KEYLAST: u32 = 14;
pub const DB_AFTER: u32 = 1;
pub const DB_BEFORE: u32 = 3;

// Modifier bits OR'd above DB_OPFLAGS_MASK
pub const DB_OPFLAGS_MASK: u32 = 0x000000ff;
pub const DB_RMW: u32 = 0x00002000;
pub const DB_MULTIPLE: u32 = 0x00000800;
pub const DB_MULTIPLE_KEY: u32 = 0x00004000;

// DBT flags
pub const DB_DBT_MALLOC: u32 = 0x00000010;
pub const DB_DBT_USERMEM: u32 = 0x00000800;
pub const DB_DBT_PARTIAL: u32 = 0x00000040;
pub const DB_DBT_MULTIPLE: u32 = 0x00000020;

// DB_SEQUENCE flags
pub const DB_SEQ_DEC: u32 = 0x00000001;
pub const DB_SEQ_INC: u32 = 0x00000002;
pub const DB_SEQ_WRAP: u32 = 0x00000008;

// Return codes (DB range, all < 0)
pub const DB_NOTFOUND: c_int = -30988;
pub const DB_KEYEXIST: c_int = -30994;
pub const DB_LOCK_DEADLOCK: c_int = -30993;
pub const DB_BUFFER_SMALL: c_int = -30999;
pub const DB_RUNRECOVERY: c_int = -30973;
pub const DB_SECONDARY_BAD: c_int = -30972;
pub const DB_DONOTINDEX: c_int = -30998;
pub const DB_KEYEMPTY: c_int = -30995;

/// One drift-gate row: the macro name and the value this binding bakes in.
pub const Entry = struct { name: []const u8, value: i64 };

/// Every constant above, so the drift gate can emit one `#if` per row with no
/// second hand-maintained list. Keep in sync when adding a `pub const`.
pub const all = [_]Entry{
    .{ .name = "DB_CREATE", .value = DB_CREATE },
    .{ .name = "DB_EXCL", .value = DB_EXCL },
    .{ .name = "DB_RDONLY", .value = DB_RDONLY },
    .{ .name = "DB_THREAD", .value = DB_THREAD },
    .{ .name = "DB_AUTO_COMMIT", .value = DB_AUTO_COMMIT },
    .{ .name = "DB_INIT_LOCK", .value = DB_INIT_LOCK },
    .{ .name = "DB_INIT_LOG", .value = DB_INIT_LOG },
    .{ .name = "DB_INIT_MPOOL", .value = DB_INIT_MPOOL },
    .{ .name = "DB_INIT_TXN", .value = DB_INIT_TXN },
    .{ .name = "DB_PRIVATE", .value = DB_PRIVATE },
    .{ .name = "DB_MULTIVERSION", .value = DB_MULTIVERSION },
    .{ .name = "DB_BTREE", .value = DB_BTREE },
    .{ .name = "DB_HASH", .value = DB_HASH },
    .{ .name = "DB_RECNO", .value = DB_RECNO },
    .{ .name = "DB_QUEUE", .value = DB_QUEUE },
    .{ .name = "DB_HEAP", .value = DB_HEAP },
    .{ .name = "DB_DUP", .value = DB_DUP },
    .{ .name = "DB_DUPSORT", .value = DB_DUPSORT },
    .{ .name = "DB_RECNUM", .value = DB_RECNUM },
    .{ .name = "DB_RENUMBER", .value = DB_RENUMBER },
    .{ .name = "DB_TXN_NOSYNC", .value = DB_TXN_NOSYNC },
    .{ .name = "DB_TXN_NOWAIT", .value = DB_TXN_NOWAIT },
    .{ .name = "DB_TXN_SNAPSHOT", .value = DB_TXN_SNAPSHOT },
    .{ .name = "DB_TXN_SYNC", .value = DB_TXN_SYNC },
    .{ .name = "DB_TXN_WRITE_NOSYNC", .value = DB_TXN_WRITE_NOSYNC },
    .{ .name = "DB_READ_UNCOMMITTED", .value = DB_READ_UNCOMMITTED },
    .{ .name = "DB_READ_COMMITTED", .value = DB_READ_COMMITTED },
    .{ .name = "DB_SET_TXN_TIMEOUT", .value = DB_SET_TXN_TIMEOUT },
    .{ .name = "DB_SET_LOCK_TIMEOUT", .value = DB_SET_LOCK_TIMEOUT },
    .{ .name = "DB_LOCK_DEFAULT", .value = DB_LOCK_DEFAULT },
    .{ .name = "DB_POSITION", .value = DB_POSITION },
    .{ .name = "DB_CURRENT", .value = DB_CURRENT },
    .{ .name = "DB_FIRST", .value = DB_FIRST },
    .{ .name = "DB_LAST", .value = DB_LAST },
    .{ .name = "DB_NEXT", .value = DB_NEXT },
    .{ .name = "DB_PREV", .value = DB_PREV },
    .{ .name = "DB_NEXT_DUP", .value = DB_NEXT_DUP },
    .{ .name = "DB_NEXT_NODUP", .value = DB_NEXT_NODUP },
    .{ .name = "DB_PREV_DUP", .value = DB_PREV_DUP },
    .{ .name = "DB_PREV_NODUP", .value = DB_PREV_NODUP },
    .{ .name = "DB_SET", .value = DB_SET },
    .{ .name = "DB_SET_RANGE", .value = DB_SET_RANGE },
    .{ .name = "DB_SET_RECNO", .value = DB_SET_RECNO },
    .{ .name = "DB_GET_RECNO", .value = DB_GET_RECNO },
    .{ .name = "DB_GET_BOTH", .value = DB_GET_BOTH },
    .{ .name = "DB_GET_BOTH_RANGE", .value = DB_GET_BOTH_RANGE },
    .{ .name = "DB_NOOVERWRITE", .value = DB_NOOVERWRITE },
    .{ .name = "DB_APPEND", .value = DB_APPEND },
    .{ .name = "DB_NODUPDATA", .value = DB_NODUPDATA },
    .{ .name = "DB_KEYFIRST", .value = DB_KEYFIRST },
    .{ .name = "DB_KEYLAST", .value = DB_KEYLAST },
    .{ .name = "DB_AFTER", .value = DB_AFTER },
    .{ .name = "DB_BEFORE", .value = DB_BEFORE },
    .{ .name = "DB_OPFLAGS_MASK", .value = DB_OPFLAGS_MASK },
    .{ .name = "DB_RMW", .value = DB_RMW },
    .{ .name = "DB_MULTIPLE", .value = DB_MULTIPLE },
    .{ .name = "DB_MULTIPLE_KEY", .value = DB_MULTIPLE_KEY },
    .{ .name = "DB_DBT_MALLOC", .value = DB_DBT_MALLOC },
    .{ .name = "DB_DBT_USERMEM", .value = DB_DBT_USERMEM },
    .{ .name = "DB_DBT_PARTIAL", .value = DB_DBT_PARTIAL },
    .{ .name = "DB_DBT_MULTIPLE", .value = DB_DBT_MULTIPLE },
    .{ .name = "DB_SEQ_DEC", .value = DB_SEQ_DEC },
    .{ .name = "DB_SEQ_INC", .value = DB_SEQ_INC },
    .{ .name = "DB_SEQ_WRAP", .value = DB_SEQ_WRAP },
    .{ .name = "DB_NOTFOUND", .value = DB_NOTFOUND },
    .{ .name = "DB_KEYEXIST", .value = DB_KEYEXIST },
    .{ .name = "DB_LOCK_DEADLOCK", .value = DB_LOCK_DEADLOCK },
    .{ .name = "DB_BUFFER_SMALL", .value = DB_BUFFER_SMALL },
    .{ .name = "DB_RUNRECOVERY", .value = DB_RUNRECOVERY },
    .{ .name = "DB_SECONDARY_BAD", .value = DB_SECONDARY_BAD },
    .{ .name = "DB_DONOTINDEX", .value = DB_DONOTINDEX },
    .{ .name = "DB_KEYEMPTY", .value = DB_KEYEMPTY },
};
