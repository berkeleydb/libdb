// SPDX-License-Identifier: Sleepycat
//! Idiomatic Zig bindings for libdb (the community Berkeley DB fork).
//!
//! Three layers, per RFC 0016:
//!   * Layer 0 -- the C library (`db.h` + `libdb-2026.0.so`).
//!   * Layer 1 -- `shim.h`/`shim.c` flat method entry points + `consts.zig`
//!                frozen constants + the mirrored `DBT`.
//!   * Layer 2 -- this file: owning `Env`/`Db`/`Txn`/`Cursor`/`Sequence`,
//!                error unions, allocator passed in, `defer deinit()`.
//!
//! Zig has no destructors, so the lifecycle is explicit: pair every `create`
//! with `defer handle.deinit()`. In safe builds (`Debug`/`ReleaseSafe`) a
//! closed handle is poisoned and asserted-against on reuse; `ReleaseFast` omits
//! the check. See `reference.md` and the `guide/` chapters.
//!
//! Pinned to Zig 0.16.
const std = @import("std");
const builtin = @import("builtin");
pub const consts = @import("consts.zig");
const c = @cImport({
    @cInclude("shim.h");
});

/// True in `Debug`/`ReleaseSafe` -- the builds that run the lifecycle asserts.
const safe = switch (builtin.mode) {
    .Debug, .ReleaseSafe => true,
    .ReleaseFast, .ReleaseSmall => false,
};

// ---------------------------------------------------------------------------
// Error model (RFC 0016 §6)
// ---------------------------------------------------------------------------

/// The errors the binding raises. The DB control-flow negatives that are not
/// genuine failures (`NotFound`, `KeyExist`, `BufferSmall`) are modelled in
/// return types where they are control flow, and appear here only on the paths
/// that treat them as errors. For `error.Db`/`error.System` read `env.lastCode`
/// for the raw number and `env.lastError()` for DB's diagnostic string.
pub const Error = error{
    /// `DB_LOCK_DEADLOCK` -- abort the txn and retry (see `Env.transact`).
    Deadlock,
    /// `DB_RUNRECOVERY` -- the environment panicked; it must run recovery.
    RunRecovery,
    /// `DB_NOTFOUND` where a value was required.
    NotFound,
    /// `DB_KEYEXIST` where a unique insert was required.
    KeyExist,
    /// `DB_BUFFER_SMALL` -- the caller buffer was too small (see `Db.getInto`).
    BufferSmall,
    /// `DB_SECONDARY_BAD` -- a secondary index is inconsistent.
    SecondaryBad,
    /// Any other DB-range (`< 0`) code; read `env.lastCode`.
    Db,
    /// An OS errno (`> 0`); read `env.lastCode`.
    System,
    /// A usage error raised by the binding itself.
    Usage,
    /// Allocation failed while copying a value out of DB memory.
    OutOfMemory,
};

/// Decode a C return code into a `Error!void`. `0` => ok. Callers that treat
/// `NotFound`/`KeyExist`/`BufferSmall` as control flow check for those codes
/// before calling this.
fn check(code: c_int) Error!void {
    if (code == 0) return;
    return switch (code) {
        consts.DB_LOCK_DEADLOCK => Error.Deadlock,
        consts.DB_RUNRECOVERY => Error.RunRecovery,
        consts.DB_NOTFOUND => Error.NotFound,
        consts.DB_KEYEXIST => Error.KeyExist,
        consts.DB_BUFFER_SMALL => Error.BufferSmall,
        consts.DB_SECONDARY_BAD => Error.SecondaryBad,
        else => if (code < 0) Error.Db else Error.System,
    };
}

/// The library version at run time.
pub fn version() struct { major: i32, minor: i32, patch: i32 } {
    var a: c_int = 0;
    var b: c_int = 0;
    var p: c_int = 0;
    _ = c.db_version(&a, &b, &p);
    return .{ .major = @intCast(a), .minor = @intCast(b), .patch = @intCast(p) };
}

/// Compile-time (header) major/minor the binding was built against.
pub const header_major: i32 = c.DB_VERSION_MAJOR;
pub const header_minor: i32 = c.DB_VERSION_MINOR;

/// Fail if the runtime library disagrees with the header the binding compiled
/// against -- the classic "built against the wrong db.h" footgun (RFC §12).
pub fn checkVersion() Error!void {
    const v = version();
    if (v.major != header_major or v.minor != header_minor) return Error.Usage;
}

// ---------------------------------------------------------------------------
// DBT helpers (RFC 0016 §5)
// ---------------------------------------------------------------------------

/// Build a borrowed-input DBT pointing at `b` for the call's duration. Guards
/// the `size==0`/`data==NULL` shape (a real UB source in the C library).
fn dbtIn(b: []const u8) c.DBT {
    var d: c.DBT = std.mem.zeroes(c.DBT);
    if (b.len != 0) {
        d.data = @constCast(@ptrCast(b.ptr));
        d.size = @intCast(b.len);
    }
    return d;
}

/// Interpret a DB-owned output DBT as a slice (valid only to the next op).
fn dbtSlice(d: c.DBT) []const u8 {
    if (d.data == null or d.size == 0) return &[_]u8{};
    return @as([*]const u8, @ptrCast(d.data))[0..d.size];
}

// ---------------------------------------------------------------------------
// Environment (RFC 0016 §4, §8)
// ---------------------------------------------------------------------------

/// A database environment. Create, configure, open; `defer env.deinit()`.
///
/// `Env` must outlive every `Db`/`Txn` opened in it (RFC survey §7). Open with
/// `DB_THREAD` (the default here) to share it across threads; `Txn` and
/// `Cursor` are never free-threaded.
pub const Env = struct {
    ptr: *c.DB_ENV,
    /// The last raw C return code decoded by this handle's methods.
    last_code: c_int = 0,
    /// DB's last diagnostic string (owned, NUL-terminated) captured via the
    /// errcall trampoline; read with `lastError`.
    err_buf: [512]u8 = undefined,
    err_len: usize = 0,
    closed: bool = false,

    inline fn alive(self: *const Env) void {
        if (safe) std.debug.assert(!self.closed);
    }

    /// Allocate an environment handle. Configure it, then call `open`. The
    /// self-pointer the errcall thunk needs is stashed by `open`/`rebind` once
    /// the `Env` is at its final address (a by-value `create` return would make
    /// a pointer stashed here dangle).
    pub fn create() Error!Env {
        var p: ?*c.DB_ENV = null;
        try check(c.db_env_create(&p, 0));
        return .{ .ptr = p.? };
    }

    /// Re-stash the self pointer. `create` returns by value, so the caller's
    /// copy has a different address than the temporary we stashed; call this
    /// once after the `Env` has reached its final home (the examples and tests
    /// show the idiom). Harmless to call repeatedly.
    pub fn rebind(self: *Env) void {
        _ = c.libdb_env_set_api1_internal(self.ptr, self);
        _ = c.libdb_env_set_errcall(self.ptr, errcallThunk);
    }

    /// Set the cache size (gigabytes + bytes). Call before `open`.
    pub fn setCachesize(self: *Env, gbytes: u32, bytes: u32) Error!void {
        self.alive();
        try check(c.libdb_env_set_cachesize(self.ptr, gbytes, bytes, 1));
    }

    /// Toggle an env-level behavior flag (e.g. `consts.DB_TXN_NOSYNC`).
    pub fn setFlags(self: *Env, flags: u32, on: bool) Error!void {
        self.alive();
        try check(c.libdb_env_set_flags(self.ptr, flags, if (on) 1 else 0));
    }

    /// Set the error-message prefix DB prepends to diagnostics.
    pub fn setErrpfx(self: *Env, pfx: [*:0]const u8) Error!void {
        self.alive();
        try check(c.libdb_env_set_errpfx(self.ptr, pfx));
    }

    /// Route DB's human-readable message strings (`DB->msgcall`) to stderr-like
    /// output. The binding installs a thunk that discards them by default;
    /// enable to see them during development.
    pub fn enableMsgcall(self: *Env, on: bool) Error!void {
        self.alive();
        try check(c.libdb_env_set_msgcall(self.ptr, if (on) msgcallThunk else null));
    }

    /// The last diagnostic string DB produced (empty if none). Valid until the
    /// next failing call. Pair with `error.Db`/`error.System` to explain a
    /// failure the way C's `err`/`errx` would have printed.
    pub fn lastError(self: *const Env) []const u8 {
        return self.err_buf[0..self.err_len];
    }

    /// Open a transactional, free-threaded environment at `home`.
    pub fn open(self: *Env, home: [*:0]const u8) Error!void {
        self.alive();
        self.rebind();
        const flags: u32 = consts.DB_CREATE | consts.DB_INIT_MPOOL |
            consts.DB_INIT_LOCK | consts.DB_INIT_LOG | consts.DB_INIT_TXN |
            consts.DB_THREAD;
        try check(c.libdb_env_open(self.ptr, home, flags, 0o644));
    }

    /// Close the environment. The handle is dead afterwards.
    pub fn deinit(self: *Env) void {
        if (safe and self.closed) return;
        _ = c.libdb_env_close(self.ptr, 0);
        self.closed = true;
    }

    /// Begin a Db builder for this environment.
    pub fn database(self: *Env) DbBuilder {
        self.alive();
        return .{ .env = self };
    }

    /// Open a Btree database in this environment (convenience over the builder).
    pub fn openBtree(self: *Env, file: [*:0]const u8) Error!Db {
        return self.database().open(file);
    }

    /// Begin a transaction. `parent` may be `null`; `flags` OR-combines
    /// `consts.DB_TXN_SNAPSHOT`/`DB_TXN_NOWAIT`/`DB_READ_COMMITTED`/...
    pub fn begin(self: *Env, parent: ?*Txn, flags: u32) Error!Txn {
        self.alive();
        var tp: ?*c.DB_TXN = null;
        const pp: ?*c.DB_TXN = if (parent) |p| p.ptr else null;
        try check(c.libdb_env_txn_begin(self.ptr, pp, &tp, flags));
        return .{ .ptr = tp.?, .env = self };
    }

    /// Run `op` inside a transaction, committing on success and retrying on
    /// `error.Deadlock` (abort + begin again, up to `max_retries` times). This
    /// is the central transactional-BDB ergonomic win (RFC §6). `op` receives
    /// `ctx` and the live `*Txn`; it must not commit/abort the txn itself.
    pub fn transact(
        self: *Env,
        comptime Ctx: type,
        ctx: Ctx,
        comptime op: fn (Ctx, *Txn) Error!void,
    ) Error!void {
        self.alive();
        const max_retries: usize = 10;
        var attempt: usize = 0;
        while (true) : (attempt += 1) {
            var txn = try self.begin(null, 0);
            if (op(ctx, &txn)) |_| {
                try txn.commit();
                return;
            } else |e| {
                txn.abort();
                if (e == Error.Deadlock and attempt < max_retries) continue;
                return e;
            }
        }
    }
};

/// errcall thunk: DB calls this with the env, our prefix, and a message. We
/// recover the `*Env` from `api1_internal` and stash the message so
/// `lastError` can return it. Zig has no exceptions, so nothing can unwind
/// across this FFI boundary.
fn errcallThunk(
    dbenv: ?*const c.DB_ENV,
    errpfx: ?[*:0]const u8,
    msg: ?[*:0]const u8,
) callconv(.c) void {
    _ = errpfx;
    const raw = c.libdb_env_get_api1_internal(@constCast(dbenv.?)) orelse return;
    const env: *Env = @ptrCast(@alignCast(raw));
    const m = std.mem.span(msg orelse return);
    const n = @min(m.len, env.err_buf.len);
    @memcpy(env.err_buf[0..n], m[0..n]);
    env.err_len = n;
}

fn msgcallThunk(dbenv: ?*const c.DB_ENV, msg: ?[*:0]const u8) callconv(.c) void {
    _ = dbenv;
    const m = std.mem.span(msg orelse return);
    std.debug.print("libdb: {s}\n", .{m});
}

// ---------------------------------------------------------------------------
// Database (RFC 0016 §4, §5, §8)
// ---------------------------------------------------------------------------

/// How to order keys / duplicates, and the extra access-method flags.
pub const DbConfig = struct {
    dbtype: c_int = consts.DB_BTREE,
    pagesize: ?u32 = null,
    dup: bool = false,
    dupsort: bool = false,
};

/// Builder for a `Db`. Set fields, then `open`.
pub const DbBuilder = struct {
    env: *Env,
    config: DbConfig = .{},

    pub fn btree(self: DbBuilder) DbBuilder {
        var b = self;
        b.config.dbtype = consts.DB_BTREE;
        return b;
    }
    pub fn hash(self: DbBuilder) DbBuilder {
        var b = self;
        b.config.dbtype = consts.DB_HASH;
        return b;
    }
    pub fn recno(self: DbBuilder) DbBuilder {
        var b = self;
        b.config.dbtype = consts.DB_RECNO;
        return b;
    }
    pub fn queue(self: DbBuilder) DbBuilder {
        var b = self;
        b.config.dbtype = consts.DB_QUEUE;
        return b;
    }
    pub fn pagesize(self: DbBuilder, ps: u32) DbBuilder {
        var b = self;
        b.config.pagesize = ps;
        return b;
    }
    pub fn dup(self: DbBuilder, sorted: bool) DbBuilder {
        var b = self;
        b.config.dup = true;
        b.config.dupsort = sorted;
        return b;
    }

    /// Open the database (auto-committed). Pass `openTxn` to open within a txn.
    pub fn open(self: DbBuilder, file: [*:0]const u8) Error!Db {
        return Db.openImpl(self.env, self.config, file, null, null);
    }
    /// Open within an existing transaction.
    pub fn openTxn(self: DbBuilder, file: [*:0]const u8, txn: *Txn) Error!Db {
        return Db.openImpl(self.env, self.config, file, txn, null);
    }
    /// Open a comparator/hash callback with the database (Phase 3). `cmp_ctx`
    /// is stashed behind the handle for the thunk; see `Comparator`.
    pub fn openWith(
        self: DbBuilder,
        file: [*:0]const u8,
        cb: *const Callbacks,
    ) Error!Db {
        return Db.openImpl(self.env, self.config, file, null, cb);
    }
};

/// Caller-supplied comparator / hash callbacks (Phase 3). A comparator cannot
/// fail (it returns an ordering); a hash returns a `u32`. The context pointer
/// is passed through to each.
pub const Callbacks = struct {
    ctx: *anyopaque,
    bt_compare: ?*const fn (ctx: *anyopaque, a: []const u8, b: []const u8) c_int = null,
    dup_compare: ?*const fn (ctx: *anyopaque, a: []const u8, b: []const u8) c_int = null,
    h_hash: ?*const fn (ctx: *anyopaque, data: []const u8) u32 = null,
};

/// A database (key/value store). `defer db.deinit()`. Must not outlive its Env.
pub const Db = struct {
    ptr: *c.DB,
    env: *Env,
    closed: bool = false,
    /// Kept alive for the handle's lifetime so comparator thunks can reach it.
    cb: ?*const Callbacks = null,
    /// Field used by `associate` to keep the extractor alive for the handle.
    ext: ?*const Extractor = null,

    inline fn alive(self: *const Db) void {
        if (safe) std.debug.assert(!self.closed);
    }

    fn openImpl(
        env: *Env,
        config: DbConfig,
        file: [*:0]const u8,
        txn: ?*Txn,
        cb: ?*const Callbacks,
    ) Error!Db {
        var p: ?*c.DB = null;
        try check(c.db_create(&p, env.ptr, 0));
        const dbp = p.?;
        errdefer _ = c.libdb_db_close(dbp, 0);

        if (config.pagesize) |ps| try check(c.libdb_db_set_pagesize(dbp, ps));
        var sf: u32 = 0;
        if (config.dup) sf |= consts.DB_DUP;
        if (config.dupsort) sf |= consts.DB_DUPSORT;
        if (sf != 0) try check(c.libdb_db_set_flags(dbp, sf));

        if (cb) |callbacks| {
            _ = c.libdb_db_set_api_internal(dbp, @constCast(callbacks));
            if (callbacks.bt_compare != null)
                try check(c.libdb_db_set_bt_compare(dbp, btCompareThunk));
            if (callbacks.dup_compare != null)
                try check(c.libdb_db_set_dup_compare(dbp, dupCompareThunk));
            if (callbacks.h_hash != null)
                try check(c.libdb_db_set_h_hash(dbp, hHashThunk));
        }

        var flags: u32 = consts.DB_CREATE | consts.DB_THREAD;
        const txp: ?*c.DB_TXN = if (txn) |t| t.ptr else blk: {
            flags |= consts.DB_AUTO_COMMIT;
            break :blk null;
        };
        try check(c.libdb_db_open(dbp, txp, file, null, config.dbtype, flags, 0o644));
        return .{ .ptr = dbp, .env = env, .cb = cb };
    }

    pub fn deinit(self: *Db) void {
        if (safe and self.closed) return;
        _ = c.libdb_db_close(self.ptr, 0);
        self.closed = true;
    }

    /// Store `data` under `key` (overwriting; Btree default). Borrowed input.
    /// Pass `txn` or `null` for auto-commit.
    pub fn put(self: *Db, txn: ?*Txn, key: []const u8, data: []const u8) Error!void {
        self.alive();
        var k = dbtIn(key);
        var d = dbtIn(data);
        try check(c.libdb_db_put(self.ptr, txnPtr(txn), &k, &d, 0));
    }

    /// Store only if `key` is absent. Returns `true` if stored, `false` if the
    /// key already existed (`DB_KEYEXIST` demoted to a bool, RFC §6).
    pub fn putNew(self: *Db, txn: ?*Txn, key: []const u8, data: []const u8) Error!bool {
        self.alive();
        var k = dbtIn(key);
        var d = dbtIn(data);
        const rc = c.libdb_db_put(self.ptr, txnPtr(txn), &k, &d, consts.DB_NOOVERWRITE);
        if (rc == consts.DB_KEYEXIST) return false;
        try check(rc);
        return true;
    }

    /// Delete `key`. Returns `false` if the key was absent (not an error).
    pub fn del(self: *Db, txn: ?*Txn, key: []const u8) Error!bool {
        self.alive();
        var k = dbtIn(key);
        const rc = c.libdb_db_del(self.ptr, txnPtr(txn), &k, 0);
        if (rc == consts.DB_NOTFOUND) return false;
        try check(rc);
        return true;
    }

    /// Whether `key` exists (no value copied out).
    pub fn exists(self: *Db, txn: ?*Txn, key: []const u8) Error!bool {
        self.alive();
        var k = dbtIn(key);
        const rc = c.libdb_db_exists(self.ptr, txnPtr(txn), &k, 0);
        if (rc == consts.DB_NOTFOUND) return false;
        try check(rc);
        return true;
    }

    /// Copy-out get (RFC §5 form 1, the safe default). Returns `null` if absent
    /// (`DB_NOTFOUND` is not an error here). The returned slice is owned by
    /// `alloc`; the caller frees it.
    pub fn get(self: *Db, alloc: std.mem.Allocator, txn: ?*Txn, key: []const u8) Error!?[]u8 {
        self.alive();
        var k = dbtIn(key);
        var d: c.DBT = std.mem.zeroes(c.DBT);
        d.flags = consts.DB_DBT_MALLOC;
        const rc = c.libdb_db_get(self.ptr, txnPtr(txn), &k, &d, 0);
        if (rc == consts.DB_NOTFOUND) return null;
        try check(rc);
        defer std.c.free(d.data);
        if (d.data == null or d.size == 0) return try alloc.alloc(u8, 0);
        return try alloc.dupe(u8, dbtSlice(d));
    }

    /// Zero-alloc get into `buf` (RFC §5 form 2, the hot path). Returns the
    /// value length on success, `null` if the key is absent, or
    /// `error.BufferSmall` with the needed length written to `need` so the
    /// caller can grow and retry.
    pub fn getInto(self: *Db, txn: ?*Txn, key: []const u8, buf: []u8, need: *usize) Error!?usize {
        self.alive();
        var k = dbtIn(key);
        var d: c.DBT = std.mem.zeroes(c.DBT);
        d.flags = consts.DB_DBT_USERMEM;
        d.data = if (buf.len != 0) buf.ptr else null;
        d.ulen = @intCast(buf.len);
        const rc = c.libdb_db_get(self.ptr, txnPtr(txn), &k, &d, 0);
        if (rc == consts.DB_NOTFOUND) return null;
        if (rc == consts.DB_BUFFER_SMALL) {
            need.* = d.size;
            return Error.BufferSmall;
        }
        try check(rc);
        return @as(usize, d.size);
    }

    /// Borrowed/window get (RFC §5 form 3, advanced). Calls `fun(ctx, value)`
    /// with a slice valid ONLY for the duration of the call -- the slice must
    /// not escape the closure. Returns `false` if the key is absent.
    ///
    /// NOTE: a `DB_THREAD` handle forbids the raw DB-owned-pointer get ("DB_THREAD
    /// mandates memory allocation flag on data DBT"), so this form fetches with
    /// `DB_DBT_MALLOC` and frees after the closure returns. The *window*
    /// contract still holds (the slice is invalid once `with` returns); it is
    /// not literally zero-alloc on a free-threaded handle -- use `getInto` for
    /// the zero-alloc hot path.
    pub fn with(
        self: *Db,
        txn: ?*Txn,
        key: []const u8,
        comptime Ctx: type,
        ctx: Ctx,
        comptime fun: fn (Ctx, []const u8) void,
    ) Error!bool {
        self.alive();
        var k = dbtIn(key);
        var d: c.DBT = std.mem.zeroes(c.DBT);
        d.flags = consts.DB_DBT_MALLOC;
        const rc = c.libdb_db_get(self.ptr, txnPtr(txn), &k, &d, 0);
        if (rc == consts.DB_NOTFOUND) return false;
        try check(rc);
        defer std.c.free(d.data);
        fun(ctx, dbtSlice(d));
        return true;
    }

    /// Open a cursor on this db (optionally transactional, RFC §8).
    pub fn cursor(self: *Db, txn: ?*Txn) Error!Cursor {
        self.alive();
        var cp: ?*c.DBC = null;
        try check(c.libdb_db_cursor(self.ptr, txnPtr(txn), &cp, 0));
        if (txn) |t| {
            if (safe) t.cursor_count += 1;
        }
        return .{ .ptr = cp.?, .db = self, .txn = txn };
    }

    /// Associate `secondary` with this (primary) db via a key extractor
    /// (Phase 3). The extractor is stored on the SECONDARY handle (its first
    /// arg is the secondary). The extractor returns a slice to index, or `null`
    /// to skip the record (`DB_DONOTINDEX`).
    pub fn associate(self: *Db, txn: ?*Txn, secondary: *Db, extractor: *const Extractor) Error!void {
        self.alive();
        secondary.ext = extractor;
        _ = c.libdb_db_set_api_internal(secondary.ptr, @constCast(extractor));
        try check(c.libdb_db_associate(self.ptr, txnPtr(txn), secondary.ptr, assocThunk, 0));
    }

    /// Secondary pget: look up by the SECONDARY key, returning the PRIMARY key
    /// and the primary data (both copied into `alloc`). `null` if absent.
    pub fn pget(
        self: *Db,
        alloc: std.mem.Allocator,
        txn: ?*Txn,
        skey: []const u8,
    ) Error!?struct { pkey: []u8, data: []u8 } {
        self.alive();
        var sk = dbtIn(skey);
        var pk: c.DBT = std.mem.zeroes(c.DBT);
        pk.flags = consts.DB_DBT_MALLOC;
        var d: c.DBT = std.mem.zeroes(c.DBT);
        d.flags = consts.DB_DBT_MALLOC;
        const rc = c.libdb_db_pget(self.ptr, txnPtr(txn), &sk, &pk, &d, 0);
        if (rc == consts.DB_NOTFOUND) return null;
        try check(rc);
        defer std.c.free(pk.data);
        defer std.c.free(d.data);
        return .{
            .pkey = try alloc.dupe(u8, dbtSlice(pk)),
            .data = try alloc.dupe(u8, dbtSlice(d)),
        };
    }

    /// A bulk-get iterator over `DB_MULTIPLE` records in one buffer (Phase 3,
    /// RFC §5). `buf` must be `u32`-aligned and reasonably large; the iterator
    /// yields `(key_slice? , value_slice)` with no per-record allocation.
    /// `buf` must be at least the db page size, a multiple of 1024 bytes, and
    /// 1024-aligned (the C library rejects anything else, db.in/DB_MULTIPLE).
    pub fn bulk(self: *Db, txn: ?*Txn, buf: []align(1024) u8) Error!BulkIter {
        self.alive();
        var cp: ?*c.DBC = null;
        try check(c.libdb_db_cursor(self.ptr, txnPtr(txn), &cp, 0));
        if (txn) |t| {
            if (safe) t.cursor_count += 1;
        }
        return .{ .cur = cp.?, .buf = buf, .txn = txn };
    }
};

fn txnPtr(txn: ?*Txn) ?*c.DB_TXN {
    return if (txn) |t| t.ptr else null;
}

// --- comparator / hash thunks (Phase 3) -----------------------------------

fn callbacksOf(dbp: ?*c.DB) *const Callbacks {
    const raw = c.libdb_db_get_api_internal(dbp.?).?;
    return @ptrCast(@alignCast(raw));
}

fn btCompareThunk(dbp: ?*c.DB, a: ?*const c.DBT, b: ?*const c.DBT) callconv(.c) c_int {
    const cb = callbacksOf(dbp);
    return cb.bt_compare.?(cb.ctx, dbtSlice(a.?.*), dbtSlice(b.?.*));
}
fn dupCompareThunk(dbp: ?*c.DB, a: ?*const c.DBT, b: ?*const c.DBT) callconv(.c) c_int {
    const cb = callbacksOf(dbp);
    return cb.dup_compare.?(cb.ctx, dbtSlice(a.?.*), dbtSlice(b.?.*));
}
fn hHashThunk(dbp: ?*c.DB, data: ?*const anyopaque, len: c.u_int32_t) callconv(.c) c.u_int32_t {
    const cb = callbacksOf(dbp);
    const slice = if (data == null or len == 0)
        &[_]u8{}
    else
        @as([*]const u8, @ptrCast(data))[0..len];
    return cb.h_hash.?(cb.ctx, slice);
}

// --- secondary extractor (Phase 3) ----------------------------------------

/// A secondary-index key extractor. Given the primary key+data, return the
/// secondary key to index (a slice borrowed from `data`/scratch), or `null` to
/// skip this record (maps to `DB_DONOTINDEX`).
pub const Extractor = struct {
    ctx: *anyopaque,
    extract: *const fn (ctx: *anyopaque, pkey: []const u8, pdata: []const u8) ?[]const u8,
};

fn assocThunk(
    secondary: ?*c.DB,
    pkey: ?*const c.DBT,
    pdata: ?*const c.DBT,
    result: ?*c.DBT,
) callconv(.c) c_int {
    const raw = c.libdb_db_get_api_internal(secondary.?).?;
    const ext: *const Extractor = @ptrCast(@alignCast(raw));
    const skey = ext.extract(ext.ctx, dbtSlice(pkey.?.*), dbtSlice(pdata.?.*));
    if (skey) |s| {
        var r = result.?;
        r.data = @constCast(@ptrCast(s.ptr));
        r.size = @intCast(s.len);
        return 0;
    }
    return consts.DB_DONOTINDEX;
}

// --- bulk iterator (Phase 3) ----------------------------------------------

/// Iterator over the records in a `DB_MULTIPLE_KEY` bulk get. Pure pointer
/// arithmetic over the returned buffer (sentinel `(u32)-1`); no per-record
/// allocation. `deinit` closes the underlying cursor.
pub const BulkIter = struct {
    cur: *c.DBC,
    buf: []align(1024) u8,
    txn: ?*Txn,
    data: c.DBT = std.mem.zeroes(c.DBT),
    pos: ?*anyopaque = null,
    started: bool = false,
    done: bool = false,

    pub const Pair = struct { key: []const u8, value: []const u8 };

    /// Yield the next (key, value) pair, or `null` at the end. Fetches another
    /// bulk page when the current buffer is drained.
    pub fn next(self: *BulkIter) Error!?BulkIter.Pair {
        while (true) {
            if (self.pos != null) {
                var kptr: ?*anyopaque = null;
                var klen: c.u_int32_t = 0;
                var dptr: ?*anyopaque = null;
                var dlen: c.u_int32_t = 0;
                self.multipleKeyNext(&kptr, &klen, &dptr, &dlen);
                if (self.pos != null) {
                    return .{
                        .key = sliceOf(kptr, klen),
                        .value = sliceOf(dptr, dlen),
                    };
                }
                // page drained; fall through to fetch the next page
            }
            if (self.done) return null;
            try self.fetchPage();
            if (self.done and self.pos == null) return null;
        }
    }

    fn fetchPage(self: *BulkIter) Error!void {
        var key: c.DBT = std.mem.zeroes(c.DBT);
        self.data = std.mem.zeroes(c.DBT);
        self.data.data = self.buf.ptr;
        self.data.ulen = @intCast(self.buf.len);
        self.data.flags = consts.DB_DBT_USERMEM;
        const op: u32 = (if (self.started) consts.DB_NEXT else consts.DB_FIRST) |
            consts.DB_MULTIPLE_KEY;
        self.started = true;
        const rc = c.libdb_dbc_get(self.cur, &key, &self.data, op);
        if (rc == consts.DB_NOTFOUND) {
            self.done = true;
            self.pos = null;
            return;
        }
        try check(rc);
        // DB_MULTIPLE_INIT: pos = (u8*)data->data + data->ulen - sizeof(u32)
        self.pos = @ptrFromInt(@intFromPtr(self.data.data) + self.data.ulen - @sizeOf(u32));
    }

    // Native reimplementation of DB_MULTIPLE_KEY_NEXT (db.in:1894). The buffer
    // holds, growing downward from the end, pairs of (data-offset,data-len)
    // Native reimplementation of DB_MULTIPLE_KEY_NEXT (db.h). From the
    // cursor `pos`, the buffer holds, growing downward as u32s: key-offset,
    // key-len, data-offset, data-len; `pos` then moves 4 u32s lower. A
    // key-offset of (u32)-1 ends the page. Offsets are from data->data.
    fn multipleKeyNext(
        self: *BulkIter,
        retkey: *?*anyopaque,
        retklen: *c.u_int32_t,
        retdata: *?*anyopaque,
        retdlen: *c.u_int32_t,
    ) void {
        const base = @intFromPtr(self.data.data);
        var pp = @intFromPtr(self.pos.?);
        const w0: *align(@alignOf(u32)) const u32 = @ptrFromInt(pp);
        if (w0.* == std.math.maxInt(u32)) {
            self.pos = null;
            retkey.* = null;
            retdata.* = null;
            return;
        }
        const koff = readWord(pp);
        pp -= @sizeOf(u32);
        const klen = readWord(pp);
        pp -= @sizeOf(u32);
        const doff = readWord(pp);
        pp -= @sizeOf(u32);
        const dlen = readWord(pp);
        pp -= @sizeOf(u32);
        retkey.* = @ptrFromInt(base + koff);
        retklen.* = klen;
        retdata.* = @ptrFromInt(base + doff);
        retdlen.* = dlen;
        self.pos = @ptrFromInt(pp);
    }

    pub fn deinit(self: *BulkIter) void {
        _ = c.libdb_dbc_close(self.cur);
        if (self.txn) |t| {
            if (safe and t.cursor_count > 0) t.cursor_count -= 1;
        }
    }
};

fn readWord(addr: usize) u32 {
    const w: *align(@alignOf(u32)) const u32 = @ptrFromInt(addr);
    return w.*;
}

fn sliceOf(ptr: ?*anyopaque, len: c.u_int32_t) []const u8 {
    if (ptr == null or len == 0) return &[_]u8{};
    return @as([*]const u8, @ptrCast(ptr))[0..len];
}

// ---------------------------------------------------------------------------
// Transactions (RFC 0016 §4, §8)
// ---------------------------------------------------------------------------

/// A transaction. Resolve with `commit` or `abort` exactly once; the handle is
/// dead afterwards. Not free-threaded -- serialize access. In safe builds
/// `commit` asserts no cursors opened on this txn remain open.
pub const Txn = struct {
    ptr: *c.DB_TXN,
    env: *Env,
    cursor_count: usize = 0,
    resolved: bool = false,

    inline fn alive(self: *const Txn) void {
        if (safe) std.debug.assert(!self.resolved);
    }

    /// Commit. In safe builds this asserts every cursor opened on the txn was
    /// closed first (the C library would otherwise close them for us, but the
    /// survey §7 rule is that the application closes them).
    pub fn commit(self: *Txn) Error!void {
        self.alive();
        if (safe) std.debug.assert(self.cursor_count == 0);
        self.resolved = true;
        try check(c.libdb_txn_commit(self.ptr, 0));
    }

    /// Abort. Cannot fail in a way the caller must handle; the handle is dead.
    pub fn abort(self: *Txn) void {
        if (safe and self.resolved) return;
        self.resolved = true;
        _ = c.libdb_txn_abort(self.ptr);
    }

    /// Discard a recovered-but-unresolved txn handle.
    pub fn discard(self: *Txn) Error!void {
        self.alive();
        self.resolved = true;
        try check(c.libdb_txn_discard(self.ptr, 0));
    }

    /// Prepare for a two-phase commit with the 128-bit global id `gid`.
    pub fn prepare(self: *Txn, gid: *const [128]u8) Error!void {
        self.alive();
        try check(c.libdb_txn_prepare(self.ptr, @constCast(gid)));
    }

    /// The txn's numeric id.
    pub fn id(self: *Txn) u32 {
        self.alive();
        return c.libdb_txn_id(self.ptr);
    }

    /// Name the txn (shows in diagnostics / `DB_ENV->txn_stat`).
    pub fn setName(self: *Txn, name: [*:0]const u8) Error!void {
        self.alive();
        try check(c.libdb_txn_set_name(self.ptr, name));
    }

    /// Set this txn's timeout in microseconds.
    pub fn setTimeout(self: *Txn, usec: u32) Error!void {
        self.alive();
        try check(c.libdb_txn_set_timeout(self.ptr, usec, consts.DB_SET_TXN_TIMEOUT));
    }
};

// ---------------------------------------------------------------------------
// Cursors (RFC 0016 §8)
// ---------------------------------------------------------------------------

/// A key/value pair copied out of a cursor op (owned by the caller's allocator).
pub const Pair = struct { key: []u8, value: []u8 };

/// A cursor. `defer cur.deinit()`. Not free-threaded. If opened on a txn it
/// must be closed before the txn resolves (safe builds assert this via a count
/// on the txn).
pub const Cursor = struct {
    ptr: *c.DBC,
    db: *Db,
    txn: ?*Txn,
    closed: bool = false,

    inline fn alive(self: *const Cursor) void {
        if (safe) std.debug.assert(!self.closed);
    }

    pub fn deinit(self: *Cursor) void {
        if (safe and self.closed) return;
        _ = c.libdb_dbc_close(self.ptr);
        self.closed = true;
        if (self.txn) |t| {
            if (safe and t.cursor_count > 0) t.cursor_count -= 1;
        }
    }

    /// Positioning get. `op` is a `consts.DB_*` cursor opcode (FIRST/NEXT/SET/
    /// SET_RANGE/...). For ops that take an input key (SET/SET_RANGE/GET_BOTH)
    /// pass it in `key`; otherwise pass `null`. Returns the copied-out pair, or
    /// `null` at EOF / not-found. The caller frees both slices.
    pub fn get(
        self: *Cursor,
        alloc: std.mem.Allocator,
        op: u32,
        key: ?[]const u8,
        data: ?[]const u8,
    ) Error!?Pair {
        self.alive();
        var k: c.DBT = if (key) |kk| dbtIn(kk) else std.mem.zeroes(c.DBT);
        var d: c.DBT = if (data) |dd| dbtIn(dd) else std.mem.zeroes(c.DBT);
        k.flags = consts.DB_DBT_MALLOC;
        d.flags = consts.DB_DBT_MALLOC;
        const rc = c.libdb_dbc_get(self.ptr, &k, &d, op);
        if (rc == consts.DB_NOTFOUND) return null;
        try check(rc);
        defer std.c.free(k.data);
        defer std.c.free(d.data);
        return .{
            .key = try alloc.dupe(u8, dbtSlice(k)),
            .value = try alloc.dupe(u8, dbtSlice(d)),
        };
    }

    /// Put at the cursor position (KEYFIRST/KEYLAST/AFTER/BEFORE/CURRENT).
    pub fn put(self: *Cursor, key: []const u8, data: []const u8, op: u32) Error!void {
        self.alive();
        var k = dbtIn(key);
        var d = dbtIn(data);
        try check(c.libdb_dbc_put(self.ptr, &k, &d, op));
    }

    /// Delete the record the cursor points at.
    pub fn del(self: *Cursor) Error!void {
        self.alive();
        try check(c.libdb_dbc_del(self.ptr, 0));
    }

    /// Count the duplicates for the current key.
    pub fn count(self: *Cursor) Error!u32 {
        self.alive();
        var n: c.db_recno_t = 0;
        try check(c.libdb_dbc_count(self.ptr, &n, 0));
        return @intCast(n);
    }

    /// Duplicate this cursor (same position unless `DB_POSITION` omitted).
    pub fn dup(self: *Cursor, position: bool) Error!Cursor {
        self.alive();
        var cp: ?*c.DBC = null;
        const flags: u32 = if (position) 0x200 else 0; // DB_POSITION
        try check(c.libdb_dbc_dup(self.ptr, &cp, flags));
        if (self.txn) |t| {
            if (safe) t.cursor_count += 1;
        }
        return .{ .ptr = cp.?, .db = self.db, .txn = self.txn };
    }

    /// Secondary-cursor pget: position by `op` (pass the secondary key in
    /// `skey` for SET-style ops, else `null`) and return the secondary key,
    /// primary key and primary data. `null` at EOF / not-found.
    pub fn pget(self: *Cursor, alloc: std.mem.Allocator, op: u32, skey: ?[]const u8) Error!?struct { skey: []u8, pkey: []u8, data: []u8 } {
        self.alive();
        var sk: c.DBT = if (skey) |s| dbtIn(s) else std.mem.zeroes(c.DBT);
        var pk: c.DBT = std.mem.zeroes(c.DBT);
        var d: c.DBT = std.mem.zeroes(c.DBT);
        // On an input (SET-style) op the key DBT is input-only -- DB leaves it
        // pointing at the caller's borrowed bytes, so it must NOT carry MALLOC
        // (freeing it would free borrowed memory). On a positional op the key
        // is returned, so MALLOC it and free it.
        const key_out = skey == null;
        if (key_out) sk.flags = consts.DB_DBT_MALLOC;
        pk.flags = consts.DB_DBT_MALLOC;
        d.flags = consts.DB_DBT_MALLOC;
        const rc = c.libdb_dbc_pget(self.ptr, &sk, &pk, &d, op);
        if (rc == consts.DB_NOTFOUND) return null;
        try check(rc);
        defer if (key_out) std.c.free(sk.data);
        defer std.c.free(pk.data);
        defer std.c.free(d.data);
        return .{
            .skey = try alloc.dupe(u8, dbtSlice(sk)),
            .pkey = try alloc.dupe(u8, dbtSlice(pk)),
            .data = try alloc.dupe(u8, dbtSlice(d)),
        };
    }

    /// A forward (`.forward`) or backward (`.backward`) iterator over the whole
    /// db. Each `next` copies the pair into `alloc`; the caller frees it.
    pub fn iterator(self: *Cursor, dir: Direction) Iterator {
        return .{ .cur = self, .dir = dir, .started = false };
    }

    pub const Direction = enum { forward, backward };

    pub const Iterator = struct {
        cur: *Cursor,
        dir: Direction,
        started: bool,

        pub fn next(self: *Iterator, alloc: std.mem.Allocator) Error!?Pair {
            const op: u32 = if (!self.started) switch (self.dir) {
                .forward => consts.DB_FIRST,
                .backward => consts.DB_LAST,
            } else switch (self.dir) {
                .forward => consts.DB_NEXT,
                .backward => consts.DB_PREV,
            };
            self.started = true;
            return self.cur.get(alloc, op, null, null);
        }
    };
};

// ---------------------------------------------------------------------------
// Sequences (RFC 0016 §8)
// ---------------------------------------------------------------------------

/// A 64-bit persistent sequence backed by a key in a Db. `defer seq.deinit()`.
pub const Sequence = struct {
    ptr: *c.DB_SEQUENCE,
    db: *Db,
    closed: bool = false,

    inline fn alive(self: *const Sequence) void {
        if (safe) std.debug.assert(!self.closed);
    }

    /// Config for a new sequence (set before `open`).
    pub const Config = struct {
        initial: ?i64 = null,
        cachesize: ?i32 = null,
        inc: bool = true,
        wrap: bool = false,
        range: ?struct { min: i64, max: i64 } = null,
    };

    /// Create and open a sequence stored under `key` in `db`. Pass `create` to
    /// make it if absent.
    pub fn open(db: *Db, txn: ?*Txn, key: []const u8, create: bool, config: Config) Error!Sequence {
        var p: ?*c.DB_SEQUENCE = null;
        try check(c.db_sequence_create(&p, db.ptr, 0));
        const sp = p.?;
        errdefer _ = c.libdb_seq_close(sp, 0);

        var flags: u32 = 0;
        if (config.inc) flags |= consts.DB_SEQ_INC else flags |= consts.DB_SEQ_DEC;
        if (config.wrap) flags |= consts.DB_SEQ_WRAP;
        if (flags != 0) try check(c.libdb_seq_set_flags(sp, flags));
        if (config.range) |r| try check(c.libdb_seq_set_range(sp, r.min, r.max));
        if (config.initial) |v| try check(c.libdb_seq_initial_value(sp, v));
        if (config.cachesize) |cs| try check(c.libdb_seq_set_cachesize(sp, cs));

        var k = dbtIn(key);
        const open_flags: u32 = if (create) consts.DB_CREATE else 0;
        try check(c.libdb_seq_open(sp, txnPtr(txn), &k, open_flags));
        return .{ .ptr = sp, .db = db };
    }

    /// Atomically advance by `delta` and return the first value of the block.
    pub fn next(self: *Sequence, txn: ?*Txn, delta: i32) Error!i64 {
        self.alive();
        var v: c.db_seq_t = 0;
        try check(c.libdb_seq_get(self.ptr, txnPtr(txn), delta, &v, 0));
        return @intCast(v);
    }

    pub fn deinit(self: *Sequence) void {
        if (safe and self.closed) return;
        _ = c.libdb_seq_close(self.ptr, 0);
        self.closed = true;
    }

    /// Remove the sequence's backing record.
    pub fn remove(self: *Sequence, txn: ?*Txn) Error!void {
        self.alive();
        self.closed = true;
        try check(c.libdb_seq_remove(self.ptr, txnPtr(txn), 0));
    }
};

test {
    // Pull the test block in src/test.zig into `zig build test` via the module.
    std.testing.refAllDecls(@This());
}
