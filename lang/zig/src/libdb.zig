// SPDX-License-Identifier: Sleepycat
//! Idiomatic Zig bindings for libdb (the community Berkeley DB fork).
//! Phase-0 proof: owning Env/Db, the error model, a put/get round trip.
//! Transactions, cursors, callbacks and bulk ops land in later phases
//! (rfc/0016). The design mirrors the Rust binding one-for-one.
const std = @import("std");
const c = @cImport({
    @cInclude("shim.h");
});

/// The DB-range control-flow negatives and the errors a Phase-0 surface can
/// raise. NotFound/KeyExist/BufferSmall are modelled in return types where they
/// are control flow, not errors; they appear here for the paths that treat them
/// as failures.
pub const Error = error{
    Deadlock,
    RunRecovery,
    NotFound,
    KeyExist,
    BufferSmall,
    /// Any other DB-range (<0) code; read env.lastCode() for the number.
    Db,
    /// An OS errno (>0); read env.lastCode() for the number.
    System,
    /// A usage error from the binding itself.
    Usage,
    /// Allocation failed while copying a value out of DB memory.
    OutOfMemory,
};

/// Decode a C return code. 0 => ok. The caller demotes NotFound etc. before
/// calling this where they are control flow.
fn check(code: c_int) Error!void {
    if (code == 0) return;
    switch (code) {
        c.DB_LOCK_DEADLOCK => return Error.Deadlock,
        c.DB_RUNRECOVERY => return Error.RunRecovery,
        c.DB_NOTFOUND => return Error.NotFound,
        c.DB_KEYEXIST => return Error.KeyExist,
        c.DB_BUFFER_SMALL => return Error.BufferSmall,
        else => return if (code < 0) Error.Db else Error.System,
    }
}

/// Library version at run time.
pub fn version() struct { major: i32, minor: i32, patch: i32 } {
    var a: c_int = 0;
    var b: c_int = 0;
    var p: c_int = 0;
    _ = c.db_version(&a, &b, &p);
    return .{ .major = @intCast(a), .minor = @intCast(b), .patch = @intCast(p) };
}

/// A database environment. Create, configure, open; `defer env.deinit()`.
pub const Env = struct {
    ptr: *c.DB_ENV,
    last_code: c_int = 0,

    /// Allocate an environment handle (configure it, then call `open`).
    pub fn create() Error!Env {
        var p: ?*c.DB_ENV = null;
        try check(c.db_env_create(&p, 0));
        return .{ .ptr = p.? };
    }
    /// Set the cache size (gigabytes + bytes). Call before `open`.
    pub fn setCachesize(self: *Env, gbytes: u32, bytes: u32) Error!void {
        try check(c.libdb_env_set_cachesize(self.ptr, gbytes, bytes, 1));
    }
    /// Open a transactional, free-threaded environment at `home` (NUL-terminated).
    pub fn open(self: *Env, home: [*:0]const u8) Error!void {
        const flags: u32 = c.DB_CREATE | c.DB_INIT_MPOOL | c.DB_INIT_LOCK |
            c.DB_INIT_LOG | c.DB_INIT_TXN | c.DB_THREAD;
        try check(c.libdb_env_open(self.ptr, home, flags, 0o644));
    }
    /// Close the environment. The handle is dead afterwards.
    pub fn deinit(self: *Env) void {
        _ = c.libdb_env_close(self.ptr, 0);
    }
    /// Open a Btree database in this environment.
    pub fn openBtree(self: *Env, file: [*:0]const u8) Error!Db {
        return Db.open(self, file, c.DB_BTREE);
    }
};

/// A database (key/value store). `defer db.deinit()`. Must not outlive its Env.
pub const Db = struct {
    ptr: *c.DB,

    fn open(env: *Env, file: [*:0]const u8, dbtype: c_int) Error!Db {
        var p: ?*c.DB = null;
        try check(c.db_create(&p, env.ptr, 0));
        const flags: u32 = c.DB_CREATE | c.DB_AUTO_COMMIT | c.DB_THREAD;
        check(c.libdb_db_open(p.?, null, file, null, dbtype, flags, 0o644)) catch |e| {
            _ = c.libdb_db_close(p.?, 0);
            return e;
        };
        return .{ .ptr = p.? };
    }
    pub fn deinit(self: *Db) void {
        _ = c.libdb_db_close(self.ptr, 0);
    }

    /// Store `data` under `key`, overwriting (Btree default). Borrowed input.
    pub fn put(self: *Db, key: []const u8, data: []const u8) Error!void {
        var k = dbtIn(key);
        var d = dbtIn(data);
        try check(c.libdb_db_put(self.ptr, null, &k, &d, 0));
    }

    /// Fetch the value under `key` into caller `alloc`-owned memory. Returns
    /// null if the key is absent (NotFound is not an error here). Caller frees.
    pub fn get(self: *Db, alloc: std.mem.Allocator, key: []const u8) Error!?[]u8 {
        var k = dbtIn(key);
        var d: c.DBT = std.mem.zeroes(c.DBT);
        d.flags = c.DB_DBT_MALLOC; // DB allocates; we copy then free.
        const rc = c.libdb_db_get(self.ptr, null, &k, &d, 0);
        if (rc == c.DB_NOTFOUND) return null;
        try check(rc);
        defer std.c.free(d.data);
        if (d.data == null or d.size == 0) return try alloc.alloc(u8, 0);
        const src = @as([*]const u8, @ptrCast(d.data))[0..d.size];
        return try alloc.dupe(u8, src);
    }
};

/// Build a borrowed-input DBT pointing at `b` for the call's duration. Guards
/// the size==0/data==null shape (a real UB source in the C library).
fn dbtIn(b: []const u8) c.DBT {
    var d: c.DBT = std.mem.zeroes(c.DBT);
    if (b.len != 0) {
        d.data = @constCast(@ptrCast(b.ptr));
        d.size = @intCast(b.len);
    }
    return d;
}
