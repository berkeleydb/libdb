// SPDX-License-Identifier: Sleepycat
//! Zig test block for the libdb binding -- real round trips against the
//! installed library, exercising all four phases. Run in Debug/ReleaseSafe
//! (lifecycle asserts fire) and ReleaseFast (the real artifact) via
//! `zig build test --summary all`. std.testing.allocator is leak-checked, so a
//! missed `deinit`/`free` fails the test.
const std = @import("std");
const libdb = @import("libdb");
const consts = libdb.consts;

/// Make a fresh temp env home via libc (std.fs reorganises between Zig
/// versions; libc mkdir keeps this pinned to a stable ABI). Unique per test by
/// a monotonically-bumped counter plus the pid.
var home_counter: u32 = 0;
fn tempHome(a: std.mem.Allocator) ![:0]u8 {
    home_counter += 1;
    const z = try std.fmt.allocPrintSentinel(
        a,
        "/tmp/libdb_zig_{d}_{d}",
        .{ std.c.getpid(), home_counter },
        0,
    );
    _ = std.c.mkdir(z, 0o755);
    return z;
}

test "version is sane and header matches runtime" {
    const v = libdb.version();
    try std.testing.expectEqual(@as(i32, 2026), v.major);
    try std.testing.expectEqual(@as(i32, 0), v.minor);
    try libdb.checkVersion();
}

// -- Phase 1 ---------------------------------------------------------------

test "round trip: put/get/exists/del, copy-out form" {
    const a = std.testing.allocator;
    const home = try tempHome(a);
    defer a.free(home);

    var env = try libdb.Env.create();
    defer env.deinit();
    try env.setCachesize(0, 16 << 20);
    try env.open(home);

    var db = try env.openBtree("data.db");
    defer db.deinit();

    try db.put(null, "alpha", "one");
    try db.put(null, "beta", "two");

    {
        const v = (try db.get(a, null, "alpha")).?;
        defer a.free(v);
        try std.testing.expectEqualStrings("one", v);
    }
    try std.testing.expect((try db.get(a, null, "gamma")) == null);
    try std.testing.expect(try db.exists(null, "beta"));
    try std.testing.expect(!try db.exists(null, "gamma"));

    // overwrite, then putNew refuses
    try db.put(null, "alpha", "ONE!");
    {
        const v = (try db.get(a, null, "alpha")).?;
        defer a.free(v);
        try std.testing.expectEqualStrings("ONE!", v);
    }
    try std.testing.expect(!try db.putNew(null, "alpha", "x"));
    try std.testing.expect(try db.putNew(null, "fresh", "y"));

    // delete
    try std.testing.expect(try db.del(null, "beta"));
    try std.testing.expect(!try db.del(null, "beta"));
    try std.testing.expect(!try db.exists(null, "beta"));

    // empty value round-trips (the null/0 guard)
    try db.put(null, "empty", "");
    {
        const v = (try db.get(a, null, "empty")).?;
        defer a.free(v);
        try std.testing.expectEqual(@as(usize, 0), v.len);
    }
}

test "getInto zero-alloc with BufferSmall grow-retry" {
    const a = std.testing.allocator;
    const home = try tempHome(a);
    defer a.free(home);
    var env = try libdb.Env.create();
    defer env.deinit();
    try env.open(home);
    var db = try env.openBtree("data.db");
    defer db.deinit();

    try db.put(null, "k", "a-twelve-byte"); // 13 bytes

    var small: [4]u8 = undefined;
    var need: usize = 0;
    const r = db.getInto(null, "k", &small, &need);
    try std.testing.expectError(libdb.Error.BufferSmall, r);
    try std.testing.expectEqual(@as(usize, 13), need);

    const big = try a.alloc(u8, need);
    defer a.free(big);
    const n = (try db.getInto(null, "k", big, &need)).?;
    try std.testing.expectEqual(@as(usize, 13), n);
    try std.testing.expectEqualStrings("a-twelve-byte", big[0..n]);

    // absent key -> null
    try std.testing.expect((try db.getInto(null, "absent", big, &need)) == null);
}

const BorrowCtx = struct { seen: []const u8 = "", buf: [64]u8 = undefined, len: usize = 0 };
fn captureBorrow(ctx: *BorrowCtx, value: []const u8) void {
    @memcpy(ctx.buf[0..value.len], value);
    ctx.len = value.len;
}

test "borrowed get window form" {
    const a = std.testing.allocator;
    const home = try tempHome(a);
    defer a.free(home);
    var env = try libdb.Env.create();
    defer env.deinit();
    try env.open(home);
    var db = try env.openBtree("data.db");
    defer db.deinit();

    try db.put(null, "k", "borrowed-bytes");
    var ctx = BorrowCtx{};
    const found = try db.with(null, "k", *BorrowCtx, &ctx, captureBorrow);
    try std.testing.expect(found);
    try std.testing.expectEqualStrings("borrowed-bytes", ctx.buf[0..ctx.len]);
    try std.testing.expect(!try db.with(null, "absent", *BorrowCtx, &ctx, captureBorrow));
}

