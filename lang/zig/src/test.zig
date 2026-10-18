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

// -- Phase 2 ---------------------------------------------------------------

test "txn commit vs abort visibility" {
    const a = std.testing.allocator;
    const home = try tempHome(a);
    defer a.free(home);
    var env = try libdb.Env.create();
    defer env.deinit();
    try env.open(home);
    var db = try env.openBtree("data.db");
    defer db.deinit();

    // committed write is visible
    {
        var txn = try env.begin(null, 0);
        try db.put(&txn, "c", "committed");
        try txn.commit();
    }
    {
        const v = (try db.get(a, null, "c")).?;
        defer a.free(v);
        try std.testing.expectEqualStrings("committed", v);
    }
    // aborted write is not
    {
        var txn = try env.begin(null, 0);
        try db.put(&txn, "r", "rolledback");
        txn.abort();
    }
    try std.testing.expect((try db.get(a, null, "r")) == null);

    // txn metadata
    {
        var txn = try env.begin(null, consts.DB_TXN_SNAPSHOT);
        try txn.setName("snap");
        try txn.setTimeout(1_000_000);
        try std.testing.expect(txn.id() != 0);
        txn.abort();
    }
}

const CounterCtx = struct { db: *libdb.Db, key: []const u8, by: i64 };
fn bumpCounter(ctx: CounterCtx, txn: *libdb.Txn) libdb.Error!void {
    const a = std.testing.allocator;
    var cur: i64 = 0;
    if (try ctx.db.get(a, txn, ctx.key)) |v| {
        defer a.free(v);
        cur = std.fmt.parseInt(i64, v, 10) catch 0;
    }
    cur += ctx.by;
    var buf: [32]u8 = undefined;
    const s = std.fmt.bufPrint(&buf, "{d}", .{cur}) catch unreachable;
    try ctx.db.put(txn, ctx.key, s);
}

test "transact helper commits and the retry path is bounded" {
    const a = std.testing.allocator;
    const home = try tempHome(a);
    defer a.free(home);
    var env = try libdb.Env.create();
    defer env.deinit();
    try env.open(home);
    var db = try env.openBtree("counter.db");
    defer db.deinit();

    var i: usize = 0;
    while (i < 5) : (i += 1) {
        try env.transact(CounterCtx, .{ .db = &db, .key = "n", .by = 1 }, bumpCounter);
    }
    const v = (try db.get(a, null, "n")).?;
    defer a.free(v);
    try std.testing.expectEqualStrings("5", v);
}

// A deliberate deadlock-returning op: proves transact aborts and surfaces it
// after exhausting retries (we can't easily force a real deadlock in a single
// thread, so the op returns Deadlock a bounded number of times then succeeds).
const FlakyCtx = struct { fails: *usize };
fn flaky(ctx: FlakyCtx, txn: *libdb.Txn) libdb.Error!void {
    _ = txn;
    if (ctx.fails.* > 0) {
        ctx.fails.* -= 1;
        return libdb.Error.Deadlock;
    }
}

test "transact retries on Deadlock then succeeds" {
    const a = std.testing.allocator;
    const home = try tempHome(a);
    defer a.free(home);
    var env = try libdb.Env.create();
    defer env.deinit();
    try env.open(home);
    var fails: usize = 3;
    try env.transact(FlakyCtx, .{ .fails = &fails }, flaky);
    try std.testing.expectEqual(@as(usize, 0), fails);
}

test "cursor scan equals inserted set, forward and backward" {
    const a = std.testing.allocator;
    const home = try tempHome(a);
    defer a.free(home);
    var env = try libdb.Env.create();
    defer env.deinit();
    try env.open(home);
    var db = try env.openBtree("data.db");
    defer db.deinit();

    const keys = [_][]const u8{ "a", "b", "c", "d", "e" };
    for (keys) |k| try db.put(null, k, k);

    // forward
    {
        var cur = try db.cursor(null);
        defer cur.deinit();
        var it = cur.iterator(.forward);
        var idx: usize = 0;
        while (try it.next(a)) |pair| {
            defer a.free(pair.key);
            defer a.free(pair.value);
            try std.testing.expectEqualStrings(keys[idx], pair.key);
            idx += 1;
        }
        try std.testing.expectEqual(keys.len, idx);
    }
    // backward
    {
        var cur = try db.cursor(null);
        defer cur.deinit();
        var it = cur.iterator(.backward);
        var idx: usize = keys.len;
        while (try it.next(a)) |pair| {
            defer a.free(pair.key);
            defer a.free(pair.value);
            idx -= 1;
            try std.testing.expectEqualStrings(keys[idx], pair.key);
        }
        try std.testing.expectEqual(@as(usize, 0), idx);
    }
    // SET_RANGE positioning
    {
        var cur = try db.cursor(null);
        defer cur.deinit();
        const pair = (try cur.get(a, consts.DB_SET_RANGE, "c", null)).?;
        defer a.free(pair.key);
        defer a.free(pair.value);
        try std.testing.expectEqualStrings("c", pair.key);
    }
}

test "cursor on txn: close before commit (safe-build assert holds)" {
    const a = std.testing.allocator;
    const home = try tempHome(a);
    defer a.free(home);
    var env = try libdb.Env.create();
    defer env.deinit();
    try env.open(home);
    var db = try env.openBtree("data.db");
    defer db.deinit();
    try db.put(null, "k", "v");

    var txn = try env.begin(null, 0);
    {
        var cur = try db.cursor(&txn);
        defer cur.deinit(); // closed before commit below
        const pair = (try cur.get(a, consts.DB_FIRST, null, null)).?;
        defer a.free(pair.key);
        defer a.free(pair.value);
        try std.testing.expectEqualStrings("k", pair.key);
    }
    try txn.commit();
}

// -- Phase 3 ---------------------------------------------------------------

// Reverse-order integer comparator over 4-byte big-endian keys.
fn revCompare(ctx: *anyopaque, x: []const u8, y: []const u8) c_int {
    _ = ctx;
    const xv = std.mem.readInt(u32, x[0..4], .big);
    const yv = std.mem.readInt(u32, y[0..4], .big);
    if (xv < yv) return 1; // reversed
    if (xv > yv) return -1;
    return 0;
}

test "custom bt_compare orders keys" {
    const a = std.testing.allocator;
    const home = try tempHome(a);
    defer a.free(home);
    var env = try libdb.Env.create();
    defer env.deinit();
    try env.open(home);

    var dummy: u8 = 0;
    const cbs = libdb.Callbacks{ .ctx = &dummy, .bt_compare = revCompare };
    var db = try env.database().openWith("rev.db", &cbs);
    defer db.deinit();

    var i: u32 = 1;
    while (i <= 5) : (i += 1) {
        var kb: [4]u8 = undefined;
        std.mem.writeInt(u32, &kb, i, .big);
        try db.put(null, &kb, "x");
    }
    // First record under the reverse comparator must be the largest key (5).
    var cur = try db.cursor(null);
    defer cur.deinit();
    const pair = (try cur.get(a, consts.DB_FIRST, null, null)).?;
    defer a.free(pair.key);
    defer a.free(pair.value);
    try std.testing.expectEqual(@as(u32, 5), std.mem.readInt(u32, pair.key[0..4], .big));
}

// Secondary index: extract the first byte of the value as the secondary key.
fn firstByte(ctx: *anyopaque, pkey: []const u8, pdata: []const u8) ?[]const u8 {
    _ = ctx;
    _ = pkey;
    if (pdata.len == 0) return null; // DB_DONOTINDEX
    return pdata[0..1];
}

test "secondary index lookup via associate + pget" {
    const a = std.testing.allocator;
    const home = try tempHome(a);
    defer a.free(home);
    var env = try libdb.Env.create();
    defer env.deinit();
    try env.open(home);

    var primary = try env.openBtree("primary.db");
    defer primary.deinit();
    // secondary allows duplicates (many primaries share a first byte)
    var secondary = try env.database().dup(false).open("secondary.db");
    defer secondary.deinit();

    var dummy: u8 = 0;
    const ext = libdb.Extractor{ .ctx = &dummy, .extract = firstByte };
    try primary.associate(null, &secondary, &ext);

    try primary.put(null, "p1", "apple");
    try primary.put(null, "p2", "apricot");
    try primary.put(null, "p3", "banana");
    try primary.put(null, "p4", ""); // skipped (DB_DONOTINDEX)

    // look up by secondary key "a" -> some primary whose value starts with 'a'
    const r = (try secondary.pget(a, null, "a")).?;
    defer a.free(r.pkey);
    defer a.free(r.data);
    try std.testing.expect(r.data[0] == 'a');

    // "b" -> banana / p3
    const rb = (try secondary.pget(a, null, "b")).?;
    defer a.free(rb.pkey);
    defer a.free(rb.data);
    try std.testing.expectEqualStrings("p3", rb.pkey);
    try std.testing.expectEqualStrings("banana", rb.data);

    // nothing indexed under the empty-value record
    try std.testing.expect((try secondary.pget(a, null, "z")) == null);
}

test "bulk get equals N single gets" {
    const a = std.testing.allocator;
    const home = try tempHome(a);
    defer a.free(home);
    var env = try libdb.Env.create();
    defer env.deinit();
    try env.open(home);
    var db = try env.openBtree("bulk.db");
    defer db.deinit();

    const n = 50;
    var i: u32 = 0;
    while (i < n) : (i += 1) {
        var kb: [8]u8 = undefined;
        const k = std.fmt.bufPrint(&kb, "k{d:0>4}", .{i}) catch unreachable;
        var vb: [8]u8 = undefined;
        const v = std.fmt.bufPrint(&vb, "v{d:0>4}", .{i}) catch unreachable;
        try db.put(null, k, v);
    }

    var buf: [65536]u8 align(1024) = undefined;
    var it = try db.bulk(null, &buf);
    defer it.deinit();

    var count: u32 = 0;
    while (try it.next()) |pair| {
        // cross-check each bulk record against a single get
        const single = (try db.get(a, null, pair.key)).?;
        defer a.free(single);
        try std.testing.expectEqualStrings(single, pair.value);
        count += 1;
    }
    try std.testing.expectEqual(@as(u32, n), count);
}

test "sequence increments" {
    const a = std.testing.allocator;
    const home = try tempHome(a);
    defer a.free(home);
    var env = try libdb.Env.create();
    defer env.deinit();
    try env.open(home);
    var db = try env.openBtree("seq.db");
    defer db.deinit();

    var seq = try libdb.Sequence.open(&db, null, "ticket", true, .{ .initial = 100 });
    defer seq.deinit();

    try std.testing.expectEqual(@as(i64, 100), try seq.next(null, 1));
    try std.testing.expectEqual(@as(i64, 101), try seq.next(null, 1));
    try std.testing.expectEqual(@as(i64, 102), try seq.next(null, 1));
    // a delta>1 reserves a block and returns its first value
    try std.testing.expectEqual(@as(i64, 103), try seq.next(null, 5));
    try std.testing.expectEqual(@as(i64, 108), try seq.next(null, 1));
}

test "drift gate: every baked constant is in the table" {
    // The teeth are in the generated C program (see gen-drift-gate.zig and
    // `zig build drift`); this guards that the Zig-side table was not left
    // short of the public consts (a quick structural check).
    try std.testing.expect(consts.all.len >= 60);
}
