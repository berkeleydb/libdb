// SPDX-License-Identifier: Sleepycat
// Zig test block: a real put/get round trip against the installed library.
const std = @import("std");
const libdb = @import("libdb");

test "version is sane" {
    const v = libdb.version();
    try std.testing.expectEqual(@as(i32, 2026), v.major);
    try std.testing.expectEqual(@as(i32, 0), v.minor);
}

test "round trip" {
    const a = std.testing.allocator;
    // Create a unique temp home via libc (std.fs reorganises between Zig
    // versions; libc mkdir keeps this test pinned to a stable ABI).
    const home_z = try std.fmt.allocPrintSentinel(a, "/tmp/libdb_zig_{d}", .{std.c.getpid()}, 0);
    defer a.free(home_z);
    _ = std.c.mkdir(home_z, 0o755);

    var env = try libdb.Env.create();
    defer env.deinit();
    try env.setCachesize(0, 16 << 20);
    try env.open(home_z);

    var db = try env.openBtree("data.db");
    defer db.deinit();

    try db.put("alpha", "one");
    try db.put("beta", "two");

    {
        const v = (try db.get(a, "alpha")).?;
        defer a.free(v);
        try std.testing.expectEqualStrings("one", v);
    }
    // absent key -> null, not an error
    try std.testing.expect((try db.get(a, "gamma")) == null);
    // overwrite
    try db.put("alpha", "ONE!");
    {
        const v = (try db.get(a, "alpha")).?;
        defer a.free(v);
        try std.testing.expectEqualStrings("ONE!", v);
    }
    // empty value round-trips (the null/0 guard)
    try db.put("empty", "");
    {
        const v = (try db.get(a, "empty")).?;
        defer a.free(v);
        try std.testing.expectEqual(@as(usize, 0), v.len);
    }
}
