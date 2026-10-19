// SPDX-License-Identifier: Sleepycat
//! A tiny key/value CLI over a Btree database.
//!
//!   example-kv <home>            -- demo: put a few keys, print them back
//!   example-kv <home> put K V    -- store V under K
//!   example-kv <home> get K      -- print the value under K
//!
//! With no sub-command it runs a self-contained demo (so `zig build
//! example-kv` shows output without arguments). Pinned to Zig 0.16: `main`
//! takes `std.process.Init`.
const std = @import("std");
const libdb = @import("libdb");

pub fn main(init: std.process.Init) !void {
    const a = init.arena.allocator();

    var args: std.ArrayList([]const u8) = .empty;
    var it = std.process.Args.Iterator.init(init.minimal.args);
    while (it.next()) |arg| try args.append(a, arg);

    const home: [:0]const u8 = if (args.items.len >= 2)
        try a.dupeZ(u8, args.items[1])
    else
        try std.fmt.allocPrintSentinel(a, "/tmp/libdb_zig_kv_{d}", .{std.c.getpid()}, 0);
    _ = std.c.mkdir(home, 0o755);

    var env = try libdb.Env.create();
    defer env.deinit();
    try env.open(home);
    var db = try env.openBtree("kv.db");
    defer db.deinit();

    const out = std.debug;
    if (args.items.len >= 5 and std.mem.eql(u8, args.items[2], "put")) {
        try db.put(null, args.items[3], args.items[4]);
        out.print("put {s} = {s}\n", .{ args.items[3], args.items[4] });
        return;
    }
    if (args.items.len >= 4 and std.mem.eql(u8, args.items[2], "get")) {
        if (try db.get(a, null, args.items[3])) |v| {
            out.print("{s}\n", .{v});
        } else {
            out.print("(not found)\n", .{});
        }
        return;
    }

    // demo
    const seed = [_][2][]const u8{
        .{ "apple", "red" },    .{ "banana", "yellow" },
        .{ "grape", "purple" },
    };
    for (seed) |kv| try db.put(null, kv[0], kv[1]);
    out.print("key/value demo:\n", .{});
    var cur = try db.cursor(null);
    defer cur.deinit();
    var cit = cur.iterator(.forward);
    while (try cit.next(a)) |pair| {
        out.print("  {s} -> {s}\n", .{ pair.key, pair.value });
    }
}
