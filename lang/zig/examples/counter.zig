// SPDX-License-Identifier: Sleepycat
//! A transactional counter that uses the deadlock-retry helper `env.transact`.
//! Each run increments a persisted counter N times inside transactions and
//! prints the final value. `example-counter <home> [n]`. Pinned to Zig 0.16.
const std = @import("std");
const libdb = @import("libdb");

const Ctx = struct { db: *libdb.Db, key: []const u8, alloc: std.mem.Allocator };

fn bump(ctx: Ctx, txn: *libdb.Txn) libdb.Error!void {
    var cur: i64 = 0;
    if (try ctx.db.get(ctx.alloc, txn, ctx.key)) |v| {
        defer ctx.alloc.free(v);
        cur = std.fmt.parseInt(i64, v, 10) catch 0;
    }
    cur += 1;
    var buf: [32]u8 = undefined;
    const s = std.fmt.bufPrint(&buf, "{d}", .{cur}) catch unreachable;
    try ctx.db.put(txn, ctx.key, s);
}

pub fn main(init: std.process.Init) !void {
    const a = init.arena.allocator();

    var args: std.ArrayList([]const u8) = .empty;
    var it = std.process.Args.Iterator.init(init.minimal.args);
    while (it.next()) |arg| try args.append(a, arg);

    const home: [:0]const u8 = if (args.items.len >= 2)
        try a.dupeZ(u8, args.items[1])
    else
        try std.fmt.allocPrintSentinel(a, "/tmp/libdb_zig_counter_{d}", .{std.c.getpid()}, 0);
    _ = std.c.mkdir(home, 0o755);

    const n: usize = if (args.items.len >= 3)
        std.fmt.parseInt(usize, args.items[2], 10) catch 10
    else
        10;

    var env = try libdb.Env.create();
    defer env.deinit();
    try env.open(home);
    var db = try env.openBtree("counter.db");
    defer db.deinit();

    var i: usize = 0;
    while (i < n) : (i += 1) {
        try env.transact(Ctx, .{ .db = &db, .key = "hits", .alloc = a }, bump);
    }

    const v = (try db.get(a, null, "hits")).?;
    std.debug.print("counter 'hits' after {d} transactions: {s}\n", .{ n, v });
}
