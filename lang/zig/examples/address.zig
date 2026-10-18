// SPDX-License-Identifier: Sleepycat
//! A secondary-index address book: the primary db maps an id -> "name|city",
//! and a secondary index (duplicates allowed) maps city -> id. Looking a
//! person up by city uses the secondary cursor `pget`. `example-address
//! <home>`. Pinned to Zig 0.16.
const std = @import("std");
const libdb = @import("libdb");

// Secondary extractor: the city is the text after the '|'.
fn cityOf(ctx: *anyopaque, pkey: []const u8, pdata: []const u8) ?[]const u8 {
    _ = ctx;
    _ = pkey;
    const bar = std.mem.indexOfScalar(u8, pdata, '|') orelse return null;
    return pdata[bar + 1 ..];
}

pub fn main(init: std.process.Init) !void {
    const a = init.arena.allocator();

    var args: std.ArrayList([]const u8) = .empty;
    var it = std.process.Args.Iterator.init(init.minimal.args);
    while (it.next()) |arg| try args.append(a, arg);

    const home: [:0]const u8 = if (args.items.len >= 2)
        try a.dupeZ(u8, args.items[1])
    else
        try std.fmt.allocPrintSentinel(a, "/tmp/libdb_zig_addr_{d}", .{std.c.getpid()}, 0);
    _ = std.c.mkdir(home, 0o755);

    var env = try libdb.Env.create();
    defer env.deinit();
    try env.open(home);

    var people = try env.openBtree("people.db");
    defer people.deinit();
    var by_city = try env.database().dup(false).open("by_city.db");
    defer by_city.deinit();

    var dummy: u8 = 0;
    const ext = libdb.Extractor{ .ctx = &dummy, .extract = cityOf };
    try people.associate(null, &by_city, &ext);

    try people.put(null, "1", "Ada|London");
    try people.put(null, "2", "Grace|Boston");
    try people.put(null, "3", "Alan|London");

    std.debug.print("people indexed by city.\n", .{});

    // Who is in London? Walk the secondary duplicates for key "London".
    var cur = try by_city.cursor(null);
    defer cur.deinit();
    std.debug.print("in London:\n", .{});
    var first = true;
    while (true) {
        const op: u32 = if (first) libdb.consts.DB_SET else libdb.consts.DB_NEXT_DUP;
        const key: ?[]const u8 = if (first) "London" else null;
        first = false;
        const r = (try cur.pget(a, op, key)) orelse break;
        std.debug.print("  id={s} record={s}\n", .{ r.pkey, r.data });
    }
}
