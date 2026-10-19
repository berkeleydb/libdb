<!-- SPDX-License-Identifier: Sleepycat -->
# 5. Secondary indices

A secondary index maps a derived key back to a primary record. You supply an
*extractor* that computes the secondary key from the primary key+value; DB keeps
the index in sync as the primary changes.

```zig
var people = try env.openBtree("people.db");
defer people.deinit();
// a secondary that allows duplicates (many people share a city)
var by_city = try env.database().dup(false).open("by_city.db");
defer by_city.deinit();

// extractor: the city is the text after '|'
fn cityOf(ctx: *anyopaque, pkey: []const u8, pdata: []const u8) ?[]const u8 {
    _ = ctx; _ = pkey;
    const bar = std.mem.indexOfScalar(u8, pdata, '|') orelse return null; // skip
    return pdata[bar + 1 ..];
}

var ctx: u8 = 0;
const ext = libdb.Extractor{ .ctx = &ctx, .extract = cityOf };
try people.associate(null, &by_city, &ext);

try people.put(null, "1", "Ada|London");
```

Returning `null` from the extractor skips the record (`DB_DONOTINDEX`).

Look up by the secondary key and get the **primary** record back with `pget`:

```zig
if (try by_city.pget(a, null, "London")) |r| {
    defer a.free(r.pkey);
    defer a.free(r.data);
    std.debug.print("id={s} record={s}\n", .{ r.pkey, r.data });
}
```

To walk every match for a secondary key, use a secondary cursor and
`cur.pget(a, op, skey?)`: `DB_SET` with the key, then `DB_NEXT_DUP` with `null`.
