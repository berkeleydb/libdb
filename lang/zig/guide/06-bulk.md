<!-- SPDX-License-Identifier: Sleepycat -->
# 6. Bulk retrieval

Fetching one record per call costs a round trip each time. A bulk get returns
many records in one buffer; the binding walks it as a zero-allocation iterator
(pure pointer arithmetic over the returned buffer, sentinel `(u32)-1`).

```zig
var buf: [65536]u8 align(1024) = undefined;   // >= page size, multiple of 1 KiB
var it = try db.bulk(null, &buf);
defer it.deinit();

while (try it.next()) |pair| {
    // pair.key / pair.value point INTO buf — valid until the next next()
    std.debug.print("{s} = {s}\n", .{ pair.key, pair.value });
}
```

The buffer must be at least the database page size, a multiple of 1024 bytes,
and 1024-aligned — the C library rejects anything else. The iterator refills the
buffer transparently when a page is drained, so one loop walks the whole
database. The yielded slices live inside `buf`; copy them out if you need them
past the next `next()`.
