<!-- SPDX-License-Identifier: Sleepycat -->
# 2. A first put / get

Open a database in the environment and store a key/value pair. The default
`get` copies the value out into memory you own.

```zig
var db = try env.openBtree("data.db");
defer db.deinit();

try db.put(null, "hello", "world");          // null txn => auto-commit

const a = std.heap.page_allocator;
if (try db.get(a, null, "hello")) |v| {       // ?[]u8 — null means absent
    defer a.free(v);
    std.debug.print("{s}\n", .{v});            // world
}
```

The three output forms (RFC §5):

1. **Copy-out** — `db.get(alloc, txn, key) -> ?[]u8`. One allocation, always
   correct. The default.
2. **Into a caller buffer** — the zero-alloc hot path:

   ```zig
   var buf: [64]u8 = undefined;
   var need: usize = 0;
   if (db.getInto(null, "hello", &buf, &need)) |maybe| {
       if (maybe) |n| std.debug.print("{s}\n", .{buf[0..n]});
   } else |e| switch (e) {
       error.BufferSmall => {
           const big = try a.alloc(u8, need);  // grow to `need` and retry
           defer a.free(big);
           const n = (try db.getInto(null, "hello", big, &need)).?;
           _ = n;
       },
       else => return e,
   }
   ```

3. **Borrowed window** — `db.with(txn, key, Ctx, ctx, fn)` calls your function
   with a slice valid only inside the call. (On a `DB_THREAD` handle the library
   forbids the raw DB-owned pointer, so this form copies under the hood; use
   `getInto` for true zero-alloc.)

Other basics: `putNew` (insert only if absent, returns `bool`), `del` and
`exists` (both return `bool`). An empty value round-trips: the binding guards the
`size==0`/`data==NULL` shape that was real UB in the C library.
