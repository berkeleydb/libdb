<!-- SPDX-License-Identifier: Sleepycat -->
# 4. Cursors and iteration

A cursor walks the database in key order. Open it, iterate, close it.

```zig
var cur = try db.cursor(null);
defer cur.deinit();

var it = cur.iterator(.forward);           // or .backward
while (try it.next(a)) |pair| {
    defer a.free(pair.key);
    defer a.free(pair.value);
    std.debug.print("{s} = {s}\n", .{ pair.key, pair.value });
}
```

For positioning use `cur.get(alloc, op, key?, data?)` with a `consts.DB_*`
opcode:

```zig
// jump to the first key >= "m"
if (try cur.get(a, consts.DB_SET_RANGE, "m", null)) |pair| {
    defer a.free(pair.key);
    defer a.free(pair.value);
}
```

Common ops: `DB_FIRST`, `DB_LAST`, `DB_NEXT`, `DB_PREV`, `DB_SET` (exact),
`DB_SET_RANGE` (≥), `DB_NEXT_DUP`, `DB_GET_BOTH`. Cursor writes:
`cur.put(key, data, consts.DB_KEYLAST)`, `cur.del()`, `cur.count()` (duplicates
for the current key), `cur.dup(position)`.

**Lifecycle:** a cursor opened on a transaction must be closed before the
transaction resolves. In safe builds `txn.commit()` asserts no cursor opened on
it remains open — so always `defer cur.deinit()` inside the txn's scope.
