<!-- SPDX-License-Identifier: Sleepycat -->
# 3. Transactions and the retry helper

Pass a `*Txn` instead of `null` to make several operations atomic. Resolve the
transaction exactly once with `commit` or `abort`.

```zig
var txn = try env.begin(null, 0);
db.put(&txn, "a", "1") catch |e| { txn.abort(); return e; };
db.put(&txn, "b", "2") catch |e| { txn.abort(); return e; };
try txn.commit();   // both visible, or neither
```

Begin flags OR-combine: `consts.DB_TXN_SNAPSHOT` (MVCC snapshot isolation),
`DB_TXN_NOWAIT` (fail instead of blocking on a lock), `DB_READ_COMMITTED`,
`DB_READ_UNCOMMITTED`. Metadata: `txn.setName`, `txn.setTimeout`, `txn.id`.

## Deadlocks are normal — retry them

Under concurrency a transaction can be chosen as the deadlock victim and must
abort and retry. That loop is so central that the binding ships it as
`env.transact`:

```zig
const Ctx = struct { db: *libdb.Db, delta: i64 };
fn bump(ctx: Ctx, txn: *libdb.Txn) libdb.Error!void {
    // ... read-modify-write inside the txn; may raise error.Deadlock ...
    _ = ctx; _ = txn;
}

try env.transact(Ctx, .{ .db = &db, .delta = 1 }, bump);
```

`transact` begins a transaction, runs your function, commits on success, and on
`error.Deadlock` aborts and retries (bounded at 10 attempts). Your function must
**not** commit or abort the txn itself.
