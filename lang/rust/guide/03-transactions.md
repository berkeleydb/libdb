# 3. Transactions and the retry helper

A transaction groups several operations so they commit or roll back as a unit.
The ergonomic heart of the binding is `env.transact`, which runs a closure in a
transaction, commits on `Ok`, aborts on `Err`, and — crucially — **retries on
deadlock**.

```rust
# let dir = std::env::temp_dir().join(format!("guide03_{}", std::process::id()));
# std::fs::create_dir_all(&dir).unwrap();
# let env = libdb::Env::builder().open(&dir)?;
# let db = env.database().btree().open("data.db")?;
env.transact(10, |txn| {
    db.tput(txn, b"a", b"1")?;
    db.tput(txn, b"b", b"2")?;
    Ok(()) // commit
})?;
assert_eq!(db.get(b"a")?.as_deref(), Some(&b"1"[..]));
# Ok::<(), libdb::Error>(())
```

If the closure returns `Err`, the transaction is aborted and the error is
returned — nothing is stored:

```rust
# let dir = std::env::temp_dir().join(format!("guide03b_{}", std::process::id()));
# std::fs::create_dir_all(&dir).unwrap();
# let env = libdb::Env::builder().open(&dir)?;
# let db = env.database().btree().open("data.db")?;
let r: Result<(), libdb::Error> = env.transact(10, |txn| {
    db.tput(txn, b"c", b"3")?;
    Err(libdb::Error::Usage("changed my mind"))
});
assert!(r.is_err());
assert_eq!(db.get(b"c")?, None); // rolled back
# Ok::<(), libdb::Error>(())
```

## Deadlocks

When two transactions each hold a lock the other needs, that is a deadlock. The
first argument to `transact` is the retry budget: on `Error::Deadlock` the
transaction aborts and the closure runs again (with a short jittered backoff so
symmetric contenders make progress), up to that many times.

For this to work the environment must either run the **deadlock detector**
(`.lock_detect(LockDetect::Default)`) or begin transactions with **`nowait`**
(`env.transact_opts(retries, TxnOptions { nowait: true, ..Default::default() },
..)`), which fails a blocking lock immediately instead of waiting. Without one of
these, libdb never reports the deadlock and the threads wait forever.

See `examples/counter.rs` for the retry helper in action, and the integration
test `tests/txncursor.rs::deadlock_retry_via_transact` for two threads driven
into a real cyclic deadlock that `transact` recovers from.

## Manual transactions

For finer control, `env.begin()` returns a `Txn`. `commit()` and `abort()`
consume it (so the handle is dead afterward — the C rule, enforced by the type
system). `begin_opts` takes a parent transaction and `TxnOptions`
(`snapshot`, `nowait`, `nosync`).

Next: [Cursors and iteration](04-cursors.md).
