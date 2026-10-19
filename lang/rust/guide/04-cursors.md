# 4. Cursors and iteration

A cursor walks the records of a database in key order. The idiomatic way to scan
is `iter`, which gives you a real Rust `Iterator`.

```rust
# let dir = std::env::temp_dir().join(format!("guide04_{}", std::process::id()));
# std::fs::create_dir_all(&dir).unwrap();
# let env = libdb::Env::builder().open(&dir)?;
# let db = env.database().btree().open("data.db")?;
db.put(b"a", b"1")?;
db.put(b"b", b"2")?;
db.put(b"c", b"3")?;

// forward scan, in sorted key order
let keys: Vec<Vec<u8>> = db
    .cursor()?
    .iter(libdb::Direction::Forward)
    .map(|r| r.unwrap().0)
    .collect();
assert_eq!(keys, vec![b"a".to_vec(), b"b".to_vec(), b"c".to_vec()]);

// backward is just the other Direction
let last = db.cursor()?.iter(libdb::Direction::Backward).next().unwrap()?;
assert_eq!(last.0, b"c");
# Ok::<(), libdb::Error>(())
```

Each item is a `Result<(Vec<u8>, Vec<u8>)>`; iteration ends at the end of the
database.

## Positioning

Besides `iter`, a cursor has the raw positioning methods: `first`, `last`,
`next`, `prev`, `current`, `next_dup`, `next_nodup`, and the seek pair `set`
(exact) and `set_range` (smallest key ≥ the one given). Each returns
`Ok(Some((key, value)))` or `Ok(None)` at the end.

## Writing through a cursor

`put` stores at the cursor, `put_dup` adds a duplicate data item, `del` removes
the record at the current position, and `count` returns how many duplicates the
current key has. In a transactional environment, open the cursor in a
transaction with `db.cursor_txn(txn)` — the cursor borrows the transaction, so
the compiler forbids it from outliving it (the C rule that a cursor must close
before its transaction resolves).

```rust
# let dir = std::env::temp_dir().join(format!("guide04b_{}", std::process::id()));
# std::fs::create_dir_all(&dir).unwrap();
# let env = libdb::Env::builder().open(&dir)?;
# let db = env.database().sorted_duplicates().open("dups.db")?;
env.transact(5, |txn| {
    let c = db.cursor_txn(txn)?;
    c.put(b"k", b"a")?;
    c.put_dup(b"k", b"b")?;
    c.set(b"k")?;
    assert_eq!(c.count()?, 2);
    Ok(())
})?;
# Ok::<(), libdb::Error>(())
```

Next: [Secondary indices](05-secondary.md).
