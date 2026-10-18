# 6. Bulk retrieval

Fetching one record per call has a round-trip cost. For a scan over many
records, `Cursor::bulk` fetches a whole buffer-full in a single call and hands
you a **zero-copy iterator** over it: each `(key, value)` is a slice pointing
into the buffer, with no per-record allocation.

```rust
# let dir = std::env::temp_dir().join(format!("guide06_{}", std::process::id()));
# std::fs::create_dir_all(&dir).unwrap();
# let env = libdb::Env::builder().open(&dir)?;
# let db = env.database().btree().open("data.db")?;
for i in 0..1000 {
    db.put(format!("k{i:04}").as_bytes(), format!("v{i}").as_bytes())?;
}

let c = db.cursor()?;
let mut buf = Vec::new(); // reused across bulk calls
let mut count = 0;
for (key, value) in c.bulk(&mut buf)? {
    // `key` and `value` borrow `buf` — consume them before the next bulk call
    assert!(key.starts_with(b"k"));
    assert!(value.starts_with(b"v"));
    count += 1;
}
assert_eq!(count, 1000);
# Ok::<(), libdb::Error>(())
```

## How it works

Under the hood this is one `DB_MULTIPLE_KEY` cursor get into your buffer. The
library packs the records into the buffer and writes an index of
`(key_offset, key_len, data_offset, data_len)` quadruples growing down from the
top, ending in a sentinel. `BulkPairs` walks that index with pure pointer
arithmetic — no copies, no allocations beyond the one buffer you pass in (which
you can reuse across calls).

The equivalence to the slow path is checked with teeth in
`tests/callbacks.rs::bulk_get_equals_n_single_gets`: a bulk scan over 500
records yields exactly the same pairs, in the same order, as 500 single gets.

## When to use it

Reach for `bulk` on large forward scans where you touch every record (export,
re-indexing, aggregation). For small or sparse lookups the per-record `get` and
cursor `iter` are simpler and just as fast.

That's the tour. The full API reference is the rustdoc (`cargo doc --open`), and
`MAPPING.md` cross-walks every C method to its Rust form.
