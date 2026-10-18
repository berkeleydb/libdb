# 2. Put and get

Open a database in the environment, then store and fetch byte-string key/value
pairs.

```rust
# let dir = std::env::temp_dir().join(format!("guide02_{}", std::process::id()));
# std::fs::create_dir_all(&dir).unwrap();
let env = libdb::Env::builder().open(&dir)?;
let db = env.database().btree().open("data.db")?;

db.put(b"key", b"value")?;
assert_eq!(db.get(b"key")?.as_deref(), Some(&b"value"[..]));
assert_eq!(db.get(b"absent")?, None); // not found is Ok(None), not an error
# Ok::<(), libdb::Error>(())
```

## Choosing a database type

`env.database()` returns a builder: `.btree()` (sorted, the default),
`.hash()`, `.recno()` or `.queue()`; plus `.pagesize(n)`,
`.duplicates()` / `.sorted_duplicates()`.

## The three output forms

Fetching a value has three shapes, from safest to fastest:

1. **Copy-out — `get`** returns an owned `Vec<u8>` (one allocation). The default.
2. **Into a caller buffer — `get_into`** writes into a `&mut [u8]` with no
   allocation. If the buffer is too small it returns `GetInto::TooSmall(need)`
   so you can grow and retry; `get_into_grow` wraps that up.
3. **Borrowed closure — `with_value`** calls your closure with a `&[u8]` view
   valid only for the call (zero heap allocation up to 256 bytes). The borrow
   cannot escape the closure — that is a compile error.

```rust
# let dir = std::env::temp_dir().join(format!("guide02b_{}", std::process::id()));
# std::fs::create_dir_all(&dir).unwrap();
# let env = libdb::Env::builder().open(&dir)?;
# let db = env.database().btree().open("data.db")?;
db.put(b"k", b"hello")?;

// zero-alloc read into a stack buffer
let mut buf = [0u8; 32];
if let libdb::GetInto::Fit(n) = db.get_into(b"k", &mut buf)? {
    assert_eq!(&buf[..n], b"hello");
}

// borrowed view — compute without copying out
let len = db.with_value(b"k", |v| v.len())?;
assert_eq!(len, Some(5));
# Ok::<(), libdb::Error>(())
```

## Delete, exists, overwrite

- `del(key)` returns `Ok(false)` if the key was absent.
- `exists(key)` tests presence without returning the value.
- `put_no_overwrite(key, val)` returns `Put::Exists` instead of overwriting.

Next: [Transactions and the retry helper](03-transactions.md).
