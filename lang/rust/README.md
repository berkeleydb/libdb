# libdb — Rust bindings

Idiomatic Rust bindings for **libdb** (the community Berkeley DB fork).

**Status: Phase 1** (the core data path, from [RFC 0016](../../rfc/0016-rust-zig-bindings.md)).
What works today: an owning `Env` and `Db` with builders (type, pagesize,
duplicates), the error model (with the control-flow negatives demoted —
`DB_NOTFOUND` is `Ok(None)`, not an error), `put`/`get`/`del`/`exists`, the
three value-output forms (copy-out `get`, zero-alloc `get_into` with
`DB_BUFFER_SMALL` grow-retry, and the borrowed-closure `with_value`), a captured
error-callback diagnostic, and the compile-vs-runtime version check. The drift
gate re-verifies every baked constant against the installed `db.h`. Transactions,
cursors, callbacks, secondary indices and bulk ops land in later phases.

## Layout

```
libdb-sys/   Layer 1: raw FFI. Opaque handles, constants, the DBT carrier,
             and a tiny C method-shim (src/shim.c) so Rust never assumes a
             handle struct offset — the C compiler, reading the real db.h,
             resolves db->put etc. (the same seam the C++/Java bindings use).
libdb/       Layer 2: the safe, idiomatic API you actually use.
```

## Build & test

Requires an installed libdb (`libdb-2026.0.so` + `db.h`). Point at it with
`LIBDB_DIR`, else the well-known prefix `/usr/local/BerkeleyDB.2026.0` is used.

```sh
export LIBDB_DIR=/usr/local/BerkeleyDB.2026.0
export LD_LIBRARY_PATH=$LIBDB_DIR/lib
cargo test
```

Verified on FreeBSD 14 (clang 21, Rust 1.98) against libdb at the first LTS,
`v2026.10.7`.

## Example

```rust
let env = libdb::Env::builder().cachesize(0, 64 << 20).open("db-home")?;
let db  = env.database().btree().open("data.db")?;
db.put(b"key", b"value")?;
assert_eq!(db.get(b"key")?.as_deref(), Some(&b"value"[..]));
```
