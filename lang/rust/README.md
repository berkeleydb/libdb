# libdb — Rust bindings

Idiomatic Rust bindings for **libdb** (the community Berkeley DB fork).

**Status: Phase 2** (transactions + cursors, from [RFC 0016](../../rfc/0016-rust-zig-bindings.md)).
What works today: the core data path (Phase 1) plus transactions (`Txn` with
parent/snapshot/nowait, commit/abort consuming `self`, the `env.transact`
deadlock-retry helper), cursors (`Cursor<'txn, 'db>` that cannot outlive its
transaction, all the get opcodes, put/del/count/dup, and forward/backward
iteration as a real `Iterator`). The lifecycle rules are proven by a `trybuild`
ui test: a cursor outliving its txn, a handle used after `commit`, and a value
escaping `with_value` are each a *compile* error. Callbacks, secondary indices,
sequences and bulk ops land in Phase 3.

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
