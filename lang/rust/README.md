# libdb — Rust bindings

Idiomatic Rust bindings for **libdb** (the community Berkeley DB fork).

**Status: Phase 3** (callbacks, secondary indices, sequences, bulk — from
[RFC 0016](../../rfc/0016-rust-zig-bindings.md)).
What works today: the core data path and transactions/cursors (Phases 1–2) plus
comparators (`bt_compare`/`dup_compare`) and a `h_hash` function via the
callback trampoline (each thunk `catch_unwind`-wrapped so a panic cannot cross
FFI), a secondary index (`associate` + `pget`, with `None` from the extractor
skipping a record via `DB_DONOTINDEX`), 64-bit `Sequence`s, and zero-copy bulk
retrieval (`Cursor::bulk`) as a native `Iterator` over the `DB_MULTIPLE_KEY`
buffer with pure pointer arithmetic. Docs, guide and examples are Phase 4.

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
