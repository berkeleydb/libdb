# libdb — Rust bindings

Idiomatic Rust bindings for **libdb** (the community Berkeley DB fork).

**Status: v1 complete** (all four phases of [RFC 0016](../../rfc/0016-rust-zig-bindings.md)).
What the binding covers:

- **Core data path** — an owning `Env` and `Db` with builders (type, pagesize,
  duplicates, comparators), `put`/`get`/`del`/`exists`, and the three
  value-output forms: copy-out `get`, zero-alloc `get_into` with a
  `DB_BUFFER_SMALL` grow-retry, and the borrowed-closure `with_value`.
- **Transactions** — `Txn` (parent/snapshot/nowait, commit/abort consuming
  `self`, prepare/id/set_name/timeouts) and the `env.transact` deadlock-retry
  helper (with jittered backoff). Set a detector with `.lock_detect(..)` or use
  `transact_opts` with `nowait` for concurrent writers.
- **Cursors** — `Cursor<'txn, 'db>` that cannot outlive its transaction, all the
  get opcodes, put/del/count/dup/pget, and forward/backward iteration as a real
  `Iterator`.
- **Callbacks** — `bt_compare`/`dup_compare`/`h_hash` via the trampoline, each
  thunk `catch_unwind`-wrapped so a panic cannot cross FFI.
- **Secondary indices** — `associate` + `pget` (`None` skips a record via
  `DB_DONOTINDEX`).
- **Sequences** — 64-bit `Sequence` counters.
- **Bulk** — `Cursor::bulk`, a zero-copy `Iterator` over the `DB_MULTIPLE_KEY`
  buffer.

The error model demotes the control-flow negatives (`DB_NOTFOUND` is `Ok(None)`,
not an error). A **drift gate** re-verifies every baked constant against the
installed `db.h` (`cargo test -p libdb-sys`), and a `trybuild` ui test proves the
lifecycle rules (cursor-outlives-txn, use-after-commit, value-escapes-closure)
are *compile* errors.

## Documentation

- **API reference:** `cargo doc --open` (rustdoc on every public item, with
  runnable `/// ```` examples that `cargo test --doc` compiles and runs).
- **Getting-started guide:** [`guide/`](guide/README.md) — chapters on opening an
  env, put/get, transactions + retry, cursors, secondary indices and bulk.
- **C-to-Rust cross-walk:** [`MAPPING.md`](MAPPING.md).
- **Examples:** [`libdb/examples/`](libdb/examples/) — `kvstore` (a key/value CLI),
  `counter` (a transactional counter via the retry helper), `address_book`
  (a secondary-index lookup). Run with `cargo run --example <name>`.

## Thread safety

`Env` and `Db` are opened with `DB_THREAD` and are `Send + Sync`: a single
handle may be shared across threads (the `deadlock_retry_via_transact` test
shares one `Env`/`Db` across scoped threads). The diagnostic slot the error
callback writes is behind a `Mutex`, so even the cold `last_diagnostic()` path
is race-free under sharing.

**Transactions and cursors are single-thread.** The C library requires a
transaction — and every cursor opened under it — to be used by one thread. The
types encode this:

- `Txn` is `Send` but **not** `Sync`: you may *move* a transaction to another
  thread and use it serially, but not *share* it.
- `Cursor` is **not** `Send`: it borrows the `Txn` (`&Txn`), and `&Txn` is
  `!Send` because `Txn` is `!Sync`. The compiler therefore refuses to send a
  cursor — or two cursors sharing one transaction — to another thread, which is
  exactly the C rule (splitting a txn's cursors across threads causes a fatal
  region error). See `tests/ui/cursor_is_not_send.rs`.

### Allocator

The crate hands libdb memory (secondary-key buffers from `associate`) and
receives libdb-allocated memory (copy-out getters), allocating and freeing both
through libdb's own allocator (`libdb_umalloc`/`libdb_ufree` →
`__os_umalloc`/`__os_ufree`). This matches the process-global allocator hooks
(`db_env_set_func_malloc`/`free`) or libc. It must **never** configure a
per-env allocator (`DB_ENV->set_alloc`); the safe API does not expose one.

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
cargo test --doc
cargo run --example kvstore
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
