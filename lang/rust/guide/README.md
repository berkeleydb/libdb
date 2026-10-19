# libdb Rust guide

A chapter-by-chapter tour of the `libdb` Rust binding. Each chapter is short and
builds on the previous one. Every snippet here is exercised as a runnable
doc-test or an example under `../libdb/examples/`, so it cannot rot.

1. [Opening an environment](01-open-env.md)
2. [Put and get](02-put-get.md)
3. [Transactions and the retry helper](03-transactions.md)
4. [Cursors and iteration](04-cursors.md)
5. [Secondary indices](05-secondary.md)
6. [Bulk retrieval](06-bulk.md)

## Prerequisites

An installed libdb (`libdb-2026.0.so` + `db.h`). Point the build at it:

```sh
export LIBDB_DIR=/usr/local/BerkeleyDB.2026.0
export LD_LIBRARY_PATH=$LIBDB_DIR/lib
cargo build
```

## The shape in one paragraph

An [`Env`] is the unit of caching, locking, logging and transactions. You open
one or more [`Db`]s inside it. Reads and writes go through the `Db` (and, for
range scans, a `Cursor`); multi-step atomic work goes through a `Txn`, usually
via the `transact` retry helper. Handles close themselves when they drop, in the
order the C library requires (databases before the environment, cursors before
their transaction) — the Rust lifetimes make the wrong order a compile error.

[`Env`]: https://docs.rs/libdb/latest/libdb/struct.Env.html
[`Db`]: https://docs.rs/libdb/latest/libdb/struct.Db.html
