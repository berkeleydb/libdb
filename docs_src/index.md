---
title: "Berkeley DB Documentation"
---
# Berkeley DB Documentation

The complete reference for **libdb** (Berkeley DB) — the embedded,
transactional key/value storage engine. Generated from Markdown source by
`docs_src/build.py`; available as HTML (here), [PDF](#pdf), and
[man pages](#man). Version and copyright are single-sourced from
`docs_src/_data/site.toml` + `dist/RELEASE`.

## API reference

- [C API Reference](api/c/index.html) — the primary interface: `DB_ENV`, `DB`,
  `DBC` (cursor), `DBT`, transactions, locking, logging, replication, and the
  command-line utilities.
- [C++ / STL API Reference](api/stl/index.html) — the `dbstl` standard-template-
  library containers and iterators backed by Berkeley DB.
- [Java API Reference](java-api/index.html) — the `com.sleepycat.*` Java
  binding (`db`, `bind`, `collections`, `persist`). This tree is
  **javadoc-generated** from `lang/java/src/` at build time (not Markdown
  source), so it always matches the current code; it is not part of the
  Markdown no-loss gate.

## Guides

- [Programmer's Reference](guides/programmer_reference/index.html) — the
  conceptual manual: access methods, the environment, memory pool, locking,
  logging, transactions, replication, XA, and tuning.
- [Getting Started with Data Storage](guides/gsg/index.html) — databases,
  cursors, secondary indexes, the four access methods.
  ([C](guides/gsg/index.html) · [C++](guides/gsg/cxx/index.html) ·
  [Java](guides/gsg/java/index.html))
- [Getting Started with Transactions](guides/gsg_txn/index.html) — ACID,
  isolation, deadlocks, recovery, checkpoints.
  ([C](guides/gsg_txn/index.html) · [C++](guides/gsg_txn/cxx/index.html) ·
  [Java](guides/gsg_txn/java/index.html))
- [Getting Started with Replication](guides/gsg_db_rep/index.html) — the
  replication framework and Replication Manager.
  ([C](guides/gsg_db_rep/index.html) · [C++](guides/gsg_db_rep/cxx/index.html) ·
  [Java](guides/gsg_db_rep/java/index.html))
- [Collections (Bindings) Tutorial](guides/collections/index.html) — the
  Java-style collections/bindings API.
- [Berkeley DB SQL](guides/bdb-sql/index.html) — the SQLite-compatible SQL
  interface.
- [Installation & Build](guides/installation/index.html) — building on Unix,
  Windows, Android; configuration flags; the test suite.
- [Upgrading](guides/upgrading/index.html) — release-to-release upgrade notes
  (2.0 through 4.7 and the 11gR2 line).
- [Porting](guides/porting/index.html) — porting Berkeley DB to a new platform.
- Articles: [In-Memory Databases](guides/articles/inmemory/index.html) ·
  [Message Text](guides/articles/mssgtxt/index.html).

## <a id="man"></a>Man pages

Every public API has a section-3 man page, plus a library overview
(**`libdb(3)`**). They are built to `docs-build/man/man3/` (787 pages) and
shipped in the release man-page tarball. Install and use like any system man
page, e.g. `man libdb`, `man db_get`.

## <a id="pdf"></a>PDF downloads

Each book is also available as a PDF (see the release assets / `docs-build/pdf/`):
`api_c.pdf`, `api_stl.pdf`, `guides_programmer_reference.pdf`,
`guides_gsg.pdf`, `guides_gsg_txn.pdf`, `guides_gsg_db_rep.pdf`,
`guides_gsg_cxx.pdf`, `guides_gsg_java.pdf`,
`guides_gsg_txn_cxx.pdf`, `guides_gsg_txn_java.pdf`,
`guides_gsg_db_rep_cxx.pdf`, `guides_gsg_db_rep_java.pdf`,
`guides_collections.pdf`, `guides_bdb-sql.pdf`, `guides_installation.pdf`,
`guides_upgrading.pdf`, `guides_porting.pdf`,
`guides_articles_inmemory.pdf`, `guides_articles_mssgtxt.pdf`.

---

*The C# language-binding manual (Sandcastle, not DocBook) is archived
separately and is not part of this Markdown-sourced tree. The Java API
reference above is regenerated from source by javadoc rather than migrated
from DocBook.*
