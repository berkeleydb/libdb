---
title: "Berkeley DB Documentation"
---
# Berkeley DB Documentation

The complete reference for **libdb** (Berkeley DB) — the embedded,
transactional key/value storage engine. Generated from Markdown source by
`docs_src/build.py`; available as HTML (here), [PDF](#pdf), and
[man pages](#man). Version and copyright are single-sourced from
`docs_src/_data/site.toml` + `dist/RELEASE`.

> This reference tracks the **community fork** at
> [github.com/berkeleydb/libdb](https://github.com/berkeleydb/libdb), which
> ships under [CalVer](https://calver.org/) (`vYYYY.0M`). See the
> [**releases**](https://github.com/berkeleydb/libdb/releases) for the full
> list and per-release notes; the version this documentation was built for is
> shown in the page header. Fork releases are a continuation of, and distinct
> from, the Sleepycat/Oracle `5.3.x` line (the last Sleepycat-licensed release
> was 5.3.28).

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

Each book is also available as a PDF:

| Book | PDF |
|------|-----|
| C API reference | [`api_c.pdf`](pdf/api_c.pdf) |
| C++/STL API reference | [`api_stl.pdf`](pdf/api_stl.pdf) |
| Programmer's Reference Guide | [`guides_programmer_reference.pdf`](pdf/guides_programmer_reference.pdf) |
| Getting Started with Data Storage | [`guides_gsg.pdf`](pdf/guides_gsg.pdf) |
| Getting Started with Transactions | [`guides_gsg_txn.pdf`](pdf/guides_gsg_txn.pdf) |
| Getting Started with Replication | [`guides_gsg_db_rep.pdf`](pdf/guides_gsg_db_rep.pdf) |
| Getting Started with Data Storage (C++) | [`guides_gsg_cxx.pdf`](pdf/guides_gsg_cxx.pdf) |
| Getting Started with Data Storage (Java) | [`guides_gsg_java.pdf`](pdf/guides_gsg_java.pdf) |
| Getting Started with Transactions (C++) | [`guides_gsg_txn_cxx.pdf`](pdf/guides_gsg_txn_cxx.pdf) |
| Getting Started with Transactions (Java) | [`guides_gsg_txn_java.pdf`](pdf/guides_gsg_txn_java.pdf) |
| Getting Started with Replication (C++) | [`guides_gsg_db_rep_cxx.pdf`](pdf/guides_gsg_db_rep_cxx.pdf) |
| Getting Started with Replication (Java) | [`guides_gsg_db_rep_java.pdf`](pdf/guides_gsg_db_rep_java.pdf) |
| Collections tutorial (Java) | [`guides_collections.pdf`](pdf/guides_collections.pdf) |
| Berkeley DB SQL interface | [`guides_bdb-sql.pdf`](pdf/guides_bdb-sql.pdf) |
| Installation | [`guides_installation.pdf`](pdf/guides_installation.pdf) |
| Upgrading | [`guides_upgrading.pdf`](pdf/guides_upgrading.pdf) |
| Porting | [`guides_porting.pdf`](pdf/guides_porting.pdf) |
| Article: In-Memory databases | [`guides_articles_inmemory.pdf`](pdf/guides_articles_inmemory.pdf) |
| Article: Message-text handling | [`guides_articles_mssgtxt.pdf`](pdf/guides_articles_mssgtxt.pdf) |

The same PDFs are attached to each [GitHub release](https://github.com/berkeleydb/libdb/releases).

---

*The C# language-binding manual (Sandcastle, not DocBook) is archived
separately and is not part of this Markdown-sourced tree. The Java API
reference above is regenerated from source by javadoc rather than migrated
from DocBook.*
