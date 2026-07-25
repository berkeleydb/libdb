# Berkeley DB (`libdb`)

> **Not affiliated with or endorsed by Oracle Corporation.** "Berkeley DB" is
> used here to name the software this project archives and forks. Releases
> `v5.3.29` and later are community fork releases and are **not** Oracle
> artifacts (Oracle's final 5.3 release was 5.3.28).

This repository is two things at once:

1. **A historical archive** of Berkeley DB — the complete published lineage of
   releases and official patches that could be recovered, preserved as tags and
   GitHub Releases for reference and `git diff`-able history.
2. **A living fork** under active development on the **`master`** branch, where
   new work (bug fixes, portability, performance, and new features) happens.

Berkeley DB is an embedded, transactional key/value storage engine (B-tree,
Hash, Queue, Recno) with ACID transactions, fine-grained locking, write-ahead
logging, MVCC snapshot isolation, and replication. It is a library you link into
your application — no server process.

## Branches and tags

| Ref | What it is |
|-----|------------|
| `master` | The living fork. Active development; the place to base PRs. |
| `historical` | A linear, chronological reconstruction of Berkeley DB **1.85 (1992) → 5.1.29 (2011)**. Archival; never rebased. |
| `vX.Y.Z` tags | Each upstream **release**, imported verbatim (with its upstream `.tar.gz` attached to the GitHub Release). |
| `vX.Y.Z.N` tags | Each official upstream **patch**, applied as its own commit. |
| `vX.Y.Z-NC` tags | "No-Crypto" export variants, branched from their base release. |
| `v4.6.21-SSI` | Michael Cahill's SIGMOD-2008 **Serializable Snapshot Isolation** research prototype, reproduced verbatim on 4.6.21. |
| `v5.3.21/28`, `v5.3.29` | The maintained 5.3.x line that `master` builds on. |

### Browsing the archive

```sh
git tag -l | sort -V                 # every archived version
git log --oneline --reverse historical
git diff v4.5.20 v4.6.21             # what changed between releases
git diff v4.6.21 v4.6.21.2           # what a single official patch changed
git diff v4.2.52 v4.2.52-NC          # what the no-crypto variant strips
```

See the [`historical` branch README](https://github.com/berkeleydb/libdb/tree/historical)
for full provenance and the per-version index.

## What's new on the living fork

- **Serializable Snapshot Isolation (SSI)** — an opt-in `DB_TXN_SNAPSHOT_SAFE`
  transaction mode that detects dangerous read/write dependency structures and
  aborts the pivot with `DB_SNAPSHOT_CONFLICT`. Both of Cahill's rw-conflict
  detection mechanisms are implemented: the lock-table path (a concurrent
  writer meeting a reader's SIREAD marker) and the MVCC version-chain path in
  `mp_fget` (a reader handed an older version than one a concurrent writer
  committed). SIREAD markers are reclaimed incrementally and bounded (not only
  at checkpoint); the commit-time pivot check is race-free against concurrent
  edge recording; and `DB_TXN_SNAPSHOT_SAFE` is rejected with `prepare()`/2PC.
  The SIREAD marker/locker/detail lifetime is hardened for concurrent writers:
  a family of pre-existing use-after-free bugs (most importantly a lock object
  reclaimed while it still held SIREAD markers) was found with TSan/ASan and
  fixed, and a multi-process concurrent-writer stress test (`ssi009`) guards
  against regression. **Still experimental** in that page-granularity conflict
  tracking can raise abort rates under contention (measured by
  `lab/bench/ssi_abort_bench`), and the HA/replication qualification is still
  being built. This is the *first* of the planned features in
  [`ROADMAP.md`](ROADMAP.md), which targets matching or beating InnoDB and
  WiredTiger on multicore/NUMA scalability and performance.

## Building

```sh
cd build_unix
../dist/configure            # see ../dist/configure --help for options
make -j

# Common variants:
#   --enable-debug --enable-diagnostic   developer build with assertions
#   --enable-cxx                          C++ API
#   --enable-sql                          SQL (SQLite-compatible) API
#   --enable-test --with-tcl=<dir>        build the TCL test harness
```

To view the original distribution and API documentation, open
`docs/index.html` in a browser.

## Testing

```sh
cd build_unix
../dist/configure --enable-debug --enable-test --with-tcl=/usr/lib/tcl8.6 && make -j
tclsh
  % source ../test/tcl/test.tcl
  % ssi001        ;# SSI write-skew test
  % run_std       ;# the standard suite (long)
```

## Contributing

See [`.github/CONTRIBUTING.md`](.github/CONTRIBUTING.md). PRs target `master`,
are built across the [CI matrix](.github/workflows/ci.yml) (Linux/macOS/Windows,
multiple compilers and configure options), and are reviewed by maintainers and
the automated OCR reviewer.

## License

Berkeley DB is distributed under its original license; see
[`LICENSE`](LICENSE). Individual archived versions carry the license in effect
for that release.
