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

- **Serializable Snapshot Isolation (SSI)** — the `DB_TXN_SNAPSHOT`
  transaction mode is now serializable: it detects dangerous read/write
  dependency structures and
  aborts the pivot with `DB_SNAPSHOT_CONFLICT`. Both of Cahill's rw-conflict
  detection mechanisms are implemented: the lock-table path (a concurrent
  writer meeting a reader's SIREAD marker) and the MVCC version-chain path in
  `mp_fget` (a reader handed an older version than one a concurrent writer
  committed). SIREAD markers are reclaimed incrementally and bounded (not only
  at checkpoint); the commit-time pivot check is race-free against concurrent
  edge recording; and a `DB_TXN_SNAPSHOT` transaction is rejected with `prepare()`/2PC.
  (This fork makes `DB_TXN_SNAPSHOT` serializable and removed the earlier
  separate `DB_TXN_SNAPSHOT_SAFE` flag — a deliberate ABI break.)
  The SIREAD marker/locker/detail lifetime is hardened for concurrent writers:
  a family of pre-existing use-after-free bugs (most importantly a lock object
  reclaimed while it still held SIREAD markers) was found with TSan/ASan and
  fixed, and a multi-process concurrent-writer stress test (`ssi009`) guards
  against regression. **Still experimental** in that page-granularity conflict
  tracking can raise abort rates under contention (measured by the
  microbenchmark suite under `test/bench`), and the HA/replication
  qualification is still being built. Planned performance work is tracked as
  design proposals under [`rfc/`](rfc/), targeting matching or beating InnoDB
  and WiredTiger on multicore/NUMA scalability and performance.

## Building

Two build systems produce the core library; the Autoconf tree is the reference
(full feature set + language bindings), Meson is a fast parallel build of the
core C library.

### Autoconf (reference)

```sh
cd build_unix
../dist/configure            # see ../dist/configure --help for options
make -j

# Common variants:
#   --enable-debug --enable-diagnostic   developer build with assertions
#   --enable-cxx                          C++ API
#   --enable-sql                          SQL (SQLite-compatible) API
#   --enable-test --with-tcl=<dir>        build the TCL test harness

make docs                    # render docs_src/ -> docs-build/ (needs pandoc)
make bench                   # build the test/bench microbenchmark drivers
make compdb                  # compile_commands.json for clangd (needs bear)
```

### Meson / Ninja (core C library)

```sh
meson setup build            # thin root meson.build drives dist/meson.build
ninja -C build               # -> build/dist/libdb.so
ninja -C build docs          # render the docs
ninja -C build bench         # build the microbenchmark drivers
```

Both `dist/meson.build` and `dist/meson_options.txt`-worth of build logic live
under `dist/` alongside the Autoconf files; the root `meson.build` is a thin
shim that Meson requires at the setup directory (`meson_options.txt` also stays
at the root because Meson binds it to the `project()` directory).

**LSP / clangd:** Meson emits `build/compile_commands.json` automatically after
`ninja`; symlink or point clangd at it (`ln -sf build/compile_commands.json .`).
The Autoconf build has no compilation database, so `make compdb` (in
`build_unix`) wraps the build with [`bear`](https://github.com/rizsotto/Bear)
to produce a repo-root `compile_commands.json`. Both are git-ignored.

To read the API and guide documentation, build it from the Markdown
source under [`docs_src/`](docs_src/) (`make docs` for Autoconf or
`ninja docs` for Meson — see above) and open `docs-build/html/index.html`
in a browser. The rendered reference is also published at
<https://libdb.org/reference/>.

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
[`LICENSE`](LICENSE). Per-component and bundled-code licenses (BSD, Harvard,
CDDL, ASM, and the Berkeley DB license in HTML form) are collected under
[`LICENSES/`](LICENSES/). Individual archived versions carry the license in
effect for that release.
