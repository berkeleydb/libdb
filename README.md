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
| `vYYYY.0M[.MICRO]` tags | **Fork releases**, versioned with [CalVer](https://calver.org/) (`v2026.09`, `v2026.09.1`, …). |

### Versioning covenant

Fork releases use **CalVer** (`vYYYY.0M`, with `.MICRO` for a same-month
re-release) to keep the community continuation unmistakably distinct from the
Sleepycat/Oracle `5.3.x` artifacts. Two promises hold from `v2026.09` onward:
published tags are **immutable** — never moved, deleted, or force-rewritten
(the one-time SemVer→CalVer history rewrite that established this scheme was the
last such event) — and the on-disk/log/region formats and the shared-object
soname (`libdb-2026.0`) change **only** through an announced, ABI-gated break,
never silently. The internal `DB_VERSION_MAJOR.MINOR.PATCH` compatibility level
is frozen and is not the release version; see [`VERSIONING.md`](VERSIONING.md).

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

### Known-missing versions and patches

The archive aims to be a complete record of Berkeley DB up to the last release
under the original Sleepycat license (**5.3.28**). A few historical artifacts
are not yet imported. **If you have an authentic copy of any of these — an
original `db-X.Y.Z.tar.gz` or an official `patch.*` file — please
[open an issue](https://github.com/berkeleydb/libdb/issues/new) with a link to
or a copy of the source** so it can be verified (SHA-256) and added to the
archive:

- **The 5.2 line** — `5.2.28` and `5.2.36` (raw tarballs exist on the `vendor`
  branch but are not yet imported as tagged, patch-tracked releases).
- **Early 5.3 point releases** before `5.3.21` (`5.3.0`–`5.3.15`).
- **Late 4.6.21 patches** beyond `4.6.21.4`, if any shipped.
- **Pre-3.x point releases** — many 2.x releases between the tagged
  `2.3.16` / `2.4.14` / `2.7.7` (e.g. `2.1.0`, `2.2.6`, `2.5.9`, `2.6.4`) and
  minor 3.x/4.x point releases not listed above.

**Deliberately excluded:** every Berkeley DB release from **6.0 (2013) onward**
is licensed by Oracle under the **AGPLv3**, which is incompatible with
redistributing them under this project's Sleepycat-license terms. Those
releases are therefore *intentionally* absent and are **not** on the
known-missing list. If Oracle ever relicenses the 6.x+ line under compatible
terms, they will be imported and the archive extended forward accordingly.

## What's new on the living fork

- **Serializable Snapshot Isolation (SSI)** — the `DB_TXN_SERIALIZABLE`
  transaction mode adds serializability on top of snapshot isolation: it
  detects dangerous read/write
  dependency structures and
  aborts the pivot with `DB_SNAPSHOT_CONFLICT`. Both of Cahill's rw-conflict
  detection mechanisms are implemented: the lock-table path (a concurrent
  writer meeting a reader's SIREAD marker) and the MVCC version-chain path in
  `mp_fget` (a reader handed an older version than one a concurrent writer
  committed). SIREAD markers are reclaimed incrementally and bounded (not only
  at checkpoint); the commit-time pivot check is race-free against concurrent
  edge recording; and a `DB_TXN_SERIALIZABLE` transaction is rejected with `prepare()`/2PC.
  `DB_TXN_SNAPSHOT` remains **plain (non-serializable) snapshot isolation**,
  exactly as legacy Berkeley DB behaved — it may exhibit write skew and the
  read-only-transaction anomaly. SSI is opt-in via the separate
  `DB_TXN_SERIALIZABLE` flag (an additive public flag, not an ABI break).
  *Migration:* code that relied on this fork's earlier behavior of
  `DB_TXN_SNAPSHOT` meaning SSI must now pass `DB_TXN_SERIALIZABLE`.
  The SIREAD marker/locker/detail lifetime is hardened for concurrent writers:
  a family of pre-existing use-after-free bugs (most importantly a lock object
  reclaimed while it still held SIREAD markers) was found with TSan/ASan and
  fixed, and a multi-process concurrent-writer stress test (`ssi009`) guards
  against regression. **Still experimental** in that page-granularity conflict
  tracking can raise abort rates under contention (measured by the
  microbenchmark suite under `test/bench`).

- **Concurrent-read scaling on a shared handle.** `DB->get` allocates and frees a
  transient cursor per operation, and moving it between the handle's free and
  active queues used to serialize every `get` on one mutex. Those queues are now
  sharded per handle, which removes the wall: on a 96-vCPU machine, uniform
  random reads through one shared `DB_THREAD` handle went from 174k to 3.7M
  ops/s at 24 threads, and from a *declining* curve past 8 threads to one that
  keeps climbing. Single-threaded throughput is unchanged, and workloads that
  contend on the same few pages gain little (they are bound by the lock
  partition, not cursor allocation). Numbers, method and limits:
  [`test/bench/CURSOR-SHARD-RESULTS.md`](test/bench/CURSOR-SHARD-RESULTS.md).

- **Replication has an executable isolation test.** `test/repiso/` runs a real
  two-process master/client pair over a socket and asserts that every page a
  replicated transaction modified appears in that transaction's commit lock
  list — the invariant whose violation caused issue #140. Replication was the
  least-tested subsystem; this is its first isolation gate.

  Further performance work is tracked as design proposals under [`rfc/`](rfc/),
  targeting matching or beating InnoDB and WiredTiger on multicore/NUMA
  scalability and performance. Changes are gated by the regression harness in
  [`test/bench`](test/bench/README.md), whose tolerances are derived from a
  measured noise floor rather than guessed.

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

Beyond the TCL suite, the fork adds targeted tiers that each guard a specific
class of defect. All run from a plain `--enable-debug` build:

| Tier | What it proves |
|---|---|
| [`test/db`](test/db) | one runner per fixed defect, each verified to fail when its fix is reverted |
| [`test/isolation`](test/isolation) | concurrent schedules validated against *some* serial order (write skew, G2, read-only anomaly) |
| [`test/soak`](test/soak) | region, mutex and locker counts return to baseline over tens of thousands of transactions |
| [`test/lockmatrix`](test/lockmatrix) | exhaustive lock-mode matrix under ASan; asserts the invariant, so a newly added mode is covered |
| [`test/repiso`](test/repiso) | two-process replication: every page a replicated txn modified must appear in its commit lock list |
| [`test/sim`](test/sim) | deterministic crash/recovery simulation with injected faults |
| [`test/fuzz`](test/fuzz) | malformed-file corpus with an ASan-instrumented library and a per-seed timeout |
| [`test/faultinject`](test/faultinject) | sweeps every allocation failure point and asserts clean teardown |
| [`test/bench`](test/bench) | performance regression gate, tolerances derived from a measured noise floor |

```sh
bash test/isolation/run.sh      # and soak / lockmatrix / repiso the same way
bash test/fuzz/check-crashes.sh
```

## Contributing

See [`.github/CONTRIBUTING.md`](.github/CONTRIBUTING.md). PRs target `master`,
are built across the [CI matrix](.github/workflows/ci.yml) (Linux/macOS/Windows,
multiple compilers and configure options), and are reviewed by maintainers and
the automated OCR reviewer.

## Design proposals (RFCs)

Non-trivial changes — a new access method, an on-disk/log/region/ABI format
change, a performance subsystem, a durability-model change — are written down as
an **RFC** in [`rfc/`](rfc/) *before* large implementation effort, so the
reasoning survives and the decision is explicit. The register is
[`rfc/INDEX.md`](rfc/INDEX.md); the full process is in
[`rfc/README.md`](rfc/README.md). In brief:

1. **Open** — copy [`rfc/0000-template.md`](rfc/0000-template.md) to the next
   free `NNNN-title.md`, fill in Summary / Motivation / Design / Alternatives /
   Risks, set `Status: Draft`, and add a row to `INDEX.md`.
2. **Review** — judged first against the north star: a proposal is **rejected
   outright** if it breaks embedded/no-server operation, ACID, crash recovery,
   any access method, multi-process correctness, or on-disk/log/region/ABI
   format stability — *unless* it argues a versioned, backward-compatible
   migration. Past that gate, review weighs correctness risk, **measured** (not
   asserted) performance evidence from [`test/bench`](test/bench), maintenance
   cost, and scope.
3. **Decide** — the maintainer records the decision in the RFC, flipping
   `Status:` to `Accepted` or `Rejected` with a dated rationale. A rejected RFC
   is never deleted — the "no" and its reasons are the value.
4. **Implement** — an accepted RFC drives the work and flips to `Implemented`,
   linking the PRs.

**RFCs welcome.** Open a PR that adds a new `Status: Draft` RFC, or one that
updates or supersedes an existing one — a measured result that changes an
assumption, an amendment to an accepted design, or a fresh proposal. Small,
obvious, or purely-internal changes do not need an RFC.

### Cross-subsystem design notes

Where the risk is the *interaction* between already-shipped subsystems rather
than any single change, the reasoning lives in [`docs/design/`](docs/design) as a
normative note about current `master` rather than as a proposal:

- [`docs/design/global-invariants.md`](docs/design/global-invariants.md) — the
  invariants that hold **across** the lock manager (+SSI), the transaction
  region, mpool/MVCC, the WAL, checkpoint, failchk, recovery, the btree
  root-snapshot read path and cursor sharding: what must be true **at a
  checkpoint**, **at a crash / during recovery**, and **at a region
  re-attach**, with the global lock order stated as one partial order, the
  subsystem pairs that must not be composed naively, and an honest map of which
  test tier enforces each invariant and where coverage is thin.

## License

Berkeley DB is distributed under its original license; see
[`LICENSE`](LICENSE). Per-component and bundled-code licenses (BSD, Harvard,
CDDL, ASM, and the Berkeley DB license in HTML form) are collected under
[`LICENSES/`](LICENSES/). Individual archived versions carry the license in
effect for that release.
