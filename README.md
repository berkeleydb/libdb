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

### Release cycle and LTS

**Regular releases** come out when there is something worth shipping, and are
named for the month they ship in (`v2026.10`, then `v2026.10.1`, `v2026.10.2`,
… for later releases in the same month). A release can change the
shared-memory region format if it measurably improves something. Such a change
is announced in the release notes, gated by the build signature (a mismatched
process fails with `DB_VERSION_MISMATCH` and reads nothing), and comes with an
upgrade note in `docs_src/guides/upgrading/`. Database and log formats, the
soname and the public ABI do not change in a regular release. Only the latest
regular release gets fixes.

**An LTS (long-term support) release** is a regular release that has also
passed the LTS qualification in
[`test/audit/QUALIFICATION-MATRIX.md`](test/audit/QUALIFICATION-MATRIX.md).
That means a 72-hour resource-accounting soak with zero failures on Linux
x86_64, FreeBSD x86_64, FreeBSD aarch64 and Windows/MSVC x86_64, in addition
to the normal release bar. Declaring a release LTS is a promise about the
**line** that starts from it:

- **Frozen formats.** The on-disk, log and region formats, the soname, and the
  public ABI do not change anywhere in the line. Every later release in it can
  open an environment created by any earlier one, and processes from different
  releases in the line can share one environment.
- **Fixes only.** Releases in the line carry bug fixes, security fixes and
  portability fixes. New features and performance work go to regular releases.
- **Supported until the next LTS, plus a 12-month overlap.** When a new LTS is
  declared, the previous line keeps getting fixes for 12 months so applications
  have time to move.

If you want a format that never changes underneath you, use an LTS release.
If you want the newest work, use the latest regular release. Regular releases
are cut from `master`. An LTS line lives on its own branch (`lts/2026.10.7`),
which receives only fixes backported from `master`, and ships patch releases
numbered `<base>.N` (`v2026.10.7.1`). [`RELEASING.md`](RELEASING.md) describes
both.

| Release | Status |
|---|---|
| `v2026.10.8` | **Current regular release.** Faster transactional writes: the deadlock detector skips its graph when no deadlock is possible, and inserts stop locking idle cursor queues. Formats, soname and ABI are the same as 2026.10.7. See the 2026.10.8 upgrade note. |
| `v2026.10.7.1` | **Current LTS.** First patch release of the 2026.10.7 line (branch `lts/2026.10.7`): two data-correctness fixes (`DB_PREV` skipping records on aarch64; `DB_TRUNCATE` with a subdatabase name destroying the file) and a lock-order fix; formats unchanged. See the 2026.10.7.1 upgrade note. |
| `v2026.10.7` | **First LTS release.** Qualified on the full matrix including a 72-hour resource-accounting soak on Linux x86_64, FreeBSD x86_64, FreeBSD aarch64 and Windows/MSVC x86_64, all clean. Start of the first LTS line: on-disk, log and region formats, the soname and the public ABI are frozen within it. |
| `v2026.10.6` | Superseded by `v2026.10.7`. Same engine on every platform except macOS arm64. |

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

### Measured against Berkeley DB 5.3.28

libdb 2026.10.8 compared with Oracle Berkeley DB 5.3.28 (2013), the last Sleepycat-licensed release. Both are
built the same way and run the same benchmark source, written against the 5.3 API, on one EC2 c6id.metal
(2 × Xeon 8375C, 128 vCPU, Fedora 45, XFS on local NVMe). Every operation is a transaction with write-no-sync
commit and deadlock detection on every conflict. 1M records, scrambled-zipfian keys. The tables give committed
operations per second, v2026.10.8 ÷ 5.3.28, median of 3 runs. † = CV above 10%, read as a direction only.

| Workload | 1 thread | 4 threads | 16 threads | 32 threads |
|---|---:|---:|---:|---:|
| Btree read-only (YCSB C) | 1.04 | 1.44 | 1.51 | 1.10 |
| Btree 95% read (YCSB B) | 1.06 | 1.58 | 2.58 | 2.31 † |
| Btree read-modify-write (YCSB F) | 0.97 | 1.34 | 1.80 | 1.05 |
| Btree short range scans (YCSB E) | 0.94 | 5.05 | 3.30 | 3.07 |
| Btree insert | 0.98 | 0.97 | 0.78 | 0.90 |
| Hash read-only | 0.99 | 1.24 | 1.60 | 1.13 † |
| Hash insert | 0.96 | 1.06 | 0.83 | 0.90 |
| Heap read-only | 1.02 | 1.68 | 2.95 | 2.61 |
| Queue short range scans | 0.97 | 2.92 | 3.31 | 3.32 |
| Recno read-only | 1.01 | 1.32 | 1.49 | 1.01 † |
| Recno insert | 0.98 | 0.78 | 0.54 | 0.65 |
| TPC-C-like (5 transaction types, 64 warehouses) | 0.93 | 6.36 † | 5.95 | 5.61 |
| TPC-B-like, write-no-sync | 0.96 | 1.09 | 0.79 | 0.81 |
| TPC-B-like, sync commit | 0.99 | 1.02 | 0.98 | 0.99 |

- **Faster:**
  - concurrent reads and scans: 1.18–5.05× at 4 and 16 threads on every access method (24 cells);
  - TPC-C-like: 5.6–6.4× at 4–32 threads.
- **About the same:** single-threaded work. The geometric mean is 0.98 across 27 single-thread cells
  (0.94–1.06).
- **Still slower:**
  - inserts at 16–32 threads, worst on Recno;
  - TPC-B-like with write-no-sync at 16–32 threads.

  Btree inserts at 16 threads are bound by every thread appending to the same last page. The cache eviction
  sweep (R3 in the report) is a known cost on bulk loads and is not fixed yet. The TPC-B gap is not yet
  explained.

The full table (27 workload and access-method combinations), the method, the raw data and the analysis of
each regression are in [`test/bench/XREL-2026-10.md`](test/bench/XREL-2026-10.md). The report also covers
v2026.10.7 against 5.3.28 at up to 128 threads, and the release A/B on Linux and FreeBSD aarch64.

### What the fork has changed since 5.3.28

**Concurrency and performance**

- Concurrent reads on a shared handle no longer serialize on one mutex. Cursor queues are sharded per handle,
  so uniform reads at 24 threads went from 174k to 3.7M ops/s on a 96-vCPU host.
- The locker table is striped (64 stripes), and locker mutexes are reused instead of destroyed per
  transaction. On FreeBSD, libthr makes each destroy cost O(live objects), and reusing them gave 1500× at one
  thread and 14.7× at 32 (2026.10.4).
- Optimistic read-path page validation (RFC 0007), with 2.05× batched reads at 96 threads.
- A hybrid spin-then-sleep mutex on AArch64 (FreeBSD, Linux and macOS).
- `DB->get_multiple()` for batched point reads.
- Since 2026.10.8, the deadlock detector skips its graph when no deadlock is possible.

**Correctness and isolation**

- Serializable Snapshot Isolation as a separate, opt-in `DB_TXN_SERIALIZABLE` mode. `DB_TXN_SNAPSHOT` stays
  plain snapshot isolation, as in Berkeley DB.
- Data-loss bugs fixed:
  - a reverse cursor walk that could skip live records (clang on aarch64);
  - `DB_TRUNCATE` with a subdatabase name, which destroyed the whole file;
  - `DB_LOG_DIRECT` durability;
  - several lock-order inversions and use-after-frees found by TSan, ASan and the lock-order checker.
- Every fixed defect has a regression test that fails without the fix.

**New features**

- Typed data and constraints (RFC 0014, opt-in `--enable-typed`): codecs for keys and fields, with NOT NULL,
  MAXLEN, VALID and UNIQUE enforced on put.
- Rust and Zig bindings (RFC 0016).
- DTrace and USDT probes, including the log fsync and the checkpoint page write.
- A Meson/Ninja build alongside autoconf. The two produce environments that can attach to each other.

**Portability**

- FreeBSD 14 (x86_64 and aarch64) is qualified. It builds a shared library and scales past one thread, which
  5.3.28 did not do on FreeBSD 10 and later.
- Linux x86_64 and aarch64 are qualified on EC2, macOS arm64 in CI, and Windows (MSVC x64) by build in CI
  plus, for an LTS release, a 72-hour soak.
- The platforms that are not qualified are listed in `test/audit/QUALIFICATION-MATRIX.md`.

**Release discipline**

- CalVer releases, and an LTS line with frozen formats (`VERSIONING.md`, `RELEASING.md`).
- A build signature: a mismatched process fails with `DB_VERSION_MISMATCH` and reads nothing.
- A per-release qualification matrix: `test/db` on every platform, ABI and handle-size gates, metal
  benchmarks, and a 72-hour soak for an LTS release.

### Details

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

Bug reports, fixes, measurements and design proposals are all welcome. The
full guide is [`.github/CONTRIBUTING.md`](.github/CONTRIBUTING.md).

### Reporting a bug

Open a [GitHub issue](https://github.com/berkeleydb/libdb/issues). Include:

- the libdb version (`db_version()`, or the tag or commit you built);
- the platform: OS and version, CPU architecture, compiler;
- the `configure` or meson options you built with;
- what you did, what you expected, and what happened, with the exact error
  text (for example `BDB0060 PANIC ...`);
- ideally, a small program or script that reproduces it.

A bug in Oracle Berkeley DB 5.3.28 is in scope too: the fork inherits that
code. If you are not sure whether something is a bug, open the issue anyway.

**Security issues:** do not open a public issue. Report them privately, as
[`.github/SECURITY.md`](.github/SECURITY.md) describes.

### Submitting a change

1. Fork the repository and branch from `master`. Never target `historical`,
   an `lts/*` branch or a version tag.
2. Make **one logical change**, with a regression test. A bug fix needs a test
   that fails without the fix. Put it under `test/db/` and add it to
   `test/MANIFEST`.
3. Use a [Conventional Commits](https://www.conventionalcommits.org/) message
   (`fix:`, `feat:`, `perf:`, `test:`, `docs:`, …) that says what changed
   and why.
4. Open a pull request against `master`. The
   [PR template](.github/pull_request_template.md) asks what you tested and
   whether formats and the ABI are preserved.
5. CI builds and tests it across the [matrix](.github/workflows/ci.yml): Linux,
   macOS and Windows, several compilers, mutex backends, sanitizers and
   configure options. Keep it green. Maintainers and the automated OCR
   reviewer then review it.
6. A performance claim needs a measurement:
   - a before/after on the same host;
   - repeated runs, with their variance;
   - the harness, so others can rerun it.
   [`test/bench`](test/bench/README.md) has the tools.

Where a fix lands, and how fixes reach a release: a fix lands on `master`
first. If an LTS line needs it too, a maintainer labels the PR
`backport-lts` and cherry-picks it onto the `lts/*` branch afterwards
([`RELEASING.md`](RELEASING.md)). Changes to the on-disk, log or region
format, the soname or the public ABI never go to an LTS line. On `master`
they need an RFC (below).

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

**Do I need an RFC?** Yes for:

- a new access method;
- any change to the on-disk, log or region format, the soname or the public ABI;
- a new subsystem;
- a change to the durability or isolation model;
- a performance change that moves a lock or latch boundary.

No for a bug fix, a test, a portability fix, or documentation. If in doubt,
open an issue first and ask.

### Cross-subsystem design notes

Where the risk is the *interaction* between already-shipped subsystems rather
than any single change, the reasoning lives in [`rfc/`](rfc) as a
normative note about current `master` rather than as a proposal:

- [`rfc/0010-global-invariants.md`](rfc/0010-global-invariants.md) — the
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
