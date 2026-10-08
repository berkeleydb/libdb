<!--
Copyright (c) 2026 libdb contributors.  All rights reserved.

SPDX-License-Identifier: Sleepycat

See the file LICENSE for redistribution information.
-->

# libdb repository audit: hard-fork baseline → today

> **Preserved from `.agent/`, which is gitignored.** This audit was produced
> as an agent working note and existed only on one developer's machine. Its
> findings drove real decisions -- the A1 handle-size gate, the packaging
> questions, and the correction of several claims the project had been
> repeating -- so the evidence belongs in the repository rather than in an
> untracked directory. Read-only inspection; it changes nothing by itself.


| | |
|---|---|
| Repository | `/home/gburd/ws/libdb` (read-only inspection) |
| Baseline | `6710649f0` — 2026-04-21 — "Update autoconf, fix atomics and tests." |
| Current | `f677e28ed` = `origin/master` — 2026-10-06 — "Merge pull request #209 from berkeleydb/fix/u8-u9-backup-csharp" |
| Span | 783 commits (189 of them merges), 2026-04-21 → 2026-10-06 |
| Tracked files | 9,231 at baseline → 5,921 at HEAD (−3,310) |

## Reproducing the numbers

Every figure below comes from one of these commands, run with
`git -C /home/gburd/ws/libdb`:

```sh
# top-line (rename detection off, so a rename counts as A + D consistently
# with the per-path tallies; with renames on, git reports 27 R pairs)
git diff --no-renames --numstat      6710649f0..origin/master   > /tmp/numstat_all.txt
git diff --no-renames --name-status  6710649f0..origin/master   > /tmp/ns_all2.txt
git diff --shortstat                 6710649f0..origin/master   # cross-check

# engine
git diff --no-renames --numstat 6710649f0..origin/master -- src > /tmp/src_numstat.txt
git diff 6710649f0..origin/master -- src/dbinc/db.in

# per-path subtotals: awk over the two files above, filtering $3 by prefix
# removal provenance
git log --diff-filter=D --format='%h %ad %s' --date=short 6710649f0..origin/master -- <path> | tail -1
# tree inventories
git ls-tree -r --name-only <rev> -- <path>
git ls-tree -d --name-only <rev> -- test/
# line counts of a tree: git ls-tree -r --format='%(objectname) %(path)' | git cat-file --batch
#   (script /tmp/count_lines2.py; skips blobs with NUL in the first 8 KB as binary)
```

Two commands did **not** produce a result and are reported as such:
- `python3 /tmp/count_lines2.py <rev> .` (whole-repo line count at each point) timed out at 900 s on both revisions. Whole-repo line totals are therefore absent; subtree totals (`src`, `test`) completed and are given.
- `git diff --name-status` *with* rename detection emitted `warning: exhaustive rename detection was skipped due to too many files` — hence `--no-renames` for all tallies.

Generated/vendored paths excluded from the "hand-written" columns, exactly as specified: `build_windows/`, `build_android/`, `dist/configure`, `src/dbinc_auto/`, `lang/java/src/com/sleepycat/db/internal/DbConstants.java`.

---

## 1. Top-line

### All paths

| | Files | Insertions | Deletions |
|---|---:|---:|---:|
| Added | 2,298 | 866,183 | 0 |
| Deleted | 5,608 | 0 | 911,392 |
| Modified | 221 | 14,043 | 5,063 |
| **Total** | **8,127** | **880,226** | **916,455** |

`git diff --shortstat` reports `8100 files changed, 880206 insertions(+), 916435 deletions(-)`; the 27-file / 20-line difference is rename pairs collapsing into single entries when rename detection is on. 226 of the changed files are binary (41 added, 185 deleted) and contribute 0 to the line counts.

### Excluding generated/vendored

| | Files | Insertions | Deletions |
|---|---:|---:|---:|
| Added | 2,298 | 866,183 | 0 |
| Deleted | 5,602 | 0 | 910,698 |
| Modified | 172 | 9,059 | 2,120 |
| **Total** | **8,072** | **875,242** | **912,818** |

Generated/vendored portion alone: 55 files, +4,984 / −3,637 (49 modified, 6 deleted — the 6 deletions are the C# `.sln`/`.vcxproj` files under `build_windows/`).

### A third cut: also excluding committed lcov output

`test/coverage/` contains four committed `cov-src.info` lcov data files totalling **634,850 inserted lines** across 4 files. They are tool output, not authored text. Excluding them *as well as* the generated paths:

| | Files | Insertions | Deletions |
|---|---:|---:|---:|
| Added | 2,294 | 231,333 | 0 |
| Deleted | 5,602 | 0 | 910,698 |
| Modified | 172 | 9,059 | 2,120 |
| **Total** | **8,068** | **240,392** | **912,818** |

**This is the honest shape of the change**: ~240 k lines of authored addition against ~913 k lines of removal, the latter dominated by the deleted `docs/` DocBook archive (764,534 lines) and the C# binding.

### Per top-level directory

| Directory | A | D | M | + | − |
|---|---:|---:|---:|---:|---:|
| `test` | 437 | 54 | 16 | 734,068 | 22,543 |
| `docs_src` | 1,762 | 0 | 0 | 115,118 | 0 |
| `src` | 13 | 5 | 123 | 10,137 | 1,754 |
| `dist` | 25 | 43 | 37 | 8,413 | 10,558 |
| `rfc` | 23 | 0 | 0 | 6,375 | 0 |
| `.github` | 22 | 0 | 0 | 4,022 | 0 |
| `(root files)` | 9 | 1 | 1 | 664 | 5 |
| `build_windows` | 0 | 6 | 28 | 645 | 751 |
| `LICENSES` | 7 | 0 | 0 | 464 | 0 |
| `lang` | 0 | 127 | 8 | 188 | 60,673 |
| `build_android` | 0 | 0 | 6 | 116 | 42 |
| `util` | 0 | 0 | 2 | 16 | 9 |
| `docs` | 0 | 5,245 | 0 | 0 | 764,534 |
| `build_vxworks` | 0 | 79 | 0 | 0 | 43,830 |
| `build_wince` | 0 | 11 | 0 | 0 | 6,569 |
| `examples` | 0 | 37 | 0 | 0 | 5,187 |

---

## 2. What was removed

| Subsystem | Files | Lines removed | Removing commit |
|---|---:|---:|---|
| `docs/` (whole DocBook archive) | 5,245 | 764,534 | `88cbdbe33` 2026-08-03 "build: remove old docs/ archive; add doc/bench/compdb build targets" (+ `eb81dcf4b`, `022a557f9` earlier partials) |
| └ of which `docs/csharp` | 2,457 | 87,677 | `080217acd` 2026-08-03 "chore(repo): remove C# binding, dead README, and public ROADMAP" |
| └ `docs/api_reference` | 1,286 | — | `88cbdbe33` |
| └ `docs/java` | 528 | — | `88cbdbe33` |
| └ `docs/programmer_reference` | 217 | — | `88cbdbe33` |
| └ `docs/upgrading` | 182 | — | `88cbdbe33` |
| └ `docs/gsg` / `docs/gsg_txn` | 142 / 142 | — | `88cbdbe33` |
| └ `docs/installation` | 111 | — | `022a557f9` |
| └ `docs/gsg_db_rep` | 83 | — | `88cbdbe33` |
| └ `docs/collections` | 39 | — | `88cbdbe33` |
| └ `docs/bdb-sql` | 32 | — | `88cbdbe33` |
| └ `docs/porting` | 18 | — | `88cbdbe33` |
| └ `docs/articles` | 6 | — | `88cbdbe33` |
| VxWorks (`build_vxworks/`) | 79 | 43,830 | `022a557f9` 2026-06-25 "build: remove VxWorks platform support" |
| VxWorks (`src/os_vxworks/`) | 5 | 638 | `022a557f9` |
| VxWorks (`dist/vx_*`, `dist/s_vxworks`, `dist/validate/s_chk_vxworks`) | 22 | 3,048 | `022a557f9` |
| **VxWorks total** (any `vxworks`/`vx_` path) | **111** | **49,363** | commit stat: `146 files changed, 314 insertions(+), 50016 deletions(-)` |
| WinCE (`build_wince/`) | 11 | 6,569 | `a6e91d033` 2026-06-25 "build: remove Windows CE platform support" |
| WinCE (`dist/adodotnet/`) | 7 | 689 | `a6e91d033` |
| WinCE (`dist/wince_config.in`) | 1 | 656 | `a6e91d033` |
| **WinCE total** (any `wince`/`_ce`/`WinCE` path) | **19** | **8,636** | commit stat: `53 files changed, 23 insertions(+), 9500 deletions(-)` |
| C# binding `lang/csharp` | 125 | 59,864 | `080217acd` |
| C# `examples/csharp` | 37 | 5,187 | `080217acd` |
| C# `test/csharp` | 54 | 22,521 | `d84ad5ca5` 2026-08-03 "chore: remove remaining C# remnants" |
| C# `dist/s_csharp*` (5 scripts) | 5 | — | `080217acd` |
| C# `dist/win_projects/*csharp*`, `dist/winmsi/fixupCsharp.xq` | 4 | — | `080217acd` |
| C# `build_windows/BDB_dotNet*.sln`, `db_csharp.vcxproj/.vcproj` | 6 | 694 | `080217acd` |
| `lang/sql` (2 VxWorks-only files) | 2 | 801 | `022a557f9` |
| `README` (plain, superseded by `README.md`) | 1 | 5 | `080217acd` |
| `ROADMAP.md` | 1 | — | `080217acd` (it was *added* at `f15717021` 2026-06-16 and removed 2026-08-03, so it nets to zero in the diff and shows 0 lines) |

C# removal commit `080217acd`: `2634 files changed, 7 insertions(+), 153906 deletions(-)`.
Docs-archive removal commit `88cbdbe33`: `2795 files changed, 641 insertions(+), 675699 deletions(-)`.

### Correction to a prior note

A memory on file names the C#-removal commit as `a77b2119d` with the message "C# is dropped as a supported binding". `a77b2119d` **exists but is not an ancestor of `origin/master`** (`git merge-base --is-ancestor a77b2119d origin/master` → false); it is a pre-rebase twin with the same tree stat (`2634 files, 153906 deletions`). The commit actually on master is `080217acd`, subject "chore(repo): remove C# binding, dead README, and public ROADMAP". The file counts in that memory (2,634 files / ~154 k lines) are correct for the commit as a whole; the per-directory split is 125 (`lang/csharp`) + 37 (`examples/csharp`) + 2,457 (`docs/csharp`) + 54 (`test/csharp`, a separate commit).

### Top-level directories that disappeared

`build_vxworks`, `build_wince`, `docs`, `README`.

Only 27 paths were actually *renamed* rather than deleted+added: 22 `docs/gsg_txn/*` and `docs/{installation,programmer_reference}/*` images into `docs_src/guides/*/img/`, `docs/license/license_db.html` → `LICENSES/license_db.html`, and `test/csharp/bdb4.7.db` → `test/db/fixtures/bdb4.7.db` (the one artefact kept out of the C# purge).

---

## 3. What was added

### New top-level entries

| Entry | Files | Lines |
|---|---:|---:|
| `docs_src/` | 1,762 | 115,118 |
| `rfc/` | 23 | 6,375 |
| `.github/` | 22 | 4,022 |
| `LICENSES/` | 7 | 464 |
| `README.md` | 1 | 273 |
| `flake.nix` + `flake.lock` | 2 | 136 |
| `VERSIONING.md` | 1 | 79 |
| `meson.build` + `meson_options.txt` | 2 | 56 |
| `.editorconfig`, `.gitattributes`, `.git-blame-ignore-revs` | 3 | (part of the 664 root-file insertions) |

### `.github/` — 15 workflows

`android.yml`, `bench.yml`, `ci-extended.yml`, `ci.yml`, `cocci.yml`, `coverage.yml`, `docs.yml`, `faultinject.yml`, `fuzz.yml`, `nightly-bigbox.yml`, `ocr-model-check.yml`, `ocr-review.yml`, `pbt.yml`, `rep-isolation.yml`, `test-tiers.yml`.
Plus 7 non-workflow files: `CODE_OF_CONDUCT.md`, `CONTRIBUTING.md`, `SECURITY.md`, `pull_request_template.md`, and `ocr/{context.md,litellm.yaml,rule.json}` (an LLM code-review bot config).

### `rfc/` — 11 numbered RFCs + template + index

`0000-template`, `0001-adaptive-lsm` (with a `0001/` prototype: `adaptive.c`, `adaptive.h`, `test_adaptive.c`, `Makefile`), `0002-buffer-swip-aio` (+2 surveys), `0003-ssi-serializable-snapshot-isolation` (+3 design docs), `0004-funnel-sparse-hash`, `0005-row-level-conflict-tracking`, `0006-chain-replicated-wal-multi-master`, `0007-optimistic-read-validation`, `0008-scalable-wal-append`, `0009-wal-backpressure`, `0010-global-invariants`, `0011-test-coverage-gaps`, `INDEX.md`, `README.md`.

### `docs_src/` — reverse-DocBook migration

| Subtree | Files |
|---|---:|
| `guides/` | 939 |
| `api/` | 797 |
| `_migrate/` (extraction + validators) | 14 |
| `_templates/`, `_site/`, `_data/` | 3 / 3 / 2 |
| `build.py`, `index.md`, `PLAN.md`, `os-aio-cross-reap-audit.md` | 4 |

1,697 of the 1,762 files are Markdown; 25 are images. `docs/` is no longer committed — `docs_src/build.py` renders Markdown → HTML + man + PDF into `docs-build/`, gated by `.github/workflows/docs.yml`.

### New `test/` tiers

| Tier | Files | Lines added | Note |
|---|---:|---:|---|
| `test/coverage` | 38 | 640,123 | **634,850 of these are 4 committed lcov `cov-src.info` files** — tool output |
| `test/bench` | 146 | 37,947 | microbenchmark drivers + committed result TSV/CSV |
| `test/c` | 38 | 15,909 | C unit/API tests (23 files existed at baseline; this is the delta) |
| `test/sim` | 65 | 14,056 | deterministic simulation testing (DST) |
| `test/db` | 28 | 4,072 | shell-driven engine regressions (1 binary fixture) |
| `test/isolation` | 12 | 4,167 | SSI / anomaly tiers |
| `test/pbt` | 16 | 2,804 | Hegel property-based tests |
| `test/repiso` | 7 | 2,279 | replication isolation |
| `test/tcl` | 21 | 1,953 | new TCL tests (483 files existed at baseline) |
| `test/cbmc` | 11 | 1,350 | bounded model checking |
| `test/faultinject` | 5 | 1,291 | SQLite-style malloc-failure injection |
| `test/fuzz` | 26 | 1,294 | 15 of 26 are binary corpus seeds |
| `test/config` | 2 | 1,247 | |
| `test/soak` | 3 | 997 | |
| `test/lockmatrix` | 3 | 826 | |
| `test/os` | 3 | 743 | |
| `test/backup` | 2 | 268 | |
| `test/xa` | 2 | 308 | additions to an existing tier |
| `test/tiers` | 1 | 74 | `meson.build` wiring B1/B2/B3 |

Plus 9 new files at `test/` root: `MANIFEST`, `harness.sh`, `check_manifest.sh`, `check_manifest_test.sh`, `KNOWN-ISSUES.md`, `TESTING-PROGRAM.md`, `TESTING-IMPROVEMENTS.md`, `GATE-GAPS-CLOSED.md`.

The `MANIFEST` + `harness.sh` pair is a vacuous-green gate: each tier runner emits `RESULT <tier> <name> <pass|fail|skip>` and `check_manifest.sh` diffs the emitted set against `test/MANIFEST`, so a test that never ran fails identically to a test that failed. `test/MANIFEST` has **184 entries across 13 tier names**: isolation 46, config 39, flagapi 29, flag 15, db 15, leak 14, sim 7, tcl 5, soak 5, p3durable 3, lockmatrix 3, shape 2, bench 1.

### New `dist/` tooling

| Addition | Files | Lines |
|---|---:|---:|
| `dist/meson.build` | 1 | 649 |
| `dist/cocci/` (Coccinelle rules + inventories) | 15 | 948 |
| `dist/meson/` (`gen_header.py`, `run_docs.py`, `run_bench.py`, `db_subs.json`, `README-windows.md`) | 5 | 314 |
| `dist/android/build_android.sh` | 1 | 250 |
| `dist/win_arm64_configs.py` | 1 | 168 |
| `dist/env_sig_print.sh` | 1 | 95 |
| `dist/s_execbits` | 1 | 54 |

---

## 4. Engine change (`src/` only)

`git diff --no-renames --shortstat 6710649f0..origin/master -- src` → **141 files changed, 10,137 insertions(+), 1,754 deletions(-)**.
Excluding `src/dbinc_auto/` (generated): **128 files, +9,907 / −1,710**.

Whole-tree line count: `src/` went from **227,146 lines (437 files)** to **235,529 lines (445 files)**, +8,383 net.

### Per-subdirectory

| Subdir | Files touched | + | − | Churn | Lines base → head |
|---|---:|---:|---:|---:|---|
| `src/os` | 16 | 1,960 | 125 | 2,085 | 6,288 → 8,123 |
| `src/mp` | 12 | 1,485 | 316 | 1,801 | 10,380 → 11,549 |
| `src/dbinc` | 19 | 1,475 | 139 | 1,614 | 17,194 → 18,530 |
| `src/lock` | 6 | 1,023 | 49 | 1,072 | 7,418 → 8,392 |
| `src/db` | 17 | 843 | 106 | 949 | 35,195 → 35,932 |
| `src/btree` | 7 | 769 | 9 | 778 | 23,887 → 24,647 |
| `src/mutex` | 4 | 655 | 28 | 683 | 5,550 → 6,177 |
| `src/os_vxworks` | 5 | 0 | 638 | 638 | 638 → 0 (deleted) |
| `src/log` | 3 | 576 | 9 | 585 | 14,922 → 15,489 |
| `src/txn` | 4 | 487 | 17 | 504 | 5,815 → 6,285 |
| `src/dbinc_auto` *(generated)* | 13 | 230 | 44 | 274 | 8,424 → 8,610 |
| `src/os_windows` | 13 | 70 | 186 | 256 | 2,996 → 2,880 |
| `src/qam` | 4 | 217 | 5 | 222 | 5,856 → 6,068 |
| `src/env` | 7 | 157 | 33 | 190 | 10,279 → 10,403 |
| `src/hash` | 2 | 37 | 14 | 51 | 13,684 → 13,707 |
| `src/fileops` | 1 | 37 | 9 | 46 | 3,283 → 3,311 |
| `src/heap` | 2 | 41 | 1 | 42 | 5,480 → 5,520 |
| `src/dbreg` | 1 | 23 | 2 | 25 | 2,513 → 2,534 |
| `src/crypto` | 1 | 16 | 8 | 24 | 3,681 → 3,689 |
| `src/hmac` | 1 | 17 | 0 | 17 | 512 → 529 |
| `src/clib` | 1 | 0 | 15 | 15 | 2,427 → 2,412 |
| `src/repmgr` | 1 | 13 | 1 | 14 | 15,042 → 15,054 |
| `src/common` | 1 | 6 | 0 | 6 | 3,079 → 3,085 |
| `src/rep` | 0 | 0 | 0 | 0 | 19,945 → 19,945 (untouched) |
| `src/sequence`, `src/xa` | 0 | 0 | 0 | 0 | unchanged |

`src/rep`, `src/sequence` and `src/xa` were **not modified at all**. Replication is the one major subsystem the fork has not touched.

### 10 most-changed source files

| # | File | + | − | Churn | Status |
|---:|---|---:|---:|---:|---|
| 1 | `src/lock/lock.c` | 709 | 17 | 726 | M |
| 2 | `src/btree/bt_search.c` | 683 | 3 | 686 | M |
| 3 | `src/mutex/mut_order.c` | 620 | 0 | 620 | **A** |
| 4 | `src/mp/mp_bh.c` | 380 | 112 | 492 | M |
| 5 | `src/os_vxworks/os_vx_map.c` | 0 | 436 | 436 | **D** |
| 6 | `src/mp/mp_alloc.c` | 341 | 46 | 387 | M |
| 7 | `src/os/os_aio_pool.c` | 367 | 0 | 367 | **A** |
| 8 | `src/mp/mp_fget.c` | 320 | 11 | 331 | M |
| 9 | `src/txn/txn.c` | 302 | 13 | 315 | M |
| 10 | `src/log/log_put.c` | 300 | 9 | 309 | M |

Next 10, for context: `src/dbinc/mp.h` (283), `src/mp/mp_fput.c` (276), `src/os/os_aio_kqueue.c` (267, A), `src/log/log_handoff_trace.c` (252, A), `src/os/os_atomic.c` (243), `src/os/os_aio_iocp.c` (237, A), `src/mp/mp_sync.c` (235), `src/db/db_iface.c` (226), `src/os/os_aio_uring.c` (223, A), `src/lock/lock_id.c` (206).

### New engine files (13)

| File | Lines | Purpose |
|---|---:|---|
| `src/mutex/mut_order.c` | 620 | lock-order verification |
| `src/os/os_aio_pool.c` | 367 | thread-pool AIO fallback |
| `src/os/os_aio_kqueue.c` | 267 | BSD kqueue+aio backend |
| `src/os/os_aio_posix.c` | 265 | POSIX aio backend |
| `src/log/log_handoff_trace.c` | 252 | group-commit handoff instrumentation |
| `src/os/os_aio_iocp.c` | 237 | Windows IOCP backend |
| `src/os/os_aio_uring.c` | 223 | Linux io_uring backend |
| `src/dbinc/lock_order.h` | 211 | |
| `src/dbinc/log_handoff_trace.h` | 201 | |
| `src/dbinc/os_aio.h` | 199 | |
| `src/os/os_aio.c` | 173 | backend dispatch |
| `src/os/os_csprng.c` | 109 | OS entropy (getrandom / arc4random_buf / /dev/urandom) |
| `src/os_windows/os_csprng.c` | 64 | Windows entropy |

### Deleted engine files (5)

`src/os_vxworks/os_vx_{map,config,rpath,yield,abs}.c` — 638 lines total.

### Changed internal headers

`src/dbinc/mp.h` +268/−15, `lock.h` +184/−5, `mutex.h` +84/−37, `txn.h` +73/−0, `db.in` +69/−14, `db_am.h` +41, `btree.h` +37, `os.h` +34/−8, `log.h` +24, `atomic.h` +21/−7, `qam.h` +15, `db_int.in` +12/−8, `mutex_int.h` +1/−28, `db_page.h` +1/−1, `win_db.h` −6, `globals.h` −10.

---

## 5. Test surface

| | Baseline | HEAD | Δ |
|---|---:|---:|---:|
| Files under `test/` | 850 | 1,233 | +383 |
| Text files | 849 | 1,217 | +368 |
| Binary files | 1 | 16 | +15 |
| Lines of text under `test/` | 206,231 | 917,756 | +711,525 |
| Lines excluding `test/coverage/` | 206,231 | 277,633 | **+71,402** |
| `test/coverage/` alone | 0 | 640,123 | +640,123 (99 % committed lcov output) |
| Tier directories (`git ls-tree -d test/`) | **10** | **25** | +15 |
| Tier runners (`test/*/run.sh`) | 5 (all `test/xa/src*`) | 11 | +6 |
| Manifest-gated tier names | 0 (no manifest) | 13 | +13 |

Baseline tiers: `c`, `csharp`, `cxx`, `java`, `micro`, `sql`, `sql_codegen`, `tcl`, `tcl_utils`, `xa`.

HEAD tiers: `backup`, `bench`, `c`, `cbmc`, `config`, `coverage`, `cxx`, `db`, `faultinject`, `fuzz`, `isolation`, `java`, `lockmatrix`, `micro`, `os`, `pbt`, `repiso`, `sim`, `soak`, `sql`, `sql_codegen`, `tcl`, `tcl_utils`, `tiers`, `xa` — `csharp` gone, 16 new.

New runners: `test/{cbmc,fuzz,isolation,lockmatrix,repiso,soak}/run.sh`.

Per-file counts at baseline → HEAD for the carried-over tiers: `tcl` 483→504, `java` 123→123, `sql_codegen` 73→73, `c` 23→61, `xa` 29→31, `cxx` 19→19, `sql` 18→18, `micro` 26→26 (unchanged), `tcl_utils` 2→2.

---

## 6. New public API (`src/dbinc/db.in`)

`git diff 6710649f0..origin/master -- src/dbinc/db.in` → **1 file changed, 69 insertions(+), 14 deletions(-)**.

### Version macros — UNCHANGED

`DB_VERSION_MAJOR`, `DB_VERSION_MINOR`, `DB_VERSION_PATCH`, `DB_VERSION_FAMILY`, `DB_VERSION_RELEASE`, `DB_VERSION_STRING`, `DB_VERSION_FULL_STRING` are all still `@...@` autoconf substitutions in `db.in` — the header text is byte-identical. What changed is the **substituted values in `dist/RELEASE`**:

| Variable | Baseline | HEAD |
|---|---|---|
| `DB_CALVER` | `2026.04` | `2026.10.1` |
| `DB_VERSION_MAJOR` | 2026 | 2026 (unchanged) |
| `DB_VERSION_MINOR` | 0 | 0 (unchanged) |
| `DB_VERSION_PATCH` | **1** | **9** |
| `DB_VERSION_FAMILY` / `RELEASE` / `LETTER` | 11 / 2 / "g" | unchanged |
| `DB_VERSION_UNIQUE_NAME` | `_2026000` (derived) | unchanged |
| `DB_RELEASE_DATE` | April 22, 2026 | October 6, 2026 |
| `LIBMAJOR` / `LIBVERSION` in `dist/Makefile.in` | `@DB_VERSION_MAJOR@` / `@MAJOR@.@MINOR@` | unchanged — **soname is stable** |

So MAJOR and MINOR did not move; PATCH went 1 → 9. `dist/RELEASE` now carries an explicit comment freezing the MAJOR/MINOR/PATCH triplet as the format/ABI compat level and designating `DB_CALVER` as the release knob.

### Added `#define`s in `db.in` (8)

| Symbol | Value | Kind | Notes |
|---|---|---|---|
| `DB_CALVER` | `"@DB_CALVER@"` | **new public macro** | fork release identity, distinct from the frozen compat triplet |
| `DB_SNAPSHOT_CONFLICT` | `-30968` | **new public error code** | SSI: conflicting snapshot update |
| `DB_SNAPSHOT_UNSAFE` | `-30967` | **new public error code** | SSI: potential snapshot anomaly |
| `DB_CURSOR_NPART` | `8` | **new public constant** | cursor-queue shard count; must be a power of two |
| `DB_ENV_MPOOL_AIO` | `0x00100000` | internal `dbenv->flags` bit | set by public `DB_MPOOL_AIO` |
| `DB_ENV_TXN_SERIALIZABLE` | `0x00200000` | internal `dbenv->flags` bit | set by public `DB_TXN_SERIALIZABLE` |
| `DB_ENV_NOIO` | `0x00400000` | **internal only, never settable** | splits "do no I/O during panic teardown" off from the public `DB_NOFLUSH`; process-local `dbenv->flags` bit, no region-layout/ABI consequence |
| `TXN_SNAPSHOT_SAFE` | `0x80000` | `DB_TXN->flags` bit | serializable snapshot isolation |

### Removed `#define`s in `db.in`

**None.** `comm -23` over the sorted `#define` name sets is empty.

### Added enum members (1)

```c
db_lockmode_t: DB_LOCK_SIREAD = 9   /* Snapshot isolation read (SSI) */
```
Appended after `DB_LOCK_WWRITE=8`, so no existing enumerator value shifted.

### Added public flags (via `dist/api_flags` → `src/dbinc_auto/api_flags.in`)

| Flag | Value | Accepted by |
|---|---|---|
| `DB_MPOOL_AIO` | `0x00100000` (`__PIN=`) | `DbEnv.set_flags` |
| `DB_TXN_SERIALIZABLE` | `0x00200000` (`__PIN=`) | `DbEnv.set_flags`, `DbEnv.txn_begin`, `Db.cursor` |

Both are explicitly value-pinned in `dist/api_flags` rather than auto-assigned, so the generator cannot renumber them. No public flag value was changed or removed; `src/dbinc_auto/api_flags.in` shows exactly 2 added `#define`s and 0 removed.

### `dist/pubdef.in` (the binding-coverage table) — +5 / −1

Added: `DB_LOCK_SIREAD` (`* I * *`), `DB_MPOOL_AIO` (`D I * *`), `DB_SNAPSHOT_CONFLICT` (`D I * *`), `DB_SNAPSHOT_UNSAFE` (`D I * *`), `DB_TXN_SERIALIZABLE` (`D I J *`).
Removed: `DB_ALIGN8` (`* I * *`) — it is a `db_int.in` internal, not a `db.h` symbol, so this is a table correction rather than an API removal.

Note on the `C` (C#) column: 311 rows still carry a `C` claim although `lang/csharp` is gone, and `dist/validate/s_chk_pubdef` reads that column. That file was modified (+44/−1) in the range, so the checker changed, but the stale column is still present at HEAD.

### Struct field changes (ABI-relevant)

**`struct __db` — layout CHANGED (breaking):**

```c
- struct __cq_fq { struct __dbc *tqh_first; struct __dbc **tqh_last; } free_queue;
- struct __cq_aq { struct __dbc *tqh_first; struct __dbc **tqh_last; } active_queue;
+ struct __cq_part {
+       db_mutex_t mutex;               /* Partition mutex (or INVALID). */
+       struct __cq_fq { struct __dbc *tqh_first; struct __dbc **tqh_last; } free_queue;
+       struct __cq_aq { struct __dbc *tqh_first; struct __dbc **tqh_last; } active_queue;
+ } cq_parts[DB_CURSOR_NPART];
```

Two 2-pointer queue heads (≈32 bytes on LP64) are replaced by an 8-element array of `{db_mutex_t + 2 queue heads}` (≈320 bytes). `struct __db` grows by roughly 288 bytes and **every field after `cq_parts` moves**. `join_queue` is deliberately *not* sharded and stays under `dbp->mutex`.

**`struct __dbc` — field ADDED (breaking):**

```c
+ u_int32_t part;    /* Cursor-queue partition index. */
```
Inserted immediately before the `links` TAILQ entry, so every subsequent `DBC` field shifts.

**`struct __db_txn`** — only flag bits added (`TXN_SNAPSHOT_SAFE`); no field added, no layout change. `TXN_SNAPSHOT`'s comment was reworded ("Snapshot Isolation" → "Snapshot isolation (MVCC) substrate") with no value change.

**`struct __db_env`** — three flag bits added in the `flags` word; no field added, no layout change.

### Methods

Method-pointer names in `db.in` (`(*name)` form): **349 at baseline, 349 at HEAD, `diff` reports identical.** No public method was added, removed or renamed. The new functionality is reached through existing entry points (`set_flags`, `txn_begin`, `cursor`) with new flag bits.

### Other `db.in` change: C++ dbm-alias fix

The historic unprefixed 4BSD `dbm` aliases are now suppressed for C++ in their entirety, not just `delete`:

```c
+#if !defined(__cplusplus)
 #define dbminit(a) ...   #define dbmclose ...
 #define delete(a)  ...   #define fetch(a) ...
 #define firstkey   ...   #define nextkey(a) ...
 #define store(a,b) ...
+#endif
```

Previously only `delete` was guarded, so `store` collided with `std::atomic<>::store` in any C++ TU that pulled in MSVC's `<atomic>`. **This removes 6 macros from the C++ namespace** — a source-compat change for C++ consumers that relied on them (none should).

### Java generated constants

`lang/java/src/com/sleepycat/db/internal/DbConstants.java` (committed generated file) changed only its version triplet: `DB_VERSION_MAJOR 5→2026`, `MINOR 3→0`, `PATCH 28→9` (+4/−3 total). The baseline value `5.3.28` disagreed with the library's 2026.0.x, and `db_javaJNI`'s class-init version handshake means **every `Environment` open threw** until this landed. (The memory on file says the stale value was `5.3.29`; the actual baseline blob says `28`.)

The Java `*Config` classes picked up real accessors in the same range: `EnvironmentConfig.java` +80, `TransactionConfig.java` +44, `CursorConfig.java` +43, `lang/java/libdb_java/db.i` −2.

---

## 7. Build system

### Autoconf (still the reference build)

`dist/configure.ac` +169/−10; `dist/configure` (generated) +3,989/−2,797; `dist/Makefile.in` +1,074/−466; `dist/srcfiles.in` +302/−330; `dist/config.hin` +33/−6; `dist/aclocal/options.m4` +21; `dist/aclocal/mutex.m4` +34/−4.

**K&R / C23 survival.** The tree is deliberately K&R; Autoconf ≥ 2.72's `AC_PROG_CC` probes `-std=gnu23` first, and C23 removed old-style definitions, which turned every definition in the tree into a hard error. The fix pre-seeds `ac_cv_prog_cc_c23=no` / `ac_cv_prog_cc_c11=no`, then *also* strips any `-std=gnu23`/`-std=c23` that Autoconf splices into `$CC` anyway, then pins `-std=gnu99` unless the caller chose a standard. The blanket `CFLAGS="$CFLAGS -Wno-deprecated-non-prototype"` from the baseline was replaced by a cached per-flag probe loop over `-Wno-deprecated-non-prototype` and `-Wno-knr-promoted-parameter`, so the flags are only added where the compiler accepts them and all other warnings survive.

**AIO detection (new).** Four backends probed, used at runtime in preference order io_uring > IOCP > kqueue+aio > POSIX aio > thread-pool:
- `AC_CHECK_HEADER(liburing.h)` + `AC_CHECK_LIB(uring, io_uring_queue_init)` → `HAVE_IO_URING`, appends `-luring`
- `AC_CHECK_HEADER(aio.h)` + `AC_SEARCH_LIBS(aio_read, [rt aio])` → `HAVE_AIO_POSIX`
- `AC_CHECK_MEMBER(struct sigevent.sigev_notify_kqueue)` + `AC_CHECK_DECL(EVFILT_AIO)` → `HAVE_AIO_KQUEUE` (macOS deliberately excluded — has `EVFILT_AIO`, lacks `sigev_notify_kqueue`)
- `AC_CHECK_HEADER(pthread.h)` → `HAVE_AIO_THREADPOOL`

**Entropy (new).** `AC_CHECK_FUNCS(getrandom arc4random_buf)` and `sys/random.h` added to `AC_CHECK_HEADERS`; `os_csprng.c` falls back to `/dev/urandom`.

**Header-dependency tracking (new).** The baseline Makefile listed only `.c` prerequisites per object, so editing a shared header left dependent objects stale — a struct-layout change could then mismatch across TUs. Now probes `-MMD -MP -MT` and, when supported, sets `DEPFLAGS='-MMD -MP -MT $@'` and `DEP_INCLUDE='-include $(wildcard *.d) $(wildcard .libs/*.d)'`; empty otherwise.

**Java version-detection bug fixed.** The baseline glob `1.[3-9]* | 1.[1-9][0-9]* | [2-9]*` rejected every JDK from 10 through 19 (a hole in the middle of the range: 9, 21 and 23 passed, which is why nobody noticed). Replaced with numeric major-version comparison, mapping legacy `1.N` to `N`.

**Three new `--enable` options** (`dist/aclocal/options.m4`), each additive and off by default so a production build is bit-for-bit stock:

| Option | Defines | Effect |
|---|---|---|
| `--enable-dst` | `HAVE_DST` | compiles `$(SIM_OBJS)` in, adds `-I$(topdir)/test/sim`, enables guarded `__os_*` I/O hooks |
| `--enable-faultinject` | `HAVE_FAULT_INJECT` | compiles `$(FI_OBJS)` in, adds `-I$(topdir)/test/faultinject`, enables the `__os_*` allocation hook |
| `--enable-handoff-trace` | `HAVE_HANDOFF_TRACE` | instruments `__log_flush_int`. **Adds a field to the shared LOG region**, so it changes region layout and `env_sig.c`'s signature — by design, an instrumented library refuses to open a stock environment rather than misreading it. Measurement only. |

`dist/Makefile.in` gained `docs`, `docs-check`, `bench`, `leak_tests` and compdb targets, plus ~40 individual `test_sim_*` driver targets and the `leak_*`/`health_stats`/`batch_diff`/`aio_concurrent_sync`/`lock_order_check`/`mvcc_purge_visible` drivers.

### Meson (new, second build system)

12 files:

| File | Role |
|---|---|
| `meson.build` (root, 44 lines) | thin shim: `project('libdb','c', version:'2026.10.1', license:'AGPL-3.0-or-later OR Sleepycat', meson_version:'>=0.56.0', default_options:['c_std=c99','warning_level=1'])`, then `subdir('dist')` |
| `meson_options.txt` | one option: `hegel` (feature, default **disabled**) |
| `dist/meson.build` (649 lines) | the real recipe: `configuration_data()` → `db_config.h`, generated headers, `libdb`, plus `docs`/`bench` run-targets |
| `dist/meson/gen_header.py`, `run_docs.py`, `run_bench.py`, `db_subs.json`, `README-windows.md` | header generation + run-target drivers |
| `test/pbt/meson.build`, `test/sim/meson.build`, `test/tiers/meson.build` | test-tier wiring |
| `test/db/run_meson_autoconf_parity.sh` | **parity gate** between the two builds |

Scope is explicit: Meson builds the core C library on POSIX; Autoconf remains the reference and covers the language bindings (C++, Java, Tcl, SQL) and non-POSIX platforms. Meson test suites: `tiers` (gating), `tiers-diag` (needs DIAGNOSTIC), `tiers-xfail` (reproduces issue #140), `tiers-slow` (soak).

The `dist/meson.build` comments record a concrete class of bug this parity work caught: the two systems must agree on feature macros or "they agree on nothing downstream" — `HAVE_DBM`/`HAVE_ATOMICFILEREAD` had been hardcoded on in Meson while autoconf leaves both off by default, and omitting `sys/random.h` from Meson's header probe left `getrandom(2)` declared nowhere; missing `_GNU_SOURCE`/`_REENTRANT` made `pthread_rwlock_t` an unknown type, silently turning the `HAVE_PTHREAD_RWLOCK_REINIT_OKAY` probe into a false negative.

### CMake

**None.** `git ls-tree -r --name-only origin/master | grep -i cmake` returns nothing at either point.

### Nix

`flake.nix` + `flake.lock` added (136 lines). The dev shell pins the docs toolchain (pandoc, weasyprint, poppler-utils, mandoc, codespell, lychee, write-good) so `.github/workflows/docs.yml` matches local builds via `nix develop`.

### Windows

6 C#-related solution/project files deleted; 28 `build_windows/` files modified (+645/−751). The modifications are overwhelmingly **ARM64 platform configurations** added mechanically by the new `dist/win_arm64_configs.py` (168 lines, byte-level transform preserving CRLF, `--check` mode for CI): every `|x64` construct duplicated as `|ARM64` with `/machine:x64` → `/machine:arm64`. Library + C utilities only; Java/Tcl/SQL/PHP/STL/example projects stay x64-only. `build_windows/db.h` (vendored generated header) +78/−20, tracking the `db.in` changes. `dist/s_windows` +2/−24.

### Android

`build_android/` modified only: `Android.mk` +11/−2, `db.h` +78/−20, `db_config.h` +3/−9, `db_int.h` +12/−8, `clib_port.h` +2/−2, `jdbc/jni/Android.mk` +10/−1. The new path is `dist/android/build_android.sh` (250 lines) — a cross-**build** via the NDK's clang against the root `meson.build`, no emulator or device, gated by `.github/workflows/android.yml`. Default target `aarch64`, API 24; accepts `arm`/`x86_64`/`x86`.

### Static analysis in the build

`dist/cocci/` (15 files, 948 lines) adds Coccinelle semantic patches run by `.github/workflows/cocci.yml`: `rule_lock_mode_enum`, `rule_tret_clobber`, `rule_malloc_leak`, `rule_relaxed_refcount`, `rule_mutex_unbalanced`, `rule_dbassert_arity`, `abi_flagbits`, plus `bdb_defs.h`, a `baseline.txt`, a `kr2ansi.py` and two inventory scripts.

`dist/validate/` scripts were tightened: `s_chk_pubdef` +44/−1, `s_chk_message_id` +29/−3, `s_chk_err` +20/−5, and `s_chk_vxworks` deleted. Also new/changed: `dist/s_execbits` (54, new — guards against the lost-exec-bit failure mode), `dist/env_sig_print.sh` (95, new), `dist/gen_inc.awk` +39, `dist/s_perm` +22/−6, `dist/s_crypto` +20/−16, `dist/s_tags` +15/−3, `dist/s_validate` +14/−3.

---

## Caveats

1. **Rename detection is off** in all tallies, so the 27 genuine renames appear once as `D` and once as `A`. With detection on, `git diff --shortstat` reports 8,100 files / +880,206 / −916,435 versus the 8,127 / +880,226 / −916,455 used here.
2. **`test/coverage/*.info` (634,850 added lines, 4 files) is lcov output**, not authored code. It was not on the exclusion list given, so it is counted in the headline additions; the third top-line table and the test-surface table both break it out.
3. **`test/bench/results/` contains committed benchmark data** (TSV/CSV/stack samples), e.g. a single 5,985-line `tproc-xengine-*.tsv`. These inflate `test/bench`'s 37,947 lines and are measurement artefacts rather than test logic. Not separately excluded.
4. **Whole-repo line counts at each revision are missing** — the `git cat-file --batch` walk over all 9,231 / 5,921 blobs exceeded a 900 s timeout. `src/` and `test/` subtree counts completed and are reported.
5. `docs_src/` was counted at its committed size; the rendered `docs-build/` output is not in the tree, so the docs change is "−764,534 committed HTML, +115,118 committed Markdown" — not a like-for-like content comparison.
6. Two untracked paths exist in the working tree (`test/.results-sabotage/`, `test/c/flagapi-run/`); nothing was modified, and they do not affect any `git diff`/`ls-tree` figure above.
