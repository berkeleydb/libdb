# libdb — Authoritative Full-Suite Code-Coverage Report

**Measurement only. No engine code was changed.** This is the authoritative
coverage number for the libdb (Berkeley DB 5.3 fork) source tree, produced by
running the standard Tcl regression suite across all access-method families
plus every standalone subsystem group on a dedicated 96-vCPU EC2 instance.

## Headline numbers (`src/` only, generated `dbinc_auto/` excluded)

| Metric    | Coverage | Hit / Total     |
|-----------|---------:|-----------------|
| **Line**     | **48.0%** | 34,705 / 72,363 |
| **Function** | **52.8%** | 1,396 / 2,644   |
| **Branch**   | **36.1%** | 28,241 / 78,211 |

For reference, the repo's prior CI baseline (`test/coverage/baseline.txt`,
a deliberately small representative subset) was line=28.8% / branch=19.0%.
Running the full standard suite roughly **+19 pts line / +17 pts branch**.

## How it was measured

- **Host:** AWS EC2 `c7i.24xlarge` (96 vCPU), AL2023, 300 GB gp3, IMDSv2.
  Instance `i-00b1b5d248535c038`, terminated at end of run.
- **Toolchain:** gcc 11.5.0, gcov 11.5.0, lcov 2.0-1, tcl 8.6.10.
- **Build:** `CC=gcc ../dist/configure --enable-debug --enable-test
  --with-tcl=/usr/lib64 CFLAGS="-O0 -g --coverage" LDFLAGS="--coverage"`
  then `make -j96`. (`CC=gcc` is required — BDB's configure otherwise picks a
  clang-as-cc whose `.gcda` gcc-gcov cannot read.)
- **Suite:** the same group list `run_std` executes, driven group-by-group so a
  hang in one group cannot starve the rest. Each group ran as its own
  `timeout`-guarded `tclsh` subprocess (this is exactly `run_std`'s own
  fork-per-group model) writing to `ALL.OUT`. Ordering front-loaded unique
  coverage: representative access methods first, then every standalone
  subsystem group, then (would-be) redundant methods and replication last.
- **Aggregation:** `lcov --capture` (with `geninfo_unexecuted_blocks=1`,
  `branch_coverage=1`), then `--extract '*/src/*'` and `--remove
  '*/dbinc_auto/*'`. Artifacts in `test/coverage/full-run/`.

### Test groups actually run (all rc=0 unless noted)

- **Access methods (135 tests each):** btree, hash, recno, queue, heap — all
  completed. (`test_names(test)` = 135 standard tests × access method.)
- **Standalone subsystem groups:** env, archive, backup, fop, lock, log, memp,
  mutex, txn, sdb, byte, rsrc, dbm, ndbm, hsearch, sindex — all completed.
  `dead` (deadlock) completed dead001–011 then hit its 50-min per-group timeout
  on the multi-process detector loop (coverage collected). `partition` /
  `compressed` / `recd` ran partially when the 4-hour whole-run ceiling fired.

### Excluded / not reached (honestly stated)

- **Replication: `rep`, `repmgr` (rep_subset / auto_repmgr / other_repmgr /
  multi_repmgr).** Deliberately ordered last (documented multi-process hang
  risk) and not reached before the wall-clock ceiling. Coverage ≈ 0.8%.
  **Update:** most of the replication suite does *not* actually need external
  multi-process orchestration — the `rep0NN` in-process message harness and
  the single-process `repmgrNN` socket tests run in one `tclsh` each. Running
  37 of them lifts `rep/`+`repmgr/` from 0.8% to **~56.6% line / ~41.9%
  branch** (~+9.7 pts of the whole-`src/` headline). See
  `REPLICATION-COVERAGE.md` and `COV_REP=1 run_coverage.sh`. Still cold:
  `rep_lease.c` (lease tests hang), the `rep*script.tcl` subprocess tests, and
  the repmgr 100-series (need the `db_repsite` utility, absent from this fork).
- **XA (`xa/xa.c`).** Not exercised by any group run. Coverage 0%.
- **Redundant access methods** rbtree, frecno, rrecno, queueext were queued
  after the subsystem groups and not reached. Their coverage is near-identical
  to btree/recno/queue (same code paths), so the loss to the *aggregate* number
  is small — but note `queueext`-specific and `rbtree`-specific branches are
  therefore under-counted.

### Wall time

Testing spanned **14:13 → ~19:37 UTC, ≈ 5h23m** of test execution (btree from
the first driver pass, then hash/recno/queue/heap + all subsystem groups in the
second pass). Total instance uptime ≈ 5.5 h.

## Per-subsystem breakdown (line% / branch%)

Sorted best-covered first. `lines`/`branch` are instrumented totals.

| line% | br%  | lines | lhit | branch | bhit | files | subsystem |
|------:|-----:|------:|-----:|-------:|-----:|------:|-----------|
| 96.5 | 79.7 |   143 |  138 |    74 |   59 |  2 | hmac |
| 74.2 | 55.4 |   829 |  615 |   909 |  504 |  6 | dbreg |
| 73.5 | 47.5 |  2190 | 1610 |  2758 | 1309 | 11 | heap |
| 72.3 | 56.0 |  1141 |  825 |  1262 |  707 |  5 | fileops |
| 66.6 | 45.9 |  2132 | 1420 |  2484 | 1141 | 11 | txn |
| 66.5 | 45.1 |  5427 | 3611 |  6518 | 2937 | 16 | hash |
| 63.3 | 42.9 |   891 |  564 |   861 |  369 |  7 | mutex |
| 63.3 | 43.6 |  2373 | 1501 |  2612 | 1138 | 11 | qam (queue AM) |
| 62.9 | 43.3 |   753 |  474 |   335 |  145 |  5 | crypto |
| 62.4 | 45.7 | 13876 | 8657 | 15802 | 7217 | 38 | db (+recno core) |
| 59.4 | 46.7 | 10214 | 6063 | 12250 | 5722 | 21 | btree |
| 59.3 | 40.0 |  2799 | 1661 |  3748 | 1500 | 11 | lock |
| 58.7 | 42.2 |  4276 | 2509 |  5171 | 2184 | 16 | mp (mpool) |
| 52.6 | 30.5 |   521 |  274 |   596 |  182 |  2 | sequence |
| 50.6 | 36.5 |  3741 | 1892 |  2981 | 1089 | 13 | env |
| 48.4 | 37.2 |  1143 |  553 |  1091 |  406 | 39 | os |
| 32.5 | 38.3 |   959 |  312 |   554 |  212 | 16 | common |
| 31.7 | 25.1 |  6081 | 1926 |  5574 | 1400 | 13 | log |
|  0.8 |  0.1 |  5334 |   45 |  4156 |    6 | 13 | repmgr *(excluded)* |
|  0.8 |  0.2 |  7106 |   55 |  8039 |   14 | 11 | rep *(excluded)* |
|  0.0 |  0.0 |   434 |    0 |   436 |    0 |  2 | xa *(excluded)* |
| **48.0** | **36.1** | **72363** | **34705** | **78211** | **28241** | | **TOTAL** |

### Well-covered subsystems
`hmac` (96.5%), `dbreg` (74%), `heap` (73.5%), `fileops` (72%), then the core
data-plane — `txn`, `hash`, `mutex`, `qam`, `crypto`, `db`, `btree`, `lock`,
`mp` — all in a tight **58–67%** band. This is the code the standard AM +
transaction/lock/log suite drives hardest, and it is genuinely well exercised.

### Big-gap subsystems
- **rep (0.8%), repmgr (0.8%), xa (0%)** — replication and XA were not run
  (see exclusions). These three alone are **12,874 instrumented lines** of the
  72,363 total (18%), so they drag the headline number down substantially.
- **log (31.7%)** — the *logging* core (`log_put`, `log_get`, cursor) is
  covered, but `log_verify*` (the `db_log_verify` machinery, ~3,275 lines
  across `log_verify_int.c`, `log_verify_util.c`, `log_verify.c`,
  `log_verify_auto.c`) is **0%** — no group runs `db_log_verify`.
- **common (32.5%)** — `db_compint.c` (compression varint, 324 lines) and
  `os_method.c` are cold; compressed AM would lift `db_compint`.
- **env (50.6%)** — `env_register.c` and `env_backup.c` are 0% (process
  registration / hot-backup init paths not driven by the groups run).

## Top 30 least-covered files (largest cold surfaces first)

From `test/coverage/full-run/cov-ranking.txt` (files ≥50 lines). Every entry
below is **0.0% line coverage**:

| lines | file | note |
|------:|------|------|
| 1769 | log/log_verify_int.c | db_log_verify not run |
| 1064 | rep/rep_record.c | replication excluded |
| 1017 | log/log_verify_util.c | db_log_verify not run |
|  923 | rep/rep_util.c | replication excluded |
|  864 | repmgr/repmgr_sel.c | replication excluded |
|  704 | repmgr/repmgr_net.c | replication excluded |
|  701 | repmgr/repmgr_msg.c | replication excluded |
|  556 | rep/rep_elect.c | replication excluded |
|  541 | rep/rep_automsg.c | replication excluded |
|  394 | xa/xa.c | XA excluded |
|  392 | rep/rep_log.c | replication excluded |
|  324 | common/db_compint.c | compressed AM not reached |
|  319 | rep/rep_stat.c | replication excluded |
|  300 | repmgr/repmgr_automsg.c | replication excluded |
|  292 | rep/rep_verify.c | replication excluded |
|  223 | env/env_register.c | DB_REGISTER path not driven |
|  209 | log/log_verify.c | db_log_verify not run |
|  205 | repmgr/repmgr_elect.c | replication excluded |
|  187 | db/db_upg.c | on-disk upgrade path not driven |
|  173 | rep/rep_lease.c | replication excluded |
|  148 | db/db_upg_opd.c | on-disk upgrade path not driven |
|  141 | os/os_aio_pool.c | async I/O pool not driven |
|  140 | log/log_verify_auto.c | db_log_verify not run |
|  140 | repmgr/repmgr_stat.c | replication excluded |
|  108 | hash/hash_upgrade.c | on-disk upgrade path not driven |
|   92 | sequence/seq_stat.c | seq stat print not driven |
|   85 | os/os_aio_posix.c | async I/O not driven |
|   68 | common/os_method.c | method-table setters not driven |
|   60 | env/env_backup.c | hot-backup init not driven |
|   56 | dbreg/dbreg_stat.c | dbreg stat print not driven |

The theme is unambiguous: the cold surface is **replication + XA + log-verify +
on-disk-upgrade + stat-print + async-I/O**. The data plane itself is warm.

## Tests: run / passed / failed / excluded

- **Run:** 5 access methods × 135 standard tests (btree, hash, recno, queue,
  heap) + 16 fully-completed subsystem groups + partial dead/partition/
  compressed/recd. All 135 distinct standard test IDs executed on at least the
  representative methods.
- **Failed (6 distinct test IDs):** `test046`, `test049`, `test139` (db_dump /
  db_load round-trip: "child process exited abnormally, expected 0 got 1"),
  and `test097`, `test128`, `test131` (`DB_VERIFY_BAD` / verify-salvage of many
  simultaneous or secondary-index databases). No `Fatal`, no crash, no
  driver-level group failure. The 2,030 raw `FAIL:` lines are dominated by
  per-page verify warnings from these few tests (`BDB0534` unreferenced page
  ×860, `BDB0535` zeroed page ×103, `BDB0552` last_pgno ×64) — i.e. a handful
  of tests each emitting hundreds of per-page lines, not thousands of distinct
  failures.
- **Excluded:** replication (`rep`/`repmgr`), XA, and the 4 redundant access
  methods (rbtree/frecno/rrecno/queueext) — not reached before the ceiling.

### Follow-up triage (do NOT fix engine code from this report)

These may be real defects **or** environment artifacts (the build is `-O0
--coverage`, which changes timing/memory; one early failure was literally
`db open: not enough memory`, suggesting a resource/env cause under coverage
instrumentation, not an engine bug). Reproduce on a **non-coverage** build
before concluding anything:

1. `test046` / `test049` / `test139` — `db_dump`→`db_load` round-trip: the
   `db_load` child exits non-zero. Check whether the coverage-instrumented
   `db_load` utility is hitting a resource limit / the dumped format.
2. `test097` — 500 simultaneous databases, `DB_VERIFY_BAD` on many sub-DBs.
   Likely fd/memory pressure under `-O0 --coverage`; retry with normal flags.
3. `test128` / `test131` — secondary-index verify failures. Retry uninstrumented.

## Honest path to 80%

At **48.0%** line coverage the gap to 80% is **+23,185 covered lines** — that
is large, and the arithmetic says the *majority* of it is not "harder tests of
warm code" but **entire cold subsystems**:

1. **Run replication + XA (biggest single lever).** `rep`+`repmgr`+`xa` are
   12,874 instrumented lines at ~0%. If you exclude them from the denominator
   entirely (i.e. "engine without replication"), the number is already
   **58.2%**, and 80% of *that* reduced base needs only ~**+13,000 lines** — so
   simply getting the replication suite to run (it needs multi-process
   orchestration that this run deliberately avoided) is the difference between a
   headline in the high-40s and one in the high-50s, and unlocks the path.
2. **Add a `db_log_verify` group.** ~3,275 cold lines in `log_verify*`; the
   log-verify tooling exists but no standard group drives it. One dedicated
   group would move `log` from 31.7% toward the 60s and add ~1.5 pts overall.
3. **Run the excluded access methods (rbtree/frecno/rrecno/queueext) and the
   `compressed` group to completion.** Small aggregate lift (paths overlap
   btree/recno) but closes `queueext`-specific and `db_compint.c` (324 lines)
   gaps.
4. **On-disk upgrade + stat-print + async-I/O** (`db_upg*`, `hash_upgrade`,
   `*_stat.c`, `os_aio*`) — a few thousand more lines, mostly reachable with
   targeted unit/DST tests rather than the Tcl suite. Good B3/PBT candidates.

**Realistic assessment:** 80% *overall* line coverage is achievable but
requires (a) a working replication/repmgr run and (b) new coverage for
log-verify, on-disk upgrade, and the stat/async surfaces — collectively several
thousand lines of test work beyond the standard suite. **80% of the
non-replication engine** is a much closer target: from 58.2% today, it is
reachable mainly by finishing the excluded AMs/compressed group plus the
log-verify and upgrade groups. The core data plane (btree/hash/qam/db/txn/
lock/mp) is already in the 58–67% band and improving it further is genuine
diminishing-returns branch-hunting.

## Artifacts (in `test/coverage/full-run/`)

- `cov-src.info` — filtered lcov trace (`src/` only, no `dbinc_auto`).
- `cov-ranking.txt` — full per-file ranking, lowest coverage first.
- `cov-subsystems.txt` — the per-subsystem table above.
- `cov-summary.txt` — `lcov --summary` output + subsystem table.

HTML tree and raw `.gcda` were intentionally **not** retained.
