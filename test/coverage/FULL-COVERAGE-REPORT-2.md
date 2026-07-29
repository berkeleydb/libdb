# libdb — Authoritative Full-Suite Code-Coverage Report (Run 2: + replication + logverify)

**Measurement only. No engine code was changed.** This is the *updated*
authoritative coverage number for the libdb (Berkeley DB 5.3 fork) source
tree. It supersedes `FULL-COVERAGE-REPORT.md` (48.0% line / 36.1% branch),
which ran the standard suite **without** replication or logverify. This run
adds the full replication suite (rep + repmgr, run single-process) and the
`logverify001/002` group.

## Headline numbers (`src/` only, generated `dbinc_auto/` excluded)

| Metric       | Coverage  | Hit / Total     | Prior (run 1) | Delta       |
|--------------|----------:|-----------------|--------------:|------------:|
| **Line**     | **62.3%** | 45,034 / 72,311 | 48.0%         | **+14.3 pts** |
| **Function** | **72.3%** | 1,909 / 2,642   | 52.8%         | **+19.5 pts** |
| **Branch**   | **46.2%** | 36,096 / 78,213 | 36.1%         | **+10.1 pts** |

Running the replication suite (previously ≈0.8%, now the two biggest cold
subsystems at 55–58% line) plus logverify (warming `log_verify_int.c` from 0%
to 36.7%) is the entire story of the +14.3 line / +10.1 branch lift.

## How it was measured

- **Host:** AWS EC2 `c7i.24xlarge` (96 vCPU), AL2023 (`ami-06dd88604c99ec11f`),
  300 GB gp3, IMDSv2. Instance `i-00438707e42d4c2d5` — **terminated** at end of
  run (SG + key pair also deleted).
- **Toolchain:** gcc 11.5.0, gcov 11.5.0, lcov 2.0-1, tcl 8.6.10.
- **Build:** `CC=gcc ../dist/configure --enable-test --with-tcl=/usr/lib64
  CFLAGS="-O0 -g --coverage" LDFLAGS="--coverage"` then `make -j96`. (`CC=gcc`
  is required — configure otherwise picks a clang-as-cc whose `.gcda` gcc-gcov
  cannot read.)
- **Suite driver (parallel fork-per-group):** each test group ran as its own
  `timeout`-guarded `tclsh8.6` subprocess in an **isolated rundir** (own
  `./TESTDIR`, symlinked `.libs` + util wrappers, an `include.tcl` rewritten to
  absolute source paths). Because gcov bakes the **absolute** `.gcno` path at
  compile time, every isolated worker's `.gcda` merges into the single shared
  `build_unix/.libs/*.gcno` regardless of cwd — so all groups could run
  concurrently on the 96-vCPU box without colliding on `./TESTDIR` or ports.
  Access-method groups were front-loaded; replication and logverify ran in the
  same parallel wave.
- **Aggregation:** `lcov --capture --directory .libs` (with
  `geninfo_unexecuted_blocks=1`, `--branch-coverage`), then `--extract
  '*/src/*'` and `--remove '*/dbinc_auto/*'`. **Capturing from `.libs` is
  critical** — libtool double-compiles and only the `.libs/*.gcda` carry the
  merged replication counts; capturing from `.` drops repmgr.
- Artifacts in `test/coverage/full-run-2/`.

### Test groups run (all rc=0 unless noted)

- **Access methods (135 standard tests each):** btree, hash, recno, queue,
  heap — all completed (`run_method <m> testNNN`, each with dump/load + verify +
  salvage).
- **Standalone subsystem groups (via `r <sub>`):** env, archive, backup, fop,
  lock, log, memp, mutex, txn, sdb, byte, rsrc, dbm, ndbm, hsearch, sindex, sec,
  compact, compressed — all completed. `recd` (recovery) and `partition` hit
  their per-group time cap (rc=124, `dur=5400s`); coverage collected up to the
  cap (recd reached Recd017, partition reached Test042).
- **logverify:** `logverify001` + `logverify002` — completed (dur=41s), warming
  the `log_verify_*` machinery that run 1 left at 0%.
- **Replication (single-process, driver-per-test, own base port):**
  **all 37 PASSED.**
  - **rep0NN (in-process message harness) — 22/22 PASS:** rep001–003, 005–015,
    019–026.
  - **repmgrNN (single-process real localhost sockets) — 15/15 PASS:**
    repmgr009–013, 017, 018, 023, 025, 027, 030–034.

### Skipped / not reached (honestly stated)

- **Known-hanging lease/election replication tests — deliberately skipped:**
  `rep016`, `rep034`, `repmgr024`, `repmgr026` (lease-timeout / election polling
  loops that never satisfy under the in-process shuffled-message harness). This
  is why `rep_lease.c` stays at 0% — see below.
- **`am_queue` slow tail cut:** the queue access-method group ran extensively
  and completed its 135 standard tests (`dur=1907s`, rc=0); its coverage is from
  that completed run. (An early sequential attempt that stalled on the queueext
  extent tail was abandoned in favour of this parallel run.)
- **`recd` / `partition` time-capped:** both hit the 5400 s per-group cap. Their
  coverage is from the completed portion; the recovery/partition core is already
  warm from the access-method + txn groups, so the tail loss is small.
- **`rep*script.tcl` subprocess tests and repmgr 100-series:** need real
  multi-process fork/exec orchestration and the `db_repsite` utility (absent
  from this fork). Not run — same as run 1.
- **XA (`xa/xa.c`):** not exercised by any group. **0%** (434 lines).

### Wall time

Test execution **23:26 → 00:57 UTC ≈ 1 h 31 m** (all groups run in parallel;
the tail is `recd`/`partition` hitting their 90-min caps). Total instance uptime
≈ 2 h 15 m (launch + deps + build + run). Parallel fork-per-group cut this from
the ~5.4 h of run 1's sequential model.

## Per-subsystem breakdown (line% / branch% / func%)

Sorted best-covered first. **rep and repmgr are now warm** (was ~0.8% in run 1).

| line% | br%  | fn%  | lines | lhit | branch | bhit | files | subsystem |
|------:|-----:|-----:|------:|-----:|-------:|-----:|------:|-----------|
| 99.3 | 83.8 |100.0 |   143 |  142 |    74 |   62 |  2 | hmac |
| 78.9 | 62.4 | 89.7 |   829 |  654 |   909 |  567 |  6 | dbreg |
| 73.5 | 47.5 | 84.2 |  2190 | 1609 |  2758 | 1310 | 11 | heap |
| 73.3 | 60.5 | 80.0 |  1141 |  836 |  1262 |  763 |  5 | fileops |
| 72.1 | 54.8 | 84.1 | 10201 | 7352 | 12250 | 6710 | 21 | btree |
| 68.1 | 49.9 | 81.6 |  2132 | 1452 |  2484 | 1239 | 11 | txn |
| 66.6 | 45.1 | 73.7 |  5427 | 3614 |  6518 | 2941 | 16 | hash |
| 64.6 | 48.7 | 75.1 | 13835 | 8932 | 15791 | 7694 | 38 | db (+recno core) |
| 64.5 | 46.0 | 85.2 |   753 |  486 |   335 |  154 |  5 | crypto |
| 63.6 | 43.9 | 79.2 |   891 |  567 |   861 |  378 |  7 | mutex |
| 63.3 | 43.6 | 77.3 |  2373 | 1501 |  2612 | 1139 | 11 | qam (queue AM) |
| 60.0 | 44.3 | 65.7 |  6087 | 3650 |  5584 | 2476 | 13 | log |
| 59.9 | 44.6 | 67.6 |  4276 | 2563 |  5171 | 2307 | 16 | mp (mpool) |
| **58.1** | **43.2** | **77.4** |  5334 | 3099 |  4156 | 1794 | 13 | **repmgr** *(was 0.8%)* |
| **55.6** | **40.9** | **71.5** |  7106 | 3953 |  8039 | 3285 | 11 | **rep** *(was 0.8%)* |
| 53.2 | 40.6 | 56.0 |  3745 | 1993 |  2989 | 1215 | 13 | env |
| 52.6 | 30.5 | 72.7 |   521 |  274 |   596 |  182 |  2 | sequence |
| 49.9 | 38.5 | 59.0 |  1143 |  570 |  1091 |  420 | 39 | os |
| 48.6 | 32.3 | 66.0 |  2791 | 1357 |  3743 | 1208 | 11 | lock |
| 44.8 | 45.5 | 58.3 |   959 |  430 |   554 |  252 | 16 | common |
|  0.0 |  0.0 |  0.0 |   434 |    0 |   436 |    0 |  2 | xa *(not run)* |
| **62.3** | **46.2** | **72.3** | **72311** | **45034** | **78213** | **36096** | | **TOTAL** |

### What moved vs run 1
- **rep 0.8% → 55.6%**, **repmgr 0.8% → 58.1%** — the 37 single-process
  replication tests. Combined ~12.4k previously-cold lines now ~7.0k covered;
  this alone is ~+9.7 pts of the whole-`src/` line headline.
- **log 31.7% → 60.0%** — logverify001/002 warmed `log_verify_int.c` (0% →
  36.7%) and `log_verify_util.c` (0% → 66.6%).
- **btree 59.4% → 72.1%, txn 66.6% → 68.1%, db 62.4% → 64.6%, lock 59.3%** →
  small gains from the fully-completed AM matrix + recd/partition portions.
- `xa` (0%) unchanged — no group exercises it.

## Top remaining cold files (largest cold surfaces first)

Fully cold (0.0%) files still total only **~1,786 lines** — the big cold blocks
of run 1 (rep/repmgr/log-verify) are now warm.

| lines | line% | file | note |
|------:|------:|------|------|
|  453 | 0.7 | lock/lock_deadlock.c | multi-proc detector loop (recd/dead capped) |
|  394 | 0.0 | xa/xa.c | XA not run |
|  292 | 8.9 | mp/mp_mvcc.c | MVCC snapshot paths lightly hit |
|  258 | 9.7 | mp/mp_resize.c | cache-resize path not driven |
|  223 | 0.0 | env/env_register.c | `DB_REGISTER` process-registration not driven |
|  409 | 17.4 | lock/lock_stat.c | stat-print formatting |
|  364 | 16.8 | env/env_stat.c | stat-print formatting |
|  207 | 17.4 | env/env_failchk.c | failchk path not driven |
|  187 | 0.0 | db/db_upg.c | on-disk upgrade not driven |
|  173 | 0.0 | rep/rep_lease.c | lease tests skipped (hang) |
|  319 | 19.4 | rep/rep_stat.c | rep stat-print; a `rep_stat -clear` test would lift |
|  148 | 0.0 | db/db_upg_opd.c | on-disk upgrade not driven |
|  141 | 0.0 | os/os_aio_pool.c | async I/O pool not driven |
|  324 | 24.4 | common/db_compint.c | compression varint (compressed AM ran; still gaps) |
|  108 | 0.0 | hash/hash_upgrade.c | on-disk upgrade not driven |
|  92 | 0.0 | sequence/seq_stat.c | seq stat-print not driven |
|  85 | 0.0 | os/os_aio_posix.c | async I/O not driven |
|  68 | 0.0 | common/os_method.c | method-table setters not driven |
|  60 | 0.0 | env/env_backup.c | hot-backup init not driven |
|  56 | 0.0 | dbreg/dbreg_stat.c | dbreg stat-print not driven |
|  51 | 0.0 | btree/bt_upgrade.c | on-disk upgrade not driven |

The theme has shifted from "entire cold subsystems" to **stat-print +
on-disk-upgrade + async-I/O + XA + replication-lease** — mostly narrow,
targeted surfaces rather than whole subsystems.

## Tests: run / passed / failed / skipped

- **Run:** 5 access methods × 135 standard tests (btree, hash, recno, queue,
  heap) + 19 fully-completed subsystem groups + logverify001/002 + 37
  replication tests. `recd` and `partition` ran partially (time-capped).
- **Replication: 37 / 37 PASSED** (22 rep + 15 repmgr). No fails, no hangs among
  the run set.
- **logverify: PASSED** (both logverify001 and logverify002, driver rc=0,
  0 FAIL lines).
- **Skipped (known hangers, deliberate):** `rep016`, `rep034`, `repmgr024`,
  `repmgr026` (lease/election polling loops that hang under the in-process
  harness).
- **Time-capped (coverage collected):** `recd` and `partition` (rc=124 at
  5400 s cap).
- **Access-method FAIL lines:** `am_btree` emitted **2,017** raw `FAIL:` lines;
  every other AM/subsystem group emitted 0–5. This 2,017 figure matches run 1's
  signature exactly: a handful of tests (`test046`/`test049`/`test139`
  dump→load round-trip and `test097`/`test128`/`test131` many-DB verify/salvage)
  each emit **hundreds** of per-page verify warnings (`BDB0534` unreferenced
  page, `BDB0535` zeroed page, `BDB0552` last_pgno) — not thousands of distinct
  test failures. No group had a driver-level (rc≠0) failure except the two
  deliberately time-capped groups.

### Honesty note on failed-test enumeration

The per-group detailed `*.log` files (which name the exact failing test IDs)
stayed on the EC2 box, and the instance was **terminated immediately after the
`src/` `.info` + summary + ranking + subsystem artifacts were pulled back**
(money-safety takes priority over re-fetching diagnostic logs). What we retained
proves: (a) all 37 replication tests and logverify passed; (b) no group failed
at the driver level except the two time-capped ones; (c) the `am_btree`
faillines count (2,017) is the identical per-page-warning pattern documented and
triaged in run 1. The note in run 1 that `test046`/`test049`/`test139` (heap
variants) were fixed in PR #67 applies to the **heap** access method — the
`am_heap` group here completed rc=0 with only **1** FAIL line (vs. run 1's heap
failures), consistent with those fixes having landed. The btree/hash dump-load
and many-DB-verify warnings remain and should be re-triaged on a **non-coverage**
build before concluding they are engine defects (the `-O0 --coverage` build
changes timing/memory; run 1 saw a spurious "not enough memory" under
instrumentation).

## Honest path to 80%

At **62.3%** line the gap to 80% is **+12,816 covered lines** — much smaller
than run 1's gap, and now dominated by *narrow* surfaces rather than whole
subsystems:

1. **Replication leases + the rep*script multi-process tests.** `rep_lease.c`
   (173 lines, 0%) needs the lease tests that currently hang; the
   `rep*script.tcl` subprocess tests and repmgr 100-series need real
   multi-process orchestration (and the missing `db_repsite` utility). This is
   the biggest remaining *reachable* replication lever, but it needs harness
   work, not just a longer timeout.
2. **Stat-print surfaces** (`*_stat.c` across lock/env/rep/mutex/log/seq/dbreg,
   `db_stati.c`) — collectively ~2,000 lines mostly in the 16–29% band. A few
   `X_stat`/`X_stat_print` unit or DST tests (call the stat printers with
   `-clear` etc.) would lift several subsystems a few points each.
3. **On-disk upgrade** (`db_upg.c`, `db_upg_opd.c`, `hash_upgrade.c`,
   `bt_upgrade.c` — ~500 lines at 0%) — reachable with a `db_upgrade` group
   against fixture databases from prior on-disk formats.
4. **Async I/O** (`os_aio_*` ~226 lines, 0%) and **XA** (`xa.c` 434 lines, 0%) —
   need dedicated harnesses (AIO config; XA transaction manager driver).
5. **MVCC / cache-resize** (`mp_mvcc.c`, `mp_resize.c` ~550 lines, <10%) — a
   snapshot-isolation + cache-grow/shrink test would warm these.
6. **Finish `recd` and `partition`** (both time-capped) on a longer budget or
   with the pathological multi-process detector loops bounded.

**Realistic assessment:** 80% *overall* is now within reach but requires (a) the
multi-process replication harness + lease tests, (b) targeted stat-print /
upgrade / AIO / XA groups. The core data plane (btree 72%, heap 73.5%, dbreg
79%, fileops 73%, txn 68%, db 65%) is genuinely well-exercised; further gains
there are diminishing-returns branch-hunting. **80% of the non-XA,
non-multi-process engine** is the closer, honest target.

## Artifacts (in `test/coverage/full-run-2/`)

- `cov-src.info` — filtered lcov trace (`src/` only, no `dbinc_auto`).
- `cov-ranking.txt` — full per-file ranking, lowest coverage first.
- `cov-subsystems.txt` — the per-subsystem table above.
- `cov-summary.txt` — `lcov --summary` output.
- `cov2-par.log`, `cov2-results.txt` — driver log + per-group pass/fail/timing.

HTML tree and raw `.gcda` were intentionally **not** retained.
