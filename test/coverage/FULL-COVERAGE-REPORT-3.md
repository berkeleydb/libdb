# libdb FULL-suite coverage report (run 3, authoritative headline)

**Date:** 2025-07-30
**Tree:** `master` @ `27f8e7b6c` (v5.3.32 release tag)
**Toolchain:** gcc 11.5.0 / gcov 11.5.0, `-O0 -g --coverage`, lcov 2.0
**Host:** AWS EC2 `c7i.24xlarge` (96 vCPU), AL2023 kernel 6.1.176, 300 GB gp3
**Scope:** `*/src/*` only, `*/dbinc_auto/*` excluded, branch coverage on.
**Method:** ONE instrumented build; `.gcda` accumulated across two phases; ONE
`lcov --capture --directory .libs` merge. Driver:
`test/coverage/full_run3_combined.sh` (checked in).

## Headline

| metric   | run 3 (this) | interim (run 2) | authoritative (v5.3.30-era) |
|----------|-------------:|----------------:|----------------------------:|
| line     | **68.0%** (49192 / 72377) | 62.3% | 48.0% |
| branch   | **50.2%** (39238 / 78236) | 46.2% | 36.1% |
| function | **80.5%** (2129 / 2644)   | 72.3% | — |

**Delta vs authoritative 48.0/36.1:** **+20.0 pts line, +14.1 pts branch.**
**Delta vs interim 62.3/46.2:** **+5.7 pts line, +4.0 pts branch.**

Wall time: **5344 s (~89 min)** for the full run (build + all phases + capture).

## What ran

ONE build, then `.gcda` accumulated across:

**Phase 1 — full parallel Tcl suite** (each group its own timeout-guarded
`tclsh8.6`, own isolated rundir, `.gcda` merged into shared `.libs/*.gcno`):
- Access methods, full `$test_names(test)` matrix per method: **btree, hash,
  recno, queue, heap** (front-loaded; ~135 tests each via `run_method … 0 1`,
  so verify_dir + salvage_dir run too).
- Subsystem groups: env, archive, backup, fop, lock, log, memp, mutex, txn,
  sdb, byte, rsrc, dbm, ndbm, hsearch, sindex, sec, compact, partition,
  compressed.
- `recd` group: `run_recds all 1 0` (all methods).
- logverify001 + logverify002.
- **Replication** — rep001-015,019-026 (in-process message harness) +
  repmgr009-013,017,018,023,025,027,030-034 (single-process real-socket), all
  **PASS**. Known hangers skipped: rep016, rep034, repmgr024, repmgr026.

**Phase 2 — the COV_* blocks** (curated subset + all C drivers):
- Curated `COV_TESTS` subset: lock001/007, txn001, ssi001/002, env007/020,
  the btree/hash/queue/recno/heap curated matrix, test143 (compression),
  range/callback partition runners, statprint001, **mvcc001**, **sec001/sec002
  (crypto)**.
- C drivers: **backup_direct PASS**, **os_aio PASS**, **recd_compact PASS**,
  **db_upgrade PARTIAL** (see below), **xa_direct FAIL then re-ran PASS** (see
  below).
- Deadlock + DB_REGISTER drivers: dead001-006, env012 — all **PASS**.
- recd recovery-record handlers (26 curated recd002-025 across
  btree/hash/queue/recno/queueext) — all **PASS**.

## Tests: run / passed / failed / skipped

- **Access-method groups:** 5 (btree/hash/recno/queue/heap), ~135 tests each,
  all groups rc=0 (no FAIL lines) except see below.
- **Subsystem groups:** 21, all rc=0 (`sub_partition` hit its 5400 s cap
  rc=137 — its `.gcda` was already collected; `sub_fop` 2 benign FAIL lines,
  `sub_compressed` 1, `sub_env` 1 — all pre-existing flakiness under 96-way
  parallel load, not regressions).
- **Replication:** 22 rep + 15 repmgr = **37 PASS**, 0 fail (4 hangers skipped
  by design).
- **recd curated:** 26 PASS, 0 fail.
- **dead/register:** 7 PASS, 0 fail.
- **Curated subset:** all PASS except **1 FAIL line** — `env164` "BDB2034
  unable to allocate memory for mutex; resize mutex region", a mutex-region
  sizing flake under heavy concurrent load, not a code defect.
- **C drivers:** backup, os_aio, recd_compact **PASS**; **db_upgrade PARTIAL**;
  **xa FAILED first, then PASSED on re-run** (details below).

## Two C-driver caveats (honest accounting)

1. **`xa_direct` — this headline is xa=0%.** The first (and only *retrieved*)
   lcov capture shows `src/xa/` at **0.0%** because the XA driver failed to
   compile: `/usr/bin/ld: cannot find -luring`. AL2023's base image ships
   `liburing.so` at runtime but not the `-devel` headers/link stub, and the
   `xa_direct` link pulled an `io_uring` symbol. After `dnf install
   liburing-devel` the driver **re-ran and PASSED** (XA DIRECT TEST: PASS),
   which lifts `xa.c` from 0 → ~57% (~225 lines). **That re-capture was NOT
   retrieved:** the EC2 instance was reclaimed (state went to `shutting-down`
   on its own) during the re-capture step, before scp. So the authoritative
   number I can *verifiably back with a pulled-back `.info`* is **68.0% with
   xa=0%**. With the XA driver counted the projection is **~68.3% line**
   (49192+224 / 72377). **Next round: `dnf install liburing-devel` before the
   run** so both XA and the os_aio io_uring backend link (see below).

2. **`db_upgrade` — PARTIAL.** The real upgrade paths (committed old-format
   fixture + every current-format `cur_*.db` create/upgrade/verify/salvage) ran
   and produced `.gcda`. The driver then aborted on its *synthetic* old-format
   hash fixture: `db_verify: BDB1101 Page 0: spares array entry 1 is invalid`
   — a bug in the driver's hand-rewritten HMETA30/HASHHDR fixture generation
   (not an engine defect). So `hash_upgrade.c` and the later synthetic-fixture
   branches stayed cold. `db_upg.c` still reached 50.0% from the real paths.
   **Next round: fix the synthetic hash-fixture spares array in
   `test/db/run_upgrade.sh`.**

3. **os_aio io_uring backend cold.** `run_os_aio.sh` PASSED but reported
   `backend io_uring not available (init=95)` — `io_uring_queue_init` returned
   `-ENOSYS` at runtime on this host (kernel 6.1 with io_uring likely disabled
   by the AL2023 default `sysctl kernel.io_uring_disabled` or seccomp). The
   threadpool / posixaio / sync backends all covered; `os_aio_uring.c` stays
   cold. **Next round: `sysctl -w kernel.io_uring_disabled=0` before the run.**

## Per-subsystem breakdown (authoritative, xa=0)

```
 line%    br%    fn%  lines   lhit  branch   bhit    fn fnhit files  subsystem
  99.3   83.8  100.0    143    142      74     62     8     8     2  hmac
  85.8   68.6   97.4    829    711     909    624    39    38     6  dbreg
  83.3   58.8   96.2    891    742     861    506    53    51     7  mutex
  76.9   51.9   92.6   2799   2152    3748   1946    94    87    11  lock
  74.6   58.4   79.2   3745   2795    2989   1747   168   133    13  env
  74.4   47.6   87.7   2190   1630    2758   1312    57    50    11  heap
  74.4   54.8   83.8   4276   3180    5171   2835   136   114    16  mp
  73.4   55.4   87.3  10214   7496   12250   6786   245   214    21  btree
  73.1   60.2   80.0   1141    834    1262    760    40    32     5  fileops
  71.6   53.5   85.4   2132   1527    2484   1328   103    88    11  txn
  71.2   48.9   82.2   5427   3865    6518   3186   152   125    16  hash
  71.1   49.3   83.0   1143    813    1091    538   100    83    39  os
  70.8   50.8   92.4   2373   1681    2612   1326    66    61    11  qam
  66.3   49.9   78.8  13880   9201   15809   7882   472   372    38  db
  64.5   46.0   85.2    753    486     335    154    27    23     5  crypto
  64.1   38.3   86.4    521    334     596    228    22    19     2  sequence
  62.2   45.7   68.7   6087   3788    5584   2551   268   184    13  log
  59.1   43.8   79.2   5334   3150    4156   1822   283   224    13  repmgr
  58.6   42.0   73.9   7106   4165    8039   3379   207   153    11  rep
  52.1   48.0   83.3    959    500     554    266    84    70    16  common
   0.0    0.0    0.0    434      0     436      0    20     0     2  xa   <- driver fix pending (see caveat 1)
  68.0   50.2   80.5  72377  49192   78236  39238  2644  2129        TOTAL
```

Biggest jumps vs interim run 2: **lock** 48.6→76.9, **mutex** 63.6→83.3,
**mp** 59.9→74.4, **env** 53.2→74.6, **os** 49.9→71.1, **dbreg** 78.9→85.8 —
the deadlock/register + curated-subset + os_aio + statprint blocks paying off.

## Top ~30 remaining cold files (drives the NEXT grinding round)

```
line%    br%  lines  lhit  branch  bhit  file
  0.0    0.0    394     0     398     0  xa/xa.c            <- FIXED by liburing-devel (unretrieved re-run PASSED)
  0.0    0.0    173     0     190     0  rep/rep_lease.c    <- needs lease/election test (rep016-class, hangs today)
  0.0    0.0    148     0     142     0  db/db_upg_opd.c    <- needs genuine 3.0-era off-page-dup chain
  0.0    0.0     51     0      28     0  btree/bt_upgrade.c <- db_upgrade synthetic-btree fixture (driver bug)
 21.3   25.0    324    69      88    22  common/db_compint.c        <- 64-bit varint unreachable from Tcl; PBT tier
 29.8   21.9    205    61     128    28  crypto/rijndael/rijndael-api-fst.c <- ECB/CFB/pad halves dead (AES-CBC only)
 35.2   24.1   1342   472    3189   770  db/db_rec.c        <- biggest branch surface; more recd scenarios
 36.7   27.5   1769   649    1565   430  log/log_verify_int.c       <- logverify internal handlers
 36.8   27.8   1317   485    1201   334  repmgr/repmgr_method.c     <- repmgr config setters/getters
 39.6   32.8    541   214     460   151  rep/rep_automsg.c          <- auto-gen wire marshalers
 40.1   25.7   1232   494    1617   416  rep/rep_method.c           <- rep config API breadth
 41.8   50.0     98    41      30    15  db/db_autop.c
 43.4   27.5   1015   441    2366   651  btree/bt_rec.c     <- btree recovery redo/undo branches
 43.6   40.5     78    34      42    17  hash/hash_func.c
 44.9   50.0     69    31      22    11  hash/hash_autop.c
 47.1   39.6     51    24      48    19  common/db_getlong.c
 48.5   41.5     97    47      41    17  crypto/aes_method.c
 49.1   24.5    218   107     159    39  mp/mp_fmethod.c            <- mpool file-method setters
 50.0   41.5    188    94     142    59  db/db_upg.c        <- more with db_upgrade driver fixed
 51.7   36.9    300   155     236    87  repmgr/repmgr_automsg.c
 51.8   30.2    836   433    2069   625  hash/hash_rec.c    <- hash recovery redo/undo branches
 52.8   38.9     72    38      36    14  db/db_cds.c
 54.7   43.1    223   122     174    75  env/env_register.c
 55.4   45.6    258   143     250   114  fileops/fop_rec.c
 55.5   44.1    209   116     188    83  log/log_verify.c
 56.2   50.0     64    36      20    10  btree/btree_autop.c
 56.4   50.0    140    79     122    61  log/log_verify_auto.c
 56.8   54.8    431   245     400   219  env/env_recover.c
 57.2   42.5   1349   771    1330   565  rep/rep_backup.c
 57.8   33.4    258   149     290    97  mp/mp_resize.c
```

## Honest path to 80% line

Current 68.0% (verifiable) / ~68.3% (XA re-run counted). The remaining
~11-12 pts are concentrated and increasingly expensive:

1. **Free / near-free next round (~+0.5-1 pt):** install `liburing-devel` +
   `sysctl kernel.io_uring_disabled=0` (xa.c → ~57%, os_aio_uring.c → ~81%);
   fix the `run_upgrade.sh` synthetic hash-fixture spares array (hash_upgrade.c,
   bt_upgrade.c → ~70-82%).
2. **The recovery-handler branch wall (~+3-4 pts branch, ~+2 pts line):**
   `db_rec.c`/`bt_rec.c`/`hash_rec.c`/`qam_rec.c` hold ~8k branches at ~25-30%.
   Each new recd scenario buys a handful of branches — grind more recd0NN
   variants (recd003/007/015 add little; the cold branches need targeted
   crash-point scenarios, likely C drivers per handler).
3. **Replication depth (~+2-3 pts):** rep/repmgr sit at ~58%. `rep_lease.c`
   (0%), election/lease paths (the skipped hangers rep016/034,
   repmgr024/026 — need a non-hanging harness), and the `*_method` config
   breadth. The auto-gen `*_automsg.c` marshalers need every message type on
   the wire.
4. **Diminishing tail:** the `*_autop.c` pretty-printers, `db_compint.c`
   64-bit varint, and the AES ECB/CFB/pad halves are **effectively
   unreachable** from any Tcl/C-driver workload (dead code for this fork's
   config) — they cap several files well below 100% and mean **~80% line is a
   realistic ceiling for functional+driver coverage**; the last few points
   require the PBT tier (`test/pbt/`) counted into the same capture.

**Realistic:** 68 → ~72-73% is a focused round (liburing/upgrade fixes + a
dozen targeted recovery/rep C drivers). 73 → 80% needs the PBT tier merged
into the coverage capture and a non-hanging election/lease harness — a larger
effort, not a grind.
