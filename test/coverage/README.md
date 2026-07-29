# Code coverage (Tier B3)

SQLite's testing ethic is **100% MC/DC branch coverage**. libdb is nowhere near
that yet — but per Tier B3 of `.agents/test-suite-maturity-plan.md` the point is
to **measure** branch coverage now, aim new DST/PBT/unit tests at the uncovered
branches, and ratchet the number upward over time. This directory holds the
measurement machinery.

## What it does

`run_coverage.sh` builds libdb with gcov instrumentation
(`CFLAGS="-O0 -g --coverage"`, `LDFLAGS="--coverage"`), runs a bounded,
representative subset of the Tcl regression suite, then aggregates with `lcov`
into a line + branch coverage summary, an HTML report, and a ranked
least-covered-files list.

## Run it locally

```sh
# from the repo root, inside the nix dev shell (has gcc/gcov/tclsh; pulls lcov
# via `nix run nixpkgs#lcov` automatically):
nix develop --command test/coverage/run_coverage.sh

# or, outside nix, with lcov + tcl installed from your distro:
test/coverage/run_coverage.sh
```

Knobs (all optional env vars):

| Var          | Default                                   | Meaning                          |
|--------------|-------------------------------------------|----------------------------------|
| `COV_TESTS`  | core `test:arg` pairs + a `method/test` access-method matrix (see script) | tests to run |
| `COV_JOBS`   | `nproc`                                   | build parallelism                |
| `TCL_LIB`    | autodetected (nix store or `/usr/lib/tcl8.6`) | Tcl lib dir for `--with-tcl` |
| `COV_TIMEOUT`| `2400`                                    | seconds ceiling for the test run |
| `COV_REP`    | `0`                                       | set `1` to also run the replication (rep/repmgr) tests |
| `COV_XA_UPG` | `1`                                       | run the XA + on-disk-upgrade drivers (fast, non-hanging) |
| `COV_BACKUP` | `1`                                       | run the hot-backup-API + compaction-recovery drivers (fast, non-hanging) |
| `COV_DEAD_REG` | `1`                                     | run the deadlock-detector + DB_REGISTER multi-process tests |

Set `COV_REP=1` to add the replication suite (biggest cold surface: rep/ +
repmgr/ ~= 12.4k lines at 0.8%). It moves them to ~56% line / ~42% branch. Those
tests each run in their own `tclsh` (driver-per-test, per-test timeout) because a
few election/lease tests hang; see `REPLICATION-COVERAGE.md` for the exact set,
the measured lift, and what still needs real multi-process orchestration
(`rep*script.tcl` subprocess tests, repmgr 100-series needing `db_repsite`).

`COV_XA_UPG` (on by default) runs two non-Tcl drivers that light up subsystems
the Tcl suite never reaches. Both self-clean their home dir and run under a hard
timeout, so they cannot hang:

- **XA** — `test/xa/run_xa_direct.sh` builds `test/xa/xa_direct.c`, a
  **Tuxedo-free** transaction manager that drives `db_xa_switch`
  (`src/xa/xa.c` + `xa_map.c`) directly: `xa_open/start/end/prepare/commit/
  rollback/recover/forget` plus the internal two-phase-commit path and the XA
  recovery scan (`__txn_get_prepared`, DB_FIRST/DB_NEXT). The full `chk.xa`
  harness needs an Oracle Tuxedo install (`atmi.h`, `tmboot`, …) which is not
  available; this driver needs none. Lift: `xa.c` **0%→~57%**, `xa_map.c`
  **0%→~78%**.
- **On-disk upgrade** — `test/db/run_upgrade.sh` runs the `db_upgrade`
  utility (which calls `DB->upgrade`) + `db_verify` over Berkeley DB files of
  many on-disk versions. It uses the one committed old-format fixture
  (`test/csharp/bdb4.7.db`, a btree meta version-9 db) plus freshly-created
  current-format dbs of every access method, and -- since this fork lacks the
  upstream Tcl `upgrade` per-version fixture tree
  (`test/tcl/upgrade/databases/`) -- it manufactures old-format fixtures
  *without an old library* by rewriting the metadata page of a current db into
  the byte layout of an older release (BTMETA2X / BTMETA30 / HASHHDR / HMETA30
  / QMETA30 / QMETA31 from `dbinc/db_upgrade.h`) with the version field set
  back. Lift: `qam_upgrade.c` **0%→~97%**, `hash_upgrade.c` **0%→~68%**,
  `bt_upgrade.c` **0%→~82%**, `db_upg.c` **0%→~57%** (the whole magic/version
  dispatch + real per-version transforms).
  - `db_upg_opd.c` **still 0%**: the 3.0→3.1 off-page-duplicate conversion
    (`__db_31_offdup`) needs a genuine 3.0-era off-page-duplicate page CHAIN
    (linked `__P_DUPLICATE` pages). That cannot be produced by rewriting a
    current db, because current off-page dups are already stored as a Recno
    tree (P_LRECNO/P_IRECNO), not a flat page chain. A genuine pre-3.1 fixture
    is required.
- **Async I/O backends (os_aio)** — `test/os/run_os_aio.sh` builds
  `test/os/os_aio_direct.c`, which links the internal libdb symbols and drives
  the async-I/O abstraction (`src/os/os_aio.c`) and its backends
  (`os_aio_pool.c`, `os_aio_posix.c`, `os_aio_uring.c`) directly. The buffer
  pool reaches os_aio ONLY via `DB_ENV->set_flags(DB_MPOOL_AIO)` (off by
  default) and then probes a SINGLE backend at runtime in preference order
  (io_uring > IOCP > kqueue+aio > POSIX aio > thread-pool), so on a Linux box
  with liburing a normal Tcl workload can only ever light up io_uring -- the
  pool + posix backends stay dark. The driver forces EACH configured backend's
  `__os_aio_*_init` in turn (submit N writes, reap, read back, verify the
  round-trip), then exercises `__os_aio_create`'s automatic selection and the
  synchronous fallback, and finally runs a real `DB_MPOOL_AIO` checkpoint
  workload (tiny cache + 4000 puts + forced checkpoints) so the production
  `mp_sync` -> `__memp_bhwrite_async` -> backend -> `__memp_aio_drain` path
  runs too. It also sets/clears every `DB_ENV` os-method function
  (`common/os_method.c`). Lift: `os_aio.c` **0%→~84%**, `os_aio_pool.c`
  **0%→~73%**, `os_aio_posix.c` **0%→~84%**, `os_aio_uring.c` **0%→~81%**,
  `common/os_method.c` **0%→100%**.
  - `os_aio_iocp.c` (Windows IOCP) and `os_aio_kqueue.c` (BSD kqueue+aio)
    **stay 0%**: on Linux they compile only their `#else` init stub (empty
    translation unit); they need their target OS to be reachable.
  - **`__db_set_lastpgno` off-by-one (finding, not fixed):** the btree v6/v7
    upgrade path calls `__db_set_lastpgno()`, which stores `__db_lastpgno()`'s
    result -- the page COUNT (`bytes/pagesize`) -- directly into
    `meta->last_pgno`, the last page NUMBER (`count-1`). On a page-aligned file
    that is off by +1, so `db_verify` reports `last_pgno is not correct: N !=
    N-1` (`db_vrfy.c`, under `HAVE_FTRUNCATE`). The hash path avoids it because
    its v8→v9 pass runs through mpool, which recomputes `last_pgno` correctly
    on close. The driver therefore upgrades the btree v6/v7 fixtures (to cover
    the transform code) but skips `db_verify` on them, asserting the metadata
    version bumped instead. `__db_set_lastpgno` is identical to upstream
    Berkeley DB 4.7/4.8, so this is likely a long-standing latent defect; the
    fixtures here are synthetic, so it is reported to confirm against a genuine
    old fixture, not fixed. See `DB-REPSITE-TODO.md` for the analogous repmgr gap.

## The hot-backup API + compaction-recovery surface (`COV_BACKUP`)

`COV_BACKUP` (on by default) runs two more standalone C drivers, same
self-clean + hard-timeout shape as the XA/upgrade drivers, aimed at code the
Tcl suite structurally cannot reach:

- **Hot-backup API** — `test/backup/run_backup_direct.sh` builds
  `test/backup/backup_direct.c`. `env/env_backup.c` is *only* the four backup
  config setters/getters (`set/get_backup_config` for `READ_COUNT`/
  `READ_SLEEP`/`SIZE`/`WRITE_DIRECT`) and the backup-callback setter/getter
  (`set/get_backup_callbacks`). The Tcl `backup.tcl` test drives hot backup
  only through the `db_hotbackup` utility, which calls `DB_ENV->backup()` with
  a **NULL** `backup_handle` — so none of `env_backup.c` runs and the
  `backup->open/write/close` callback branches of `db/db_backup.c` stay cold.
  The driver calls those public entry points directly (as an embedding app
  would): getters-before-alloc (the `EINVAL` branches), all four config enums,
  `WRITE_DIRECT` on **and** off (both `F_SET`/`F_CLR`), then installs write
  callbacks and runs `DB_ENV->backup()` + `DB_ENV->dbbackup()` so the callback
  copy path executes. Lift: `env_backup.c` **0%→~97% line / ~82% branch**,
  plus `db/db_backup.c`'s callback path (**~43% line / ~30% branch**).
- **Compaction recovery** — `test/db/run_recd_compact.sh` builds
  `test/db/recd_compact.c`. Three recovery handlers in `db/db_rec.c` —
  `__db_merge_recover`, `__db_pgno_recover`, `__db_pg_trunc_recover` (~330
  lines) — fire only when btree **compaction** / page **truncation** log
  records are replayed, and no `recd0NN` test runs compaction under recovery,
  so they were completely cold. The driver fills a small-page btree, deletes a
  large contiguous range to leave sparse/empty pages, runs
  `DB->compact(DB_FREE_SPACE)` in a txn (logging `__db_merge`/`__db_pgno`/
  `__db_pg_trunc` records), then re-opens under `DB_RECOVER_FATAL`
  (catastrophic recovery replays the whole log from the start → the redo /
  forward-roll branches of those handlers). It verifies the db still opens and
  reads back afterward. Combined with `recd002:btree` (splits) + `recd016`
  (catastrophic recovery), both added to the default `COV_TESTS`, this lifts
  `db/db_rec.c` from **18%→~27% line / ~13%→~18% branch** in the subset
  (merge/pgno/pg_trunc/relink no longer cold).

## The statistics (`*_stat_print`) surface

The verbose `stat_print` / `DB_STAT_ALL` formatters (`env_stat.c`,
`lock_stat.c`, `rep_stat.c`, `db_stati.c`, `log_stat.c`, `mut_stat.c`,
`seq_stat.c`, `dbreg_stat.c`, `heap_stat.c` …) were ~0-29% covered: functional
tests call `stat()` for values but almost never `stat_print(DB_STAT_ALL)`. Two
Tcl tests now drive the whole surface (both in the default `COV_TESTS`):

- **`env020`** (already registered, was just never in the coverage subset)
  exercises every Tcl `*_stat_print` binding
  (`env`/`lock`/`log`/`mpool`/`mutex`/`txn`/`rep`/`repmgr`/`db`/`seq`) with
  each flag (default, `-clear`, `-all`, `-subsystem`, `-lk_*`, `-hash`, …).
- **`statprint001`** (new) closes the two spots `env020` misses and adds the
  `db_stat` **utility** entry path:
  - `heap_stat.c` — `env020` opens no heap DB.
  - `dbreg_stat.c`'s `__dbreg_print_all` — reached only with
    `DB_STAT_ALL | DB_STAT_SUBSYSTEM` set *together* and databases open;
    `env020` passes those flags separately.
  - a `db_stat` flag sweep (`-e -E -c -C -l -L -m -M -x -X -r -R -t -Z -d -f`)
    over a populated all-subsystems env, driving `util/db_stat.c ->
    __*_stat_print` through the read-only on-disk path.

  Measured lift (env020 + statprint001 vs. the full-suite baseline in
  `full-run-2/cov-ranking.txt`):

  | file | before | after |
  |------|-------:|------:|
  | `env/env_stat.c`     | 16.8% | **79.9%** |
  | `lock/lock_stat.c`   | 17.4% | **73.6%** |
  | `rep/rep_stat.c`     | 19.4% | **72.4%** |
  | `db/db_stati.c`      | 22.0% | **61.8%** |
  | `log/log_stat.c`     | 22.9% | **87.1%** |
  | `mutex/mut_stat.c`   | 29.4% | **86.8%** |
  | `sequence/seq_stat.c`| 0.0%  | **63.0%** |
  | `dbreg/dbreg_stat.c` | 0.0%  | **69.6%** |
  | `heap/heap_stat.c`   | 0.0%  | **80.3%** |
  | `qam/qam_stat.c`     | 57.6% | **78.0%** |
  | `txn/txn_stat.c`     | 56.8% | **69.0%** |

  Still cold: `seq_stat.c`'s `__seq_print_all` (a no-op stub, and the Tcl seq
  binding exposes only `-clear`, not `-all`); `repmgr_stat.c` needs a live
  repmgr transport (`repmgr009`+, `COV_REP=1`) for its send-path counters.

`COV_DEAD_REG` (on by default) runs the two multi-process suites that reach the
deadlock detector and the process-registry crash/recovery path. Like `COV_REP`,
they run driver-per-test with a per-test timeout and orphan-worker cleanup
(they reset `TESTDIR` and each spawns child `tclsh` via `wrap.tcl`, so a hung
worker must not wedge the whole run):

- **Deadlock detector** — the `dead` group (`dead001`–`dead006`) spawns
  `ddscript.tcl` workers that grab locks in a ring/clump cycle; the detector
  (`src/lock/lock_deadlock.c`, `__lock_detect`) then picks a victim. `dead002`/
  `dead003` set `-lock_detect` so detection runs **in-process** in each worker
  (which flushes its own `.gcda`); `dead001`/`dead004`/`dead005` use the
  standalone `db_deadlock` utility. Proc counts are trimmed to `{2 4}` (`{4}`
  for `dead005`) — the full `{2 4 10}` matrix adds only minutes, no new lines.
  Lift: `lock_deadlock.c` **0.7%→~66%**.
- **DB_REGISTER** — `env012` (with `env007`) uses `envscript.tcl` to open the
  env with `DB_REGISTER`, "crash" a process (kill without close), then reopen
  with `-recover`/`-failchk` so the survivor detects the dead slot
  (`src/env/env_register.c`, `__envreg_register`/`__envreg_isalive`) and runs
  recovery. Lift: `env_register.c` **0%→~55%**.

## MVCC freeze/thaw + cache resize (`mvcc001`)

`mp/mp_mvcc.c` and `mp/mp_resize.c` sat near-cold (functional baseline ~9%)
because the suite uses multiversion DBs but never applies the *cache pressure*
that spills old page versions to disk, nor grows the cache after open.
`mvcc001` (COV group `mvcc`, in the default `COV_TESTS`) forces both:

- **freeze/thaw** (`mp_mvcc.c`): a tiny 512K multiversion cache, two
  long-lived `txn -snapshot` readers pinning the original versions, and a
  writer churning every page — old versions spill to `__db.freezer.*` (freeze)
  and are read back when the readers touch them (thaw). Asserts the readers
  still see their snapshot and the `Buffers frozen`/`Buffers thawed` counters
  are non-zero. Lift: **mp_mvcc.c 0%→~70%**.
- **cache growth** (`mp_resize.c`): a small 2-region cache with a larger
  `cache_max`, grown in steps via `resize_cache`, re-hashing buffers into new
  regions; the data is re-verified after each grow and an over-max resize is
  asserted to fail cleanly. Lift: **mp_resize.c 0%→~58%**.

The cache **shrink** path is intentionally *not* exercised: it SIGSEGVs on an
off-by-one region index in `__memp_remove_region`. See
[`MVCC-RESIZE-COVERAGE.md`](MVCC-RESIZE-COVERAGE.md) for the stack, root cause,
and reproduction.

## Encryption (`sec001` + `sec002`)

`crypto/` + `hmac/` sat cold in the subset because **no other subset test opens
an encrypted env or db** -- AES page/log encryption, the HMAC-SHA1 password /
checksum path, and the mt19937 IV generator (`__db_generate_iv`) only run when
`DB_ENV->set_encrypt(passwd, DB_ENCRYPT_AES)` + `DB->set_flags(DB_ENCRYPT)` are
in play. `sec001` and `sec002` (both now in the default `COV_TESTS`, both
already-registered Tcl tests -- no new Tcl written) drive that whole path:

- **`sec001`** -- the encryption *interface*: create/open/join an encrypted
  env + db, `DB_ENCRYPT_ANY`, and every failure branch (empty password,
  algorithm-not-supplied, joining a non-encrypted env with a key and vice
  versa, wrong-length password, **wrong password**, opening an encrypted db
  with no key). These light up `crypto.c`'s cipher setup + the auth-failure
  branches and `aes_method.c`'s key-derivation.
- **`sec002`** -- the page-encryption *round-trip* and *tamper* paths:
  encrypted put/get across pages (AES CBC block encrypt/decrypt +
  IV generation), then scribbling on the meta page / swapping a root page and
  reopening -- driving the HMAC-SHA1 `metadata page checksum error` and the
  `checksum error` -> `DB_RUNRECOVERY` branches.

Measured lift (subset baseline -> with sec001+sec002):

| file | before | after (line% / br%) |
|------|:------:|:------:|
| `crypto/mersenne/mt19937db.c` | 0.0 | **95.6 / 56.2** |
| `hmac/hmac.c`                 | 10.4 | **91.0 / 80.0** |
| `crypto/rijndael/rijndael-alg-fst.c` | 18.8 | **83.5 / 55.0** |
| `crypto/crypto.c`             | 50.7 | **77.9 / 60.5** |
| `crypto/aes_method.c`         | 27.8 | **44.3 / 36.6** |
| `crypto/rijndael/rijndael-api-fst.c` | 6.3 | **29.8 / 21.9** |
| `hmac/sha1.c`                 | 97.4 | **98.7 / 72.7** |

`rijndael-api-fst.c` caps at ~30% *by design*: `aes_method.c` only ever calls
`__db_blockEncrypt`/`__db_blockDecrypt` with `MODE_CBC`, so the file's ECB and
CFB1 branches and the entire `__db_padEncrypt`/`__db_padDecrypt` /
`__db_cipherUpdateRounds` halves are **dead code from BDB's point of view** --
unreachable by any Tcl workload. Similarly, `aes_method.c`'s remaining cold
lines are the `HAVE_CRYPTO_IPP` alternate-backend blocks (this build is not
IPP) and the `__aes_err` message table + `EAGAIN` branches, which fire only if
an internal cipher call returns a negative error (bad key length / bad cipher
state) -- states not reachable through the public API without fault injection.

(`run_secmethod` / `run_secenv`, which run a full access-method test *twice*
under encryption, add only ~1-2 pp over sec001+sec002 -- not worth ~doubling
the subset runtime, so they are left out.)
## Lock configuration + region growth + the varint codec (`lock007`, `test143:btree`)

Three cold files, all added to the default `COV_TESTS`:

- **`lock/lock_method.c`** (was ~22%): the `DB_ENV` lock-subsystem config
  setters/getters. Cold because the rest of the suite runs with default lock
  sizing and detection. **`lock007`** (COV group `lock`) sets every knob
  before open — `set_lk_max_locks`/`lockers`/`objects`, `set_lk_partitions`,
  `set_lk_tablesize`, the `DB_MEM_LOCK`/`LOCKER`/`LOCKOBJECT` init counts, and
  `set_lk_detect` for all nine deadlock-detection policies (plus a rejected
  bogus policy) — then reads them back through the getters. Lift:
  **lock_method.c 10%→~38%** (isolated subset; both the `ENV_ILLEGAL_AFTER_OPEN`
  set path and the `LOCKING_ON` region get path are hit).
- **`lock/lock_alloc.incl`** (was ~27%): the lock-region object/locker/lock
  allocator, `#include`d into `lock_region.c`. Cold because default runs never
  exhaust the initial free lists. `lock007`'s Part c allocates 200 lockers ×
  40 distinct read-lock objects, exhausting the free lists and forcing the
  region-growth loop (`__env_alloc` + free-list refill). Lift:
  **lock_alloc.incl 14%→~82%**.
- **`common/db_compint.c`** (was ~24%): the compressed-integer (varint) codec
  used by btree compression (`bt_compress.c`). Cold in the subset because no
  default test opens a `-compress` btree. **`test143:btree`** (COV group
  `test`) stores records whose *data sizes* span the codec's 1-byte, 2-byte
  and 3-byte size classes (1 B … 100 KB) into a `-compress` btree and reads
  them back, driving `__db_compress_int` on write and `__db_decompress_int32`
  on read, plus the key prefix/suffix length compression. Lift:
  **db_compint.c 0%→~16%** in isolation.

  **Codec ceiling / PBT tier.** db_compint cannot reach 100% from Tcl: btree
  compression only ever marshals **32-bit** lengths, so the 64-bit
  `__db_decompress_int` (~130 of the file's 324 lines) and the 4–9 byte size
  classes are unreachable from *any* Tcl workload. Those paths are exhaustively
  property-tested by [`test/pbt/pbt_compint.c`](../pbt/pbt_compint.c) — a
  separate property-based-testing tier that round-trips random 64-bit values
  through `__db_compress_int`/`__db_decompress_int` across every size class,
  and is **not** part of this Tcl coverage subset (hence db_compint shows
  "cold" in the ranking even though the codec is well covered overall). The
  tcl-reachable ceiling is ~24%.

Outputs land in `build_unix/` (gitignored):

- `coverage-summary.txt` — the line/branch/function totals
- `coverage-html/index.html` — the browsable report
- `coverage-src.info` — the raw lcov data (src/ only)

## Reading the HTML report

Open `build_unix/coverage-html/index.html`. lcov colours each source line:
**blue = executed**, **red = never executed**. Branch coverage is shown per
line (e.g. `+`/`-` markers and a `taken` count) — a line can be 100% line-covered
but 50% branch-covered if only the true side of an `if` ever ran. **Branch
coverage is the number that matters** for the SQLite ethic: it's what tells you
a conditional's false path is untested.

Directory drill-down: the index lists per-directory then per-file rates; click
into `src/btree/`, `src/lock/`, etc. to find red regions.

## Finding the least-covered files (where to aim next)

`run_coverage.sh` prints this at the end, or run it directly:

```sh
python3 test/coverage/rank_coverage.py build_unix/coverage-src.info | head -20
```

It ranks src files (>= 50 lines) by ascending line coverage, largest first — so
the top of the list is the biggest untested surface. Point new DST scenarios
(Tier A3), PBT generators (Tier B4), or unit tests at those files, then re-run
and watch the number climb.

## The ratchet

`baseline.txt` holds the last-known good `line=` and `branch=` percentages. The
`Coverage` CI workflow (`coverage.yml.workflow`, see below) warns (advisory,
never gates) if branch coverage drops more than 0.5% below the baseline. When
you land tests that raise coverage, update `baseline.txt` — ratchet **up**,
never down.

## CI

`test/coverage/coverage.yml.workflow` is the GitHub Actions workflow. It runs
nightly (cron) and on `workflow_dispatch`, is **not** on every PR (coverage
builds are slow), and is `continue-on-error: true` (advisory). It uploads
`coverage-html/` as a build artifact and prints the summary + ratchet result in
the job log.

> **Install note:** it is committed here as `coverage.yml.workflow`, *not* at
> `.github/workflows/coverage.yml`, because agent OAuth tokens lack the
> `workflow` scope and cannot push files under `.github/workflows/` (same
> constraint that landed pbt.yml/cocci.yml via a separate SSH push — see commit
> `6224220`). A maintainer installs it with:
> ```sh
> git mv test/coverage/coverage.yml.workflow .github/workflows/coverage.yml
> ```
> then pushes over SSH.

---

## Measured baseline (2026-07-27, updated 2026-07-28, extended 2026-07-29)

`CC=gcc`, `--coverage`, src/-only. Two subsets have been measured:

**Original core subset** (`lock001 txn001 test001:btree ssi001 ssi002
recd001:btree`) exercised only the transaction/lock/**btree**/SSI/recovery
core, leaving every other access method and all of db verification at 0%:

| Metric    | Coverage                    |
|-----------|-----------------------------|
| Lines     | 18.6% (13534 / 72681)       |
| Branches  | 12.3% (9631 / 78233)        |
| Functions | 25.6% (712 / 2781)          |

**Current subset** (default in `run_coverage.sh`) adds a curated access-method
matrix run via `run_method` — which runs each test **and then `verify_dir` +
`salvage_dir`** on the databases it leaves behind, so one form lights up the
hash / queue / recno / heap access methods *and* the db-verification and
salvage paths — plus a compaction test (`btree/test111`) and two
partition runs (`run_range_partition` / `run_partition_callback` over
`test001`) that light up `bt_compact.c` and `partition.c`:

| Metric    | Coverage                    | vs. original |
|-----------|-----------------------------|-------------|
| **Lines** | **28.8%** (20970 / 72757)   | **+10.2 pp** |
| **Branches** | **19.0%** (14921 / 78331) | **+6.7 pp** |
| Functions | 34.9% (971 / 2782)          | +9.3 pp     |

The access-method matrix is:
`btree/test001`, `btree/test111` (compaction),
`hash/test001,006,010,025,077`, `queue/test001,007,025`,
`recno/test001,006,024,025`, `heap/test001,013,024`, plus
`run_range_partition test001 btree` and `run_partition_callback test001 btree`
(all reuse existing Tcl tests — no new Tcl written; the win is just running
tests the CI subset had not). The formerly-0% files it moved:

| file | before | after (line% / br%) |
|------|:------:|:------:|
| `hash/hash.c`        | 0% | 53.4 / 36.5 |
| `hash/hash_page.c`   | 0% | 42.5 / 27.5 |
| `hash/hash_verify.c` | 0% | 62.1 / 45.9 |
| `qam/qam.c`          | 0% | 52.5 / 26.0 |
| `qam/qam_verify.c`   | 0% | 44.7 / 26.3 |
| `btree/bt_recno.c`   | 0% | 41.4 / 21.7 |
| `btree/bt_compact.c` | 0% | 30.1 / 17.4 |
| `db/partition.c`     | 0% | 53.7 / 45.0 |
| `heap/heap.c`        | 0% | 31.4 / 19.2 |
| `heap/heap_verify.c` | 0% | 48.0 / 37.9 |
| `db/db_vrfy.c`       | 0% | 48.0 / 34.3 |
| `btree/bt_verify.c`  | 0% | 49.5 / 37.3 |

The full Tcl suite (`run_std`) and the PBT/DST tiers would raise these
further — the nightly job (`coverage.yml.workflow`) / the `ci-extended`
full-tcl job can widen the matrix toward that. The point is the *floor is
measured* and each future test is aimed at a specific red file below.

## Top least-covered source files (aim tests here next)

The remaining big untested surfaces are now dominated by **replication /
repmgr** and **log/db verification** — subsystems that need multi-process
harnesses (rep/repmgr) or crafted corrupt inputs (`log_verify_*`), which is
why they lag the single-process access-method tests:

| line% | br% | lines | branches | file | subsystem |
|------:|----:|------:|---------:|------|-----------|
| 0.0 | 0.0 | 1769 | 1565 | `log/log_verify_int.c` | log verification |
| 0.0 | 0.0 | 1064 | 1619 | `rep/rep_record.c` | replication |
| 0.0 | 0.0 | 1017 | 644 | `log/log_verify_util.c` | log verification |
| 0.0 | 0.0 | 923 | 942 | `rep/rep_util.c` | replication |
| 0.0 | 0.0 | 864 | 540 | `repmgr/repmgr_sel.c` | replication manager |
| 0.0 | 0.0 | 836 | 2069 | `hash/hash_rec.c` | hash recovery records |
| 0.0 | 0.0 | 704 | 556 | `repmgr/repmgr_net.c` | replication manager |
| 0.0 | 0.0 | 701 | 556 | `repmgr/repmgr_msg.c` | replication manager |
| 0.0 | 0.0 | 556 | 656 | `rep/rep_elect.c` | replication elections |
| 0.0 | 0.0 | 541 | 460 | `rep/rep_automsg.c` | replication |
| 0.0 | 0.0 | 431 | 470 | `sequence/sequence.c` | sequences |
| 5.4 | 2.0 | 445 | 306 | `qam/qam_files.c` | queue extent files |
| 30.1 | 17.4 | 1397 | 1702 | `btree/bt_compact.c` | btree compaction (was 0%) |
| 54.7 | 43.1 | 223 | 174 | `env/env_register.c` | DB_REGISTER / failchk (was 0%, COV_DEAD_REG) |
| 57.1 | ~ | 394 | 398 | `xa/xa.c` | XA transactions (was 0%, COV_XA_UPG) |
| 65.9 | 43.3 | 457 | 674 | `lock/lock_deadlock.c` | deadlock detection (was 0.7%, COV_DEAD_REG) |

Next-highest-leverage additions (future work): a **replication smoke test**
(needs the multi-process rep harness — `rep0NN` / `repmgr0NN` exist but each
spins up multiple envs; scope as a separate job), **`log_verify_*`**
(`db_log_verify` over a real log), **`hash/hash_rec.c`** (hash recovery
records — needs a crash/recovery run over a hash DB), and **`sequence.c`** /
**`xa.c`** (each has a small dedicated Tcl test that the subset omits).
`btree/bt_compact.c` and `db/partition.c` moved off 0% in this subset via
`btree/test111` and the partition runners; the PBT tier
(`test/pbt/pbt_hash_func.c`, `pbt_compint.c`, `pbt_compress.c`,
`pbt_recno.c`) covers pure logic in `hash_func.c`, `db_compint.c`,
`bt_compress.c`, and `bt_recno.c` respectively (verified via hegel-c, run
separately from this Tcl subset).
