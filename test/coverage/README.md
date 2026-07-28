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

Set `COV_REP=1` to add the replication suite (biggest cold surface: rep/ +
repmgr/ ~= 12.4k lines at 0.8%). It moves them to ~56% line / ~42% branch. Those
tests each run in their own `tclsh` (driver-per-test, per-test timeout) because a
few election/lease tests hang; see `REPLICATION-COVERAGE.md` for the exact set,
the measured lift, and what still needs real multi-process orchestration
(`rep*script.tcl` subprocess tests, repmgr 100-series needing `db_repsite`).

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
| 0.0 | 0.0 | 457 | 674 | `lock/lock_deadlock.c` | deadlock detection |
| 0.0 | 0.0 | 431 | 470 | `sequence/sequence.c` | sequences |
| 0.0 | 0.0 | 394 | 398 | `xa/xa.c` | XA transactions |
| 5.4 | 2.0 | 445 | 306 | `qam/qam_files.c` | queue extent files |
| 30.1 | 17.4 | 1397 | 1702 | `btree/bt_compact.c` | btree compaction (was 0%) |

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
