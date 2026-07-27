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
| `COV_TESTS`  | `lock001: txn001: test001:btree ssi001: ssi002: recd001:btree` | `test:arg` pairs to run |
| `COV_JOBS`   | `nproc`                                   | build parallelism                |
| `TCL_LIB`    | autodetected (nix store or `/usr/lib/tcl8.6`) | Tcl lib dir for `--with-tcl` |
| `COV_TIMEOUT`| `2400`                                    | seconds ceiling for the test run |

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

## Measured baseline (2026-07-27)

`CC=gcc`, `--coverage`, src/-only, subset
`lock001 txn001 test001:btree ssi001 ssi002 recd001:btree`
(278 source files, ~72.7k lines, ~78.2k branches):

| Metric    | Coverage                    |
|-----------|-----------------------------|
| **Lines** | **18.6%** (13534 / 72681)   |
| **Branches** | **12.3%** (9631 / 78233) |
| Functions | 25.6% (712 / 2781)          |

This subset exercises the transaction/lock/btree/SSI/recovery core. The full
Tcl suite (`run_std`) and the PBT/DST tiers would raise these substantially —
the nightly job can widen `COV_TESTS` toward that. The point is the *floor is
now measured* and every future test can be aimed at a specific red file below.

## Top 15 least-covered source files (aim tests here next)

Entirely-untested subsystems in this subset — the biggest actionable gaps
(the recovery-critical and access-method files near the top are the highest
leverage for DST scenarios):

| line% | br% | lines | branches | file | subsystem |
|------:|----:|------:|---------:|------|-----------|
| 0.0 | 0.0 | 1769 | 1565 | `log/log_verify_int.c` | log verification |
| 0.0 | 0.0 | 1397 | 1702 | `btree/bt_compact.c` | btree compaction |
| 0.0 | 0.0 | 1385 | 1590 | `hash/hash_page.c` | hash access method |
| 0.0 | 0.0 | 1307 | 1838 | `heap/heap.c` | heap access method |
| 0.0 | 0.0 | 1089 | 933 | `hash/hash.c` | hash access method |
| 0.0 | 0.0 | 1064 | 1619 | `rep/rep_record.c` | replication |
| 0.0 | 0.0 | 1017 | 644 | `log/log_verify_util.c` | log verification |
| 0.0 | 0.0 | 923 | 942 | `rep/rep_util.c` | replication |
| 0.0 | 0.0 | 883 | 620 | `db/partition.c` | partitioning |
| 0.0 | 0.0 | 864 | 540 | `repmgr/repmgr_sel.c` | replication manager |
| 0.0 | 0.0 | 842 | 1129 | `qam/qam.c` | queue access method |
| 0.0 | 0.0 | 836 | 2069 | `hash/hash_rec.c` | hash recovery records |
| 0.0 | 0.0 | 704 | 556 | `repmgr/repmgr_net.c` | replication manager |
| 0.0 | 0.0 | 701 | 556 | `repmgr/repmgr_msg.c` | replication manager |
| 0.0 | 0.0 | 556 | 656 | `rep/rep_elect.c` | replication elections |

Grouped by subsystem, the biggest untested surfaces are **replication /
repmgr** (`rep_*`, `repmgr_*`), the **hash** and **heap** and **queue** access
methods (only btree is exercised by `test001:btree`), **log/db verification**
(`log_verify_*`, `db_vrfy.c`, `bt_verify.c`), **btree compaction**, and
**partitioning**. Adding a hash/queue/heap variant of the access-method tests
and a replication smoke test to `COV_TESTS` would move the needle most.
