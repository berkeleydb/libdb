# libdb — Full-Suite Coverage Report #4: wiring the #147 drivers into CI

Measurement only. No engine code touched. Run on two dedicated EC2
`c7i.24xlarge` instances (96 vCPU, AL2023, verified idle at `load ~0.0` before
every timing), both **terminated**, key pair and security group deleted.
master @ `b4d65c25f`, gcc/gcov 11.5.0, lcov 2.0-1, tcl 8.6.10,
`-O0 -g --coverage`, lcov captured from `.libs`.

## The headline is a wiring fix, not a new test

PR #147 added five validated coverage drivers (`cov_api_surface`, `cov_codecs`,
`cov_logrec_print`, `cov_oom_paths`, `cov_rep_api`) plus runners for two
never-measured tiers (`cov_cutest`, `cov_fuzz_corpus`). They were only invoked
by `test/coverage/full_run4.sh`, and **no workflow calls full_run4.sh**.
`.github/workflows/coverage.yml` runs `test/coverage/run_coverage.sh`, which
referenced none of them. So every driver in #147 has been dead code from CI's
point of view since it merged, and the nightly number never moved.

This run wires them in and measures the result.

## Measured before/after — `run_coverage.sh`, same commit, same toolchain

| Metric   | BEFORE (master as-is) | AFTER (wired)        | Delta            |
|----------|----------------------:|---------------------:|-----------------:|
| Line     | 46.2% (33733/73005)   | **59.4%** (43394/73013) | **+13.2 pp** (+9661 lines) |
| Branch   | 31.6% (24963/78984)   | **40.6%** (32059/78996) | **+9.0 pp** (+7096 branches) |
| Function | 58.8% (1643/2795)     | **78.6%** (2196/2795)   | **+19.8 pp** (+553 functions) |

Wall time: 31m48s before, 34m28s after — the whole block costs **+2m40s**.

Excluding `dbinc_auto/` (which reports #2/#3 stripped and `run_coverage.sh` does
not) the picture is identical: line 46.2%→59.4%, branch 31.6%→40.6%, function
59.1%→79.0%. 279 of 279 files matched between the two traces; **zero files
regressed** on any metric.

## The baseline was measuring the wrong thing

`baseline.txt` read `line=68.0 / branch=50.2`. The task framed these as "the
pre-#147 figures", and they are — but they were never `run_coverage.sh` numbers.
Git archaeology on the file:

| commit | line/branch | produced by |
|--------|-------------|-------------|
| `84de20c` | 18.6 / 12.3 | `run_coverage.sh` (original core subset) |
| `e07a9be` | 26.9 / 17.8 | `run_coverage.sh` |
| `8620bb7` | 28.8 / 19.0 | `run_coverage.sh` |
| `1ced502` | 48.0 / 36.1 | **switched to full-suite** (report #1) |
| `f50c1c6` | 62.3 / 46.2 | full-suite (report #2) |
| `c16ff58` | 68.0 / 50.2 | full-suite (report #3) |

At `1ced502` the baseline was switched from the subset driver to the full-suite
driver and never switched back. `run_coverage.sh` runs a deliberately bounded
~34-test curated subset; the reports run the entire Tcl suite in parallel (5
access methods × ~135 tests, ~20 subsystem groups, 37 replication tests). So for
three baseline generations **the advisory ratchet has compared a subset
measurement against a full-suite ceiling it structurally cannot reach**, which
means the warning fired unconditionally and carried no signal.

`baseline.txt` is now set to the measured `run_coverage.sh` values
(59.4/40.6/78.6) with a comment stating which driver produced them, and a
`function=` line so the function metric can ratchet too. Full-suite figures stay
in the report files where they belong.

One caveat: the ratchet step in `.github/workflows/coverage.yml` still compares
**branch only**, so the new `function=` line is informational until someone wires
it up. I deliberately did not edit `.github/workflows/` — agent tokens lack the
`workflow` OAuth scope (the same constraint documented in the README's install
note), and touching it would block the push. A maintainer can add the function
compare in three lines mirroring the existing branch compare.

## Per-driver attribution

Each driver was run **alone** against a wiped `.gcda` set on a
`--enable-faultinject` build, so these are absolute footprints (they overlap
heavily with each other and with the Tcl subset — they do not sum to the delta):

| driver | line | func | branch | dur | verdict |
|--------|-----:|-----:|-------:|----:|---------|
| `cov_cutest` | 21467 | 1076 | 14525 | 17s | biggest single contributor |
| `cov_logrec_print` | 18467 | 985 | 12263 | 2s | best coverage-per-second in the tree |
| `cov_oom_paths` | 13535 | 708 | 8644 | 113s | 389 fault points swept |
| `cov_fuzz_corpus` | 10384 | 531 | 6401 | 13s | 15 seeds, 3 harnesses |
| `cov_rep_api` | 9176 | 531 | 5597 | <1s | |
| `cov_api_surface` | 8984 | 614 | 5450 | <1s | |
| `cov_codecs` | 275 | 11 | 101 | <1s | pure functions, exactly as designed |

Where the +553 functions actually landed (normalized paths, before→after):

| file | funcs | note |
|------|-------|------|
| `repmgr/repmgr_method.c` | 4→53 of 57 | `cov_cutest`'s TestChannel |
| `repmgr/repmgr_util.c` | 6→44 of 51 | TestChannel |
| `rep/rep_method.c` | 2→34 of 35 | `cov_rep_api` |
| `repmgr/repmgr_net.c` | 1→29 of 32 | TestChannel (real sockets) |
| `repmgr/repmgr_sel.c` | 0→27 of 32 | TestChannel |
| `repmgr/repmgr_automsg.c` | 0→25 of 32 | TestChannel |
| `log/log_verify_int.c` | 37→61 of 99 | `cov_logrec_print` |
| `env/env_method.c` | 42→65 of 65 | `cov_api_surface` — now complete |
| `db/db_method.c` | 24→47 of 50 | `cov_api_surface` |
| `repmgr/repmgr_msg.c` | 0→22 of 26 | TestChannel dispatch |
| `rep/rep_backup.c` | 1→21 of 37 | |
| `mp/mp_fmethod.c` | 12→22 of 22 | `cov_api_surface` — now complete |
| `common/os_method.c` | 0→22 of 22 | now complete |

21 files went from **zero functions touched** to non-zero. `repmgr` as a
subsystem went to 86.6% function / 64.2% line — the single biggest structural
change, and it comes entirely from running a CuTest binary that already existed
and already passed.

## A second bug found: three already-wired drivers were failing silently

Unrelated to #147, found because the BEFORE run surfaced it. In the BEFORE run:

```
FAIL recd_compact (rc=1)
FAIL recd_handlers (rc=1)
FAIL hash_unsorted_cmp (rc=1)
undefined reference to `__gcov_init' / `__gcov_exit' / `__gcov_merge_add'
```

Those three scripts prefer the **static** `build_unix/libdb.a` (deliberate — on
macOS the `.dylib`'s baked-in install name beats `-rpath`), and their `gcc` line
passes only `${CFLAGS:-}`, no `--coverage`. Nothing pulls in libgcov, so the link
fails. The `.so`-linking drivers were unaffected because the shared library
already carries gcov. These three have been failing on **every** coverage run
since they landed, contributing zero — and because the driver block only prints
`FAIL` and continues, nothing ever escalated it.

Fixed by `export CFLAGS="--coverage"` before the driver phase. All three PASS in
the AFTER run. Part of the measured +13.2 pp is therefore attributable to this
fix rather than to #147's drivers; the two are reported together because a
single run cannot separate them without a third full run, and the honest split
is "the driver phase now works, end to end".

## The failing cutest suite: what I did, and what is actually wrong

The task said `run_cov_cutest.sh` had 1 of 9 suites failing (`TestQueue`,
`sh_l_as_string` buffer overrun at `TestQueue.c:64`, crashing at `-O2`) and to
isolate or skip it. **I did neither, because it does not fail in the coverage
build.** Measured, on the coverage build (`-O0 -g --coverage`):

```
run_cov_cutest.sh: ran 9 suites, 9 clean, 0 with test failures
run_cov_cutest.sh: PASS
```

Re-running `TestQueue` alone: `rc=0`, `OK (1 test)`. The wrapper's existing
one-suite-per-process design already guarantees a crash cannot cost the other
suites their coverage, so no change was needed.

The `-O2` crash is real and I reproduced it deliberately to characterise it —
built the same suites at `-O2` against the same library:

```
Program received signal SIGSEGV
#0  sh_l_as_string (l=<optimized out>) at ../test/c/suites/TestQueue.c:64
#1  0x...  in TestQueue () at ../test/c/suites/TestQueue.c:827
```

with `i=2, t=0, fc=5, eval=0`. Line 827 is the **failure diagnostic printer**.
It is only reached when `f_verify()` returned non-zero. So the accurate
description is *two* defects, not one:

1. an `-O2`-only verify failure on shared-list op case `i=2` (the real bug, and
   the reason the printer runs at all), and
2. `sh_l_as_string()`'s unbounded fill of `static char buf[1024]`, which then
   segfaults while trying to report defect 1.

Calling it "a harness bug that crashes at -O2" describes only the second. The
first is why it triggers. Both are in test code, not in libdb, and neither
affects this measurement — but a `-O2` verify failure in the shared-list unit
tests is worth its own issue, and the crash masks whatever the verifier was
about to say. I did not fix either: out of scope for a measurement change, and
fixing the printer would expose the verify failure as a hard test failure that
would then need triage.

## >85% function coverage: not reachable here, with arithmetic

The corrected target was >85% **function**, described as needing only +118 from
80.5% (2129/2644). That arithmetic is against **report #3's full-suite basis**,
not against what CI measures. Both bases, measured:

**Against report #3's full-suite basis** (2129/2644 = 80.5%): 85% needs
`ceil(0.85 × 2644)` = 2248, so **+119 functions**. That is the number the task
quoted and it is correct for that basis — but no CI job produces that basis.

**Against the CI-measured `run_coverage.sh` basis**, which is what this PR
changes and what the ratchet reads:

- BEFORE: 1643/2795 = 58.8%. 85% needs 2376 → **+733 functions**.
- AFTER: 2196/2795 = 78.57%. 85% needs 2376 → **+180 functions**.

So wiring the drivers closed 553 of the 733, i.e. **75% of the gap**, in +2m40s
of runtime. The remaining **+180** is not reachable from this subset, and the
per-file distribution says why. There are 599 uncovered functions left; the
largest concentrations are:

| uncovered | file | why the subset cannot reach it |
|----------:|------|--------------------------------|
| 38 | `log/log_verify_int.c` (61/99) | needs crafted corrupt logs |
| 31 | `btree/bt_compress.c` (21/52) | needs the full `compressed` group |
| 18 | `db/db_rec.c` (8/26) | recovery handlers; needs full `recd` |
| 16 | `rep/rep_backup.c` (21/37) | multi-process replication |
| 15 | `dbinc_auto/db_auto.h` (21/36) | generated inlines |
| 13 | `rep/rep_automsg.c` (13/26) | multi-process replication |
| 13 | `common/db_err.c` (20/33) | panic/abort paths |
| 12 each | `sequence.c`, `rep_util.c`, `partition.c`, `db_autop.c` | dedicated groups |
| 11 | `rep/rep_elect.c` (1/12) | elections — known to hang in-process |
| 9 | `rep/rep_lease.c` (0/9) | leases need real multi-process (documented) |

Getting +180 from here means adding whole test groups to the subset — the full
`compressed` group, the full `recd` group, a multi-process replication harness
(and the absent `db_repsite` utility) — which is exactly the "grinding round"
the task said to avoid. **>85% function is reachable on the full-suite basis
(+119, and report #3 was already at 80.5% before these drivers) but not on the
CI subset basis without expanding the subset itself.** Recommendation: run
`full_run4.sh` on a dedicated box to get the full-suite number with the drivers
included; that is the run where 85% is a live question.

On branch coverage the task's conclusion is confirmed and I did not chase it:
40.6% with 46,937 branches missing, of which ~1,662 are
`if ((ret = f()) != 0)` OOM legs reachable only one-per-fault-injection-run.

## What I could not measure — honest list

- **`cov_dst` never ran.** It SKIPs cleanly (as designed) because I did not put
  `--enable-dst` in the default configure line: the 41 DST scenarios need a time
  budget well past a CI run. Gated behind `COV_DST=1`. Its contribution is
  therefore **unmeasured**, not zero — same class of gap as the one this PR
  closes, and the honest thing is to say so rather than quote a projection.
- **`cov_oom_paths` ran at `COV_OOM_STRIDE=4`, not 1.** Baseline allocation
  count `M = 1554`; the sweep covered 389 of them (318 tolerated, 62 clean
  error, 8 crashed, 1 hung). The exhaustive sweep would cover more error legs.
  I did verify the mechanism the task flagged: `HAVE_FAULT_INJECT` is defined in
  `db_config.h`, and the driver contributes 13,535 lines / 708 functions
  standalone, which is only possible if `__gcov_dump()` is reached before
  `_exit()`. It is not measuring zero.
- **The +13.2 pp cannot be split** between #147's drivers and the `CFLAGS`
  link fix without a third full run. Both are in the AFTER number.
- **Per-driver footprints are absolute, not marginal.** They overlap heavily;
  they do not sum to +553 and should not be read as independent contributions.
- **`env164` fails in both runs** (`BDB2034 unable to allocate memory for
  mutex; resize mutex region`) — pre-existing, identical before and after, not
  investigated.
- One measurement box ran the BEFORE and one the AFTER, in parallel, to halve
  wall-clock cost. Same AMI, instance type, toolchain and commit; the only
  difference is the absolute source path baked into the `.gcno` (`/A/src` vs
  `/C/src`), which I normalized when diffing per-file.

## Reproducing

```sh
# BEFORE
git checkout b4d65c25f
TCL_LIB=/usr/lib64 COV_JOBS=96 test/coverage/run_coverage.sh

# AFTER
TCL_LIB=/usr/lib64 COV_JOBS=96 test/coverage/run_coverage.sh   # with this PR
# exhaustive OOM sweep:            COV_OOM_STRIDE=1
# include the 41 DST scenarios:    COV_DST=1
# skip the whole block:            COV_C_DRIVERS=0
```
