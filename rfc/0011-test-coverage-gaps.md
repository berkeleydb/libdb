# RFC 0011: Test-coverage gaps that let shipped defects through

- **Status:** Accepted
- **Type:** Normative
- **Author:** libdb maintainers
- **Date:** 2026-09-21
- **Tracking:** gap register G12-G15; defects P2, P6-P8, U7

> **Normative.** The G-numbers defined here are cited from CI workflows,
> `test/KNOWN-ISSUES.md` and several test drivers.

---

The 2026-09 cross-engine campaign (`test/bench/TPROC-XENGINE-2026-09.md`) surfaced
two defects and one performance characteristic that no gate in this repository could
have caught. This note records *why*, because the answer is structural rather than a
matter of someone forgetting to run something — and then says what to change.

## What the run actually found

### 1. Negative scaling above 8 threads (the important one)

The headline "WiredTiger is 9.8× faster" understates the problem. libdb does not
merely lose; **it gets slower as cores are added.**

| threads | libdb-sync-btree (tpm) | WiredTiger (tpm) | WT/libdb | libdb vs its own peak |
|---:|---:|---:|---:|---:|
| 1 | 251 | 258 | 1.0× | 24% |
| 8 | **1,041** | 1,455 | 1.4× | **100% (peak)** |
| 32 | 522 | 2,805 | 5.4× | 50% |
| 96 | 337 | 3,308 | 9.8× | **32%** |

WiredTiger rises monotonically, 12.8× from t=1 to t=96. libdb peaks at 8 threads and
falls to **a third of its own peak** by 96. At t=1 the engines are indistinguishable,
which is what makes this a *concurrency* defect and not a throughput deficit.

The mechanism is visible in the latency distribution for the insert-heavy transaction:

| arm | t=96 new-order p50 | p99 | p99.9 |
|---|---:|---:|---:|
| `libdb-sync-btree` | 74.8 ms | **2,064 ms** | 2,949 ms |
| `wt-btree` | 21.0 ms | **182 ms** | 336 ms |

Same device, same page-read rate (~20 pages/txn), 11× the tail. This is **P1**, the
`PGNO_BASE_MD` allocation convoy: `__db_new` holds the metadata page write-locked
until commit, across its own fsync, so every allocating writer serializes behind one
lock. Already characterized in `test/bench/BTREE-LOCK-SCOPE-2026-09.md`; this run
prices it against a competitor.

### 2. `DB_DIRECT_DB` is completely non-functional (P2)

A documented public flag under which **no database can be opened at all**. The first
metadata read fails `EINVAL` because `__fop_read_meta` receives an unaligned stack
buffer. Found only because the benchmark needed `O_DIRECT` to keep the OS page cache
out of the measurement.

### 3. `DB_MPOOL_AIO` (io_uring) is worth nothing on this workload

NULL at all 7 measured points against a 40.8% noise floor. Not a defect, but it means
an opt-in feature carrying a known deadlock class (S1) currently buys nothing
measurable — which is a decision input, not just a number.

## Why no existing gate could have caught these

Four independent structural gaps. None is "we forgot to run the tests."

### Gap A — CI has no machine that can exhibit the defect

**Every one of the 34 CI jobs runs on `ubuntu-latest`**, a 2–4 vCPU shared runner.
The negative-scaling defect *first appears above 8 threads* and is worst at 96. No
grep of the test suite finds any thread count above the low teens, because there
would be no point: the hardware cannot produce the contention.

This is the whole answer to "why didn't CI catch it." A defect that requires 32+
cores to manifest is **structurally invisible** to a 2-core runner, no matter how
good the tests are.

### Gap B — there is no performance regression gate, by design

The only perf job in the repository (`bench.yml`) is labelled *"informational only"*
and carries `continue-on-error: true`, with the honest comment that a shared runner's
noise floor makes it useless as a gate. So performance is **not gated at all**, and a
regression from 1,041 tpm to 337 tpm would be reported by nothing.

### Gap C — 42 of 54 configure options are never exercised

CI builds 12 of 54 `configure` options. The 42 never built include, among the ones
that change engine behaviour:

`o_direct`, `atomicsupport`, `atomicfileread`, `mutexalign`, `mutexsupport`,
`uimutexes`, `umrw`, `log_checksum`, `partition`, `hash`, `heap`, `queue`,
`replication`, `perfmon_statistics`, `statistics`, `verify`, `bigfile`, `stacksize`,
`debug_rop`, `debug_wop`, `handoff-trace`, `compression`, `uniquename`

**`o_direct` is in that list.** P2 is not a subtle bug — the flag cannot open a
database — and it survived because nothing ever compiled or ran that path.

### Gap D — six runtime behaviour flags are referenced by no test

`DB_DIRECT`, `DB_DSYNC_DB`, `DB_LOG_DIRECT`, `DB_LOG_DSYNC`, `DB_LOG_WRNOSYNC`,
`DB_NOSYNC` appear in **zero** test files. These are the durability and I/O-path
knobs — precisely the flags whose failure modes are silent data loss or, as with P2,
total non-function.

Worse, the one test that *mentions* `DB_DIRECT_DB` (`test/c/cov_api_surface.c`)
checks only that **the setter accepts the flag**. It never opens a file with it. That
is the vacuous-green pattern one layer out: a coverage test that raises coverage
without exercising behaviour.

## What to change

Ordered by defect-caught-per-unit-effort.

### 1. A scaling-shape gate on real hardware (addresses Gap A + B)

The cheapest gate that would have caught the headline defect does **not** need
absolute throughput stability, which is what makes perf gating hard. It needs only a
*shape* assertion:

> throughput at t=32 must not be below throughput at t=8

That is a monotonicity property, robust to a noisy runner and to machine-class
changes, and it fails loudly on exactly this defect. Run it on a self-hosted or
on-demand 32+ core box, nightly rather than per-PR, with the existing
`xe_tproc_c.c` harness. Compare a stored baseline shape, not a stored number.

Corollary: record `tpm(t=8)`, `tpm(t=32)`, `tpm(t=96)` as first-class release
artifacts. A single-threaded number would have shown these engines as equals.

### 2. A build matrix over the untested configure options (Gap C)

Not 2^42 combinations — a **one-at-a-time** sweep: build with each option enabled
alone, plus a small set of known-interacting pairs. Build-only is already worth
having (it would catch `--enable-o_direct` compile breaks), but the option sweep must
*run the smoke suite* for flags that change runtime behaviour, or it only proves the
tree compiles.

Cheap to make honest: fail if an option is neither in the sweep nor on an explicit,
commented exclusion list. That way adding a `configure` option forces a decision
about testing it — the same discipline `test/MANIFEST` applies to tests.

### 3. Behaviour tests for the six untested runtime flags (Gap D)

For each of `DB_DIRECT*`, `DB_DSYNC*`, `DB_LOG_*`, `DB_NOSYNC`: open an environment
with the flag, write, read back, and **verify the flag actually took effect** — for
`DB_DIRECT_DB` that means an `strace`/`/proc` check that `O_DIRECT` is set on the
data file, not merely that `set_flags` returned 0. P2 would have failed this test on
the first run.

This is the generalizable rule, and it is the same lesson as the vacuous-green
series: **a test that asserts an API call succeeded has tested the API, not the
feature.** Assert the observable consequence.

### 4. Retire or fix `cov_api_surface.c`'s flag coverage

It currently converts "flag is accepted" into apparent coverage of the flag. Either
extend it to exercise behaviour or stop counting those flags as covered, because at
present it actively conceals Gap D.

## The honest summary

The test suite is not weak in the areas it covers — 24 tiers, a manifest gate, nine
recorded vacuous-green instances all now fixed. But it has been built almost entirely
around **correctness on a small machine**, and these three findings are:

- a defect that only exists on a **large** machine (A, B),
- a defect in a **build configuration nobody builds** (C),
- and a defect behind a **runtime flag nobody sets** (D).

No amount of additional testing *inside the current envelope* would have found any of
them. The envelope has to grow: more cores, more configure permutations, and
behaviour assertions on the flags rather than acceptance checks.
