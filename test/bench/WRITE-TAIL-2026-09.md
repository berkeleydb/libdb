# Write-path tail latency: the 272 ms p99 was the benchmark, not the log (2026-09)

Assignment: characterise the write path's p99/p50 spread (272 ms against a flat
3.8 ms, 68x) and the throughput ceiling (~4,100 ops/s at 32 threads regressing
to 3,623 at 96), then fix what the measurement justified. The leading
hypothesis was starvation in the group-commit waiter queue.

**Both effects reproduce exactly. Neither is in the commit path.** The waiter
queue is fair, the software handoff costs 14 µs per round rather than 700, and
the tail belongs to `db->put`. All three headline numbers were artifacts of a
benchmark that measured an empty B-tree filling up.

Nothing in `src/log/log_put.c`'s flush protocol was changed. The instrument
added to it is off by default and does not alter behaviour when on.

## Reproduction, before touching anything

`c7i.24xlarge` (96 vCPU Xeon 8488C, 1 NUMA node), Debian 13, kernel 6.1.187,
`gp3` 10k IOPS / 500 MB/s, THP off, ASLR off, governor `performance`. Stock
`29f616142`, `DB_TXN_SYNC`, 10 s windows, 5 reps, medians with CV:

| threads | ops/s | flush/commit | max commits/fsync | p50 | p99 |
|--------:|------:|-------------:|------------------:|----:|----:|
| 1 | 883 (0.1%) | 1.0000 | 1 | 883 µs | 2.86 ms |
| 8 | 2,260 (2.3%) | 0.2609 | 7 | 3.68 ms | 8.22 ms |
| 32 | 4,072 (2.9%) | 0.1176 | 27 | 3.82 ms | 70.5 ms |
| 96 | 3,706 (3.4%) | 0.1323 | 26 | 3.81 ms | **275.8 ms** (2.9%) |

This matches `WRITE-PATH-2026-09.md`'s table (828/2,284/4,097/3,623 ops/s, p99
4.0/8.2/68.8/272.6 ms) case for case. The phenomenon is real, stable, and
libdb's.

Device floor, `fsync_probe`, n=500: `fsync` p50 **2.77 ms**, p99 2.87 ms;
`fdatasync` p50 2.77 ms. Max serial rate 361/s.

### One correction to the prior report's arithmetic

`WRITE-PATH-2026-09.md` derived "~0.7 ms per round of pure software handoff" by
subtracting a ~1.2 ms device `fsync` from a ~2.0 ms observed round. That
subtraction is not valid, and on this box it is not even sign-correct: the probe
measures 2.77 ms while libdb sustains ~491 rounds/s at 96 threads — *faster*
than the probe's own maximum serial rate. The two are different operations. The
probe appends to a growing file (allocating extents, so the sync carries
metadata); the log flush usually syncs an already-sized region of an existing
file. A device number from one cannot be differenced against a round time from
the other. Measured in the leader's own thread instead, below, the software cost
is 13–15 µs.

## The instrument

`--enable-handoff-trace` (`src/dbinc/log_handoff_trace.h`,
`src/log/log_handoff_trace.c`) records, per waiter and per round:

- **rounds waited** — flush rounds completed between a waiter enqueueing and
  being released. The fairness metric. It is a difference between two reads of
  a `u_int64_t` bumped under `mtx_region` after each wake pass, so no clock
  behaviour can skew it.
- waiter time, split into the blocked interval and the interval spent
  re-acquiring the region lock after being woken.
- leader hold, and inside it the `__os_fsync` itself, both on one thread and one
  clock.

Existing statistics could not settle the question: `st_scount` and
`st_maxcommitperflush` prove coalescing happens, but "every waiter waits one
round" and "most wait one, a few wait ninety" have identical flush/commit ratios
and completely different tails.

Off by default: every macro becomes `NOP_STATEMENT`, no field is added, no
symbols exist (`nm` finds 4 `__db_hoff` symbols in the instrumented library, 0
in the stock one), and the region signature is byte-identical to master —
`0xb86f77f0` on both, same script, same box. On, it adds a field to the `LOG`
region and so *deliberately* changes that signature, which makes an
instrumented library refuse to open a stock environment rather than misread one.

**The instrument does not perturb what it measures.** Warmed workload, stock
library vs instrumented, alternating, 3 reps:

| build | t=32 ops/s | t=32 p99 | t=96 ops/s | t=96 p99 |
|---|---:|---:|---:|---:|
| stock | 9,119 (0.06%) | 4,058 µs | 16,198 (0.32%) | 9,084 µs |
| instrumented | 9,112 (0.08%) | 4,146 µs | 16,164 (0.04%) | 9,066 µs |

Deltas of 0.08% / 0.21% on throughput are inside a noise floor whose own CV is
of the same size.

## Finding 1: the waiter queue is fair. Starvation is disproved.

Rounds waited, 96 threads, 37,683 waiters in one run:

```
rounds_waited  n=37683  mean=1.87  p50=2  p90=2  p99=2  p99.9=2  max=2
rounds_hist    1:4781   2:32986   (>=3: 0)
```

p99 waiters cross **the same number of rounds** as p50 waiters. The distribution
is two-valued: a waiter is released either by the round in progress when it
enqueued or by the next one. Across the whole 5-rep sweep `rounds.p99 = 2` in
every one of the 30 measured rows, with CV 0.0%, and `p99.9 = 2` in 29 of 30
(the one exception is 3).

The extreme tail of this distribution is worth stating precisely rather than
rounding away, because it is the only place any unfairness could hide. Over the
full sweep the per-run **maximum** rounds-waited is 2 in 24 runs, 3 in three
runs, 4 in two, and **6 once** (unwarmed, t=8). So a handful of waiters out of
millions do cross a few extra rounds. That cannot be the 272 ms: six rounds at
~2 ms is ~12 ms, and the 6 occurs at t=8, where the observed p99 is 8.2 ms — not
at the t=96 where the 272 ms lives. The effect is real, bounded, and two orders
of magnitude too small to be the phenomenon under investigation.

No waiter is *repeatedly* overtaken, so none can accumulate a large wait from
queue position. **The FIFO-fairness and anti-starvation candidates are dead** —
the queue is fair at every percentile that carries weight, and the residual
maximum is far too small to matter. This closes the hypothesis rather than
leaving it open, and it is why no fix was made here.

## Finding 2: the software handoff costs 14 µs, not 700 µs

Per round, at 96 threads, measured in the leader's thread:

```
hold_us    p50=2776  p90=2854  p99=2923   mean=2008
fsync_us   p50=2760  p90=2834  p99=2894   mean=1995
per_round_mean: hold=2019.3  fsync=2006.2  software=13.1
```

**0.65% of a round is software**; the rest is the device. Across the sweep the
software term is 0.2 µs at t=1, 6.9 µs at t=8, 14.5 µs at t=32, 13.1 µs at t=96.
The `hold_us` p50 of 2.78 ms also agrees with the standalone probe's 2.77 ms, so
the two independent measurements of the device corroborate.

There is no handoff overhead to reclaim, which kills the remaining two
candidates: shortening the leader's critical path and pre-designating the next
leader both target a cost that is 14 µs out of 2,019 µs.

## Finding 3: the tail is in `db->put`, and the driver mislabelled it

`commit_bench` timed `txn_begin` + `put` + `commit` as one interval and reported
the result as commit latency. Timing the phases separately, unwarmed, 5 reps:

| threads | begin p99 | **put p99** | commit p99 | commit p50 |
|--------:|----------:|------------:|-----------:|-----------:|
| 1 | 4 µs | 17 µs | 2.84 ms | 877 µs |
| 8 | 47 µs | 3.79 ms | 5.65 ms | 3.65 ms |
| 32 | 131 µs | **66.3 ms** | 5.77 ms | 3.77 ms |
| 96 | 123 µs | **271.8 ms** | 5.73 ms | 3.73 ms |

The commit phase's own p99 is 5.73 ms against a 3.73 ms p50 — a **1.5x spread,
not 68x** — and it barely moves from 8 to 96 threads. The entire tail is in
`put`, which the reported number had folded into "commit".

A stack census at 96 threads (two samples, 97 threads each, one run) shows the
same thing structurally: **81–83 frames in `__lock_get_internal`, 1 in
`__os_fsync`, 95 in `__db_pthread_mutex_condwait`.** The threads are queued for
page locks, not for the log. Lock statistics from the same run agree: 4,840
conflicts waited on, 1,333 conflicts not waited on, 0 deadlocks.

## Root cause: the benchmark never warmed up

The environment was created empty and the workload wrote 1,000,000 distinct
keys, so every `put` was an *insert into a growing B-tree*. The run measured
page splits and new-page allocation, and a writer that holds a split's locks
across its own ~3.7 ms durable commit convoys every other writer behind it.
That convoy — not the flush protocol — is the 272 ms.

Three independent probes confirm the mechanism, each with a control:

1. **Remove the fsync, keep the locking.** `DB_TXN_NOSYNC` at 96 threads: put
   p99 collapses 269 ms → 0.12 ms. The convoy exists only because holders sit
   in a durable commit while holding page locks.
2. **Vary key-space temperature.** Same binary, same threads, only the key
   range: 1,000 keys → put p99 76 ms; 100,000 → 258 ms; 10,000,000 → 256 ms
   with put p90 jumping to 142 ms. The tail tracks the amount of *fresh
   insertion*, which is what a split-convoy predicts and a commit-path defect
   does not.
3. **Warm up and re-measure.** Prepopulate the key range, change nothing else.

## Before / after: warmup is the whole effect

5 reps, arms alternating within each rep, `KEYRANGE=1000000`, medians (CV):

| t | arm | ops/s | p50 | p99 | p99.9 | commit p99 | put p99 | flush/commit | commits/fsync | rounds waited p99 |
|--:|---|------:|----:|----:|------:|-----------:|--------:|-------------:|--------------:|------------------:|
| 1 | unwarmed | 883 (0.1%) | 883 µs | 2.86 ms | 2.93 ms | 2.84 ms | 17 µs | 1.0000 | 1 | – |
| 1 | warmed | **1,060** (0.0%) | 877 µs | 2.83 ms | 2.88 ms | 2.82 ms | 5 µs | 1.0000 | 1 | – |
| 8 | unwarmed | 2,260 (2.3%) | 3.68 ms | 8.22 ms | 14.9 ms | 5.65 ms | 3.79 ms | 0.2609 | 7 | 2 (0.0%) |
| 8 | warmed | **3,640** (2.3%) | 1.80 ms | **3.78 ms** | 3.91 ms | 3.76 ms | 8 µs | 0.2500 | 7 | 2 (0.0%) |
| 32 | unwarmed | 4,072 (2.9%) | 3.82 ms | 70.5 ms | 90.4 ms | 5.77 ms | 66.3 ms | 0.1176 | 27 | 2 (0.0%) |
| 32 | warmed | **9,134** (0.3%) | 3.72 ms | **3.95 ms** | 5.39 ms | 3.89 ms | 13 µs | 0.0625 | 31 | 2 (0.0%) |
| 96 | unwarmed | 3,706 (3.4%) | 3.81 ms | 275.8 ms | 318.8 ms | 5.73 ms | 271.8 ms | 0.1323 | 26 | 2 (0.0%) |
| 96 | warmed | **16,244** (2.4%) | 6.05 ms | **7.47 ms** | 9.22 ms | 7.06 ms | 287 µs | 0.0206 | 94 | 2 (0.0%) |

At 96 threads: **4.4x the throughput and a 37x smaller p99**, from warming up
the benchmark. Every headline symptom disappears:

- **Tail spread.** p99/p50 falls from 72x to **1.24x**. The remaining 7.5 ms p99
  against a 2.8 ms device fsync is two-to-three fsyncs of queueing — the honest
  cost of durable commit under load, with no fairness pathology in it.
- **Throughput ceiling and regression.** Unwarmed, 4,072 → 3,706 from 32 to 96
  threads (the reported regression). Warmed, 9,134 → **16,244**: it scales.
- **Batching improves too.** flush/commit 0.1323 → 0.0206, and commits per fsync
  27 → **94**. The convoy was *starving* the batcher: threads stuck behind page
  locks were not at the log to be batched. Coalescing was never the limiter, but
  it was a victim.

`rounds_waited p99 = 2` in every row, warmed or not. The queue was fair the
whole time, under both regimes.

## What changed

No change to the flush protocol. Three commits:

1. **`fix(bench): don't let the drivers link a system Berkeley DB behind our
   back.`** `test/bench/Makefile` linked `-ldb-5.3`, which names a version.
   `2bad51f4f` renamed the fork's soname to `libdb-2026.0.so`, so post-CalVer
   that name matched nothing in the build tree and resolved to Debian's
   `/lib/x86_64-linux-gnu/libdb-5.3.so` — Oracle Berkeley DB 5.3.28 from 2013.
   `-rpath` cannot help: the soname recorded in the binary is the system
   library's. `db_version()` from the as-committed link reports `Berkeley DB
   5.3.28: (September 9, 2013)`; linking the tree's `.so` by path reports `libdb
   2026.09.6`.

   *This did not corrupt the published numbers* — the resulting binary is a
   2026-headers/2013-code ABI mismatch that produces obvious garbage (1.79M
   ops/s at one thread, `flushes=0`, `p50=0`), not a plausible lie, and the
   reproduction above confirms the report's table came from the real library.
   But that loudness is luck: the same substitution between ABI-compatible
   builds would be undetectable. `run_bench.sh`'s guard was blind to it twice
   over — it grepped for the literal string `libdb-5.3` and required it inside
   `$BDB/.libs` (unsatisfiable post-rename), and its driver list omitted
   `commit_bench` and `fsync_probe`. The Makefile now discovers the library from
   the tree by wildcard; the guard names no version, covers every driver, and
   cross-checks the loaded library's runtime `db_version()` against the same
   tree's `db.h`.

2. **`perf(log): instrument the group-commit handoff.`** Described above.

3. **`fix(bench): commit_bench measured an insert ramp and mislabelled put as
   commit.`** Prepopulates before measuring (`PREPOP=0` restores the ramp and
   says so on stdout); times and reports `begin`/`put`/`commit` separately;
   dumps the handoff histograms via a weak symbol so one driver serves both
   builds. `KEYRANGE` is now a knob.

## Durability

The flush path was not modified, so nothing here can weaken it — but it was
verified rather than asserted.

**`test/sim/gc-durability-gate.sh` on this branch: PASS.** Phase 1, clean
`--enable-dst` build, 8 concurrent committers: **30 of 30** runs across 5 seeds
x 6 crash points — every transaction whose `commit()` returned success is
present after crash recovery. Phase 2, the negative control, with the
`NODURABLE` bug planted *in the library* (`CPPFLAGS=-DDB_DST_INJECT_BUG=1`,
which is the part that is easy to get wrong): **caught 9 of 9**, e.g. "66 of 66
acked commits lost because the log fsync was skipped". A green phase 1 without a
catching phase 2 would be vacuous; both hold.

Additionally, after a 32-thread `DB_TXN_SYNC` run: `db_verify` succeeded,
`db_recover` completed, `db_verify` succeeded again afterwards.

The two properties the assignment named still hold, in the same runs as the
performance data:

- **flush/commit ratio**: 0.0206–1.0 across the matrix, i.e. coalescing intact
  (and better warmed than unwarmed).
- **N waiters : 1 fsync**: up to 94 commits retired per fsync warmed, and the
  stack census shows exactly one thread in `__os_fsync` against 95 parked
  waiters.

`DB_TXN_NOSYNC` and `DB_TXN_WRITE_NOSYNC` are unchanged: 8 threads, 5 s, both
report `flushes` of 6–7 total against ~620,000 commits (`flush_per_commit
0.0000`), versus 3,683 flushes for 14,714 `sync` commits. No mode acquired or
lost a flush.

No error path was touched, so the `__memp_aio_drain` class of fast liar — a
swallowed write error yielding a false durable frontier — has no new surface
here. The instrument's own failure mode is handled the other way: allocation
failure `abort()`s rather than silently recording a partial histogram, and
dropped samples are counted and printed so a truncated distribution cannot be
mistaken for a complete one (`wait_dropped=0`, `round_dropped=0` in every run
reported here).

## Verdict

**Characterised; no fix made, and none is justified by this data.** The
assignment's three candidate fixes are all refuted by measurement rather than
declined:

- *FIFO-fair / anti-starvation queue* — `rounds_waited` p50 = p99 = p99.9 = 2.
  There is no unfairness. Adding a priority mechanism would add shared-state
  complexity to a path that is already fair.
- *Shorten the leader's critical path* — 13.1 µs of 2,019 µs is software.
- *Pre-designate the next leader* — same 0.65% budget.

The real defect was in the measurement, and it is fixed: the benchmark measured
a load ramp and attributed page-lock convoying to the commit path. Warmed, the
write path does 16,244 durable commits/s at 96 threads with a p99 of 7.5 ms and
a p99/p50 of 1.24x.

**What this does not license.** It says nothing about the 20–48x write-side gap
to WiredTiger in `CROSS-ENGINE-2026-09.md`; that comparison used different
drivers and stands or falls on its own re-measurement. It does not mean libdb's
write path is fast — 16k durable commits/s on a device that can do ~360 serial
fsyncs is good batching, but the p50 is still one full fsync (`commit p50` 3.7 ms
at t=32), and single-threaded throughput is 1,060 ops/s.

**The genuinely open question this exposes is the one the convoy was hiding:**
`db->put` holds page locks across the durable commit, so under *real* insert
load (a growing index, not a pre-sized one) the convoy is not an artifact — it
is what applications will hit. The unwarmed arm is a fair model of bulk-insert
workloads, and there the 272 ms is real. That is a lock-scope problem in the
B-tree, not a fairness problem in the log, and it is the correct next
investigation. Warming up the benchmark did not fix it; it measured around it.

## Reproducing

```sh
cd build_unix && ../dist/configure && make -j$(nproc)
cd ../test/bench && make BDB=../../build_unix commit_bench fsync_probe

# steady state vs the ramp that produced the 272 ms
mkdir -p /tmp/e && KEYRANGE=1000000 ./commit_bench /tmp/e 96 10 sync
mkdir -p /tmp/r && PREPOP=0 KEYRANGE=1000000 ./commit_bench /tmp/r 96 10 sync

# the handoff histograms
cd ../../ && mkdir -p build_hoff && cd build_hoff
../dist/configure --enable-handoff-trace && make -j$(nproc)
cd ../test/bench && make BDB=../../build_hoff commit_bench && ./commit_bench /tmp/e 96 10 sync

# durability, with its negative control
cd ../sim && sh ./gc-durability-gate.sh
```

Raw data: `results/write-tail-2026-09/`.
