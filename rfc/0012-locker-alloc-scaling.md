<!--
Copyright (c) 2026 libdb contributors.  All rights reserved.

SPDX-License-Identifier: Sleepycat

See the file LICENSE for redistribution information.
-->

# RFC 0012 — `mtx_locker_stripe[0]`: the locker-allocation latch

| | |
|---|---|
| Status | **Draft** — diagnosis complete and measured; no fix implemented |
| Tracker | `P12` |
| Supersedes | nothing. Follows P1 (locker-stripe keying), G13 and B2. |

## The measurement

64 vCPU `c6id.16xlarge`, TPROC-C btree, scale 96, `-d nosync`, 20 s measured
after 8 s warmup, lock statistics **cleared before each run**, normalised per
commit:

| t | commits | partition | **locker-alloc** | region | obj-queue | lock-conflict |
|---:|---:|---:|---:|---:|---:|---:|
| 8 | 639,029 | 0.1496 | **0.1189** | 1.1819 | 0.0005 | 0.1663 |
| 16 | 391,736 | 0.1899 | **0.2220** | 1.4552 | 0.0023 | 0.3373 |
| 32 | 311,811 | 0.2291 | **0.3446** | 1.4761 | 0.0058 | 0.5582 |
| 64 | 270,224 | 0.2902 | **0.6220** | 1.5028 | 0.0090 | 0.7667 |

Per-commit growth from t=8 to t=64: **locker-alloc 5.2×**, lock-conflict 4.6×,
partition 1.9×, region 1.3×. Throughput over the same span: **−57.7%**.

`obj-queue` grows 18.8× but its absolute value is 0.009 per commit, so it is not
the story. Normalising matters in both directions — raw counts would have
pointed at the region latch, which has the largest absolute count and the
*slowest* growth.

## Why it was hard to see

Three separate things hid it, each worth naming because each is a trap that will
recur.

**`db_stat -l` is the LOG subsystem, not the lock subsystem.** The lock
subsystem is `-c`. An earlier pass read `-l`, found a line matching "required
waiting", and concluded the lock region latch was the bottleneck. The number was
real and the subsystem was wrong. The superseded table is preserved in
`test/bench/G13-RESOLVED-2026-10.tsv`.

**`mtx_locker_stripe[0]` is not a stripe.** It lives in the stripe array and
`db_stat` reports it alone (`lock_stat.c:168`), so the figure reads like one
stripe of 64 — a 1.6% sample. It is in fact a single global latch;
`src/dbinc/lock.h:383` documents the role explicitly.

**P1's fix was real and did not cover this.** P1 made the stripes bucket-keyed
and cut `LOCK_LOCKERS` from 64 acquisitions to 2, measured at +52% at 96
threads. Stripe 0 was left as a global latch on the same path, which is why that
win did not extend past t=8.

## What the latch actually protects

`__lock_getlocker` (the `txn_begin` path, `txn.c:588`) takes stripe 0 for the
**whole call**. Its comment says this makes the free-list emptiness test atomic
with the refill that may follow. That is true but incomplete: the common path
also mutates shared state under it.

Per transaction, with the free list non-empty:

1. `SH_TAILQ_REMOVE(&region->free_lockers, ...)` — pop from a **shared** free list
2. `++region->nlockers` — a **shared** counter
3. `SH_TAILQ_INSERT_HEAD(&region->lockers, ...)` — the **shared** ulinks list

So "hold stripe 0 only around the refill" **does not work**. The refill is not
what makes the latch necessary; three per-transaction mutations of region-wide
state do. Measured on this workload the refill itself is almost never taken —
**78 lockers allocated in total** against a 2,000,000 maximum, i.e. the refill
path runs in roughly 0.007% of acquisitions — so the latch is paid ~4× per
transaction to protect something that essentially never happens, plus three
mutations that could be made per-stripe.

`__lock_id` and `__lock_freelocker` take it too, for the id counters and the
reverse of the above.

## Direction: remove the sharing, not the latch

Give each stripe its own free list and counter, so the three per-transaction
mutations become stripe-local and stripe 0 stops being on the hot path:

- `free_lockers` → per-stripe free lists. A stripe that empties refills from the
  region under `LOCK_REGION_LOCK`, which is already what the refill path does.
- `nlockers` → per-stripe counters, summed on demand by `db_stat`. The exact
  value is only read for statistics and the `st_maxnlockers` watermark.
- `region->lockers` (ulinks) → per-stripe lists. This is the one that needs care:
  three places walk it, and all three must then walk every stripe. All three are
  **rare**, which is what makes this viable — the deadlock detector
  (`lock_deadlock.c:509`), locker-id wraparound (`lock_id.c:112`, after 2^31
  ids), and failchk (`lock_id.c:646`).

A cheaper intermediate, if the full sharding proves invasive: keep the shared
free list but make the emptiness test a relaxed atomic load outside the latch,
taking stripe 0 only when it reads empty and re-checking under it. That removes
the latch from the common path but leaves `nlockers` and the ulinks insert, so it
is likely a partial win. **It must be measured, not assumed.**

## Evidence standard for the fix

This subsystem has produced **three wrong attributions**, two of them mine
(`tas_spins` on code that was not compiled in; the log-vs-lock `db_stat` error;
and P9, retracted after the mechanism was found not to exist). So:

- A/B on **one binary** with a runtime switch, not two builds — the
  `DB_PRIVATE` layout warning in `test/bench/run_bench.sh` applies.
- Thread sweep t=8/16/32/64, arms alternated **within** each rep.
- Report `db_stat -c` per-commit waits for **every** latch in the table above,
  not just the one being fixed — a fix that moves contention elsewhere must show
  as such.
- The scaling-shape gate must be run at **both** `SCALE=96` and `SCALE=32`: at 32
  the workload already passes at −3.5%, so only the 96 arm can demonstrate an
  improvement.
- `cv` must be under the gate's 10% usability ceiling before any delta is
  quoted. It refused to report at 5 reps and needed 9.
