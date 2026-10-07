<!--
Copyright (c) 2026 libdb contributors.  All rights reserved.

SPDX-License-Identifier: Sleepycat

See the file LICENSE for redistribution information.
-->

# libdb negative-scaling diagnosis — G13, c6id.16xlarge (64 vCPU)

**Host** c6id.16xlarge, 64 vCPU, Intel Xeon Platinum 8375C @ 2.90GHz, Fedora 43,
kernel 7.2.9-100.fc43.x86_64, XFS on /nvme, THP `[never]`.
Same instance class and CPU model as the committed baseline
`test/bench/G13-SCALE-SHAPE-2026-10.tsv`.

**Build** `git clone https://github.com/berkeleydb/libdb.git /nvme/w` at
`750fb0302`, built in the sibling dir `/nvme/w/build_unix` with
`CFLAGS="-O3 -g -fno-omit-frame-pointer" ../dist/configure && make -j64`.
Nothing in the repo was modified; nothing was committed or pushed.

> **Build-flag correction worth recording.** My first build used
> `--enable-debug`, which in `dist/configure.ac:319` sets `CFLAGS="-g $CFLAGS"`
> and **suppresses the optimizer** — useless for a profile. Rebuilt optimized.
> A second rebuild was needed because `--disable-shared` leaves no
> `.libs/libdb-*.so`, and `scale_shape_gate.sh` refuses to build its driver
> without one (correctly — it will not fall back to a system libdb).

**Measured mutex configuration (not assumed):** `checking for mutexes...
POSIX/pthreads/library`; `db_config.h` defines `HAVE_MUTEX_PTHREADS` and
**does not define `HAVE_MUTEX_HYBRID`**, and `nm` finds no
`__db_tas_mutex_lock` in the shared object. On Linux/x86_64 the hybrid/TAS
probe block in `dist/aclocal/mutex.m4` is nested inside
`if test "$db_cv_mutex" = no` (line 203), which is already false once
POSIX/pthreads matched at line ~174 — so the comment at line 344 ("we check for
them even if we've already found a pthreads-style mutex") does not describe what
the script does. **Everything below is a pure-pthread-mutex build: there is no
TAS spinning in it at all.** Any future claim about `tas_spins` on Linux/x86_64
has to start by fixing that configure nesting.

---

## 0. The defect reproduces here, harder than recorded

Gate teeth first (`--self-test`, both directions, on this box):

```
VERDICT scale-shape FAIL threads_lo=8 threads_hi=96 tpm_lo=1041 tpm_hi=337 delta_pct=-67.6 tol_pct=5.0 failing_steps=t32:-49.9%,t96:-67.6%
OK: descending shape FAILED the gate, as required.
VERDICT scale-shape PASS threads_lo=8 threads_hi=32 tpm_lo=1455 tpm_hi=2805 delta_pct=+92.8 tol_pct=5.0
OK: ascending shape PASSED the gate, as required.
SELFTEST PASS -- the gate discriminates in BOTH directions:
```

Then the real measurement, 3 alternating reps, scale=96, 20s + 10s warmup:

```
tproc-c libdb 8  1 tpmC_like tpm 1008744      tproc-c libdb 32 1 tpmC_like tpm 124883
tproc-c libdb 8  2 tpmC_like tpm 1019468      tproc-c libdb 32 2 tpmC_like tpm 125742
tproc-c libdb 8  3 tpmC_like tpm  912368      tproc-c libdb 32 3 tpmC_like tpm 124323

tolerance: max(5.0% floor, 3.0 x cv(t=8)=4.77%) = 14.3%
step:      tpm(t=32 )=124883    vs tpm(t=8)=1008744    =    -87.6%   BELOW TOLERANCE
VERDICT scale-shape FAIL threads_lo=8 threads_hi=32 tpm_lo=1008744 tpm_hi=124883 delta_pct=-87.6 tol_pct=14.3 failing_steps=t32:-87.6%
```

−87.6% here vs −76.8% recorded. t=32 cv is ~0.6%. Not noise.

---

## 1. Profile comparison, t=8 vs t=32

`perf record --call-graph dwarf,16384 -F 199`, one run each, serialised.
Verdicts from the profiled runs themselves: t=8 **749,661 tpm**, t=32
**112,446 tpm** — the defect is present under perf.

### The number that reframes the whole question

| | t=8 | t=32 |
|---|---|---|
| perf event count (cycles) | **184,049,604,455** | **83,641,899,809** |
| `perf stat` task-clock | 53,846.97 msec | **39,242.86 msec** |
| CPUs utilized | 1.65 | **1.16** |
| instructions | 198,140,603,935 | 86,076,268,206 |
| context-switches | 709,382 | 667,419 |

At 4x the threads the box burns **less than half** the cycles and **27% less
CPU time**, at the same wall clock. The machine is going *idle*. Whatever t=32
is doing, it is **not** spinning on a latch — it is blocking and throwing work
away. On a 64 vCPU box, 1.16 CPUs utilized at t=32 is the headline.

### Self-time, top 15 (percent of that arm's own samples)

| # | t=8 | % | t=32 | % |
|---|---|---|---|---|
| 1 | `__GI___pthread_mutex_unlock_usercnt` | 7.59 | `native_queued_spin_lock_slowpath` [k] | 8.77 |
| 2 | `__db_pthread_mutex_lock` | 6.07 | **`__lock_detect`** | **6.76** |
| 3 | `pthread_mutex_lock` | 5.84 | `__ham_func4` | 5.46 |
| 4 | `native_queued_spin_lock_slowpath` [k] | 5.03 | `__db_pthread_mutex_lock` | 3.52 |
| 5 | `pthread_rwlock_unlock` | 2.82 | `pthread_rwlock_unlock` | 3.17 |
| 6 | `__db_pthread_mutex_unlock` | 2.75 | `pthread_mutex_lock` | 2.40 |
| 7 | `__os_atomic_read` | 2.70 | `__GI___pthread_mutex_unlock_usercnt` | 2.13 |
| 8 | `__memp_fget` | 2.68 | `__memp_fget` | 2.02 |
| 9 | `__memp_get_bucket` | 2.48 | `__lock_get_internal` | 1.94 |
| 10 | `__lock_get_internal` | 2.38 | `__db_pthread_mutex_unlock` | 1.89 |
| 11 | `__memp_fput` | 2.11 | `__memp_get_bucket` | 1.74 |
| 12 | `__db_cursor_int` | 1.93 | `pthread_rwlock_wrlock` | 1.48 |
| 13 | `__ham_func4` | 1.87 | `__bam_search` | 1.37 |
| 14 | `__bam_cmp` | 1.82 | `__db_pthread_mutex_readlock` | 1.37 |
| 15 | `__bam_search` | 1.81 | `try_grab_folio_fast` [k] | 1.26 |

Self-time is **diffuse in both arms** — no symbol exceeds 9%. Read alone, this
table says "it is spread out", and that is how this defect has been
misattributed before. The one qualitative change is `__lock_detect`:
**1.32% → 6.76%, a 5.1x rise in share** on an arm doing 5.2x less work.

### Inclusive (`--children`) is where the signal is

| symbol | t=8 | t=32 |
|---|---|---|
| **`do_payment`** | **26.34%** | **67.30%** |
| `__bamc_search` | 25.73% | 49.50% |
| `__bam_search` | 25.54% | 49.14% |
| `__db_lget` | — | 42.80% |
| `__lock_get` | — | 41.97% |
| `__lock_get_internal` | — | 41.61% |
| **`__lock_detect`** | **8.60%** | **24.88%** |
| `__dd_build` (inlined) | 6.56% | **18.84%** |
| `__lock_vec` | 4.83% | 11.51% |
| `__x64_sys_futex` | 15.22% | 26.29% |
| `do_stock_level` | 43.63% | *(fell out of top 12)* |

At t=32 **two thirds of all cycles are one transaction type**, and the
deadlock detector's waits-for-graph build (`__dd_build`) alone is 18.84%.
Call-graph attribution of `__lock_detect` at t=32:

```
 --6.59%--do_payment
          |--3.46%--put_rec -> __db_put -> __dbc_iput -> __bamc_put
          |          __bamc_search -> __bam_search -> __db_lget
          |          __lock_get -> __lock_get_internal -> __lock_detect
          |          |--2.34%--__dd_build (inlined)
          |           --0.87%--__dd_find (inlined)
           --2.88%--xe_txn_abort -> __txn_abort -> __txn_end
                     __lock_vec -> __lock_detect
```

The detector runs **inline, inside `__lock_get_internal`**, on the blocking
path — `src/lock/lock.c:1634-1635`, immediately after `region->need_dd = 1` and
`LOCK_SYSTEM_UNLOCK` at line 1616/1629 — and `__dd_build` retakes the **global**
`LOCK_SYSTEM_LOCK` (`src/lock/lock_deadlock.c:416`) and walks every locker.

---

## 2. Which locks are WAITED ON (engine counters, not reasoning)

Method: run 10s warmup + 40s measure; `db_stat -XA -Z`, `-c -Z`, `-l -Z` at
t=12s to zero the counters **inside** the measured window; snapshot at t=47s.
Window ≈ 35s of steady state. `db_stat -XA` (not `-m`, which is mpool) prints
per-mutex `[wait/nowait pct%, <name>]`; aggregated by `alloc_id`.
Window verdicts: t=8 **1,017,114 tpm**, t=32 **125,020 tpm**.
Per-txn divisors: 593,316 and 72,930 transactions respectively.

### t=8 — mutex waits, normalised per transaction

| mutex (alloc_id) | n | WAIT (ex+rd) | nowait | pct_wait | acq/txn | **wait/txn** |
|---|---|---|---|---|---|---|
| db handle | 63 | 10,996,403 | 324,452,349 | 3.28% | 565.40 | **18.5344** |
| lock region | 705 | 499,228 | 121,999,209 | 0.41% | 206.47 | 0.8414 |
| env region | 1 | 314,798 | 7,269,982 | 4.15% | 12.78 | 0.5306 |
| txn active list | 1 | 1,605 | 1,531,596 | 0.10% | 2.58 | 0.0027 |
| mutex region | 1 | 359 | 1,558,419 | 0.02% | 2.63 | 0.0006 |
| mpool buffer | 63,223 | 5 | 66,823,885 | 0.00% | 112.63 | 0.0000 |
| mpool hash bucket | 131,048 | 0 | 70,888,415 | 0.00% | 128.75 | 0.0000 |
| log region | 1 | 1 | 16,277 | 0.01% | 0.03 | 0.0000 |

### t=32 — mutex waits, normalised per transaction

| mutex (alloc_id) | n | WAIT (ex+rd) | nowait | pct_wait | acq/txn | **wait/txn** |
|---|---|---|---|---|---|---|
| **lock region** | 705 | **628,522** | 68,861,279 | 0.90% | 952.86 | **8.6184** |
| db handle | 63 | 70,991 | 41,144,652 | 0.17% | 565.16 | 0.9734 |
| env region | 1 | 5,247 | 1,737,781 | 0.30% | 23.90 | 0.0719 |
| txn active list | 1 | 46 | 617,220 | 0.01% | 8.46 | 0.0006 |
| mutex region | 1 | 25 | 620,895 | 0.00% | 8.51 | 0.0003 |
| mpool buffer | 60,416 | 0 | 8,677,298 | 0.00% | — | 0.0000 |
| mpool hash bucket | 131,048 | 0 | 9,389,479 | 0.00% | 128.75 | 0.0000 |
| log region | 1 | 0 | 31 | 0.00% | 0.00 | 0.0000 |

What this says, and what it does not:

* **The lock region is the only mutex class whose waits/txn RISE** with threads:
  0.84 → 8.62, a **10.2x** increase. Its acquisitions/txn also rise 206 → 953
  (4.6x) — the same transaction takes 4.6x as many lock-region trips at t=32.
* **`db handle` waits/txn FALL 19x** (18.53 → 0.97) even though acquisitions/txn
  are flat (565.40 → 565.16). It is the biggest absolute waiter at t=8 and is
  *not* the scaling defect. A diagnosis that had only looked at the t=8 column,
  or at raw counts rather than per-txn, would have named `db handle`.
* **The log region is not a participant.** 0 waits at t=32, and `db_stat -l`
  gives **5,298** region waits at t=32 vs **316,278** at t=8 — the log latch is
  **60x less** contended in the failing arm. Per-txn: 0.0726 vs 0.5331, a 7.3x
  *fall*. Log records/txn are identical (9.33 vs 9.36), so this is not a
  workload-shape artifact. **P5/RFC 0008's log latch cannot be this defect.**

### `db_stat -c` (lock subsystem), same windows

| | t=8 | t=32 |
|---|---|---|
| conflicts for which we **waited** | 441,179 | 485,859 |
| conflicts for which we did **not** wait | 432,623 | 487,741 |
| **Number of deadlocks** | **165,817** | **235,761** |
| partition locks that required waiting | 225,813 (0%) | 343,577 (1%) |
| locker allocations that required waiting | 274,626 (**12%**) | 286,486 (**21%**) |
| region locks that required waiting | 316,262 (4%) | 5,294 (0%) |
| Max locks at any one time | 3,767 | 2,181 |
| Max locks in any one bucket | 16 | **64** |

**Deadlocks per commit: 0.28 at t=8 → 3.23 at t=32.**

---

## 3. The root cause: upgrade deadlock on a hot row set, not a latch

The driver's own per-type output names it:

```
t=8   TXN payment  committed 291117  retries 178536   (0.613 retries/commit)  p99 1904us
t=32  TXN payment  committed  35337  retries 267147   (7.56  retries/commit)  p99 9984us
```

Two independent cross-checks that these are the same events as the engine's
deadlock counter — rates computed over each source's own window:

```
t=8   deadlocks 165817/35s = 4738/s   driver retries 187454/40s = 4686/s   ratio 1.01
t=32  deadlocks 235761/35s = 6736/s   driver retries 267173/40s = 6679/s   ratio 1.01
```

Mechanism, from the driver source: `xe_get` calls
`t->db->get(t->db, txn->dbtxn, &k, &d, 0)` — **no `DB_RMW`**
(`test/bench/xe_engine.h:1019`). `do_payment` then does get→put on
*warehouse*, *district* and *customer* (`xe_tproc_c.c:501-518`): a shared read
lock **upgraded** to exclusive, three times per transaction. Two concurrent
upgraders on the same row deadlock by construction. The hot row count is fixed
by the **warehouse count (scale)**, not by the thread count, so collision
probability grows ~t² while available rows stay constant.

### The controlled test: vary ONLY the hot-row count

Same engine, same build, same box, same thread count; scale 96 → 960
(10x more warehouses). 3 alternating reps:

```
DATA scale=96  t=32 rep=1 tpm=124314  commit=17576  retry=132647
DATA scale=96  t=32 rep=2 tpm=123325  commit=17456  retry=132792
DATA scale=96  t=32 rep=3 tpm=124748  commit=17594  retry=133059
DATA scale=960 t=32 rep=2 tpm=1324181 commit=190170 retry=4504
DATA scale=960 t=32 rep=3 tpm=1371208 commit=196701 retry=3369
```

**11x throughput; retries fall 39x.** And the shape *inverts* — scale=960,
3 reps alternating t=8/t=32:

```
DATA scale=960 t=8  rep=1 tpm=1454530 commit=208286 retry=374
DATA scale=960 t=32 rep=1 tpm=1428506 commit=205094 retry=11676
DATA scale=960 t=8  rep=2 tpm=1382342 commit=197984 retry=275
DATA scale=960 t=32 rep=2 tpm=1469305 commit=210848 retry=7270
DATA scale=960 t=8  rep=3 tpm=1372796 commit=196387 retry=308
```

The gate's own verdict logic, run on scale=960:

```
tolerance: max(5.0% floor, 3.0 x cv(t=8)=1.61%) = 5.0%
step:      tpm(t=32 )=1411464   vs tpm(t=8)=1415617    =     -0.3%   ok
VERDICT scale-shape PASS threads_lo=8 threads_hi=32 tpm_lo=1415617 tpm_hi=1411464 delta_pct=-0.3 tol_pct=5.0
```

**The same binary FAILS at −87.6% (scale=96) and PASSES at −0.3% (scale=960).**
The only variable is the number of distinct hot rows.

### Detector-policy arm: the deadlocks are REAL, not a detector artifact

Via `DB_CONFIG` only (read at `env->open`, `src/env/env_open.c:476`, i.e.
*after* the driver's `set_lk_detect`, so it wins — confirmed by `db_stat`
rejecting a mismatched region with `BDB2041 lock_open: incompatible deadlock
detector mode`). Setting `set_lk_detect db_lock_expire`, which makes
`__lock_detect` take the EXPIRE-only path and never build the waits-for graph:

```
rc=124 (timed out)
# warmup window 0: 28 txn/s
```

The no-detector arm **collapses to 28 txn/s and hangs** at t=4. So the detector
is not manufacturing the aborts — it is *resolving* genuine cycles, and its
24.88% of cycles is the **cost of cleaning up** contention, not its cause.
Removing or cheapening the detector would make this workload worse, not better.

---

## 4. The valsz sweep, and what it means for D0

`d0_probe`, `rows_per_txn=4`, `empty_data=0`, 15s, 3 reps, arms alternating
(the full valsz list is walked once per rep).

### t=32

| valsz | rows/s (3 reps) | median | cv | rec/row | rel. to valsz=8 | region_wait/txn |
|---|---|---|---|---|---|---|
| 8 | 47845, 51488, 51176 | **51,176** | 4.02% | 2.276 | 1.000 | 1.895–1.982 |
| 100 | 18163, 20217, 20337 | **20,217** | 6.24% | 2.343 | 0.395 | 0.759–0.833 |
| 1000 | 5533, 6451, 6512 | **6,451** | 8.90% | 3.258 | 0.126 | 0.005–0.016 |
| 4000 | 5437, 6188, 6418 | **6,188** | 8.53% | 4.280 | 0.121 | 0.010–0.012 |

### t=1

| valsz | rows/s (3 reps) | median | cv | rec/row | rel. to valsz=8 | region_wait/txn |
|---|---|---|---|---|---|---|
| 8 | 55354, 54285, 54678 | **54,678** | 0.99% | 2.276 | 1.000 | 0.000 |
| 100 | 26128, 29413, 36768 | **29,413** | **17.71%** | 2.343 | 0.538 | 0.000 |
| 1000 | 19528, 19577, 17526 | **19,528** | 6.20% | 3.261 | 0.357 | 0.000 |
| 4000 | 20473, 17931, 20630 | **20,473** | 7.70% | 4.276 | 0.374 | 0.000 |

### Does the effect scale with thread count? Yes — but only ~3x, not infinitely

```
EFFECT SIZE valsz 8 -> 4000:
  t=1     54678 ->   20473 rows/s  = -62.6%  (ratio 2.67x)
  t=32    51176 ->    6188 rows/s  = -87.9%  (ratio 8.27x)
amplification = 8.27 / 2.67 = 3.10x

Throughput LOST going t=1 -> t=32, per valsz:
  valsz=8     54678 ->  51176  =  -6.4%
  valsz=100   29413 ->  20217  = -31.3%
  valsz=1000  19528 ->   6451  = -67.0%
  valsz=4000  20473 ->   6188  = -69.8%
```

**Value size is strongly thread-count-dependent.** At valsz=8 there is
essentially *no* negative scaling (−6.4%); by valsz=1000 it is −67%. Bytes do
not merely cost time, they cost *scalability*. That is a real finding and it
does support the D0 qualification's direction.

### Two caveats that must not be dropped

**(a) The sweep does NOT hold record count fixed, so it is confounded.**
`rec_per_row` is not constant across the sweep: **2.276 → 2.343 → 3.258 →
4.280**. Larger values mean more records per row (overflow pages / more
`__db_addrem` records), so "valsz" varies bytes *and* record count together.
The task framing assumed fixed record count; the probe's own counter says
otherwise. Removing the confound by comparing *log records per second*:

```
  t=1    124447 ->  87624 recs/s = -29.6%
  t=32   116477 ->  26485 recs/s = -77.3%
```

The amplification survives (−29.6% vs −77.3%), so the conclusion holds in
direction — but the −87.9% figure is **not** a pure bytes effect and should not
be quoted as one.

**(b) At the value sizes where the effect is largest, the log latch is
*un*contended.** `region_wait/txn` from the probe's own `log_stat`:
**1.9 at valsz=8 → 0.01 at valsz=4000**, a 190x *fall*, while throughput falls
8.3x. The arm that loses the most throughput waits on the log region the
**least**. So whatever large values cost, it is **not** log-latch handoff — it
is per-record work and overflow-page handling further up. The t=1 column
confirms it: valsz 8→4000 loses 62.6% with **zero** log-region waits and no
concurrency at all.

### Verdict on D0

**The data does not support implementing D0 as specified, and does not support
the by-reference redesign either — because it does not support the premise
shared by both.**

* D0's premise (RFC 0008 Finding 3) is that **log latch acquisitions per
  transaction** govern throughput. In the failing TPROC-C arm the log region
  records **0 mutex waits** and 5,298 region waits vs 316,278 at t=8 — per-txn,
  7.3x *fewer*. Halving `__db_addrem` records would halve acquisitions of a
  latch that **nothing is waiting for**.
* The valsz sweep shows bytes matter and scale with threads (3.1x
  amplification), which is the by-reference design's motivation — but the same
  sweep shows log-region waits going to **~zero** exactly where the loss is
  largest. So the by-reference design would be aimed at the wrong latch too.
  The cost is per-record and overflow-page work, not handoff.
* Both would be measured against a workload whose t=32 collapse is **67.30%
  one transaction type** doing read-then-upgrade on ~96 rows, resolved by a
  detector that consumes 24.88% of cycles. D0's ±tens-of-percent would sit
  inside that, unmeasurable.

**Recommendation:** do not build D0 now. Its projection rests on a latch this
box shows to be idle in the failing arm. Re-derive the premise with a workload
that does not spend two thirds of its cycles on upgrade deadlock, then re-ask.

---

## 5. The named serialisation point

**I name it — but it is not a latch, and that distinction is the finding.**

> **At t=32 the dominant serialisation point is the lock-table entry for a
> small set of hot rows, entered through a read-lock-then-upgrade pattern, with
> the global `LOCK_SYSTEM_LOCK` in `__dd_build` as the amplifier.**

Evidence, each from a command run on this box:

1. **The box is idling, not spinning.** 184.0G → 83.6G cycles; task-clock
   53,847 → 39,243 msec; 1.65 → 1.16 CPUs utilized. Threads are blocked and
   discarding work. This alone excludes every "hot latch" story.
2. **Only the lock region's waits/txn rise:** 0.84 → 8.62 (10.2x), with
   acquisitions/txn 206 → 953. Every other class falls, including the previous
   leader `db handle` (18.53 → 0.97) and the log region (0.53 → 0.07).
3. **Deadlocks per commit 0.28 → 3.23**, cross-validated at **ratio 1.01**
   against the driver's independent retry counters at both thread counts.
4. **67.30% of t=32 cycles are `do_payment`** (26.34% at t=8) — the one
   transaction doing three get→put upgrades, via `db->get(..., 0)` with no
   `DB_RMW`.
5. **`__dd_build` is 18.84% inclusive**, and it takes the global
   `LOCK_SYSTEM_LOCK` (`lock_deadlock.c:416`) on the **blocking** path
   (`lock.c:1634-1635`) — so each blocked locker serialises every other
   locker behind a whole-table walk. Amplifier, not origin.
6. **Diluting the hot rows 10x fixes it**: 124,748 → 1,371,208 tpm at t=32,
   retries 133,059 → 3,369, and the gate flips **FAIL −87.6% → PASS −0.3%** on
   the same binary.
7. **Removing the detector makes it worse** (`set_lk_detect db_lock_expire`:
   28 txn/s, rc=124 hang), proving the cycles are genuine.

### What would have to change

In priority order, by the evidence:

1. **Acquire write intent on the first touch.** `xe_get` must pass `DB_RMW`
   where the transaction will write the row back. This removes the upgrade, and
   therefore the deadlock cycle, at the source. **This is a change to
   `test/bench/xe_engine.h` — i.e. to the BENCHMARK, not to libdb.** Which
   means a large part of the measured "libdb negative scaling" is the
   driver asking the engine for a lock mode no sane OLTP client would use.
   This needs to be established before any engine work is justified by this
   gate.
2. **Make the comparison honest about scale.** TPROC-C at scale=96 with 32
   threads is ~3 threads per warehouse; real TPC-C fixes terminals per
   warehouse. The committed baseline and the gate both run scale=96 at t=8,
   t=32 **and t=96**, so the higher thread counts measure row contention that
   the specified workload shape creates. Either scale with threads or
   state plainly that the gate measures behaviour under deliberate hot-row
   oversubscription.
3. **Only then, engine work.** Move `__lock_detect` off the synchronous
   blocking path (`lock.c:1634`) — a dedicated detector thread, or a
   wait-then-detect delay — so a blocked locker does not serialise all
   lockers behind `LOCK_SYSTEM_LOCK`. Worth up to the measured 24.88%, but it
   treats the amplifier; items 1–2 treat the cause.

### What I explicitly refuse to claim

* **Not a diffuse defect.** Self-time looks diffuse (max 8.77%) and that
  reading has misled this project before; inclusive attribution and the
  counters both converge on one point.
* **Not the log latch.** 0 mutex waits, 60x fewer region waits at t=32.
  P5/RFC 0008 is measuring a real cost that is **not** this one.
* **Not a mutex-implementation or spin problem.** This build has no TAS path
  at all (`HAVE_MUTEX_HYBRID` undefined, confirmed by `nm`), and the box is
  *under*-utilizing CPU.
* **Not a bytes-under-the-log-latch problem**, which is what the D0
  qualification raised. Bytes do cost scalability (3.1x amplification), but
  log-region waits/txn fall 190x across the same sweep.
* **I have not shown the −87.6% is entirely workload-induced.** scale=960
  removes it at t=32, but I did not test t=96 at scale=960, and I did not
  test a `DB_RMW` arm (that needs a driver edit, out of scope here). The
  honest statement: at scale=96 the collapse is dominated by upgrade
  deadlock; how much engine defect remains underneath is **unmeasured**.

---

## 6. Incidental: a real NULL-deref crash in the optimistic read path

Not what I was asked to find, but it is a reproducible SIGSEGV in libdb and it
cost me two benchmark reps (`NO_VERDICT rc=139`), so it is data.

**3 of 8 runs at scale=960, t=32 crashed.** Core-dump backtrace:

```
#0  __memp_fget_opt_valid (sample=0x7f8dcd7f7980) at ../src/mp/mp_fget.c:1503
#1  __bam_search (dbc=0x7f8d68002a20, root_pgno=1, ... flags=257, slevel=1) at ../src/btree/bt_search.c:954
#2  __bamc_search (... root_pgno=0 ...) at ../src/btree/bt_cursor.c:2804
#3  __bamc_get  #4 __dbc_iget  #5 __dbc_get  #6 __dbc_get_pp
#7  xe_cursor_seek_ge at xe_engine.h:1203
#8  do_delivery at xe_tproc_c.c:605

(gdb) p *sample
$1 = {bhp = 0x0, pgno = 17058, mf_offset = 922072, gen = 96 '`'}
```

`bhp` is **NULL**. `BH_SAMPLE_VALID` (`src/dbinc/mp.h:817-821`) dereferences
`(s)->bhp->gen` with no NULL guard, and `__memp_fget_opt_valid`
(`mp_fget.c:1503`) passes it straight through.

Reading the control flow, the likely path: at `bt_search.c:898` the stale-root
*snapshot* branch does `from_snap = 0; ... goto retry;` — jumping to `retry:`
at line 868 **without clearing `from_opt`**. Note frame #1 shows
`root_pgno=1, root_pgno@entry=0`, i.e. a retry did occur. If `from_opt` was
set, `opt_parent` was already released at line 880 (and
`__memp_fget_opt_release` sets `sample->bhp = NULL`, `mp_fget.c:1538`), so the
re-descent reaches the validation at line 954 with a released sample. Same
shape at line 994.

I did not build a patch or a reducer for this — out of scope, and I was told
not to modify the repo. Flagging it as its own item: a NULL guard in
`BH_SAMPLE_VALID` would stop the crash, but the actual defect is the retry path
not re-arming or clearing `from_opt`, and that deserves its own diagnosis.

---

## 7. Reproduction

```sh
# host: c6id.16xlarge, 64 vCPU, Fedora 43, /nvme XFS, THP never
sudo dnf install -y git gcc make perf gdb
git clone -q https://github.com/berkeleydb/libdb.git /nvme/w          # 750fb0302
mkdir -p /nvme/w/build_unix && cd /nvme/w/build_unix
CFLAGS="-O3 -g -fno-omit-frame-pointer" ../dist/configure && make -j64
cd /nvme/w/test/bench
cc -O2 -g -pthread -I/nvme/w/build_unix -I. xe_tproc_c.c \
   /nvme/w/build_unix/.libs/libdb-2026.0.so \
   -Wl,-rpath,/nvme/w/build_unix/.libs -lm -o xe_tproc_c
cc -O2 -g -pthread -I/nvme/w/build_unix -I. d0_probe.c \
   /nvme/w/build_unix/.libs/libdb-2026.0.so \
   -Wl,-rpath,/nvme/w/build_unix/.libs -lm -o d0_probe
sudo sysctl -w kernel.perf_event_paranoid=-1 kernel.kptr_restrict=0

# teeth, then the defect
BUILD=/nvme/w/build_unix ./scale_shape_gate.sh --self-test
./xe_tproc_c -i -e libdb -a btree -h /nvme/shape_data/d -S 96 -P 256 -c 4294967296
touch /nvme/shape_data/.loaded_S96
SHAPE_DATA=/nvme/shape_data BUILD=/nvme/w/build_unix \
  ./scale_shape_gate.sh -r 3 -s 20 -W 10 -n "8 32" -o /nvme/res_repro.tsv

# profile (serialised)
for t in 8 32; do
  perf record -q --call-graph dwarf,16384 -F 199 -o /nvme/perf_t$t.data -- \
    ./xe_tproc_c -e libdb -a btree -h /nvme/shape_data/d -S 96 -P 256 \
    -c 4294967296 -t $t -s 20 -W 10
done
perf report -i /nvme/perf_t32.data --children --stdio -g none --comms xe_tproc_c

# waits: zero inside the window, snapshot before the run ends
/tmp/wait_probe.sh 8 40 ; /tmp/wait_probe.sh 32 40
python3 /tmp/mtxagg.py 72930 < /nvme/wp_mtx_t32.txt

# the controlled hot-row test
./xe_tproc_c -i -e libdb -a btree -h /nvme/big/d -S 960 -P 256 -c 4294967296
/tmp/scale_ab.sh 32 20 3

# valsz sweep
/tmp/valsz_sweep.sh 32 15 3 4 ; /tmp/valsz_sweep.sh 1 15 3 4
```

Helper scripts live in `/tmp` on the host (`wait_probe.sh`, `mtxagg.py`,
`dd_ab.sh`, `scale_ab.sh`, `valsz_sweep.sh`); raw logs, TSVs and `perf.data`
files are under `/nvme`. Nothing was written to the repo.
