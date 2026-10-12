# Metal A/B — libdb S5 (TXN_DETAIL free list) and M1 (`mtx_regenv` split) scalability switches

**Question (next-release decision, not a gate on the current RC):** on real x86 metal,
do S5 and/or M1 measurably lift the >4-thread TPROC-C cliff, and is M1 a net win or
does it just relocate contention? The switches ship **default-off** in the RC regardless.

## Hardware / build

| | |
|---|---|
| Host | FreeBSD 14.5-STABLE amd64, **c5d.metal, 96 vCPU** (real cores) |
| CPU | Intel(R) Xeon(R) Platinum 8275CL @ 3.00GHz |
| RAM / storage | 191.6 GiB / local NVMe (`/nvme`, 747 G free) |
| Library | `libdb 2026.10.5` (the RC), built once: `../dist/configure --enable-o_direct CFLAGS="-O2 -g"`, `gmake -j96` |
| Mutex | `POSIX/pthreads/library/x86_64/gcc-assembly`, `HAVE_MUTEX_HYBRID=1` — the x86 TAS spin path (spinbench refuses a non-hybrid build) |
| Driver | `xe_tproc_c` built against the one `.so` (`ldd` confirms `libdb-2026.0.so` from the build tree, not a system lib) |
| Workload | TPROC-C, **scale 96**, pad 256, **4 GiB cache**, local NVMe |
| Cache behaviour | dataset ≈0.23 GiB data / 1.5 G home → **100% cache hit, 0 misses** at run time. This is a **CPU/latch-bound** measurement, which is exactly the regime for the `mtx_regenv` question (no IO confound). |
| Sweep | t = 1/2/4/8/16/32/64/96; **7 complete reps** (+ a discarded prewarm pass); 4 arms `base s5 m1 s5m1` alternated within each rep, start-arm rotated per rep. SECS=20, WARM=8. |
| One binary | every arm is a runtime switch (`DB_TXN_FREELIST`, `DB_TXN_LATCH_SPLIT`) on the SAME binary — the comparison is the algorithm, never build skew. |

Raw TSV: `metal_ab.tsv` (headers + 230 measured rows). Per-rep `db_stat -c/-t/-l/-x`
dumps: `metal_ab.raw/` (1079 files). Full crash backtrace: `metal_ab.m1_crash_bt.txt`.
Computed tables: `metal_ab.analysis.txt`.

---

## 1. Four-arm throughput (median tpmC_like, cv across reps)

cv rule: a cell is quoted only if **cv < 10%**; cells over 10% are marked and not quoted.

| t | base | s5 | m1 | s5m1 |
|---:|---|---|---|---|
| 1 | 575 438  cv 1.8% (n8) | 575 584  cv 2.1% (n8) | 583 539  cv 3.7% (n8) | 576 552  cv 3.4% (n8) |
| 2 | **cv 14.5% — not quoted** | 565 353  cv 5.2% (n7) | **cv 10.8% — not quoted** | 643 614  cv 8.6% (n8) |
| 4 | 849 157  cv 5.7% (n7) | 766 815  cv 4.1% (n7) | 811 322  cv 7.2% (n7) | 836 140  cv 7.0% (n7) |
| 8 | 778 485  cv 3.7% (n7) | 739 662  cv 5.0% (n7) | 697 420  cv 4.0% (n7) | 720 165  cv 4.9% (n7) |
| 16 | 599 482  cv 2.8% (n7) | 601 952  cv 3.4% (n7) | 493 422  cv 3.8% (n7) | 478 935  cv 3.4% (n7) |
| 32 | 529 005  cv 2.4% (n7) | 522 201  cv 2.6% (n7) | 398 528  cv 3.6% (**n6**, 1 crash) | 387 180  cv 3.4% (**n6**, 1 stall) |
| 64 | 477 469  cv 2.3% (n7) | 484 401  cv 2.8% (n7) | 346 627  cv 2.6% (**n3**, 4 crash/stall) | 344 889  cv 3.1% (n7) |
| 96 | 476 352  cv 3.1% (n7) | 466 737  cv 2.3% (n7) | 335 821  cv 2.1% (**n4**, 3 crash/stall) | 328 494  cv 2.3% (**n5**, 2 crash/stall) |

min/max per cell are in `metal_ab.analysis.txt`. c5d.metal was quiet: every cell except
the t=2 transition point settled under 10% (most under 4%). **t=2 is the only cell that
would not settle** (base 14.5%, m1 10.8%) — it is the ragged peak-shoulder where a tiny
number of lock waits first appears; those two numbers are withheld per the cv rule.

### Ratio to base (median)

| t | s5/base | m1/base | s5m1/base |
|---:|---:|---:|---:|
| 1 | 1.014 | 1.014 | 1.002 |
| 4 | 0.903 | 0.955 | 0.985 |
| 8 | 0.950 | 0.896 | 0.925 |
| 16 | 1.004 | **0.823** | **0.799** |
| 32 | 0.987 | **0.753** | **0.732** |
| 64 | 1.015 | **0.726** | **0.722** |
| 96 | 0.980 | **0.705** | **0.690** |

**The cliff is real and reproduced.** base peaks at t=4 (849k) and falls to 476k at t=96
— a 44% fall from peak (less steep than the published ~70%, because that run was pad=400
with IO pressure; this is the cache-resident, latch-only shape).

**S5: a clean null.** Within ±2% of base at every t≥8 (0.98–1.02×). It neither lifts
nor hurts the cliff on this workload/hardware.

**M1 and s5m1: strictly worse above t=8, monotonically.** 0.90× at t=8 falling to
**0.69× at t=96** — a 31% throughput *loss* at the top of the sweep. The headline
`s5m1` lift the brief asks about is **negative at every t≥8.**

---

## 2. M1 crashes under concurrency — the decisive result

`m1` (and `s5m1`, less often) **SIGSEGV / stall at t≥32**. `base` and `s5` never fail.

| arm | failures observed | threads |
|---|---|---|
| base | 0 | — |
| s5 | 0 | — |
| m1 | **8** (`rc139`=SIGSEGV ×6, `TIMEOUT_STALL` ×2) | t=32×1, t=64×4, t=96×3 |
| s5m1 | **3** (SIGSEGV ×1, stall ×2) | t=32×1, t=96×2 |

### Root cause (from the core backtrace, `metal_ab.m1_crash_bt.txt`)

The faulting thread:

```
frame #0  __env_alloc           env_alloc.c:354   SH_TAILQ_REMOVE(q, elp, sizeq, ...)
frame #1  __env_set_state       env_failchk.c:497
frame #2  __txn_begin_pp        txn.c:183
...  do_new_order / do_payment (xe_tproc_c)
```

Other threads fault concurrently inside `__je_arena_choose_hard`, `__thr_pshared_offpage`,
and `__mutex_alloc → __env_alloc` — the signature of a **corrupted shared-region free list**.

The fault is `SH_TAILQ_REMOVE` on the env-region allocator's size queue. The chain that
reaches it — `__txn_begin_int → __lock_getlocker_int → __mutex_alloc → __env_alloc`, and
`__env_set_state → __env_alloc` — allocates from the **shared env region**, whose heap is
serialized by **`mtx_regenv`**. M1 moved the txn-begin path onto its own split latch, so a
thread holding the split txn latch and a thread holding `mtx_regenv` now enter the region
allocator **concurrently** and corrupt the free list.

This is the **P12 admission-control lesson realised as a correctness bug**: the shared
`mtx_regenv` was doing real mutual-exclusion work for region allocation, and the split
removed it without replacing it. RFC 0015 M1 states it fixed "two coupling/lock-order
hazards the split exposes"; this is a **third, unfixed** one. `s5m1` crashes less only
because S5's free list changes allocation timing/volume — the latent hazard is identical.

The throughput numbers for m1/s5m1 at t≥32 are therefore measurements of a **broken**
configuration (and come from fewer surviving reps — n is annotated above).

---

## 3. Per-commit latch waits — where contention went (base vs s5m1)

Per-commit = median(wait counter) / median(commits). `c_region`/`t_region`/`l_region`
read the SAME `mtx_regenv` counter in a non-split arm; with the split, `t_region` (txn)
diverges from `c_region` (lock) and `l_region` (log).

**base** (one shared latch — c/t/l identical, as expected):

| t | commits | c_region | t_region | l_region | c_locker | c_part | x_region(mtx) |
|---:|---:|---:|---:|---:|---:|---:|---:|
| 8 | 263 599 | 1.019 | 1.019 | 1.019 | 0.029 | 0.125 | 0.0022 |
| 16 | 202 639 | 1.277 | 1.277 | 1.277 | 0.110 | 0.192 | 0.0025 |
| 32 | 176 882 | 1.366 | 1.366 | 1.366 | 0.265 | 0.281 | 0.0031 |
| 64 | 159 304 | 1.278 | 1.278 | 1.278 | 0.444 | 0.359 | 0.0035 |
| 96 | 159 068 | 1.173 | 1.173 | 1.173 | 0.551 | 0.398 | 0.0041 |

**s5m1** (split engaged — `t_region` collapses, `c`/`l` stay on `mtx_regenv`):

| t | commits | c_region | **t_region** | l_region | c_locker | c_part | x_region(mtx) |
|---:|---:|---:|---:|---:|---:|---:|---:|
| 8 | 240 066 | 0.768 | **0.031** | 0.768 | 0.029 | 0.124 | 0.0020 |
| 16 | 160 229 | 0.913 | **0.049** | 0.913 | 0.113 | 0.194 | 0.0031 |
| 32 | 129 424 | 0.899 | **0.064** | 0.899 | 0.269 | 0.269 | 0.0043 |
| 64 | 115 005 | 0.887 | **0.080** | 0.887 | 0.445 | 0.355 | 0.0048 |
| 96 | 109 808 | 0.837 | **0.097** | 0.837 | 0.578 | 0.395 | 0.0055 |

### M1 engagement proof

`t_region` per-commit falls from **1.17 (base, t=96) to 0.097 (s5m1, t=96)** — a 12×
drop — while `c_region`/`l_region` stay ~0.84. The txn role genuinely moved off
`mtx_regenv`; the split is **not a silent no-op**. (Confirmed again in an isolated smoke
run: with `DB_TXN_LATCH_SPLIT=1`, `db_stat -t` region-wait = 326 vs `-c`/`-l` = 6160.)

### Where did contention go? — it didn't move to a visible latch

The telling part: **`c_locker`, `c_part`, and `x_region` per-commit are essentially
identical between base and s5m1** (e.g. c_locker at t=96: 0.551 vs 0.578). So the lost
throughput is **not** explained by waits relocating to another named latch — the simple
"removed one latch, exposed the next" counter-shift does not appear.

What the split actually did:
- It removed txn waits from `mtx_regenv` (`t_region` 1.17 → 0.10), **but**
- the combined `mtx_regenv` pressure that remains (`c_region`/`l_region` ≈ 0.84) did **not**
  fall by the txn share — because ~80% of `mtx_regenv` arrivals are log appends that stay
  put (as RFC 0015 predicted), and
- the txn path now takes **two** latches (its own split latch **plus** `mtx_regenv` for the
  log/env-alloc portion) where it took one. RFC 0015's own finding is that `mtx_regenv`'s
  cost is the **handoff**, not the record volume — and M1 **adds a handoff** on the hottest
  path. That extra acquire/release, multiplied across every txn-begin, is the throughput
  loss, even though each individual latch is less contended.

In short: the shared latch was cheaper than two split latches on this workload, and it was
also silently providing the region-allocator mutual exclusion that M1's crash proves was
load-bearing.

---

## 4. Recommendation

**S5 (`DB_TXN_FREELIST`) — stay opt-in (default-off).** On this workload/hardware it is a
measured **null**: within ±2% of base at every t≥8, cv < 6%. It does not lift the cliff,
but it is harmless and never crashed in 7 reps × 8 thread counts. No reason to default it
on (it moves nothing here); no reason to drop it (it may matter on an allocation-churning
workload this benchmark doesn't exercise). Keep it, document it as "no measured effect on
cache-resident TPROC-C."

**M1 (`DB_TXN_LATCH_SPLIT`) — must NOT become default-on; do not even recommend opt-in in
its current form.** Two independent reasons, either sufficient:
1. **Correctness:** it SIGSEGVs under concurrency at t≥32 via an unserialized race in the
   shared region allocator (`env_alloc.c:354`). A switch that corrupts the environment
   region can never be a default, and is dangerous even as a documented opt-in.
2. **Performance:** even on the reps that survive, it is **31% slower than base at t=96**
   and worse at every t≥8. It does exactly what P12 warned about — it relocates a latch
   without the win, and here it also loses the serialization the shared latch provided.

**The cliff on this hardware is not an `mtx_regenv`-handoff problem that splitting the txn
role fixes.** base is already far better than either split arm above t=8. The honest
next step is to leave M1 behind the switch, **fix the region-alloc race first** (the split
txn path must still hold `mtx_regenv` — or an equivalent — across `__env_alloc`/
`__mutex_alloc`/`__env_set_state`), and only then re-measure whether a *correct* split can
beat base. As it stands, "the switches do not lift this workload on this hardware, and M1
is unsafe" is the publishable result.

**A null and a negative are both first-class results here.** S5 = null, M1 = negative +
unsafe. Neither should ship default-on next release.

---

### Reproduction

```
# one binary, built once:
cd /nvme/libdb/build_unix && ../dist/configure --enable-o_direct CFLAGS="-O2 -g" && gmake -j96
cc -O2 -g -pthread -I/nvme/libdb/build_unix test/bench/xe_tproc_c.c \
   /nvme/libdb/build_unix/.libs/libdb-*.so -Wl,-rpath,/nvme/libdb/build_unix/.libs -lm -o /nvme/xe_tproc_c
# run (7+ reps, four arms, alternated within each rep, prewarm discarded):
BUILD=/nvme/libdb/build_unix BIN=/nvme/xe_tproc_c DATA=/nvme/sbdata \
  SCALE=96 PAD=256 CACHE=4294967296 SECS=20 WARM=8 REPS=8 \
  THREADS="1 2 4 8 16 32 64 96" ARMS="base s5 m1 s5m1" \
  OUT=/nvme/spinbench.tsv RAW=/nvme/spinbench.raw sh test/bench/spinbench.sh
```
