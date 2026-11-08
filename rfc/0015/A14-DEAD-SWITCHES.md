# A14 — the dead concurrency switches: delete

Review item A14 (Bostic, Design Lesson 11): *finish or delete the
off-by-default concurrency switches.* Two switches were evaluated and both are
**deleted**. The evidence is `test/bench/METAL-AB-2026-10.md` and the code
comments the switches carried.

## M1 — the `mtx_regenv` latch split (`DB_TXN_LATCH_SPLIT`, BDB5317)

Shipped inert: `099224090` neutered the split so `mtx_region` was always aliased
to `mtx_regenv`, and a `getenv("DB_TXN_LATCH_SPLIT")` only printed BDB5317 and
otherwise did nothing.

The metal A/B is decisive on **two independent grounds**, either sufficient
(`METAL-AB-2026-10.md` §2, §4):

1. **Unsafe.** `m1` SIGSEGVs (and stalls) under concurrency at t≥32 — 8
   failures across t=32/64/96. The core backtrace shows a corrupted shared
   env-region free list: the split moved `__txn_begin` off `mtx_regenv`, but
   the hot path `__txn_begin_pp → __env_set_state → __env_alloc` still mutates
   the region free list `mtx_regenv` protected, so a split-latch holder and an
   `mtx_regenv` holder corrupt it (`env_alloc.c:354 SH_TAILQ_REMOVE`).
2. **Slower.** Even on surviving reps it is **−31%** at t=96 and worse at every
   t≥8 — the P12 admission-control lesson: it relocates a latch and adds a
   handoff on the hottest path without the win.

A switch that corrupts the environment region can never be a default and is
dangerous even as a documented opt-in. **Deleted** (getenv + BDB5317, the
`TXN_REGION_LATCHSPLIT` flag, and the now-dead `F_ISSET(...LATCHSPLIT)` branches
in `lock.c` and `txn_chkpt.c`, which were always false once the flag was never
set). A future, correct split would be a new change with its own measurement.

## TAS bounded exponential backoff (`DB_MUTEX_BACKOFF`)

A complete opt-in (`7c2c44c1b`): default cap 1 was exactly the historical
one-pause-per-poll loop; `DB_MUTEX_BACKOFF=<cap>` doubled pauses between polls.

It has **no measured win on any hardware we have.** The only measurements, in
its own comment, are on a 16-vCPU Graviton3: cap 16 was a null and cap 64 was
**−7%** at a 2 µs critical section. It was never measured on the many-core metal
the coherence storm it targets would need, and `test/bench/spinbench.sh` carried
`bo16`/`bo64` arms for exactly that measurement that were never run on metal.

An unmeasured-useful switch is the dead switch Design Lesson 11 warns against.
**Deleted** (`__db_tas_backoff_max`, the `TAS_BACKOFF()` macro, the `backoff`/
`backoff_max` locals, and the two call sites revert to `MUTEX_PAUSE`). The spin
budget (`mutex_set_tas_spins`) and the benchmark harness `spin_lat.c` /
`spinbench.sh` stay, so a *measured* metal result could re-justify a backoff.

## No format change

Neither switch changed a region-resident struct. The `TXN_REGION_LATCHSPLIT`
flag was a `#define` bit, never a field; `__db_tas_backoff_max` kept all state
process-local. The region signature is unchanged — verified with
`BUILD_DIR=<b> sh dist/env_sig_print.sh .`.
