# Known test-suite and harness issues

Issues that make a test tier fail or skip without indicating an engine defect,
plus engine defects that are shipped knowingly. Every entry is reproduced on a
pristine baseline before being listed here, so "pre-existing" is a measurement
and not an assumption.

**These `T` labels are distinct from the `G` labels in
`docs/design/global-invariants.md`.** The `G` gaps number *invariant-coverage*
holes ("no test targets this invariant"); the `T` issues below are *harness and
test-suite* problems. `G4` in the invariants note (wired frames have no failchk
story) has nothing to do with `T1` here. The two schemes were briefly conflated
in the v2026.09.6 release notes; if you are following a `G4`/`G5`/`G6` reference
from that release text, it means `T1`/`T2`/`T3`.

| id | issue | status |
|----|-------|--------|
| **T1** | `ssi_gc_pressure@serializable` was tuning-sensitive. **FIXED** in two parts, both measured: (1) the marker sweep fired on every `txn_begin` and reclaimed *nothing*, because one long-lived transaction pins `__txn_oldest_reader` and the per-marker LSN gate then retains every committed reader's marker — each pinning a `TXN_DETAIL` until the **txn** region cannot allocate (`BDB4525` is `txn.c:516`, not the lock region, so the trigger divisor was never the lever); `__lock_siclean_obj` now coalesces provably interchangeable markers. (2) the anti-vacuity check asserted on a *harness-sampled* peak, which shrinks as GC improves, so it failed precisely when the mechanism worked; it now uses the engine's `st_maxnlocks`. Cliff was 525 (sharp, 5/5 reps each side); the gate now passes to `SSI_GC_FILLER=50000` with no cliff found, marker high-water flat at 324 across a 100× transaction-count range. See `test/isolation/SSI-GC-MARGIN.md`. | **fixed**, ≥95× margin |
| **T2** | ASan reports a bad free in the `lockmatrix` harness. In the harness, not the engine. | open |
| **T3** | `run_upgrade` hash-v5 fixture missing, so the upgrade pass skips. | open, needs the fixture |
| **T4** | `cutest` aborts (exit 134, `BDB2032` unlock 632). | open, pre-existing |
| **T5** | `log_put.c` zero-length `memcpy` UBSan note. | open, benign but real |
| **T6** | `clang` absent on some runners, so the lockmatrix tier runner cannot run there. | environmental |

## Shipped engine defects

| id | issue | status |
|----|-------|--------|
| **S1** | `os_aio` stall under concurrent checkpoint/trickle/sync. Opt-in `DB_MPOOL_AIO`, **default OFF**. **FIXED** — the deferred-write path held buffer pins (ref + shared `mtx_buf`) across a wait, because it drained only at `nflight >= MEMP_AIO_WINDOW`. Measured baseline 11/192 (5.7%, 95% CI 3.2–10.0%); sharpened to 18/96 with a larger window. gdb showed **two** variants, correcting the original one-variant diagnosis: (A) 6/18 — the winner blocks in `MUTEX_READLOCK(bhp->mtx_buf)` for a new buffer while a splitting writer needs a pinned buffer exclusive; (B) 12/18 — the winner never blocks, every remaining buffer is `BH_EXCLUSIVE` held by the writer that is itself waiting on one of *its* pins, so it spins the `required_write` retry loop forever while RUNNABLE, with no mutex wait in its own backtrace. Fix: never wait holding deferred pins — drain before the retry-loop yield, and `MUTEX_TRY_READLOCK` + drain before the blocking acquire. Both needed. Post-fix 0/384 sharpened and 0/384 at the shipped window. `lost=0`, recovery + `db_verify` clean throughout, and a write error still surfaces. See `docs/design/os-aio-deadlock-fix.md`. | **fixed**, still default-off |
| **S5** | A second, independent `lk_partitions=1` failure in multi-process locker teardown (`ssi009` / `BDB2047`). **Not** fixed by the v2026.09.6 latch-alias fix, which addressed a different nesting. | open |

## Why this file exists

Each of these was, at some point, rediscovered from scratch by someone who could
not tell a pre-existing failure from a regression they had just caused. A release
that names its known issues only in its own release notes forces the next reader
to diff release texts. Tracked here instead, cross-referenced from the notes.

Rule: do not delete an entry to make a run green. Either fix it, or record
honestly that it is still open.
