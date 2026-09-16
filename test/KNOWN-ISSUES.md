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
| **S1** | `os_aio` deadlock under concurrent checkpoint/trickle/sync, ~2–5 runs in 40–67. Opt-in `DB_MPOOL_AIO`, **default OFF**. The trylock *winner* blocks acquiring `mtx_buf` for a new buffer while holding deferred-write pins, because the deferred path only drains at `nflight >= MEMP_AIO_WINDOW` — hold-and-block across a window that only drains when full. No data loss observed (`lost=0`); recovery and `db_verify` clean in every occurrence. Two candidate fixes named: drain before blocking on a new buffer, or `MUTEX_TRY_READLOCK` and defer. The lock-order checker does **not** cover this class, because `mtx_buf` is a pin rather than an ordered latch. | open, default-off |
| **S5** | A second, independent `lk_partitions=1` failure in multi-process locker teardown (`ssi009` / `BDB2047`). **Not** fixed by the v2026.09.6 latch-alias fix, which addressed a different nesting. | open |

## Why this file exists

Each of these was, at some point, rediscovered from scratch by someone who could
not tell a pre-existing failure from a regression they had just caused. A release
that names its known issues only in its own release notes forces the next reader
to diff release texts. Tracked here instead, cross-referenced from the notes.

Rule: do not delete an entry to make a run green. Either fix it, or record
honestly that it is still open.
