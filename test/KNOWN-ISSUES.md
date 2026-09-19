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

## Known defects on unmerged branches

Not shipped, so not release issues — recorded because the branch still exists and
someone may pick it up.

| id | issue | status |
|----|-------|--------|
| **B1** | `perf/bhpin-r1` has a **latent correctness bug**, independent of its throughput. Its option (c) returns a buffer frame without taking `bhp->mtx_buf`, but `__memp_fput` is unchanged and unlocks it unconditionally at `mp_fput.c:197`. A DIAGNOSTIC build panics (`BDB2031 shared unlock N already unlocked`). **A production build does not check**: the `DB_ASSERT(env, sharecount > 0)` in `mut_tas.c` is DIAGNOSTIC-only, so `atomic_dec` runs regardless and silently drives another reader's share count toward zero — and since exclusive acquisition CASes against `sharecount == 0`, a writer can be granted the exclusive latch while a reader still holds a share. Silent data corruption under a race, not a crash. Controls: only bhpin+DIAGNOSTIC+private panics; base clean in the same config; both production and both shared-env arms clean. **Do not merge this branch on correctness grounds, regardless of any throughput number.** Its measured "neutral" verdict was also vacuous — 0 hits in 9M attempts, because the load left every page `BH_DIRTY` and shared envs never attempt the path (gated on `ENV_PRIVATE`). Full analysis: `test/bench/PIN-REMEASURE-2026-09.md`. | open, branch not merged |

## Confirmed engine bugs found by benchmarking

| id | issue | status |
|----|-------|--------|
| **P2** | **`DB_DIRECT_DB` (O_DIRECT on data files) is broken.** The first metadata read fails `EINVAL`: `__fop_read_meta` (`src/fileops/fop_util.c:1115`) passes a caller-supplied buffer straight to `__os_read`, and its callers declare it as a plain stack array — `u_int8_t mbuf[DBMETASIZE]` at `src/fileops/fop_rec.c:71,146,360` and the same pattern elsewhere — with no alignment attribute. `O_DIRECT` requires the buffer, the file offset and the length all to be block-aligned (512 B or 4 KiB), so the read is rejected before any database opens. Found while trying to keep the OS page cache out of a cross-engine benchmark; the campaign had to cap the page cache with a cgroup instead. **No test exercises the flag against a real file** — `test/c/cov_api_surface.c` only checks that the setter accepts it, which is why a documented public flag could be completely non-functional. Fix is to give the metadata buffers aligned storage (`__os_malloc` with alignment, or a union with a `DB_ALIGN8`/page-sized member) and to add a test that opens a database with `DB_DIRECT_DB` and reads a page back. | open |

## Measured, characterized, not yet fixed

| id | issue | status |
|----|-------|--------|
| **P1** | **`PGNO_BASE_MD` allocation convoy.** `__db_new` (`db_meta.c:134`) takes the metadata page `DB_LOCK_WRITE` with `LCK_ALWAYS` and releases via `__TLPUT`, which is a **no-op for a write lock inside a transaction** (`__db_lput`'s ladder at `db_meta.c:1416-1427` falls through to `action = 0` when `dbc->txn != NULL` and mode is `DB_LOCK_WRITE`). So one page allocation holds the metadata page write-locked **until commit, across its own ~3.7 ms fsync**, and every other allocating writer queues behind it. Measured at t=96 bulk insert: **400 of 400 waits are on page 0**; leaf and split-time ancestor locks held across commit have **zero** waiters. Offered utilisation 0.89; Little's law predicts 333 ms against a measured put p99 of 282 ms. This is the whole of the write tail latency previously misattributed first to the log and then to leaf/split lock scope. **Fix is a 2PL question** — the metadata page records the free list, so releasing its write lock early risks exposing an allocation a later abort undoes; needs a proper safety argument plus a crash/recovery proof. Full analysis: `test/bench/BTREE-LOCK-SCOPE-2026-09.md`. | open, characterized |

## Pre-existing upstream defects (inherited from Oracle 5.3.28)

Found by a pre-release audit conducted in the voices of the original BDB authors.
Verified present at the `v5.3.28` tag, so these are **not** fork regressions — but
they are silently weakening our own validation, which is why they are tracked.

| id | issue | status |
|----|-------|--------|
| **U1** | `dist/validate/s_chk_err` and `s_chk_pubdef` **print failures and exit 0** — their `exit 1` / `exitv=1` sits inside a subshell, so the value never reaches the caller. `s_validate` therefore reports them as passing whatever they find. Identical at the 5.3.28 import. | open |
| **U2** | `dist/validate/s_chk_message_id` only works when run **from `dist/validate/`**. Its `MSG_DIR` is `../../src/ ../../util/ ../../lang/dbm/`, so invoked from `dist/` every path misses and it exits 0 having checked nothing. It does work correctly from its own directory — that is how the two fork-introduced duplicate IDs were found. | open |
| **U3** | One duplicate `DB_STR()` message id remains: `3675`, used at **29 sites** across the generated `rep_automsg.c` / `repmgr_automsg.c`. Verified inherited: 29 occurrences at `v5.3.28` and 29 at HEAD. **Erratum:** an earlier revision of this row claimed all six then-duplicated ids were upstream's. That was wrong — I had checked whether each id *existed* at `v5.3.28` rather than whether it was *duplicated* there. Five of the six (`1136`, `1169`, `3015`, `3037`, `3672`) appeared **once** upstream and **twice** at HEAD, i.e. our own hardening commits (`4ae82539f`, `36cf8cb13`, `36f300b57`, `3e49ca740`) each copied a neighbouring id. All five are now renumbered to `1176`, `1177`, `3043`, `3044`, `3683`. | open, upstream |
| **U4** | `dist/s_tags` probes `ctags` capabilities against `../../src/db/db.c`, a path that has never existed in this repository. Every probe fails silently (`2>/dev/null`), so `flags` stays empty and `ctags` runs without `-d -t -w`. Degraded developer convenience only. | open, upstream |
| **U5** | `dist/s_crypto` references `docs/index.html`, which this fork does not have. It is an export-restriction tool not driven by `s_all`, so the reference is dormant. | open, upstream |

## Windows build gaps

| id | issue | status |
|----|-------|--------|
| **U6** | **The meson build is not feature-parity with autoconf, so the two produce incompatible environments.** `meson setup` omits `HAVE_ATOMIC_SUPPORT`, `HAVE_ATOMIC_BUILTINS`, `HAVE_ATOMIC_64BIT`, `HAVE_ATOMIC_GCC_BUILTIN`, `HAVE_IO_URING`, `HAVE_AIO_POSIX`, `HAVE_AIO_THREADPOOL`, `HAVE_GETRANDOM`, `HAVE_ARC4RANDOM_BUF` and `HAVE_PTHREAD_COND_REINIT_OKAY`, all of which autoconf detects. Several change struct layout, so `__env_struct_sig()` differs (`0xb86f77f0` autoconf vs `0x8e25fcb1` meson) and a cross-build attach fails `BDB1539 Build signature doesn't match environment`. Pre-existing and unrelated to the version-triplet fix: the identical drift is present at `v2026.09.7`, where a *different* error (`BDB1538 Program version 5.3 doesn't match environment version 2026.0`) masked it because the meson library also reported the wrong triplet. Fixing the triplet removed `BDB1538` and left `BDB1539` visible. **Consequence: no release note may claim meson/autoconf environment interoperability.** The meson path is a convenience build, not a supported artifact, until it reaches feature parity. | open |
| **W1** | `src/log/log_handoff_trace.c` and `src/mutex/mut_order.c` are absent from the Visual Studio project files, so `--enable-handoff-trace` and the DIAGNOSTIC lock-order checker cannot be built on Windows. **Not** a build break: both files are whole-file `#ifdef`-gated (`HAVE_HANDOFF_TRACE`, `DIAGNOSTIC`) and compile to nothing when their option is off, which is the default. | open |

## Why this file exists

Each of these was, at some point, rediscovered from scratch by someone who could
not tell a pre-existing failure from a regression they had just caused. A release
that names its known issues only in its own release notes forces the next reader
to diff release texts. Tracked here instead, cross-referenced from the notes.

Rule: do not delete an entry to make a run green. Either fix it, or record
honestly that it is still open.
