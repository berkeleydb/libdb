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
| **P2** | `DB_DIRECT_DB` could not open a database: `__fop_read_meta` read into a caller-supplied stack `u_int8_t mbuf[DBMETASIZE]` (8 sites) carrying only scalar alignment, so `O_DIRECT` rejected the first metadata read with `EINVAL`. **FIXED** — aligned at `__fop_read_meta`, the single choke point for metadata reads, using `ALIGNP_INC` over an over-sized stack buffer (no allocation, so no error path can leak on the database-open path). Gated on `DB_ENV_DIRECT_DB`, so the default build is unchanged. Because the behaviour test passes on kernels that tolerate an unaligned buffer, `test/c/p2_align.c` asserts the MECHANISM: bare `mbuf` 0x7ffd3b755240 (not aligned) vs bounce 0x7ffd3b756000 (4096-aligned, in bounds). | **fixed** |
| **P3** | **`DB_LOG_DIRECT` has the same alignment defect as P2** — a separate site, found by the new `flag_behaviour` test. `__log_write` passes a caller-supplied buffer and an arbitrary length to `__os_io` (`src/log/log_put.c:1453`), so `O_DIRECT` rejects the write on both buffer alignment *and* length — observed as `write: 0x7ffff129d287, 1: Invalid argument`, a 1-byte write. **Diagnostically important:** the same test set shows `DB_MPOOLFILE->open(DB_DIRECT)` **passes** with `O_DIRECT` confirmed on the descriptor, so `__os_open`'s plumbing is correct and the defect is localized to the fop and log layers. Neither check alone could establish that. Both P2 and P3 are XFAIL-marked in `test/c/flag_behaviour.c` and will start passing when fixed. | open |
| **P2/P3 scope** | **The O_DIRECT defects are latent, not filesystem-specific — corrected 2026-09-19.** An earlier revision of this row attributed the varying reproduction to XFS-vs-other alignment enforcement. That was wrong. Measured: two different XFS-on-NVMe boxes both report `sectsz=512`, and one has `direct_db` PASS while the other has it FAIL. The actual defect is unconditional in the code — a stack `u_int8_t mbuf[DBMETASIZE]` is **not** 512-byte aligned (measured: `0x7ffdbe1022d0`, neither 512- nor 4096-aligned), so `O_DIRECT` correctness depends on whether the kernel/filesystem happens to tolerate an unaligned buffer for that particular call. `DB_DIRECT_DB` appearing to work is luck, not correctness. `DB_LOG_DIRECT` (P3) fails more reliably because `__log_write` also passes an arbitrary *length*, violating a second O_DIRECT constraint. Consequence for gating unchanged: the strict-mode teeth stay informational on runners whose storage we do not control. | open |
| **U7** | `--disable-mutexsupport` did not build. **FIXED** — three stacked defects: `src/dbinc/os.h` declared `os_ext.h`'s `db_atomic_t` prototypes without including `dbinc/atomic.h` (reachable only via `mutex_int.h`, which `mutex.h:12` guards behind `HAVE_MUTEX_SUPPORT`); the no-mutex `MUTEX_*` stubs expanded to the assignment expression `(mutex) = (mutex)`, which cannot be compared against 0 as our os_aio cross-reap latch does at `mp_sync.c:396`; and `lock_stub.c` was missing `__lock_sicommit`/`__lock_sicleanup` stubs, since `configure.ac:1168` substitutes `lock_stub.o` for all of `LOCK_OBJS` and our SSI work only added `__lock_sireap_lockers` there. Same shape as the exec-bit and manifest gaps: a hand-maintained second list with nothing checking it — the G14 sweep is now that check. | **fixed** |

## Measured, characterized, not yet fixed

| id | issue | status |
|----|-------|--------|
| **P1** | `LOCK_LOCKERS` acquired all 64 locker stripes on every transaction begin and end — 128 mutex operations per transaction, defeating the striping. **FIXED**: stripes are now keyed on the **bucket index** (`LOCK_LOCKER_STRIPE(LOCK_LOCKER_NDX(region, id))`) so a stripe genuinely owns whole `locker_tab` chains, plus `mtx_locker_stripe[0]` reused as the allocation latch for the shared free-list/counter state. **128 mutex ops/txn → 4**, with no new region field, so `__env_struct_sig()` stays `0xb86f77f0`. Measured: **+29.7% @t=1, +22.9% @t=8, +37.0% @t=32, +52.1% @t=96**; locker path 18.90% → 4.29% of profile; kernel spinlock slowpath 3.22% → 0.01%; t=96 CV 11.13% → 1.29%. Converted `__lock_getlocker`, `__lock_freelocker`, `__lock_id`; deliberately left the multi-bucket and full-table-walk sites (addfamilylocker, familyremove, sireap, deadlock, failchk, stat, sicleanup) with reasons recorded. **Throughput still does not climb monotonically**, so the shape gate's concern is not fully retired — the t=32 dip is removed (0.883x → 0.932x) and t=96 now beats the baseline's best point at any thread count, but more remains. Full analysis: `test/bench/P1-FIX-2026-09.md`. | **fixed, partially** |

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

## Test-coverage gaps that let shipped defects through

Identified by asking why the 2026-09 cross-engine benchmark found defects CI did not.
Full analysis: `docs/design/perf-gate-gaps.md`.

| id | gap | status |
|----|-----|--------|
| **G12** | **No CI machine can exhibit a concurrency defect.** All 34 jobs run on `ubuntu-latest` (2-4 vCPU). libdb's throughput *peaks at 8 threads and falls to 32% of that peak by 96* — a defect that is structurally invisible on a 2-core runner. Needs a nightly scaling-shape gate on 32+ cores asserting monotonicity (`tpm(t=32) >= tpm(t=8)`), which is robust to noise in a way an absolute-throughput gate is not. | open |
| **G13** | **Performance is not gated at all.** The only perf job (`bench.yml`) is `continue-on-error` and self-described as "informational only", correctly, because a shared runner's noise floor makes absolute numbers useless. A regression from 1,041 to 337 tpm would be reported by nothing. | open |
| **G14** | **42 of 54 `configure` options are never exercised in CI.** Includes `o_direct` (hence **P2**), `atomicsupport`, `mutexalign`, `log_checksum`, `partition`, `hash`, `heap`, `queue`, `replication`, `statistics`, `verify`, `handoff-trace`. Needs a one-at-a-time option sweep, plus a check that any new `configure` option is either in the sweep or on a commented exclusion list. | open |
| **G15** | **Six runtime behaviour flags are referenced by zero tests:** `DB_DIRECT`, `DB_DSYNC_DB`, `DB_LOG_DIRECT`, `DB_LOG_DSYNC`, `DB_LOG_WRNOSYNC`, `DB_NOSYNC` — the durability and I/O-path knobs. Worse, `test/c/cov_api_surface.c` counts `DB_DIRECT_DB` as covered while only asserting that the *setter accepts* it, which is how a flag that cannot open a database at all (**P2**) showed as covered. Tests must assert the observable consequence (e.g. `O_DIRECT` really set on the data file), not that the API call returned 0. | open |

## Toolchain flakes (not libdb defects)

| id | issue | status |
|----|-------|--------|
| **F1** | **Apple clang 15.0.0 (clang-1500.3.9.4) crashes compiling `src/env/env_register.c`** on the `macos-14` runner: `clang: error: unable to execute command: Abort trap: 6`, `clang frontend command failed due to signal`, with a "PLEASE submit a bug report" note and preprocessed source dumped to the runner's temp dir. **Not reproducible** — an unmodified rerun of the identical commit passed, and the job had succeeded on master's previous 5 runs. Most likely a resource/OOM abort on the hosted runner rather than a deterministic frontend bug. Not filed upstream: Apple's clang tracker needs the preprocessed source and run script the crash dumps, and those live on an ephemeral runner that is destroyed with the job, so there is nothing reproducible to submit. **If it recurs, capture the artifacts first** — add a step that uploads `/var/folders/**/*.c` and the `*.sh` run script from the crash note as a build artifact, then file with those attached. A crash report without the reproducer would be closed unactionable. | monitoring |

## Why this file exists

Each of these was, at some point, rediscovered from scratch by someone who could
not tell a pre-existing failure from a regression they had just caused. A release
that names its known issues only in its own release notes forces the next reader
to diff release texts. Tracked here instead, cross-referenced from the notes.

Rule: do not delete an entry to make a run green. Either fix it, or record
honestly that it is still open.
