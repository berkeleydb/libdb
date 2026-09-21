# Known test-suite and harness issues

Issues that make a test tier fail or skip without indicating an engine defect,
plus engine defects that are shipped knowingly. Every entry is reproduced on a
pristine baseline before being listed here, so "pre-existing" is a measurement
and not an assumption.

**These `T` labels are distinct from the `G` labels in
`rfc/0010-global-invariants.md`.** The `G` gaps number *invariant-coverage*
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
| **S1** | `os_aio` stall under concurrent checkpoint/trickle/sync. Opt-in `DB_MPOOL_AIO`, **default OFF**. **FIXED** — the deferred-write path held buffer pins (ref + shared `mtx_buf`) across a wait, because it drained only at `nflight >= MEMP_AIO_WINDOW`. Measured baseline 11/192 (5.7%, 95% CI 3.2–10.0%); sharpened to 18/96 with a larger window. gdb showed **two** variants, correcting the original one-variant diagnosis: (A) 6/18 — the winner blocks in `MUTEX_READLOCK(bhp->mtx_buf)` for a new buffer while a splitting writer needs a pinned buffer exclusive; (B) 12/18 — the winner never blocks, every remaining buffer is `BH_EXCLUSIVE` held by the writer that is itself waiting on one of *its* pins, so it spins the `required_write` retry loop forever while RUNNABLE, with no mutex wait in its own backtrace. Fix: never wait holding deferred pins — drain before the retry-loop yield, and `MUTEX_TRY_READLOCK` + drain before the blocking acquire. Both needed. Post-fix 0/384 sharpened and 0/384 at the shipped window. `lost=0`, recovery + `db_verify` clean throughout, and a write error still surfaces. See `test/c/OS-AIO-DEADLOCK-FIX.md`. | **fixed**, still default-off |
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
| **P3** | `DB_LOG_DIRECT` could not complete a transactional open: the log write path satisfied none of `O_DIRECT`'s three constraints (buffer address, file offset, transfer length). **FIXED** — `__log_write_direct` restages each write into 4096-byte aligned whole blocks (`base = w_off & ~(B-1)`, read back the leading partial block, zero-pad the trailing one) using `ALIGNP_INC` over a stack buffer, so there is nothing to free on an error path; log preallocation's extend is skipped under `DBLOG_DIRECT`. `lp->w_off` still advances by exactly `len`, so no LSN arithmetic changes meaning, and `db_log_verify` succeeds on a log written entirely through the new path. **Two failing sites, not one** — the tracked diagnosis named `__log_write` but the 1-byte `EINVAL` came from `__db_file_extend`. Offset is aligned essentially never (`head != 0` on 1,999 of 2,000 writes), so the read-back is the hot path. Durability proven: 300 `DB_TXN_SYNC` commits survived `SIGKILL` with no clean shutdown or checkpoint, 43,982/43,982 log I/Os aligned, `st_maxcommitperflush` unchanged. No struct field added, so `__env_struct_sig()` stays `0xb86f77f0`. Disclosed: more fsyncs under the flag (347 vs 173) — throughput, not correctness. Analysis: `test/c/P3-LOG-ODIRECT.md`. | **fixed** |
| **P2/P3 scope** | **The O_DIRECT defects are latent, not filesystem-specific — corrected 2026-09-19.** An earlier revision of this row attributed the varying reproduction to XFS-vs-other alignment enforcement. That was wrong. Measured: two different XFS-on-NVMe boxes both report `sectsz=512`, and one has `direct_db` PASS while the other has it FAIL. The actual defect is unconditional in the code — a stack `u_int8_t mbuf[DBMETASIZE]` is **not** 512-byte aligned (measured: `0x7ffdbe1022d0`, neither 512- nor 4096-aligned), so `O_DIRECT` correctness depends on whether the kernel/filesystem happens to tolerate an unaligned buffer for that particular call. `DB_DIRECT_DB` appearing to work is luck, not correctness. `DB_LOG_DIRECT` (P3) fails more reliably because `__log_write` also passes an arbitrary *length*, violating a second O_DIRECT constraint. Consequence for gating unchanged: the strict-mode teeth stay informational on runners whose storage we do not control. | open |
| **U7** | `--disable-mutexsupport` did not build. **FIXED** — three stacked defects: `src/dbinc/os.h` declared `os_ext.h`'s `db_atomic_t` prototypes without including `dbinc/atomic.h` (reachable only via `mutex_int.h`, which `mutex.h:12` guards behind `HAVE_MUTEX_SUPPORT`); the no-mutex `MUTEX_*` stubs expanded to the assignment expression `(mutex) = (mutex)`, which cannot be compared against 0 as our os_aio cross-reap latch does at `mp_sync.c:396`; and `lock_stub.c` was missing `__lock_sicommit`/`__lock_sicleanup` stubs, since `configure.ac:1168` substitutes `lock_stub.o` for all of `LOCK_OBJS` and our SSI work only added `__lock_sireap_lockers` there. Same shape as the exec-bit and manifest gaps: a hand-maintained second list with nothing checking it — the G14 sweep is now that check. | **fixed** |

## The post-P1 scaling ceiling

| id | issue | status |
|----|-------|--------|
| **P4** | `__db_walk_cursors` held `env->mtx_dblist` **exclusively** across the whole cursor walk, so the 8-way `cq_parts[]` sharding bought nothing and one environment-wide mutex serialized every B-tree insert. **FIXED** — the latch is now taken **shared**: it guards the *shape* of `env->dblist`, the walk is a pure reader, and `__db_refresh` unlinks under the same latch **exclusively**, so a handle cannot be unlinked while a reader holds it shared. That makes the handle-lifetime question vanish rather than requiring a pin that does not exist. `__bam_ca_di` **30.74% → 2.89%** of profile at t=64. **Throughput effect is modest and workload-dependent** — independently measured over 3 alternating reps on 96 vCPU: **t=64 +10.7%** (median 125,890 → 139,396), **t=32 −5.3% at 6.4% CV, i.e. inside noise**. The implementing agent measured a t=64 *regression*; my re-measurement found the opposite sign, so the honest statement is that this is not a large throughput win. **`__log_put` is now the ceiling** — 56% of time at t=64, 87% of that on the single log-region latch — which is the next target and is tracked as **P5**. Analysis: `test/bench/P4-FIX-2026-09.md`. | **fixed** |

## The scaling ceiling beyond the log

| id | issue | status |
|----|-------|--------|
| **P9** | **Lowering `tas_spins` HANGS — the "free config win" is unsafe, and it exposes a latent hybrid-mutex defect.** Measured on 96 vCPU, `DB_ENV->mutex_set_tas_spins(1)`: **3 of 4 runs hang** (rc=124) at t=32, and the hang reproduces serially with settle time, so it is not oversubscription. gdb at the hang: **all 32 threads in `__db_hybrid_mutex_suspend` → `__db_pthread_mutex_condwait`**, each on a distinct condvar — everyone waiting, nobody holding. Mechanism: `mut_tas.c:251-257` routes into `__db_hybrid_mutex_suspend` once the spin budget is exhausted (`HAVE_MUTEX_HYBRID` is **on in default builds**), and that function asserts `F_ISSET(mutexp, DB_MUTEX_SELF_BLOCK)` (`mut_pthread.c:561`) — a flag never set at allocation. With the default 4,800 spins the suspend path is reached rarely enough that the lost wakeup is effectively invisible; at 1 spin it is the common path. **So the earlier "`tas_spins=1` is faster at t=4 and t=16" observation was measuring runs that sometimes complete, not a safe tuning win**, and P9 must be re-scoped: the spin count is a *symptom* surface, and the real defect is the suspend/wake protocol. Note this also means any future work that reduces spinning — a plausible scalability direction — will hit this first. The underlying scaling observation still stands: with the log removed entirely (`DB_TXN_NOT_DURABLE`) throughput still falls 235k@t=8 → 131k@t=96, so a non-log ceiling is real. | open, re-scoped |

## Engine bugs in untested public API flags

Found by the `flagapi` behaviour tier, which was built because 112 of 229 public flags
had **no** test. Each is XFAIL'd with a reference and flips to PASS with no edit once
fixed. **All three were reproduced on stock master.**

| id | issue | status |
|----|-------|--------|
| **P6** | **`DB_BACKUP_NO_LOGS` is accepted and silently ignored.** It appears exactly *twice* in the tree: its own `#define` and the accepted-flag mask at `src/db/db_backup.c:684`. It is **read nowhere**. Measured: a plain `DB_ENV->backup()` copied 62 log files; the same call with `DB_BACKUP_NO_LOGS` copied the same 62. `docs_src/api/c/envbackup.md` documents it as "Back up only the `*.db` files. Do not backup the log files." Severity: silent — an operator asking for a logless backup gets logs, so wasted space and a false expectation rather than data loss. | open |
| **P7** | **`DB_INORDER` + `DB_CONSUME` across a deleted record hangs forever at 98% CPU.** Minimal repro with **no concurrency**: queue with `set_flags(DB_INORDER)`, 20 records via `DB_APPEND`, `DB->del()` record 10, drain with `DB_CONSUME`. Without the flag it drains all 19 survivors and returns `DB_NOTFOUND`; with it, it consumes 1–9, reaches the hole, and never returns. gdb hit counts on the `retry:` label in `__qamc_get` (`src/qam/qam.c:691`): **22** for the default arm versus **>100,001 and climbing** under `DB_INORDER`. Verified in source: `inorder = F_ISSET(dbp, DB_AM_INORDER) && with_delete` (`qam.c:667`) causes the record lock to be taken **without `DB_LOCK_NOWAIT`** (`qam.c:838`), and `first != cp->recno` (`qam.c:866`) breaks out of the switch without advancing past the gap, so it never converges. Severity: **highest of the three** — an unkillable spin in a documented public flag, reachable single-threaded. | open |
| **P8** | **`DB_NOFLUSH` makes an environment unusable** — `SIGBUS` on a shared environment, `DB_PAGE_NOTFOUND` on a private one. `LAST_PANIC_CHECK_BEFORE_IO` is an unconditional `return (0)` inside every write path. | open |

## The next ceiling

| id | issue | status |
|----|-------|--------|
| **P5** | **The log latch is a handoff cost, not a work cost — and reserve-then-copy is DISQUALIFIED on correctness.** Analysed in RFC 0008. Four independent lines of evidence that the critical section's *contents* are not the bottleneck: `__memmove` is **1.46%** of time at t=64 versus **90.45%** in mutex acquire+release; raising `lg_bsize` 32KB→8MB cut in-latch `pwrite`s **170×** with **no throughput change**; a standalone model puts reserve-then-copy's ceiling at 10–20% *with a regression at t=8*; and a `DB_TXN_NOT_DURABLE` control shows the log is only 36–62% of per-txn cost, so a *perfect* log fix buys ≤ **+57%** at t=96. Mean latch hold inflates **21ns → ~300ns (14×)** with thread count for constant work — coherence traffic, which reserve-then-copy does not remove. **Why D1 (PostgreSQL-style reserve-then-copy) is unsafe here specifically:** PG and InnoDB are single-process, so a dead writer means a dead server. libdb's log buffer is shared across processes and `__mut_failchk` reclaims **only** `DB_MUTEX_PROCESS_ONLY` mutexes (`mut_failchk.c:50-52`), which the log latch is not — so a reserve-then-die hole becomes a **zero header, which `log_get.c:1238` treats as virtual EOF and returns success**, making recovery *silently discard committed transactions*. Verified in source. Partitioned WAL (Kafka/Redpanda/Silo/Taurus) rejected outright: physiological redo's page-LSN precondition makes a cross-log merge unsound. **What does move throughput:** a single-key insert costs **3.10 log records** (2× `__db_addrem` + `__txn_regop`), so batching 4 rows/txn gives **+189% at t=32** against a ±22.2% floor, and the same latch then sustains **2.19× more appends/s** — the cost is *entering* the serialized stage, not the work inside. The real invariant is ~90k **transactions**/sec regardless of per-txn work. | analysed, see RFC 0008 |

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
Full analysis: `rfc/0011-test-coverage-gaps.md`.

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
