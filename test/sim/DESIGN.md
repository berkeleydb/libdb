# libdb Deterministic Simulation Testing (DST) — design & roadmap

> Canonical copy lives at `.agents/dst-design.md` (gitignored, per repo convention); this is the committed PR copy.

Status: **v1 foundation landed + v1.x depth grown** (this branch). Seeded
PRNG tree, determinism
guard, buggify, simulated-I/O fault knobs, the write-back-cache durable-frontier
crash model, the `__os_*` I/O hooks, a `--enable-dst` build switch that is
zero-overhead when off, and a **36-scenario** catalog plus a FoundationDB-style
**swarm runner** with per-fault activation coverage (now with a hard
coverage-gap guard) and **eight** planted-bug yardsticks. Modeled on
FoundationDB / TigerBeetle and the xtc project's DST (`/home/gburd/ws/xtc`).

This document is the PLAN. It records what shipped, the honest architectural
gap vs xtc, and the full scenario catalog to grow into.

---

## 0. Why libdb DST looks different from xtc DST

xtc is a **single-process cooperative-fiber actor runtime**: its DST can run N
"loops" as N fibers on one thread under a seed-determined interleaving, with a
virtual clock and a fully deterministic scheduler. Every source of
nondeterminism (time, RNG, I/O completion order, message delivery) funnels
through one seam the scheduler owns, so a seed reproduces the *entire* run
bit-for-bit including thread interleaving.

**libdb cannot get that for free.** libdb is:

- **multi-process** — multiple processes attach the same environment via
  shared `mmap` regions (the buffer pool, lock table, log region, txn region);
- **real-threaded with real fsync durability** — durability is `pwrite` +
  `fsync` to real files, and correctness depends on genuine OS scheduling and
  memory visibility across processes.

A single-threaded deterministic scheduler does **not** directly apply: there is
no one seam that owns cross-process interleaving. Pretending otherwise would be
coverage theater.

So libdb DST is **phased**, and v1 deliberately targets the axis that *does*
map cleanly onto BDB's architecture:

- **v1 (this branch): deterministic FAULT-INJECTION + CRASH/RECOVERY.**
  The seeds control *what faults happen and when* (I/O latency, EIO, short
  transfer, torn write, corrupt read, ENOSPC, and the ack-before-fsync
  write-back crash model), plus a deterministic workload (keys/values drawn
  from a seeded stream). We sidestep cross-process scheduling nondeterminism by
  running the pilots **single-process** (and, where a scenario needs a cold
  cache, `DB_PRIVATE` so the buffer pool is process-local). This is exactly the
  FoundationDB "simulated disk + crash" discipline, and it is where the
  highest-value storage-engine bugs live (lost commits, torn logs, silent page
  corruption).

- **v2 (later): deterministic scheduler / multi-process.** A genuine
  deterministic interleaving of multiple BDB processes/threads requires either
  (a) a PCT/coarse-interleaving controller that pins scheduling at BDB's own
  yield/lock seams, or (b) a record-replay layer over the shared-region
  accesses. Both are large; the `DB_SIM_RNG_SCHED` stream and the virtual-clock
  seam are reserved now so v1 draw ordering does not shift when v2 lands.

---

## 1. Architecture (what shipped in v1)

All of it lives in `test/sim/` and is compiled into the library **only** under
`--enable-dst` (`HAVE_DST`). With DST off, none of these symbols exist and the
`__os_*` hooks compile to the stock code path — verified: the OFF library has
zero `__db_sim_*` symbols and builds with no undefined references.

### 1.1 Seeded PRNG tree (`sim_rng.c/.h`, in `sim_core.c`)

Per-stream **splitmix64**, adapted from xtc. One root seed splits into
independent sub-streams via the golden-ratio finalizer, so a draw added at one
decision site never perturbs another site's sequence (stable replay under code
change — the FoundationDB discipline).

Streams (`enum db_sim_stream`):

| Stream | Use |
|---|---|
| `DB_SIM_RNG_IO` | simulated I/O latency, fault/torn/corrupt/ENOSPC toggles |
| `DB_SIM_RNG_FAULT` | generic fault-injection toggles |
| `DB_SIM_RNG_BUGGIFY` | buggify per-run activation coins |
| `DB_SIM_RNG_APP` | application/test workload draws |
| `DB_SIM_RNG_SCHED` | **v2**: deterministic scheduler / multi-process (kill point; see `test/sim/DST-V2-DESIGN.md`) |

`SCHED` is reserved (not yet drawn) so adding the scheduler later does not move
v1 seeds. `__db_sim_rng(s)` returns 0 when sim is inactive; callers gate on
`__db_sim_active()` (a single relaxed atomic load).

### 1.2 Determinism guard

`__db_sim_nondeterminism(what)`: a sim-reachable primitive that would break
seed replay (a real clock read, an unseeded `rand()`, a raw pid) calls this.
Outside a sim run it is a no-op; inside a run it records the violation and, in
**strict mode (default)**, aborts naming the source. `__db_sim_strict(0)`
switches to count-only for a diagnostic sweep. `__db_sim_nondeterminism_count()`
lets a harness assert 0 to *prove* a run was fully deterministic.

> v1 does not yet *plant* guard calls at BDB's nondeterministic primitives
> (`__os_clock`, `__os_unique_id`, `__os_id`). That wiring is a small,
> mechanical follow-up (§4, item G) and is where the guard earns its keep.

### 1.3 Buggify (per-run cached coin)

`DB_BUGGIFY(name)`: a named point in real library code that, under sim, takes a
legal-but-pessimal path. Unlike a per-call fault, a buggify point is a coin
flipped **once per run per site**, cached, so all reaches of a name agree and
the run replays. Drawn from the dedicated `BUGGIFY` stream so enabling it never
perturbs the IO/FAULT streams. Compiles to constant 0 when DST is off (verified:
`nm` shows 0 sim symbols and no point name appears in any engine `.o`).

The invariant that makes buggify safe: **every buggified path is legal**
(correctness-preserving) — it only changes timing/sizing/path-choice, never a
result. So the whole scenario suite must still pass with buggify forced on. If
turning a point on ever breaks an invariant, either the point isn't actually
legal (a bug in the point) or the engine mishandles a rare-but-legal path (a
real engine bug) — that is buggify's purpose.

**Point catalog** (9 points, all `#ifdef HAVE_DST`, each a legal-but-pessimal choice):

| Point | Site | Pessimal choice |
|---|---|---|
| `bt.split_early` | `bt_put.c` | force `DB_NEEDSPLIT` when the page is >3/4 full (guarded ≥4 entries + <pgsize/4 free, so no split loop) |
| `hash.expand_early` | `hash_page.c` | force `H_EXPAND` before the fill factor is reached |
| `mp.alloc_aggressive` | `mp_alloc.c` | start the eviction scan in aggressive mode |
| `mp.evict_cold` | `mp_fput.c` | pin the buffer at the coldest warmth |
| `log.flush_now` | `log_put.c` | force `DB_FLUSH` on a would-be-buffered log put |
| `log.newfile_early` | `log_put.c` | roll to a new log file at >half full |
| `txn.chkpt_force` | `txn_chkpt.c` | checkpoint past the byte/time threshold |
| `lock.dd_now` | `lock.c` | run the deadlock detector on a lock-vector op |
| `lock.dd_wait_now` | `lock.c` | run the detector before blocking |

**Measured activation** (`test_sim_buggify`, 24-seed sweep, all pessimal paths
forced): bt.split_early 79%, hash.expand_early 67%, mp.alloc_aggressive 100%,
mp.evict_cold 83%, log.flush_now 75%, log.newfile_early 75%, txn.chkpt_force 88%,
lock.dd_now 83%, **lock.dd_wait_now 0%** (never reached — this single-writer
workload has no blocked waiter to drive the site; the point is valid, the
coverage is workload-limited; a WARN, not a failure). **0 invariant violations
across all 24 seeds with every pessimal path on** — every committed txn survived
crash+recovery, no uncommitted survived, both DBs verified clean. **No real
engine bug found**: an early over-aggressive split point caused a non-termination
loop, which was an *illegal* buggify point (self-inflicted), correctly fixed by
the >3/4-full guard — buggify's own self-check working as designed.

### 1.4 Simulated I/O faults + the write-back crash model

Knobs the `__os_*` hooks consult (all off by default; a no-op unless a sim run
arms them; all seeded on the IO stream so enabling them never shifts the
schedule):

- **latency** — `__db_sim_io_latency()` (v1 records intent; a synchronous
  single-process pilot does not yet *sleep*, since there is no scheduler to
  reorder against — this becomes load-bearing in v2 with concurrent I/O).
- **short transfer / EIO** — `__db_sim_io_should_fault()`.
- **ENOSPC** — `__db_sim_io_enospc()`: a whole write fails, nothing persists.
- **torn write** — `__db_sim_io_torn_prefix(len)`: persist a seeded strict
  prefix but report full success (latent bad tail a checksum must catch).
- **corrupt read** — `__db_sim_io_flip_byte(len)`: flip one returned byte
  (wired into `__os_io` read path via `__db_sim_io_read_hook`).
- **write-back cache crash model** — the ack-before-fsync durability catcher,
  below.

**The write-back model (THE key to catching ack-before-fsync bugs) — NOW FULLY
WIRED.** The sim writes to a *real* file, so bytes reach the file on `pwrite`
regardless of `fsync` — which means a naive crash test cannot catch a writer
that ACKs a commit without fsyncing it. The model fixes this honestly:

- a write records the **written high-water** offset for the file
  (`__db_sim_io_wb_wrote`) plus the file's on-disk **name**;
- `fsync`/`fdatasync` promotes written → **durable** (`__db_sim_io_wb_synced`);
- at the crash boundary a scenario calls **`__db_sim_wb_crash()`**, which
  `truncate()`s every tracked real file back to its durable frontier — so
  bytes written but never fsync'd genuinely vanish, exactly as a disk loses
  them on power loss;
- recovery then runs against the truncated files, so a commit acked without
  its log being fsync'd is **detectably lost**.

This is what makes planted bug 1 (NODURABLE: `__log_flush_int` skips the log
fsync) a hard catch: the durable frontier never advances, `wb_crash` truncates
the un-synced tail, and every "committed" txn is lost after recovery.

The write-side knobs are consumed now too: `__os_io`'s write fast path consults
`__db_sim_io_write_fault_hook` (ENOSPC = whole write fails; torn = persist a
strict prefix, report full; short/EIO = whole write fails with EIO). The read
fast path consults `__db_sim_io_read_hook` (corrupt bit-flip + stale-read ring)
and a per-I/O `__db_sim_io_latency_hook` (a tiny capped sleep when armed). The
stale-read ring is now **fully wired**: the write fast path calls
`__db_sim_io_presnapshot_hook` (a no-op unless stale injection is armed) which
reads the current on-disk bytes before an overwrite and records the prior
version, so a later seeded stale read hands back a well-formed but out-of-date
block -- caught by `test_sim_stale`'s monotonic-version check.

Each fault firing (not merely arming) bumps a **fault-activation counter**
(`__db_sim_fault_count(class)`), so the swarm can report per-fault activation
coverage across a seed sweep -- a class that never fires is a coverage gap.

Keyed by a **stable FNV-1a hash of the file name** (not the fd), so the
frontier tracks a *logical* file across libdb's frequent close/reopen — exactly
how a real disk behaves.

### 1.5 The `__os_*` I/O seam (where the hooks attach)

BDB funnels durability through a tiny OS layer, which is the clean seam:

| Function (file) | Hook added (under `#ifdef HAVE_DST`) | Purpose |
|---|---|---|
| `__os_io` write fast path (`os_rw.c`) | `__db_sim_io_write_off_hook(fhp, off+len)` | record written high-water (covers WAL + page writes) |
| `__os_io` write fast path (`os_rw.c`) | `__db_sim_io_presnapshot_hook(fhp, off, len)` | snapshot prior on-disk bytes into the stale ring (no-op unless stale armed) |
| `__os_io` read fast path (`os_rw.c`) | `__db_sim_io_read_hook(buf, len)` | corrupt-read bit flip + stale-read return |
| `__os_fsync` (`os_fsync.c`) | `__db_sim_io_sync_hook(fhp)` | promote written → durable |

The WAL write path (`__log_fill`/`__log_write` → `__os_io(DB_IO_WRITE, lfhp,
0,0, w_off, len, …)`) and the WAL fsync (`__log_flush_int` → `__os_fsync(env,
lfhp)`) both route through these exact hooks — so the write-back model tracks
the log's durable frontier precisely, which is the ack-before-fsync seam.

Each hook is a single `__db_sim_active()` relaxed load in the common no-sim
case, and vanishes entirely when `HAVE_DST` is undefined.

> **Remaining integration (documented, not yet wired):** the slow/`__os_physwrite`
> path (non-`pread` platforms, `j_write` hook, `HAVE_FILESYSTEM_NOTZERO`) is not
> hooked -- on Linux with pread/pwrite the fast path always wins, so v1 is fully
> covered there. The WAL *record* read (`__os_read` in log_put.c/log.c) uses the
> unhooked slow read path, so a corrupt-read on a log record only fires when the
> log is read via `__os_io` (the common `log_get.c` path). `os_aio*` (the newer
> async buffer-pool I/O) is likewise not yet hooked; the buffer pool's
> *synchronous* `__os_io` path (`mp_bh.c`) is, which is what the scenarios
> exercise. All six fault knobs -- ENOSPC, torn, short/EIO, corrupt-read,
> stale-read, per-I/O latency -- are now consumed by the `__os_io` fast path and
> the swarm activates every one (§3).

### 1.6 The determinism-proof harness

`test_sim_rng` runs a workload twice with the same seed and asserts the byte
sequences are identical, asserts different seeds diverge, asserts stream
independence (interleaving one stream's draws does not shift another's), and
asserts the determinism-guard count behaves. `test_sim_crash_recover` re-derives
the expected committed set from the seed after recovery — a mismatch across a
replay of the same seed would fail.

---

## 2. Build integration

- **Autoconf (primary; what the validation recipe uses).**
  `--enable-dst` → `db_cv_dst` (`dist/aclocal/options.m4`) → in
  `dist/configure.ac`: `AC_DEFINE(HAVE_DST)`, append `$(SIM_OBJS)` to
  `ADDITIONAL_OBJS`, and add `-I$(topdir)/test/sim` to `CPPFLAGS`.
  `dist/Makefile.in` defines `SIM_OBJS = sim_core@o@ sim_os_hooks@o@`, their
  compile rules, and a `dst_tests` target (all fourteen `test_sim_*`).
  `DSTBUG=<n>` plants a TEST-side bug n; a LIBRARY-site planted bug is built by
  configuring with `CFLAGS="-DDB_DST_INJECT_BUG=<n>"` (a dedicated build tree),
  which `test/sim/dst-bug-inject.sh` automates.
  All additions are additive and DST-scoped so they merge cleanly alongside the
  concurrent PBT work.

- **Meson (TODO, documented).** The PBT agent owns `meson.build` /
  `meson_options.txt`. To avoid clobbering, v1 does **not** wire DST into meson.
  The follow-up is one option + one guarded subdir:
  ```
  # meson_options.txt
  option('dst', type: 'boolean', value: false,
    description: 'Build with Deterministic Simulation Testing hooks.')
  # meson.build (near the hegel subdir block)
  if get_option('dst')
    add_project_arguments('-DHAVE_DST', language: 'c')
    subdir('test/sim')   # test/sim/meson.build fully owned by DST
  endif
  ```
  and a `test/sim/meson.build` that adds `sim_core.c`/`sim_os_hooks.c` to the
  lib sources and declares the three pilot executables. The `#ifdef HAVE_DST`
  guards in `os_rw.c`/`os_fsync.c` already work with either build system.

---

## 3. Scenarios (all runnable now, all PASS)

Thirty-six scenarios (plus the swarm runner); the crash/fault ones each pass
across a multi-seed sweep and replay bit-identically per seed.

| Scenario | Proves | Result |
|---|---|---|
| `test_sim_rng` | seeded PRNG determinism, seed-sensitivity, stream independence, guard | PASS |
| `test_sim_crash_recover` | **capstone**: N durable txns survive a mid-txn crash via the write-back drop; uncommitted does not; DB verifies clean **after recovery** | PASS (64 txns; 30/30 seed sweep) |
| `test_sim_torn` | corrupt reads caught by DB_CHKSUM or invisible — **never silently wrong** | PASS (e.g. 297 correct / 103 detected / 0 silent-bad) |
| `test_sim_hash_crash` | DB_HASH op+crash+recover | PASS (64 committed survive) |
| `test_sim_recno_crash` | DB_RECNO append+crash+recover | PASS (64 appends survive) |
| `test_sim_queue_crash` | DB_QUEUE enqueue+crash+recover | PASS (64 enqueues survive) |
| `test_sim_ckp_crash` | page-flush durability across crash (no log) | PASS (200 flushed survive; catches LOSTUPDATE) |
| `test_sim_torn_log` | recovery safe past a torn log tail; durable prefix intact | PASS (32 durable commits present) |
| `test_sim_enospc` | ENOSPC graceful degradation, no corruption | PASS (all observed-committed durable) |
| `test_sim_abort_atomic` | committed present + aborted leave no trace after crash | PASS (deterministic commit/abort split) |
| `test_sim_recover_idempotent` | recover twice → identical full-state hash | PASS |
| `test_sim_dup_crash` | DB_DUPSORT dups survive crash with exact multiplicity | PASS (24 keys x 8 dups) |
| `test_sim_overflow_torn` | overflow (>page) records: corrupt read caught, never silently wrong | PASS |
| `test_sim_split_crash` | btree split/merge churn survives crash, tree clean | PASS (380 live keys, 120 deletes) |
| `test_sim_stale` | **stale-read ring**: a monotonic-version check catches every out-of-date read; none adopted as current | PASS (40 seeds, 654 stale reads caught) |
| `test_sim_ckp_enospc` | checkpoint under ENOSPC degrades cleanly; committed txns still durable | PASS (96 committed survive) |
| `test_sim_split_torn` | torn write during a split-heavy flush caught by the page checksum (incl. at open) | PASS (never silent-bad) |
| `test_sim_recover_corrupt` | corrupt reads DURING recovery never yield silently-wrong committed data | PASS (clean refusal or correct) |
| `test_sim_secondary_crash` | primary + secondary (associate) index mutually consistent after recover | PASS (80 records, pget matches) |
| `test_sim_largetxn_crash` | a 2000-op single txn is atomic across a crash (all / none) | PASS (2000 ops) |
| `test_sim_cursor_crash` | cursor c_put/c_del durable; post-recovery cursor walk sees exact live set | PASS (240 live) |
| `test_sim_multi_fault` | latency + ENOSPC both active across a crash; committed durable, no corruption | PASS |
| `test_sim_latency_load` | slow disk makes forward progress; committed set byte-identical to fast disk | PASS (latency fires) |
| `test_sim_ckp_lsn` | checkpoint LSN is the correct recovery start; pre+post-ckp committed survive | PASS (catches CKPBADLSN) |
| `test_sim_multidb_crash` | 3 sub-databases in one file+log recover mutually consistent; committed survives, uncommitted gone, each sub-tree verifies | PASS (144 records / 3 sub-dbs) |
| `test_sim_largeabort` | a 1500-op txn EXPLICITLY aborted leaves no trace, a following committed txn survives, across a crash | PASS (1500 aborted / 300 committed) |
| `test_sim_log_enospc` | ENOSPC on the LOG write: every ACKED commit is durable, failed commits cleanly absent (catches LOGWRITEIGNORE) | PASS (e.g. 184 acked, 0 lost) |
| `test_sim_data_log_order` | the durability window: NOSYNC commits made durable ONLY by a checkpoint survive a crash (log durable before data trusted) | PASS (96 NOSYNC commits) |
| `test_sim_torn_meta` | torn write of the METADATA page during a checkpoint: caught by the checksum (clean error or correct), never silent-bad | PASS |
| `test_sim_stale_meta` | stale read of a real DB meta page after overwrite: caught by the page LSN+checksum, never silent-bad | PASS (stale reads fired) |
| `test_sim_compound_fault` | latency + ENOSPC + torn all active at once across a crash: durable prefix intact, tree clean, no silent-bad | PASS (3 faults fire) |
| `test_sim_logrollover_crash` | crash with the WAL spread over multiple log files: every commit survives across the rollover boundary | PASS (400 commits / 3 log files) |
| `test_sim_crash_in_recovery` | **crash-during-recovery capstone**: crash at every recovery-phase I/O op (+ a double-crash), then finish recovery — converges to the SAME reference state hash regardless of how many partial-recovery crashes happened (recovery is idempotent + convergent) | PASS (19 trials / 20 crash points) |
| `test_sim_recovery_undo_crash` | crash mid-UNDO then recover: committed present, aborted gone, DB clean | PASS |
| `test_sim_recovery_redo_crash` | crash mid-REDO (tiny cache forces real page evictions), recovery-ckp not durable, then recover: idempotent re-apply, clean (catches RECINITNOSTAMP) | PASS |
| `test_sim_recovery_ckp_crash` | crash during the recovery checkpoint write, then recover: converges, committed intact | PASS |
| `test_sim_swarm` | **swarm**: mixed-fault sweep, per-fault activation coverage + gap guard, replay | PASS (512 seeds, 0 violations) |

**Recovery-before-verify discipline** (from
`.agents/concurrent-btree-corruption.md`): a crashed txn env verified *without*
recovery falsely looks corrupt. `test_sim_crash_recover` **always** runs
`DB_RECOVER` before `db->verify`.

**Planted-bug harness** (`sim_inject.h`, `DB_DST_INJECT_BUG=<n>`) — the
FoundationDB-grade "DST finds real bugs" proof, LANDED. **Nine** known bugs
planted at real library sites, each caught by a scenario within **K=1**
seeds (`test/sim/dst-bug-inject.sh` builds a dedicated library per bug and
asserts the catch, reporting the catch-latency K):

| Bug | Site | Caught by | Effect |
|---|---|---|---|
| 1 NODURABLE | `__log_flush_int` (log_put.c) skips the log fsync, still acks | `test_sim_crash_recover` | 64 "committed" txns lost after the write-back crash drops the un-synced log |
| 2 NOCKSUM | `__db_check_chksum` (hmac.c) ignores a checksum mismatch | `test_sim_torn` | corrupt pages flow in as SILENT-BAD data |
| 3 LOSTUPDATE | `__memp_pgwrite` (mp_bh.c) skips a dirty-page write, reports success | `test_sim_ckp_crash` | flushed records lost after crash (no log to redo) |
| 4 ABORTNOUNDO | `__txn_abort` (txn.c) skips the `__txn_undo` rollback pass | `test_sim_abort_atomic` | aborted txn's changes left in place; recovery hits a log-sequence error |
| 5 CKPBADLSN | `__txn_checkpoint` (txn_chkpt.c) writes a checkpoint record with a wrong (too-far-forward) `ckp_lsn` | `test_sim_ckp_lsn` | recovery starts too late; post-checkpoint committed txns lost |
| 6 REDONOSTAMP | `__db_addrem_recover` (db_rec.c) applies a redo but skips the page-LSN stamp | `test_sim_recover_idempotent` | a second recovery re-applies the same redo -- recovery is not idempotent |
| 7 SYNCSKIP | `__memp_sync_int` (mp_sync.c) writes a single-file sync's pages but skips the fsync | `test_sim_ckp_crash` | pages written but not durable; write-back crash drops them, flushed records lost |
| 8 LOGWRITEIGNORE | `__log_write` (log_put.c) ignores an `__os_io` write error and advances `w_off` | `test_sim_log_enospc` | a commit is acked whose log bytes never persisted; lost after crash |
| 9 RECINITNOSTAMP | `__db_pg_alloc_recover` (db_rec.c) skips the meta-page LSN stamp on redo | `test_sim_recovery_redo_crash` | a non-idempotent redo -- caught ONLY by the crash-during-recovery loop (invisible to plain double-recover + single crash+recover): re-applying the redo after a mid-recovery crash corrupts the meta page |

Measured catch-latency (dst-bug-inject.sh, K max 8): **all nine caught at K=1**.
A normal build (`DB_DST_INJECT_BUG` undefined) compiles all nine out and every
scenario passes; the OFF library has **0** `__db_sim_*` symbols (verified via
`nm` on a fresh `--enable-debug` build tree).

### 3.0 Crash-during-recovery (recovery is itself crash-safe)

v1 originally crashed *once* then recovered *once*. FoundationDB reboots
repeatedly, so recovery's OWN crash-safety was an untested surface. The
crash-during-recovery mechanism (`__db_sim_reccrash_enable/ticks/tick` in
`sim_core.c`, fired from the existing `__os_io`/`__os_fsync` write-back seam,
all `#ifdef HAVE_DST`) crashes at the Nth recovery-phase I/O op and drops its
un-fsynced work, exactly modelling a process dying part-way through
`__db_apprec`. An opt-in `DB_SIM_WB_SEED_ONDISK` mode lets a recovery process
treat inherited (already-durable) files correctly.

The capstone `test_sim_crash_in_recovery` sweeps every recovery-phase I/O op as
a crash point (plus a double-crash), runs recovery to completion after each,
and asserts the final full-state hash is **identical** no matter how many
partial-recovery crashes intervened — i.e. recovery is idempotent AND
convergent. **Finding:** the initial "recovery not re-runnable" failure was a
*mechanism artifact* (the write-back model was dropping bytes that were durable
*before* the recovery process started), fixed via `DB_SIM_WB_SEED_ONDISK`; **no
real recovery-safety bug was found** — libdb recovery is idempotent + convergent
across every interruption point tested. Planted bug 9 (RECINITNOSTAMP) proves
the check has teeth: a non-idempotent redo that is *invisible* to the plain
double-recover and single crash+recover scenarios is caught by the
crash-during-recovery loop at K=1.

### 3.1 Swarm methodology + measured fault-activation coverage

`test_sim_swarm` is the FoundationDB-style swarm: a shardable seed sweep
(`test_sim_swarm <count> <base>`; default 256 for CI, thousands for soak) over
a single mixed-fault workload driving the real `__os_open`/`__os_io`/`__os_fsync`
seam. Each seed's fault mix + magnitudes are derived from the seed bits (so the
seed fully determines the scenario and replays). The workload writes and
re-reads versioned, self-checksummed pages and asserts the safety invariants:

- a torn/corrupt page (checksum fails) is never accepted as valid;
- a stale read (older version) is caught by the per-page version stamp;
- the run reaches quiescence and REPLAYS byte-identically (two runs of the
  same seed produce identical result + activation counts).

It reports pass/fail, distinct-result count (seed-sensitivity), and **per-fault
activation** (seeds on which each class actually FIRED, not merely armed).  A
sweep of ≥ 64 seeds fails hard if any fault class NEVER activates (a coverage
gap that would let a regression silently stop arming a class slip through).

**Measured (512-seed swarm):** 0 invariant violations; fault activation
`torn=74.0% corrupt=72.7% stale=49.6% enospc=49.4% latency=75.0%
shorteio=49.4%` — every fault class activates, no coverage gap. (torn/corrupt
exceed their ~50% armed rate because they share the IO corrupt knob, so arming
either can fire both on writes and reads.)

`test/sim/dst-swarm.sh` is the aggregate driver: it sweeps the FULL scenario set
over a seed range (default 30 seeds/scenario) plus the fault-mix swarm, emitting
one CI-readable summary. Measured: **32 scenarios × 10 seeds = 284 pass, 0
fail** (and every new scenario passes a 20-seed sweep + replays bit-identically).

---

## 4. Immediate next steps (ordered)

Items A–E landed on this branch. Remaining:

- ~~**A. Real ack-before-fsync catch.**~~ DONE: `__db_sim_wb_crash()` truncates
  each tracked file to `durable_end` before recovery, and planted bug 1
  (`__log_flush_int` skips fsync) makes `test_sim_crash_recover` lose every
  "committed" txn — caught at K=1.
- ~~**B. Torn-write into the write hook.**~~ DONE: `__os_io` consults
  `__db_sim_io_write_fault_hook`; `test_sim_torn_log` proves recovery is safe
  past a torn log tail.
- ~~**C. Disk-full scenario.**~~ DONE: `test_sim_enospc`.
- **D. Stale-read model.** DONE (v1.x): `__db_sim_io_presnapshot_hook` is now
  wired into the `__os_io` write fast path (a no-op unless stale is armed),
  reading the current on-disk bytes before an overwrite and recording the prior
  version; `test_sim_stale` drives the real OS seam and proves a monotonic-
  version check catches every out-of-date read (654 stale reads caught over 40
  seeds, 0 adopted as current), deterministic replay.
- ~~**E. Seed sweep driver.**~~ DONE: `test/sim/dst-sweep.sh` and
  `test/sim/dst-bug-inject.sh`.
- **F. Meson wiring** (§2).
- **G. Determinism-guard planting** at `__os_gettime` / `__os_id`. Deferred:
  these are read on legitimate recovery/txn paths, so a strict-abort guard
  there would fire on every run; the guard earns its keep once a v2 scheduler
  owns virtual time. The guard machinery + count API are in place.
- **H. `os_aio*` + `__os_physwrite` hooks** for full I/O-path coverage. Still a
  follow-up: the sync `__os_io` fast path (WAL + mpool) is fully hooked and all
  six fault classes fire through it; `os_aio*` and the slow `__os_physwrite`
  path remain unhooked (documented in §1.5).

### Landed on this branch (v1.x depth)

- **Stale-read scenario** (item D above) — the ring is now load-bearing.
- **Ten new scenarios**: `test_sim_stale`, `test_sim_ckp_enospc`,
  `test_sim_split_torn`, `test_sim_recover_corrupt`, `test_sim_secondary_crash`,
  `test_sim_largetxn_crash`, `test_sim_cursor_crash`, `test_sim_multi_fault`,
  `test_sim_latency_load`, `test_sim_ckp_lsn` (§3 table).
- **Swarm runner** `test_sim_swarm` + `dst-swarm.sh` with per-fault activation
  coverage (§3.1).
- **Two more planted bugs** (4 ABORTNOUNDO, 5 CKPBADLSN) at real sites, each
  caught at K=1 (§3).
- **short/EIO knob** wired into the write fault hook (was defined, unused).

---

## 5. Full scenario catalog (grow into FoundationDB-grade coverage)

Marked **v1** (mappable now on the single-process fault/crash axis),
**v1.x** (small extension of v1), or **v2** (needs the deterministic
scheduler / multi-process). ~34 scenarios across BDB subsystems.

### Access methods (workload correctness under faults)
1. **btree put/get/del** round-trip under corrupt-read — *v1* ✅ `test_sim_torn`.
2. **btree split/merge** churn across a crash + recovery — *v1* ✅ `test_sim_split_crash`.
3. **hash** insert/lookup/delete round-trip under faults — *v1.x* ✅ `test_sim_hash_crash`.
4. **recno / queue** append + consume across a crash — *v1.x* ✅ `test_sim_recno_crash`, `test_sim_queue_crash`.
5. **secondary index / join** consistency after recovery — *v1.x* ✅ `test_sim_secondary_crash`; multi-file/sub-database recovery ✅ `test_sim_multidb_crash`.
6. **large / overflow records** torn-write + checksum detection — *v1.x* ✅ `test_sim_overflow_torn`.
7. **duplicate keys** (sorted/unsorted) survive crash+recover — *v1.x* ✅ `test_sim_dup_crash`.

### Log / WAL
8. **commit durability**: every fsync-acked commit survives a crash — *v1* ✅ `test_sim_crash_recover` (capstone).
9. **ack-before-fsync bug caught** via the write-back frontier — *v1* ✅ planted bug 1, caught by the capstone.
10. **torn log write**: recovery stops cleanly at the torn record — *v1.x* ✅ `test_sim_torn_log`.
11. **log record checksum** mismatch detected on replay — *v1.x* ✅ (partial) `test_sim_recover_corrupt` (corrupt reads during recovery caught, never silent).
12. **log file rollover** crash at the boundary, clean recovery — *v1.x* ✅ `test_sim_logrollover_crash`.
13. **in-memory log** (`DB_LOG_IN_MEMORY`) crash semantics — *v1.x*.

### Recovery
14. **crash at every phase**: pre-commit, post-log-pre-fsync, post-fsync,
    mid-checkpoint, mid-recovery — parameterized by a seeded crash step — *v1*
    (recovery-robustness angle ✅ `test_sim_recover_corrupt`; latency angle ✅
    `test_sim_latency_load`; the checkpoint durability-window ✅
    `test_sim_data_log_order`; a compound latency+ENOSPC+torn crash ✅
    `test_sim_compound_fault`; the full per-phase parameterization remains).
15. **catastrophic (fatal) recovery** from an archived log set — *v1.x*.
16. **recovery idempotency**: recover twice, identical state hash — *v1* ✅ `test_sim_recover_idempotent`.
17. **partial page write** at crash; recovery repairs via WAL — *v1* ✅ `test_sim_torn_meta` (torn META page during a checkpoint caught by the checksum).

### Checkpoint
18. **crash mid-checkpoint**; recovery from the prior checkpoint — *v1* ✅ `test_sim_ckp_crash` (page-flush durability; catches planted bug 3), `test_sim_ckp_lsn` (checkpoint-LSN correctness; catches planted bug 5).
19. **checkpoint + ENOSPC**: checkpoint fails cleanly, txns still durable — *v1.x* ✅ `test_sim_enospc`; log-write ENOSPC + recover ✅ `test_sim_log_enospc` (catches LOGWRITEIGNORE).

### Buffer pool (mpool)
20. **eviction under memory pressure** + corrupt-read on refetch — *v1* ✅ `test_sim_torn` (DB_PRIVATE small cache).
21. **dirty-page flush torn write** caught by page checksum — *v1.x* ✅ `test_sim_split_torn` (torn split-page flush caught by the checksum); `test_sim_overflow_torn` covers overflow-page corrupt reads.
22. **MVCC version-chain fget** returns the correct snapshot under faults — *v1.x* ✅ (stale-metadata angle) `test_sim_stale_meta` (a stale read of a real DB meta page is caught by the page LSN+checksum, never adopted).
23. **trickle / sync** interaction with a crash — *v2* (needs concurrency).

### Lock / deadlock
24. **deadlock detection** picks a victim deterministically (seeded) — *v2*.
25. **lock timeout** under seeded clock skew — *v2* (needs virtual clock).
26. **lock-table region exhaustion** graceful degradation — *v1.x*.

### Transactions
27. **commit/abort mix**: aborted txns leave no trace after recovery — *v1* ✅ `test_sim_abort_atomic`; large explicit-abort atomicity ✅ `test_sim_largeabort`.
28. **nested / child txn** commit+abort correctness across crash — *v1.x*.
29. **prepare/2PC**: prepared txns recover to the resolvable state — *v1.x*.
30. **cursor stability** across abort — *v1.x* ✅ `test_sim_cursor_crash` (cursor mutations durable + exact live set after recover).

### MVCC / SSI (roadmap feature #0)
31. **snapshot isolation visibility** under faults — *v1.x*.
32. **SSI rw-conflict abort** deterministically reproduced — *v2* (concurrency).
33. **SIREAD marker reclaim** correctness across crash+recover — *v1.x*.

### Multi-process / scheduler (the v2 frontier)
34. **concurrent writers** deterministic interleaving + crash — *v2* (scheduler
    pending); **process-death-mid-txn + failchk recovery** ✅ **PILOT LANDED**
    (`test/sim/mp_failchk_pilot.c` + `mp-failchk.sh`; see
    `test/sim/DST-V2-DESIGN.md`) — two real processes share a real
    (non-`DB_PRIVATE`) region, one is killed mid-txn holding a write lock, the
    other runs `DB_ENV->failchk`; **found a real failchk EBUSY recovery defect**
    (DST-V2-DESIGN §3a).
35. **network partition / replication** role change (repmgr) — *v2* (xtc models
    partition at the message seam; BDB's real socket transport needs a v2 shim).

---

## 6. References

- xtc DST core: `/home/gburd/ws/xtc/src/evt/sim.c`,
  `src/io/io_sim.c`, `src/inc/xtc_sim.h`, `src/inc/xtc_dst_inject.h`,
  `test/sim/test_sim_*.c`.
- BDB OS seam: `src/os/os_rw.c`, `os_fsync.c`, `src/dbinc/os.h`,
  `src/dbinc_auto/os_ext.h`.
- Recovery-before-verify lesson: `.agents/concurrent-btree-corruption.md`.
- FoundationDB simulation testing; TigerBeetle VOPR.
