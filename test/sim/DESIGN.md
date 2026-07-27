# libdb Deterministic Simulation Testing (DST) — design & roadmap

> Canonical copy lives at `.agents/dst-design.md` (gitignored, per repo convention); this is the committed PR copy.

Status: **v1 foundation landed** (this branch). Seeded PRNG tree, determinism
guard, buggify, simulated-I/O fault knobs, the write-back-cache durable-frontier
crash model, the `__os_*` I/O hooks, a `--enable-dst` build switch that is
zero-overhead when off, and three runnable pilot scenarios (RNG, crash+recover,
torn/corrupt-read). Modeled on FoundationDB / TigerBeetle and the xtc project's
DST (`/home/gburd/ws/xtc`).

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
| `DB_SIM_RNG_SCHED` | **reserved** for the v2 deterministic scheduler |

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

`DB_SIM_BUGGIFY("name")`: a named point in real library code that, under sim,
takes a legal-but-pessimal path. Unlike a per-call fault, a buggify point is a
coin flipped **once per run per site**, cached, so all reaches of a name agree
and the run replays. Drawn from the dedicated `BUGGIFY` stream so enabling it
never perturbs the IO/FAULT streams. Compiles to constant 0 when DST is off.

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

**The write-back model (THE key to catching ack-before-fsync bugs).** The sim
writes to a *real* file, so bytes reach the file on `pwrite` regardless of
`fsync` — which means a naive crash test cannot catch a writer that ACKs a
commit without fsyncing it. The model fixes this honestly:

- a write records the **written high-water** offset for the file
  (`__db_sim_io_wb_wrote`);
- `fsync`/`fdatasync` promotes written → **durable** (`__db_sim_io_wb_synced`);
- a crash loses everything past the last fsync;
- a recovery test asks `__db_sim_io_durable_end(key)` for the true post-crash
  durable frontier to truncate to, instead of trusting the writer.

Keyed by a **stable FNV-1a hash of the file name** (not the fd), so the
frontier tracks a *logical* file across libdb's frequent close/reopen — exactly
how a real disk behaves.

### 1.5 The `__os_*` I/O seam (where the hooks attach)

BDB funnels durability through a tiny OS layer, which is the clean seam:

| Function (file) | Hook added (under `#ifdef HAVE_DST`) | Purpose |
|---|---|---|
| `__os_io` write fast path (`os_rw.c`) | `__db_sim_io_write_off_hook(fhp, off+len)` | record written high-water (covers WAL + page writes) |
| `__os_io` read fast path (`os_rw.c`) | `__db_sim_io_read_hook(buf, len)` | corrupt-read bit flip |
| `__os_fsync` (`os_fsync.c`) | `__db_sim_io_sync_hook(fhp)` | promote written → durable |

The WAL write path (`__log_fill`/`__log_write` → `__os_io(DB_IO_WRITE, lfhp,
0,0, w_off, len, …)`) and the WAL fsync (`__log_flush_int` → `__os_fsync(env,
lfhp)`) both route through these exact hooks — so the write-back model tracks
the log's durable frontier precisely, which is the ack-before-fsync seam.

Each hook is a single `__db_sim_active()` relaxed load in the common no-sim
case, and vanishes entirely when `HAVE_DST` is undefined.

> **Remaining integration (documented, not yet wired):** the slow/`__os_physwrite`
> path (non-`pread` platforms, `j_write` hook, `HAVE_FILESYSTEM_NOTZERO`) is not
> hooked — on Linux with pread/pwrite the fast path always wins, so v1 is fully
> covered there. `os_aio*` (the newer async buffer-pool I/O) is likewise not yet
> hooked; the buffer pool's *synchronous* `__os_io` path (`mp_bh.c`) is, which is
> what the pilots exercise. Torn-write and latency knobs exist but are only
> consulted by the corrupt-read hook so far; wiring torn-write into the write
> hook and latency into a v2 async path are §4 items.

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
  compile rules, and a `dst_tests` target (`test_sim_rng`,
  `test_sim_crash_recover`, `test_sim_torn`). `DSTBUG=<n>` plants bug n.
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

## 3. Pilot scenarios (runnable now)

| Pilot | Proves | Result |
|---|---|---|
| `test_sim_rng` | seeded PRNG determinism, seed-sensitivity, stream independence, guard | PASS |
| `test_sim_crash_recover` | **capstone**: N durable txns survive a mid-txn crash; uncommitted txn does not; DB verifies clean **after recovery** | PASS (64 txns; deterministic across seeds; replays) |
| `test_sim_torn` | corrupt reads are caught by DB_CHKSUM or invisible — **never silently wrong** | PASS (e.g. 297 correct / 103 detected / 0 silent-bad) |

**Recovery-before-verify discipline** (from
`.agents/concurrent-btree-corruption.md`): a crashed txn env verified *without*
recovery falsely looks corrupt. `test_sim_crash_recover` **always** runs
`DB_RECOVER` before `db->verify`.

**Planted-bug harness** (`sim_inject.h`, `DB_DST_INJECT_BUG=<n>`): the crash
pilot has a `NODURABLE` hook (bug 1) that "acks" a commit with `DB_TXN_NOSYNC`
and asserts it must *not* survive the crash — the scaffold for the
FoundationDB-grade "DST finds real bugs within K seeds" proof. v1 wires the hook
and the harness; planting the bug *inside* `__log_flush` (so the write-back
model's durable frontier truly drops the un-fsynced tail) is the immediate next
step (§4, item A) that turns this into a hard catch.

---

## 4. Immediate next steps (ordered)

- **A. Real ack-before-fsync catch.** Truncate the log to
  `__db_sim_io_durable_end(logkey)` before `DB_RECOVER` in the crash pilot, and
  add a `DB_SIM_BUGGIFY("log.flush.skip_fsync")` in `__log_flush_int` that skips
  the fsync while still returning success. Then bug-build must lose the "acked"
  commit → capstone fires. This is the headline DST proof.
- **B. Torn-write into the write hook.** Consult `__db_sim_io_torn_prefix` in
  the write path and add a torn-log scenario (recovery must stop at the torn
  record, not misparse past it).
- **C. Disk-full scenario.** Arm `__db_sim_io_enospc` mid-workload; assert
  graceful degradation (clean error, no corruption, recoverable).
- **D. Stale-read model.** Port xtc's superseded-write ring
  (`__xtc_sim_io_stale_*`) to catch recovery/cache code that skips an LSN check.
- **E. Seed sweep driver.** `scripts/dst-sweep.sh SEED_LO SEED_HI` running each
  scenario across a seed range, plus `scripts/dst-bug-inject.sh` asserting each
  planted bug is caught within K seeds (the bug-detection-latency yardstick).
- **F. Meson wiring** (§2).
- **G. Determinism-guard planting** at `__os_clock`, `__os_unique_id`,
  `__os_id`, so a regression that reads wall-clock time on a sim path aborts.
- **H. `os_aio*` + `__os_physwrite` hooks** for full I/O-path coverage.

---

## 5. Full scenario catalog (grow into FoundationDB-grade coverage)

Marked **v1** (mappable now on the single-process fault/crash axis),
**v1.x** (small extension of v1), or **v2** (needs the deterministic
scheduler / multi-process). ~34 scenarios across BDB subsystems.

### Access methods (workload correctness under faults)
1. **btree put/get/del** round-trip under corrupt-read — *v1* (shipped: torn).
2. **btree split/merge** churn across a crash + recovery — *v1*.
3. **hash** insert/lookup/delete round-trip under faults — *v1.x*.
4. **recno / queue** append + consume across a crash — *v1.x*.
5. **secondary index / join** consistency after recovery — *v1.x*.
6. **large / overflow records** torn-write + checksum detection — *v1.x*.
7. **duplicate keys** (sorted/unsorted) survive crash+recover — *v1.x*.

### Log / WAL
8. **commit durability**: every fsync-acked commit survives a crash — *v1* (capstone).
9. **ack-before-fsync bug caught** via the write-back frontier — *v1* (item A).
10. **torn log write**: recovery stops cleanly at the torn record — *v1.x* (item B).
11. **log record checksum** mismatch detected on replay — *v1.x*.
12. **log file rollover** crash at the boundary, clean recovery — *v1.x*.
13. **in-memory log** (`DB_LOG_IN_MEMORY`) crash semantics — *v1.x*.

### Recovery
14. **crash at every phase**: pre-commit, post-log-pre-fsync, post-fsync,
    mid-checkpoint, mid-recovery — parameterized by a seeded crash step — *v1*.
15. **catastrophic (fatal) recovery** from an archived log set — *v1.x*.
16. **recovery idempotency**: recover twice, identical state hash — *v1*.
17. **partial page write** at crash; recovery repairs via WAL — *v1*.

### Checkpoint
18. **crash mid-checkpoint**; recovery from the prior checkpoint — *v1*.
19. **checkpoint + ENOSPC**: checkpoint fails cleanly, txns still durable — *v1.x* (item C).

### Buffer pool (mpool)
20. **eviction under memory pressure** + corrupt-read on refetch — *v1* (torn uses DB_PRIVATE small cache).
21. **dirty-page flush torn write** caught by page checksum — *v1.x* (item B).
22. **MVCC version-chain fget** returns the correct snapshot under faults — *v1.x*.
23. **trickle / sync** interaction with a crash — *v2* (needs concurrency).

### Lock / deadlock
24. **deadlock detection** picks a victim deterministically (seeded) — *v2*.
25. **lock timeout** under seeded clock skew — *v2* (needs virtual clock).
26. **lock-table region exhaustion** graceful degradation — *v1.x*.

### Transactions
27. **commit/abort mix**: aborted txns leave no trace after recovery — *v1*.
28. **nested / child txn** commit+abort correctness across crash — *v1.x*.
29. **prepare/2PC**: prepared txns recover to the resolvable state — *v1.x*.
30. **cursor stability** across abort — *v1.x*.

### MVCC / SSI (roadmap feature #0)
31. **snapshot isolation visibility** under faults — *v1.x*.
32. **SSI rw-conflict abort** deterministically reproduced — *v2* (concurrency).
33. **SIREAD marker reclaim** correctness across crash+recover — *v1.x*.

### Multi-process / scheduler (the v2 frontier)
34. **concurrent writers** deterministic interleaving + crash — *v2*.
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
