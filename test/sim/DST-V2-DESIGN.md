# libdb DST v2 — deterministic multi-process design & failchk pilot

> Canonical working copy lives at `.agents/dst-v2-design.md` (gitignored, per
> repo convention); this is the committed PR copy. v1 (single-process
> fault-injection + crash/recovery) is documented in `test/sim/DESIGN.md`;
> read that first. This document is the v2 plan and records the **pilot that
> actually landed** on this branch.

Status: **v2 scaffold + failchk-recovery pilot landed.** The `DB_SIM_RNG_SCHED`
stream (reserved in v1) is now consumed by a new `sim_sched.h` seam that seeds
the multi-process kill point. A working two-process pilot
(`test/sim/mp_failchk_pilot.c` + `test/sim/mp-failchk.sh`) opens a **real
shared region** (not `DB_PRIVATE`), kills one process mid-transaction while it
holds write locks, and drives `DB_ENV->failchk` in a survivor to prove the dead
process's transaction is aborted and its locks released. No deterministic
scheduler is claimed or built — the honest boundary is drawn in §3.

---

## 0. The hard architectural truth

v1 got determinism by staying **single-process** and using `DB_PRIVATE` where a
cold cache mattered: heap regions, no cross-process mmap, no real inter-process
mutex contention. Every source of nondeterminism funnels through the `__os_*`
seam the sim owns, so a seed replays bit-for-bit.

BDB's actual differentiator — the thing v1 does **not** test — is the opposite:
**multiple OS processes sharing `mmap`'d regions** (buffer pool, lock table,
log region, txn region), coordinating through **real region mutexes/latches**
and **real `fsync`** durability. That is where BDB's crash-recovery story is
uniquely load-bearing: a process can die *holding a shared latch or mid-txn*,
and the surviving processes must detect it (`DB_ENV->failchk`) and recover the
shared region without a full environment restart.

This is genuinely nondeterministic in a way v1 is not:

- **OS scheduling** decides which process touches the shared region when. There
  is no single seam that owns cross-process interleaving.
- **Memory visibility** across processes is real hardware/OS behavior.
- **A killed process** leaves the shared region in whatever intermediate state
  the OS scheduler happened to stop it at.

FoundationDB sidesteps all of this by running every "process" as a cooperative
fiber on **one thread** with a virtual clock — one scheduler owns every
interleaving. **BDB cannot do that**: its processes are real `fork`/`exec`
processes on real `mmap`, precisely so that a hard `kill -9` models a real
crash. Turning them into cooperative fibers would delete the exact property v2
exists to test.

So v2 is fundamentally harder than xtc's fiber model, and pretending a
single-thread scheduler applies would be coverage theater. v2 is **phased**,
and this branch delivers the phase with the best value-to-tractability ratio.

---

## 1. The three v2 approaches (assessed honestly)

### (a) N child processes + a harness that imposes a seeded schedule via yield points

Each process, under sim, blocks at seeded **yield points** on a shared
coordination primitive (a per-process semaphore or a named pipe). A controlling
harness holds a seeded ready-queue and releases exactly one process to run one
step at a time, in a seed-determined order. This gives a **deterministic
interleaving of real processes sharing a real region** — the real thing.

- **Cost:** high. Every yield point must be *planted* at BDB's own
  lock/latch/commit seams (not just at test-level boundaries), or the
  interleaving is too coarse to catch the interesting races. The harness must
  drive N processes through a barrier per step; a process that dies must be
  detected and dropped from the ready-queue without wedging the others.
- **Coverage:** this is the only approach that catches *interleaving-dependent*
  shared-region bugs (a latch acquired in the wrong order across processes, a
  torn shared-region update visible to a peer). It is the v2 north star.
- **Determinism boundary:** even with yield points, work *between* yield points
  runs at real OS speed and real memory-visibility rules — so it is
  deterministic at yield-point granularity, not instruction granularity. That
  is the honest limit and it is still enormously valuable (it is essentially
  PCT / coarse-interleaving controlled scheduling).

### (b) Single-process, multiple ENVs / thread-simulated "processes", cooperatively scheduled

Run N "processes" as N threads (or N `DB_ENV` handles) in one process,
cooperatively scheduled like xtc fibers.

- **Cost:** low — closest to xtc, reuses the v1 seam philosophy.
- **Coverage:** poor for the thing that matters. Threads in one process share
  one address space, not a cross-process `mmap` region with separate page
  tables; `kill -9` of a thread is not a process crash; region mutexes behave
  differently (`DB_MUTEX_PROCESS_ONLY`). **It does not test the real
  cross-process shared-region mmap path**, which is the entire point of v2.
  Rejected as the primary vehicle — it would be faithful-looking coverage of
  the wrong thing.

### (c) A failchk-focused pilot: process-death-mid-operation + failchk recovery

The single highest-value multi-process fault is exactly the one v1 cannot
reach: **a process died holding a lock / mid-transaction, and another process
runs `DB_ENV->failchk` to detect and recover** — without a full deterministic
scheduler. Two real processes, a real shared (non-`DB_PRIVATE`) region, a
seeded kill point, and `failchk` in the survivor.

- **Cost:** low-to-moderate. No scheduler, no yield-point planting. `fork` +
  `kill` + `failchk` + assertions, `timeout`-guarded.
- **Coverage:** it directly exercises `src/env/env_failchk.c`,
  `__lock_failchk`, `__txn_failchk`, `__dbreg_failchk`, `__memp_failchk`,
  `__mut_failchk` — the multi-process crash-recovery path that has **zero** DST
  coverage today and is BDB's actual differentiator.
- **Determinism:** the *fault* (which point A dies at) is seeded on
  `DB_SIM_RNG_SCHED`; the *interleaving* is not controlled (A is killed by a
  signal at a chosen operation boundary, not a scheduler-owned step). This is
  honest: it is a **deterministic fault, nondeterministic interleaving** pilot,
  which is the right first rung.

**Chosen for the pilot: (c).** It is the most valuable *and* most tractable
multi-process fault to land now, and it is the natural substrate that approach
(a) later builds the scheduler on top of. Approach (b) is rejected as
low-fidelity; approach (a) is the roadmap's phase 2.

---

## 2. The pilot: what actually landed

Files (all new, owned by v2, zero-overhead when DST is off):

- `test/sim/sim_sched.h` — the v2 seam. Draws the kill point from
  `DB_SIM_RNG_SCHED` (the reserved stream, so v1 seeds do not shift) and
  defines the yield-point model that phase 2 will consume. Header-only,
  dependency-free.
- `test/sim/mp_failchk_pilot.c` — the two-role driver (victim / survivor).
- `test/sim/mp-failchk.sh` — the multi-process runner (spawns, kills, cleans up
  orphans, `timeout`-guards, verifies).

### 2.1 Roles and flow

The pilot is one executable run in two roles (`victim` / `survivor`), plus a
top-level `run` role that orchestrates. All share one real environment home
opened with `DB_INIT_LOCK | DB_INIT_LOG | DB_INIT_MPOOL | DB_INIT_TXN`
(**never** `DB_PRIVATE` — the region is a real cross-process `mmap`), with
`is_alive` / `thread_id` / `set_thread_count` configured so `failchk` can run.

1. **Setup (run role):** create the shared env + a btree DB, commit a set of
   "durable" records, close cleanly.
2. **Victim (child process):** attach the shared env, begin a transaction,
   `put` a record (acquiring a **write lock** on a page in the shared lock
   table), then **stop at the seeded yield point** — it writes a sentinel file
   announcing "I hold a write lock in txn T" and blocks (a bounded sleep) so
   the parent can kill it deterministically at that operation boundary. The
   yield point index is drawn from `DB_SIM_RNG_SCHED(seed)`.
3. **Kill:** the run role waits for the sentinel, then `kill -9`s the victim —
   a real process crash leaving its transaction open and its write lock held in
   the shared region.
4. **Survivor (child process):** attach the **same** shared env, run
   `DB_ENV->failchk`. `failchk` walks the thread table, finds the victim's pid
   is no longer alive (`is_alive` returns 0), and:
   - `__lock_failchk` sees the dead locker is transactional with write locks →
     leaves it for `__txn_failchk`;
   - `__txn_failchk` aborts the dead transaction, which releases its write
     locks;
   - `__dbreg_failchk` / `__memp_failchk` / `__mut_failchk` clean up dbreg,
     pinned buffers, and region mutexes the dead process held.
5. **Verify (survivor, then run role):** after `failchk`, the survivor asserts
   it can now acquire a write lock on the same key the victim held (proving the
   lock was released), reads back the committed set (proving durable data
   intact and the victim's uncommitted put is gone), and the DB `verify`s
   clean. If `failchk` returns `DB_RUNRECOVERY`, the survivor runs
   `DB_RECOVER` and re-verifies (the documented failchk→recovery escalation).

### 2.2 How it uses `DB_SIM_RNG_SCHED`

The kill point (which operation boundary the victim stops at) is
`__db_sim_sched_killpoint(nsteps)` = `__db_sim_rng_range(DB_SIM_RNG_SCHED,
nsteps)`. Because `SCHED` was reserved (never drawn) in v1, seeding it here does
**not** move any v1 draw sequence — a v1 scenario replays byte-identically with
or without v2 present. The pilot passes its seed to the victim via argv so both
roles derive the same kill point.

### 2.3 The failchk fault model

| failchk step | Dead-process state it recovers | Contract |
|---|---|---|
| `__env_in_api` | died inside a BDB API call | `DB_RUNRECOVERY` if a non-blocked thread died in the library |
| `__lock_failchk` | dead **non-transactional** locker holding **write** locks | `DB_RUNRECOVERY` (only 1-of-N pages may be modified) |
| `__lock_failchk` | dead locker holding **read** locks | releases them in place, no recovery |
| `__txn_failchk` | dead **transactional** locker (open txn) | aborts the txn → releases its write locks, no recovery |
| `__dbreg_failchk` | dead process's open DB handles | reclaims dbreg slots |
| `__memp_failchk` | dead process's pinned buffers | unpins |
| `__mut_failchk` | dead process holding a **region mutex** | recovers/clears the mutex |

The pilot's primary scenario is the **transactional-write-lock** row: the
victim dies mid-txn holding a write lock, and `failchk` must abort the txn and
free the lock **without** a full environment restart. This is the multi-process
recovery path with zero prior DST coverage.

### 2.4 The severe-bug contract (the highest-value find)

If the pilot shows that after `kill -9` + `failchk` the shared region is
**still bad** — the victim's write lock never released, its txn never aborted,
or a region mutex held forever (survivor deadlocks / `failchk` returns clean but
the lock is still held) — that is a **severe real bug in BDB's multi-process
crash-recovery**. The pilot is `timeout`-guarded precisely so a "held forever"
mutex shows up as a timeout, not a wedged machine. Such a find is reported with
the seed + repro; engine code is **not** touched (report for a focused fix).

---

## 3. What is deterministic, and what is NOT (the honest boundary)

- **Deterministic:** the kill point (seeded on `SCHED`), the workload keys/values
  (seeded on `APP`), the committed set, and therefore the *post-recovery
  expected state*. Re-running the same seed drives the victim to the same
  operation boundary and expects the same survivor outcome.
- **NOT deterministic:** the exact instruction at which `kill -9` lands
  (signal delivery timing), the OS scheduling of the two processes between
  yield points, and cross-process memory-visibility ordering. The pilot
  controls the *fault* to an operation boundary, not the *interleaving* to an
  instruction. This is a **deterministic-fault, nondeterministic-interleaving**
  pilot — not a deterministic scheduler.

A genuine deterministic multi-process scheduler (approach (a)) is **not** in
this branch and is not faked. It is phase 2.

---

## 3a. FINDING: failchk leaves a dead process's DB-handle mutex unrecovered (EBUSY)

The pilot found a **real multi-process crash-recovery defect**, reproducible on
**every** seed (all four kill points), against **stock** engine.

### Symptom

When the victim dies mid-txn while it has an **open DB handle** (holding its
per-process `MTX_DB_HANDLE` mutex, `DB_MUTEX_PROCESS_ONLY`), the survivor's
`DB_ENV->failchk`:

1. correctly aborts the dead transaction (`BDB4503 Aborting txn 0x80000024`),
2. correctly frees the dead locker's read locks (`BDB2053`),
3. correctly frees the dead process's log/dbreg info (`BDB1502`),
4. **then returns `EBUSY` (16, `BDB2027 unable to destroy mutex: Device or
   resource busy`) instead of `0` or `DB_RUNRECOVERY`.**

EBUSY is **outside failchk's documented return contract** (it should return `0`
for recovered-in-place, or `DB_RUNRECOVERY` to tell the caller to run full
recovery). A caller that gets a raw EBUSY has no defined recovery path. The
pilot escalates to `DB_RECOVER` anyway and the DB then verifies clean, so data
is not lost -- but failchk's promise of *in-place* recovery without a full
environment restart is broken for this (extremely common) case.

### Root cause (confirmed by a one-line probe)

`__env_failchk_pp` marks the failchk thread `THREAD_FAILCHK`
(`FAILCHK_THREAD(env, ip)`). `__db_pthread_mutex_destroy` relies on that marker
(`ip->dbth_state == THREAD_FAILCHK`) to *skip* the hard-error on a mutex it
cannot destroy because a dead process still holds it (the intended, benign
failchk behavior -- the mutex is reclaimed lazily later).

But the failchk **cleanup** path re-enters the library:
`__dbreg_failchk` -> `__dbreg_close_id_int` -> `__dbreg_log_close` ->
`__dbreg_register_log` -> `__log_put`, and `__log_put` does its own
`ENV_ENTER(env, ip)`, which calls `__env_set_state(..., THREAD_ACTIVE)` and
**flips the failchk thread's state from `THREAD_FAILCHK` back to
`THREAD_ACTIVE`** (then `ENV_LEAVE` sets it to `THREAD_OUT`). When control
returns to `__dbreg_teardown_int` -> `__mutex_free` ->
`__db_pthread_mutex_destroy`, the `THREAD_VERIFY` lookup finds
`dbth_state == THREAD_ACTIVE`, so `failchk_thread` is computed as `FALSE`, and
the EBUSY becomes a hard error propagated out of `failchk`.

**Proof:** re-running the pilot with a single-line probe in
`__db_pthread_mutex_destroy` -- treat any thread under a `DB_ENV_FAILCHK` env
as the failchk thread (`if (ret == 0 && ip != NULL) failchk_thread = TRUE;`) --
makes `failchk` return **`0 (recovered in place)`**, no EBUSY, no DB_RECOVER
escalation, DB verifies clean. Reverting the probe restores the EBUSY. This
isolates the defect to the clobbered `THREAD_FAILCHK` marker.

### Repro

```
cd build_unix && make mp_failchk_pilot
../test/sim/mp-failchk.sh 0x51ED    # any seed reproduces it
# survivor prints: failchk returned: Device or resource busy  ==> SEVERE
```

### Suggested fix (for a focused, separate change -- NOT applied here)

The robust fix keeps the `THREAD_FAILCHK` marker stable across the failchk
cleanup path. Two candidate approaches, either as a focused PR:

1. In `__db_pthread_mutex_destroy` (and the fcntl/tas twins), when
   `F_ISSET(env->dbenv, DB_ENV_FAILCHK)` is set and the *current* thread is the
   one running failchk, treat it as the failchk thread even if its transient
   `dbth_state` was momentarily reset by a nested `ENV_ENTER`. (The probe above
   is a blunt version; the proper version distinguishes "this pid+tid is the
   failchk thread" from "an unrelated thread in a failchk-flagged env".)
2. Have `ENV_ENTER`/`ENV_LEAVE` preserve and restore a `THREAD_FAILCHK`
   marker instead of overwriting it with `THREAD_ACTIVE`/`THREAD_OUT` when the
   thread was already the failchk thread.

Engine code is deliberately **not** touched on this branch (per the pilot's
report-don't-fix rule); this section is the precise, reproducible report.

---

## 4. Phased roadmap

- **Phase 1 (this branch): failchk-recovery pilot.** Approach (c). Two real
  processes, real shared region, seeded kill point, `failchk` recovery,
  `timeout`-guarded, orphan-safe. DONE.
- **Phase 1b (partially realized via the finding above): region/handle-mutex-held
  pilot.** The failchk EBUSY finding (sec.3a) IS a process killed while holding
  a per-process mutex (its `MTX_DB_HANDLE`), where the survivor's failchk cannot
  destroy it -- exactly the "held-forever mutex" risk this phase targets,
  surfaced by the txn-lock pilot without extra code. A dedicated
  region-mutex-held scenario (e.g. `MTX_LOCK_REGION`) is a small follow-up.
- **Phase 2: the deterministic scheduler.** Approach (a). Plant seeded yield
  points (`__db_sim_sched_yield(site)` in `sim_sched.h`, already stubbed) at
  BDB's real lock/latch/commit seams under `#ifdef HAVE_DST`; a harness holds
  the seeded ready-queue and releases one process per step through a shared
  barrier. This makes cross-process interleaving deterministic at yield-point
  granularity and lets the swarm explore *interleaving-dependent* shared-region
  bugs. Large; the `SCHED` stream + `sim_sched.h` seam are the foundation.
- **Phase 3: replication / network faults on top.** With the deterministic
  multi-process substrate in place, model repmgr role changes and network
  partitions at the message seam (as xtc does), driving replication crash /
  election scenarios deterministically. Depends on phase 2.

---

## 5. Running the pilot

From the `--enable-dst --enable-test` build tree (`build_unix`):

```
make mp_failchk_pilot
../test/sim/mp-failchk.sh            # default seed sweep, timeout-guarded
../test/sim/mp-failchk.sh 0x51ED     # a single seed
```

The runner spawns the victim + survivor as real processes, kills the victim,
runs `failchk` in the survivor, verifies, and **kills every process it spawned
on exit** (a trap handler + process-group kill) so a hang or a "held-forever"
mutex cannot leave orphans holding the shared region. Each run is
`timeout`-wrapped.

---

## 6. References

- v1 design: `test/sim/DESIGN.md` (§0 the xtc gap, the `SCHED` reservation).
- Multi-process test orchestration model: `test/tcl/env012.tcl` (DB_REGISTER +
  failchk, kill process 1, process 3 runs failchk and cleans up — env012.j),
  `test/tcl/ssi009.tcl` + `test/tcl/wrap.tcl` (spawn N workers, `watch_procs`).
- failchk internals: `src/env/env_failchk.c`, `src/lock/lock_failchk.c`,
  `src/txn/txn_failchk.c`, `src/dbreg/dbreg_stat.c` (`__dbreg_failchk`).
- FoundationDB simulation (single-thread fiber determinism — why BDB can't copy
  it) and TigerBeetle VOPR.
