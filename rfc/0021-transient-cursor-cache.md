# RFC 0021: Per-thread transient-cursor cache

- **Status:** Draft
- **Type:** Normative
- **Author:** libdb contributors (P1 workstream)
- **Date:** 2026-11-03
- **Tracking:** review item O-D1 (B/access-methods.md §6.4), perf rank 1
- **Prototype:** `src/db/db_am.c`, `src/db/db_cam.c` (this branch)

---

## Summary

`DB->get`, `DB->put` and `DB->del` with no application cursor allocate a
transient cursor per call (`__db_cursor_int`) and free it per call
(`__dbc_close`). Both ends take a cursor-queue partition mutex, which under
contention goes to the kernel futex and dominates the read path. This RFC adds a
small **process-local, per-thread** cache of idle transient cursors keyed by DB
handle. On the common path a transient operation reuses a cached cursor without
touching any handle queue mutex. The cached cursors stay linked on the handle's
active cursor queue for their whole cached lifetime, so cursor adjustment
(`__db_walk_cursors`) still sees every cursor that could be positioned on a page
being changed. No on-disk, log, region or ABI format changes; `sizeof(DB)` and
`sizeof(DBC)` are unchanged.

## Motivation

The decisive profile is in `test/bench/BATCHED-READS-RESULTS.md` §1.2: at t=96 on
an in-cache read-only workload through a shared handle,

```
47.07%  __db_cursor_int
46.49%  __dbc_close
 0.69%  __bam_cmp   (the actual key comparison)
```

`__db_cursor_int` + `__dbc_close` = **85% of the profile**. The real work
(`__bam_cmp`) is under 1%. The cost is not the B-tree descent or the buffer pin;
it is the cursor lifecycle. Every `DB->get` allocates a transient cursor and
frees it, and both ends take a `DB->cq_parts[part]` partition mutex to move the
cursor between the handle's free and active queues. Under contention that mutex
goes to `__db_pthread_mutex_lock -> futex`.

This is the single most-measured bottleneck in the access-method review packet.
It caps every point read on btree/hash/recno/queue/heap, every UNIQUE-constrained
typed put (O-T2), and it is the named cause of RFC 0007's t=96 per-key regression
(`OPTIMISTIC-READS-2026-09.md` §7: the faster optimistic descent reaches this
mutex sooner, so the point-read curve turns negative at 0.77× at t=96). The
`db_get_multiple` batched path (`BATCHED-READS-RESULTS.md`) already proved the
prize by amortising exactly this cost over N keys: 2.47× at t=96. This RFC
delivers the same relief to the unbatched single-key path that most applications
use.

The cursor-queue sharding history (`CURSOR-SHARD-RESULTS.md`, commit `7da0fadd3`,
`DB_CURSOR_NPART=8`) made the queues scale for reads by sharding them, but the
alloc/free still takes a partition mutex per call, and the *writer*-side walk
(`__db_walk_cursors`, O-D2) then had to lock all 8 partitions. The lead's R2
empty-partition skip (`src/db/db.c:1417-1446`, from #228) cut the writer cost by
reading each partition head unlocked and skipping empty ones. **This RFC's
correctness argument depends on that skip's invariant, and preserves it** (see
Design §5).

## North-star check

- **Embedded / no server:** unchanged. The cache is process-local state reached
  through a thread-local pointer; no new IPC, no server.
- **ACID / recovery:** unchanged. A cached cursor is functionally identical to a
  freshly allocated transient cursor; it goes through the same `__db_cursor_int`
  refresh and the same `__dbc_close` teardown of locks, pins and txn association.
  Recovery and abort never see the cache — recovery cursors (`DBC_RECOVER`,
  `DB_AM_RECOVER`) are **not** cached (Design §4).
- **All access methods:** the cache keys on `(DB handle, DBTYPE)` and reuses the
  existing per-type `am_*` init/refresh; btree, recno, hash, queue, heap all use
  the same transient path.
- **Multi-process correctness:** the cache holds **process-local pointers** to
  `DBC`/`DB` structures that already belong to this process. Nothing about the
  cache is placed in the shared region; no other process ever reads it. The
  cached cursors remain on the handle's active queue, which is in process-local
  heap (the `DB` handle and its `cq_parts` are per-process), exactly as any open
  application cursor is today.
- **On-disk / log / region / ABI format stability:** **none touched.**
  `DB_THREAD_INFO` (the shared-region per-thread block) is **not** changed — it
  holds `roff_t` offsets visible to other processes and is walked by failchk, so
  it cannot hold process-local pointers. The cache lives in a new process-local
  thread-local struct, not in any struct covered by `env_sig.c`. `sizeof(DB)`
  and `sizeof(DBC)` are unchanged. No escape token is required.

Because this moves where the cursor-queue latch boundary sits on the common path
(the queue mutex is no longer taken per transient op), it is RFC-gated per the
review's own rule, even though it is format-free and could in principle reach an
LTS line.

## Design

### 1. What is cached

Only **transient** cursors: those `__db_cursor` is asked to create with
`DB_CURSOR_TRANSIENT` and no other shape-changing flag. These are created by
`__db_get`, `__db_put` (non-bulk) and `__db_del`, used for exactly one
positioned operation, then closed. They are position-independent on error (the
reason the flag exists, `db_iface.c:780`).

A cursor is **eligible** for the cache only when all of these hold at close time:

- `DBC_TRANSIENT` is set and `DBC_BULK`, `DBC_OPD`, `DBC_DUPLICATE`,
  `DBC_PARTITIONED`, `DBC_FAMILY`, `DBC_RECOVER`, `DBC_WRITECURSOR`,
  `DBC_WRITER`, `DBC_READ_COMMITTED`, `DBC_READ_UNCOMMITTED`,
  `DBC_WAS_READ_COMMITTED` are clear;
- the cursor is not partitioned, not a secondary-index foreign cursor, not CDB;
- the handle is a `DB_THREAD` handle (so per-thread reuse is sound);
- the cursor's locker is its own default locker (`dbc->lref`, `DBC_OWN_LID`),
  not a txn locker — a cached cursor must never retain a transaction association
  (Design §3).

Everything else closes exactly as today. The eligibility test is a single flag
mask compare plus two pointer checks, off the critical cost.

### 2. Where the cache lives

A new **process-local, thread-local** structure, reached through a TLS pointer,
using the existing `DB_LO_TLS` portability guard pattern already in
`src/mutex/mut_order.c` (`__thread` / `__declspec(thread)`, with a no-TLS
fallback that simply disables the cache). The structure:

```c
/* Process-local; one per thread.  NOT in the shared region. */
struct __dbc_cache_entry {
	DB       *dbp;		/* Owning handle (identity key; NEVER deref'd
				 * once possibly stale -- validated only via
				 * the live-handle registry). */
	DBC      *dbc;		/* The idle cached cursor, on dbp's active queue. */
	DBTYPE    dbtype;
	u_int32_t gen;		/* dbp->cursor_cache_gen when cached (ABA). */
	u_int32_t lru;		/* Recency stamp for eviction. */
};
#define	DBC_CACHE_SLOTS	4	/* Small, fixed; LRU-evict on miss. */
static DBC_CACHE_TLS struct __dbc_cache_entry __dbc_cache[DBC_CACHE_SLOTS];
```

A fixed 4-slot direct cache per thread, keyed on the `DB *` handle pointer. Four
slots covers the overwhelmingly common 1–2-handle workloads with headroom; a
miss on a full cache evicts the least-recently-used slot by genuinely closing
its cursor (full `__dbc_close` + removal from the active queue). No heap
allocation for the cache itself.

`shortcut: 4 fixed slots, no per-handle growth. If a real workload fans a hot
thread across >4 handles, raise DBC_CACHE_SLOTS or make it a small open-addressed
map; the direct array keeps lookup to a 4-iteration pointer compare.`

### 3. The lifecycle

**Acquire (cache hit), on the common path of `__db_get`/`__db_put`/`__db_del`:**

1. Look up the thread cache for a slot whose `dbp` and `dbtype` match. On a hit,
   detach the `DBC` from the slot (mark the slot busy so re-entrancy cannot hand
   the same cursor out twice) and run the **same refresh** `__db_cursor_int`
   already runs on a reused free-queue cursor: `RESET_RET_MEM`, re-establish the
   locker/txn, clear per-op flags, `__bamc_refresh`/etc. The cursor is already
   `DBC_ACTIVE` and already on the active queue, so **no `CQ_LOCK` is taken.**
2. If there is no hit, fall through to the existing `__db_cursor_int`, which
   allocates or reuses a free-queue cursor under `CQ_LOCK` exactly as today
   (the cache-miss cost is unchanged from master).

**Release (eligible), on the common path of `__dbc_close`:**

1. Run the normal position/lock/pin teardown: `am_close(PGNO_INVALID, NULL)`
   resets `cp->pgno = PGNO_INVALID`, drops the page, `__LPUT` releases
   `dbc->mylock`, txn cursor count is decremented. After this the cursor holds
   **no page, no lock, no txn** — identical to any idle cursor on a free queue.
2. Instead of moving it to the free queue under `CQ_LOCK`, **leave it on the
   active queue** and store the pointer in the thread cache slot for `dbp`. The
   cursor stays `DBC_ACTIVE`. If the slot is occupied by a different handle's
   cursor, evict that one first (genuine `__dbc_close` tail: move to free queue
   under `CQ_LOCK`).
3. If the cursor is **not** eligible, close exactly as today (free-queue insert
   under `CQ_LOCK`).

The txn association is torn down at release and re-established at acquire from
the *current* call's txn, so a cached cursor never carries a stale transaction
across operations — this is why a txn-locker cursor (not its own `lref`) is not
cacheable: its locker belongs to a txn that may commit/abort between ops.

### 4. Invalidation — the correctness-critical events

A cached cursor is a live `DBC` on a live `DB` handle. The design keeps it
reachable for reclamation (it stays on the handle's active queue, which
`__db_refresh` walks and destroys at handle close) and makes a *stale* slot — one
whose handle was closed — a safe non-event, never a dangling dereference.

The mechanism is a process-global **epoch** used purely as a **change signal**
(not a validity stamp — the distinction is the fix for review #238's BLOCKER,
see the box below), a per-handle never-reused **serial**, and a process-global
**live-handle registry** that is the single source of truth for "is this slot's
handle still open?" — consulted without ever dereferencing a slot's `DB *` or
`ENV *`.

- **DB handle close** (`__db_close` / `__db_refresh`): `__db_refresh` walks
  `cq_parts[].active_queue` and destroys every cursor there under `CQ_LOCK`; a
  cached-but-idle cursor **is on that active queue**, so this closed handle's
  cached cursors are destroyed by the existing walk. Before that walk (and
  before the unlocked active-queue resync probe), `__db_cursor_cache_discard`:
  1. **Removes the handle from the registry** (`(dbp, serial)`, under the
     registry mutex). This happens-before the cursor teardown, so a concurrent
     reconcile on another thread either already finished moving the cached
     cursor to the free queue (the registry mutex serialises it) or now sees
     the handle as closed and leaves its queues alone. After discard returns,
     only the closing thread touches this handle's queues.
  2. **Clears the closing thread's own slots for `dbp`** (its own cached
     cursors are about to be destroyed).
  3. **Bumps the global epoch** so every *other* thread reconciles its slots.
- **Other threads' slots** (the cross-thread case): a thread does not clear its
  slots at another handle's close; instead, the next time it touches the cache
  it notices the epoch moved and **reconciles** (`__dbc_cache_reconcile`) under
  the registry mutex. For each occupied slot it asks
  `__dbc_cache_handle_live(dbp, serial)` — a pure registry lookup — and
  - **live** → keeps the slot (the cached cursor is still valid and reusable),
    or on thread exit returns the cursor to the handle's free queue;
  - **closed** → drops the slot, dereferencing nothing (the cursor was already
    destroyed by that handle's `__db_refresh`).
- **Environment close / `DB_ENV->close`:** handles close first (the contract
  forbids using a handle during env close), and each handle close removed its
  registry entry. A stale slot naming a handle in a since-freed env is simply
  **not found** in the registry, so it is dropped without touching the freed
  `DB`/`ENV` — this is the fix for review #238's cross-thread env
  use-after-free (rev238_a10), where the earlier `env->dblist` validation
  dereferenced a slot's freed `ENV`.
- **failchk** (`__env_failchk`): operates on the **shared region** thread-info
  of *dead* processes/threads; the process-local cache of a dead process dies
  with its address space, so there is nothing for failchk to clean in the cache.
- **Thread exit (implemented):** a pthread-key destructor
  (`__dbc_cache_thread_exit`, armed the first time a thread touches the cache)
  runs **in the exiting thread** and reconciles all its slots with the
  free-queue action: a still-live handle's cached cursor is returned to that
  handle's free queue under `CQ_LOCK`; a closed handle's slot is dropped. This
  replaces the earlier "leave them parked until handle close" behaviour, which
  review #238 (concern 2) measured as a real write-path regression — a churning
  thread pool piled inert cursors on the active queue and every insert walked
  them (up to 2.3× slower at 256 exited threads).

> **Why a process-wide epoch as a *validity stamp* was the BLOCKER.** The first
> implementation treated the epoch as "is my slot still valid?": a close of
> *any* handle bumped it, and a thread seeing the bump NULLed **every** slot.
> That silently dropped a *live* handle's cached cursor from its slot while the
> cursor stayed `DBC_ACTIVE` on that handle's active queue **forever** — an
> orphan walked by every later insert, unbounded. Measured: one thread doing
> get-on-H plus open/close-of-T for 20 k cycles made inserts on H 237× slower
> and added 32.7 MB. The fix is to treat the epoch only as a *change signal* and
> decide each slot's fate individually by registry lookup, so a live handle's
> slot is kept, never orphaned.

#### 4a. Telling a live slot from a stale one (the stale-slot design)

A stale slot holds a dangling `DB *` whose `ENV *` may also be freed. We must
never dereference either. The decision rests on three things:

1. **A process-global live-handle registry.** A small linked list of
   `(dbp pointer, serial)` for every currently-open handle, under one process
   mutex (`__dbc_reg_mtx`) that lives in no env (a `pthread_mutex_t`).
   `DB->open` adds (`__db_cursor_cache_register`); `__db_refresh` removes
   (`__db_cursor_cache_discard`) **before** the handle's cursors are destroyed.
   Validation is a registry lookup that dereferences **nothing** in the slot: a
   "live" result means the handle is open, so for as long as the caller holds
   the registry mutex both the handle and its env are valid and may be touched
   (e.g. to return the cursor to the handle's free queue).

   > The earlier design validated against `slot->env->dblist`, resting on the
   > claim "no slot outlives its env". That is false when the thread that
   > **closes** a handle is not the one that **cached** a cursor on it — the
   > normal pool-worker case. The registry removes the env dereference entirely.

2. **A per-handle, never-reused serial.** `DB` carries `cursor_cache_gen`,
   assigned at open from a process-global atomic counter. The registry entry and
   the slot both carry it. It defeats ABA: if a freed `DB`'s heap address is
   reused by a later open, the pointer may match an old slot but the serial will
   not, so the lookup misses and the slot is dropped.

3. **The epoch gates when validation runs.** Taking the registry mutex and
   walking the registry must stay off the per-op hot path. It runs **only when
   the epoch moved** since this thread last looked — once per observed handle
   close, not once per `DB->get`. On the common path (no handle closed recently)
   the epoch is unchanged and the cache take is a plain TLS array scan with no
   mutex. The cache-hit path for the handle the caller is *currently* operating
   on never validates at all: that handle is held open by the caller, so it is
   live by definition. (Measured: the registry adds no per-get cost — the read
   win is unchanged, §Prototype.)

**Lock order.** The registry mutex is a leaf: `DB->open` and `__db_refresh`
take it holding no cursor-queue or dblist mutex; reconcile and the put-path
eviction take it and then, only for a confirmed-live slot, `CQ_LOCK`
(registry → `CQ_LOCK`). Nothing takes `CQ_LOCK` then the registry mutex, so
there is no inversion.

Both the epoch and the serial counter are `db_atomic_t` read/incremented with
`atomic_read`/`atomic_inc`, so the cross-thread epoch read is ordered against
the close-time write on weakly ordered CPUs (review #238 concern 1: the original
plain `u_int32_t` raced, TSan-confirmed; benign on x86-TSO but UB in the C
memory model and unordered on ARM64/POWER).

### 5. Why cursor adjustment stays correct

`__db_walk_cursors` (`src/db/db.c`) must still see every cursor that could be
positioned on a page a writer is changing. The adjustment callbacks
(`__bam_ca_di_func`, `bt_curadj.c:195`) match a cursor **only** when
`cp->pgno == pgno && cp->indx >= indx`. The key facts:

1. **An idle cached cursor is inert to the walk.** At release, `am_close`
   dropped every page and lock (`DISCARD_CUR` → `__memp_fput` + `__TLPUT`). It
   does **not** reset `cp->pgno`/`cp->root` to `PGNO_INVALID` — those keep their
   stale values until the next *reuse*. An idle cached cursor can therefore sit
   on the active queue with a stale `cp->pgno`, and an adjustment callback
   (`__bam_ca_di_func`, say) *can* match it if that stale page number is later
   reused and changed, and mutate `cp->indx`/`cp->pgno`. This is still safe:
   **reuse wipes the position before the cursor is ever used again.**
   `__db_cursor_int` sets `cp->indx = 0; cp->pgno = PGNO_INVALID`
   unconditionally (`src/db/db_am.c`), and the access-method refresh
   (`__bamc_refresh`/etc.) recomputes `cp->root`, **before** any positioning
   search. So any idle-window mutation of a cached cursor's position cannot
   survive into a real operation: every transient op re-searches from a clean
   state under the page latch. (An earlier draft of this RFC said `am_close`
   sets `cp->pgno = PGNO_INVALID`; it does not — the safety comes from the
   reuse-time reset, not from a reset at release. Corrected per review #238.)

2. **A reused cached cursor is on the active queue before it positions.** On a
   cache hit we do **not** take it off the active queue; it was never removed.
   The refresh sets `cp->pgno = PGNO_INVALID` and only the subsequent
   `__bam_search`/`__bamc_search` positions it. So the moment the cursor acquires
   a real `cp->pgno`, it is already linked in the active queue. This is the
   precise property the empty-partition skip relies on
   (`src/db/db.c:1420-1446`): *"It was put on its partition queue at allocation,
   under the partition mutex, before it positioned itself. Positioning took the
   page latch, or a lock, that the writer now holds exclusively. So the queue
   insertion happens-before the writer's latch or lock acquisition."* With the
   cache, the queue insertion happened at the original allocation and was never
   undone, so it still happens-before any positioning — the happens-before edge
   is **strictly earlier**, never later. The invariant holds unchanged.

3. **No weaker than master.** On master a transient cursor is on the active queue
   from `__db_cursor_int`'s tail insert until `__dbc_close`'s removal, i.e. for
   its whole positioned life. With the cache it is on the active queue for a
   *superset* of that interval (also while idle). The walk never misses a cursor
   it would have caught; at worst it iterates over a few extra inert idle cursors
   (all `PGNO_INVALID`), which the callbacks skip in one compare.

4. **The walk's unlocked head read.** The skip reads
   `cqp->active_queue.tqh_first` unlocked and skips empty partitions. A cached
   cursor keeps the queue non-empty, so the partition is *not* skipped while a
   cached cursor lives there — strictly safe (we never cause a skip that master
   would not have caused; we can only prevent a skip). Mutation of the queue
   (the rare genuine alloc/evict) still happens under `CQ_LOCK`.

This argument is the RFC's load-bearing claim and is tested directly (Design §8,
and the new `run_cursorcache_adjust.sh` with a sabotage arm).

### 6. Struct growth

- **`DB`:** gains one field, `u_int32_t cursor_cache_gen`, the per-handle
  never-reused serial (§4a). It is assigned at open from a process-global
  atomic counter, recorded in the live-handle registry, and matched when
  validating a cache slot. **`sizeof(DB)` is unchanged (1744 on LP64):** the
  field lands in the 4-byte alignment hole that already existed between
  `adj_fileid` (offset 128) and the next pointer `log_filename` (offset 136),
  so it costs no extra bytes. `DB` is **not** in `env_sig.c` (it is a
  process-local handle, not a region struct), so even had it grown this would
  be ABI-only, not a region change; as it stands it is neither — `handle_sizes`
  passes unchanged and no escape token is needed. (The process-global epoch,
  serial counter, registry list and its mutex are all file-static in
  `src/db/db_cam.c`, not in any struct and not in `env_sig.c`.)
- **`DBC`:** unchanged. The cache stores a `DBC *`, it does not add fields to the
  cursor.
- **`DB_THREAD_INFO`:** unchanged (it is a shared-region struct; see North-star).
- **Region signature:** unchanged (`env_sig.c` covers none of the above).

### 7. Config / kill switch

A one-time cached `getenv("DB_NO_CURSOR_CACHE")` disables the cache entirely,
falling back to the master alloc/free path. This matches the fork's established
feature-switch pattern (`DB_NO_OPTREAD` in `bt_search.c`, `DB_NO_FNAME_CACHE` in
`db_am.c`, `DB_NO_GROUP_COMMIT` in `log_put.c`, `DB_NO_LOCKER_SHARD`) and adds no
public flag, no `db.h` regeneration and no ABI surface. It is the A/B and
bisection lever and the safety valve if a workload hits the eligibility edge.
Default: cache **on** for `DB_THREAD` handles in a threaded env; **off**
otherwise (non-threaded handles are never cached).

The cache is compiled in only on a **pthreads or hybrid** mutex backend, which
is where `__thread` TLS, a `pthread_key` destructor, and a `pthread_mutex` for
the registry are all available together. The default autoconf build is hybrid,
so the mainstream library has the cache; niche backends (pure TAS assembly,
UI-threads, Solaris LWP, WIN32) compile it out and behave exactly like master.

### 8. Tests

- **New `test/db/run_cursorcache_adjust.sh`** (+ its C driver): Part A opens,
  uses and closes a `DB_THREAD` handle `REOPENS` times — each cycle caches a
  transient cursor and then closes the handle (destroying that cursor) — so a
  reopened handle that reused freed memory would pick up a dangling cached
  cursor. Part B is a concurrent storm: many threads do transient
  `DB->put`/`DB->get` on crowded shared leaves of one handle while readers
  sweep and verify every value names its own key. The **sabotage arm** rebuilds
  `db_cam.o` with `-DDBC_CACHE_SABOTAGE`, which disables the stale-handle epoch
  guard (§4a); the test **must FAIL** in that arm (Part A dereferences a
  destroyed cursor — a crash/abort under `--enable-diagnostic`) and **PASS**
  with the guard intact. Note: the reuse of a cached cursor is itself safe by
  construction — every transient `DB->get`/`DB->put` re-searches from the root
  and runs under the page latch, so a stale position or a brief
  off-active-queue window cannot corrupt a single transient op; the sabotage
  therefore targets the one invariant whose failure IS observable, the
  stale-handle guard. (With the orphan fix there are now two stale-slot guards
  — the closing thread's self-clear and the cross-thread reconcile — so the
  sabotage arm disables *both* to recreate the use-after-free.)
- **New `test/db/run_cursorcache_orphan.sh`** (+ `cursorcache_orphan.c`): the
  review #238 BLOCKER reproducer, bounded for CI. A long-lived handle H, a loop
  of `DB->get` on H plus open/close of an unrelated handle T, then timed inserts
  on H and an RSS measurement. **FAIL if the insert ratio > 2 or RSS grows
  > 2 MB.** Pre-fix: ratio ~24 and +7.9 MB at 5 k cycles (diagnostic build);
  fixed: ~1.0 and +0 KB.
- **New `test/db/run_cursorcache_parkcost.sh`** (+ `cursorcache_parkcost.c`):
  the review #238 concern-2 reproducer, bounded for CI. Single-thread insert
  time on a shared handle before vs after 256 worker threads fill their cache
  slots and exit. **FAIL if the ratio > 1.5.** Pre-fix: ~2.1 at 256 threads;
  fixed: ~1.0.
- **New `test/db/run_cursorcache_envuaf.sh`** (+ `cursorcache_envuaf.c`): the
  review #238 rework reproducer (rev238_a10), built with ASan when available.
  A worker caches a cursor on H in env E and stays alive; the main thread
  closes H and E; the worker then either exits (pthread-key destructor
  reconciles) or opens a new env and gets (check_epoch reconciles).  Pre-fix
  (the first fix): heap-use-after-free in `__dbc_cache_handle_live` on both
  arms; the registry fix: clean.
- **Diagnostic tier:** `BUILD=<dir> bash ../test/db/run_all.sh` with
  `--enable-diagnostic`.
- **ASan/TSan adversarial (review #238):** A1 (adjustment+del+verify), A2
  (fork), A3 (lock-accounting+secondary), A4 (thread-exit+reopen), A6 (CDB),
  a9 (orphan), a10 (env-UAF) all pass ASan-clean; A7 (epoch race) passes under
  TSan with **zero** races on the epoch, serial or registry state.
- **TCL:** `run_method -btree test001..010` and `-hash` (cursor-reuse,
  dup/opd, secondary, and delete-adjust coverage).

## Alternatives considered

- **Lock-free active-cursor list** (replace the queue mutex with a lock-free
  list). Larger surface, touches every `__db_walk_cursors` caller and the
  adjustment correctness proof becomes a lock-free-memory-reclamation problem
  (the same blocker that parked the mpool optimistic work). The cache gets ~all
  the measured win with none of that risk.
- **Per-thread DB handles** (the `sepdb` workload that measured +49% at t=24).
  Changes the application contract (a shared handle is the documented pattern)
  and multiplies memory. The cache gives shared-handle users the per-thread-handle
  speed without the API change.
- **Caching in `DB_THREAD_INFO`.** Rejected: it is a shared-region struct holding
  `roff_t` offsets, walked cross-process by failchk; process-local pointers
  cannot live there without a region-format change and a multi-process hazard.
- **`db_get_multiple` only** (the shipped batched path). Already in tree; it helps
  only callers that batch. Most applications call single-key `DB->get`; this RFC
  serves them.

## Risks & open questions

- **Cursor-reset completeness (the sharp edge, O-D1 names it).** A cached cursor
  must be reset to a pristine state on reuse. Mitigation: reuse the *existing*
  `__db_cursor_int` refresh verbatim — the cache does not invent a new reset; it
  reuses the one a free-queue cursor already goes through. The only delta is
  skipping the queue move. Tested by test001/003/011/026 and jointest in the TCL
  run, plus the new adjust test.
- **Stale txn association.** Addressed by making txn-locker cursors ineligible
  and tearing down/rebuilding the txn link at release/acquire (§3).
- **Handle close across threads (the review #238 BLOCKER).** A close of any
  handle must not strand a live handle's cached cursor. Addressed by the
  epoch-as-change-signal + registry reconcile (§4, §4a): the closing thread
  removes the handle from the registry and clears its own slots; other threads
  reconcile on the next op, keeping live slots and dropping only closed ones.
  Tested by `run_cursorcache_orphan`.
- **Cross-thread env use-after-free (review #238 rework, rev238_a10).** A slot
  must never dereference its `DB`/`ENV` once the handle may be closed by another
  thread. Addressed by the process-global live-handle registry (§4a): liveness
  is a registry lookup that dereferences nothing. Tested by
  `run_cursorcache_envuaf` (ASan, both the thread-exit and env-reuse reconcile
  paths).
- **Thread exit (the review #238 concern 2).** Addressed by the pthread-key
  destructor that returns an exiting thread's live cached cursors to their free
  queues (§4). Tested by `run_cursorcache_parkcost`.
- **Epoch data race (review #238 concern 1).** Addressed by making the epoch and
  serial counters `db_atomic_t` (atomic_read/atomic_inc); the registry is under
  its own process mutex. Verified under TSan (§8): zero races on the epoch,
  serial or registry state.
- **Resolved — the `DB->cursor_cache_gen` field (former open question).** The
  field is present and simpler than the earlier field-free scheme; it costs no
  bytes (fits existing padding, §6) and is not a region field, so there is no
  ABI size change and no escape token.
- **Performance risk:** the eligibility test and TLS lookup add a few
  instructions to every transient op; on a cache *miss* the path is master plus
  that overhead. Measured by the A/B, including a worst-case all-miss arm.

## Prototype / evidence

Implemented on this branch. A/B and profile in
`test/bench/CURSOR-CACHE-RESULTS.md` (raw data `cursor-cache-ab.tsv`, profile
summaries `cursor-cache-prof-{base,branch}.txt`). Measured on `c6id.metal`
(128 vCPU, XFS NVMe) against the non-transactional per-key `DB->get` path
(`batch_bench indiv`, the workload behind the 85%-cursor-lifecycle profile):
6 interleaved reps, all CV ≤ 5.2%. Branch vs base: **1.08×** at t=1 (no
single-thread regression), **1.53×** at t=64, **2.81×** at t=96, with p99
262 µs → 33 µs. Base scales to t=32 then declines (997k/s at t=96, the
cursor-mutex wall); the branch keeps climbing (2.80M/s). The t=96 profile shows
the machine idle 67% (base) vs 47% (branch): base leaves two-thirds of the
cores blocked on the per-handle cursor-queue mutex, the branch recovers that
time. Functional gates (db-tier `--enable-diagnostic` 30/0 fail incl.
`handle_sizes` proving `sizeof(DB)`/`sizeof(DBC)` unchanged; the sabotage arm
failing then passing; TCL btree+hash test001-010 20/20) all green.

---

## Decision

*(Filled by the reviewer when the RFC is decided.)*

- **Decision:** Accepted | Rejected — YYYY-MM-DD
- **Rationale:** why.
- **Conditions / follow-ups:** any.
