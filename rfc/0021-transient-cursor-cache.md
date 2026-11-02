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
	DB    *dbp;		/* Owning handle (identity key). */
	DBC   *dbc;		/* The idle cached cursor, on dbp's active
				 * queue, pgno == PGNO_INVALID, no lock. */
	DBTYPE dbtype;
};
#define	DBC_CACHE_SLOTS	4	/* Small, fixed; LRU-evict on miss. */
static DB_LO_TLS struct __dbc_cache_entry __dbc_cache[DBC_CACHE_SLOTS];
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

A cached cursor is a live `DBC` on a live `DB` handle. It must be destroyed
before anything that would make that handle or thread invalid. Every cache entry
is drained (each cursor genuinely `__dbc_close`d, then removed and destroyed as
the normal path would) at:

- **DB handle close** (`__db_close` / `__db_refresh`): before the handle walks
  and destroys its cursor queues, it must drain any cache slot pointing at this
  `dbp`, across **all** threads that cached a cursor for it. Because the cache is
  thread-local, a handle cannot reach another thread's TLS directly. We solve
  this the same way the active queue already solves it: `__db_refresh` already
  walks `cq_parts[].active_queue` and destroys every cursor there under
  `CQ_LOCK`. A cached-but-idle cursor **is on that active queue**, so
  `__db_refresh`'s existing walk already finds and destroys it. The only extra
  work is clearing the stale TLS slot *lazily*: a thread that later looks up a
  freed `dbp` pointer must not use it. We make lookup safe by validating the slot
  against a per-handle **generation counter** check, below (§4a).
- **Environment close / `DB_ENV->close`:** handles close first, so handle-close
  draining covers it. A belt-and-braces full-thread cache flush is done when the
  env's thread-info is torn down (`THREAD_OUT`).
- **failchk** (`__env_failchk`): failchk operates on the **shared region**
  thread-info blocks of *dead* processes/threads; the process-local cache of a
  dead process dies with the process address space, so there is nothing for
  failchk to clean in the cache. A dead thread in a *live* process is handled by
  §4a (the handle-close/active-queue path still owns the cursor memory).
- **Thread exit:** the TLS storage is reclaimed by the platform on thread exit;
  the cursors it referenced are still on the handle's active queue and are
  destroyed at handle close. To avoid leaking those cursors for the life of the
  handle after a thread exits, a thread that is about to leave the env
  (`THREAD_OUT`) flushes its own cache (genuine close of each cached cursor).

#### 4a. Making stale-pointer lookup safe

Between a handle close and a thread's next lookup, a TLS slot can hold a dangling
`DB *`. We never dereference a slot's `dbp` to decide a hit; instead the slot
also stores the handle's **open-generation id**. `DB` already has a per-open
identity we can read without a new field: the slot caches `dbp` *and* a copy of
`dbp->cursor_cache_gen` taken when the cursor was cached. A lookup is a hit only
if `slot->dbp == dbp && slot->gen == dbp->cursor_cache_gen`. Handle close bumps a
process-wide generation epoch (not the freed handle's memory), so a reused `DB`
allocation at the same address fails the gen check. See §6 for why this needs no
`sizeof(DB)` change (the gen lives in the TLS slot and a single process-global
counter, not in `DB`).

`shortcut: the gen check uses a process-global monotonic epoch stamped into the
handle at open and copied into the slot; a slot whose stamp != the handle's
current stamp is a miss and is cleared. If a DB were reopened 2^32 times at the
identical heap address within one thread's cache lifetime the epoch could alias;
in practice a handle close drains the active queue (which owns the cursor) so the
pointer is already destroyed, making this a belt-and-braces guard, not the
primary safety mechanism.`

### 5. Why cursor adjustment stays correct

`__db_walk_cursors` (`src/db/db.c`) must still see every cursor that could be
positioned on a page a writer is changing. The adjustment callbacks
(`__bam_ca_di_func`, `bt_curadj.c:195`) match a cursor **only** when
`cp->pgno == pgno && cp->indx >= indx`. The key facts:

1. **An idle cached cursor is unpositioned.** At release, `am_close` set
   `cp->pgno = PGNO_INVALID` and dropped every page and lock. So while a cursor
   sits in the cache, no callback can ever match it (`PGNO_INVALID` is never a
   real page a writer changes). An idle cached cursor on the active queue is
   therefore inert to the walk — exactly as inert as a free-queue cursor, but
   visible.

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

### 6. No struct growth

- **`DB`:** the open-generation epoch is a **process-global** `u_int32_t` plus a
  per-open stamp. To avoid a new `DB` field (and any `sizeof(DB)` change), the
  stamp is read from an existing stable field combination. If review prefers an
  explicit field, note that `DB` is **not** in `env_sig.c` (it is a process-local
  handle, not a region struct), so adding a `u_int32_t` to `DB` would be ABI-only,
  not a region-format change — but the soname contract (Q6) still argues against
  it, so the base design keeps `sizeof(DB)` unchanged and carries the epoch in
  the TLS slot keyed by `(dbp pointer, open-count)`. **Decision for review:
  whether an explicit `DB->cursor_cache_gen` field is acceptable given the Q6
  soname bump already in flight (W6); if yes it simplifies §4a.**
- **`DBC`:** unchanged. The cache stores a `DBC *`, it does not add fields to the
  cursor.
- **`DB_THREAD_INFO`:** unchanged (it is a shared-region struct; see North-star).
- **Region signature:** unchanged (`env_sig.c` covers none of the above).

### 7. Config / kill switch

A single env flag `DB_ENV->set_flags(DB_NO_CURSOR_CACHE)` (and the matching
`DB_CONFIG` token) disables the cache entirely, falling back to the master
alloc/free path. This is the A/B and bisection lever and the safety valve if a
workload hits the eligibility edge. Default: cache **on** for `DB_THREAD` handles
in a threaded env; **off** otherwise (non-threaded handles are never cached).

### 8. Tests

- **New `test/db/run_cursorcache_adjust.sh`** (+ its C driver): many threads do
  concurrent `DB->put`/`DB->del` and `DB->get` on **shared pages** of one shared
  `DB_THREAD` handle, with the cache on, then verify the final key/value set and
  every surviving cursor's reported position against a serial oracle. The
  **sabotage arm** compiles with `-DDBC_CACHE_SABOTAGE`, which removes the cached
  cursor from the active queue while idle (breaking the §5 invariant); the test
  **must FAIL** in that arm (a missed adjustment corrupts a concurrent cursor's
  position or the data) and **PASS** with the invariant intact.
- **Diagnostic tier:** `BUILD=<dir> bash ../test/db/run_all.sh` with
  `--enable-diagnostic`.
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
- **Handle close across threads.** Addressed by keeping the cursor on the active
  queue so `__db_refresh`'s existing walk owns teardown (§4), with the gen stamp
  making a later TLS lookup safe (§4a).
- **Open question for review:** explicit `DB->cursor_cache_gen` field vs the
  process-global epoch (§6). The field is simpler; it is ABI-only (not region)
  and the Q6 soname bump may absorb it.
- **Performance risk:** the eligibility test and TLS lookup add a few
  instructions to every transient op; on a cache *miss* the path is master plus
  that overhead. Measured by the A/B, including a worst-case all-miss arm.

## Prototype / evidence

Implemented on this branch; A/B and profile in
`test/bench/CURSOR-CACHE-RESULTS.md` (filled by the P1 benchmark run: base =
origin/master vs this branch, xrel_kv btree/hash C and I at t=1,4,16,32,64 with a
shared handle, ≥5 interleaved reps, median + CV, and a t=32 C perf profile on
both arms).

---

## Decision

*(Filled by the reviewer when the RFC is decided.)*

- **Decision:** Accepted | Rejected — YYYY-MM-DD
- **Rationale:** why.
- **Conditions / follow-ups:** any.
