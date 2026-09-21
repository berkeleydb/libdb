# RFC 0010: Global invariants across subsystems

- **Status:** Accepted
- **Type:** Normative
- **Author:** libdb maintainers
- **Date:** 2026-09-21
- **Tracking:** cited from `src/mutex/mut_order.c` (A3) and `src/dbinc/lock_order.h`; gap register G1-G15

> **Normative.** This RFC is the specification the DIAGNOSTIC lock-order checker
> encodes, and its A-numbers and G-numbers are cited from shipped source and from
> `test/KNOWN-ISSUES.md`. Changing an A- or G-number here changes those citations.

---

- **Status:** Normative (describes current `master`)
- **Scope:** the invariants that hold *across* subsystems, not within one
- **Anchored at:** `53a8fc45a`
- **Related:** [`rfc/0003`](../../rfc/0003-ssi-serializable-snapshot-isolation.md)
  (SSI), [`rfc/0003/M2`](../../rfc/0003/M2-partition-design.md) (SIREAD GC vs.
  partitioned lock regions), [`rfc/0003/M4`](../../rfc/0003/M4-commit-lifecycle.md)
  (SIREAD lifetime across commit), [`rfc/0002`](../../rfc/0002-buffer-swip-aio.md)
  (async I/O, optimistic descent)

---

## Why this note exists

Berkeley DB's durability argument used to be small enough that one person could
hold it end to end: log before data, checkpoint bounds recovery, recovery redoes
committed and undoes the rest. This fork has added machinery that reaches
*through* that argument rather than sitting beside it — SSI markers threaded
through the lock manager, the mpool version chain and the commit path; async
writeback across five backends; checkpoint-driven MVCC purge; a lock-free
root-snapshot read path; wired frames; cursor-queue sharding. Every one of those
is individually tested. The composition is not, because no document states what
the composition is supposed to preserve.

This note is that statement. It is organised by the three moments where
cross-subsystem state must be mutually consistent — **at a checkpoint**, **at a
crash / during recovery**, **at a region re-attach** — followed by the pairs that
must not be composed naively, and an honest map of what each invariant is
actually tested by.

Every invariant cites the code that establishes or enforces it. Where a claim
could not be settled from code it is filed as an **OPEN QUESTION** rather than
asserted; those are the note's most useful output, together with the coverage
gaps in §6.

Convention: `file:function` is the anchor; line numbers are given only where the
site is a specific statement and are correct as of `53a8fc45a`.

---

## 1. The subsystems in play, and what state each owns

**Lock manager** (`src/lock/`) owns the lock region: the object hash table
(`obj_tab`, bucket → partition via `LOCK_PART`), per-object `holders` /
`waiters` / **`sireaders`** lists, the locker hash table (striped shared
latches, `dbinc/lock.h` `LOCK_LOCKERS` / `RDLOCK_LOCKER`), and the deadlock
detector's `dd_objs`. SSI adds three pieces of state here: a **SIREAD marker**
is an ordinary `struct __db_lock` with `mode == DB_LOCK_SIREAD` parked on an
object's `sireaders` list (`lock.c:__lock_get_internal`, the `GRANT` arm at
`lock.c:1347-1361`); `DB_LOCKREGION.nsireaders` (`dbinc/lock.h:115`) is an
*approximate* atomic count of live markers used only to trigger GC; and
`DB_LOCKER_FREED`
(`dbinc/lock.h:196`) marks a locker whose reclamation was deferred because
markers still name it. Markers reach into the txn region through
`LOCKER_TD` / `LOCK_OWNER` (`dbinc/lock.h:276-287`) — that pointer is why
marker lifetime and detail lifetime are one problem, not two.

**Txn region** (`src/txn/`) owns `DB_TXNREGION`: `active_txn`, `mvcc_txn`,
`last_ckp` / `time_ckp`, `mtx_ckp`, id allocation, and the `TXN_DETAIL` array.
A `TXN_DETAIL` (`dbinc/txn.h`) carries the per-txn MVCC and SSI state:
`read_lsn` (snapshot point), `visible_lsn` (serialization point once
committed), `mvcc_mtx` + `mvcc_ref` (buffer versions this txn created that are
still in cache), `si_ref` (atomic count of SIREAD markers naming this detail,
`dbinc/txn.h:96-101`),
`status`, and the flags `TXN_DTL_SNAPSHOT` (parked on `mvcc_txn` for deferred
reclaim), `TXN_DTL_WCONF` / `TXN_DTL_RCONF` (the SSI pivot flags) and
`TXN_DTL_SICHECKED` (published atomically with the one pivot check). Two
subsystems can hold the last reference to a detail — mpool via `mvcc_ref`, the
lock manager via `si_ref` — which is the single most delicate lifetime rule in
the fork.

**Mpool** (`src/mp/`) owns the cache regions: per-bucket `DB_MPOOL_HASH`
(`mtx_hash`, `hash_bucket`, and the cached oldest-reader frontier
`old_reader`, `dbinc/mp.h:359`), buffer headers `BH` with `ref` / `mtx_buf` /
flags / `td_off` (the creating detail) / MVCC version chain links (`vc`), the
`wired` byte (`dbinc/mp.h:606-615`, non-evictable frames) and the region-level
`wired_pages` atomic (`dbinc/mp.h:204-211`) with its `MPOOL_WIRED_MAX_PCT` = 25%
cap (`dbinc/mp.h:434-440`). Visibility is decided entirely by
`BH_VISIBLE` / `BH_OBSOLETE` (`dbinc/mp.h:686-694`) against a reader's
`read_lsn` and a bucket's `old_reader`.

**Log / WAL** (`src/log/`) owns the log region: `lsn` (next write point),
`s_lsn` (last-synced), `f_lsn`, buffer offsets, `mtx_flush`.
`log_put.c:__log_flush_int` is the only place `s_lsn` advances, and it advances
it only after `__os_fsync` succeeds (`log_put.c:1170-1198`).

**Checkpoint** (`src/txn/txn_chkpt.c:__txn_checkpoint`) owns no state of its own;
it *sequences* other subsystems: SIREAD GC → `mtx_ckp` → compute `ckp_lsn` from
the oldest active txn → flush the cache → purge obsolete MVCC versions → log the
checkpoint record → publish `last_ckp`.

**failchk** (`src/env/env_failchk.c:__env_failchk_int`) is the multi-process
recovery-without-restart path: thread-table scan (`__env_in_api`), then
`__lock_failchk`, `__txn_failchk`, `__dbreg_failchk`, `__memp_failchk`, then
`__mut_failchk`.

**Recovery** (`src/env/env_recover.c:__db_apprec`) is the four-pass
redo/undo driven from `last_ckp`'s `ckp_lsn` (passes documented at
`env_recover.c:143-196`). It runs against **freshly created regions**:
`env_open.c:199-207` removes and recreates the environment before attaching
when `DB_RECOVER` is set.

**Btree root snapshot (rsnap)** (`src/btree/bt_search.c`) is *per-handle,
process-local* state: `bt_rootpage` (a cached pointer to the wired live root
frame), `bt_rsnap` (a private page-sized copy plus its LSN), `bt_rsnap_lsn`,
`bt_rsnap_free` (retired copies). None of it is shared, none of it is logged.

**Cursor-queue sharding** (`src/dbinc/db_am.h`, `src/db/db_am.c`) splits each
handle's free/active cursor queues into `DB_CURSOR_NPART` = 8 partitions, each
with its own `DB_MUTEX_PROCESS_ONLY` mutex (`db.c:508-516`), chosen by a hash of
the *thread* id (`db_am.c:__db_cursor_part`) and recorded per cursor in
`dbc->part` (`db_am.c:160`).

---

## 2. Invariants AT A CHECKPOINT

`__txn_checkpoint` is the fork's busiest cross-subsystem seam: it now drives
SIREAD GC and MVCC purge in addition to the cache flush and the ckp record.

### C1 — WAL: log before data, per page

**No dirty page reaches the file before the log records describing it are
fsync'd.** Enforced in one place for both the synchronous and the async
writeback path: `mp_bh.c:__memp_pgwrite_prep` reads the page's own LSN out of
the frame and calls `__log_flush(env, &lsn)` (`mp_bh.c:352`) before it reports
that there is an image to write. Both writers go through it — the sync path at
`mp_bh.c:498` (`__memp_pgwrite`) and the async path at `mp_bh.c:645`
(`__memp_bhwrite_async`) — so adding async writeback did not add a second WAL
site to keep in sync. `__log_flush` → `__log_flush_int` advances `s_lsn` only
after `__os_fsync` returns 0 (`log_put.c:1170-1198`), so "flushed" means
durable, not buffered. A `DIAGNOSTIC` build additionally asserts
`s_lsn > LSN(bhp->buf)` under `mtx_flush` after the flush
(`mp_bh.c:356-372`).

### C2 — The durable frontier only advances over writes that completed

**`ckp_lsn` may be logged, and `region->last_ckp` published, only if every
page write the checkpoint issued succeeded and the required fsync succeeded.**
The sequence is: `__memp_sync_int` returns non-zero → `__txn_checkpoint` takes
the `err:` exit at `txn_chkpt.c:277-283` → `__txn_ckp_log` (`txn_chkpt.c:371`)
is never reached → `__txn_updateckp` (`txn_chkpt.c:381`) never runs → recovery
still starts from the *previous* checkpoint. `__txn_updateckp` additionally
refuses to move `last_ckp` backwards (`txn_chkpt.c:483-484`).

For the async path this is exactly what the os_aio drain error propagation
protects. A completion callback only records its result
(`mp_bh.c:__memp_aio_writeback_done`); `mp_bh.c:__memp_aio_drain` reaps all
outstanding writes, runs each finish, and stores the **first non-zero** error
through `errp` (`mp_bh.c:709-712`). `__memp_sync_int` passes `&ret` at all three
drain sites (`mp_sync.c:613`, `:627`, `:656`), and the final drain runs
*before* the `required_write` fsync (`mp_sync.c:648-668`), so a failed async
write both fails the checkpoint and suppresses the fsync that would otherwise
imply durability. A drain that swallowed the error would produce a checkpoint
record that claims a frontier the data never reached — the "fast liar" failure
mode. The failed page is deliberately left `BH_DIRTY` by
`__memp_pgwrite_finish`, so it is not lost from cache and a later sync retries
it.

### C3 — `ckp_lsn` is a point before which no transaction can still need undo

`__txn_getactive` (`txn_chkpt.c:406`) walks `active_txn` under
`TXN_SYSTEM_LOCK` and lowers `ckp_lsn` to the minimum `begin_lsn` of any active
transaction, having started from the current log LSN
(`txn_chkpt.c:231`, `:165`). Checkpoints are single-threaded on
`region->mtx_ckp` (`txn_chkpt.c:159`) precisely so two concurrent checkpoints
cannot publish out-of-order frontiers and let archiving remove a log a
checkpoint depends on.

### C4 — SIREAD GC at checkpoint cannot drop a marker a live edge still needs

`__lock_sicleanup` (`lock.c:209`) runs from the checkpoint path
(`txn_chkpt.c:157`) and from a pressure trigger in `txn_begin`
(`txn.c:326-328`, threshold `st_objects / SI_CLEANUP_TRIGGER_DIV`, `txn.c:71`).
The reclamation predicate is in `__lock_siclean_obj` (`lock.c:106`) and has
exactly two conditions, both necessary:

1. the marker's owning transaction is **not** `TXN_RUNNING` (`lock.c:123-125`) —
   a running reader's marker is always kept; and
2. the marker's snapshot point is **at or behind** the oldest active reader:
   `visible_lsn` for a committed reader that wrote, falling back to `read_lsn`
   for a read-only committed reader whose `visible_lsn` stayed `MAX_LSN`
   (`lock.c:127-152`).

The frontier is `__txn_oldest_reader` (`txn_region.c:419`), computed **once, up
front, before any partition mutex is taken** (`lock.c:225-226`) because it takes
`TXN_SYSTEM_LOCK`. Since a new reader's `read_lsn` is only ever assigned at or
after the current log LSN (`mp_fget.c:259-265`, `txn.c:509` initialises it to
`MAX_LSN`), a marker that fails test 2 can never become relevant to a *future*
reader: any transaction that starts later has a newer snapshot, and any
still-active transaction sharing that snapshot carries its own marker. Hence
dropping it cannot lose a needed edge.

Reclaim ordering across the three domains is fixed and is the reason this is
safe rather than merely plausible: markers are removed under the object
partition mutex; lockers are freed afterwards under `LOCK_LOCKERS`
(`__lock_sireap_lockers`, `lock_id.c:565`, called at `lock.c:250`); details are
freed last under `TXN_SYSTEM_LOCK` (`__txn_reap_si_details`,
`txn_region.c:466`, called at `lock.c:259`). Never the reverse, and never
nested.

### C5 — MVCC purge at checkpoint cannot free a version any reader can still see

`__memp_purge_obsolete` (`mp_alloc.c:787`) is called from the checkpoint path
after the cache flush (`txn_chkpt.c:298`). It exists because eviction only
revisits a bucket under allocation pressure, so with a large cache an obsolete
version — and the committed detail plus `mvcc_mtx` slot it pins — could be
retained without bound (#138).

Its safety is structural, not additional: it computes the frontier once with
`__txn_oldest_reader` (`mp_alloc.c:816`), advances each bucket's cached
`hp->old_reader` **toward but never past** it under `mtx_hash`
(`mp_alloc.c:835-836`) — the same guarded advance `__memp_alloc` performs
(`mp_alloc.c:412-423`) — and frees only a buffer that is the **oldest version
of a non-singleton chain**, unreferenced, not `BH_DIRTY|BH_FROZEN|BH_EXCLUSIVE`,
and `BH_OBSOLETE` (`mp_alloc.c:845-865`). `BH_OBSOLETE`
(`dbinc/mp.h:692`) is defined as "the *next newer* version is already visible
at `old_reader`", which is exactly "no live reader can ever read this version
again". The claim is then re-verified under both `mtx_buf` and `mtx_hash` after
the latch dance (`mp_alloc.c:899-909`) before `__memp_bhfree` runs, and any
busy buffer is skipped rather than waited on (`MUTEX_TRYLOCK`,
`mp_alloc.c:882`). The actual free goes through the pre-existing
`__memp_bhfree` → `__txn_remove_buffer` path (`mp_bh.c:862-866`), so no new
reclamation protocol and no new lock order were introduced.

### C6 — Detail reclamation has exactly one claimant

A committed snapshot transaction's `TXN_DETAIL` can be the last reference of
either mpool (`mvcc_ref`) or the lock manager (`si_ref`). Both reclaimers test
the same predicate, so `TXN_DTL_SNAPSHOT` is used as a **single-claim flag
taken under `td->mvcc_mtx`**: `__txn_remove_buffer` clears it while still
holding `mvcc_mtx` (`txn_region.c:584-588`) and `__txn_reap_si_details` does
the same (`txn_region.c:494-500`), so exactly one of them frees the detail.
Both free `mvcc_mtx` before `__env_alloc_free` (`txn_region.c:517`, `:597`) — a
leaked mutex slot is a region-exhaustion bug, not merely untidy. `__txn_end`
establishes the parking invariant: a detail with `mvcc_ref != 0` or
`si_ref != 0` goes onto `region->mvcc_txn` flagged `TXN_DTL_SNAPSHOT` and is
*not* freed (`txn.c:1890-1937`).

### C7 — Ordering within `__txn_end`

`__txn_end` frees the **locker before the detail** (`txn.c:1854-1866`), because
`__lock_freelocker_int` reads `td->si_ref` through the locker's `td_off`
(`lock_id.c:516-520`) to decide whether to defer. Reversing them dereferences a
freed detail on the OOM/abort cleanup path. The locker lives in its own lock
domain (`LOCK_LOCKERS`), so doing it there introduces no ordering constraint.

---

## 3. Invariants AT A CRASH / DURING RECOVERY

### R1 — What recovery restores

After `__db_apprec` the state satisfies:

- **every committed transaction is durable** — pass 3 rolls forward from the
  checkpoint's `ckp_lsn` (`env_recover.c:251`, `:585`), and C1 guarantees the
  log records exist for every page modification;
- **every unresolved transaction leaves no trace** — pass 2 undoes backwards to
  the checkpoint (`env_recover.c:165-182`);
- **no page exists whose modification lacks a log record** — the WAL rule of C1,
  applied per page rather than per transaction, is what makes redo total.

### R2 — The #136 commit-window property survives a crash

**A transaction that should have aborted as an SSI pivot is not durable.** This
is not a recovery-time property — recovery knows nothing about SSI — it is a
property of the commit path, which is why it holds across a crash: the pivot
check happens *before* the commit log record is written, so a transaction that
fails it never gets a `regop` record to redo.

`__txn_commit` (`txn.c:817-830`) performs its **one and only** pivot check under
`TXN_SYSTEM_LOCK`: if `TXN_DTL_WCONF && TXN_DTL_RCONF` it returns
`DB_SNAPSHOT_CONFLICT` and falls to `err:` → `__txn_abort`; otherwise it
publishes `TXN_DTL_SICHECKED` **in the same critical section**. That publication
is what closes the window: a writer arriving after the check but before
`__txn_end`'s status store used to see `TXN_RUNNING` and defer to a check that
had already happened, and both transactions committed a write skew (#136).
`TXN_SI_PAST_CHECK` (`dbinc/txn.h:133-160`) is the shared predicate — committed,
*or* running-and-`SICHECKED` — and both edge-forming sites consult it under the
same mutex: the lock manager at `lock.c:1119-1148` and mpool at
`mp_fget.c:129-174`. An aborted transaction is deliberately *not* "past check":
its reads never committed, so an edge into it is not a conflict at all — hence
the status test rather than testing the flag alone.

Consequences worth stating explicitly, because they are what makes the property
crash-safe:

- the log contains **no SSI state at all** — no `TXN_DTL_*`, no `si_ref`, no
  SIREAD marker appears in any `.src` log record (verified: no match in
  `src/*/*.src`). SSI is a pre-commit admission decision, and its entire
  footprint is region-only;
- therefore recovery cannot "re-decide" an SSI abort, and does not need to: an
  aborted pivot has no commit record, and a committed transaction was, by
  construction, not a pivot at its check.
- `__txn_prepare` refuses `TXN_SNAPSHOT_SAFE` outright (`txn.c:1449-1455`),
  because a prepared transaction must be guaranteed committable while an SSI
  transaction can still acquire a second edge after prepare. This keeps R2 from
  colliding with the 2PC "prepared implies committable" rule.

### R3 — Which state is region-only and which is logged

This distinction *is* the recovery argument: every piece of region-only state
must be either rebuilt from the log or re-initialised, and none of it may be
required to interpret the log.

| State | Home | Logged? | How it comes back |
|---|---|---|---|
| Page contents, page LSNs | data files | yes (per-page log records) | redo/undo, `db_rec.c` handlers |
| `regop` / `ckp` / `child` / `prepare` records | log | yes | the log *is* the record |
| `region->last_ckp`, `time_ckp` | txn region | recorded **in** the ckp record | `txn_region.c:__txn_init` re-derives it via `__log_get_cached_ckp_lsn` / `__txn_findlastckp` (`txn_region.c:87-104`) |
| txn id space (`last_txnid`, `cur_maxid`) | txn region | `__txn_recycle` records | `__txn_reset` / `__txn_recycle_id` at the end of recovery (`env_recover.c:598-606`) |
| `TXN_DETAIL` (`read_lsn`, `visible_lsn`, `mvcc_ref`, `si_ref`, `status`, all `TXN_DTL_*`) | txn region | **no** | region is recreated; details are allocated fresh in `__txn_begin_int` with `read_lsn`/`visible_lsn` = `MAX_LSN`, `mvcc_ref` = 0, `atomic_init(&td->si_ref, 0)` (`txn.c:509-532`). Prepared txns are the one exception: they are *restored* from the log as `TXN_DTL_RESTORED` details, but their SSI/MVCC fields are still fresh |
| SSI pivot flags `TXN_DTL_WCONF` / `RCONF` / `SICHECKED` | txn region | **no** | never persist; see R2 |
| SIREAD markers, `sireaders` lists, `nsireaders`, `DB_LOCKER_FREED` | lock region | **no** | lock region recreated; `atomic_init(&region->nsireaders, 0)` (`lock_region.c:196`) |
| Locks, lockers, deadlock-detector state | lock region | write locks only, and only inside a `regop` record for replication apply | recreated empty |
| `BH` frames, MVCC version chains, `BH_DIRTY`/`BH_FROZEN`, `td_off` | cache region | **no** | cache recreated empty; dirty pages are recovered from the log, not from the cache |
| `hp->old_reader` | cache region | **no** | `ZERO_LSN(hp->old_reader)` in `mp_region.c:323` |
| `bhp->wired`, `mp->wired_pages` | cache region | **no** | zeroed at header (re)init (`mp_fget.c:743`, `:892`, `:1097`) and `atomic_init(&mp->wired_pages, 0)` (`mp_region.c:327`) |
| rsnap (`bt_rootpage`, `bt_rsnap*`) | process heap, per handle | **no** | per-handle; rebuilt on first read descent, freed at handle close (`bt_method.c:116-129`) |
| Cursor queues / `cq_parts[]` mutexes | process heap, per handle | **no** | per handle; `DB_MUTEX_PROCESS_ONLY` |
| `TXN_IN_RECOVERY` | txn region | no | set at `env_recover.c:102`, cleared at `:636` |

The load-bearing consequence: **recovery never reads region-only state**. It is
handed regions that were destroyed and recreated (`env_open.c:199-207`), so
"rebuild" is mostly "there is nothing to rebuild". That is what keeps the SSI
and MVCC additions out of the recovery argument entirely.

### R4 — Recovery is idempotent and restartable

Recovery may itself crash. Because every recovery action is derived from the log
and the checkpoint, and `last_ckp` only advances on a successful checkpoint (C2),
re-running recovery from the same log yields the same state.

---

## 4. Invariants AT REGION RE-ATTACH / MULTI-PROCESS

### A1 — The region compatibility gate

A process may only attach to a region stamped with its own compatibility
identity. `env_region.c:253-269` rejects on three checks, in order:

1. `renv->majver != DB_VERSION_MAJOR || renv->minver != DB_VERSION_MINOR` →
   `DB_VERSION_MISMATCH` (or `EINVAL` for an all-zero region, treated as
   corruption);
2. `renv->signature != __env_struct_sig()` → `DB_VERSION_MISMATCH`;
3. `renv->magic != DB_REGION_MAGIC` (`db.in:2278`) → retry as uninitialised.

Check 2 is the one that matters for this fork: `__env_struct_sig`
(`env/env_sig.c`) hashes `sizeof` of every shared structure, explicitly
including `__db_lockregion` (`env_sig.c:70`), `__db_locker`, `__db_lock`,
`__bh`, `__db_txnregion` (`env_sig.c:91`), `__db_mpool_hash`
(`env_sig.c:174`) and `__txn_detail` (`env_sig.c:194`). So **any layout change
to the SSI or MVCC
region state automatically prevents a mismatched binary from attaching** — that
is the mechanical guarantee behind adding `si_ref` to `TXN_DETAIL` or
`nsireaders` to `DB_LOCKREGION`.

The compat triplet (`DB_VERSION_MAJOR/MINOR/PATCH` = 2026.0.9 -- see the erratum
in `VERSIONING.md`; it was documented as 5.3.37 but the shipped value has been
2026.0.9 since v2026.04) is frozen for
exactly this reason plus three others: the soname `libdb-5.3.so`
(`dist/Makefile.in:60`, `LIBVERSION = MAJOR.MINOR`), `DB_VERSION_UNIQUE_NAME`
symbol mangling, and downstream packaging. See `VERSIONING.md` §"Compatibility
level" and the header comment in `dist/RELEASE`. Release identity is `DB_CALVER`;
bumping the triplet is a format/ABI break and goes through the
`ABI-BREAK-INTENDED` process, never a release.

### A2 — What failchk can and cannot reclaim

`__mut_failchk` (`mutex/mut_failchk.c`) reclaims **only** mutexes that are both
`DB_MUTEX_ALLOCATED` and `DB_MUTEX_PROCESS_ONLY`, and only when
`is_alive()` says the owning process is gone (`mut_failchk.c:44-59`). It returns
immediately for `ENV_PRIVATE` (`mut_failchk.c:32-33`). Everything else — every
*region* mutex — is outside its reach.

So the contract is: **a dead process holding a region mutex is unrecoverable
without full recovery.** The mechanism is `mut_pthread.c:247-273`: under
`DB_ENV_FAILCHK`, a thread that finds a mutex busy and its holder dead returns
`DB_RUNRECOVERY` if it is the designated failchk thread
(`ip->dbth_state == THREAD_FAILCHK`), and otherwise blocks and waits for
someone else to do the cleanup. `__env_in_api` likewise turns "a thread died
*inside* the library" into `DB_RUNRECOVERY` via `__db_failed`
(`env_failchk.c:353-355`, `common/db_err.c:1109-1124`), while a thread that died
merely *blocked* is downgraded to `THREAD_BLOCKED_DEAD` and its pinned buffers
are released with `__memp_unpin_buffers` (`env_failchk.c:344-347`, `:361-365`).

What each subsystem's failchk can do:

- `__lock_failchk` (`lock/lock_failchk.c`) releases a dead locker's **read**
  locks; a dead **non-transactional** locker holding write locks is
  `DB_RUNRECOVERY` (`lock_failchk.c:72-77`), because a DB operation may have
  been interrupted with 1-of-N pages modified. Write locks of dead
  *transactional* lockers are released by aborting the transaction.
- `__txn_failchk` (`txn/txn_failchk.c`) aborts dead-owner active transactions,
  skipping `TXN_PREPARED` ones (whose fate belongs to the coordinator) and
  failing hard on `TXN_DTL_INMEMORY`.
- `__memp_failchk` (`mp/mp_backup.c:318`) only cleans up in-progress hot
  backups; it does **not** reclaim wired frames or purge MVCC versions.

Two fork-specific notes that follow from A2:

- The **cursor-queue partition mutexes are `DB_MUTEX_PROCESS_ONLY`**
  (`db.c:508-516`), the same class as `dbp->mutex`, so sharding multiplied the
  per-threaded-handle mutex count by 8 but did not move any state into the
  region or change what failchk can reclaim.
- **Wired frames are not failchk's business.** `bhp->wired` is process-agnostic
  region state with no owner, so a process dying while a frame is wired leaves
  it wired. That is *safe* (the frame stays resident and readable; the wiring
  cap bounds the damage to 25% of a region) but it is not reclaimed until the
  page is freed (`db_meta.c:320`), the buffer is discarded
  (`mp_bh.c:823-826`) or the region is recreated. See gap **G4**.

### A3 — The global lock order

Every subsystem must respect one partial order. Stated as a single order so a
new cross-subsystem path can be checked against it:

```
                    env region (mtx_regenv / infop->mtx_alloc)
                                    |
   ------------------------------------------------------------------
   |                  |                     |                       |
handle mutex     LOCK_LOCKERS        mpool bucket mtx_hash      log region
(dbp->mutex,          |                     |                    |
 cq_parts[i])   object partition       buffer mtx_buf         mtx_flush
                      |                     |
                      +----------> TXN_SYSTEM_LOCK <-----------+
                                            |
                                        td->mvcc_mtx
                                            |
                                    MUTEX_SYSTEM_LOCK
                                    (mutex region, allocation)
```

The edges that are actually load-bearing, each with its citation:

| Order | Why / where |
|---|---|
| `LOCK_LOCKERS` → object partition | `__lock_sicommit` holds `LOCK_LOCKERS` across the `heldby` walk (`lock.c:308-337`) and nests `OBJECT_LOCK_NDX` inside for the `sireaders` op (`lock.c:321-334`); the deadlock detector uses the same direction. Never the reverse — that is why `__lock_siclean_obj` only *marks* a locker (clears `td_off`) and leaves the free to `__lock_sireap_lockers` (`lock.c:165-187`). |
| object partition → `TXN_SYSTEM_LOCK` | `__lock_get_internal` takes `TXN_SYSTEM_LOCK` tightly around the pivot-flag read-modify-write while holding the partition mutex (`lock.c:1044-1057`, `:1118-1148`). Held across no list change and no `goto`. |
| `mtx_hash` → `TXN_SYSTEM_LOCK` | `__memp_si_rwconflict` is called with `hp->mtx_hash` held and takes `TXN_SYSTEM_LOCK` for the same flag ops (`mp_fget.c:109-111`, `:130-131`, call site `mp_fget.c:368-370`). Consistent with the row above: the txn region is *below* both the lock partition and the mpool bucket. |
| `TXN_SYSTEM_LOCK` → `td->mvcc_mtx` | `__txn_end` (`txn.c:1869` then `:1891`) and `__txn_reap_si_details` (`txn_region.c:480`, `:495`). `__txn_remove_buffer` takes `mvcc_mtx` *alone* and only takes `TXN_SYSTEM_LOCK` after dropping `hash_mtx` (`txn_region.c:569-601`) — which is why the free claim must be taken under `mvcc_mtx`, the only latch common to both. |
| GC frontier before any partition mutex | `__lock_sicleanup` computes `__txn_oldest_reader` first (`lock.c:225-226`, before the partition loop at `:228`), and `__memp_purge_obsolete` does the same (`mp_alloc.c:816`), because that helper takes `TXN_SYSTEM_LOCK` and the reverse nesting would invert the row above. |
| `mtx_hash` → `mtx_buf` | Universal in mpool: acquire the bucket, take a `ref`, drop the bucket, then latch the buffer (`mp_fget.c:387-397`, `mp_alloc.c:510-520`, `mp_alloc.c:879-891`). |
| lock region → txn region, unnested | The deadlock detector deliberately *drops* the lock region before taking `TXN_SYSTEM_LOCK` for `cur_maxid`, accepting a stale answer rather than nesting (`lock_deadlock.c:190-206`). |
| anything → mutex region | `MUTEX_SYSTEM_LOCK` is taken innermost by `__mutex_alloc` / `__mutex_free` (`mut_alloc.c:92`, `:251`) and by `__mut_failchk`. Every `__mutex_free` in a reclaim path (e.g. `txn_region.c:508`, `:600`) is therefore legal wherever it stands. |

Two rules that are not orderings but are equally load-bearing:
`LOCK_SYSTEM_LOCK` is a **no-op when the lock table is partitioned**
(`dbinc/lock.h:321-327`) — it must never be relied on for mutual exclusion of
locker or object lists (this was the original SSI marker use-after-free, see
the comment at `lock.c:294-307`); and `LOCK_LOCKERS` acquires all stripes in a
fixed order, stripe 0 first (`dbinc/lock.h:381-390`), so the striping itself
cannot deadlock.

#### Corrections to the diagram above, from building the mechanical checker

The table and diagram were written by reading the code. Turning them into an
executable model (`src/mutex/mut_order.c`, gap **G9**) falsified five things.
They are recorded here rather than silently fixed above, because each one is a
trap for the next reader.

1. **The lock, txn and log "regions" are ONE latch, not three.** All three
   alias the env region latch, explicitly and by design:
   `region->mtx_region = renv->mtx_regenv` at `lock_region.c:179`,
   `txn_region.c:118` and `log.c:224`, each with the comment "We share the
   region so we need the same mutex"; `mtx_regenv` is allocated once as
   `MTX_ENV_REGION` (`env_region.c:713`). Measured on a live environment,
   `LOCK_SYSTEM_LOCK`, `TXN_SYSTEM_LOCK` and `LOG_SYSTEM_LOCK` are all mutex
   index 2. Only mpool's region latch is genuinely separate.
   Consequently the diagram's edges *between* those three nodes are edges from
   a node to itself, and **a rank per `MTX_*` id cannot express this order** —
   node identity has to be the mutex index. The env region latch also cannot be
   ranked at all, because it appears at three different depths: as
   `infop->mtx_alloc` it is an innermost allocator latch (`env_open.c:1122`,
   asserted at `env_alloc.c:219`), as `LOCK_SYSTEM_LOCK` it is outermost, and as
   `TXN_SYSTEM_LOCK` it sits in the middle (taken while `mtx_filelist` is held,
   `dbreg.c:283` → `log_put.c:174`).
2. **The `object partition → TXN_SYSTEM_LOCK` edge is unsatisfiable as drawn.**
   Because of (1) it is the same latch on both ends. With `lk_partitions > 1`
   `LOCK_SYSTEM_LOCK` is a no-op so the nesting never happens; with
   `lk_partitions == 1` it self-deadlocked at `lock.c:1119`. That was the
   previously-unexplained "`lk_partitions=1` hangs" report.

   **This is FIXED as of v2026.09.6.** The SSI `TXN_SYSTEM_LOCK` acquisition in
   `__lock_get_internal` is now guarded on `part_t_size == 1` (the `si_txn_lock`
   local), matching three neighbouring sites that already used that guard, so the
   latch is never taken twice. The #136 commit-window property is preserved by
   interval containment: the caller's existing hold `[lock.c:801, 803]` strictly
   contains the window `[1119, 1148]` that was removed, so the state published
   under the latch is unchanged. Worth recording *why* this was user-reachable
   rather than a corner case: `lock_method.c:42` sets
   `lk_partitions = ncpu > 1 ? 10 * ncpu : 1`, so **1 partition is the default on
   any single-CPU machine**. `test/c/lock_order_check.c` is now a regression gate
   on the fix — it requires a clean completion at `lk_partitions=1` and treats a
   checker report there as a failure, meaning the nesting returned.

   Still open, and *not* addressed by that fix: a second, independent
   `lk_partitions=1` failure in multi-process locker teardown (`ssi009` /
   `BDB2047`), tracked as **S5**.
3. **`mtx_buf` is a page pin, not an ordered latch.** `__memp_fget` *returns*
   holding it — that is what "pinned" means — and the caller then acquires
   record locks (`lock.c:911`) and other buckets while holding it. A3's mpool
   rule ("take the bucket, take a ref, **drop the bucket**, then latch the
   buffer") is really a rule about `mtx_hash`; `mtx_buf`'s deadlock freedom
   comes from the ref/pin protocol, not from a global order.
4. **The mpool region latch is an allocator latch**, in the same innermost tier
   as `MUTEX_SYSTEM_LOCK`, not a peer of the env region. It *is*
   `infop->mtx_alloc` (`mp_region.c:160`, `:361`) and every caller takes it
   innermost and drops it at once, including while holding `mtx_hash`
   (`mp_alloc.c:725`) and a file bucket (`mp_method.c:800`). A3 says "anything →
   mutex region" but omits this latch.
5. **"handle mutex" at the top of the diagram is right for `dbp->mutex` and
   wrong for the process-local list latches.** `mfp->mutex` is a leaf counter
   latch despite its name (take, bump `block_cnt`, release — `mp_fget.c:962`),
   taken *inside* `mtx_buf`. And the `DB_MUTEX_PROCESS_ONLY` latches are a
   separate domain that resists ranking in both directions at once: some are
   leaves taken under a region latch (`mp_sync.c:60` → `:837`,
   `mp_fopen.c:350` → `os_handle.c:43`), while `env->mtx_dblist` is an outer
   latch held across work that then takes region latches (`db.c:964` →
   `mp_fopen.c:1009`, `db.c:1381` → `mp_mvcc.c:92`). Being process-local they
   cannot cause the multi-process hang this order exists to prevent.

---

## 5. The dangerous interactions

| # | Pair | Naive composition fails because | Rule that makes it safe |
|---|---|---|---|
| **D1** | SIREAD marker GC × live-txn edge formation | A sweep that dropped a committed reader's marker while some active transaction's snapshot is still at-or-before that reader's serialization point erases an edge that a *future* writer would have formed → a write skew commits. | Two-part predicate in `__lock_siclean_obj` (`lock.c:123-152`): not `TXN_RUNNING`, **and** snapshot point at/behind `__txn_oldest_reader`. Frontier computed once before any partition mutex (`lock.c:225`). |
| **D2** | Marker/locker/detail reclamation × mpool version reclamation | Two subsystems each hold a last reference (`si_ref`, `mvcc_ref`) and both test the same free predicate → double free, or (with a mistaken `td_off`) use-after-free. | `TXN_DTL_SNAPSHOT` is a single claim taken under `td->mvcc_mtx` by both claimants (`txn_region.c:494-500`, `:584-588`); `__lock_siclean_obj` clears `td_off` as its *last* dereference of the detail and lets `__lock_sireap_lockers` finish outside the partition mutex (`lock.c:165-187`); `__txn_end` frees locker before detail (`txn.c:1854-1866`). |
| **D3** | MVCC obsolete purge × snapshot visibility | A purge driven on a schedule (checkpoint) rather than by allocation pressure could free a version an open snapshot still reads → wrong answer, worse than the leak it fixes. | `__memp_purge_obsolete` reuses `BH_OBSOLETE` against a frontier it only ever advances *toward* (`mp_alloc.c:816`, `:835-836`), re-verifies under both latches (`mp_alloc.c:899-909`), and skips any busy buffer. Gated by `mvcc_purge_visible` / `mvcc_purge_stress`. |
| **D4** | Checkpoint durable frontier × async writeback completion | An async write whose error is dropped makes the checkpoint look successful, so `ckp_lsn` advances past data that never landed → committed data lost after a crash, with no error anywhere. | `__memp_aio_drain` propagates the first error through `errp` (`mp_bh.c:709-712`); all three call sites pass `&ret` (`mp_sync.c:613`, `:627`, `:656`); the final drain precedes the fsync (`mp_sync.c:648-668`); non-zero `ret` skips `__txn_ckp_log`. WAL itself is unaffected because both paths share `__memp_pgwrite_prep`. |
| **D5** | rsnap cached root frame × page free / unwire | Caching a frame pointer for lock-free LSN reads dangles if the frame can be evicted, freed or reused; a stale child pgno can point at a reused non-btree page. | Cache the pointer **only if wiring took**: `__memp_wire` reports `*wiredp` and `__bam_rsnap_refresh` sets `bt_rootpage = wired ? h : NULL` (`bt_search.c:105-129`); `__memp_wire` refuses mmap'd addresses and honours the 25% cap (`mp_fput.c:338-374`). Validity is LSN-checked before use (`bt_search.c:174-178`) **and again after the child is fetched** (`bt_search.c:577-599`). A stale child that turns out to be a non-btree page is treated as staleness, not corruption, under `SR_SNAPSHOT` (`bt_search.c:284-299`). Retired copies go to `bt_rsnap_free` rather than being freed, so a concurrent reader's copy never disappears (`bt_search.c:131-135`). `DB->compact` moving a subdb root disarms the cache under the handle mutex (`bt_compact.c:2638-2648`). The fast path is restricted to plain non-MVCC read descents (`bt_search.c:515-529`). |
| **D6** | Wired frames × eviction / failchk / file discard | A wired frame is exempt from eviction (`mp_alloc.c:337-339`), so unbounded wiring starves the cache, and a wired frame freed by a path other than `__db_free` would leak the region's wired count. | Hard cap `MPOOL_WIRED_MAX_PCT` = 25% per region, checked in `__memp_wire` (`mp_fput.c:367-369`); over the cap wiring is simply skipped and the caller falls back to a normal pin. `__db_free` unwires (`db_meta.c:320`) and `__memp_bhfree` unwires defensively for the close/discard paths (`mp_bh.c:818-826`). Only the one common tree root is wired (`bt_search.c:303-312`). **Not** reclaimed by failchk — see G4. |
| **D7** | Cursor shard choice × handle sharing across threads | Partition chosen from the wrong identity degenerates the sharding (every thread on one partition) or, worse, returns a cursor to a different partition than it was taken from → queue corruption. | Partition is derived from the **thread** id via `dbenv->thread_id`, never from `pid` and never from the optional `DB_THREAD_INFO` (both reasons documented at `dbinc/db_am.h:22-37`); it is recorded in `dbc->part` at allocation (`db_am.c:160`) and every later access uses `DB_CURSOR_PART(dbc)` (`db_cam.c:113`, `:122`, `:169`, `:178`, `:211`), so close-by-another-thread is correct. Non-threaded handles leave the mutexes `MUTEX_INVALID` and `CQ_LOCK` is a no-op, preserving the old behaviour exactly (`db_method.c:249-252`, `dbinc/db_am.h:41-54`). All whole-handle operations iterate every partition (`db.c:903-926`, `db_am.c:__db_cq_active_any`, `db_iface.c:131`, `partition.c:360`). |
| **D8** | SSI × replication commit lock list | The `regop` lock list is replayed by `__rep_process_txn` as `DB_LOCK_WRITE`, so a retained non-write mode leaking into it both overflows the allocation and can displace a real write lock from a truncated list (#140). | `__lock_vec` sizes and populates from the **same** `IS_WRITELOCK` predicate (`lock.c:447-455` and `:532-552`), with a runtime bounds check (`lock.c:540-548`) rather than a `DB_ASSERT` because the corruption would otherwise be silent in release builds. `DB_LOCK_SIREAD` is retained, not listed, and handled by `__lock_sicommit` just before `DB_LOCK_PUT_ALL` (`txn.c:1840-1846`). |
| **D9** | SSI × 2PC / prepare | A prepared transaction must be committable, but an SSI transaction can acquire its second edge after prepare → the upstream "prepared txn cannot commit" panic becomes reachable. | `__txn_prepare` rejects `TXN_SNAPSHOT_SAFE` with `EINVAL` (`txn.c:1449-1455`). Plain `DB_TXN_SNAPSHOT` remains preparable (gated by `ssi011`). |
| **D10** | SSI serializability × **conflict-tracking granularity** | Berkeley DB locks and tracks conflicts at **page** granularity. Serializability against *phantoms* (a scan that must not miss a concurrently-inserted key) is therefore an **emergent property of that granularity, not a designed mechanism**: an INSERT takes a write lock on the target leaf, and that acquisition is what walks the leaf's `sireaders` list and forms the edge against any transaction that previously scanned the page. There is no predicate/next-key lock anywhere in the engine. Consequence: any move toward a **key-precise read set silently loses phantom prevention** — the failure mode is not a slowdown but a non-serializable schedule that commits, with no error and no crash. | Today: nothing explicit — the invariant holds only because the read set is recorded per *page* and an insert must write-lock the page it lands on, so a scan's marker is unavoidably encountered. **This is load-bearing and must be treated as such**: it is the reason page granularity cannot simply be refined without adding range/next-key predicate locking *first*. Recorded here because it was previously unstated, and an unstated invariant is how a later "improvement" breaks correctness. See [RFC 0005](../../rfc/0005-row-level-conflict-tracking.md), whose blocking open question is exactly this, and the false-abort measurement that quantifies what the granularity costs in exchange. |

---

## 6. How to audit it

Per invariant: what actually enforces it today, and where the enforcement is
thin. "Thin" is stated honestly — a named gap is more useful than a reassuring
table.

| Invariant | Enforced by | Assessment |
|---|---|---|
| **C1** WAL log-before-data | `test/sim/test_sim_crash_recover` (capstone), `test_sim_data_log_order`, `test_sim_ckp_crash`; planted bug 1 NODURABLE and bug 3 LOSTUPDATE are both caught at K=1 (`test/sim/README.md`) | **Strong.** The planted-bug yardstick is the real evidence: the invariant has a demonstrated detector, not just a passing test. |
| **C1** on the *async* path | `test/sim/test_sim_aio_crash_recover` (crash with async writes outstanding; surviving set must match the synchronous run) | **Adequate for threadpool / sync-fallback.** See **G1**: io_uring bypasses the `__os_io` fault hook, so faults are not injectable there. |
| **C2** durable frontier | `test/sim/test_sim_ckp_lsn` (planted bug 5 CKPBADLSN), `test_sim_ckp_enospc`, and for the async path `test/sim/test_sim_aio_ckp_enospc` — which asserts the return-code contract directly (`txn_checkpoint` must fail when a page write fails under AIO) | **Strong for the two backends the fault hook reaches.** |
| **C3** `ckp_lsn` from oldest active | `test_sim_ckp_lsn`; TCL `recd*` | **Adequate**, inherited and long-exercised. |
| **C4** SIREAD GC safety | `test/isolation` (all 9 scenarios, incl. `write_skew_trigger` / `_late` / `g2_antidep` / `read_only_anomaly`); TCL `ssi001`–`ssi011`; `test/c/leak_si_locker` for the *bound*; `test/c/test_lock_sireads` + `chk.locksireads` under ASan | **Good.** `test_ssi_gc_pressure` now targets GC concurrent with edge formation directly, with both an anti-vacuity and a tamper control (see `test/isolation/SSI-GC-MARGIN.md`). See **G2** for the part still open: the "still needed?" gate is an `old_lsn` comparison one long-lived reader can pin. |
| **C5** MVCC purge safety | `test/c/mvcc_purge_visible` (a live snapshot must still see its version across 120 forced checkpoints) and `test/c/mvcc_purge_stress` (writers + readers + a tight `txn_checkpoint(DB_FORCE)` loop, intended for ASan **and TSan**), plus `DB->verify` | **Strong.** This is the best-covered of the new interactions: it has both a direct correctness gate and a race gate. |
| **C6/C7/D2** detail & locker lifetime | `ssi009` (multi-process marker churn, must not crash), `test/c/leak_si_locker`, `test/c/leak_si_mvcc_mtx`, `test/soak` slope check, ASan | **Good for the crash/leak shapes.** The double-free window itself is argued from the claim protocol, not directly provoked; see **G3**. |
| **R1** redo/undo correctness | `test/sim` (30+ scenarios: per-access-method crash, torn page/log, ENOSPC, split/merge, secondary, large txn, cursor, recovery-during-recovery, idempotent recover), `db_verify`, TCL `recd*` | **Strong**, and the planted-bug set makes it falsifiable. |
| **R2** #136 commit window | `test/isolation` `write_skew_trigger` and `write_skew_samebtree_trigger` (asymmetric verdict: one violation in any of 40 attempts is a reproduction, a pass needs all attempts clean); `ssi010` for the flag split; `test/bench/ssi_abort_bench` distinguishes SI from SSI | **Strong for the live property.** See **G5**: the *durable* consequence (crash immediately after the losing side's abort) is not itself a scenario. |
| **R3** region-only vs. logged | Implicitly by every `test/sim` scenario (regions are recreated on recover) plus `test_sim_recover_idempotent` | **Weak as a stated invariant** — see **G6**: nothing asserts that no *new* piece of region state became load-bearing for recovery. The table in §3 is currently the only artifact. |
| **A1** region compat gate | `__env_struct_sig` is mechanical and self-enforcing; the CI `abi-drift` gate covers the header/ABI side | **Adequate but untested end to end** — see **G7**: no test attaches a deliberately mismatched region and asserts `DB_VERSION_MISMATCH`. |
| **A2** failchk contract | `test/sim/mp_failchk_pilot` + `test/sim/mp-failchk.sh` (two real processes, shared non-`DB_PRIVATE` region, victim killed while holding a write lock, survivor runs `failchk`), `ssi009` (multi-process) | **This is the fork's only executable multi-process fault test.** Good that it exists; narrow — one kill point, one fault, uncontrolled interleaving (its own header says so). See **G8**. |
| **A3** global lock order | `src/mutex/mut_order.c` — a `DIAGNOSTIC`-only per-thread checker over this order (gap **G9**), plus `test/lockmatrix` for lock *modes* and `mvcc_purge_stress` under TSan | **Mechanically enforced for the region-level latches**, which are the ones whose misordering hangs multiple processes. Found one real violation (the `lk_partitions=1` self-deadlock, **since fixed in v2026.09.6** and now covered by a regression gate) and corrected five errors in the order as documented — see the corrections under §4. **Not** covered: `mtx_buf` (a pin, not a latch), so the os_aio stall class is invisible to it; that defect (S1) is **fixed structurally** in `__memp_sync_int` instead, and one of its two variants stalls while RUNNABLE — acquiring nothing — so no acquisition-time checker could see it at all. See **G9** and `test/c/OS-AIO-DEADLOCK-FIX.md`. |
| **D5** rsnap | Correctness rides on the whole read path: TCL suite, `test/sim` btree scenarios, `db_verify`. `DB_NO_RSNAP` gives an A/B switch (`bt_search.c:56-70`) | **Indirect.** See **G10**: no test targets the specific race (root change between LSN check and child fetch), and no test asserts the `DB_NO_RSNAP` A/B produces identical results. |
| **D6** wired frames | `mp_alloc` skips wired singletons; the cap is arithmetic. `test/bench` covers the throughput side | **Weak.** See **G4**. |
| **D7** cursor sharding | TCL suite exercises cursors heavily; whole-handle iteration paths are exercised by `db_close` / `associate` / `partition` tests | **Weak for the specific hazard.** See **G11**. |
| **D8** #140 lock list | `test/lockmatrix` (asserts the *invariant*, so a newly added mode is covered) under ASan, `test/repiso` (two real processes over real TCP, the client-isolation consequence) | **Strong, and the model for how a fork-specific hazard should be gated.** |
| **D9** SSI × prepare | TCL `ssi011` | **Adequate** (the rule is a flat rejection). |

### Named gaps

- **G1 — io_uring is not fault-injectable.** The DST ENOSPC/EIO faults fire
  inside `__os_io`, which the threadpool and synchronous-fallback backends call;
  io_uring submits a raw `io_uring_prep_write` and bypasses the hook (stated in
  `test/sim/test_sim_aio_ckp_enospc.c`'s header). So C2's error-propagation
  guarantee is *tested* on 2 of 5 backends and *argued* on the rest. The drain
  code is backend-agnostic, which is why this is a coverage gap rather than a
  correctness worry — but on io_uring nothing has ever forced the error path.
- **G2 — SIREAD GC concurrent with edge formation: the *safety* direction is
  now covered; the *visibility gate* is still an `old_lsn` comparison.**
  `test/isolation/test_ssi_gc_pressure` closes the part this gap named: it runs
  forced `__lock_sicleanup` (checkpoint *and* the `txn_begin` pressure sweep)
  squarely inside the window where one committed reader's marker is the only
  record of the edge, and asserts the anomaly is still caught — with a snapshot
  control showing the skew *is* committable when SSI is off (120/120), and a
  tamper control showing the assertion has teeth (40/40 skews when the safety
  predicate is neutered). So the asymmetry with `mvcc_purge_visible` is largely
  resolved.

  What remains is the gate itself, and issue **T1** is a concrete instance:
  `__lock_siclean_obj` decides "still needed?" by comparing the marker's LSN
  against `__txn_oldest_reader`, which **one** long-lived transaction pins
  indefinitely — so every later committed reader's marker was retained, the sweep
  reclaimed nothing, and the `TXN_DETAIL`s those markers pin exhausted the txn
  region. Fixed for the coalescible class (committed, read-only, no `WCONF`:
  those markers are interchangeable, so one suffices — see
  `test/isolation/SSI-GC-MARGIN.md`). For markers *outside* that class the
  question is still answered by the pinnable comparison, so a workload holding a
  long-lived reader while accumulating committed readers that **wrote** can still
  retain more than it needs. Bounded there by the write rate rather than the read
  rate, so far less severe — but the general gate is not closed.
- **G3 — the D2 double-free window is argued, not provoked.** The
  `TXN_DTL_SNAPSHOT` claim protocol is the only thing standing between
  `__txn_remove_buffer` and `__txn_reap_si_details`. `mvcc_purge_stress` under
  TSan exercises the mpool claimant hard; nothing drives *both* claimants at
  maximum rate simultaneously (SSI marker GC and eviction racing on the same
  detail). ASan would catch it if provoked; nothing provokes it deliberately.
- **G4 — wired frames have no dedicated test and no failchk story.** Three
  distinct things are unverified: that the 25% cap actually holds under
  concurrent wiring (the count is explicitly approximate under races,
  `mp_fput.c:361-366`); that every path that frees or reuses a wired frame
  unwires it (two sites do — `db_meta.c:320`, `mp_bh.c:823` — and the argument
  that these are exhaustive is by inspection); and what happens to
  `wired_pages` when a process dies mid-operation (A2: nothing, until the region
  is recreated). A stat is exposed (`mp_stat.c:713`), which is the hook a soak
  assertion could use.
- **G5 — R2's durable consequence is not a scenario.** `test/isolation` proves
  the losing transaction is told to abort; `test/sim` proves committed
  transactions survive a crash. No scenario crashes the environment in the
  window right after an SSI abort and asserts the aborted pivot left nothing
  durable. The argument in R2 (no commit record ⇒ nothing to redo) is solid, and
  this would be a cheap DST scenario to add.
- **G6 — nothing enforces the region-only/logged partition.** If a future change
  made some region field load-bearing for recovery, no test would notice; every
  DST scenario recreates the regions and so would silently be testing the new
  dependency rather than catching it. The §3 table is the only artifact; a
  mechanical check (e.g. asserting the set of fields read before pass 1) does
  not exist.
- **G7 — A1 is untested end to end.** No test writes a region with a bumped
  `majver`/`minver` or a perturbed `signature` and asserts
  `DB_VERSION_MISMATCH`. This is exactly the kind of gate that quietly stops
  working (e.g. if a code path attached before the check).
- **G8 — multi-process fault coverage is one pilot.** `mp-failchk.sh` is a real
  two-process kill-and-recover test, but: one kill point, one fault class, no
  interleaving control (v2 of the DST design would add the scheduler), and
  nothing exercising A2's *hard* case — a dead process holding a **region**
  mutex, which must produce `DB_RUNRECOVERY` and must not hang. That path
  (`mut_pthread.c:247-273`) is the difference between "failchk recovered it" and
  "you must run recovery", and it is the one an operator will hit.
- **G9 — lock-order enforcement now exists, partially.** A3 is a documented
  partial order; as of `src/mutex/mut_order.c` there is a `DIAGNOSTIC`-only
  per-thread checker over it (hooked at the `__mutex_*` redirection layer in
  `dbinc/mutex.h`, so all five `MUTEX_*` macros and the direct callers in
  `mut_region.c` / `mut_method.c` are covered by two hook sites). It validates
  *before* each acquisition — checking afterwards cannot work, because a
  self-deadlock never returns from the acquire call. Zero cost in production
  (`cc -E` shows `do { } while (0)`, no new symbol or string, `__env_struct_sig()`
  byte-identical); ~32% slower in a diagnostic build.

  Building it corrected five things in A3 — see the note under §4 — the largest
  being that the lock, txn and log "regions" are **one latch**, so a rank per
  `MTX_*` id cannot express the order at all.

  It found one real violation, the previously-unexplained `lk_partitions=1`
  hang: with a single partition `LOCK_SYSTEM_LOCK` is live
  (`dbinc/lock.h:340`) and `__lock_get_internal`'s SSI branch then takes
  `TXN_SYSTEM_LOCK` — the same latch — at `lock.c:1119`.

  **Still not covered:** `mtx_buf` ordering, because `mtx_buf` is a page *pin*
  held across arbitrary caller work rather than an ordered latch (`__memp_fget`
  returns holding it). So the checker would **not** by itself have caught the
  os_aio stall (S1, **since fixed** — see
  `test/c/OS-AIO-DEADLOCK-FIX.md`), whose shape is hold-and-wait-on-a-pin,
  not a latch misordering. That fix implements exactly the rule named here — do
  not wait while holding deferred-write pins — but enforces it *structurally in
  `__memp_sync_int`* rather than as a checker rule, and it has to cover **two**
  waits, not one: the blocking `mtx_buf` acquire (6 of 18 captures) and the
  `required_write` retry-loop yield (12 of 18). The second is the reason a
  checker alone would still not be sufficient: in that variant the stalled
  thread is **RUNNABLE and acquires nothing**, so there is no acquisition event
  to validate. Pin-aware accounting would need to be checked at *waits*, not at
  acquires. Also not covered: cross-*process* ordering (the checker is
  per-thread, per-process), and the process-local `DB_MUTEX_PROCESS_ONLY`
  latches, which are tracked for self-deadlock but not rank-ordered.
- **G10 — rsnap's race window is untested.** D5's second LSN check
  (`bt_search.c:577-599`) exists to close the gap between "snapshot looked
  valid" and "child fetched". Nothing forces that interleaving. Cheapest useful
  addition: a concurrent split/compact workload against readers with
  `DB_NO_RSNAP` on and off, asserting identical results — which would also turn
  the A/B switch into a correctness oracle rather than a benchmarking aid.
- **G11 — cursor sharding's cross-thread close is untested as such.** D7's
  correctness rests on `dbc->part` being honoured on close. A cursor allocated
  by thread A and closed by thread B is legal and is what the design explicitly
  supports; no test does it deliberately and asserts partition accounting
  (`db_stati.c:395-412` reads per-partition queues and would be the oracle).

### OPEN QUESTIONS

These could not be settled from the code and are recorded rather than asserted.

- **Q1 — `nsireaders` drift.** It is documented as approximate
  (`dbinc/lock.h:115`) and every decrement is guarded by
  `atomic_read_relaxed(...) > 0` (`lock.c:154`, `:325`, `lock.c:~1089`). If it
  drifts *low* the pressure trigger in `txn_begin` (`txn.c:326-328`) fires late.
  Whether the drift is bounded, and whether the checkpoint-driven sweep alone
  bounds the footprint when checkpoints are rare *and* the trigger has drifted,
  is not derivable from the code. The measured bound (#137, peak independent of
  txn count) is empirical evidence that it is fine in practice, not a proof.
- **Q2 — `__memp_purge_obsolete` starvation.** Every skip is best-effort
  (`MUTEX_TRYLOCK`, busy-buffer skip). A bucket whose oldest version is
  perpetually contended is retried "at the next checkpoint or by eviction"
  (`mp_alloc.c:780-782`). Whether progress is guaranteed under a pathological
  steady state (continuous readers keeping the chain head referenced) is not
  established.
- **Q3 — `hp->old_reader` monotonicity across a purge and an eviction racing in
  the same bucket.** Both advance it only *toward* the frontier under `mtx_hash`
  (`mp_alloc.c:412-423` and `:835-836`), so neither can move it past a live
  reader. Whether two frontiers computed at different times can interleave such
  that a bucket briefly carries a frontier *newer* than the one the concurrent
  caller validated against — harmless if the per-buffer `BH_OBSOLETE` re-check
  under both latches is the real gate, which it appears to be — was not proven.
- **Q4 — rsnap torn LSN read.** `__bam_rsnap_child` reads the live root LSN
  without a latch and the comment says a torn read "just forces a refresh"
  (`bt_search.c:173-178`). That is true for any *mismatching* value. Whether a
  torn read could reconstruct a value equal to the snapshot LSN while the root
  has in fact changed depends on `DB_LSN` store atomicity on the target
  platform; it is not argued in code. The post-fetch re-check plus the
  `SR_SNAPSHOT` bad-page-type fallback (`bt_search.c:284-299`) appear to make
  even that benign, but the reasoning is not written down anywhere.
- **Q5 — the narrow `TXN_DTL_SICHECKED` error window.** `dbinc/txn.h:150-153`
  documents it: if commit publishes `SICHECKED` and then *fails*, a writer may
  abort itself needlessly until the status reaches `TXN_ABORTED`. This is
  asserted to err safe (spurious `DB_SNAPSHOT_UNSAFE`, never a missed one). The
  claim is convincing but has no test.
- **Q6 — SSI under replication.** `__txn_commit`'s pivot check runs on the
  master; a client applies the `regop` lock list. Since no SSI state is logged
  (R3), a replica does not re-derive pivots — correct, because the master
  already admitted only serializable schedules. What is *not* established is
  the behaviour of a client that itself runs SSI read transactions against
  applied data. `txn.c:277-283` notes a replication client has already been
  rejected with `EINVAL` before the SSI path is entered; whether that covers
  every entry point was not audited here.

---

## Reading order for a reviewer

1. §3 R3 (the region-only vs. logged table) — it is the recovery argument, and
   the shortest path to seeing why the SSI/MVCC additions do not touch it.
2. §4 A3 (the global lock order) — the classic failure mode for a change of this
   shape, and the thinnest coverage (**G9**).
3. §5 (the interaction table) — each row is a place where two individually
   correct subsystems could be composed wrongly.
4. §6's gap list — G2, G4, G8, G9 are the four worth arguing about.
