# RFC 0022: page allocation as a nested top-level action (release `PGNO_BASE_MD` before commit)

- **Status:** Draft — **phase-1 implementation found unsafe; see Implementation findings**
- **Type:** Normative
- **Author:** P3 workstream (libdb contributors)
- **Date:** 2026-11-03
- **Tracking:** review item M2 / O-B2, perf rank 3; BTREE-LOCK-SCOPE-2026-09.md Deliverable 2
- **Prototype:** `rfc/0022/` (design spike + safety argument); `perf/meta-nta` branch
  (RFC, recovery test, convoy reproducer). The engine change was implemented,
  measured, found unsafe, and reverted — the branch keeps the tests and this
  record.

---

## Summary

`__db_new` (page allocation) and `__db_free` (page free) write-lock the database
metadata page (`PGNO_BASE_MD`, page 0) and **hold that lock until the enclosing
user transaction commits**, because they release it with `__TLPUT`, which is a
no-op for a write lock inside a transaction. Under concurrent inserts this
serialises every page-allocating writer behind one lock object across a full
durable commit. BTREE-LOCK-SCOPE-2026-09.md measured **400 of 400 lock waiters on
page 0** at t=96 and a put p99 of 282 ms, with **zero** waiters on any leaf or
split-ancestor page.

This RFC makes the free-list mutation a **nested top-level action (NTA)** in the
ARIES sense: a small, independently-committed unit of work that releases the
meta-page lock the moment the allocation finishes, instead of at the parent's
commit. The user transaction keeps the *per-page* lock on the page it allocated
(so no reader sees the page before the parent commits), but it no longer holds
the *meta-page* lock. If the parent aborts after the NTA committed, the page is
reclaimed by a compensating free, exactly as ARIES prescribes for an NTA whose
parent rolls back.

## Implementation findings (2026-11-03) — the sub-transaction route is unsafe

The no-log-format route (a child transaction, §Design below) was implemented in
`__db_new`, measured on a 32-vCPU box, and **found unsafe**. The findings are the
substance of this RFC's decision and they redirect the correct design. Both
convoy findings are confirmed, not inferred.

**Finding 1 — a child-transaction commit inherits locks UP to the parent; it
does not release them.** `__txn_commit` of a child calls `__lock_inherit_locks`
(`src/lock/lock.c:523`), handing the child's locks to the parent rather than
releasing them. So committing the NTA child still left `PGNO_BASE_MD` held by the
parent to its commit — the exact convoy. The concurrency reproducer
(`test/db/meta_nta_conc.c`) measured full serialisation (elapsed ≈ N×HOLD_MS) on
both the "fixed" and the baseline builds: **zero win**.

**Finding 2 — explicitly releasing the child's meta lock with `__LPUT` before
commit deadlocks via lock aliasing.** `__bam_split` **pre-acquires** the meta
write lock under the *parent* locker (`src/btree/bt_split.c:84`, comment: "we
will have to allocate pages and cannot get a lock while we have the search tree
pinned") and holds it across its `__db_new` calls, releasing it at
`bt_split.c:191`. Because a child locker is **compatible** with its parent
(`src/lock/lock.c:2379-2407`), the lock manager satisfies the child NTA's
`__db_lget(meta, WRITE)` by **refcount-bumping the parent's existing lock**, not
by a new grant. `__LPUT` of "the child's" meta lock therefore released the
**parent's** lock that `__bam_split` still needs — corrupting the split's lock
state. Under 8 concurrent allocators every worker then hung in `__bam_split` at
`bt_split.c:84` waiting for the meta lock (gdb-confirmed, all 8 stacks identical).

**Finding 3 — the abort-undo safety argument rules out the bare early release
(alternative A4), independently.** Recovery/undo routines take no lock-manager
locks (`src/db/db_rec.c` `__db_pg_alloc_recover` acquires none); they rely on the
aborting transaction already holding them (2PL). So if `__db_new` released the
meta lock early with `__LPUT` and the txn later aborted, the undo
(`meta->free = argp->pgno`) would run against a free-list head another allocator
had advanced in the meantime → **free-list corruption**. This is why the
allocation must be *committed independently and compensated*, not merely
released, and why a correct NTA needs an **independent locker** whose meta lock
is genuinely released.

**What is actually required (the corrected design).** `__bam_split` already
demonstrates the meta lock need not be held to commit: it releases its own meta
lock at `bt_split.c:191`, before commit. The lock that persists to commit is
`__db_new`'s *additional* reference, left by its `__TLPUT`. The correct ARIES
shape is therefore to make the **structure-modification operation (the split)
the NTA**, not each `__db_new`: `__bam_split` (and the other allocation sites)
begin an independent NTA, acquire the meta lock under the NTA's own locker, run
their `__db_new`/`__db_free` calls under it, and commit the NTA — releasing the
meta lock — before returning to the user transaction, which keeps only the
per-page locks. One locker owning the meta lock for the whole SMO removes the
aliasing and gives a genuinely independent, compensation-on-abort unit. It is a
coordinated change across `__bam_split`, `__bam_root`, `__db_new`, `__db_free`
and every other `__db_new` caller, with recovery coverage for each — larger and
higher-risk than the sub-transaction spike, and **out of scope for the phase-1
attempt**. It is the recommended path for a phase-2 RFC revision.

**Decision implication:** the lazy no-format route does not work. A correct fix
is either (a) the SMO-as-NTA restructure above (no log-format change, but a
substantial recovery-sensitive change), or (b) a dedicated NTA log record
(master-only, §Alternatives A1). Both exceed this workstream's phase-1 budget; the
phase-1 engine change has been reverted. The recovery test
(`meta_nta_recover.c`, which passes and exercises the abort/recover paths the Tcl
suite does not reach) and the convoy reproducer (`meta_nta_conc.c`, which
demonstrates the measured serialisation on master) are kept as the foundation for
phase 2.

The chosen implementation needs **no log-format change**: it reuses the existing
`__db_pg_alloc` / `__db_pg_free` / `__db_pg_freedata` records and their recovery
routines, and the existing `TXN_FAMILY` / `TXN_COMPENSATE` child-transaction
machinery. A log-format alternative (a dedicated NTA/`__db_pg_alloc_nta` record)
is described and rejected as unnecessary.

## Motivation

### The measured defect

`__db_new` (`src/db/db_meta.c:134`) takes `PGNO_BASE_MD` `DB_LOCK_WRITE` with
`LCK_ALWAYS` and releases it at `db_meta.c:263` with `__TLPUT`. `__TLPUT`
(`src/dbinc/db_am.h:234` → `__db_lput`) **keeps a write lock inside a
transaction** — it only releases read locks or no-txn locks. So the meta write
lock acquired to splice one page off the free list is held across the allocating
transaction's own ~3.7 ms `fsync` and to commit. `__db_free`
(`src/db/db_meta.c:339`) does the same.

BTREE-LOCK-SCOPE-2026-09.md, t=96 unwarmed bulk insert, census of 400 WAIT
records:

| lock object | WAIT records | HELD records |
|---|---:|---:|
| page 0 (`PGNO_BASE_MD`) | **400** | 5 |
| every other page (leaves + internal) | **0** | 95 |

6.1 % of puts allocate a page; at 240 allocations/s each holding page 0 across a
3.72 ms commit, Little's law predicts a 333 ms queue against a measured put p99
of 282 ms — the queue closes. WRITE-TAIL-2026-09.md probe 1 is the corroborating
control: at t=96, switching the same workload to `DB_TXN_NOSYNC` collapses put
p99 from **269 ms to 0.12 ms** with locking unchanged, which can only happen if
holders sit in the durable commit while holding page locks. XREL-2026-10.md's
addendum attributes the TPC-B write-no-sync gap at t≥16 (0.79–0.81× of 5.3.28) to
this same convoy (review B §4: "strong code-reading inference backed by two
measured probes").

### Why this is the right lever

The meta lock does **not** need to be held to commit for correctness. 2PL
requires holding a lock until commit only to protect data the transaction read or
wrote *and that another transaction's isolation depends on*. The meta page's
free-list head is engine bookkeeping, not user data: no user transaction's
serialisability depends on the order in which free pages are handed out. What the
allocator genuinely must prevent is **two allocators handing out the same page**
and **a reader seeing a half-initialised page before the parent commits**. The
first is a short critical section (the latch + lock while the free list is
spliced); the second is enforced by the *per-page* lock, which the parent keeps.
That is precisely the decomposition an NTA provides.

This is also how Berkeley DB historically justified the operation: `__db_new`'s
log record `__db_pg_alloc` already carries everything undo needs
(`src/db/db.src:182`), and `__db_pg_alloc_recover` (`src/db/db_rec.c:660`)
already implements the ARIES undo: on `DB_UNDO` it puts the page back on the free
list (or marks a never-written page for truncation). The free-list change is
*already* a logically self-contained, independently recoverable action. The only
thing missing is releasing its lock early.

## North-star check

- **Embedded / no-server:** unchanged. No new thread, no new process, no new
  region field in the chosen (no-format-change) design.
- **ACID:** preserved. See Design §4 (safety argument) and §5 (abort). The NTA
  is itself atomic and durable-on-recovery; the parent's atomicity is preserved
  by the compensating free on abort; isolation is preserved because the parent
  retains the per-page lock.
- **Crash recovery:** the chosen design reuses the existing `__db_pg_alloc` /
  `__db_pg_free` records and recovery routines unchanged, so recovery behaviour
  is identical to today's for a committed allocation. The new behaviour is only
  that the alloc record belongs to a sub-transaction that commits before the
  parent; recovery already handles committed sub-transactions (`DB___txn_child`).
  A crash between the NTA commit and the parent commit leaves an allocated,
  unused page — handled in §5, identical to an ARIES NTA whose parent never
  committed, and already the behaviour BDB tolerates for a crash mid-txn (the
  page is off the free list but owned by no one; §5 argues this is harmless and
  how `db_verify` and existing freelist reclamation treat it).
- **Access methods:** btree (incl. recno), hash, queue, heap all reach the
  allocator through `__db_new`/`__db_free`; the change is in that shared code, so
  all five get it. Hash has an extra path (`__ham_return_meta`) handled in §3.3.
- **Multi-process correctness:** the meta-page latch (buffer) and the meta-page
  lock-manager lock are the two serialisers; both are process-shared and both are
  respected by the NTA. The NTA acquires the same lock the same way; it just
  releases it with `__LPUT` (an unconditional put) at the end of the action
  instead of `__TLPUT`.
- **On-disk / log / region / ABI format stability:** **no change** in the chosen
  design. The log-format alternative (§Alternatives) is master-only and gated,
  but it is **not** the recommended path.

## Design

### 1. What an NTA is here

An ARIES *nested top-level action* is a sub-sequence of a transaction that:

1. does its own logging,
2. **commits independently** of the parent (its effects survive even if the
   parent aborts), and
3. is undone by a **compensating action** if the parent later decides it did not
   want the effect — not by rolling back the NTA's own log records.

For page allocation the NTA is: "splice page P off the free list (or extend the
file by one page) and log it." Its compensating action is: "free page P back to
the free list." Both already exist as `__db_pg_alloc` and `__db_pg_free`.

Berkeley DB's existing vehicle for an NTA is a **child transaction** begun with
the engine-internal flag set, committed with `DB_TXN_NOSYNC` (durability of the
allocation rides on the parent's commit record ordering; see §4.4). The child's
commit releases the child's locks — including the meta-page lock — immediately.

### 2. The lock decomposition (the crux)

`__db_new` today acquires **two** lock-manager locks:

- `metalock` on `PGNO_BASE_MD` (`DB_LOCK_WRITE`, `LCK_ALWAYS`) — the convoy.
- `*lockp` on the newly allocated page `pgno` (`DB_LOCK_WRITE`), returned to the
  caller, which holds it to commit so no concurrent reader sees the page before
  the allocating transaction commits (`__bam_split` keeps `llock`/`rlock`;
  `bt_put.c:1105` keeps its lock; `__db_overflow`/hash pass `NULL` and rely on
  the page being unreachable until the parent links it in).

The NTA splits ownership:

| lock | who acquires | who releases | when |
|---|---|---|---|
| `metalock` (page 0) | the **NTA child** locker | the NTA child, via commit | at end of the NTA (µs) |
| `*lockp` (page P) | **still the parent** locker | the parent, at commit | unchanged |

The page lock stays with the parent. This is the invariant that preserves
isolation: a concurrent transaction that somehow reaches page P (it cannot,
until the parent links it in, but we do not rely on that) blocks on the parent's
page lock exactly as today. Only the *meta* lock moves to the short-lived child.

The page lock **must be acquired under the parent locker before or during the
NTA**, not under the child locker, otherwise the child's commit would release it
and expose the page. `__db_new` already acquires `*lockp` with `dbc` (the
parent's locker) — so the fix is specifically to run only the *meta* acquisition
and free-list splice under the child locker, and leave the `*lockp` acquisition
on `dbc`.

### 3. Code shape

#### 3.1 A helper that runs the free-list splice as an NTA

Add to `src/db/db_meta.c`:

```
static int __db_nta_begin __P((DBC *, DBC **));   /* begin child + child dbc */
static int __db_nta_commit __P((DBC *, DBC *, DB_LSN *));   /* commit child, keep page lock on parent */
static int __db_nta_abort  __P((DBC *, DBC *));
```

`__db_nta_begin` begins a child transaction of `dbc->txn` with
`DB_TXN_NOSYNC | <internal NTA flag>` and opens a cursor (`__db_cursor_int`) on
the same `dbp` bound to the child txn, with `DBC_DONTLOCK` **not** set (the child
*does* take the meta lock — that is the point) but inheriting the parent's
`thread_info`. If `dbc->txn == NULL` (non-transactional / autocommit-off / the
`DBC_RECOVER` path), there is nothing to defer: fall back to today's inline code
exactly (the meta lock is released by `__TLPUT`/`__LPUT` at end of op anyway when
there is no txn). So the NTA path is taken **only when `dbc->txn != NULL` and
logging is on**.

`__db_nta_commit` commits the child (`__txn_commit(child, DB_TXN_NOSYNC)`), which
releases the child's meta lock. It returns the commit LSN so the parent can order
its own writes after it (already guaranteed by log append order).

`__db_nta_abort` aborts the child (used only on an error *inside* the NTA, before
it committed — e.g. the `__memp_fget` of the free page failed). It rolls back the
child's own records normally (no compensation needed — the NTA never committed).

#### 3.2 `__db_new` restructured

```
pgno = PGNO_BASE_MD;
nta = (dbc->txn != NULL && DBC_LOGGING(dbc) && dbp->type != <recover/dontlock>);

if (nta) {
    /* (a) Acquire the PAGE lock under the PARENT locker first, so the
     *     child's commit cannot release it. For the free-list case we
     *     know pgno only after reading meta, so we take the page lock
     *     after reading the free head but under dbc (parent), as today. */
    if ((ret = __db_nta_begin(dbc, &ndbc)) != 0) goto err;
    /* run the existing splice under ndbc instead of dbc:
     *   - __db_lget(ndbc, LCK_ALWAYS, PGNO_BASE_MD, DB_LOCK_WRITE, ...)
     *   - __memp_fget(DIRTY) meta
     *   - read free head / extend
     *   - __db_lget(dbc,  0, pgno, DB_LOCK_WRITE, lockp)   <-- PARENT locker
     *   - __db_pg_alloc_log(ndbc->txn, ...)                <-- child txn
     *   - meta->free = newnext; (extend: meta->last_pgno=pgno)
     *   - __memp_fput meta
     * then commit the child, releasing the meta lock: */
    ret = __db_nta_commit(dbc, ndbc, &commit_lsn);
    /* page lock (*lockp) is still held by the parent */
} else {
    ...today's inline code, __TLPUT at end...
}
```

Two subtleties:

- **`__txn_add_fe_watermark`** (file-extension watermark, `db_meta.c:217`) must be
  recorded against the **parent** `dbc->txn`, not the child, because the watermark
  governs the parent's bulk file-extend semantics. Pass `dbc->txn` explicitly.
- The **sorted-free-list** in-memory array (`__memp_get_freelist` /
  `__memp_extend_freelist`, `HAVE_FTRUNCATE`) is process-local mpool state keyed
  by the open `dbp`, not by the txn; mutating it under the child is correct
  because it is protected by the meta buffer latch, which the child holds while
  dirtying meta.

#### 3.3 `__db_free` restructured

Identical pattern. `__db_free` is more involved because it may write either the
meta page or a *previous* free-list page (the sorted-list middle-insert case) and
may **truncate the file** (`__db_truncate_freelist`, which itself flushes the
log). The NTA child does the free-list splice and the truncate; the parent keeps
no page lock on a freed page (the page is being given up), so there is nothing to
retain on the parent — the free is wholly inside the NTA. This is the cleaner
case.

One ordering constraint: `__db_free` is sometimes called while the parent holds a
*write lock on the page being freed* (e.g. `__bam_delete` freed a page it had
locked). Freeing a page back to the free list while the parent still holds its
page lock is fine: the page lock is released at parent commit, and the NTA's
free-list splice does not depend on it. But a subtlety: if the parent later
**aborts**, the parent's undo of the delete wants the page back. Because the free
was an NTA that committed, the page is on the free list and may have been
re-allocated by another transaction. This is the ARIES rule: **a page freed
inside a committed NTA must not be reusable until the freeing transaction
commits.** See §5.2 — this is why `__db_free`'s NTA keeps a *deallocation hold*:
the page lock on the freed page is retained by the parent (not released by the
child) exactly as the allocation case retains the new-page lock, so no other
transaction can allocate it until the parent commits. (Today `__db_free` does not
return a lock; the NTA version keeps the existing page lock the parent already
holds, and the allocator in `__db_new` must respect it — it already does, via the
`__db_lget(dbc, 0, pgno, DB_LOCK_WRITE, lockp)` on the free page it is about to
hand out, which will block on the parent's retained lock. This is the existing
mechanism; we simply stop no-op-releasing it.)

#### 3.4 Hash

Hash reaches the allocator with `meta` already pinned via `__ham_return_meta`
(it keeps the hash metapage dirty across the op). For hash the meta-page *lock*
is the hash metapage lock, which RFC 0017 is independently removing from the hot
path. The NTA for hash runs the same way; the only care is that
`__ham_return_meta`'s pinned meta is released/re-pinned around the child commit.
Because hash's own metapage lock scope is being reworked by RFC 0017, **this RFC
scopes the NTA to btree/recno/queue/heap in phase 1 and leaves hash's allocator
on the inline path**, to avoid a two-front change to the hash metapage. Phase 2
unifies them once RFC 0017 change 1 lands on master (P4). This is recorded as an
open item, not a silent gap.

### 4. Safety argument (ACID)

#### 4.1 Atomicity of the NTA itself

The NTA is a child transaction; its one or two log records (`__db_pg_alloc`,
possibly a freelist/truncate record) are made atomic and durable by the ordinary
child-commit log record (`DB___txn_child`). Recovery already redoes a committed
child's records and undoes an uncommitted child's records. No change.

#### 4.2 No two allocators hand out the same page

The free-list head splice happens under the meta buffer latch **and** the meta
`DB_LOCK_WRITE` lock, both held by the child for the duration of the splice.
Serialisation of allocators is unchanged — it is still one-at-a-time through the
meta lock. We shortened *how long* the lock is held (to the splice, ~µs) not
*how* it serialises. Two concurrent `__db_new`s still cannot both take page P.

#### 4.3 No reader sees page P before the parent commits

The parent retains `*lockp` (`DB_LOCK_WRITE` on page P) to commit. Any
transaction that reaches page P takes a conflicting lock and blocks on the parent
— identical to today. The child's commit releases only the *meta* lock, never the
page lock. (Verified against every `__db_new` caller: `__bam_split`,
`__bam_iitem` via `bt_put.c`, `db.c`, `db_compact.c`, `db_overflow.c`,
`hash_dup.c`, `hash_page.c` — the ones that need reader-invisibility pass a
non-NULL `lockp` and hold it; the ones that pass `NULL` allocate a page that is
not yet linked into the tree, so it is unreachable by key, and the parent's
subsequent write of the linking page is itself under a held lock.)

#### 4.4 Durability ordering

The allocation record is written and the child commits with `DB_TXN_NOSYNC`:
the child does **not** force the log. Durability of the allocation rides on the
parent's eventual commit (or any later group flush), because the log is a single
ordered stream and the parent's commit record is after the child's records. If
the system crashes before the parent commits, the allocation may or may not be on
disk; either way recovery reaches a consistent state (§5). This is the standard
NTA durability contract and matches how BDB already treats sub-transaction
commits (they do not force the log independently).

#### 4.5 WAL / redo ordering within the file

`LSN(meta)` and `LSN(h)` are stamped by the child's records exactly as today.
The page-P write that the parent later makes (linking it into the tree) carries a
later LSN. Redo replays meta-splice then page-link in LSN order — unchanged.

### 5. Abort and crash (the ARIES NTA question)

#### 5.1 Parent aborts after the NTA committed

The NTA (allocation) is durable; the parent must **compensate**, i.e. free the
page. The parent's abort path (`__db_txn_undo`) walks the parent's log records
backward and undoes them. The allocation record belongs to the *child*, which
committed, so parent undo does **not** touch it. Therefore the parent's abort
must issue a *new* compensating free.

Two options, both standard:

- **(A) Compensate at abort time.** Register the allocated page with the parent
  (a txn-detail list of "pages allocated by committed NTAs") and, in the parent's
  abort, call `__db_free` on each. This is the exact ARIES pattern. Cost: a small
  per-txn list and a free per allocated page on abort. Aborts are the rare path,
  so this is cheap in the common case.
- **(B) Leak the page (reclaimed later).** Do nothing on abort; the page is off
  the free list and owned by no one. It is not corruption — it is a leaked free
  page, identical to the state after a crash mid-allocation today. `db_verify`
  already tolerates pages that are neither on the free list nor reachable (it
  reports them, it does not fail), and `DB->compact` / a future freelist audit
  reclaims them. This is what Berkeley DB historically did for NTA allocations:
  **an aborted allocation leaks the page; it is harmless.**

**Recommendation: (B), with a diagnostic counter**, matching historical BDB and
ARIES's observation that NTA allocations may leak on abort and that this is
acceptable because the page is simply unused space, not inconsistent state. (A)
is available as a follow-up if leak rate under abort-heavy workloads proves
material; a bulk-abort microbenchmark in the evidence section decides this.
Leaked-page count is exposed via `DB->stat` (`bt_pagecnt` vs free + reachable) and
asserted zero in the non-abort tests.

The RFC is explicit per the brief: **when the allocating transaction aborts after
the NTA committed, the page is allocated but unused; option (B) argues it is
harmless (a leaked free page, reclaimable by compact/verify, never an
inconsistency), which is exactly what ARIES and historical BDB do.**

#### 5.2 `__db_free` NTA + parent abort

Symmetric and sharper: a page freed by a committed NTA must not be re-allocated
until the freeing parent commits, else the parent's abort (which wants the page
back with its old contents) finds it reused. The parent retains the **page lock**
on the freed page (§3.3), so `__db_new` cannot hand it out — it blocks on the
parent's lock. On parent abort, the page is still on the free list but locked by
the parent; the parent's undo of whatever freed it (e.g. re-inserting the btree
page) re-allocates it via the normal undo path, which already handles this. On
parent commit, the page lock drops and the page becomes allocatable. This is the
existing BDB invariant ("no one holds a free page locked" is **false** during
this window — it is held by the freeing parent — and `__db_new`'s comment at
`db_meta.c:150-156` about "no one will hold a free page locked" must be updated:
the NTA makes the freeing transaction hold it until commit, and `__db_new`
already `__db_lget`s the free page with `DB_LOCK_WRITE` before using it, so it
correctly blocks).

#### 5.3 Crash between NTA commit and parent commit

Recovery redoes the committed NTA (page is off the free list), then finds the
parent uncommitted and undoes the parent's records. The parent never issued a
compensating free (it did not reach abort cleanly — it crashed), so the page is
left off the free list, owned by no one: a leaked free page, as in §5.1(B).
Harmless. The deallocation case (§5.2): recovery redoes the committed free (page
on free list) and undoes the parent; the parent's page lock is gone after a
crash, so the page is genuinely free — which is correct, because the parent did
not commit the operation that freed it, but the free itself was logically
complete and the page's old contents are recoverable from the log if any
committed transaction needs them (they do not — the page was being freed).

### 6. Config / flags

No new user-visible flag. An engine-internal txn flag marks the NTA child so
`__txn_commit` / `__db_check_txn` treat it like the existing compensating/child
txns (it may run during the parent, it does not force the log, it is not subject
to the "no begin during recovery" check if invoked from recovery — though the NTA
path is disabled during recovery, §3.2). The `TXN_COMPENSATE` flag already carries
most of this semantics; the NTA reuses it or adds a sibling `TXN_NTA` bit in the
in-memory `DB_TXN.flags` (process-local, no region/ABI impact).

## Alternatives considered

### A1. A new dedicated NTA log record (`__db_pg_alloc_nta`) — **rejected**

A master-only log-format change (allowed by user decision Q1) adding a record
that is self-describing as an NTA, with its own redo/undo and a
`DB_LOGVERSION` bump. This would require: recovery routines, `dist/*.src` edits,
regeneration via `dist/s_recover` + `dist/s_include`, `db_printlog` support,
`db_verify` coverage, and an upgrade note. It buys **nothing** over reusing the
existing `__db_pg_alloc` record inside a child transaction, because the existing
record's recovery already does the right redo and undo. We reject it on the
laziness principle: do not change the log format when the existing records plus
the existing child-transaction machinery already express the action. (If a future
need arises — e.g. encoding the compensation inline to avoid the abort-time free
in option 5.1(A) — this alternative is revisited; it is documented so the path is
known.)

### A2. `DB_TXN_BULK` page reservation — **complementary, not a substitute**

`DB_TXN_BULK` (`src/txn/txn.c:374`) already watermarks file extension. Review
O-B2 suggests reserving a run of pages under one meta hold so one acquisition
serves many inserts. This reduces the *frequency* of the meta hold but still holds
it to commit for the reservation, so it does not fix the convoy for the
steady-state (non-bulk) insert mix, and it changes the free-page accounting
(reserved-but-unused pages). It is a good complement for bulk load and can layer
on top of the NTA later, but it is not the general fix. Not in scope here.

### A3. Lock-free / atomic free-list head — **rejected for now**

A CAS-based free-list head would remove the meta *lock* entirely. But the meta
page also carries `last_pgno` and is a logged, recoverable object; making its
mutation lock-free while keeping it crash-consistent is a much larger change
(page-format and recovery implications) for a convoy the NTA already removes.
Deferred; noted in RFC 0015's broader scalability arc.

### A4. Early `__LPUT` of the meta lock without an NTA — **rejected (unsafe)**

Simply replacing `__TLPUT` with `__LPUT` in `__db_new` releases the meta lock
mid-transaction while the allocation's log records still belong to the parent.
On parent abort, parent undo rolls back the `__db_pg_alloc` record and expects
the meta lock to still protect the free list — but it was released, so a
concurrent allocator may have changed `meta->free` in the meantime, and the undo
(which sets `meta->free = argp->pgno`) corrupts the list. This is why the action
must be an NTA (independently committed, compensated on abort), not merely an
early release. Recorded as the trap to avoid.

## Risks & open questions

- **Correctness of child-txn commit on the allocator hot path.** The NTA adds a
  child `__txn_begin` + `__txn_commit` per page allocation. `__txn_begin`/`end`
  take `TXN_SYSTEM_LOCK` (= `mtx_regenv`), which review B §3 identifies as a
  contended latch. So the NTA **trades the meta-page lock convoy for extra
  `mtx_regenv` traffic**. This must be measured: if `mtx_regenv` becomes the new
  convoy, the win is smaller than hoped. Mitigation: only 6.1 % of puts allocate,
  so the added begin/commit rate is ~6 % of the txn rate, not 100 %. Still, this
  is the single biggest performance risk and the benchmark must profile it
  (`perf --call-graph dwarf` on one cell, per the brief). If `mtx_regenv`
  dominates, a lighter-weight NTA (reuse a per-thread cached child txn, or a
  bare "mini-transaction" that logs + releases the lock without a full
  `__txn_begin`) is the fallback — described as a phase-1b option.
- **Abort-time leak rate (5.1).** Option (B) leaks a page per allocation whose
  parent aborts. Deadlock-heavy workloads (recno renumbering, hash splits pre-RFC
  0017) abort often. A bulk-abort microbenchmark measures the leak rate; if it is
  material, switch to option (A) (compensate at abort).
- **Hash deferred to phase 2** (§3.4). Recorded, not silent.
- **Interaction with `DB_TXN_BULK` watermark** (§3.2): the watermark must stay on
  the parent. Covered, but the test matrix must include a bulk-load arm.
- **Recovery idempotency under interrupted recovery.** The DST bug harness
  (`DB_DST_INJECT_BUG=9`, `db_rec.c`) already stresses `__db_pg_alloc` redo
  idempotency; the NTA changes *who commits* the record, not the record, so the
  existing crash-in-recovery tests apply unchanged and must pass.
- **`db_printlog`:** no change (no new record). **`db_verify`:** must learn that a
  leaked free page (off the free list, unreachable) is a warning, not an error —
  verify it already treats it so, and add an explicit test.

## Prototype / evidence

`rfc/0022/` holds:

1. A written safety argument cross-checked against every `__db_new`/`__db_free`
   caller (the §4.3 caller audit).
2. The benchmark plan: TPC-B-like (`tproc_b.c`, `TPROC_B_RMW=1`, write-no-sync
   and sync) at t=1/4/16/32; `xrel_kv` btree/hash/heap insert at t=4/16/32; base
   vs fix plus 5.3.28 where comparable; ≥5 reps; a lock-wait census mirroring
   BTREE-LOCK-SCOPE (expect page-0 waiters 400→~0).
3. The two decisive tests: recovery redo/undo of the NTA allocation (a C
   crash-loop and `run_recd -btree recd001..recd025`), and a concurrency test
   showing two page-allocating transactions no longer serialise to commit.

**If the benchmark shows the NTA merely moves the convoy to `mtx_regenv` with no
net win, this RFC's phase 1 is a negative result and we stop (or fall back to the
cached-mini-transaction variant), which the brief names as a valid outcome.**

---

## Decision

*(Filled by the reviewer when the RFC is decided.)*

- **Decision:** Accepted | Rejected — YYYY-MM-DD
- **Rationale:**
- **Conditions / follow-ups:**

### Phase-1 outcome (P3, 2026-11-03)

- **Phase-1 (sub-transaction, no log-format change): implemented, measured,
  reverted.** See *Implementation findings* above. The child-txn route gives no
  win (lock inheritance on child commit) and the explicit-release variant
  deadlocks (lock aliasing with `__bam_split`'s pre-held meta lock). The bare
  early-release (A4) corrupts the free list on abort. All three are confirmed on
  a 32-vCPU box.
- **Kept on the branch:** this RFC with the findings; `test/db/meta_nta_recover.c`
  (passes — abort + catastrophic-recovery consistency, a path the Tcl recd suite
  does not cover); `test/db/meta_nta_conc.c` (the convoy reproducer — measures the
  serialisation on master; it is wired as expected-serialised until a correct fix
  lands).
- **Recommended next step:** a phase-2 revision making the **split/SMO the NTA**
  (an independent locker owns the meta lock for the whole structure modification),
  or a dedicated NTA log record. Both are larger, recovery-sensitive changes
  beyond this workstream's phase-1 budget. The measured convoy (BTREE-LOCK-SCOPE:
  400/400 page-0 waiters) remains real and worth the phase-2 investment.
