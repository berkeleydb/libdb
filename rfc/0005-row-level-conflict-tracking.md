# RFC 0005: Optional row-level (key-level) SSI conflict tracking

- **Status:** Draft
- **Type:** Prospective
- **Author:** libdb maintainers
- **Date:** 2026-09-14
- **Tracking:** follow-up to RFC 0003's own condition ("reduce abort rate under
  contention (finer-grained conflict tracking; see `test/bench`)"); raised in
  review by Michael Cahill, author of the SIGMOD-2008 SSI paper RFC 0003
  implements
- **Prototype:** none yet; `rfc/0005/` reserved for the measurement spike

---

## Summary

libdb's SSI (`DB_TXN_SERIALIZABLE`, RFC 0003) tracks read/write
antidependencies at **page** granularity, because the lock object it hangs
SIREAD markers on *is* a page (`{fileid, pgno, type}`) and because the MVCC
version chain it consults is a **buffer header**, i.e. also a page. Two
serializable transactions that touch *different keys on the same leaf page*
therefore produce a phantom rw-edge and, if the edge pattern closes, a **false
abort** — an abort with no serializability anomaly behind it. This RFC proposes
making conflict tracking **optionally key-precise**, evaluates three designs
(SSI-side key-precise read sets, true row-level lock objects, and hybrid
escalation), and states the phantom-prevention, region-sizing, and
deadlock-detector consequences of each. It commits to measured evidence
(`test/bench/ssi_abort_bench`) before acceptance, and it is explicitly willing
to conclude that the win does not justify the complexity.

## Motivation

### The false abort

Under page granularity the recorded conflict relation is coarser than the real
data dependency. Concretely, in `__lock_get_internal`
(`src/lock/lock.c:1043`), a `DB_LOCK_WRITE` acquirer walks
`sh_obj->sireaders` — every SIREAD marker on **that page object** — and records
`R --rw--> W` for each still-relevant reader. Nothing in that walk knows which
*key* either party touched: the marker carries a holder, a mode and an object
offset (`struct __db_lock`, `src/dbinc/lock.h`), not a key. Mechanism (b),
`__memp_si_rwconflict` (`src/mp/mp_fget.c:114`), is the same shape one layer
down: it walks the **page's** version chain (`SH_CHAIN_NEXT(..., vc, __bh)`) and
records an edge to the owner of every newer version the reader skipped — again
regardless of which key in the page changed.

So for a reader of key *k* and a writer of key *k'* with *k ≠ k'* but
`pgno(k) == pgno(k')`, libdb records an edge that a row-granularity SSI
implementation (PostgreSQL's, for instance) would not. Enough such phantom
edges and a transaction becomes a "pivot" (both `TXN_DTL_RCONF` and
`TXN_DTL_WCONF`) and is aborted at its commit check (`src/txn/txn.c:817`) with
`DB_SNAPSHOT_CONFLICT`, or aborted by a peer with `DB_SNAPSHOT_UNSAFE`. That
abort is *sound* (aborting is always safe) but not *necessary*. A user migrating
from a row-SSI system will see a higher abort rate than their workload's true
conflict graph implies. Cahill's own framing: this tension is inherent to
retrofitting SSI onto a page-locking engine, and he hit it too.

### Direct evidence that page granularity forces conflicts

`test/bench/ssi_abort_bench.c` is the strongest evidence in the tree, and it
is evidence *by construction*: the benchmark could not be made to measure SSI
at all until it was reshaped around page granularity. Its header comment and
constants record exactly that:

- The workload is a write-skew **ring**: worker *i* writes key `i*SP` and reads
  key `((i+1) mod N)*SP` — reads and writes are key-disjoint, so a
  row-granularity engine would see only the intended rw-edges.
- The spread `SP` is forced to `SSI_MIN_SPREAD = 8`, and the comment says why:
  *"the btree packs ~3 records per leaf, so a spread of 8 comfortably lands
  every worker's write key on a DISTINCT leaf page -> no page-granularity write
  conflict."* Values are padded (`SSI_PAD 200`, `SSI_PAGESIZE 1024`) to get
  ~3 records per leaf, and deliberately kept *inline* — the comment notes that
  letting them overflow *"would collapse all the pointers onto one leaf page and
  reintroduce page-granularity conflicts, defeating the whole design."*
- If the spread falls below `SSI_MIN_SPREAD`, the run prints
  `[SPREAD TOO SMALL -- deadlocks expected]` and the numbers are declared
  meaningless: adjacent keys on one leaf page turn a key-disjoint schedule into
  a page-level **ww** conflict, and the deadlock detector aborts the
  transactions before the SSI pivot fires.

That is the false-conflict effect, quantified in the only way an existing
harness quantifies it: *keys must be spread ≥ 8 apart in key order — not merely
named apart — for a logically key-disjoint workload to behave as key-disjoint.*
Any real workload with a hot, dense key range gets the opposite of that
treatment for free.

We do **not** yet have a number for "false aborts as a fraction of aborts" on a
realistic workload. Producing that number is the first deliverable of the
measurement plan below, and no design here should be accepted without it.

### Where this was already written down

RFC 0003 shipped with this as a known limitation ("Page-granularity conflict
tracking can raise abort rates under contention … Still **experimental** in that
sense") and closed with the follow-up condition this RFC responds to. The user
documentation (`docs_src/guides/gsg_txn/isolation.md`) tells applications that
`DB_TXN_SERIALIZABLE` may fail with `DB_SNAPSHOT_UNSAFE` /
`DB_SNAPSHOT_CONFLICT` and must be retried like `DB_LOCK_DEADLOCK` — true, but
it does not tell them the abort rate carries a page-shaped tax.

## North-star check

The proposal is **optional and additive** by construction:

- **Opt-in, two levels.** A per-transaction flag (a new public
  `DB_TXN_ROW_CONFLICTS` bit, or an additive `DB_ENV->set_flags` default
  mirroring how `DB_TXN_SERIALIZABLE` was added at `0x00200000`) and/or a
  per-DB flag at `DB->set_flags` time. Neither `DB_TXN_SNAPSHOT` (plain SI) nor
  `DB_TXN_SERIALIZABLE` changes meaning; a transaction that does not ask for
  key-precision gets today's code path, byte for byte.
- **No on-disk or log format change.** SSI state is in-memory bookkeeping (RFC
  0003: "SIREAD markers are in-memory bookkeeping, not logged state"). Nothing
  proposed here is logged. The one log-adjacent surface is the replication
  commit lock list (`__lock_fix_list` / `__lock_get_list`,
  `src/lock/lock_list.c`), which serializes **write** locks only
  (`IS_WRITELOCK`, the #140 fix) — read-set representation never enters it.
  Design (b) must keep any new lock object *the same size* as
  `DB_LOCK_ILOCK` for a second reason given under Design.
- **Region format.** Option (a) can be done with **no** `DB_LOCKREGION` /
  `DB_LOCKOBJ` layout change (see Design (a)). Options (b)/(c) may add fields;
  those are shared-region layout changes, and the only thing standing between a
  new binary and an old region today is the build-version check in
  `__env_attach` (`renv->majver`/`minver` vs `DB_VERSION_MAJOR`/`MINOR`,
  `src/env/env_region.c:253`) plus the `DB_REGION_MAGIC` sanity test
  (`src/dbinc/db.in:2278`, checked at `src/env/env_region.c:290`) — there is no
  finer region-layout version. So a `DB_LOCKREGION`/`DB_LOCKOBJ` layout change
  must either ride a version bump that those checks reject a mismatch on, or add
  its own explicit region-layout version. **Open: which.** Environments must not
  silently mix.
- **Multi-process correctness.** Everything new lives either (i) entirely in
  per-transaction process-local memory *and is therefore not visible to a peer
  process* — which is a correctness constraint, not a convenience: a
  cross-process rw-edge must still be recordable, so any process-local read-set
  representation must be a *filter* applied on top of the shared-region edge
  discovery, never a replacement for it — or (ii) in the shared lock region
  under the existing partition-mutex / `LOCK_LOCKERS` / `TXN_SYSTEM_LOCK`
  ordering documented in `rfc/0003/M2-partition-design.md` and
  `M4-commit-lifecycle.md`. No new lock ordering is introduced by any option
  here; that is a hard review condition, because the SSI marker/locker lifetime
  is where RFC 0003 found its use-after-free family.
- **Recovery.** Unchanged. SSI does not participate in recovery; recovery
  reacquires **write** locks from the commit record
  (`__lock_get_list`, `src/txn/txn_rec.c:230`) and never reconstructs a read
  set. An SSI transaction still cannot be `prepare()`d.
- **Access methods.** Key-precision applies where keys exist and MVCC applies:
  B-tree/Recno (and Hash, at the bucket level, with the caveat in Risks). Queue
  and Heap keep page/record behavior. No access method regresses, because none
  changes unless the flag is set.
- **ACID / embedded / no-server.** Untouched. Aborting *less* never weakens
  isolation as long as the retained edge set is still a superset of the true
  dependency edges — which is precisely what the phantom question below is
  about, and precisely why this RFC cannot be accepted on the strength of a
  lower abort rate alone.

## Design

### Where page granularity is baked in

Read this list as the blast radius. Every entry was verified in the tree.

1. **The lock object *is* a page.** `DB_LOCK_ILOCK`
   (`src/dbinc/db.in:445`) is `{db_pgno_t pgno; u_int8_t fileid[20];
   u_int32_t type;}`. `__db_cursor_int` (`src/db/db_am.c:227`) points
   `dbc->lock_dbt` at `&dbc->lock` with `size = sizeof(dbc->lock)` and
   `type = DB_PAGE_LOCK`; `__db_lget` (`src/db/db_meta.c:1148`, the single
   entry point for every access-method lock — 86 call sites in 25 files across
   `src/btree`, `src/db`, `src/hash`, `src/heap`, `src/qam`) sets
   `dbc->lock.pgno = pgno` and hands that DBT to `__lock_get`/`__lock_vec`.
   The lock manager itself is granularity-agnostic: it hashes and `memcmp`s
   opaque bytes (`__lock_getobj`, `src/lock/lock.c:2001`). **The page-ness lives
   in the caller, not in the lock manager.** That is the single most important
   fact for this RFC.
2. **The object-hash fast path is keyed on the *size* of that struct.**
   `__lock_ohash` / `__lock_lhash` (`src/lock/lock_util.c:51,66`) use
   `FAST_HASH` only `if (dbt->size == sizeof(DB_LOCK_ILOCK))` — a 4-XOR of the
   page number against the fileid — and otherwise fall through to
   `__ham_func5` over the whole object. A wider lock object silently leaves the
   fast path on **every** lock get in the environment.
3. **A larger lock object also leaves the inline object buffer.**
   `DB_LOCKOBJ` carries `u_int8_t objdata[sizeof(struct __db_ilock)]`
   (`src/dbinc/lock.h`), and `__lock_getobj` uses it when
   `obj->size <= sizeof(sh_obj->objdata)`; otherwise it calls `__env_alloc`,
   taking `LOCK_REGION_LOCK` when the table is partitioned
   (`src/lock/lock.c:2044-2060`). So an oversized lock object converts a
   partition-local allocation into a **region-global** one on the lock-get path.
4. **SIREAD markers hang off the page object.** `sireaders` is a list head *in*
   `DB_LOCKOBJ` (`src/dbinc/lock.h:149`); the marker is inserted at
   `src/lock/lock.c:1348` and the edge-recording walk is
   `src/lock/lock.c:1058`. Object reclamation is blocked while markers remain
   (`__lock_put_internal`, `src/lock/lock.c:1872`) — a marker pins its page
   object, its locker (`DB_LOCKER_FREED`, `src/lock/lock_id.c:516`) and its
   `TXN_DETAIL` (`si_ref`).
5. **Marker GC sweeps the whole object table.** `__lock_sicleanup`
   (`src/lock/lock.c:209`) iterates `region->object_t_size` buckets and every
   object in each, under each partition mutex in turn. Its trigger is
   `nsireaders > st_objects / SI_CLEANUP_TRIGGER_DIV` (=8) at `txn_begin`
   (`src/txn/txn.c:71,326`). **Marker count and object count both feed this
   loop**, so any granularity change directly changes GC cost and the sawtooth
   ceiling that issue #137 was closed against.
6. **MVCC conflict detection is per buffer header = per page.**
   `__memp_si_rwconflict` (`src/mp/mp_fget.c:114`) is called from `__memp_fget`
   (`src/mp/mp_fget.c:369`) with the *visible* `BH` and walks its version chain.
   mpool has no key: at that call site the key has not been compared yet
   (`__bam_search`'s comparison loop runs *after* `__memp_fget` returns the
   page). Making mechanism (b) key-precise therefore cannot be done in mpool at
   all — it must move to, or be filtered at, the access-method layer.
7. **Snapshot reads take a SIREAD instead of a read lock, per page.**
   `__db_lget` (`src/db/db_meta.c:1184-1194`) rewrites
   `mode = DB_LOCK_READ` to `DB_LOCK_SIREAD` and sets
   `DB_LOCK_SNAPSHOT_SAFE` for an SSI transaction. Every leaf page a cursor
   touches gets a marker; a scan over *n* pages leaves *n* markers, one per
   page, never one per key.
8. **Lock coupling discards read locks but not markers.** `LCK_COUPLE`
   (`__db_lget`, `__db_lput`, `src/db/db_meta.c`) releases the previous lock as
   the cursor descends/advances. For SSI the marker must survive the couple —
   it does, because `__lock_sicommit` detaches markers from the locker rather
   than releasing them. Any per-transaction key-list must have the same lifetime
   discipline, and it is the part most likely to be got wrong.
9. **There is already a precedent for a non-page lock object, and it is
   instructive.** Queue takes `DB_LOCK_RECORD` locks (`src/qam/qam.c:252` etc.)
   by passing the *record number* through `__db_lget`'s `pgno` argument and
   setting `dbc->lock.type = DB_RECORD_LOCK` (`src/db/db_meta.c:1239`). The
   struct — and therefore the fast hash and the inline `objdata` — is
   **unchanged**; only the interpretation of two fields differs. This is the
   shape any row-level object should imitate.
10. **Statistics and diagnostics assume page-ness cosmetically.**
    `__lock_printlock` (`src/lock/lock_stat.c:747`) prints
    `page/record/database/handle` and formats `pgno` as a page number;
    `__lock_list_print` (`src/lock/lock_list.c`) likewise. Also
    `__db_has_pagelock` / `__db_haslock` (`src/db/db_meta.c:1364,1331`,
    `DIAGNOSTIC` only, asserted from `src/mp/mp_fget.c:1261`) construct a page
    ilock to verify a held lock — a row-granularity path must keep that
    assertion true or teach it the new identity.
11. **Mode enumerations.** Any new `DB_LOCK_*` mode changes `db_lockmode_t` and
    the `nmodes × nmodes` conflict matrix (`db_riw_conflicts`,
    `src/lock/lock_region.c:21`, `DB_LOCK_RIW_N` = 10), and trips
    `dist/cocci/lockmode_inventory.sh` by design (issue #140,
    `rfc/0003/lock-mode-audit.md`). **A strong preference of this RFC: add no
    new lock mode.** Reuse `DB_LOCK_SIREAD` and change only the object identity.

### The phantom problem — read this before any option

A key-precise read set **does not, by itself, preserve serializability.** This
is the classic trap and it is worth being blunt about, because every option
below can be implemented in a way that looks correct, passes `ssi001`–`ssi011`,
lowers the abort rate, and is wrong.

Today, page granularity prevents a whole class of phantom anomaly *by
accident*: an `INSERT` of a brand-new key takes `DB_LOCK_WRITE` on the leaf page
it lands in (`SR_INSERT` implies `SR_WRITE`, `src/dbinc/btree.h:151`), that
`DB_LOCK_WRITE` acquisition walks the page's `sireaders`
(`src/lock/lock.c:1043`), and a reader that scanned that page — even though it
never saw the new key, because the new key did not exist — gets the rw-edge.
Coarse tracking is *over*-approximate, and over-approximation is what makes the
edge set a superset of the truth.

A read set recorded as "the exact keys I read" loses that: the inserted key was
never read, so there is no edge, so a scan-then-write anomaly can commit. To
keep serializability, a key-precise design **must** record predicates, not
points, for anything that is not a single-point equality read:

- **Range / cursor scans** must record an *interval*, and a writer must test
  interval containment, not key equality.
- **Point reads that miss** (`DB_NOTFOUND`) must record the *gap*, i.e. the
  key's insertion position — classic next-key locking. A read of an absent key
  followed by another transaction inserting it is a real anomaly.
- **Splits** relocate keys between pages; an interval expressed in key space
  survives a split, an interval expressed in `(pgno, indx)` does not (see (b)).

Any option that cannot express "the reader depended on the *absence* of keys in
this range" must fall back to page granularity for that read. The honest
formulation is: **key-precision is an optimization applicable to reads whose
dependency is genuinely a point; every other read must remain
over-approximate.** An implementation that fails to make that distinction is a
correctness regression disguised as a performance win.

### Option (a) — key-precise SSI edges only; lock protocol untouched

**Idea.** Keep the lock manager exactly as it is: page locks, page objects,
`DB_LOCK_SIREAD` markers on page objects, unchanged conflict matrix, unchanged
deadlock detector. Additionally record, per transaction, *which keys* it read on
each page, and *which keys* it wrote. When the SSI machinery is about to record
`R --rw--> W` on a page object, first ask: *do R's reads on this page and W's
writes on this page actually intersect?* If provably not, skip the edge.

This is the smallest change that addresses the actual complaint, because the
false abort is an **SSI artifact**, not a locking artifact: the page *lock* is
doing its job (it serializes physical page access, and read locks are not even
taken under SI), while the page-shaped *edge* is what is over-approximate.

**Where it hooks.**
- Mechanism (a): in the `sireaders` walk at `src/lock/lock.c:1058`, between
  finding a marker and setting `TXN_DTL_RCONF`/`TXN_DTL_WCONF`, consult the
  reader's and writer's per-page read/write key summaries.
- Mechanism (b): **cannot** hook in `__memp_si_rwconflict` (no key at that
  layer, item 6 above). Two sub-options, both real work: (i) move the
  version-chain check to the access-method layer, called after the key
  comparison in `__bam_search`/`__bamc_get` with the found index in hand; or
  (ii) leave mpool recording a *provisional* edge and filter it at commit
  against the reader's key summary. (ii) is simpler and preserves the existing
  atomicity discipline (`TXN_SYSTEM_LOCK` around the flag RMW) but it must not
  re-open the #136 commit-window race: the filter has to run *inside* the same
  critical section that publishes `TXN_DTL_SICHECKED` (`src/txn/txn.c:817`), or
  a peer will resolve an edge the pivot has already filtered away. This is the
  single most dangerous detail in option (a).

**Representing the read set compactly.** Three candidates, in increasing cost:

| Representation | Space | Precision | Verdict |
|---|---|---|---|
| Per-txn key list, bucketed by `(fileid,pgno)` | O(keys read) | exact | fine for point-read workloads, unbounded for scans |
| **Per-page fingerprint** (small Bloom filter / hash bitmap in the marker or in the locker) | O(1) per page | one-sided: no false negatives, some false positives | **preferred**: a false positive keeps today's behavior (an edge that might be phantom), a false negative would be a *missed anomaly* — so the error direction is safe by construction |
| Key-space interval set | O(ranges) | exact, and expresses gaps/next-key | required for scans and `DB_NOTFOUND` reads; strictly more machinery |

A per-page fingerprint is the attractive middle: it needs no new region
structure if it fits in spare bits reachable from the marker, it is a pure
filter (so multi-process correctness is preserved — a peer that cannot read
another process's private read set simply does not filter, and records the edge,
i.e. falls back to today's behavior), and its failure mode is a false abort
rather than a lost abort. **But** it only serves point reads; ranges must
degrade to page granularity or use the interval set. Mixing the two per read
type is the design work.

**Costs.** No region layout change (if the fingerprint rides in existing space
or in process-local memory), no lock-object explosion, no deadlock-detector
change, no new lock mode. Abort-rate win limited to the false edges that a
one-sided filter can prove absent — i.e. bounded above by "false aborts caused
by disjoint keys on a shared page", which is exactly the quantity the
measurement plan must produce first.

### Option (b) — true row-level lock objects

**Idea.** Make the lock object itself finer: `{fileid, key-identity, type}`.

**Two identities, and the choice is not close.**
- `{fileid, pgno, indx}` — cheap to produce (the index is in hand at
  `BT_STK_ENTER`, `src/btree/bt_search.c:1176`), but **invalidated by a page
  split**: `__bam_ca_split_func` (`src/btree/bt_curadj.c:553`) rewrites live
  cursors' `pgno`/`indx` when a page splits, and no equivalent fixup exists — or
  is plausible — for lock objects already granted and hashed under the old
  `(pgno, indx)`. A held lock whose identity silently starts naming a different
  row is a correctness hole, not a performance question.
- `{fileid, key-hash, type=DB_ROW_LOCK}` — **stable across splits** (the key
  does not move in key space), and, crucially, expressible *without changing the
  struct*: put the key hash in the `pgno` field and use a distinct `type`, the
  way Queue already puts a record number there (item 9). The object stays
  `sizeof(DB_LOCK_ILOCK)`, so `FAST_HASH` still applies (item 2) and `objdata`
  still absorbs it (item 3). Collisions are false conflicts (safe direction);
  the hash need only be good, not invertible, since recovery/replication
  reacquire opaque bytes.

**Costs, honestly.**
- **Lock-object count explodes.** Today one object serves ~3 records (that
  number is the bench's own figure for its config; it is a function of page size
  and record size, not a constant). Row identity multiplies live objects by the
  records-per-page factor for every locked row. `st_objects` /
  `set_lk_max_objects` (→ `region->stat.st_maxobjects`,
  `src/lock/lock_region.c:210`) must be resized accordingly. There are two
  distinct failure modes and both are `ENOMEM` out of `lock_get`: the free-object
  pool exhausting (`__lock_allocobj` → `src/lock/lock_alloc.incl:137` →
  `__lock_nomem`, *"Lock table is out of available lock entries"*), and — only if
  the object is made larger than `objdata` — the region allocation for the object
  bytes failing (`__lock_getobj`, *"No space for lock object storage"*,
  `src/lock/lock.c:2058`). This is the same resource class as issues #137/#138:
  a statically sized region, a population that grows with workload shape, and a
  reclamation path that must keep up. `test/soak` exists precisely for that
  shape and must gate this.
- **Marker GC gets more expensive**, twice over: `__lock_sicleanup` walks all
  objects (item 5), and the `nsireaders > st_objects/8` trigger fires more often
  because a scan now leaves one marker per *key* instead of per *page*. The #137
  sawtooth ceiling and the ~17% soak-time improvement measured when
  `SI_CLEANUP_TRIGGER_DIV` was lowered from 2 to 8 (commit `bf5b53039`) were
  measured against page-granularity marker populations; both must be re-measured.
- **Deadlock detector.** Be precise about what does and does not grow.
  `__dd_build` (`src/lock/lock_deadlock.c:384`) allocates a
  `count × count` bitmap where `count = region->nlockers` — **locker**-shaped,
  not object-shaped, so the matrix does not grow. What grows is the walk over
  `region->dd_objs`, the list of objects **with waiters**
  (`src/lock/lock.c:1373`), and the per-object holder/waiter traversal.
  Row-level locking should *reduce* waiter counts (fewer real conflicts), so the
  net effect is plausibly favorable — but it is an empirical question, and the
  detector also runs on every conflict when `set_lk_detect` is on.
- **Two-level locking, or none.** Row locks alone do not protect page
  *structure*: a splitting or merging writer must still exclude readers of the
  page. Real row-locking engines pair row locks with page latches; libdb's page
  lock is doing both jobs today. Either keep the page lock *and* add a row lock
  (two lock gets per operation — measurable cost on every access, for everyone
  who enables it), or make page locks intention locks (`DB_LOCK_IREAD`/`IWRITE`
  already exist in the matrix and are used for handle locks) and take row locks
  underneath — a genuine change to the locking protocol, with a new lock-order
  argument to make and `test/lockmatrix` to satisfy.
- **`__lock_change`** (`src/lock/lock.c:2496`) moves all locks from one object
  to another for metadata-page moves, ordering the two partition mutexes by
  index. With row objects, "move the object" becomes "move *n* objects", which is
  not obviously expressible under the same two-mutex ordering.

### Option (c) — hybrid with escalation

**Idea.** Track row-level until a transaction's read set on one page exceeds
*N* rows, then escalate that page to a single page-granularity marker and drop
the per-row state. Classic, and it addresses the one unbounded cost in (a) and
(b): memory proportional to rows touched.

**Why it fits libdb specifically.** The escalation trigger has an obvious
natural value — escalate when the per-page row set reaches the point where a
page marker is *cheaper* than the row markers, i.e. roughly the records-per-page
factor. Below that, precision where it matters (point reads on hot rows); above
it, today's behavior (scans, bulk reads) with today's costs. It also bounds the
region-pressure and GC-sweep regressions that make (b) risky, which is the
strongest argument for it.

**Costs.** Escalation is a *widening* of the read set, so it is safe in the
edge-set-superset sense — but it must be atomic against a concurrent writer
recording an edge, or a window opens in which neither the row markers (already
dropped) nor the page marker (not yet installed) is visible, and an edge is
lost. That window is the same class of bug as #136 and must be closed by
construction (install the page marker before dropping the row markers, under
the object partition mutex), not by argument. Escalation policy also becomes a
tunable that users will get wrong, and a hysteresis question (do we ever
de-escalate? proposal: no — YAGNI until measured).

### Recommended sequencing

1. **Measure first** (below). If the false-abort fraction is small, stop; write
   the negative result into this RFC and reject it. The tree has precedent for
   parking measured negative results rather than shipping them
   (`test/bench/CROSS-ENGINE-2026-09.md`; the parked `perf/*` branches).
2. If it is large, prototype **(a)** with a per-page fingerprint for point reads
   only, ranges falling back to page granularity. It is the only option with no
   region-format change, no lock-protocol change, and no new failure mode in the
   region-exhaustion class.
3. Consider **(c)** only if (a)'s measured win is materially short of the
   row-SSI ideal. Consider **(b)** only with the key-hash identity, never the
   `(pgno, indx)` identity.

## Alternatives considered

- **Do nothing; document the tax.** Cheapest and not unreasonable: SSI is
  opt-in, aborts are already required to be retried, and the abort is sound.
  The cost is a user-visible surprise for people arriving from row-SSI systems.
  This is the baseline every option must beat *with numbers*.
- **Smaller pages as the user-space workaround.** Already available
  (`DB->set_pagesize`) and genuinely effective — it is exactly what
  `ssi_abort_bench` does (1024-byte pages plus padding to reach ~3 records per
  leaf). It costs tree height, I/O amplification and cache efficiency, and it is
  a blunt global knob rather than a property of the conflict tracking. Worth
  documenting regardless of this RFC's outcome.
- **Two-phase read locking for serializability.** Rejected in RFC 0003 for the
  reason SI exists: readers block writers. Unchanged here.
- **Full predicate locking.** The correct general answer to phantoms, and far
  larger than this proposal. If key-precision needs interval/next-key locking
  anyway (it does, for scans), a future RFC could argue for a proper predicate
  lock manager — but that is a new subsystem, not a refinement.

## Risks & open questions

1. **Phantom prevention is the blocking correctness question.** A key-precise
   read set silently loses the accidental phantom protection page granularity
   provides. Options (a)/(b)/(c) are only acceptable with an explicit rule for
   which reads may be key-precise and which must stay coarse, plus tests that
   *fail* if the rule is violated. **This must be resolved before this RFC
   leaves Draft.**
2. **Whether the win justifies the complexity.** Genuinely open, and this RFC
   is prepared to answer "no". libdb's SSI is a retrofit onto a page-locking
   engine; the false-abort rate may well be dominated by real conflicts in any
   workload dense enough to care. The measurement below decides it.
3. **Region sizing / exhaustion.** Options (b)/(c) multiply lock objects and
   SIREAD markers against a statically sized region. This is the #137/#138
   family: `__lock_nomem` on the lock-get path, and a GC sweep
   (`__lock_sicleanup`) whose cost is proportional to the object table. Any
   implementation must come with a `test/soak` workload showing the bounded
   sawtooth still bounds, and with guidance for `set_lk_max_objects`. The #137
   trigger constant (`SI_CLEANUP_TRIGGER_DIV = 8`) was tuned against
   page-granularity marker populations and will need re-tuning.
4. **Deadlock-detector cost.** The `nlockers²` bitmap does not grow with object
   count, but the `dd_objs` walk does, and detector frequency depends on
   conflict rate — which row-level locking should *lower*. Net effect unknown;
   `test/bench/lock_bench` must measure it.
5. **Escalation policy.** Threshold, atomicity of the escalation step, and
   whether de-escalation ever happens. The atomicity requirement is a
   correctness requirement, not a tuning knob.
6. **Mechanism (b) has no key at its call site.** `__memp_si_rwconflict` runs
   in mpool, before the key comparison. Either the check moves up into the
   access method (invasive, touches `bt_search`/`bt_cursor`) or the edge is
   recorded provisionally and filtered at commit inside the
   `TXN_DTL_SICHECKED` critical section (subtle, and adjacent to the #136 race).
   **Open: which.**
7. **Hash and Heap.** Hash locks the *bucket* page (`__ham_lock_bucket`,
   `src/hash/hash_page.c:2959`) and there is no key ordering, so intervals and
   next-key locking are meaningless there — key-precision for Hash can only ever
   be point-precise, and phantom inserts into a bucket must keep bucket
   granularity. Heap has no user key order either. **Open: whether the flag is
   rejected, or silently degrades to page granularity, for these methods.**
   Silent degradation is the safer default; a rejected flag is the more honest
   API. Undecided.
8. **Duplicate keys and off-page duplicates.** `DB_DUPSORT` sets and off-page
   duplicate trees (`DBC_OPD`, which `__db_lget` deliberately does *not* lock:
   `action != LCK_ALWAYS && F_ISSET(dbc, DBC_OPD)` returns early,
   `src/db/db_meta.c:1174`) make "the key I read" ambiguous. **Open.**
9. **Secondary indices / `DB_MULTIVERSION` interaction.** A secondary read
   resolves through the primary; the read set spans two databases and two
   `fileid`s. Believed to fall out naturally (each gets its own objects) but
   **not verified**.
10. **Diagnostics and operator visibility.** `__lock_printlock` and
    `db_stat -Cl` would print row objects as pages (item 10). If the object
    count and marker population become the operator's early warning for region
    pressure, the stat output needs to distinguish them — and
    `__db_has_pagelock`'s `DIAGNOSTIC` assertion (`src/db/db_meta.c:1364`,
    asserted from
    `src/mp/mp_fget.c:1261`) must be taught the new identity or it will fire.
11. **Public flag bit budget.** `DB_TXN_*` currently occupies
    `0x01,0x02,0x04,0x08,0x10,0x20,0x40,0x80` and `0x00200000`
    (`src/dbinc_auto/api_flags.in`). A new `DB_TXN_ROW_CONFLICTS` needs a bit
    that is free across every API that shares the `txn_begin` flag word; the
    `DB_TXN_SERIALIZABLE` addition is the template, and
    `dist/cocci/abi_flagbits.cocci` / `flagbits_inventory.sh` gate it.

## Prototype / evidence

**None yet, and that is the point of this section.** No claim in this RFC is
measured; the code-level claims are verified by reading, the abort-rate claim is
not yet quantified. RFC review requires measured evidence
(`rfc/README.md`: "performance evidence (measured, not asserted)"), so the RFC
commits to producing it *before* proposing acceptance.

### Measurement plan

**Step 1 — quantify the false-abort rate at page granularity (no engine
change).** `test/bench/ssi_abort_bench` already separates the two isolation
levels (`ISO_LEVEL=snapshot|serializable`) and reports `commit`, `ssi_abort`
(both `DB_SNAPSHOT_CONFLICT` and `DB_SNAPSHOT_UNSAFE`, via `IS_SSI_ABORT`),
`deadlock`, and `abort_rate`, with `deadlock ≈ 0` at both levels as its own
validity check. Sweep the **spread** rather than the thread count:

- `SP ≥ SSI_MIN_SPREAD` (8): write keys on distinct leaf pages — the *reference*
  abort rate, all of it genuine rw-ring pivots.
- `SP = 1, 2, 3` (dense keys, ~3 records per leaf in this config): the *same
  logical schedule*, still key-disjoint, now sharing leaf pages.

The delta between those two abort rates, at equal logical conflict structure, is
the page-granularity tax — the first real number this proposal needs. Because
dense spreads also reintroduce ww conflicts, the `deadlock` counter must be
reported alongside and the two effects separated (`ssi_abort` vs `deadlock`);
that separation is exactly what the reworked bench was built to do. A variant
that keeps `SP` large in key space but shrinks it in *page* space (larger
`SSI_PAGESIZE`, or unpadded values so more records pack per leaf) isolates the
SSI edge effect from the ww effect and should be added.

**Step 2 — A/B the same workload with row-level tracking.** Identical binary,
identical schedule, flag off vs on. Named comparison, gated:
`ssi_abort_bench` at `SP ∈ {1,2,3}`, `ISO_LEVEL=serializable`,
`row_conflicts ∈ {off,on}` — `ssi_abort` must fall materially, `commit` must
rise, `deadlock` must not rise, and the `--selfcheck` two-transaction write skew
must still be **prevented** with the flag on. That last one is the correctness
teeth: a "win" that also lets the write skew through is a broken
implementation, and the check is already in the harness.

**Step 3 — cost A/B (this is where the proposal most plausibly dies).**
- Throughput: `test/bench/run_bench.sh` fixed configuration, `lock_bench`
  (distinct and shared objects) and `scale_bench`, flag off vs on, compared with
  `bench_cmp.py` against the committed tolerances. The page-level path must
  show **no regression** with the flag off — that is a hard gate, not a
  tolerance.
- Region pressure: `DB_ENV->lock_stat` `st_nobjects` / `st_maxnobjects` /
  `st_objectsteals` / `st_nlocks`, and `mutex_stat` `st_mutex_inuse`, at both
  granularities, on the same workload. Row-level must not turn a working
  `set_lk_max_objects` into `ENOMEM`.
- Resource accounting: a `test/soak` workload (the #137/#138 tier) with the flag
  on, asserting the bounded sawtooth still bounds and that the peak is
  independent of transaction count.
- Isolation regression: `test/isolation` (write skew visible under
  `DB_TXN_SNAPSHOT`, prevented under `DB_TXN_SERIALIZABLE`) at **both**
  granularities, plus `ssi001`–`ssi011` including the `ssi009` multi-process
  stress, plus `test/lockmatrix` if any lock-protocol change is involved.

**Acceptance condition.** Step 1 must show a false-abort fraction large enough
to matter; Step 2 must show it actually shrinks *without* letting the write skew
through; Step 3 must show no regression on the page-level path and no new
region-exhaustion failure mode. If any of the three fails, the correct outcome
is a dated **Rejected** decision recording the number — which is a useful
result, not a wasted RFC.

---

## Decision

*(Filled by the reviewer when the RFC is decided.)*

- **Decision:** **Rejected — do not implement.** The Step-1 measurement was produced
  (`test/false-abort-rate`, 585 runs, 117 points x 5 reps) and it argues against
  the RFC rather than for it, and open question #1 (phantom prevention) has since
  been confirmed as a real trap by an independent implementation. Details below.
- **Rationale:** two independent findings, one internal and one external.

  **1. The measurement did not support the premise.** Holding the *logical*
  conflict graph empty and varying only physical co-location gives **11.37%
  aborts when keys share a leaf page and 0.000% when they do not**, with the rate
  spanning ~630x purely from records-per-leaf (0.000% at 512 B pages -> 11.35% at
  32 KB). So the false aborts are real and are page-granularity artifacts — but
  users already control the knob that removes them (`set_pagesize`), and on
  Zipfian workloads page-level write-write *deadlocks* exceed false aborts by
  roughly 3x (40.1% vs 12.8%), which key-precise *read* edges would not remove.
  A cheap existing knob beats a large new mechanism.

  **2. TidesDB independently built the key-precise design and hit exactly the
  phantom trap this RFC warned about.** TidesDB v10.0.1 (`d62d694`) implements the
  same Cahill dangerous-structure rule with key-precise read/write sets — the
  thing this RFC proposed — and gets the precision for free because an LSM has no
  shared leaf pages. Reading its source (verified, not inferred):

  - `src/txn/readset.h` records `(cf_index, key, key_size, seq)` — **points only,
    no interval or gap representation.**
  - `tidesdb_readset_record` is called from exactly one place, `txn_get_impl`
    (`src/txn/txn.c:339,347`) — i.e. **point gets only**. Iterators and range
    scans record nothing.
  - It *does* get the absent-read case right: a miss is recorded at the snapshot
    seq (`txn.c:339`), so a later insert of that same key is caught. That is the
    gap case for point reads, which this RFC lists as mandatory.
  - But with no predicate recorded for a scan, a **scan-then-write phantom can
    commit under its SERIALIZABLE level**, and the limitation is not documented
    in its public header.

  That is precisely the failure this RFC predicted: *"every option below can be
  implemented in a way that looks correct, passes `ssi001`-`ssi011`, lowers the
  abort rate, and is wrong."* An independent team, building the same design
  competently, shipped the over-approximation loss. This raises the estimated
  cost of doing option (a) or (b) *correctly* — predicates for every scan, plus
  next-key/gap handling — and correspondingly lowers the expected value, given
  finding 1 says the payoff is a knob users already have.

  Note the tension worth keeping in view: libdb's page granularity is what
  *causes* ~100% of its false aborts **and** what provides its phantom prevention
  (invariant **D10**). Those are the same mechanism. Removing the cost removes the
  protection, which is why this is not a local optimization.
- **Conditions / follow-ups:** none — the RFC is closed as *analysed and
  declined*, which is a first-class outcome here. If it is ever reopened, the bar
  is now higher and concrete: (i) a workload where `set_pagesize` demonstrably
  cannot recover the aborts, (ii) a predicate/interval read-set design that
  covers range scans and gaps, with a test that *fails* when the predicate is
  removed (the TidesDB shape is the negative example to test against), and
  (iii) an answer for the page-level ww deadlocks that dominate on Zipfian keys.
  Evidence: `test/bench/` false-abort data and `rfc/0010-global-invariants.md`
  invariant D10.
