# RFC 0003: Serializable Snapshot Isolation (SSI)

- **Status:** Implemented
- **Type:** Normative
- **Author:** libdb maintainers
- **Date:** 2026-08-03
- **Prototype:** design/porting notes in [`rfc/0003/`](0003/); reference
  prototype on the `v4.6.21-SSI` tag (Cahill's SIGMOD-2008 research build)

---

> **Amendment (2026, post-implementation):** the public flag naming was
> revised twice. SSI first shipped under a separate `DB_TXN_SNAPSHOT_SAFE`
> flag; that flag was then removed and SSI was folded into `DB_TXN_SNAPSHOT`
> (making every snapshot transaction serializable). That second step silently
> strengthened `DB_TXN_SNAPSHOT` and dropped legacy plain snapshot isolation,
> which was the wrong trade. The **final** design restores the original
> contract: **`DB_TXN_SNAPSHOT` means plain (non-serializable) snapshot
> isolation** exactly as legacy Berkeley DB, and **SSI is a new, additive
> public flag `DB_TXN_SERIALIZABLE`** (`= DB_TXN_SNAPSHOT` substrate plus the
> SSI conflict-detection layer). This is an additive ABI change, not a break:
> `DB_TXN_SNAPSHOT` keeps its value and semantics, and `DB_TXN_SERIALIZABLE`
> takes a previously-unused public flag bit. Everything below describing the
> SSI mode applies to `DB_TXN_SERIALIZABLE`; the internal `TXN_SNAPSHOT_SAFE`
> state and the SSI machinery are unchanged — they are simply no longer forced
> on for a plain `DB_TXN_SNAPSHOT` transaction. Environments can default to SSI
> with `DB_ENV->set_flags(DB_TXN_SERIALIZABLE, 1)`. *Migration:* code that
> relied on `DB_TXN_SNAPSHOT` meaning SSI must now pass `DB_TXN_SERIALIZABLE`.

## Summary

The `DB_TXN_SERIALIZABLE` transaction mode provides full
serializable isolation on top of MVCC snapshot isolation, using Michael
Cahill's Serializable Snapshot Isolation algorithm: detect the dangerous
read/write dependency structures that let snapshot isolation admit
non-serializable schedules, and abort the pivot transaction with
`DB_SNAPSHOT_CONFLICT`.  Plain `DB_TXN_SNAPSHOT` remains non-serializable
snapshot isolation and does not pay the SSI tracking cost.

## Motivation

Snapshot isolation is fast but not serializable — write skew and other
anomalies slip through. Cahill's SSI (2008) adds serializability at a small,
tracked cost by watching for rw-antidependency pivots rather than taking read
locks. libdb already had the MVCC substrate; SSI layers serializability on it
without a server and without giving up embedded operation.

## North-star check

- Embedded / no-server: unchanged (in-library, no coordinator).
- ACID: strengthens isolation (adds serializability); does not weaken A/C/D.
- Crash recovery: SIREAD markers are in-memory bookkeeping, not logged state;
  recovery is unaffected.
- Access methods: works for the MVCC-capable methods; others are unaffected.
- Multi-process correctness: SIREAD markers/lockers live in the shared lock
  region; the concurrent-writer lifetime is hardened (see the M2/M4 notes) and
  guarded by `ssi009` (multi-process stress).
- On-disk/log/region/ABI: no on-disk or log format change. `DB_TXN_SERIALIZABLE`
  is the SSI mode (an additive public flag; `DB_TXN_SNAPSHOT` keeps its legacy
  plain-SI meaning and value); `prepare()`/2PC rejects an SSI transaction but
  accepts a plain snapshot transaction.

## Design

Both of Cahill's rw-conflict detection paths are implemented:

1. **Lock-table path** — a concurrent writer meeting a committed reader's
   SIREAD marker in the (partitioned) lock region.
2. **MVCC version-chain path** in `mp_fget` — a reader handed an older version
   than one a concurrent writer committed.

SIREAD markers are reclaimed incrementally (not only at checkpoint), and
reclamation is bounded: `test/soak` asserts that region, mutex and locker
counts return to baseline over tens of thousands of sequential transactions
(issues #137, #138).

The commit-time pivot check is atomic with respect to conflict recording. Both
pivot flags are read under `TXN_SYSTEM_LOCK` — the mutex every recorder
(`__lock_get_internal`, `__memp_si_rwconflict`) takes around its flag
read-modify-write — and, in the same critical section, a passing check publishes
`TXN_DTL_SICHECKED` on the detail. That flag is what makes the window between
the check and `__txn_end`'s `TXN_COMMITTED` store safe: a recorder that arrives
during it sees that the committing transaction will not re-examine its flags and
resolves the edge itself (`DB_SNAPSHOT_UNSAFE`) instead of deferring to a check
that has already happened. Deferring on `status == TXN_RUNNING` alone was
issue #136 — a write skew where both transactions committed; `test/isolation`
gates it.

> **External review, 2026-09 (issues #136–#140) — all five fixed and gated.**
> An outside reviewer found five defects in the delivered implementation. Per the
> rule this section carried ("removed only when each item is fixed *and* covered
> by a test"), it is now retired; each item has a fix and a CI-gated regression
> test:
>
> | Issue | Defect | Fix | Gated by |
> |---|---|---|---|
> | **#136** | Write skew committed when the second writer's edge landed inside `__txn_commit` | `TXN_DTL_SICHECKED` published under `TXN_SYSTEM_LOCK` atomically with the pivot check (see above); mirror site `__memp_si_rwconflict` fixed too | `test/isolation` (hard gate) |
> | **#137** | Committed-reader lockers not reclaimed → mutex region exhausted (`ENOMEM`) | reader bookkeeping released on the deferred path | `test/soak` |
> | **#138** | `__txn_reap_si_details` freed a detail without `__mutex_free(&td->mvcc_mtx)` | mutex freed on the reap path | `test/soak` |
> | *(third leak)* | `si_ref` not decremented on a SIREAD→WRITE upgrade in `__lock_get_internal` | found during #137/#138 validation, not externally reported | `test/soak` |
> | **#140** | `DB_LOCK_SIREAD` uncounted in replication lock-list sizing → heap overflow | sizing and population share one `IS_WRITELOCK` predicate; `DB_ASSERT` promoted to `__env_panic` | `test/lockmatrix` |
>
> **Root-cause class, now guarded.** #140 existed because SSI added a lock mode
> without auditing pre-existing exhaustive mode enumerations. All 19 sites were
> audited (4 were wrong), and `dist/cocci/lockmode_inventory.sh` now fails CI if a
> new `DB_LOCK_*` mode appears without updating the inventory. See
> `rfc/0003/lock-mode-audit.md`.
>
> One reported symptom was **not** a separate defect: two records on different
> pages of one B-tree appeared to escape conflict detection entirely, but with two
> independent constructions (including 33 verified leaf pages) the control timing
> correctly returns `DB_SNAPSHOT_CONFLICT` — it was the same #136 commit-window
> race.

The two working notes in `rfc/0003/` are the porting/design record:

- **`M2-partition-design.md`** — porting Cahill's 4.6.21 single-global-lock-table
  SIREAD GC onto 5.3.x's *partitioned* lock regions (`OBJECT_LOCK_NDX` is now
  3-arg; `LOCK_PART` maps a bucket to a partition mutex). This is the area most
  likely to be subtly wrong, agreed before coding.
- **`M4-commit-lifecycle.md`** — the commit/GC lifecycle and the SIREAD
  marker/locker/detail lifetime under concurrent writers.

## Alternatives considered

- Classic two-phase read locking for serializability: correct but with the
  reader-blocks-writer cost SI was chosen to avoid. SSI keeps SI's read
  concurrency and pays only on genuine pivots.

## Risks & open questions

- Page-granularity conflict tracking can raise abort rates under contention
  (measured by the microbenchmarks under `test/bench`). Still **experimental**
  in that sense.
- HA/replication qualification is still being built.

## Prototype / evidence

The `v4.6.21-SSI` tag reproduces Cahill's original SIGMOD-2008 prototype
verbatim on 4.6.21. A family of concurrent-writer use-after-free bugs (most
importantly a lock object reclaimed while it still held SIREAD markers) was
found with TSan/ASan and fixed; `ssi001`–`ssi009` (Tcl) guard the behavior,
`ssi009` being the multi-process concurrent-writer stress test.

---

## Decision

- **Decision:** Accepted — 2026-08-03 (recorded retroactively; the feature is
  shipped on `master`).
- **Rationale:** serializability on top of the existing MVCC substrate with no
  format change and embedded operation preserved; a well-studied algorithm with
  a reproducible reference prototype.
- **Conditions / follow-ups:** reduce abort rate under contention (finer-grained
  conflict tracking; see `test/bench`); complete HA/replication qualification.
