# RFC 0003: Serializable Snapshot Isolation (SSI)

- **Status:** Implemented
- **Type:** Normative
- **Author:** libdb maintainers
- **Date:** 2026-08-03
- **Prototype:** design/porting notes in [`rfc/0003/`](0003/); reference
  prototype on the `v4.6.21-SSI` tag (Cahill's SIGMOD-2008 research build)

---

> **Amendment (2026, post-implementation):** the public API was simplified. SSI
> is no longer a separate `DB_TXN_SNAPSHOT_SAFE` flag — that flag was **removed**
> and its behavior folded into **`DB_TXN_SNAPSHOT`**, which is now always
> serializable. There is no separate plain (non-serializable) snapshot-isolation
> mode in the public API. This is a deliberate ABI break, accepted to avoid the
> awkward `_SAFE` flag name. Everything below describing `DB_TXN_SNAPSHOT_SAFE`
> now applies to `DB_TXN_SNAPSHOT`; the internal `TXN_SNAPSHOT_SAFE` state and
> the SSI machinery are unchanged.

## Summary

The `DB_TXN_SNAPSHOT` transaction mode provides full
serializable isolation on top of MVCC snapshot isolation, using Michael
Cahill's Serializable Snapshot Isolation algorithm: detect the dangerous
read/write dependency structures that let snapshot isolation admit
non-serializable schedules, and abort the pivot transaction with
`DB_SNAPSHOT_CONFLICT`.

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
- On-disk/log/region/ABI: no on-disk or log format change. `DB_TXN_SNAPSHOT`
  is now the SSI mode (the separate `DB_TXN_SNAPSHOT_SAFE` flag was removed — a
  deliberate ABI break); `prepare()`/2PC rejects an SSI transaction.

## Design

Both of Cahill's rw-conflict detection paths are implemented:

1. **Lock-table path** — a concurrent writer meeting a committed reader's
   SIREAD marker in the (partitioned) lock region.
2. **MVCC version-chain path** in `mp_fget` — a reader handed an older version
   than one a concurrent writer committed.

SIREAD markers are reclaimed incrementally (not only at checkpoint).

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

> **Known limitations (2026-09, from external reports #137–#140).** The claims in
> this RFC describe the *intended* design; the delivered behavior is weaker in
> ways confirmed by outside review. Until the fixes land with regression tests,
> treat the serializability guarantee as **best-effort, not absolute**:
>
> - **#137 / #138 — marker reclamation is not fully bounded.** SIREAD cleanup
>   does not reclaim the deferred committed-reader locker, and
>   `__txn_reap_si_details` frees a transaction detail without releasing its MVCC
>   mutex. Long-lived environments running many snapshot transactions can
>   therefore exhaust the mutex region and see `ENOMEM`.
> - **#140 — lock-list sizing.** `DB_LOCK_SIREAD` was not accounted for in the
>   replication commit lock-list sizing (a heap overflow in release builds).
>
> This section is removed only when each item is fixed *and* covered by a test.

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
