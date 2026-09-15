# RFC index

The register of libdb design proposals. See [`README.md`](README.md) for the
process. Status: Draft · Accepted · Rejected · Superseded · Implemented.

| # | Title | Status | Type |
|---|-------|--------|------|
| [0001](0001-adaptive-lsm.md) | Adaptive LSM access method (HanoiDB + segment-policy, Bitcask, index-in-WAL) | Draft | Prospective |
| [0002](0002-buffer-swip-aio.md) | Scalable buffer access: tagged swip, optimistic descent, async I/O | Draft | Prospective |
| [0003](0003-ssi-serializable-snapshot-isolation.md) | Serializable Snapshot Isolation (SSI): `DB_TXN_SERIALIZABLE` flag, rw-antidependency detection (`DB_TXN_SNAPSHOT` remains plain SI) | Implemented | Normative |
| [0004](0004-funnel-sparse-hash.md) | Funnel-sparse HASH: bounded-probe overflow cascade, sparsehash-dense pages, sparsemap free-space | Draft | Prospective |
| [0005](0005-row-level-conflict-tracking.md) | Optional row-level (key-level) SSI conflict tracking instead of page-level (false-abort reduction) | Draft | Prospective |

<!-- Add a row per RFC. Keep the number 4-digit zero-padded and monotonic. -->

## Cross-subsystem design notes

Normative notes about the composition of already-shipped subsystems (not
proposals). See [`README.md`](README.md) § "Cross-subsystem design notes".

| Note | Status | Subject |
|------|--------|---------|
| [`docs/design/global-invariants.md`](../docs/design/global-invariants.md) | Normative | Global invariants across the lock manager (+SSI), txn region, mpool/MVCC, WAL, checkpoint, failchk, recovery, rsnap and cursor sharding — at a checkpoint, at a crash, at a region re-attach; the global lock order; the dangerous subsystem pairs; audit coverage and named gaps |
