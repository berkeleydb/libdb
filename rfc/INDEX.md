# RFC index

The register of libdb design proposals. See [`README.md`](README.md) for the
process. Status: Draft · Accepted · Rejected · Superseded · Implemented.

| # | Title | Status | Type |
|---|-------|--------|------|
| [0001](0001-adaptive-lsm.md) | Adaptive LSM access method (HanoiDB + segment-policy, Bitcask, index-in-WAL) | Draft | Prospective |
| [0002](0002-buffer-swip-aio.md) | Scalable buffer access: tagged swip, optimistic descent, async I/O | Draft | Prospective |
| [0003](0003-ssi-serializable-snapshot-isolation.md) | Serializable Snapshot Isolation (SSI): `DB_TXN_SERIALIZABLE` flag, rw-antidependency detection (`DB_TXN_SNAPSHOT` remains plain SI) | Implemented | Normative |
| [0004](0004-funnel-sparse-hash.md) | Funnel-sparse HASH: bounded-probe overflow cascade, sparsehash-dense pages, sparsemap free-space | Draft | Prospective |

<!-- Add a row per RFC. Keep the number 4-digit zero-padded and monotonic. -->
