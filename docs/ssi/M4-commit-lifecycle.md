# M4 design note — faithful (A) SIREAD lifecycle across commit

Chosen approach (A): SIREAD markers persist past the reading transaction's
commit, so conflicts against an already-committed reader are still detected
(full Cahill SSI). This requires the reader's `TXN_DETAIL` and `DB_LOCKER` to
outlive its commit, because each SIREAD marker dereferences them through
`LOCK_OWNER` (status / read_lsn / visible_lsn).

## The reference-counting model

Master already extends `TXN_DETAIL` lifetime past commit for MVCC: a committed
snapshot txn with `mvcc_ref > 0` is parked on `region->mvcc_txn` (flag
`TXN_DTL_SNAPSHOT`) and freed by the mpool when its last page is evicted.

We add a **parallel reference**: `TXN_DETAIL.si_ref` counts outstanding SIREAD
markers naming this detail. The detail is freed only when **both** `mvcc_ref`
and `si_ref` reach zero. Symmetrically, the `DB_LOCKER` is kept (flag
`DB_LOCKER_FREED`) until its last SIREAD marker is gone.

## Lifecycle

1. **Acquire (reader):** granting a SIREAD marker increments the owner's
   `td->si_ref` (the marker names the reader's own detail).
2. **Commit (`__lock_sicommit`, before `PUT_ALL`):** detach the txn's SIREAD
   markers from the locker `heldby` list so normal release does not free them;
   they stay on each object's `sireaders` list. If any remain, mark the locker
   `DB_LOCKER_FREED`.
3. **`__txn_end`:** if `si_ref > 0`, retain `td` (do not `__env_alloc_free` it);
   it is already on no active list, so it simply stays allocated, pinned by the
   markers. (`mvcc_ref` retention is unchanged and composes with this.)
4. **`__lock_freelocker_int`:** if `DB_LOCKER_FREED` and the locker still has
   SIREAD markers, defer — return without freeing.
5. **GC (`__lock_sicleanup` / `__lock_siclean_obj`):** for a committed reader
   whose snapshot is older than `__txn_oldest_reader`, remove the marker from
   `sireaders`, free the lock struct (no `UNLINK` — it is already off `heldby`),
   `td->si_ref--`, and locker `nlocks--`. When a locker's markers reach zero and
   it is `DB_LOCKER_FREED`, free it (deferred to after the partition mutex is
   released). When `td->si_ref == 0 && mvcc_ref == 0`, free the detail via a
   txn-region helper (cross-subsystem free, mirroring how mpool frees MVCC
   details today).

## Lock ordering (unchanged from M2 note)

`__txn_oldest_reader` (takes the txn system lock) is computed before any
partition mutex. Lockers and details are freed after the partition mutex is
released. No partition→txn-system or partition→mtx_lockers nesting is
introduced.

## Why this is the highest-risk area for M5

Two subsystems (lock GC and mpool eviction) can each hold the last reference to
a `TXN_DETAIL`. The M5 campaign must stress concurrent commit + eviction +
SIREAD GC to prove there is no use-after-free or double-free of details/lockers.
