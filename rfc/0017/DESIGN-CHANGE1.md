# RFC 0017 change 1: implementation design (v2)

The code is in `src/hash/hash_meta.c` (`__ham_get_meta_fast`, `__ham_fast_lock`)
and `src/hash/hash.c` (`__hamc_get`, `__hamc_put` and `__ham_lookup`). Results
are in `test/bench/rfc0017-ab-results.md`.

## The cost being removed

Every keyed hash get or put used to start in `__ham_get_meta`:

1. `__db_lget(meta_pgno, DB_LOCK_READ)` through the lock manager;
2. `__memp_fget(meta_pgno)` to read `max_bucket`, the masks and `spares[]`;
3. map key → bucket → page, lock the bucket, do the work;
4. `__ham_release_meta`. Under a degree-3 transaction `__db_lput` keeps the
   READ lock until commit, and `__txn_end` releases it there through
   `__lock_vec`.

Every operation on the database acquires and releases the same lock object.
A profile at 32 threads (perf, DWARF unwind, m5d.8xlarge) showed master
spending about 42% of lookup time in step 1 and about 42% in the matching
release at commit.

A split also upgrades the cursor's metapage lock to WRITE
(`__ham_dirty_meta`), which conflicts with every reader's READ lock. Inserts
that meet a split therefore deadlock or wait. Master aborts 200–2450
transactions per second this way in the insert benchmark.

## The fast path

A cursor keeps a **hint**: a copy of the metapage (`hcp->hdr_cache`, valid when
`hcp->hint_valid`), kept across operations and across cursor reuse. The hint is
never trusted on its own.

`__ham_get_meta_fast(dbc, key, mode)`:

1. If there is no hint, pin the metapage with `DB_MPOOL_TRY` and copy it.
2. Map key → bucket → page from the hint, and lock that bucket page through
   the lock manager. Nothing is read under this lock yet, so it is a probe.
3. Pin the live metapage with `DB_MPOOL_TRY`. This takes a buffer latch for a
   few instructions, never a lock-manager lock. Map the key again from the
   live fields.
4. Same bucket and page: the hint is current for this key. Proceed with the
   bucket locked and `hcp->hdr = &hcp->hdr_cache`. No metapage pin or lock is
   held for the rest of the operation.
5. Different page: copy the live fields into the hint, `__LPUT` the probe and
   retry, at most 4 times.
6. `DB_MPOOL_TRY` fails because the metapage is latched (a split is updating
   it): `__LPUT` the probe and return `DB_MPOOL_RETRY`. The caller then runs
   master's locked path for this one call.

`__ham_fast_lock` resets the cursor, calls the function above, and on success
sets `H_FASTLOCKED`. `__ham_lookup` then keeps the bucket lock instead of
re-mapping. `__ham_get_cpage` finds `lbucket == bucket` with the right mode
(its case 2) and only fetches the page.

## Why it is correct

**C1: the mapping is current for this key while the bucket lock is held.**
A key's page changes only when the key's bucket is split (records move to a new
bucket), contracted (merged into a lower bucket) or compacted (the page is
exchanged). Each of these write-locks the pages it changes:

- **Split.** `__ham_split_page` write-locks the old bucket. **This change also
  makes it write-lock the new bucket page.** Before, the metapage WRITE lock
  was the only fence on the new bucket. Fast-path operations no longer hold the
  metapage, so the new bucket needs its own lock until the split's transaction
  ends.
- **Contraction.** `__ham_merge_pages` write-locks both buckets.
- **Compaction.** `__ham_compact_hash` write-locks each page before
  `__db_exchange_page`, and holds the metapage WRITE lock while it changes
  `spares[]`.

The live re-map in step 3 happens after the bucket lock is granted. If it
agrees with the hint, no split or contraction of this bucket can be pending or
committed-but-unseen: one would have had to hold this bucket's WRITE lock, and
we now hold a conflicting lock on it. If one committed before our lock was
granted, the live fields already show it, so the re-map differs and we retry.

**C2: no wait on a latch while holding a bucket lock.** A splitter holds the
metapage buffer latch (the page is dirty) and may then wait for a bucket
WRITE lock. A blocking metapage read while holding a bucket lock would close a
cycle the deadlock detector cannot see, because latches are not lock objects.
The first implementation hung exactly this way on 2 processes × 2 threads.
Both metapage reads use `DB_MPOOL_TRY`, and a failed TRY releases the probe
before doing anything else.

**C3: dropping the probe inside a transaction is safe.** At degree 3,
`__TLPUT` keeps read locks until commit. The probe is released with `__LPUT`,
which really releases it. That is safe because nothing was read under it. If an
earlier operation in the same transaction already held that lock, the lock
manager incremented the hold's reference count (`__lock_get_internal`,
`lp->refcount++`). `__LPUT` then only decrements it
(`__lock_put_internal`), so the earlier hold survives.

**C4: an aborted split.** Readers that meet the split's WRITE locks wait for
commit or abort. On abort, the metagroup undo restores `max_bucket`, the masks
and `spares[]`, so the live re-map shows the pre-split mapping again. A reader
holding a post-split hint then finds a different page and retries.

**C5: what still uses the locked metapage.** These keep master's behaviour:
- scans (`DB_FIRST`/`DB_NEXT`/`DB_PREV`/`DB_LAST`), because a split during a
  scan could return a moved record twice;
- positional ops (`DB_CURRENT`, `DB_BEFORE`/`DB_AFTER`);
- bulk get, delete and count;
- anything excluded by `__ham_use_cache`: CDB or no locking, `DB_READ_UNCOMMITTED`,
  MVCC on the file (`MULTIVERSION`), recovery (`DBC_RECOVER`, `DB_AM_RECOVER`,
  `IS_RECOVERING`);
- a subdatabase whose revision changed (`__db_reopen`).

**C6: metapage writers on a fast-path cursor.** `__ham_dirty_meta` (split or
contract) and `__ham_return_meta` (`__db_new`/`__db_free`) detect
`HCP_META_CACHED` and take the real metapage with a WRITE lock first. The
cursor already holds its bucket lock, so this is bucket-then-metapage. That is
the order master's insert path has always used when it expands, because
`__hamc_put` holds the bucket when it calls `__ham_expand_table`.

**C7: fields read from the hint after step 4.** `__ham_add_el` reads `ffactor`
and `__ham_overwrite`/`__ham_dup_return` read `dbmeta.pagesize`. Both are fixed
at create time. `nelem` is written only when `!STD_LOCKING`, which never takes
the fast path.

## Evidence

- `test/db/run_hash_conc.sh`: 2 processes × 4 threads of inserts and lookups
  with forced splits (ffactor 4). Every committed key is found exactly once,
  no aborted key is visible, and `db_verify` passes.
  - **Sabotage arm:** `HASH_CONC_SABOTAGE=1` builds `hash_meta.c` with
    `RFC17_SABOTAGE_NO_VALIDATE`, which trusts a stale hint. 13697 of 13715
    committed keys go missing and `db_verify` fails.
- `test/db/run_hash_abort_split.sh`: readers run against a split that aborts.
- `test/db/run_hash_notdurable.sh`: `DB_TXN_NOT_DURABLE`, where the metapage
  LSN does not advance. v1's LSN stamp depended on it, and v2 does not.
- The full `test/db` tier on an `--enable-diagnostic` build (lock-order checker
  on): 43/43.
- Region signature unchanged: `HASH_CURSOR` is process-private and not in
  `env_sig.c`.

## v1, kept as a negative result

The first implementation filled a per-cursor copy under a metapage READ lock
on every operation, released at once, and validated it by comparing metapage
LSNs. It was slower than master for split-heavy inserts at 16–32 threads, for
two reasons:

- **The refill still took the metapage lock** through the lock manager on
  every operation. The cost stayed; only the hold time shrank.
- **The LSN changes on every split of any bucket**, so nearly every insert
  failed validation and retried.

Its benchmark also used a 512 MB cache for a database of about 1 GB, so it
mostly measured buffer eviction. Those numbers are kept, marked invalid, in
the results file.

## Change 2 (design only, not implemented)

A non-fencing split needs:
- per-bucket "being split" / "being populated" markers in the page header;
- new `hash.src` log records;
- a new hash format version;
- `db_verify` support;
- recovery that finishes or discards a partial split by reading the markers.

v2 already removes the metapage from the reader's path. What change 2 would add
is shorter bucket hold times during a split: today the old and new buckets stay
WRITE-locked until the splitting transaction commits. The measurements so far
do not show that cost to be large.
