# RFC 0017: HASH concurrency and online resize — drop the per-operation metapage lock, make bucket splits non-fencing

- **Status:** Draft
- **Type:** Prospective
- **Author:** libdb maintainers
- **Date:** 2026-10-22
- **Tracking:** HASH scalability; complements RFC 0004 (page structure). Branch `feat/rfc17-hash-concurrency`.
- **Prototype:** `rfc/0017/` (measurement plan, results, and the change-1 implementation)

---

## Summary

Berkeley DB HASH serialises on its metadata page. Every cursor operation takes
a lock-manager lock on the hash metapage (`DB_LOCK_READ`). Under transactional
locking, at degree-3 isolation, that lock is held until the transaction ends. A
bucket split (`__ham_expand_table`) upgrades the cursor's metapage lock to
`DB_LOCK_WRITE`. The write lock conflicts with every other cursor's read lock on
that page, and it is held through the whole split, record relocation included.
Under a transaction it is held until that transaction commits. As a result, two
things scale badly:

- concurrent operations contend on one lock object even when they touch
  different buckets;
- a split stops the whole hash database for as long as the splitting
  transaction stays open.

This RFC proposes two changes:

1. **Take the metapage lock only when the cached mapping might be stale**
   (format-compatible). Keep the bucket-mapping fields in a per-handle cache
   and validate them against the bucket page, so the common lookup or insert
   never locks the metapage.

2. **Make a bucket split non-fencing** (opt-in, new format version). Replace the
   whole-database `DB_LOCK_WRITE` with per-bucket split state, so a split
   blocks only the two buckets it touches.

Both are changes to concurrency and lock scope, not to page layout. They are
orthogonal to RFC 0004 (funnel cascade, dense pages), which reshapes the page
and the overflow chain. The two RFCs can ship independently or together.

## Motivation

The two problem shapes, read from the source on `master`:

- **Per-operation metapage lock.** `__ham_get_meta` (`src/hash/hash_meta.c`)
  calls `__db_lget(dbc, 0, meta_pgno, DB_LOCK_READ, …)` at the start of every
  lookup, insert and delete (`src/hash/hash.c` calls it from seven sites). It
  holds the lock in the cursor's `hlock` until `__ham_release_meta`, which
  calls `__TLPUT`, that is `__db_lput`.
  - Inside a transaction at degree 3, `__db_lput` takes `action = 0` for a
    read lock (`src/db/db_meta.c`), so the lock is **kept until the transaction
    ends**.
  - Read locks share, so read-only workloads are not blocked. But every
    operation acquires and releases a lock-manager object on one hot page,
    which costs partition-latch traffic per operation under `STD_LOCKING`, and
    every open transaction pins that page in read mode.

- **The split fences the whole database until commit.** When `H_EXPAND` is set
  (a bucket exceeded `ffactor`, decided per page by
  `H_NUMPAIRS(page) > ffactor` in `__ham_add_el`, `src/hash/hash_page.c`),
  `__hamc_put` calls `__ham_expand_table`. Its first act is
  `__ham_dirty_meta`, which upgrades `hcp->hlock` to `DB_LOCK_WRITE` with
  `LCK_COUPLE`. This write lock:
  - waits for every other cursor's read lock on the metapage to drain;
  - blocks every new cursor while it is held.

  **It is held through `__ham_split_page`** (the relocation of the bucket's
  records), which `__ham_expand_table` calls last. It is released only when
  `__hamc_put` reaches `__ham_release_meta` at `err2:`. Under a transaction,
  `__db_lput` keeps a write lock too (`action = 0`), so **the whole hash
  database is fenced until the splitting transaction commits or aborts**.

  The comment at the relocation step reads "after releasing metapage", but the
  lock released there is `metalock`. That is the *master* database's metapage
  (`PGNO_BASE_MD`), taken only when a subdatabase must allocate new pages. It
  is not the hash metapage. A split is therefore a global latency event whose
  length is the length of the inserting transaction, and it recurs every time a
  bucket splits.

- **The element count is not the bottleneck.** `__ham_add_el` bumps
  `hdr->nelem` only on the non-locking path:
  `if (!STD_LOCKING(dbc)) { __ham_dirty_meta(…); hcp->hdr->nelem++; }`. The
  transactional split decision is per page, not driven by a global counter. So
  the problem is the metapage *lock*, which is taken regardless of `nelem`. Any
  "batch the counter" fix would not apply.

The evidence this RFC must produce on libdb itself is described in
"Prototype / evidence". No number in this section is a measurement.

## North-star check

- **Embedded, no server, multi-process:** unchanged. Both changes stay inside
  the existing mpool and lock-manager model.

- **Change 1 is format-compatible.** It does not alter the on-disk metapage, any
  page layout, the log or the region. It changes when a lock is taken and adds
  a per-`DB`-handle in-memory cache of the mapping fields. The safety argument
  rests on a property of linear hashing that holds in this code:
  - a bucket's primary page number is fixed by `spares[]` once that page
    exists;
  - a stale cached `max_bucket` or mask can therefore send an operation only to
    a bucket that has since *split*, never to a freed or unrelated page.

  The validation step below detects that case and falls back to the metapage.

- **Change 2 needs a new format version and is opt-in.** It adds per-bucket
  split state and new log-record variants. So it is gated behind a create-time
  flag and a new hash format version, with `db_verify` support and no implicit
  upgrade of existing `DB_HASH` databases. RFC 0004 uses the same contract.
  **This is the hard review gate, and the reason change 2 is Prospective.**

- **ACID and crash recovery:** change 1 logs nothing new; it only moves a lock.
  Change 2's split must be crash-atomic: a sequence of logged page changes with
  an explicit split-in-progress marker that both recovery and concurrent readers
  understand.

## Design

### Change 1 — metapage lock only when the mapping might be stale

Today every operation does the following:

1. lock the metapage (READ);
2. read `max_bucket`, `high_mask`, `low_mask` and `spares[]`;
3. map the key to a bucket and lock the bucket;
4. do the work;
5. release the metapage at the end of the operation, or at commit under a
   transaction.

The change:

1. **Cache the mapping fields in the `DB` handle.** Add to `HASH` (the
   in-memory `dbp->h_internal`, not the on-disk meta) a cached `max_bucket`,
   `high_mask`, `low_mask` and a copy of `spares[]`, refreshed under a short
   metapage read lock that is *released immediately* (not transaction
   duration). `__ham_call_hash` and `BUCKET_TO_PAGE` read the cache.

2. **Validate after locking the bucket.** The operation maps the key with the
   cached fields, then locks and reads the bucket page. It must then confirm
   that the bucket has not split since the cache was taken.
   - If it has not, proceed without any metapage lock.
   - If it has, release the bucket, refresh the cache, and retry.

   Retries are rare and bounded: a given bucket can split at most about log₂ of
   the table's final size times.

3. **Where the "has it split?" fact lives** is the main open question (risk #1).
   A hash primary page has no dead header field: `prev_pgno` is live (it marks
   the primary page in the overflow chain), and the btree `level` byte is
   unused but only one byte wide. The options, in increasing cost:
   - **(a) No stamp.** Keep a per-environment split-generation counter in shared
     memory (the `MPOOLFILE`, beside the existing `revision`). Every split bumps
     it, and an operation compares it before and after with a single read.
     Invalidation is coarse (any split invalidates every handle's cache), but
     splits are rare compared with operations. No format change.
   - **(b) A one-byte split epoch in `level`.** Each split of a bucket
     increments it, and a cached per-bucket epoch is compared with it. The
     counter is narrow, so it has to wrap-check against (a).
   - **(c) A versioned header addition.** This makes change 1 ride along with
     change 2's new format version.

   Option (a) is the implementation target for change 1, because it is
   format-compatible and its correctness argument is simple.

4. **Lock duration.** The bucket-page lock keeps its present semantics. Only
   the metapage lock leaves the operation's lock set. That removes a
   transaction-duration read lock from every hash transaction (risk #2).

### Change 2 — non-fencing bucket split

The goal is to remove the metapage `DB_LOCK_WRITE` from the relocation, so a
split blocks only the old and new buckets.

- **Per-bucket split state.** Give the primary bucket page "being split" (old
  bucket) and "being populated" (new bucket) markers, plus a per-record "moved
  by split" marker, in page-header flag bits (new format version). A split sets
  the markers, relocates records from old to new under the two buckets' locks,
  then clears them.

- **Readers and inserters cope with a split in progress** instead of being
  fenced out. A scan of a being-populated bucket also scans the old bucket for
  records not yet moved. An inserter that meets a being-split bucket waits on
  that bucket only.

- **Publishing the new `max_bucket`/`spares[]` becomes a short step.** It is a
  brief metapage write, released straight away. It does not span the
  relocation, and with change 1 in place it does not fence other cursors,
  because they no longer hold the metapage.

- **Crash atomicity.** The markers bracket a sequence of logged changes.
  Recovery either completes a partial split or cleanly ignores it by reading
  the markers, and a reader during recovery sees the same old-plus-new union a
  live reader sees.

Deletes and reverse splits keep today's semantics. Bucket contraction
(`__ham_contract_table`) and `__ham_reclaim` must learn the same markers.

### Findings from the change-1 prototype that bear on change 2

Implementing and measuring change 1 (see `0017/DESIGN-CHANGE1.md` and
`0017/README.md`) produced three results that constrain change 2:

- **The metapage buffer latch, not just the lock-manager lock, is a hazard.**
  A splitter holds the metapage buffer latch (the page is dirtied) *and then*
  waits for a bucket WRITE lock-manager lock. Any reader that holds a bucket
  lock and then touches the metapage buffer — even without a lock-manager lock —
  can close a cycle the deadlock detector cannot see, because a latch is not a
  lock object. Change 1 had to read the metapage with `DB_MPOOL_TRY` and treat
  a contended latch as "refresh and retry". **Change 2's non-fencing splitter
  must publish the new mapping without ever holding the metapage buffer latch
  across a bucket-lock acquisition**, or readers will have the same problem
  against the new per-bucket markers.
- **Validate-and-retry has a real cost under heavy splitting.** Change 1 is a
  5–20% insert-throughput *regression* at high concurrency precisely because
  the retry storm under constant splits costs more than it saves. Change 2 must
  avoid a busy-retry: a reader that meets a being-split bucket should *wait on
  that bucket* (as the RFC says) rather than release-and-remap, so it makes
  progress without re-reading the mapping repeatedly.
- **The win change 1 did deliver — ~4 orders of magnitude fewer deadlocks and
  ~2× better tail latency — is the win change 2 should keep and extend.** The
  metapage WRITE fence is the root cause of both the deadlocks and the tail; a
  non-fencing split removes it at the source instead of working around it.

### Seams touched

- `src/hash/hash_meta.c`: `__ham_get_meta` / `__ham_release_meta` become
  "refresh the cache under a short lock"; the cache struct goes into `HASH` in
  `src/dbinc/hash.h`.
- `src/hash/hash.c`: `__ham_call_hash`, `__ham_expand_table`, `__hamc_put`
  (the `H_EXPAND` path), and the lookup, insert and delete entry points.
- `src/hash/hash_page.c`: `__ham_split_page` gains the per-bucket markers
  (change 2).
- Log records (change 2): new `hash.src` entries. The generated files come from
  `dist/s_*`; never hand-edit them.
- `hash_verify.c` / `db_verify`: validate the change-2 markers.
- `hash_upgrade.c`: nothing for change 1; change 2 is a new format version with
  no implicit upgrade.
- DST: a `BUGGIFY` point that stalls a split mid-relocation, so tests can assert
  that readers and recovery still see every live record. The existing
  `BUGGIFY_HASH_EXPAND_EARLY` in `hash_page.c` is the template.

## Alternatives considered

- **Leave HASH as it is and point users at B-tree under concurrency.** B-tree is
  the default and scales better today. But users choose HASH for point lookups
  on small keys, and change 1 in particular is cheap.

- **Partition the metapage lock into N stripes.** This reduces read-lock
  contention but keeps a whole-table write fence on split, because a resize has
  to take every stripe. Its ceiling is lower than cache-and-validate.

- **Incremental two-table rehash.** Keep the old and new tables and migrate a few
  buckets per operation. HASH is already incremental in the linear-hashing sense
  (one bucket per split, no second table). Per-bucket markers get the
  non-blocking property more directly, and a second table would be a much
  larger format and recovery change.

- **Extendible hashing with a directory split one segment at a time.** This is a
  structural overhaul and belongs with RFC 0004, not here.

- **Batching a global element counter.** It does not apply: the transactional
  path keeps no per-insert global counter.

## Risks & open questions

1. **Where the split-generation fact lives (change 1).** See Design step 3.
   Option (a), a counter in shared memory, is format-compatible but coarse. If
   measurement shows that coarse invalidation costs too much under heavy
   splitting, change 1 needs (b) or (c).
2. **Transactional lock footprint.** Removing the transaction-duration metapage
   read lock means fewer locks per hash transaction, which is the point. But
   anything that relied on that lock as an incidental serialisation point must
   be audited. That includes deadlock-detector behaviour, `DB_RMW`, cursor
   adjustment (`__hamc_update`), secondary indexes and the
   `DB_READ_UNCOMMITTED` / `DB_TXN_SNAPSHOT` / `DB_TXN_SERIALIZABLE` paths. The
   lock-order checker (RFC 0010) must be re-run.
3. **Stale-cache retry under concurrent splits.** The validation must be safe
   against a bucket splitting repeatedly between mapping and locking, and the
   retry must terminate. Test it under DST with `BUGGIFY_HASH_EXPAND_EARLY`
   forcing frequent splits.
4. **Split crash atomicity and recovery (change 2).** The markers add recovery
   states. This is the largest correctness surface, and it needs DST and
   `test/sim` coverage (cf. `test_sim_hash_crash.c`) before acceptance.
5. **Contraction and reclaim interaction.** A reverse split must not race a
   forward split.
6. **Measured evidence.** The RFC leaves Draft only with libdb measurements
   showing both the problem and the gain, with no regression elsewhere.

## Prototype / evidence

See [`rfc/0017/README.md`](0017/README.md) for the plan and, when produced, the
results. Before this RFC leaves Draft:

1. **Concurrency benchmark** (`test/bench`): T threads doing point inserts and
   lookups on one HASH database, transactional and non-transactional, at a
   controlled fill factor. It reports throughput against thread count and a
   latency histogram, with a B-tree control. The run is A/B-tested against the
   base on one host, with repeated alternating runs.
2. **Split-stall probe:** how long concurrent cursors wait on the metapage while
   a split is in progress, before and after.
3. **Correctness:** the `test/db` tier, the TCL hash tests, DST with
   `BUGGIFY_HASH_EXPAND_EARLY`, and a multi-process hash test, all green with
   the change, and the region signature unchanged for change 1.

---

## Decision

*(Filled by the reviewer when the RFC is decided.)*

- **Decision:** Pending — Draft.
- **Rationale:** —
- **Conditions / follow-ups:**
  - produce the `rfc/0017/` evidence on libdb;
  - resolve where the split-generation fact lives (risk #1) and the change-2
    recovery design (risk #4) before proposing acceptance;
  - keep RFC 0017 (concurrency/resize) and RFC 0004 (page structure)
    independently reviewable.
