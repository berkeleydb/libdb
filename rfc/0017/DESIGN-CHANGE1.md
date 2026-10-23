# RFC 0017 change 1 — implementation design

*Written before coding, per the checkpoint protocol. Facts are from
`/scratch/libdb/rfc17-STATE.md` and a focused source read; see that file for
line numbers already verified.*

## The problem, restated precisely

Every HASH cursor operation calls `__ham_get_meta` (`src/hash/hash_meta.c`):

1. `__db_lget(dbc, 0, meta_pgno, DB_LOCK_READ, …, &hcp->hlock)` — a
   lock-manager READ lock on the metapage;
2. `__memp_fget(meta_pgno, …, &hcp->hdr)` — pin the metapage buffer.

`hcp->hdr` (an `HMETA *`) is then read by `__ham_call_hash` (the
`high_mask`/`low_mask`/`max_bucket` key→bucket map, `src/hash/hash.c:1421`) and
by `BUCKET_TO_PAGE` (the `spares[]` bucket→page map, `src/dbinc/hash.h:108`).
`__ham_release_meta` puts the buffer and `__TLPUT`s the lock; under a txn at
degree 3 the READ lock is **kept to transaction end** (`__db_lput` action=0).

A split (`__ham_expand_table`, `hash.c:1200`) calls `__ham_dirty_meta`, which
upgrades `hlock` to `DB_LOCK_WRITE` with `LCK_COUPLE`, held through
`__ham_split_page`. That WRITE lock conflicts with every reader's metapage READ
lock, so a split under a transaction fences the whole hash DB until commit.

## The change: cache the mapping, validate by metapage LSN, no lock in the common path

### What is cached (in-memory only, no format change)

Add to `HASH` (`dbp->h_internal`, `src/dbinc/hash.h`) a per-handle cache:

```
struct { u_int32_t valid; DB_LSN lsn;
         u_int32_t max_bucket, high_mask, low_mask;
         u_int32_t spares[NCACHED]; } cache;  /* NCACHED == 32 */
```

`max_bucket`, `high_mask`, `low_mask`, `spares[]` are exactly the fields
`__ham_call_hash` and `BUCKET_TO_PAGE` read. `lsn` is the metapage LSN at the
moment the cache was filled.

### The staleness stamp: the metapage LSN (no region change, no on-disk change)

Every logged split dirties and logs the metapage, so `LSN(metapage)` advances on
every split. The metapage buffer is reachable with `__memp_fget` alone — **no
lock-manager lock**. So the staleness check is: read the metapage buffer
(unlocked fget), compare its LSN to `cache.lsn`.

- Equal → the mapping has not changed since the cache was filled → the cached
  fields are exact → map and lock the bucket with **no metapage lock-manager
  lock at all**.
- Different → refresh: take the short READ lock (as today), copy the four
  fields and the LSN into the cache, release the READ lock immediately
  (`__LPUT`, not `__TLPUT`), and re-map.

This is why the LSN beats RFC option (a)'s MPOOLFILE counter: it needs **no
region-format change** (`__env_struct_sig` unchanged) and **no on-disk change**.
It is strictly better than (a) and is the implementation target.

#### Non-logging / non-transactional environments

If the environment does not log (no `DB_INIT_LOG`, or `DB_TXN_NOT_DURABLE`, or a
non-transactional `DB`), metapage LSNs may not advance on a split. Two sub-cases:

- **`!STD_LOCKING(dbc)`** (no locking subsystem, e.g. `DB_PRIVATE` without
  locking, or `DB_INIT_CDB` degrades differently — see CDB below). There is no
  concurrency the cache must defend against beyond a single writer; but to be
  safe we **do not use the cache fast-path when the env does not log**: we fall
  back to the existing locked `__ham_get_meta`/`__ham_release_meta`. This keeps
  the non-logging `nelem++` path (`hash_page.c:2669`, `hash_dup.c:653`) exactly
  as it is.

- **Logging but non-transactional cursor** (autocommit off, `txn == NULL`, but
  the env logs): splits still log and still bump the metapage LSN, so the LSN
  stamp is valid. The fast path applies.

Decision rule, computed once per operation:
`use_cache = STD_LOCKING(dbc) && LOGGING_ON(dbc->dbp->env)`.
When false, behave exactly as master. This confines the change to the locked,
logged configuration — the one the benchmark targets and the only one where the
metapage lock is a contention point.

### Where validation happens

The map-then-validate sequence lives at the two map sites, driven by a new
helper `__ham_get_meta_cache(dbc)` that fills `hcp` with a *cache view* instead
of a locked `hdr`:

1. Operation entry computes `use_cache`. If false → `__ham_get_meta` as today.
2. If true → `__ham_get_meta_cache` ensures the handle cache is filled
   (refreshing under a transient READ lock if empty), and leaves **no** metapage
   lock held and **no** `hcp->hdr` pin. `__ham_call_hash`/`BUCKET_TO_PAGE` are
   taught to read the cache when `hcp->hdr == NULL` (see "Reads of hcp->hdr").
3. Map key → bucket → page from the cache; `__ham_get_cpage` locks the bucket
   page (`__ham_lock_bucket`) exactly as today.
4. **Validate**: re-read the metapage buffer unlocked; if its LSN differs from
   `cache.lsn`, the mapping changed under us — release the bucket lock, refresh
   the cache under the short READ lock, and retry from step 3. Retries are
   bounded: a given bucket splits at most ⌈log₂(final buckets)⌉ times, and each
   refresh observes a strictly newer LSN, so the loop terminates.

### Why validation after the bucket lock is sufficient (the linear-hashing property)

Linear hashing (this code): a bucket's primary page number is fixed by
`spares[__db_log2(bucket+1)]` once that page exists. A split only ever (a) adds
a new higher bucket, (b) bumps `max_bucket`/masks, (c) moves some records from
an existing bucket to the new one. It never renumbers or frees the page of a
bucket that a stale cache could name. So a stale `max_bucket`/mask can only send
an operation to a bucket that has **since split** — i.e. the page still exists
and still holds the subset of records whose key did not move. The LSN mismatch
catches exactly that case and forces a refresh+retry, after which the key maps
to the correct (possibly new) bucket. A stale cache can never name a freed or
unrelated page.

## Hazard-by-hazard safety argument

**H1 — Lock order / deadlock.** Today: meta READ then bucket. A splitter holds
meta WRITE then takes bucket WRITE. The danger is a reader holding a bucket lock
then waiting on the meta.
*Resolution:* the refresh never waits on the meta while holding a bucket lock.
On an LSN mismatch we **first release the bucket lock** (`__ENV_LPUT`/`__TLPUT`
of `hcp->lock`), **then** take the transient meta READ lock. The validate read
itself uses `__memp_fget` only (no lock-manager lock), so it cannot block on the
splitter's meta WRITE. Thus no reader ever holds bucket-then-waits-on-meta, and
the degree-3 "cannot release bucket READ to retry" problem (`__db_lput` keeps
it) does not arise, because the retry releases the bucket *before* the txn has a
reason to keep it — the retry happens inside the same operation before the
bucket lock is recorded as held-to-end. (If a prior operation in the same txn
already holds that bucket READ to end, the LSN check still returns the right
page, because that bucket's own page number is immutable; we just may re-lock
the same object, which is a no-op upgrade.)

**H2 — Aborted splits.** A split's `max_bucket`/mask/`spares` are uncommitted
until the splitter's txn ends; abort reverts them *and* the metapage LSN (abort
is logged, so the metapage LSN changes again on abort). A reader that mapped
through a cache taken mid-split: the splitter holds the two buckets' WRITE locks
through the relocation, so our bucket lock in step 3 blocks until the splitter
commits or aborts. When we get the bucket lock, the split is fully done or fully
undone, and our LSN validate (step 4) sees the post-commit / post-abort LSN,
mismatches our pre-split cache, and we refresh to the committed truth. The
splitter's bucket WRITE locks are exactly what make this correct — unchanged.

**H3 — The 7 `__ham_get_meta` reader sites** (`hash.c` lines 111, 203, 384,
491, 801, 1006, 1076). Each becomes: compute `use_cache`; fast path or fall
back. The sites and their ops:
- lookup/get (`__hamc_get`), insert (`__hamc_put`), delete, `__ham_c_count`,
  `__ham_bulk`'s meta use, cursor adjustment — all read-map then bucket-lock, so
  all follow the map-then-validate shape.
- `DB_READ_UNCOMMITTED`: still bucket-locks (dirty-read downgrade is on the
  bucket lock, not the meta), LSN validate unaffected.
- `DB_RMW`: takes WRITE on the bucket; the meta was only ever READ, so dropping
  the meta lock does not change RMW's write intent on data.
- `DB_TXN_SNAPSHOT`/MVCC: MVCC versions live on the bucket/data buffers via
  mpool; the metapage is not MVCC-versioned and the mapping fields are
  monotonic, so a snapshot reader mapping through the cache still reaches a page
  whose MVCC chain it then reads normally.
- secondaries: a secondary HASH is just another HASH `DB`; same path.

**H4 — `__ham_contract_table` / reverse split / `__ham_reclaim`.** Contraction
lowers `max_bucket` and frees the top bucket's page under meta WRITE + that
bucket's WRITE lock, and logs the metapage (LSN advances). A stale cache that
still names the freed top bucket: step 3 blocks on that bucket's WRITE lock
held by the contractor; when released the page may be freed. **This is the one
case where a stale map could name a freed page**, so for HASH with contraction
the validate must happen *before* dereferencing bucket contents — which it does:
`__ham_get_cpage` fetches the page after the lock, and we validate the LSN right
after the lock and before using the page. On mismatch we refresh and the lowered
`max_bucket` maps the key to a live bucket. Contraction is already rare and
already serialises on meta WRITE; we add no new window.

**H5 — Subdatabases / `revision` / `__db_reopen`.** `__ham_get_meta`'s subdb
revision check and `__db_reopen` path is preserved verbatim inside the refresh
(the only place that takes the meta READ lock and reads the HMETA). The cache
view does not bypass it: a refresh goes through the same code, so a subdb whose
metapage moved still triggers `__db_reopen`. We also invalidate the handle cache
(`cache.valid = 0`) on `__db_reopen` and whenever `revision` changes.

**H6 — CDB / `DB_INIT_CDB` and non-locking envs.** `use_cache` is false when
`!STD_LOCKING` (CDB uses its own lock model, not fine-grained page locks the
same way) OR when the env does not log. CDB and non-logging keep master's exact
locked path, including the `!STD_LOCKING` `nelem++` under `__ham_dirty_meta`.

## Scope of edits

- `src/dbinc/hash.h`: add the `cache` struct to `HASH`; teach `BUCKET_TO_PAGE`
  to read the cache when `hdr == NULL`.
- `src/hash/hash_meta.c`: add `__ham_get_meta_cache`, `__ham_validate_meta`
  (LSN check), `__ham_cache_invalidate`; `__ham_get_meta`/`release` unchanged.
- `src/hash/hash.c`: `__ham_call_hash` reads the cache when `hdr == NULL`; the 7
  sites compute `use_cache` and branch; `__hamc_put` split path refreshes/
  invalidates the cache after `__ham_expand_table`.
- `src/hash/hash_page.c`: `__ham_lock_bucket` already fetches meta if absent;
  add the post-lock LSN validate in `__ham_get_cpage`'s bucket path, guarded by
  `use_cache`.
- No change to `src/dbinc/db_page.h` (on-disk), `src/dbinc/mp.h` (region), log
  records, or `dist/s_*` generated files.

## Fallback decision (checkpoint protocol)

If, during implementation, the LSN stamp proves unsafe (e.g. a split path that
does not advance the metapage LSN, or an abort that does not re-log the
metapage), STOP, record the counter-evidence here, and fall back to RFC option
(a) (MPOOLFILE split-generation counter, `REGION-SIG-CHANGE-INTENDED` + upgrade
note) only with numbers justifying it. First choice remains the LSN: zero
format and zero region impact.

## Change 2 (design only — not implemented here)

Non-fencing split needs new per-bucket page-header markers ("being split",
"being populated", per-record "moved"), new `hash.src` log records, a new hash
format version, `db_verify` support, and recovery that completes or ignores a
partial split by reading the markers. It is a new on-disk format and a large
recovery surface; it stays Prospective and is **not** built here. Findings from
change-1 work that inform change 2 are appended to the RFC's change-2 section.
