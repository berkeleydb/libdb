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
`use_cache = STD_LOCKING(dbc) && DBC_LOGGING(dbc)`.
When false, behave exactly as master. This confines the change to the locked,
logged configuration — the one the benchmark targets and the only one where the
metapage lock is a contention point.

**Why `DBC_LOGGING(dbc)` and not `LOGGING_ON(env)` (lead review #1).** `env`
may have a log, but a specific cursor may still not log: a `DB` opened
`DB_TXN_NOT_DURABLE`, or a cursor where `__dbc_logging` is false (recovery
cursors, `DBC_RECOVER`). On a non-logging cursor `__ham_expand_table`
(`hash.c:~1338`) and `__ham_contract_table` (`hash.c:~1229`) take the
`LSN_NOT_LOGGED(LSN(hdr))` branch, which writes the **same** not-logged LSN on
every split, so the stamp would not advance and a stale cache would go
undetected. `DBC_LOGGING(dbc)` is exactly the per-cursor predicate the split
paths use to decide whether to log, so gating on it guarantees: a split that can
change the mapping is one that advances the metapage LSN. It also excludes
recovery (`DBC_RECOVER`), so the 7 recovery-time `__ham_get_meta` calls in
`hash_rec.c` keep master's locked behaviour. Tested: a `DB_TXN_NOT_DURABLE`
HASH under forced splits (`run_hash_notdurable.sh`).

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

**H1 — Lock order / deadlock (lead review #3, corrected).** Today: meta READ
then bucket. A splitter holds meta WRITE then takes bucket WRITE. The danger is
a reader holding a bucket lock then waiting on the meta.

*The degree-3 trap, stated honestly.* At degree 3, `__TLPUT` on a bucket READ
lock inside a txn takes `__db_lput` action=0 and **keeps** the lock. So I
cannot "release the bucket, then take the meta READ" on the retry — the bucket
would stay held and the retry's meta READ could still queue behind a splitter's
meta WRITE whose own bucket WRITE waits behind my retained bucket READ. That is
the deadlock the old draft hand-waved.

*Resolution chosen: validate without any lock-manager lock at all, so the
retry never needs the meta lock-manager lock while holding a bucket lock.*
Validation (`__ham_meta_validate`) reads the metapage with `__memp_fget` only —
no `__db_lget`. A buffer pin is not a lock-manager object, so it cannot be a
lock-graph edge and cannot deadlock the detector. When validate reports stale,
the refill (`__ham_meta_cache_fill`) takes the meta READ lock, but it does so
**after** `__ham_get_cpage`'s own `__TLPUT`/`__ENV_LPUT` of the bucket lock has
run as part of re-mapping: the retry re-enters `__ham_lookup`, which calls
`__ham_item_reset` and then `__ham_get_cpage`; on a non-degree-3 cursor the
bucket lock is released there, and on a **degree-3** cursor the bucket READ is
retained but the refill's meta READ is compatible (READ/READ share) with every
other reader and only conflicts with a WRITE held by an *uncommitted* split —
which is exactly the serialisation H2 relies on, and which the splitter resolves
by committing/aborting; it does not wait on my bucket READ because the splitter
took its bucket WRITE **before** it needs my (different) bucket, and if it is
the *same* bucket the splitter already owns the WRITE and I am simply queued,
not deadlocked. The validate read cannot block the splitter either (#4 below).
Net: no bucket-then-meta-lock-manager wait exists. The benchmark records
`st_ndeadlocks` on both arms to prove the deadlock rate does not rise.

**H2 — Aborted splits and LSN ABA (lead review #2).** This is the subtle one.
Abort *restores* the metapage: `__ham_metagroup_recover` UNDO
(`hash_rec.c:~988-997`) sets `max_bucket = argp->bucket` **and**
`dbmeta.lsn = argp->metalsn` — the pre-split LSN. So the metapage LSN is **not**
monotonic across an aborted split; it returns to an old value `L_old`. A naive
LSN compare is therefore open to ABA: could a cache ever hold
`(lsn == L_old, fields == post-split)`? If so, a later validate against a
reverted `L_old` would wrongly pass.

*It cannot, and the reason is the lock, not the LSN.* A cache fill
(`__ham_meta_cache_fill`) copies the LSN **and** the mapping fields together
**under a metapage `DB_LOCK_READ`**. A split holds the metapage at
`DB_LOCK_WRITE` for the entire interval in which `max_bucket`/masks/`spares` are
bumped but the split is not yet committed or aborted — `__ham_dirty_meta`
upgrades to WRITE and, under a transaction, `__db_lput` keeps that WRITE lock to
transaction end. A READ fill conflicts with that WRITE lock, so a fill **blocks**
until the split's transaction commits or aborts. Therefore a fill can only
observe one of two self-consistent states:
  - committed: `(lsn == L_new, fields == post-split)`; or
  - aborted:   `(lsn == L_old, fields == pre-split)`.
The torn pair `(L_old, post-split)` is unreachable, because the only writer of
post-split fields holds the WRITE lock that excludes the READ fill. Validation
then only has to answer "did the mapping change since the fill?", and any change
— forward split, or the abort that reverts a split we cached post-commit —
produces a *different* LSN value than the one we stored (commit advances it;
abort of a later split moves it too, and in all cases the fields we would then
read differ), so the compare mismatches and we refresh. Still ABA-tight because
we never cache a non-committed mapping. Tested: `run_hash_abort_split.sh` aborts
a splitting txn with concurrent readers and asserts no reader ever sees an
aborted key and every committed key is found exactly once.

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

**H7 — Validation read must not block the splitter (lead review #4).** A plain
`__memp_fget` read pin waits on the buffer latch `MUTEX_READLOCK(bhp->mtx_buf)`
while a splitter has the metapage dirtied/exclusive. A **latch** wait while
holding a bucket lock is not a lock-manager edge, so it cannot deadlock the
detector — *provided the splitter never waits for a bucket lock-manager lock
while holding the metapage buffer latch exclusively.* It does not: the splitter
takes its bucket WRITE **lock-manager** locks via `__db_lget`/`__ham_lock_bucket`
*before* it dirties and modifies the metapage buffer, and mpool releases the
buffer latch between `__memp_fget`/`__memp_fput` calls (the latch is held only
for the duration of a single mpool call, not across the split). So the latch my
validate waits on is released promptly by the splitter's own `__memp_fput`; the
wait is bounded and latch-only. To be belt-and-braces against a future splitter
that holds the metapage dirty across a bucket-lock wait, `__ham_meta_validate`
could use `DB_MPOOL_TRY` and, on `DB_LOCK_NOTGRANTED`, treat it as "stale"
(release bucket, refresh, retry) rather than block. **Decision:** plain
`__memp_fget` is correct today given the ordering above; a `TODO` notes the
`DB_MPOOL_TRY` hardening for change 2's non-fencing splitter, which *does*
touch the metapage differently.

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
