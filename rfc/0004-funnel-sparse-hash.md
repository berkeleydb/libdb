# RFC 0004: Funnel-sparse HASH — bounded-probe overflow, dense pages, bitmap free-space

- **Status:** Draft
- **Type:** Prospective
- **Author:** libdb maintainers
- **Date:** 2026-09-14
- **Tracking:** revisit if/when HASH performance is re-opened (no implementation planned yet)

---

## Summary

Propose an **optional new hash access-method variant** that replaces Berkeley
DB's unbounded overflow-page *chain* with a **funnel-structured cascade of
geometrically-decreasing hashed levels** (Farach-Colton / Krapivin / Kuszmaul,
*Optimal Bounds for Open Addressing Without Reordering*, arXiv:2501.02305v2,
2025), packs each page's slots **sparsehash-style** (occupancy bitmap + a
densely-packed array of only the occupied entries, position→offset by
`popcount`), and drives insert placement / split decisions with a
**sparsemap-style page-resident free-space bitmap** (`rank`/`select`) instead of
walking pages. The combination gives BDB hash something it has never had — a
**worst-case probe bound** (O(log δ⁻¹) page probes instead of an unbounded
chain) — while raising the per-page fill factor so the fallback is reached less
often. It is proposed as a *new* access method, not a change to `DB_HASH`, so
the on-disk format contract is untouched.

## Motivation

BDB hash is **Litwin linear hashing**: a bucket array (`max_bucket`,
`high_mask`/`low_mask`, the `spares[]` segment directory), incremental
one-bucket-at-a-time splitting, fill factor `h_ffactor`. When a bucket's key set
exceeds a page, the overflow spills into a **chained list of overflow pages**
(`__ham_add_ovflpage`, `src/hash/hash_page.c`).

That chain is the dominant real-world weakness:

- **No worst-case bound.** A lookup or insert to an unlucky bucket walks the
  whole chain — **one random page I/O per overflow page**. Linear hashing's
  incremental split amortizes load *globally* but never bounds any *individual*
  bucket; a skewed or adversarial key distribution (or simply a bucket that got
  unlucky before its next split) produces a long chain and a long tail latency.
  There is, today, *no* worst-case probe guarantee for a BDB hash operation.
- **The tail is the pain.** Well-distributed keys at a good fill factor rarely
  overflow; the cost lands as p99/p999 latency on the skewed buckets, exactly
  the workloads where a user chose hash (point lookups on small keys) expecting
  O(1).

The 2025 result is directly relevant: it shows that, **without reordering**
(which BDB requires — reordering committed items would be a WAL/recovery
nightmare), a greedy open-addressed table can achieve O(log² δ⁻¹) worst-case
expected probe complexity — asymptotically better than the Θ(δ⁻¹) the field
assumed optimal. Its "funnel" construction is *page-shaped*: fixed-size buckets
(β slots) and independent geometrically-shrinking levels. That is a
drop-in-shaped replacement for "chain another overflow page."

## North-star check

- **New access method, not a change to `DB_HASH`.** Existing hash databases
  keep their exact on-disk format, code path, and behavior. The variant is
  opt-in at create time (a new access-method type / flag), with its own
  `db_verify` support, its own format version, and no implicit migration.
  Nothing about the current format contract moves.
- **No reordering.** The scheme is *greedy* (funnel, not the paper's non-greedy
  "elastic" variant): an item, once placed, never moves. This preserves the
  WAL/redo model — an insert is a single "place in slot" that logs and recovers
  like today's; there is no compaction-style relocation to make crash-atomic.
- **ACID / crash recovery / multi-process:** the page-resident bitmaps live in
  the page under the same latch/lock discipline as any hash page; level-cascade
  inserts are ordinary logged page modifications. Recovery redoes them exactly
  as it does bucket/overflow modifications today.
- **Embedded / no-server / footprint:** pure on-disk-structure change, no new
  process model, negligible code-size cost.
- **The hard gate — deletes.** See Risks: open addressing without reordering has
  a genuinely unsolved-here reclamation story. This RFC does **not** yet claim to
  meet the north star on delete/space-reclamation; that is the item that must be
  resolved before the RFC could move past Draft.

## Design

Three independent ideas, each addressing a different layer, composed:

### 1. Funnel cascade replaces the overflow chain (the worst-case bound)

Keep BDB's linear-hashed bucket array as **level A₁** (the backward-compatible
entry point). When a key does not fit its primary bucket page, instead of
chaining an overflow page, it cascades:

- The store is `α ≈ 4·log δ⁻¹ + O(1)` **levels** `A₁, A₂, …, A_α`, plus a small
  overflow region `A_{α+1}`, with `|A_{i+1}| ≈ (3/4)|A_i|` (geometrically
  decreasing). Each level is subdivided into fixed-size **buckets of β ≈
  2·log δ⁻¹ slots** — i.e. one bucket ≈ one page.
- **Insert**: hash the key to a bucket in `A₁`; if that bucket page has a free
  slot, place it. Else hash to a bucket in `A₂`, then `A₃`, … stopping at the
  first level whose hashed bucket has a free slot. If none of the α levels
  accept it, place it in the special overflow region `A_{α+1}`.
- **Lookup**: the same cascade, at most α probes — **O(log δ⁻¹) page reads,
  worst case**, versus today's unbounded chain.

`δ` here is the target free fraction (1 − load factor); the level count and
bucket size are chosen at create time from a target load factor. Levels are
allocated lazily (like `spares[]` segments today) so a small database costs
nothing for the deep levels.

This is the paper's Theorem 2 (funnel hashing) mapped onto pages: each "probe"
is a whole bucket (contiguous page), levels are independent, insertion is greedy
and non-reordering.

### 2. sparsehash-style dense pages (fewer keys ever reach the cascade)

Store each page's β slots as a **sparsegroup**: an occupancy bitmap plus a
densely-packed array of only the *occupied* `(key,data)` entries; the physical
offset of logical slot *i* is `popcount(bitmap & ((1<<i)−1))` (Google
`sparsehash`'s `sparsetable` trick). Benefits, aimed at the hash sweet spot of
**small keys**:

- Higher effective fill factor per page — BDB's current `inp[]` `(offset,len)`
  index pair per slot is significant overhead for short keys; the bitmap costs
  ~1 bit per empty slot instead. More keys fit in `A₁`, so the cascade is
  entered less often (fewer I/Os on the common path).
- "Is slot *i* free?" — the exact question the cascade asks at every level —
  becomes a **single-word bit test / popcount**, not a scan of `inp[]`.

### 3. sparsemap free-space bitmaps (jump to the right level, cheap splits)

Maintain a **page-resident free-space bitmap per level**, using the `sparsemap`
C library's wrap-a-fixed-buffer model (`sm_wrap`/`sm_init` over a region of the
segment/meta page) with `rank`/`select`/`span`. This is sparsemap's stated use
case — "allocation bitmaps for storage engines, free-list tracking." It turns:

- **Insert placement** from "probe level 1, 2, … reading and rejecting full
  pages" into a `rank`/`select` to the first level whose hashed bucket has a free
  slot — often a direct jump, avoiding dead-page reads.
- **Split / reverse-split accounting** from page walks into rank queries. Free
  space clusters (whole empty deep levels early in a database's life), which is
  exactly what sparsemap's RLE encoding compresses to a few bytes.

### Seams touched (for a future implementer)

- New access-method type/flag at `db_create`/`DB->open`; new meta-page fields
  for α, β, level segment directory, and the per-level free bitmaps.
- `src/hash/`: a parallel insert/lookup/delete path selected by the new type;
  new log records for level-cascade placement; `hash_verify`/`db_verify`
  support; `hash_upgrade` is a no-op (new type only, no in-place upgrade of
  existing `DB_HASH`).
- `sparsemap` vendored as a small C dependency (single header `sm.h`), or its
  rank/select re-implemented against BDB's page conventions.

## Alternatives considered

- **Just tune the existing chain (bigger pages, higher `h_ffactor`, more
  aggressive splitting).** Reduces chain frequency but never bounds worst case;
  the tail persists. This is the status quo's ceiling.
- **Cuckoo hashing / d-ary cuckoo.** Bounded lookup (2 probes) but *requires
  reordering* on insert (kick-outs) — disqualified by the WAL/recovery cost and
  the non-reordering constraint that makes the funnel approach attractive.
- **The paper's "elastic hashing" (non-greedy, O(1) amortized).** Better
  amortized bound, but non-greedy placement (probing far down the sequence then
  "snapping back") is harder to make crash-atomic and log cleanly. Funnel is the
  greedy, WAL-friendly choice; elastic is a possible later refinement.
- **Switch users to B-tree.** Already available; but hash wins on small-key point
  lookups when it avoids overflow — this RFC is about making hash keep that win
  under skew, not about abandoning it.

## Risks & open questions

1. **Deletes without reordering — THE open problem.** Funnel hashing as
   published is insertion-only at a fixed load factor. BDB must support delete +
   space reclamation. Open-addressing deletes need tombstones or a
   reclamation scheme that does not reorder committed items; naive tombstones
   degrade the probe bound over a delete-heavy workload. **This must be solved
   before the RFC can leave Draft** — it is the reason this is Prospective.
2. **Dynamic growth vs. fixed δ.** The paper assumes a known capacity and load
   factor; BDB grows by splitting. Mapping the α/β parameters onto a growing,
   segment-directory store (and re-choosing them as the DB grows) needs design.
3. **On-disk format + recovery surface.** New page layout, new log records,
   `db_verify`, and a fuzz/DST pass — non-trivial and gated by the north star.
4. **CPU vs. space tradeoff (sparsehash packing).** popcount + intra-page
   `memmove` on insert pays off only when key/data are small relative to `inp[]`
   overhead. Must be **measured** (`test/bench`) — the RFC review requires
   measured evidence, and the benchmark must target skewed / high-load
   distributions (where the win is) without regressing the well-distributed
   common case.
5. **Concurrency.** `sparsemap` is explicitly not thread-safe; a page-resident
   free bitmap must obey BDB's existing page latch/lock discipline. Designable,
   but real work, and TSan/lockmatrix must cover it.
6. **Where the win actually is.** This improves the *tail* (skewed/adversarial
   keys, high load factor), not necessarily the mean. The case for it stands or
   falls on measured p99/p999 on those workloads.

## Prototype / evidence

None yet. If revisited, a spike in `rfc/0004/` should: (a) micro-simulate funnel
vs. chain probe counts under skewed key distributions at several load factors
(no engine changes — a standalone model), and (b) measure sparsehash-style page
packing fill-factor gains for representative small-key sizes, before any engine
work. No claim here is measured; this RFC is a design to review, not a result.

---

## Decision

*(Filled by the reviewer when the RFC is decided.)*

- **Decision:** Pending — Draft, parked until HASH performance is re-opened.
- **Rationale:** —
- **Conditions / follow-ups:** resolve delete/reclamation-without-reordering
  (open question #1) and produce the `rfc/0004/` simulation evidence before
  proposing acceptance.
