<!--
Copyright (c) 2026 libdb contributors.  All rights reserved.
SPDX-License-Identifier: Sleepycat
-->

# RFC 0002 addendum: bounding the COOL-first eviction sweep (regression R3)

- **Status:** Implemented (branch `perf/evict-bound`)
- **Parent:** [RFC 0002](../0002-buffer-swip-aio.md) §4 (Stage 0 cooling), §11
  (probationary admission + COOL-first eviction)
- **Review item:** O-MP1 (`/scratch/libdb/review/B/mp-log-os.md` §1.4, ranked #1)
- **Regression:** R3 in [`test/bench/XREL-2026-10.md`](../../test/bench/XREL-2026-10.md)
- **Format impact:** none — pure eviction-policy heuristic in `src/mp/mp_alloc.c`.
  No region/log/on-disk/ABI change; no hashed struct grows. LTS-admissible.

## 1. The regression (measured)

Stage 0.5 (`dea815bb4`, RFC 0002 §11) made `__memp_alloc`'s CLOCK sweep
*COOL-first*: in the normal (non-`aggressive`) sweep a HOT-band buffer
(warmth `>= MPOOL_CLOCK_HOT`) is **skipped without aging**, a COOL buffer is
aged one step (second chance), and a warmth-0 buffer is the victim. The engine
only cools the HOT band — escalates to `aggressive` — once a **full sweep of all
`htab_buckets`** has found no COOL victim. That full-sweep proof is the
scan-resistance guarantee: while a COOL victim exists anywhere, the hot set is
never cooled to make room (RFC 0002 §11).

The cost of that proof is `O(htab_buckets)` **per allocation**, and it is paid
on every allocation whenever the cache is genuinely all-HOT. A bulk insert of
fresh dirty pages is exactly that case: each data page is `fput` many times as
it fills, climbing warmth to the ceiling (`MPOOL_CLOCK_MAX`), so every
unreferenced buffer is HOT. Each `__memp_alloc` then sweeps the whole table,
finds no COOL victim, escalates, and only then evicts in ~2 more buckets.

`test/bench/xrel_evict_probe.c` (`evict.tsv`), 1M sequential inserts, buckets
examined per allocation (5.3.28 → master):

| cache | btree ×5.3.28 | queue ×5.3.28 | buckets/alloc 5.3.28 → master |
|------:|-------------:|-------------:|------------------------------:|
| 1 MB  | 0.80 | 0.18 | 2 → 130 |
| 4 MB  | 0.73 | **0.05** | 2 → 460–520 |
| 16 MB | 0.73 | 0.79 | 0.3–1 → 80–275 |
| 64 MB (fits) | 0.85 | 0.85 | 0 → 0 |

5.3.28's timestamp-LRU evicted the coldest buffer in the first one or two
buckets. The COOL-first design turned that into a full sweep.

## 2. Why the full sweep is intrinsic to COOL-first — and where the waste is

The full-sweep-before-cooling-HOT rule is *correct*: when the cache really is
all-HOT there is no COOL victim and cooling the HOT band is the only way to make
progress. The waste is not the escalation; it is re-proving "all-HOT" from a
cold start, bucket by latched bucket, on every single allocation.

Two escalation triggers exist today (`mp_alloc.c`):

- line 240 `buckets++ == htab_buckets` — a **full sweep** found nothing; the
  correct scan-resistance boundary.
- line 290 `buckets >= MPOOL_ALLOC_SEARCH_LIMIT` (500) — an **early** escalation
  that only fires when `htab_buckets > 500`.

In the measured caches `htab_buckets <= 520`, so line 240 is the binding trigger
and the non-aggressive phase sweeps the whole table.

## 3. What preserves scan resistance (the key observation)

Scan resistance means **a scan must not evict the HOT working set**. That
property has *two* mechanisms in the current code, and only one of them needs
the full sweep:

1. **Selection by warmth.** Both the non-aggressive and the `aggressive` sweep
   choose the victim with the **lowest warmth** across the buckets they compare
   (`bucket_priority > warmth`, `mp_alloc.c:365`). A scan-admitted COOL page
   (warmth 0–1) always beats a HOT page (warmth >= 2) in that comparison, so the
   COOL page is evicted first **whether or not the sweep is aggressive**.
   Critically, `aggressive` mode does **not** age HOT buffers (the `warmth--` is
   inside `if (!aggressive)`), it only *considers* them — so escalating early
   does not decay the hot set.
2. **The HOT-band skip + second chance.** The non-aggressive sweep protects HOT
   from *consideration* and gives COOL pages a second chance (age, don't evict
   on first pass). This is a recency refinement for the COOL band, not the
   hot-set guarantee; the hot-set guarantee is (1).

Therefore the hot set stays protected by the warmth ordering (1) even if the
engine escalates to `aggressive` after only a small window — escalation lets it
*consider* HOT buffers, but it still evicts the lowest-warmth buffer it finds,
which is a COOL page whenever one exists. The full table scan buys only the
second-chance refinement (2) for COOL pages, not the hot-set protection.

## 4. The fix

Bound the non-aggressive COOL-first phase to a small fixed window
`MPOOL_COOL_SWEEP_LIMIT`, then escalate to `aggressive`. Concretely, lower the
pre-aggressive bucket budget from `MPOOL_ALLOC_SEARCH_LIMIT` (500, a near-full
sweep for typical caches) to a small constant.

- In the all-HOT case (bulk insert), the non-aggressive phase skips HOT buffers
  cheaply for `MPOOL_COOL_SWEEP_LIMIT` buckets, escalates, and `aggressive`
  finds a victim in ~2 more buckets — buckets/alloc drops from ~460 toward a
  small constant, back to 5.3.28 territory.
- In a mixed hot+scan workload, the non-aggressive phase still gives COOL pages
  their second chance within the window, and after escalation the warmth
  ordering (§3.1) continues to evict COOL scan pages before the HOT set — so the
  scan-resistance exit criterion (RFC 0002 Stage 0) still holds. This is the
  risk the review flagged ("a too-eager HOT demotion reintroduces scan
  pollution"); it is addressed because escalation here does not *demote* HOT, it
  only widens *consideration*, and selection is still lowest-warmth-first.

A tiny window costs second-chance quality for COOL pages; too large re-introduces
the regression. The value is chosen empirically (§6) against both the XREL evict
probe (no regression vs 5.3.28 on the all-HOT case) and the scan-resistance test
(a scan must not evict the hot set), then fixed as a named constant.

### Why not a cross-call "all-HOT" hint

A sticky per-region flag ("the last full sweep was all-HOT, so start aggressive")
would be the theoretically tightest fix, but it must live in shared region state
and be cleared when a COOL buffer reappears. Every candidate location
(`struct __mpool`, `struct __db_mpool`) is hashed by `__env_struct_sig()`
(`src/env/env_sig.c`), so adding a field is a **region-signature change** — which
would make this a format change, disqualify it from the LTS line, and require the
`REGION-SIG-CHANGE-INTENDED` escape token. The whole value of O-MP1 is that it is
format-free. Overlaying the hint on the existing `config_flags` word is unsafe:
the clear must happen in `__memp_fput` (which does not hold the region lock) and
would race its read-modify-write with the sync path and `memp_set_config`. The
bounded-window fix needs no new state and is provably scan-safe by §3, so it is
preferred.

## 5. Correctness invariants (unchanged)

- Eviction still requires `BH_REFCOUNT == 0` and the buffer latch exclusive
  (`mp_alloc.c`); only *which* victim and *when* we escalate changes.
- The optimistic-read pin check (`__memp_bh_pinned`) is unchanged.
- MVCC chain handling is unchanged (the window gates the singleton COOL-first
  scan; the MVCC-chain and obsolete-buffer paths are untouched).
- No shared-memory layout changes; `__env_struct_sig()` is byte-identical.

## 6. Validation

- **Deterministic test** (`test/db/run_evict_bound.sh`): fill a small cache with
  fresh dirty pages and assert `st_alloc_buckets / st_alloc` (buckets examined
  per allocation, from `db_stat -m`) stays under a bound. Fails on master
  (~130–520), passes on the fix.
- **Scan-resistance** (RFC 0002 Stage 0 exit): a cursor scan of ~2× cache must
  not evict a pre-warmed hot set — `st_page_in` on hot-set re-access must not
  rise materially vs master.
- **Benchmark** (`xrel_evict_probe`, c6id.metal): 1/4/16/64 MB × {btree,queue},
  three arms (base master, fix, 5.3.28). The fix must recover the all-HOT case
  toward 5.3.28 and not regress the in-cache (64 MB) case. Plus `xrel_kv`
  btree/hash at t=16/32 with a fitting cache (read-scaling must not regress).

## 7. Measured outcome (window = 16)

Full A/B in [`test/bench/R3-FIX-2026-11.md`](../../test/bench/R3-FIX-2026-11.md)
(c6id.metal, 5 reps interleaved, CV ≤ 1.5%). With `MPOOL_COOL_SWEEP_LIMIT` = 16:

- buckets/alloc (btree 4 MB) **459 → 15.6**; (queue 4 MB) **520 → 13.8**; 5.3.28 is 1.6–1.8.
- throughput (queue 4 MB) **35×** master, now 0.82× of 5.3.28 (was 0.05×);
  btree under pressure 1.05–1.18×; 64 MB (data fits) **1.00×** — no change when
  eviction never runs.
- dirty evictions (queue 4 MB) **849,148 → 4,676**, identical to 5.3.28.
- profile: `__memp_alloc` falls from **78.7%** self-time to out of the top; the
  cost moves to ordinary insert work.
- in-cache reads (`xrel_kv` C, 16 GB cache): `db_stat -m` shows 0 pages forced,
  so the eviction path never runs and the change is neutral by construction.

The `run_evict_bound` test asserts buckets/alloc < 40, which the window clears
(15.6, max 42) and the unbounded master sweep (459) fails.
