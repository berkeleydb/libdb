# R1 (optimistic seqlock BH pin) — MEASURED: correct, safe, but NO throughput win

Status: **do not merge as a perf win.** R1 is correct, crash-safe, ABI-clean,
and composes with the shipped root snapshot — but it delivers **no measurable
throughput improvement** on the workload it can reach. Kept as a documented
result alongside `perf/mpool-pin` and the rsnap negative result. Branch
`perf/bhpin-r1` (`201fdb507`).

## What R1 does (design option c, after the refcount-free variant was withdrawn)

An optimistic, seqlock-validated read-hit fast path in `__memp_fget`: for a
clean, singleton, **wired**, cache-resident page it validates a per-bucket
seqlock (`db_atomic_t seq` on `DB_MPOOL_HASH`, bracketed odd/even by every
`mtx_hash`-exclusive mutator), takes `bhp->ref`, re-checks the seqlock, and
returns the frame **without acquiring `mtx_hash` or `mtx_buf`** — removing 2 of
the 3 hot shared cache-line RMWs per read. `bhp->ref` is still taken (option c),
so `__memp_fput` is unchanged and can never mis-account or dangle a frame.

The refcount-**free** variant (return with no `ref`) was implemented first and
**withdrawn**: it panicked (`BDB3012 unpinned page returned`) because the
DB_PRIVATE envs R1 targets run with `ip == NULL` (no per-thread pinlist to
record the borrow sentinel into). Option c needs no sentinel and works for
`ip == NULL`.

## Correctness (all verified, independently re-checked)

- Fires without thrashing: hit rate stable at 50–66.7% across 500k–3M reads
  (root+internal served latch-free, leaves fall through). Unlike the rsnap
  multi-level attempt, R1 wires the **shared** frame in place — no per-descent
  malloc/memcpy, so no thrash.
- Composes with the shipped root snapshot: rsnap serves the root copy, R1 serves
  the internal pins; disjoint, no double-count (rsnap-off → R1 also serves the
  root, hit rate rises 50%→66.7%).
- SSI intact: `ssi_abort` ~1494 ON vs ~1499 OFF (write-skew still detected; the
  optimistic path only serves singleton pages where `__memp_si_rwconflict`
  returns 0).
- Concurrent reader/writer-with-splits/cursor-delete: `mismatches=0` +
  `db_verify` clean, fast-path ON and OFF, private and shared.
- ASan/UBSan clean; TSan zero races on `seq`/probe/`ref`.
- ABI: `sizeof(DB/DBC/DB_ENV)` unchanged; `DB_MPOOL_HASH` 56→128 B is the
  deliberate region-format change behind a `DB_REGION_MAGIC` bump.

## Measurement — EC2 c7i.24xlarge (96 vCPU, Debian 12), 3M-key btree, 8 GB cache

A/B `DB_NO_BHPIN` on/off, in-cache uniform-random reads (`rrand`), the workload
R1 targets, DB_PRIVATE so the fast path fires:

| threads | ON/OFF ratio |
|--------:|-------------:|
| 1 | 0.99× |
| 16 | 1.06× |
| 32 | 1.00× |
| 64 | 1.05× (5 reps: 1.11/1.05/0.94/1.07/1.03 — straddles 1.0) |
| 96 | 1.00× |

`rhot` (single hot key, R1's theoretical best case) was even noisier
(0.54×–1.25×, no direction).

**R1 is statistically neutral.** The ~5% medians are inside the noise band (a
rep goes the other way).

## Why no win — the real finding

The DB_PRIVATE in-cache read workload **scales positively to t=64** here
(414K→3.66M ops/s); the severe negative-scaling-past-t=8 from the cross-engine
benchmark (`test/bench/CROSS-ENGINE-2026-09.md`) **does not reproduce in
DB_PRIVATE**. That negative scaling lives in the **shared-region / lock-manager
read-lock path** (`__lock_get_internal`, lock-partition latch on hot keys) —
which R1, gated to DB_PRIVATE and touching only the mpool buffer-header pin,
does not address. In the single-process case the mpool pin's cache line is
simply not the bottleneck: removing its RMW buys nothing measurable because
other costs (the `__bam_cmp` search itself, the lock manager) dominate.

`rhot` confirms it: a single hot key maxes out the mpool-pin cache-line
contention R1 removes, yet R1 doesn't help — because the hot-key bottleneck is
the **lock-manager read lock**, not the pin.

## Conclusion / next lever

R1 correctly removes the mpool buffer-header latch/pin RMWs, but that is not the
binding constraint for either (a) DB_PRIVATE single-process reads (which already
scale) or (b) the shared-handle negative scaling (which is lock-manager-bound).
The measured next lever is the **lock-manager read-lock path** — the
`__lock_get_internal` / lock-partition contention on hot keys and internal-page
read locks — which is what actually caps the shared-env workload the
cross-engine benchmark measured. R1 stays gated + off by default (it is safe and
may help a future shared-env variant once the lock path is addressed), but it is
not a throughput win today. Do not merge as perf.
