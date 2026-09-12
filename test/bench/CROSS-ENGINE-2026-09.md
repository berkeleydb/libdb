# Cross-engine benchmark: libdb 5.3.37 vs WiredTiger (2026-09)

Statistically-qualified replacement for the July-2026 single-figure comparison
that lived in the ROADMAP. Same harness (`kvbench.c`, a single-source
dual-backend YCSB driver), same machine class, but 5 reps per point with
median/spread reported, and two methodology bugs from the July run fixed.

## What this answers

The July numbers were stale in exactly the place that changed most: they
predated the cursor-queue sharding shipped in 5.3.37 (#178). The two questions:

1. **Did #178 close the shared-vs-per-thread "API trap"?** — **Yes, completely.**
2. **Did #178 close the gap to WiredTiger?** — **No. It widens with thread count.**

Both are unambiguous in the data.

## Environment

| | |
|---|---|
| instance | EC2 `i4i.metal` (lava account) |
| CPU | Intel Xeon Platinum 8375C @ 2.9 GHz, **128 vCPU, 2 NUMA nodes** |
| RAM | 1 TB |
| storage | 8×3.75 TB local NVMe, **RAID0 (`md0`) → 27 TB XFS at `/data`** |
| kernel | 6.18.44 AL2023 · THP `never` · numa_balancing `0` |
| engines | libdb 5.3.37 (this fork); WiredTiger (latest release, built from source) |
| method | 5 reps/point, warm-cache steady state, data on NVMe (never tmpfs/EBS) |

Raw data: `results/cross-engine-2026-09-i4i-metal.csv` (1006 rows, schema
`engine,mode,numa,workload,recs,valsz,cache_mb,threads,secs,ops,ops_per_sec,p50_us,p99_us,rep,memcap`).

## Finding 1 — #178 eliminated the API trap

Workload C (read-only, in-cache, 200 M records, 72 GB cache, `spread`), median
ops/s, `bdb-perthr` ÷ `bdb-shared`:

| threads | bdb-shared | bdb-perthr | ratio |
|--------:|-----------:|-----------:|------:|
| 1 | 88 058 | 88 656 | 1.01× |
| 8 | 364 218 | 365 585 | 1.00× |
| 16 | 294 147 | 291 035 | 0.99× |
| 32 | 140 669 | 139 038 | 0.99× |
| 64 | 68 253 | 73 493 | 1.08× |
| 128 | 68 314 | 81 450 | 1.19× |

July measured this same trap at **2.4–3.8×**. It is now **~1.0×** — statistically
indistinguishable. Per-thread handles are no longer worth the API contortion.
This is a real, user-visible win, and it is why the ROADMAP's "~7× shared-handle
trap" language is now deleted rather than merely corrected.

## Finding 2 — the WiredTiger gap is real and grows with threads

WT ÷ bdb-shared, median ops/s, `spread`, 72 GB cache:

| workload | t=8 | t=32 | t=128 |
|---|---:|---:|---:|
| A (50/50 r/w) | 7.9× | 28.4× | 48.6× |
| B (95/5) | 6.9× | 22.2× | 47.8× |
| C (read-only in-cache) | 6.8× | 38.4× | 58.2× |
| D (read-latest) | 4.5× | 20.9× | 20.3× |
| E (short range scan) | 5.0× | 6.9× | 8.0× |

The shape is unchanged from July: **libdb peaks near t=8 and negatively scales;
WT climbs to ~t=32 and holds.** libdb's absolute numbers improved over July
(workload C t=32: 140 K now vs 52 K in July, ~2.7×), but WT's did too — and
WT's July figure (~1.22 M) was itself under-measured; the correct warm-cache
figure is ~5.4 M (see methodology note below).

This is expected. #178 fixed cursor-allocation serialization. The remaining wall
is the **buffer-header pin plus lock-partition latch taken on every B-tree
descent** — the measured #1/#2 bottleneck (see `.agent/notes/scaling-findings.md`)
— which #178 never touched. WT's steady CV of 1–6% versus libdb's 24–62% at high
thread counts is the contention showing up directly as variance. **ROADMAP #2
(latch-free buffer-header pin) is the next lever, and this is the evidence for
it.**

Range scans (E) fare best for libdb — 5–8× rather than 20–58× — because a scan
amortizes the per-operation cursor and lock cost over many records, so the
per-op serialization matters proportionally less.

## Finding 3 — NUMA (the axis metal restored)

`i4i.metal` has 2 sockets, so this axis is measurable again (an
`i4i.8xlarge`, the alternative within the older quota, is single-socket).
Workload A, node0-pinned (one socket, 64 vCPU, local memory) vs spread (both
sockets):

| threads | engine | node0 | spread | spread ÷ node0 |
|--------:|---|---:|---:|---:|
| 16 | wt | 3 303 031 | 1 086 766 | 0.33× |
| 16 | bdb-shared | 165 094 | 75 438 | 0.46× |
| 32 | wt | 4 798 082 | 990 308 | 0.21× |
| 32 | bdb-shared | 108 276 | 34 916 | 0.32× |
| 64 | wt | 2 833 588 | 796 044 | 0.28× |
| 64 | bdb-shared | 91 007 | 16 127 | 0.18× |

**Read this carefully — it does not cleanly reproduce July's "libdb regresses
across sockets, WT scales up."** Here *both* engines are far faster pinned to one
socket than spread across two. That is not a like-for-like comparison: node0 has
local memory and no cross-node coherence traffic, while spread has twice the
cores but pays cross-socket latency on every shared cache line. So this measures
"single-socket-local vs dual-socket-remote", not "does the engine scale across
sockets."

What can be said honestly: libdb's cross-socket penalty is at least as severe as
WiredTiger's (spread/node0 0.18–0.46× for libdb vs 0.21–0.33× for WT), consistent
with libdb's shared regions being NUMA-oblivious — but this run does **not**
support the stronger July claim that WT *gains* from the second socket while
libdb loses. A clean test needs a workload that pins memory per-node and measures
aggregate throughput as sockets are added; that was not built here. The July
cross-socket finding is left standing as historical and this run is reported as a
refinement, not a refutation.

## Methodology bugs fixed from the July run

1. **Cache-warming masqueraded as scaling.** WT first appeared to "scale"
   398 K → 1.47 M → 3.9 M across t=8/16/24 — actually a 106 GB dataset warming
   into the 1 TB page cache. Both datasets are now pre-read (`cat db > /dev/null`)
   before measuring; WT then reads flat (2.38/2.36/2.33 M at t=8, CV 1.3 %). The
   cold-first-touch data is quarantined as
   `results/cross-engine-2026-09-SUSPECT-warmtrend.csv` and must not be used.
2. **The eviction pass now actually evicts.** July's cgroup memory cap was never
   applied at the call site, so its "cold-NVMe" numbers were served from a 1 TB
   page cache. This run uses `O_DIRECT` on the data files so neither engine gets
   page cache for data, with cache sized 0.5/0.75/1.0× the working set.
3. **`DB_TXN_SNAPSHOT_SAFE` → `DB_TXN_SNAPSHOT`.** That flag was removed in the
   5.3.34 ABI break, so July's `kvbench.c` did not compile against 5.3.37. The one
   token was changed; per 5.3.34, `DB_TXN_SNAPSHOT` now *means* SSI, so workload
   E's isolation is preserved in name — **but see the fairness note.**

## Fairness audit

- **Same cache** given to both engines (72 GB resident tier; 24/36/48 GB
  eviction tiers). WT's `wiredtiger_open` config is recorded in `kvbench.c`.
- **Data reached disk** for both: verified on-disk volumes (`/data/db_wt` 52 GB,
  `/data/db_bdb` 186 GB — libdb's larger footprint is its own overflow/page
  overhead, not a measurement error) and confirmed the binaries link the *built*
  libraries, not the system ones.
- **Workload E is NOT strictly comparable to July.** July-E ran on a libdb where
  `DB_TXN_SNAPSHOT` meant plain non-serializable snapshot isolation; on 5.3.37
  there is no non-SSI snapshot mode, so new-E may pay serialization costs July-E
  did not. It is reported, but the E delta is not a clean like-for-like.
- **Not eliminated:** libdb's on-disk footprint is ~3.6× WT's for the same
  logical data, so libdb does more I/O per operation in the eviction tiers; that
  is a genuine engine difference, not a harness artifact, and it counts against
  libdb legitimately.

## Where libdb still loses, plainly

- Concurrent reads at scale: 6–58× behind WT, widening with thread count. Root
  cause is the buffer-header pin, unaddressed by 5.3.37.
- Writes (A/B/D): fsync-per-commit and no group commit; WT's async, batched
  durability is 20–48× ahead at high thread counts.
- Cross-socket: at least as penalized as WT, from NUMA-oblivious shared regions.

## Where libdb is now competitive

- **Single-thread and low-thread**: within 4–8× across all workloads, and the
  shared-handle API is no longer a trap, so the common usage pattern gets the
  engine's real single-thread speed without contortion.
- **Range scans** degrade most gracefully under contention (5–8×).
