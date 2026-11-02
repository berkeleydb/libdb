# Cursor-cache A/B results (RFC 0021)

Per-thread transient-cursor cache, review item O-D1. base = `origin/master`
(`e1fe5c2c4`), branch = `perf/cursor-cache`.

## Host and method

- **Host:** `c6id.metal` (128 vCPU, 2 sockets, Debian 12), local NVMe formatted
  XFS at `/data`, mounted `noatime`. Governor `performance`, THP `never`,
  `swappiness=1`, `numa_balancing=0`.
- **Workload:** `test/bench/batch_bench.c` arm `indiv` — a shared `DB_THREAD`
  btree handle, each op a **non-transactional** `DB->get(db, NULL, &key, &data,
  0)` (random key, 1,000,000 keys, 100-byte values). This is the exact path the
  cache targets and the same workload that produced the 85%-cursor-lifecycle
  profile in `BATCHED-READS-RESULTS.md`.
- **Why not xrel_kv:** `xrel_kv` runs every op inside an explicit transaction.
  A txn-associated cursor is **not** cacheable (its locker belongs to a txn that
  commits/aborts between ops — see RFC 0021 §1 eligibility), so the cache does
  not engage there and `xrel_kv` shows no delta by construction. The cache
  helps the non-transactional `DB->get`/`DB->put` path, which `batch_bench
  indiv` measures directly.
- **A/B:** 6 reps, interleaved, arm order rotated each rep; each arm loads its
  own 1M-key database fresh per rep; 10 s measured window per cell. Working set
  (~104 MB data + btree) fits the 512 MB cache (notfound=0 throughout, so every
  key is present and in cache). Count is committed read ops (keys/s).

## Result — `DB->get` per key, keys/s (median of 6), CV, p99

| t | base med (k/s) | CV% | branch med (k/s) | CV% | ratio | base p99 µs | branch p99 µs |
|---|---------------|-----|------------------|-----|-------|-------------|---------------|
| 1 | 544,331 | 0.5 | 587,380 | 0.9 | 1.08× | 2.0 | 2.0 |
| 4 | 748,551 | 3.4 | 806,988 | 2.4 | 1.08× | 6.1 | 4.1 |
| 16 | 1,328,496 | 1.8 | 1,517,649 | 1.7 | 1.14× | 16.4 | 8.2 |
| 32 | 1,636,261 | 2.2 | 1,973,034 | 2.3 | 1.21× | 32.8 | 16.4 |
| 64 | 1,479,038 | 5.2 | 2,267,230 | 5.0 | **1.53×** | 65.5 | 32.8 |
| 96 | 997,152 | 2.8 | 2,802,036 | 4.4 | **2.81×** | 262.1 | 32.8 |

All cells CV ≤ 5.2% (≤ 10% gate met; no cell marked †).

**Reading.** Base scales to t=32, then the per-handle cursor-queue mutex becomes
the wall: throughput *declines* from 1.64M (t=32) to 1.48M (t=64) to 997k
(t=96), the classic negative-scaling signature of the cursor lifecycle. The
branch keeps climbing — 1.97M → 2.27M → 2.80M — so the gain grows with
contention: 1.08× single-thread, 1.53× at t=64, **2.81× at t=96**. Tail latency
improves in lockstep: p99 262 µs → 33 µs at t=96 (an 8× tail cut).

## Profile — t=96 read phase, both arms (`perf record -a --call-graph dwarf`)

The 96 worker threads are mostly **off-CPU** on the cursor-queue mutex, so the
decisive measure is how much of the 128-core machine stays idle:

| arm | `swapper` (idle) share | throughput |
|-----|------------------------|------------|
| base | **66.99%** | 997k/s |
| branch | **46.76%** | 2.80M/s |

Base leaves two-thirds of the machine idle — the threads are serialized on the
per-handle cursor-queue mutex (`__db_tas_mutex_lock_int`/`unlock` are the top
libdb on-CPU symbols, and the rest is the futex/TAS wait that shows as idle).
The branch cuts idle to under half and turns that recovered time into 2.8× the
useful read throughput. The remaining idle in both arms is background dirty-page
writeback from the load (`iomap`/`folio`/`nvme` kernel symbols), not the read
path.

Raw: `cc_indiv.tsv` (this directory). Profile summaries: `prof_base.summary`,
`prof_branch.summary`.

## Single-thread cost

t=1 is +8% (the cache saves the free-queue scan + two partition-mutex
acquisitions per op even uncontended), so there is **no single-thread
regression** — the common case is strictly faster. A worst-case all-miss arm
(cache never hits, e.g. a thread fanning across > `DBC_CACHE_SLOTS` handles)
pays only the few-instruction eligibility test and TLS lookup on top of the
master path.

## Functional gates (shared dev box, `--enable-diagnostic`)

- `test/db` tier: 30 PASS / 4 SKIP (no javac/dtrace) / **0 FAIL**, including the
  new `run_cursorcache_adjust.sh` (PASS) and `run_handle_sizes.sh` (PASS —
  `sizeof(DB)=1744`, `sizeof(DBC)=552`, **unchanged**).
- `run_cursorcache_adjust.sh` sabotage arm: with `-DDBC_CACHE_SABOTAGE`
  (stale-handle epoch guard disabled) the test **FAILS** deterministically
  (ASan SEGV, use-after-free at `__dbc_cache_put`), and **PASSES** with the
  guard intact — so the test exercises a real invariant.
- TCL `run_method -btree test001..010` and `-hash test001..010`: 20/20 PASS,
  0 failures.
- Region signature and `env_sig.c` byte-identical to base (no struct in the
  signature changed).
