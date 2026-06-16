# libdb ROADMAP

**Goal:** revive Berkeley DB into a competitive, high-performance embedded
storage engine that **matches or beats InnoDB and WiredTiger** on throughput
*and* scalability — scaling with core count on large multi-socket **NUMA**
systems and with the volume of data under management.

Berkeley DB's architecture predates many-core NUMA hardware. Its biggest
limiters today are contention on shared regions (the buffer pool, lock manager,
log, and transaction region), cache-line false sharing, and NUMA-oblivious
memory placement. The roadmap is ordered roughly by expected scalability impact;
the heaviest items are the multicore/NUMA ones.

Feature **#0 — Serializable Snapshot Isolation (SSI)** — is already landed on
`master` (opt-in `DB_TXN_SNAPSHOT_SAFE`). Its current follow-ups: port the
`mp_fget` MVCC version-chain detection (second rw-conflict mechanism) and make
the committed-reader locker/detail reclaim fully incremental (today markers are
reclaimed at checkpoint; the locker/detail structs persist until env close).

---

## Core scalability (multicore / NUMA)

### 1. NUMA-aware regions and a sharded buffer pool (mpool)
Partition the buffer cache into many independent shards (per-NUMA-node, scaling
toward per-core), each with its own hash table, eviction state, and latch, so
unrelated pages never contend; place each region on the local NUMA node with
thread/CPU affinity. Evaluate adopting **LeanStore**'s design — pointer
swizzling for near-zero-overhead resident-page access and its scalable
replacement strategy — and **exmap**-style explicit, scalable memory mapping to
cut page-fault/TLB-shootdown contention on the cache. *Parity target: InnoDB
buffer-pool instances; the single biggest multicore win.*

### 2. Latch-free buffer-header lookup
Replace the per-hash-bucket mpool mutex with an optimistic, epoch- or
hazard-pointer-based scheme so a cache hit on a resident page takes no exclusive
latch and generates no cache-line write traffic. Pairs with LeanStore-style
swizzled pointers (resident children referenced directly, no hash probe).
*Parity target: WiredTiger's lock-free cache.*

### 3. Scalable write-ahead log: group commit + parallel logging
Group commit (batch many transactions per `fsync`), shorter log-region mutex
hold time, pipelined LSN assignment, and multiple in-memory log buffers / log
streams. *Parity target: InnoDB redo group commit.*

### 4. Modern, low-contention lock manager
NUMA-local partitions, a latch-free read/intention-lock fast path, scalable
deadlock detection, and lock-free garbage collection of the SSI SIREAD markers.

### 5. MVCC overhaul with concurrent version GC
Per-page or skiplist-based version chains with scalable visibility and
**concurrent, lock-free reclamation** of obsolete versions, generalizing the SSI
`si_ref`/`mvcc_ref` reference counting into one unified reclaim path.
*Parity target: WiredTiger MVCC.*

### 6. Concurrent, sharded eviction and non-stalling checkpoints
Multiple eviction workers (hazard pointers), incremental/fuzzy checkpoints that
never stall foreground transactions, write-combining to storage. *Parity
target: WiredTiger eviction threads.*

### 7. Cache-line-aware structures and false-sharing elimination
Align/pad hot shared structures (mutexes, buffer headers, counters), per-CPU
sharded counters, and a false-sharing audit across cores.

## I/O and access methods

### 8. Asynchronous I/O and prefetch across all access methods
Platform-dependent async I/O — **`io_uring`** on Linux, **`kqueue`/`aio`** on
the BSDs/macOS, IOCP on Windows — behind a common `os/` async abstraction.
Apply it on the **read path** (deep prefetch / read-ahead for B-tree, Hash,
Queue, Recno scans and point lookups) **and the write path** (async page flush,
log writes, checkpoint I/O), so the engine keeps the device queue full instead
of stalling a worker per I/O.

### 9. Adaptive LSM access method (HanoiDB-inspired, two-axis adaptive)
Add a log-structured merge-tree access method alongside B-tree/Hash/Queue/Recno
for write-heavy workloads. Design synthesized in
[`docs/design/lsm.md`](docs/design/lsm.md) from three implementations plus a
paper: HanoiDB Towers-of-Hanoi levels + SuRF/Bloom filters and **structure-level
adaptation** (SingleIndex - Hybrid - MultiLevel) from `gburd/aether`'s
`src/lsm`, combined with **segment-level adaptive compaction** (per-segment
leveled - tiered selection via a finite-state controller with cooldowns) from
*"Amethyst: Adaptive Compaction for LSM Trees via Segment-Level Policy
Selection"* (Shankar & Rose). These are two **orthogonal adaptation axes**
driven by one shared rolling-counter + cooldown controller (the same primitive
as the SSI marker GC): the structure axis decides *how much* LSM to run by
workload rate; the policy axis decides *how* to compact each segment by its
read/write hotness. *Parity target: WiredTiger LSM / RocksDB, without their
static-policy penalty.*

### 10. Pluggable compression
Block/page compression (lz4/zstd) and key prefix compression, usable by all
access methods. *Parity target: WiredTiger/InnoDB compression.*

### 11. B+Tree skip scan
Add **skip scan** to the B-tree access method, as implemented in PostgreSQL's
`nbtree`: when a query constrains non-leading index columns but not the leading
one, synthesize the distinct leading-column values and probe each subtree,
turning a full index scan into a series of targeted descents. Big win for
multi-column indexes with low-cardinality leading columns.

### 12. Adaptive hash index and read fast paths
An adaptive hash index over hot B-tree pages, bulk-load and read-mostly fast
paths, smarter prefetch heuristics. *Parity target: InnoDB adaptive hash index.*

### 13. HASH access-method review — partitionable, scalable algorithms
Review the current (Litwin linear-hashing) HASH implementation and evaluate
modern concurrent structures that **partition and scale in a mostly
uncoordinated manner** — concurrent tries (**Ctrie**), **hash array mapped tries
(HAMT)**, split-ordered / lock-free hash tables, and Cuckoo/Hopscotch variants —
to reduce coordination on the hash directory and per-bucket latching under high
core counts.

## Durability model and HA

### 14. JE-style index-in-WAL with a log cleaner (new config option)
Offer a **log-structured storage model** as a per-database configuration option
for **both B-tree and Hash**, modeled on Berkeley DB **Java Edition (JE)** and
Oracle NoSQL DB (cf. `gburd/noxu`): the index and data live *in the WAL/log*
itself, with a background **cleaner** reclaiming obsolete log segments instead
of updating pages in place. This trades in-place writes for sequential log
writes (great on flash and for write amplification). A **`LSM-HASH`** variant
along these lines resembles Riak's **Bitcask** (append-only log + in-memory key
directory) and is a natural fit for write-heavy, point-lookup workloads.

### 15. Scalable replication / HA: quorum systems + Fast Paxos
Rework replication toward flexible, analyzable consensus. Use **quorum systems**
via `gburd/rs-quoracle` (construct and analyze read/write quorum systems to tune
the latency/fault-tolerance/throughput trade-off) together with **Fast Paxos**
for low-latency commit, following the approach taken in `gburd/noxu`. Goal:
replication that scales out reads, survives node loss with tunable quorums, and
avoids the leader bottleneck of classic single-master log shipping.

## Project / process

### 16. Finish re-creating the documentation tree
Reconstruct and modernize the full `docs/` tree (reference, API, and "Getting
Started" guides) for the living fork — currently only the legacy distribution
HTML is present. Generate from source where possible so docs track the code.

### 17. Observability and a reproducible benchmark/perf-regression harness
Built-in performance counters and tracing hooks (USDT/eBPF), plus a reproducible
benchmark harness (YCSB, TPC-C-style via HammerDB, and targeted
microbenchmarks) wired into CI to continuously track InnoDB/WiredTiger parity
across core counts and data sizes — so scalability claims are measured, not
asserted.

---

*Cross-cutting:* every change above is gated by the test suite (correctness) and
the benchmark harness (no scalability regressions), and must preserve Berkeley
DB's embedded, no-server, ACID guarantees and on-disk/log compatibility within a
release line. Build support (Autoconf, Meson/Ninja, and the Nix flake) and the
CI matrix expand alongside these features.
