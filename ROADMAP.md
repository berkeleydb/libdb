# libdb ROADMAP

**Goal:** revive Berkeley DB into a competitive, high-performance embedded
storage engine that **matches or beats InnoDB and WiredTiger** on throughput
*and* scalability — scaling with core count on large multi-socket **NUMA**
systems and with the volume of data under management.

Berkeley DB's architecture predates many-core NUMA hardware. Its biggest
limiters today are contention on shared regions (the buffer pool, lock manager,
log, and transaction region), cache-line false sharing, and NUMA-oblivious
memory placement. The roadmap is ordered by expected scalability impact; the
heaviest items are the multicore/NUMA ones.

Feature **#0 — Serializable Snapshot Isolation (SSI)** — is already landed on
`master` (opt-in `DB_TXN_SNAPSHOT_SAFE`). The following ten build on that base.

---

### 1. NUMA-aware regions and a sharded buffer pool (mpool)
Partition the buffer cache into many independent shards (per-NUMA-node, scaling
toward per-core) each with its own hash table, LRU/eviction state, and latch, so
unrelated pages never contend. Place each region's shared memory on the local
NUMA node and add thread/CPU affinity hints. *Parity target: InnoDB buffer-pool
instances; the single biggest multicore win.*

### 2. Latch-free buffer-header lookup
Replace the per-hash-bucket mpool mutex with an optimistic, epoch- or
hazard-pointer-based scheme so the common case (cache hit, page already resident)
takes no exclusive latch and generates no cache-line write traffic on hot pages.
*Parity target: WiredTiger's lock-free cache.*

### 3. Scalable write-ahead log: group commit + parallel logging
Add group commit (batch many transactions per `fsync`), shrink the log-region
mutex hold time, pipeline LSN assignment, and allow multiple in-memory log
buffers / log streams. The log is currently a global serialization point.
*Parity target: InnoDB redo group commit.*

### 4. Modern, low-contention lock manager
Make the partitioned lock table NUMA-local, add a latch-free read/intention-lock
fast path, and scale deadlock detection. Fold the SSI SIREAD machinery into this
redesign and make its garbage collection lock-free. *Removes the lock region as a
scaling wall on contended workloads.*

### 5. MVCC overhaul with concurrent version GC
Move from the current globally-serialized version handling toward per-page or
skiplist-based version chains with scalable visibility checks and **concurrent,
lock-free reclamation** of obsolete versions — generalizing the SSI
`si_ref`/`mvcc_ref` reference counting into one unified reclaim path (this also
retires the current "locker/detail persists until env close" limitation).
*Parity target: WiredTiger MVCC.*

### 6. Concurrent, sharded eviction and non-stalling checkpoints
Multiple eviction workers using hazard pointers, plus incremental/fuzzy
checkpoints that never stall foreground transactions, with write-combining to
storage. *Parity target: WiredTiger eviction threads.*

### 7. Cache-line-aware structures and false-sharing elimination
Align and pad hot shared structures (mutexes, buffer headers, counters), replace
global atomics with per-CPU/sharded counters, and audit the engine for false
sharing. Low-glamour, high-yield on many-core systems.

### 8. Pluggable compression and a log-structured (LSM) access method
Block/page compression (lz4/zstd) and an optional LSM-tree access method for
write-heavy workloads, alongside the existing B-tree/Hash/Queue/Recno methods.
*Parity target: WiredTiger compression + LSM.*

### 9. Adaptive indexing, async I/O, and read fast paths
An adaptive hash index over hot B-tree pages, bulk-load and read-mostly fast
paths, smarter prefetch/readahead, and asynchronous/`io_uring` I/O on Linux.
*Parity target: InnoDB adaptive hash index.*

### 10. Observability and a reproducible benchmark/perf-regression harness
Built-in performance counters and tracing hooks (USDT/eBPF), plus a reproducible
benchmarking harness (YCSB, TPC-C-style via HammerDB, and targeted
microbenchmarks) wired into CI to continuously track InnoDB/WiredTiger parity
across core counts and data sizes — so scalability claims are measured, not
asserted. A modern CMake build and perf-regression gates support this.

---

*Cross-cutting:* every change above is gated by the test suite (correctness) and
the benchmark harness (no scalability regressions), and must preserve Berkeley
DB's embedded, no-server, ACID guarantees and on-disk/log compatibility within a
release line.
