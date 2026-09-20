# RFC 0008: Scalable WAL append — shrinking `__log_put`'s critical section

- **Status:** Draft
- **Type:** Prospective
- **Author:** libdb maintainers
- **Date:** 2026-09-20
- **Tracking:** defect **P5** in `test/KNOWN-ISSUES.md`. Follows P1 (locker
  stripes) and P4 (cursor-walk latch), both fixed, which exposed this as the
  next ceiling.
- **Prototype:** measurement harness in `test/bench/p5_log_bench.c`,
  `test/bench/p5_sweep.sh`; results in `test/bench/P5-LOG-APPEND-2026-09.md`.

---

## Summary

`__log_put` serializes every log append behind one region latch
(`LOG_SYSTEM_LOCK`), and with P1 and P4 fixed that latch is where the engine
stops scaling: **56% of all time at t=64, 87% of it on this one latch.** This
RFC establishes the anatomy of that critical section with line citations,
surveys the two systems that have solved precisely this problem in production
(PostgreSQL's WAL insertion locks, InnoDB 8.0's redo-log rewrite) plus the
canonical research (Aether), and ranks concrete designs for libdb.

**The RFC's central finding is negative and it changes the ranking.** The
obvious design — PostgreSQL-style *reserve-then-copy*, which shortens the
critical section by moving the buffer copy outside it — addresses a cost this
workload does not actually pay. Two measurements say so. First, the profile:
`__memmove` is **1.46%** of total time while mutex acquire+release is
**90.45%**. Second, a zero-code experiment: raising the log buffer from 32 KB
to 8 MB cut writes issued *inside* the latch by **170×** (2,608/s → 15/s) and
changed throughput **not at all** (t=96: 88,435 → 94,421 ops/s, inside the
noise floor). If neither the copy nor the syscall inside the critical section
is the cost, then shortening the critical section is not the fix.

What the same experiments *do* implicate is the **handoff**: on a 96-vCPU box
each waiter burns up to **4,800** test-and-set attempts on one shared cacheline
per acquisition (`MUTEX_SPINS_PER_PROCESSOR` = 50 × 96 CPUs,
`src/dbinc/mutex.h:30`, `src/mutex/mut_region.c:56-60`), and every successful
acquisition *writes* to that cacheline twice (`mutexp->pid`/`tid`,
`src/mutex/mut_tas.c:205-206`). A spin-count probe that is pure configuration —
no code change — moves throughput more than any critical-section surgery is
projected to. That reorders the proposals: **D0 (spin/handoff tuning, no format
break, no durability argument) before D1 (reserve-then-copy, format break, hard
durability argument).**

The recommendation is therefore **do not implement reserve-then-copy yet**, and
land the cheap, safe, measurable thing first.

## Motivation

### The measured ceiling

With P1 and P4 fixed, throughput on a 96-vCPU box peaks at **t=2 (~203k
ops/s)** and *declines* to **~104k at t=96** — a scaling curve that goes the
wrong way. Profiling at t=64 attributes **56% of all time to `__log_put`, 87%
of that to the log-region latch**.

The device is nowhere near saturated. On the reference box (`fio`, 4k random
write, io_uring, iodepth 64, 4 jobs): **494k IOPS / 1930 MiB/s**. The log
subsystem's own counters, read from `DB_ENV->log_stat` in the same runs, show
libdb pushing **79.5 MiB/s at t=1 and 43.1 MiB/s at t=96** — i.e. **4.1%
falling to 2.2%** of the device. Throughput *and* device utilisation both
decline as threads are added. Whatever the log is doing at t=96, it is not
waiting for storage.

### Why upstream fixes did not help

P1 and P4 each removed real contention and each produced a modest or null
throughput change. That is the expected behaviour of a queueing network with a
saturated serial stage downstream: removing contention upstream of a serialized
bottleneck moves the queue rather than shortening it. Every committing
transaction must append to the log, and today the append is serialized. This is
the stage that has to change for the others to pay off.

## `__log_put` anatomy

All citations are `src/log/log_put.c` unless noted.

### The call chain under the latch

```
__log_put                          :91
  ├─ (outside the latch) __os_calloc + memcpy of caller record   :161-165
  ├─ (outside the latch) __log_encrypt_record                    :167
  ├─ (outside the latch) __db_chksum                             :173   ← precedent
  ├─ LOG_SYSTEM_LOCK                                             :176
  │   ├─ __log_put_next                                          :179 → :452
  │   │    ├─ old_lsn = lp->lsn                                  :476
  │   │    ├─ version check / adv_file                           :483
  │   │    ├─ file-switch test                                   :513
  │   │    │    └─ __log_newfile  (rare)                          :524 → :665
  │   │    │         └─ __log_flush_int(NULL, 0)                  :705  ← fsync under latch
  │   │    └─ __log_putr                                         :542 → :798
  │   │         ├─ save b_off / w_off / f_lsn for rollback       :833-835
  │   │         ├─ hdr->prev, hdr->len                           :842-843
  │   │         ├─ LOG_HDR_SUM (fold prev+len into caller chksum) :868
  │   │         ├─ *lsn = lp->lsn        ← LSN ASSIGNED HERE     :879
  │   │         ├─ __log_fill(hdr)                               :886 → :1335
  │   │         ├─ __log_fill(payload)                           :894 → :1335
  │   │         │    ├─ f_lsn = *lsn when b_off == 0             :1363
  │   │         │    ├─ memcpy(bufp + b_off, addr, nw)           :1382  ← DATA MOVEMENT
  │   │         │    ├─ b_off += nw                              :1385
  │   │         │    └─ if buffer full: __log_write (pwrite!)    :1388-1392
  │   │         ├─ lp->len = hdr->size + dbt->size               :897
  │   │         └─ lp->lsn.offset += lp->len                     :898
  │   ├─ lsnp->file/offset = lsn.*   (return LSN to caller)      :189-190
  │   ├─ [rep master only] unlock, send, relock                  :207,270-284
  │   ├─ __log_flush_commit (if DB_FLUSH|DB_LOG_WRNOSYNC)        :331 → :550
  │   └─ STAT(++lp->stat.st_record)                              :343
  └─ LOG_SYSTEM_UNLOCK                                           :358
```

**The confirmed opportunity:** the latch covers both *reservation* (bump
`lp->lsn` at `:898`, `lp->b_off` at `:1385`) and *data movement* (the `memcpy`
at `:1382`). Only reservation needs mutual exclusion. **The confirmed
complication:** measurement says the data movement is not what costs (§Findings).

**Precedent for splitting work out of this exact critical section already
exists in this exact function.** `__log_putr` accepts a checksum its caller
computed *outside* the latch, and the comment at `:844-852` names it as an
optimization: *"If we were passed in a nonzero checksum, our caller calculated
the checksum before acquiring the log mutex, as an optimization."* The
`LOG_HDR_SUM` at `:868` then folds in the two fields (`prev`, `len`) that were
not yet known outside. So "compute outside, commit inside" is established
practice here, not a foreign import — which materially lowers the design risk
of doing more of it.

### State under the latch, and what each piece actually needs

| State | What it is | Needs | Fast path? |
|---|---|---|---|
| `lp->lsn` | append frontier, the LSN handed to the caller (`:879`, `:898`) | **atomic reservation + total order** | yes |
| `lp->b_off` | write cursor in the shared buffer (`:1385`) | **atomic reservation** | yes |
| `dblp->bufp[...]` | the shared buffer bytes (`:1382`) | none once the range is privately reserved | yes |
| `lp->len` | length of the *last* record; feeds `hdr->prev` of the next (`:897`) | **ordering** — it is the back-chain | yes |
| `hdr->prev` | byte-wise back-pointer (`:842`) | **ordering** (derived from `lsn.offset - len`) | yes |
| `lp->f_lsn` | LSN owning the buffer's first byte (`:1363`) | mutual exclusion with the flusher | yes (first record per buffer) |
| `lp->w_off` | file offset of next write (`:1594`) | mutual exclusion with the flusher | on buffer-full only |
| `lp->stat.st_*` | counters (`:343`, `:1596-1608`) | **atomicity only** | yes |
| `lp->s_lsn` | durable watermark | already on `mtx_flush`, not the region latch (`log.h:263-266`) | flush only |
| `lp->commits`, `ncommit`, `t_lsn`, `in_flush` | group-commit queue (`:1240-1260`) | mutual exclusion | commit only |
| file-switch bookkeeping | `lp->lsn.file++`, `w_off = 0`, persist record (`:713-719`) | mutual exclusion | **rare** |

Three entries are the hard part of any sharding design:

1. **`lp->len` / `hdr->prev` make the log a byte-wise back-chained list.** A
   record's header stores the *offset of the previous record*. So a reservation
   cannot compute its own header without knowing the length of the record
   immediately before it. This is a genuine serial dependency in the *format*,
   not an implementation artifact.
2. **`lp->f_lsn` and `w_off` couple the append path to the flusher.** A
   reserved-but-not-yet-filled hole in the buffer must never be written.
3. **File switch** flushes *and* fsyncs while holding the region latch
   (`:705` → `__log_flush_int`), and the comment at `:689-698` explains why the
   latch cannot be dropped there: a thread with a smaller record could otherwise
   see space in the old file after the switch decision.

### What recovery requires of the on-disk byte order

This is the invariant that kills any design that reorders bytes. Three
independent mechanisms each require that the log be a **densely packed,
strictly ascending, gap-free byte sequence per file**:

1. **Forward scan stops at the first bad record.** `__log_recover`
   (`src/log/log.c:303`) positions at the start of the last file and walks
   `DB_NEXT` until `__logc_get` fails (`src/log/log.c:373-381`); the end of the
   log is *defined* as where that walk stops, and `lp->lsn` is set from it
   (`src/log/log.c:393-397`). A hole in the middle of the file therefore
   truncates the log at the hole — **every record after it is silently lost**,
   even though the bytes are present.
2. **A zero header is virtual EOF.** `__logc_hdrchk`-adjacent logic in
   `__logc_get` treats `hdr->prev == 0 && hdr->chksum[0] == 0 && hdr->len == 0`
   as end-of-file (`src/log/log_get.c:1238-1240`). A reserved-but-unfilled
   region is exactly this bit pattern (buffers and files are zero-filled —
   `__db_file_extend`/`DBLOG_ZERO`, `:1546-1552`). So a crash mid-copy does not
   produce a detectably corrupt log; it produces a log that **ends early and
   looks clean.**
3. **The back-chain is load-bearing in both directions.** `DB_PREV` walks via
   `logc->prev = hdr.prev` (`src/log/log_get.c:775`, `:489`), and the
   partial-record reassembly in `__logc_inregion` walks the in-buffer chain
   looking for the record whose `prev` points at the target
   (`src/log/log_get.c:990-1001`). Both break if `prev` is wrong.

**Conclusion, stated for the record: any design that writes log bytes out of
order, or that can leave a gap in the byte stream at a crash, is dead on
arrival.** Not "risky" — it silently discards committed transactions, because
mechanism (1) and (2) turn a gap into a clean-looking early end-of-log rather
than an error. Every design below is evaluated against this first.

### Multi-process: where libdb differs from PG and InnoDB

PostgreSQL and InnoDB both solve this problem inside a **single process**, with
all writers as threads sharing a heap and a fate: if a backend dies mid-copy,
PostgreSQL restarts the whole cluster and replays WAL. libdb's log buffer is in
a **shared-memory region attached by mutually untrusting processes**, and libdb
is expected to survive one of them dying via `DB_ENV->failchk`.

`__mut_failchk` (`src/mutex/mut_failchk.c:20`) recovers a mutex held by a dead
process **only if** `DB_MUTEX_PROCESS_ONLY` is set (`:50-52`) — which the log
region latch is not. So today a process that dies holding the log latch wedges
the environment into `DB_RUNRECOVERY`, which is at least *loud*. Under
reserve-then-copy, a process that dies **after reserving and before filling**
leaves a hole in the buffer while holding no lock at all, and by the invariant
above that hole becomes a silent early end-of-log. **Reserve-then-copy converts
a detectable failure into silent data loss in exactly the configuration libdb
supports and its two model systems do not.** This is the single most important
asymmetry in this RFC and §Design weighs every proposal against it.

## Prior art

### PostgreSQL WAL insertion locks — the closest analogue

PostgreSQL faced this exact defect: a single `WALInsertLock` serializing all
appends. The fix (8.1 through 9.4, `src/backend/access/transam/xlog.c`) is
*reserve-then-copy-in-parallel* and is the direct model for design D1.

**Mechanism.** `XLogInsertRecord` reserves space by atomically advancing a
shared counter — `ReserveXLogInsertLocation` bumps `Insert->CurrBytePos` under
a dedicated spinlock (`insertpos_lck`), or `ReserveXLogSwitch` for the
rarer file-switch case — returning the byte range `[StartBytePos, EndBytePos)`.
The record is then copied into the WAL buffers **outside** that spinlock, by the
inserting backend itself, in parallel with other backends copying into their own
ranges.

**The hard part — tracking which reserved ranges are filled.** This, not the
reservation, is the real content of the design. PostgreSQL uses
`NUM_XLOGINSERT_LOCKS` (8 by default) *insertion locks*, each with an
`insertingAt` field (`XLogCtlInsert->WALInsertLocks[i].l.insertingAt`). A
backend acquires one insertion lock (round-robin by backend, so the 8 locks are
much less contended than one), reserves its range, publishes how far it has
progressed in `insertingAt`, and releases. A process that needs to know the
safe, contiguous, fully-written prefix — the WAL writer flushing, or
`XLogWrite` — calls `WaitXLogInsertionsToFinish`, which reads every insertion
lock's `insertingAt` and **waits for the minimum to pass the target LSN**. The
"filled prefix" is thus derived as `min(insertingAt)` over a small fixed array,
not tracked per byte. `LWLockWaitForVar`/`LWLockUpdateVar` exist specifically to
make "wait until this lock's variable passes X" cheap.

**Assumptions, and whether they hold for libdb.**

| PG assumption | libdb |
|---|---|
| Single process; a backend crash → crash-recovery of the whole cluster | **False.** Multi-process, `failchk` is expected to recover. This is the fatal asymmetry (§Multi-process). |
| WAL is a flat byte space with **no back-chain**; a record's header has no pointer to the previous record | **False.** `hdr->prev` (`:842`) is a byte-wise back-pointer, so reservation *n* needs the length of reservation *n−1*. |
| A fixed small number of concurrent inserters, bounded by `NUM_XLOGINSERT_LOCKS` | Transferable. |
| Recovery reads WAL as one ordered byte stream | **True, and preserved** — PG's reservation assigns *contiguous ascending* ranges, so the on-disk order is unchanged. No merge at recovery. |

**Does it preserve a single recoverable byte order?** Yes — and this is why it
is the right model. Reservation is a monotonic counter bump, so byte order on
disk is identical to what a single global lock would produce. The only new
hazard is a *temporally* incomplete prefix, which is precisely what
`insertingAt`/`WaitXLogInsertionsToFinish` exists to prevent the flusher from
writing.

### InnoDB 8.0 redo log rewrite — the other production answer

MySQL 8.0 removed `log_sys_t::mutex` from the redo append fast path (WL#10310).
Pre-8.0 InnoDB looked much like libdb today: one mutex covering LSN assignment
and the copy into the log buffer.

**Mechanism.** Writers atomically reserve an LSN range
(`log_buffer_reserve`, a CAS on `log.sn`), copy into the (now much larger) log
buffer without holding any global mutex, and then *announce* completion. Three
dedicated background threads replaced the inline work: **`log_writer`** (moves
buffer → OS), **`log_flusher`** (fsync), **`log_closer`** (advances the dirty
frontier); plus `log_write_notifier`/`log_flush_notifier` to wake waiters.

**The hard part — `link_buf`.** InnoDB's answer to "which reserved ranges are
filled" is a dedicated lock-free data structure, `link_buf<lsn_t>`, used twice:
`log.recent_written` tracks ranges copied into the buffer, and
`log.recent_closed` tracks ranges whose dirty-page bookkeeping is done. A
`link_buf` is a fixed-size circular array of slots indexed by LSN mod capacity;
a writer finishing `[start, end)` stores `end` at slot `start`, and a reader
(`log_advance_ready_for_write_lsn`) walks forward from the current frontier
following the links, advancing while contiguous. That walk yields exactly the
**contiguous filled prefix**. Capacity is bounded
(`innodb_log_recent_written_size`), so a writer that would outrun the window
waits — which *is* InnoDB's append backpressure.

**Comparison of the two "filled prefix" structures** — this is the design
choice libdb would have to make:

| | PostgreSQL | InnoDB 8.0 |
|---|---|---|
| Structure | 8 insertion locks, each with `insertingAt` | `link_buf`, circular array of LSN links |
| Prefix derived by | `min(insertingAt)` over a fixed array | forward link-walk from frontier |
| Concurrency bound | `NUM_XLOGINSERT_LOCKS` inserters | window capacity, not inserter count |
| Cost per append | acquire/release one of 8 LWLocks | one CAS + one store |
| Backpressure | buffer-full wait | window-full wait (explicit) |
| Fits libdb's back-chain? | No better than InnoDB — neither has one | No |

**Assumptions.** Same single-process assumption as PG (a crashed thread takes
the server down; recovery replays redo). Also assumes redo records are
**self-describing without a back-pointer**, so a reserved range can be filled
independently. Preserves one recoverable order; no merge at recovery.

### Aether (Johnson, Pandis, Stoica, Athanassoulis, Ailamaki; VLDB 2010)

"Aether: A Scalable Approach to Logging" is the canonical research treatment,
and it names four distinct log bottlenecks — which is useful here because it
separates the one libdb has from the ones it does not.

- **Early lock release** — release locks before the commit record is durable.
- **Flush pipelining** — decouple a transaction's worker thread from its flush;
  the worker detaches at commit and a separate thread completes it after the
  fsync. Group commit batches flushes; flush pipelining stops the *worker* from
  blocking. libdb's `__log_flush_int` leader/follower is group commit but not
  flush pipelining — the follower blocks on `mtx_txnwait` (`:1125`).
- **Consolidation array (consolidated buffer allocation)** — the mechanism most
  relevant to P5. Instead of every thread taking the log mutex, threads
  *combine* their requests: a small fixed array of slots, each thread CASes its
  length into a slot to join a group, one thread per slot becomes the leader,
  acquires the mutex **once** for the whole group's combined length, and the
  group members then copy their records into the group's reserved region in
  parallel. This converts N lock acquisitions into N/k, and is a better fit for
  a back-chained format than per-record reservation, because the *group leader*
  can compute the intra-group back-chain locally.
- **Decoupled buffer fill** — the copy happens outside the critical section.

**Assumptions.** Shared-memory multicore, single process, atomic CAS. Preserves
a single byte order (the consolidation array hands out contiguous ranges).
Aether's measurements are on Shore-MT.

### Silo (Tu, Zheng, Kohler, Liskov, Madden; SOSP 2013)

Silo decentralises logging: per-core log buffers written by per-disk logger
threads, and **epoch-based group commit** — time is divided into short epochs
(~40 ms) and a transaction is durable once its epoch is; the *epoch*, not a
per-record LSN, is the recovery unit.

**Does it preserve a single recoverable byte order? No — and it does not need
one.** Silo's serialization order within an epoch is recovered from
transaction-local read/write sets, and recovery merges per-core logs by epoch.
This requires that no transaction's outcome depend on intra-epoch order across
logs. libdb's redo records carry **page-LSN preconditions** (`CHECK_LSN`,
`src/dbinc/log.h:412`; `__log_check_page_lsn` at `:2256`), i.e. physiological
redo against a specific page image — so a merge that reorders two records
touching the same page is unsound. **Silo's decentralisation does not transfer.**
Its epoch idea *does* inform backpressure (§D0/D3).

### Scalable Logging through Emerging Non-Volatile Memory (Wang & Johnson, VLDB 2014)

Distributed logging with per-core log buffers in NVM, using **passive group
commit**: because NVM writes are durable at cacheline granularity with no
fsync, the log can be decentralised and ordering resolved by LSN comparison at
recovery. **Assumes byte-addressable persistent memory** — hardware libdb does
not require and cannot require. Requires a merge at recovery. Not transferable;
included because it is the standard citation for "multiple logs, one order."

### Taurus / Taurus-MM (Xia et al., VLDB 2020 / 2023)

Taurus provides **multiple parallel logs with no global LSN**: each transaction
records a vector of dependencies (an "LSN vector" — per-log watermarks) instead
of a single sequence number, and recovery replays respecting the partial order
the vectors encode. Taurus-MM extends this to multi-socket/NUMA with vector
compression.

**Mechanism relevance.** Taurus is the most rigorous answer to "can a
multi-core engine have N logs and still recover?" — yes, if you replace the
total order with an explicit partial order. **This requires a merge at recovery
and a format change to every record** (to carry the dependency vector). For
libdb this means: new record format, new recovery driver, `db_log_verify`
rewritten, and the `hdr->prev` back-chain replaced. That is a different product,
not a patch. Recorded as out of scope and *why*, so nobody re-derives it.

### Kafka and Redpanda — where the analogy breaks

Both scale writes by **partitioning**: a topic is split into partitions, each
partition is an independently-ordered append-only log owned by one broker (and
in Redpanda, by one core — a thread-per-core Seastar shared-nothing design with
one log per core, its own memory, its own io_uring queue, no cross-core locks).
Ordering is guaranteed **within** a partition and explicitly **not** across
partitions.

**Where it breaks, precisely.** The transferable part of partitioning is
availability of an *independent* order per partition. A single-node ACID engine
cannot use it, for a reason more specific than "it needs a total order":

- libdb's redo is **physiological** — a record says "apply this delta to page
  P, whose LSN must be X" (`CHECK_LSN`, `src/dbinc/log.h:412`). Two
  transactions touching the same page produce records whose replay order is
  fixed by the page-LSN chain. Partitioning the log by transaction or by thread
  puts those two records in different partitions with no defined relative
  order, and recovery cannot reconstruct one, because the page LSN tells it only
  that it is missing something, not where to find it.
- Partitioning by **page** (or by database file) would preserve the per-page
  chain — and is the one variant that is not immediately unsound — but a
  transaction spanning two pages then commits across two logs, requiring
  two-phase commit between them, plus a merged order for the *transaction*
  records. libdb's single `__txn_regop` commit record has no place to express
  "durable in log 3 at position 91 and log 7 at position 12."
- Kafka's consumers tolerate per-partition order because application semantics
  supply the cross-partition ordering (or do not need it). Recovery has no
  application to defer to.

**What does transfer:** (a) per-core/per-process *buffering* with a
consolidation step before a single ordered commit — that is Aether's
consolidation array, arrived at from the other direction; (b) **backpressure**,
below.

### Backpressure: how these systems signal "storage is saturated"

libdb has **no** mechanism here, which is a gap independent of P5's latch.

| System | Mechanism |
|---|---|
| Kafka | Client quotas (byte-rate/request-rate per principal). The broker computes a throttle delay and **delays the response**, pushing the stall into the producer without an error. `max.in.flight`/`buffer.memory` bound the producer's own queue; `BufferExhaustedException` when full. |
| Redpanda | Built-in, no tuning: per-shard io_uring queues with explicit scheduling groups, and Seastar's reactor accounts for disk bandwidth so an overloaded shard naturally stops accepting. |
| PostgreSQL | `wal_buffers` full → the inserting backend must *itself* write WAL before it can reserve (`AdvanceXLInsertBuffer` → `XLogWrite`), so the producer becomes the writer. Plus `max_wal_size`/checkpoint throttling. |
| InnoDB | Two layers. `log_free_check()` before a mini-transaction: if the redo log's free space is below a watermark, the thread **stalls** until the checkpointer advances. And the `link_buf` window: a writer that would outrun `recent_written` capacity waits. Space is *reserved* before work begins, so the stall happens before a transaction is half-done. |

**The libdb analogue.** The natural seams are `DB_TXN->commit()` and
`DB->put()`. Today, if the device cannot keep up, the log buffer fills and
`__log_fill` performs the write inline under the region latch (`:1388-1392`) —
the queue forms *on the latch*, invisible to the caller, and every other
appender waits behind a syscall. That is accidental, unbounded-in-latency
backpressure. A deliberate design would: (1) reserve log space before doing
transactional work (InnoDB's `log_free_check` shape); (2) when the durable
frontier lags the append frontier by more than a configured bound, return a
retryable indication or block *outside* the latch; (3) expose the lag as a
statistic so an operator can see saturation. Note libdb has a precedent for a
"retry, the resource is exhausted" return in `DB_LOCK_NOTGRANTED`/
`DB_LOCK_DEADLOCK`, which applications already handle — so `DB_LOG_FULL`-style
backpressure would fit existing call sites. Detailed as **D3**.

---

*(§Design, §Findings, §Risks and §Decision follow; the anatomy and prior-art
survey above are committed first, per the RFC process, so the analysis lands
independently of how far the prototyping gets.)*
