# P5: the log-region latch — measurement record (2026-09)

Backing data for [RFC 0008](../../rfc/0008-scalable-wal-append.md). Raw TSVs in
this directory: `p5_shape.tsv`, `p5_bsize8m.tsv`, `p5_floor.tsv`,
`p5_batch.tsv`.

**Headline: the expected fix does not survive measurement.** Reserve-then-copy
(PostgreSQL / InnoDB 8.0's pattern) targets the buffer copy inside the log
critical section. On this workload that copy is 1.46% of total time, and removing
170× of the *syscalls* inside the same critical section changed throughput not at
all. What does move throughput — by up to +189% — is reducing how many times a
transaction *enters* the serialized stage. RFC 0008 ranks the designs
accordingly and recommends against reserve-then-copy on current evidence.

## Setup

- 96 vCPU, 185 GiB RAM, `/nvme` 5.2 TB XFS striped local NVMe.
- Device ceiling, `fio` 4k random write, io_uring, iodepth 64, 4 jobs:
  **494k IOPS / 1930 MiB/s**.
- Production build (`--enable-shared`, no DIAGNOSTIC), hybrid mutexes
  (`HAVE_MUTEX_HYBRID`), `HAVE_MUTEX_PTHREADS`.
- Workload `p5_log_bench.c`: insert-heavy on a growing btree, unique
  non-colliding keys, 100-byte values, `DB_TXN_NOSYNC`, 4 KB pages, 512 MB
  cache. Every iteration is its own transaction unless `batch` says otherwise.
- All counters (`st_record`, `st_wcount`, `st_wcount_fill`, `st_scount`,
  `st_region_wait`/`nowait`, `st_w_mbytes`) come from `DB_ENV->log_stat`.
- Protocol: arms **alternate within each rep**, 5 reps, median + CV.

## 1. Noise floor — base against itself

Same binary in both arms. The ratio is computed **paired within each rep**,
which is the honest floor for an alternating design; a floor taken from medians
across reps would understate it.

| t | per-rep B/A ratios | max \|dev\| |
|---:|---|---:|
| 1 | −4.1% +3.3% −3.5% +1.8% +0.5% | 4.1% |
| 2 | +2.6% −2.2% +0.5% +3.9% −1.1% | 3.9% |
| 4 | −0.7% +0.3% +0.0% +4.5% +0.5% | 4.5% |
| 8 | −4.9% +0.5% −0.2% −10.4% +8.7% | 10.4% |
| 16 | −9.3% +1.7% +2.6% +0.8% +2.3% | 9.3% |
| 32 | +13.6% +10.8% −11.8% +9.3% +7.0% | 13.6% |
| 64 | −1.6% **−22.2%** +2.5% −6.0% −13.2% | **22.2%** |
| 96 | −7.1% +6.7% +0.9% +10.6% −0.7% | 10.6% |

**Floor: ±22.2%**, driven by bimodality at t=64 — one rep returned 148,708 and
115,649 where the other four returned ~85,000. Compare P4's ±4.6% on the same
box with a different workload: this workload is intrinsically noisier and the
bar is correspondingly higher.

## 2. Scaling shape (`p5_shape.tsv`)

| t | rows/s | MiB/s | % of 1930 MiB/s | region_wait | region_nowait | wait% |
|---:|---:|---:|---:|---:|---:|---:|
| 1 | 163,091 | 79.5 | 4.1% | 0 | 8,318,270 | 0.0% |
| 2 | 213,685 | 104.2 | 5.4% | 706,131 | 10,164,624 | 6.5% |
| 4 | 166,810 | 81.3 | 4.2% | 2,327,112 | 6,068,487 | 27.7% |
| 8 | 141,717 | 69.1 | 3.6% | 3,859,624 | 2,803,972 | 57.9% |
| 16 | 87,607 | 42.7 | 2.2% | 2,408,194 | 1,612,462 | 59.9% |
| 32 | 78,614 | 38.3 | 2.0% | 2,090,991 | 1,504,830 | 58.1% |
| 64 | 90,622 | 44.2 | 2.3% | 2,441,002 | 1,714,119 | 58.7% |
| 96 | 88,435 | 43.1 | 2.2% | 2,366,740 | 1,695,966 | 58.3% |

Three facts: throughput peaks at t=2 and declines; **device utilisation declines
with it** (5.4% → 2.2%), so the log is not waiting for storage; and the log-latch
wait fraction saturates near **58%** from t=8 onward.

## 3. Profile at t=64

`perf record --call-graph dwarf,16384`, 37K samples:

| symbol | share |
|---|---:|
| `__db_tas_mutex_lock_int` | **85.57%** |
| `__db_tas_mutex_unlock` | 4.88% |
| `__bam_ca_di_func` | 1.62% |
| `__memmove_evex_unaligned_erms` | **1.46%** |
| `__db_tas_mutex_readlock_int` | 0.49% |

Within `__db_tas_mutex_lock_int`, `__log_put` is **52.31%**, of which
`__db_addrem_log` → `__db_pitem` → `__bam_iitem` → `__db_put` is 33.89%. This
reproduces P5's 56%/87% attribution.

**The memcpy that reserve-then-copy removes from the critical section is 1.46% of
total time.** Mean record size is 157 bytes (795 MB / 5.06M records) — roughly
10 ns of copying.

## 4. Buffer-size control (`p5_bsize8m.tsv`) — a NULL result that matters

`__log_fill` calls `__log_write` (a `pwrite`) **while holding the region latch**
whenever the buffer fills (`log_put.c:1388-1392`). `lg_bsize` sets how often.

| lg_bsize | writes/s | `wcount_fill`/`wcount` | t=32 rows/s | t=96 rows/s |
|---|---:|---|---:|---:|
| 32 KB (default) | 2,608 | 10,779 / 10,811 | 78,614 | 88,435 |
| 8 MB | **15** | 77 / 154 | 78,537 | 94,421 |

A **170× reduction** in syscalls inside the critical section produced −0.1% at
t=32 and +6.8% at t=96 — **both inside the floor. NULL result.**

Note `wcount_fill ≈ wcount`: essentially every log write is issued by
`__log_fill` under the latch, not by the flusher. Even so, removing them does not
help, because at 32 KB per write the device is doing ~2,600 IOPS out of 494k.

## 5. Critical-section model (`p5_cslen.c`)

Standalone model: two counter bumps plus a 157-byte memcpy into a shared ring,
under a TAS latch that writes owner identity into the same cacheline as
`mut_tas.c:205-206` does. mode 0 = copy inside the latch (today); mode 1 =
reserve inside, copy outside (PG/InnoDB). Each arm twice, alternating:

| t | mode 0 acq/s | mode 1 acq/s | mode 0 mean hold | mode 1 mean hold | mode 0 p99 | mode 1 p99 |
|---:|---:|---:|---:|---:|---:|---:|
| 1 | 14,018,178 / 14,028,228 | 14,133,688 / 14,135,524 | 21.6 ns | 19.3 ns | 40 ns | 20 ns |
| 8 | 1,785,565 / 1,682,748 | 1,619,678 / 1,615,784 | 221–225 ns | 227–228 ns | 645–648 | 673–680 |
| 32 | 1,097,066 / 1,049,300 | 1,221,639 / 1,211,822 | 364–377 ns | 308–320 ns | 1854–1929 | 1521–1551 |
| 96 | 1,129,606 / 1,306,681 | 1,562,870 / 1,321,430 | 310–422 ns | 269–339 ns | 1006–1078 | 895–1023 |

Two conclusions:

1. **Reserve-then-copy's ideal-case ceiling is ~10–20%** at t≥32, and it is a
   *regression* at t=8. Not a multiple.
2. **Mean hold time grows 21 ns → ~300 ns (14×) for constant work.** The
   critical section is dominated by coherence misses on the latch line and the
   counters, which reserve-then-copy does not remove. This is the single most
   useful number in this document: it explains why shortening the section
   fails, and it points at mutex acquisition cost as the deeper issue.

The model latch sustains **1.05M–1.56M acq/s at t=32–96**, while libdb sustains
**243k–279k appends/s**. The latch mechanism has ~4–5× headroom that libdb is not
using.

## 6. Records per transaction

`db_printlog` over a t=1 run producing 1,006,639 rows:

| record type | count | per row |
|---|---:|---:|
| `__db_addrem` | 2,013,278 | **2.00** |
| `__txn_regop` | 1,006,640 | 1.00 |
| `__db_pg_alloc` | 50,326 | 0.05 |
| `__bam_split` | 50,323 | 0.05 |
| **total** | | **3.10** |

The two `__db_addrem` are the key and the data: `__bam_iitem` calls
`__db_pitem` twice (`src/btree/bt_put.c:366`, `:478-483`) and each logs
independently (`src/db/db_dup.c:205`). **One row enters the serialized append
stage three times.**

## 7. Batch A/B (`p5_batch.tsv`) — the result that is outside the floor

Rows per transaction, 5 reps, arms alternating within each rep. Throughput in
**rows/s for both arms**, so the unit of user work is identical.

| t | batch=1 | CV% | batch=4 | CV% | 4/1 | floor at this t | verdict |
|---:|---:|---:|---:|---:|---:|---:|:--|
| 1 | 163,738 | 3.97 | 188,363 | 4.07 | +15.0% | 4.1% | outside |
| 2 | 211,587 | 5.31 | 257,813 | 4.78 | +21.9% | 3.9% | outside |
| 4 | 169,452 | 2.08 | 235,224 | 2.51 | +38.8% | 4.5% | outside |
| 8 | 146,786 | 2.92 | 241,359 | 1.72 | +64.4% | 10.4% | outside |
| 16 | 90,333 | 4.15 | 239,005 | 1.90 | **+164.6%** | 9.3% | outside |
| 32 | 78,439 | 5.32 | 226,841 | 1.22 | **+189.2%** | 13.6% | outside |
| 64 | 86,017 | 2.76 | 213,982 | 1.42 | **+148.8%** | 22.2% | outside |
| 96 | 89,896 | 4.44 | 197,229 | 1.10 | **+119.4%** | 10.6% | outside |

CV *falls* from ~4–5% to ~1–2%: batching removes the bimodality as well as
raising the mean. The curve flattens — batch=4 holds 197k–257k across the whole
sweep instead of collapsing from 211k to 78k.

### Why this is not simply "fewer appends"

Records per row falls 3.10 → 2.35, i.e. **−24%**, but throughput rises up to
**+189%**. Sustained appends/s:

| t | appends/s batch=1 | appends/s batch=4 | ratio |
|---:|---:|---:|---:|
| 1 | 507,588 | 442,653 | 0.87× |
| 2 | 655,920 | 605,861 | 0.92× |
| 4 | 525,301 | 552,776 | 1.05× |
| 8 | 455,037 | 567,194 | 1.25× |
| 16 | 280,032 | 561,662 | **2.01×** |
| 32 | 243,161 | 533,076 | **2.19×** |
| 64 | 266,653 | 502,858 | 1.89× |
| 96 | 278,678 | 463,488 | 1.66× |

**The same latch does 2.19× more appends/s when the appends arrive in bursts from
one thread rather than singly from many.** A latch capacity-limited at 243k
could not reach 533k. 243k is therefore not capacity — it is what the latch
delivers when consecutive acquisitions come from different cores.

Corroboration from the wait counters at t=32: region-lock wait fraction falls
**58.4% → 27.7%** at batch=4 *while appends/s doubles*.

### The real invariant: ~90k transactions/sec

| batch | rows/s | **txns/s** | appends/s | appends/txn |
|---:|---:|---:|---:|---:|
| 1 | 91,609 | **91,609** | 283,988 | 3.10 |
| 2 | 175,494 | **87,747** | 456,284 | 5.20 |
| 4 | 229,921 | 57,480 | 540,314 | 9.40 |
| 8 | 215,330 | 26,916 | 480,186 | 17.84 |

batch=1 → 2 doubles rows/s while **txns/s stays flat**. The ceiling is
transactions, not appends or bytes.

### Device utilisation

| arm | t=32 | t=96 | share of 1930 MiB/s |
|---|---:|---:|---|
| batch=1 | 38.2 MiB/s | 43.8 MiB/s | 2.0% / 2.3% |
| batch=4 | 103.5 MiB/s | 89.9 MiB/s | 5.4% / 4.7% |

**The best arm reaches ~5% of the device.** At 32 KB per write and ~1,440
writes/s, IOPS utilisation is ~0.3% of 494k. Nothing here is device-bound.

## 8. Confound, stated

Batching reduces per-transaction cost **engine-wide** — `txn_begin`/`txn_end`
(the P1 locker path), lock acquisition, the commit record and its flush — not
only log appends. So §7 establishes that the **per-transaction fixed cost** is
the ceiling and that the log's share is large (wait fraction halves), but not
that the log is all of it.

A `nolog` control (no transaction, no logging, identical btree work) gives
270,767 rows/s at t=1 and 307,276 at t=8, against 163,738 and 146,786 for the
logged arm — removing the log/txn path roughly doubles throughput at t=8. Above
t=8 that control is **invalid**: concurrent `DB->put` without `DB_INIT_LOCK`
returns `EINVAL`. It therefore cannot settle the high-thread case and no claim
is made from it. RFC 0008 Risk 1 records the valid control still needed.

## 9. Two harness defects of mine, so they are not re-derived

**Missing `set_lk_detect`.** With `batch > 1`, several keys in one transaction
can genuinely deadlock against a concurrent transaction on a growing tree.
Without a detector configured, the parties block forever — which presented as a
library hang, complete with 33 threads stopped in `__db_pthread_mutex_condwait`
on distinct mutexes. The other benches here set it
(`test/bench/bdb_bench.h:189`). Fixed, plus raised lock-region limits, which the
default ~1000 entries cannot satisfy at t=96 with batching.

**`tas_spins=1` "hangs" that were oversubscription.** Repeated hangs at low spin
counts looked like a lost wake in the hybrid mutex path. Re-tested on an idle
machine: **0 of 6 runs hang**; and 0 of 3 under deliberate 2× oversubscription.
The hangs occurred only while several 96-thread sweeps overlapped on 96 cores,
where a 6-second run cannot finish inside a 40-second timeout. No libdb defect.
Relatedly, an early single-rep probe suggested large gains from low spin counts;
properly repeated, the default (4800 = 96 × `MUTEX_SPINS_PER_PROCESSOR`) was
*best* at t=8 (145,204 vs 110,286 at spins=1). `tas_spins` is not a lever.

## 10. Reproducing

```sh
# scaling shape and the buffer-size control
sh p5_sweep.sh <build> p5_shape.tsv   0       nosync 10 3 0 1 2 4 8 16 32 64 96
sh p5_sweep.sh <build> p5_bsize8m.tsv 8388608 nosync 10 3 0 1 2 4 8 16 32 64 96

# noise floor (same arm twice) and the batch A/B
sh p5_batch_ab.sh <build> p5_floor.tsv 10 5 1,1 1,2,4,8,16,32,64,96
sh p5_batch_ab.sh <build> p5_batch.tsv 10 5 1,4 1,2,4,8,16,32,64,96
python3 p5_report.py p5_batch.tsv

# critical-section model: copy-inside vs reserve-then-copy
for t in 1 8 32 96; do for m in 0 1 0 1; do ./p5_cslen $t 157 4 4800 $m; done; done
```
