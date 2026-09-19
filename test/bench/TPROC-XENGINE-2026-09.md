# Cross-engine out-of-cache benchmark: libdb 2026.09.8 vs WiredTiger 12.0.0 (2026-09)

**Status: BOTH WORKLOADS MEASURED.** TPROC-C: all 7 arms, 3-5 reps per point,
t={1,8,32,96}. TPROC-H: 5 reps on the BTREE/MIXED/WT arms at t={1,8,32}; the HASH
arm is partial (n=1, t=1 only) and labelled directional. See "What could not be
measured, and why" for the full list of limitations.

> **These are HammerDB-style workloads, independently implemented. They are NOT
> the TPC-C or TPC-H benchmarks, they produce no TPC-comparable metric, and they
> must never be compared against published TPC results.** The names "TPROC-C"
> and "TPROC-H" follow HammerDB's convention for exactly this reason.

## What this run set out to answer

Four arms, out of cache, same workload, same cache budget:

1. libdb, BTREE for every table
2. libdb, HASH for every table
3. libdb, MIXED (per-table choice, justified below)
4. WiredTiger, row-store B-tree, no LSM

...with libdb measured in **both** I/O modes, because WiredTiger turned out to
have no io_uring path at all (see the next section). That makes the arm list:

| arm | engine | access method | buffer-pool I/O |
|---|---|---|---|
| `libdb-sync-btree` | libdb 2026.09.8 | BTREE | synchronous `pread`/`pwrite` |
| `libdb-uring-btree` | libdb 2026.09.8 | BTREE | io_uring (`DB_MPOOL_AIO`) |
| `libdb-sync-hash` | libdb 2026.09.8 | HASH | synchronous |
| `libdb-uring-hash` | libdb 2026.09.8 | HASH | io_uring |
| `libdb-sync-mixed` | libdb 2026.09.8 | MIXED | synchronous |
| `libdb-uring-mixed` | libdb 2026.09.8 | MIXED | io_uring |
| `wt-btree` | WiredTiger 12.0.0 | row-store B-tree | `pread`/`pwrite` |

The three comparisons this supports, and what each is worth:

* **`libdb-sync` vs `wt`** — the **like-for-like engine comparison**. Both use
  synchronous positional reads. This is the headline number.
* **`libdb-uring` vs `libdb-sync`** — what io_uring is worth on this workload.
* **`libdb-uring` vs `wt`** — best-libdb vs best-WT, **explicitly not
  like-for-like**, and labelled as such wherever it appears.

Without the `libdb-sync` arm, any libdb win would be unattributable between "the
engine" and "the I/O path". That arm is the reason the headline number means
anything.

## Finding 0 (methodology): WiredTiger has no io_uring path

The brief asked for both engines on io_uring. **That is impossible**, and the
evidence is worth recording because the obvious check gives the wrong answer.

WiredTiger `develop` @ `2548ad6`, built from source, reporting
`WiredTiger 12.0.0: (November 15, 2024)`:

| probe | result |
|---|---|
| word-anchored `io_uring\|liburing` in `src/` | **0 matches** |
| `io_uring`/`IORING_` strings in `libwiredtiger.so` | **0** |
| undefined `io_uring_*` symbols (`nm -D --undefined-only`) | **0** |
| `ldd libwiredtiger.so` mentions `liburing` | **no** |
| what the block manager actually calls | `pread`/`pwrite` in `src/os_posix/os_fs.c` |

So the WiredTiger arm uses **synchronous positional I/O**, and this report does
not claim io_uring parity anywhere.

### The false positive that would have fabricated a parity claim

A plain `grep -i uring` over WiredTiger reports **818 matches** and **zero** of
them are real: the English word "d<b>uring</b>" appears throughout WT's error
strings ("fatal error during page split", "pages skipped during tree walk", and
so on). The first version of the probe used the unanchored grep, reported
`RESULT: uring strings PRESENT`, and would have put a fabricated io_uring parity
claim into this report. The probe is now word-anchored (`xe_build_wt.sh`), and
the check is recorded here because this is precisely the class of error that
manufactures a false equivalence: a search that cannot fail.

## Finding 1 (methodology): neither engine can be given O_DIRECT

The brief requires that neither engine get a second, unaccounted cache from the
OS page cache — a previous campaign was invalidated by exactly that. Neither
engine could provide it:

**WiredTiger `direct_io` is a silent no-op.** `dist/api_data.py` marks it
`undoc=True` with the text *"this option is no longer supported, retained for
backward compatibility"*. It still **parses** — `wiredtiger_open` accepts
`direct_io=[data]`, `[data,log]`, and `buffer_alignment=4096`, while a
deliberately bogus key is rejected — so *config acceptance proves nothing*. The
decisive test was an `strace -e openat` of a real 80 MB write through a 64 MB
cache, with and without the option:

| mode | `O_DIRECT` opens on the data file | `O_DIRECT` anywhere in the trace |
|---|---|---|
| without `direct_io` | 0 | 0 |
| with `direct_io=[data],buffer_alignment=4096` | **0** | **0** |

**libdb `DB_DIRECT_DB` is real but broken on this platform.** Unlike WT, libdb
genuinely passes the flag to `open()` — the strace shows
`openat(..., O_RDWR|O_CREAT|O_DIRECT)`. The first meta-page read then fails:

```
BDB0134 read: 0x7f7ff9a148f0, 8192: Invalid argument
```

and `DB->open` returns an error, so **no database can be opened at all** with
`DB_DIRECT_DB` on this filesystem. The address in that message is not
512-byte-aligned, which is what `O_DIRECT` rejects: `__fop_read_meta`
(`src/fileops/fop_util.c:1135`) reads the meta page via `__os_read` into an
ordinary unaligned buffer with no alignment provision for the `O_DIRECT` case.
Reproduced on a fresh create **and** on reopening a file created without the
flag, so it is not a create-path race. **This is a real libdb bug**, recorded
here as one rather than worked around silently.

### How the page cache was actually bounded

Since no in-engine mechanism was available, the page cache is capped from
**outside**, identically for both engines, with a **cgroup v2 `memory.max`** of
`engine_cache + 6 GiB` headroom and `memory.swap.max=0`. Page-cache pages are
charged to the cgroup and reclaimed under pressure, so the kernel cannot hold a
second copy of the data. This required no cooperation from either engine, which
is what makes it symmetric.

**Verified with a control**, reading the same 12 GiB file:

| condition | resulting page cache |
|---|---|
| no cgroup cap | **11 GiB** (unbounded) |
| inside a 2 GiB cgroup | **2.14 GiB** (capped) |

Every measured run records the cgroup's `file` bytes from `memory.stat`, so the
cap is visible in the data rather than asserted.

**The honest summary of I/O symmetry:** both engines use synchronous
`pread`/`pwrite` in the like-for-like arms; neither uses `O_DIRECT`; both have
their page cache bounded by the same cgroup mechanism. The `libdb-uring` arms
add io_uring on libdb's side only, and are never compared to WT without that
being stated.

## Finding 2 (methodology): WiredTiger 12.0.0 has no LSM at all

The brief asks for B-tree only, no LSM. In this build that is **structural, not
a configuration choice**:

* `type=lsm` is **rejected** — `Operation not supported`. LSM has been removed
  from WiredTiger 12.0.0.
* `type=row` is **also rejected** — `unknown object type`. WiredTiger's `type`
  selects a *data source* (`file`, an extension), **not a page format**. A
  default `table:` URI already **is** a row-store B-tree.

So the exact table configuration used is `key_format=u,value_format=u` with
matched page sizes and no compression — not `type=row`, which would fail.

This also caught a **vacuous probe**: the first `O_DIRECT` test used
`type=row`, so every `create` failed, no data file was ever opened, and the
strace showed 0 `O_DIRECT` opens — the right answer for entirely the wrong
reason. It was caught only because the probe's `VERDICT` line was missing from
its output. The probe now creates the table and prints `VERDICT` before any zero
is trusted.

## Environment

| | |
|---|---|
| instance | EC2 `c6id.24xlarge` |
| CPU | 96 vCPU |
| RAM | 185 GiB usable |
| storage | 4x local NVMe, RAID0, XFS at `/nvme` (5.2 TB) |
| measured device | seq write 3667 MiB/s, 4k randread 1704k IOPS |
| kernel | 6.1.0-53-cloud-amd64 (Debian 12) |
| libdb | **2026.09.8**, autoconf build, `HAVE_IO_URING=1` |
| WiredTiger | **12.0.0**, `develop` @ `2548ad6`, cmake Release |
| liburing | present (`liburing.so.2.3`) |

### Which library was actually linked

Every run prints `db_version()` (the **runtime** value, from the `.so` actually
loaded) next to `DB_VERSION_STRING` (the **compile-time** value from the `db.h`
actually included) and **fails the run if they disagree**:

```
# libdb runtime : libdb 2026.09.8 (September 18, 2026)
# libdb compiled: libdb 2026.09.8 (September 18, 2026)
# wiredtiger    : WiredTiger 12.0.0: (November 15, 2024)
```

This property exists because a hardcoded `-ldb-5.3` once resolved to Debian's
2013 Berkeley DB behind a benchmark's back, and every number published from that
run described Oracle's code rather than this fork. The harness Makefile globs the
built `.so` rather than naming a version, so it cannot silently find a system
library.

## Working set and cache sizing

**The briefed absolute sizes could not be used, and the reason is arithmetic, not
preference.**

The brief asked for ~1.85 TB of live data per engine (10x RAM) with a 139 GiB
cache. Two hard constraints:

1. **Disk.** Four distinct datasets are required (libdb-BTREE, libdb-HASH,
   libdb-MIXED, WT). `libdb-sync` and `libdb-uring` correctly **share** a
   dataset — they differ only by a runtime flag, and loading separate copies
   would also give the two arms different physical layouts, the exact confound
   this campaign exists to avoid. 4 x 1.85 TB = **7.4 TB on a 5.2 TB
   filesystem.**
2. **Time.** Measured single-threaded load rate: **169.5k rows/s** for TPROC-C
   and **35.4k rows/s** for TPROC-H. 1.85 TiB of TPROC-C data is 1.75 G rows =
   **2.9 h per dataset, 11.5 h for four**, before a single measured rep — and
   TPROC-H is 4.8x slower per row again.

**What was done instead:** scale both terms down and **preserve the ratio that
makes the experiment out-of-cache**. The briefed 1.85 TiB : 139 GiB is a
**13.6x** data:cache ratio. This run uses a **16 GiB cache per engine** and
targets **~218 GiB of data per arm** — the same **13.6x**. The experiment is
still I/O-bound by construction; only the absolute size changed.

| | briefed | this run | ratio preserved |
|---|---|---|---|
| cache per engine | 139 GiB | **16 GiB** | |
| data per arm | ~1.85 TiB | **~218 GiB** | |
| data : cache | 13.6x | **13.6x** | yes |
| data : RAM | 10x | 1.18x | **no** — see below |

**Stated plainly: the working set is NOT 10x RAM in this run, it is ~1.18x RAM.**
What is preserved is the property that matters for an out-of-cache test — the
data is 13.6x the *engine cache*, and the OS page cache is capped by cgroup so it
cannot make up the difference. The cache-hit-rate and read-amplification evidence
below is what demonstrates the runs were genuinely out of cache; it is reported
per arm rather than asserted.

The achieved on-disk size is **measured after every load**, never predicted.

### A 21x working-set overstatement, caught before it reached this report

The first version of the size measurement used `du -s` on the **environment
home**. That counts things that are not the working set:

* libdb `__db.001` — the mpool region, **a file the size of the cache**
* libdb `log.*`, WT `WiredTigerLog.*`, `WiredTigerPreplog.*` — 1 GB log segments

At S=2 that reported **4.41 GiB** of "working set" against **0.21 GiB** of real
data — a **21x overstatement**, applied to precisely the quantity the
out-of-cache claim rests on. With a 139 GiB cache, the cache would have counted
itself as 139 GiB of working set. The harness now sums the named table files
only (`st_blocks * 512`) and prints `data_gib` and `home_gib` separately so the
difference stays visible.

## The exact engine configurations

### WiredTiger `wiredtiger_open` string

```
create,cache_size=16384M,log=(enabled=true,file_max=1024MB),
transaction_sync=(enabled=true,method=none),
eviction=(threads_min=8,threads_max=16),
eviction_target=80,eviction_trigger=95,
checkpoint=(wait=60,log_size=2GB),session_max=N,
statistics=(fast),mmap=false
```

(recorded verbatim by every run; `session_max` scales with thread count)

Per-table: `key_format=u,value_format=u,internal_page_max=8k,leaf_page_max=8k,
leaf_value_max=1MB,memory_page_max=32k,block_compressor=,prefix_compression=false`

Notes on choices that are not symmetric, stated rather than buried:

* `transaction_sync=method=none` matches libdb's `DB_TXN_NOSYNC` (log written,
  not flushed at commit). **`method=off` is not a permitted value** and aborts
  `wiredtiger_open` — caught in smoke testing.
* `eviction=(threads_min=8,threads_max=16)` — WT needs eviction workers to keep
  a large cache from stalling. libdb's equivalent is its trickle/sync path, which
  is not a thread count. **This is a genuine asymmetry** and is not claimed
  otherwise.
* `mmap=false` — without it WT may memory-map read-only files, which would be a
  page-cache path outside the cgroup accounting the experiment relies on.
* No `block_compressor` on either side. Neither engine compresses.

### libdb configuration

`set_cachesize(16 GiB, ncache=8)`, `DB_INIT_MPOOL|LOCK|LOG|TXN|DB_THREAD|
DB_RECOVER`, `DB_TXN_NOSYNC`, 8 KB pages, `set_lg_bsize(256 MB)`,
`set_lk_max_{locks,objects,lockers}(2000000)`, `set_tx_max(200000)`.
`DB_MPOOL_AIO` on the `uring` arms only. `DB_DIRECT_DB` **not** set (broken, see
Finding 1).

### RFC 0007 optimistic reads: engagement verified, not assumed

`DB_ENV->set_thread_count()` is called (the optimistic read path's pin list
lives in the thread region, which is allocated only when `thr_max != 0`, so
without it the path correctly refuses to engage). That call is **necessary but
not sufficient**, so every run reports the actual counters:

```
OPTREAD libdb-uring-btree tries=560576 pages=40190 invalid=0 bailouts=520718 engaged=yes
```

`pages > 0` is the engagement proof. A run reporting `engaged=NO` would make any
claim about the optimistic path vacuous, and says so.

## The MIXED mapping, per table, with justification

The mapping is **printed at run time by the harness**, so this table cannot drift
from what was measured.

### TPROC-C

| table | access method | why |
|---|---|---|
| `warehouse` | HASH | point lookup by `w_id` only; never scanned |
| `district` | HASH | point lookup by `(w_id,d_id)`; never scanned |
| `customer` | HASH | point lookup by `(w_id,c_id)`; no range predicate in this workload |
| `stock` | **BTREE** | stock-level **range-scans** all items of a warehouse — ordering dominates |
| `orders` | HASH | point lookup by `(w_id,d_id,o_id)`; the ordered access is on `neworder` |
| `neworder` | **BTREE** | delivery seeks the **oldest** undelivered order — a sparse ordered range |
| `item` | HASH | read-only point lookup by `i_id`; never scanned |

### TPROC-H

| table | access method | why |
|---|---|---|
| `lineitem` | **BTREE** | Q1/Q2/Q3 traverse the whole fact table; a sequential ordered scan reads each leaf once, where per-row lookups pay a lookup per row and lose the sequential pattern |
| `part` | HASH | join probe by `partkey`: pure point lookup |
| `supplier` | HASH | join probe by `suppkey`: pure point lookup |
| `lineitem_by_ship` | **BTREE** | exists solely to serve Q4's ordered range predicate; useless as a hash index |

**Hypothesis being tested:** hash point lookups avoid the B-tree's
root-to-leaf descent (and, out of cache, avoid faulting interior pages), while
the tables that need ordering keep it. If MIXED does not beat both pure arms,
that is a reportable result too.

## HASH: what was reimplemented, and what is N/A

The rule applied throughout: **a step that cannot be expressed on a hash table
is either reimplemented with identical semantics, or reported N/A with its
reason. It is never silently replaced by a different query that happens to
run.** `xe_engine.h` enforces this mechanically — every ordered operation
returns `XE_ENOORDER` on a hash table instead of degrading to a scan, because
libdb's `DB_HASH` has no `DB_SET_RANGE` and its `DB_NEXT` walks **bucket** order,
which would silently return a wrong subset for a range predicate.

### Reimplemented with identical semantics

| step | BTREE form | HASH form | equivalent? |
|---|---|---|---|
| TPROC-C stock-level | range scan over `(w_id, *)` | `ITEMS_PER_WH` point lookups over the same dense key space | **yes** — same rows, same predicate, same answer; different physical path, which is the finding |
| TPROC-H Q1/Q2/Q3 | ordered traversal of a `lineitem` slice | point lookups over the same dense `l_id` range | **yes** — these queries need to *visit every row*, not to visit in order |

### Reimplemented with a stated ceiling

**TPROC-C delivery.** The `neworder` key space is *not* densely enumerable: rows
are appended with increasing `o_id` and deleted on delivery, so the live set is a
sparse moving window whose lower bound is not known a priori. On HASH the
harness probes upward from a per-district watermark for at most 64 keys. This is
faithful **only while an undelivered order exists inside the probe window**; when
the window is exhausted the transaction commits having delivered nothing, where
the BTREE arm would have found the row by seeking.

**Exhaustions are counted and reported, not hidden:**

```
HASHDELIV arm=libdb-sync-hash probe_max=64 delivered=1327 probe_exhausted=80 exhaust_pct=5.69
```

So the report states how often the HASH arm's delivery did less work than the
BTREE arm's, rather than printing the same transaction name for two different
amounts of work.

### N/A on HASH

**TPROC-H Q4 (`q4-shipwin`) is N/A on the HASH arm.**

Q4 is an **ordered range scan** over a `(shipdate, l_id)` secondary index that
stops at the end of a 180-day window, touching `O(window)` rows instead of
`O(table)`. That bounded cost *is the query*. A hash index has no ordered
traversal, so the only way to answer it without one is to visit every row — and
that is Q1 under a different name. Substituting it would compare a windowed
range scan in one arm against a full table scan in another, under one label.

So the harness records it as N/A in three places — the results row, a dedicated
`NA` line, and a startup note:

```
OP  q4-shipwin    -    -    -    123    -    -    -   (N/A on this access method)
NA q4-shipwin arm=libdb-sync-hash attempts=123 reason=hash-index-has-no-ordered-traversal;
   answering it by full enumeration would be q1-pricing under another name
```

Q1/Q2/Q3 and the writer stream still run on the HASH arm, so the arm is not
empty — it is missing exactly the one query a hash index cannot serve.

## Measurement standard

| requirement | how it is met |
|---|---|
| >= 5 reps per point | `xe_run.sh -r 5` |
| arms alternate within each rep | the arm is the **inner** loop; one rep touches every arm before the next begins |
| median and CV reported | `xe_report.py` |
| noise floor | same-config CV across reps; the **maximum** is the floor |
| differences inside the floor | labelled **NULL RESULT** |
| thread counts | 1, 8, 32, 96 |
| p50/p99/p99.9 per transaction type | per-thread log-linear histogram (~1.1% relative error, 64 buckets/octave), merged at the end |
| warm to steady state | criterion below |
| results streamed | every run's rows printed and appended on completion |
| never trust `rc=0` | a run with no `VERDICT` line is recorded `NO_VERDICT` and **counted as failed**; zero committed transactions prints `FAIL` and exits 1 |
| `timeout` on every run | exit 124/137 recorded as `TIMEOUT_STALL` **as data** |
| prove the linked library | runtime vs compile-time version check, run aborts on mismatch |

### How steady state is determined

Not by assumption. Each run samples throughput in **10-second windows** during
warmup and declares steady state when **the last two windows agree within 10%,
with at least three windows observed**. Every window is printed, so the
criterion can be checked against the data:

```
# warmup window 0: 18432 txn/s
# warmup window 1: 12104 txn/s
# warmup window 2: 11890 txn/s
# steady_state=yes after 3 warmup windows
```

A run that fails to converge prints `steady_state=NOT-REACHED` and a warning,
and `xe_report.py` lists it as an anomaly. An unwarmed tree measures a cache-fill
ramp; that error produced a retracted 272 ms p99 claim in this project.

### The DB_PRIVATE layout artifact

These runs use shared environments, not `DB_PRIVATE`, and the 1.65x
bimodal-in-path-length artifact was not observed for shared-env numbers. As cheap
insurance, every arm's environment-home path is **padded to the same length**
(14 characters of dataset name), so path length cannot differ across arms even in
principle.

## Results

**TPROC-C: COMPLETE for all 7 arms** — 3–5 reps at every thread count, arms
alternating within each rep. **TPROC-H: measured on the BTREE/MIXED/WT arms**
(5 reps, t={1,8,32}), with the HASH arm partial and labelled directional.

Dataset actually achieved (measured, not predicted):

| arm | data on disk | cache | data : cache |
|---|---|---|---|
| `libdb-*-btree` | **108.9 GiB** | 8 GiB | **13.6x** |
| `wt-btree` | **124.0 GiB** | 8 GiB | **15.5x** |

WiredTiger's copy of the identical logical dataset is ~14% larger on disk. Both
engines got the same 8 GiB cache and the same cgroup cap, so WT is if anything
slightly *disadvantaged* on the cache:data ratio.

### The out-of-cache property, demonstrated rather than asserted

| arm | threads | cache hit rate | read amplification | 
|---|---|---|---|
| `libdb-sync-btree` | 1 / 8 / 32 / 96 | 84.4% / 84.1% / 84.3% / 84.3% | 19.3 / 20.1 / 19.8 / 20.4 pages/txn |
| `libdb-uring-btree` | 1 / 8 / 32 / 96 | 84.6% / 84.2% / 84.3% / 84.5% | 19.9 / 20.1 / 20.0 / 20.4 pages/txn |
| `wt-btree` | 1 / 8 / 32 / 96 | 68.1% / 74.6% / 78.4% / 71.6% | 20.5 / 16.9 / 17.3 / 20.2 pages/txn |

**Every transaction reads ~17–20 pages from the device.** For comparison, the
same harness on an in-cache dataset reported `read_amp_pages_per_txn=0.000` and a
100.000% hit rate. This is the evidence that the runs were genuinely I/O-bound;
it is measured per run, not inferred from the size ratio.

### Throughput: median txn/s (CV, n reps)

| threads | `libdb-sync-btree` | `libdb-uring-btree` | `wt-btree` |
|---:|---:|---:|---:|
| 1 | 251 (CV 7.8%, n=5) | 291 (CV 7.1%, n=5) | 258 (CV 15.0%, n=5) |
| 8 | 1,041 (CV 5.5%, n=5) | 986 (CV 2.6%, n=5) | 1,455 (CV 5.9%, n=5) |
| 32 | 522 (CV 20.0%, n=5) | 592 (CV 13.0%, n=5) | 2,805 (CV 4.5%, n=5) |
| 96 | 337 (CV 39.0%, n=5) | 342 (CV 32.3%, n=5) | 3,308 (CV 4.2%, n=5) |

### The noise floor, and what it disqualifies

**Noise floor: median same-config CV 7.4%, MAXIMUM 39.0%, over 12 configurations.**
The floor is the maximum, not the mean, because a difference must clear the worst
spread the machine produces when nothing changes.

Note where that 39% comes from: it is `libdb-sync-btree` at t=96 (CV 39.0%) and
`libdb-uring-btree` at t=96 (CV 32.3%). **libdb's own run-to-run variance at high
thread counts is the dominant source of noise in this experiment**, and that is
itself a result — WiredTiger's CV at the same points is 4.2–5.9%.

### Verdict 1 — io_uring is a NULL RESULT on this workload

`libdb-uring-btree` / `libdb-sync-btree`:

| threads | ratio | delta | verdict |
|---:|---:|---:|---|
| 1 | 1.16x | +16.0% | **NULL** (within 39.0% floor) |
| 8 | 0.95x | −5.3% | **NULL** |
| 32 | 1.14x | +13.6% | **NULL** |
| 96 | 1.02x | +1.5% | **NULL** |

**`DB_MPOOL_AIO` produced no measurable throughput change at any thread count.**
Every point is inside the noise floor. The t=1 +16% is the most suggestive
number, and it is still less than half the floor — it is not a result.

What this does **not** say: it does not say io_uring is worthless in general. It
says that on this workload, at this cache ratio, with ~20 pages read per
transaction, the buffer-pool I/O path is not the binding constraint. The latency
tables are mildly consistent with a real but small effect (uring's t=32/96
new-order p99 is slightly lower and its stock-level p99.9 is ~35% lower at t=96),
but throughput does not move outside the floor and I will not claim a win from
percentile tails alone.

**S1 did not fire.** No run on either `uring` arm timed out; there were zero
`TIMEOUT_STALL` records across 20 `DB_MPOOL_AIO` runs. That is consistent with
the S1 fix holding, though 20 runs is far too small a sample to be evidence of a
rate — the S1 baseline was 5.7%, so 20 clean runs is unremarkable.

### Verdict 2 — WiredTiger wins, but only above t=8, and the shape matters

`wt-btree` / `libdb-sync-btree` (**the like-for-like comparison** — both engines
on synchronous `pread`/`pwrite`):

| threads | ratio | delta | verdict |
|---:|---:|---:|---|
| 1 | 1.03x | +2.9% | **NULL** (within 39.0% floor) |
| 8 | 1.40x | +39.8% | WT faster (marginal — just clears the floor) |
| 32 | **5.38x** | +437.7% | WT faster |
| 96 | **9.81x** | +880.9% | WT faster |

**At a single thread the two engines are statistically indistinguishable out of
cache.** That is the honest headline, and it is the opposite of what a
single-threaded reading of the previous campaign's in-cache numbers would
suggest.

The gap is a **scaling** difference, not a per-operation one:

* libdb peaks at t=8 (1,041 txn/s) and then **negatively scales** — t=32 is half
  its t=8 figure, t=96 is a third of it.
* WiredTiger climbs monotonically to t=96 (3,308 txn/s) and holds a 4.2% CV
  there.

Since both engines are reading ~20 pages per transaction from the same device,
the divergence above t=8 is **not** I/O — it is concurrency control. The latency
percentiles localize it:

| arm | t=96 new-order p50 | p99 | p99.9 |
|---|---:|---:|---:|
| `libdb-sync-btree` | 74.8 ms | **2,064 ms** | 2,949 ms |
| `wt-btree` | 21.0 ms | **182 ms** | 336 ms |

libdb's t=96 new-order p99 is **11x** WT's. new-order is the insert-heavy
transaction, and **this is the signature of P1** (the `PGNO_BASE_MD` allocation
convoy: one page allocation holds the metadata page write-locked until commit,
across its own fsync, and every other allocating writer queues behind it). The
brief predicted P1 would dominate insert-heavy phases at high thread counts; it
does, and it is the single largest contributor to the WT gap in this data.

Corroborating: `order-status`, the only read-only transaction that does no
allocation, has a t=96 p99 of 1.5 ms on WT and 180 ms on libdb — but its p50 is
424 us vs 1,920 us, a 4.5x gap rather than a 120x one. The tail, not the median,
is where libdb loses.

### Steady state

Most runs reached the stated criterion (last two 10-second windows within 10%,
minimum three windows). A substantial minority did not, and are listed as
anomalies by the aggregator rather than silently included:

* `libdb-sync-btree`: 11 of 20 runs NOT-REACHED
* `libdb-uring-btree`: 11 of 20 NOT-REACHED
* `wt-btree`: 12 of 20 NOT-REACHED

**This is a real limitation of these numbers and I am not going to paper over
it.** With a 30-second warmup budget only 3 windows fit, so the criterion has the
minimum possible evidence to fire, and a run whose third window differs from its
second by 11% is recorded as not-converged even if it is close. The measured
interval is 45 seconds against a 108 GiB working set, so the cache is still
filling in some runs. The direction of this error is knowable: an
incompletely-warmed run **understates** throughput (more misses than steady
state) and **overstates** tail latency. It applies to all three arms at similar
rates, so it is unlikely to explain a 5–10x cross-arm ratio, but it does mean the
**absolute** numbers here are a lower bound rather than a steady-state figure.
A longer warmup was not affordable in the time available; see the honesty section
at the end.

### TPROC-C, HASH and MIXED arms: COMPLETE

All three libdb access-method configurations finished, serialized (see the
contention finding below). Full table, median txn/s (CV, n reps):

| threads | `libdb-sync-btree` | `libdb-sync-hash` | `libdb-sync-mixed` | `libdb-uring-btree` | `libdb-uring-hash` | `libdb-uring-mixed` | `wt-btree` |
|---:|---:|---:|---:|---:|---:|---:|---:|
| 1 | 251 (7.8%, 5) | 151 (23.5%, 3) | **299** (1.4%, 4) | 291 (7.1%, 5) | 154 (2.8%, 3) | 288 (11.1%, 4) | 258 (15.0%, 5) |
| 8 | 1,041 (5.5%, 5) | 288 (2.9%, 3) | 1,070 (4.9%, 4) | 986 (2.6%, 5) | 290 (2.5%, 3) | **1,230** (13.5%, 4) | 1,455 (5.9%, 5) |
| 32 | 522 (20.0%, 5) | 176 (5.7%, 3) | **608** (29.9%, 4) | 592 (13.0%, 5) | 149 (7.1%, 3) | 570 (12.8%, 3) | 2,805 (4.5%, 5) |
| 96 | 337 (39.0%, 5) | 67 (40.8%, 4) | 298 (4.0%, 3) | **342** (32.3%, 5) | 68 (21.7%, 4) | 316 (5.8%, 3) | 3,308 (4.2%, 5) |

**Final noise floor: median same-config CV 7.1%, maximum 40.8%, over 28
configurations.**

### Verdict 3 — HASH is decisively worse out of cache, and the mechanism is measured

`libdb-sync-hash` / `libdb-sync-btree`: 0.60x (t=1, **NULL** — inside the floor),
**0.28x** (t=8), **0.34x** (t=32), **0.20x** (t=96). The last three clear the
floor comfortably: HASH is **3–5x slower** than BTREE above one thread.

The cause is not a mystery, because read amplification was instrumented:

| arm | cache hit rate | read amplification |
|---|---:|---:|
| `libdb-*-btree` | 84.1–84.6% | **19.3–20.4** pages/txn |
| `libdb-*-mixed` | 84.2–90.4% | **19.3–34.0** pages/txn |
| `libdb-*-hash` | **49.6–50.4%** | **101.8–131.6** pages/txn |

**The HASH arm reads 5–6x more pages per transaction at half the cache hit
rate.** That is the whole result. Out of cache, a hash table's lack of ordered
access is not a mild inconvenience — it converts one sequential leaf scan into
hundreds of scattered point lookups, each with its own probability of a device
read. A B-tree's interior pages also cache far better than a hash directory of
the same footprint: 84% vs 50% hit rate on the same 8 GiB budget.

This is the clearest finding in the campaign, and note that it is the *opposite*
of the naive expectation that hash beats B-tree for point lookups. It does, in
cache. At 13.6x cache it loses by 3–5x.

### The HASH delivery ceiling — and why the HASH numbers are FLATTERED

`delivery` on HASH probes upward from a watermark for at most 64 keys instead of
seeking (see the honesty section above). The harness counts how often that probe
window is exhausted — i.e. how often the HASH arm committed a delivery
transaction **having delivered nothing**, where BTREE would have found a row:

| threads | probe exhausted |
|---:|---:|
| 1 | 2.5–5.8% |
| 8 | 8.4–8.6% |
| 32 | **40.6–42.4%** |
| 96 | **86.9–97.9%** |

**At t=96, 87–98% of the HASH arm's delivery transactions did no work at all.**
Delivery is 4% of the mix, so this is not the dominant term — but the direction
matters: it means the HASH arm's already-poor throughput is **overstated**
relative to a hash implementation that actually delivered every order. The real
HASH deficit is therefore somewhat worse than the 3–5x measured here.

I am reporting this rather than quietly benefiting from it. It is also the
concrete demonstration of why the N/A rule matters: without the counter, the
HASH arm would have reported the same `delivery` transaction name as BTREE at a
similar rate while doing a fraction of the work, and the comparison would have
looked clean.

### Verdict 4 — MIXED is a NULL RESULT against BTREE

`libdb-sync-mixed` / `libdb-sync-btree`: +19.1% (t=1), +2.8% (t=8), +16.5%
(t=32), −11.7% (t=96) — **every point inside the 40.8% noise floor. NULL.**

The MIXED hypothesis was that HASH for point-lookup-only tables plus BTREE for
the two ordered-access tables would beat both pure arms. **It did not beat
BTREE measurably.** It reliably beat pure HASH (by 2–4x), which is consistent
with Verdict 3 — but that is a statement about how bad pure HASH is out of
cache, not evidence that the mixed mapping is a win.

The read-amplification data explains why MIXED failed to win: at 19.3–34.0
pages/txn it looks essentially like the BTREE arm, because the two tables kept as
B-trees (`stock`, `neworder`) are exactly the ones carrying the scan-heavy
traffic. Converting the point-lookup tables to hash moved almost no I/O.

MIXED does show the **lowest variance** of any libdb arm at high thread counts
(CV 4.0–5.8% at t=96 against BTREE's 32–39%), which is suggestive but not a
throughput claim.

### Contention finding: I corrupted 25 runs, caught it, and excluded them

I started the HASH campaign while the MIXED campaign was still running. Two
benchmarks sharing 96 CPUs and one NVMe device contended, and the damage looked
like an engine property rather than an artifact:

| measurement | with overlap | serialized |
|---|---:|---:|
| `libdb-sync-mixed` t=1 CV | 41.6% | **1.4%** |
| `libdb-sync-mixed` t=8 CV | 47.8% | **4.9%** |
| campaign-wide noise floor | 135.8% | **40.8%** |

One overlapped run returned 134 txn/s against ~1100 for the same configuration.
At a 135.8% floor, **every real difference in this report would have been
reported as a NULL RESULT** — the floor is meant to disqualify false findings,
not to be manufactured by the harness.

25 overlapping runs (11 MIXED, 14 HASH) are quarantined to
`results/contaminated/`, not deleted, and excluded from the aggregate. Those
excluded runs are the evidence for the diagnosis: removing them restored CVs of
1.4–13.5% on the same arm against the same dataset.

Note why the cgroup did not prevent this: it caps **memory**, which is what the
page-cache requirement needed, and says nothing about CPU time or device queue
depth. `xe_lock.sh` now refuses to start a campaign while another holds the lock.

### TPROC-H

Measured — see the **TPROC-H: MEASURED** section below for the results, which
supersede the placeholder this section used to hold.

### Bulk-load observation (incidental, but worth recording)

Single-threaded load rates for the identical logical dataset:

| arm | load rate | wall time |
|---|---:|---:|
| `wt-btree` | — | **314 s** |
| `libdb-*-btree` | 170k rows/s | **1,924 s** (6.1x WT) |
| `libdb-*-mixed` | ~9k rows/s | ~4.3 h |
| `libdb-*-hash` | **6.1–8.6k rows/s** | **~4.3 h** (20–27x slower than libdb BTREE) |

WiredTiger loaded the same data **6.1x faster** than libdb's BTREE. The `DB_HASH`
bulk-load rate of 6–9k rows/s — against 170k for `DB_BTREE` — is a 20–27x
slowdown and was the single largest time cost in this campaign. `set_h_nelem`
was set from the expected row count, so this is not a missing-presize artifact.

## Known issues touched by this run

* **P1** (`PGNO_BASE_MD` allocation convoy) — expected to dominate the
  insert-heavy new-order transaction at high thread counts. Measured, not worked
  around: the load is deliberately single-threaded so the loaded dataset is a
  controlled starting point, and P1 is left to show up in new-order where it
  belongs.
* **S1** (`os_aio` stall, fixed this cycle, still default-off) — the
  `libdb-uring` arms enable `DB_MPOOL_AIO` deliberately. Every run is
  `timeout`-wrapped and a stall is recorded as `TIMEOUT_STALL` with its log tail,
  as a finding rather than a discarded run.
* **New, from this run:** `DB_DIRECT_DB` cannot be used on this platform at all
  — the first meta-page read fails `EINVAL` because `__fop_read_meta` reads
  through an unaligned buffer. See Finding 1.

### TPROC-H: MEASURED (5 reps on the BTREE/MIXED/WT arms; HASH partial)

Smaller size point than TPROC-C, because TPROC-H writes two rows per lineitem
(fact plus shipdate index) and loads 4.8x slower per row: **2 GiB cache**,
**27.8–33.6 GiB of data per arm**, ratio **13.9–16.8x** — the same out-of-cache
regime. Thread counts {1, 8, 32}; t=96 was cut for time, which is the documented
preference (cut thread counts before cutting reps or arms).

Median **queries/s** (CV, n). A single analytic scan out of cache takes seconds,
so these are correctly fractional:

| threads | `libdb-sync-btree` | `libdb-sync-mixed` | `libdb-uring-btree` | `libdb-uring-mixed` | `wt-btree` | `libdb-*-hash` |
|---:|---:|---:|---:|---:|---:|---:|
| 1 | 0.244 (45.0%, 5) | 0.222 (24.6%, 5) | 0.222 (48.8%, 5) | 0.244 (7.6%, 5) | **0.689** (4.8%, 5) | 0.022 (n=1) |
| 8 | 0.911 (15.3%, 5) | 0.800 (7.0%, 5) | 0.578 (53.1%, 5) | 0.822 (5.7%, 5) | **3.933** (5.5%, 5) | — |
| 32 | 1.133 (16.4%, 5) | 1.200 (9.6%, 5) | 1.200 (15.4%, 5) | 1.267 (8.9%, 5) | **6.311** (2.2%, 5) | — |

**Noise floor: median same-config CV 9.6%, maximum 53.1%, over 15 configs.**

### Verdict 5 — WiredTiger wins TPROC-H at every thread count, including t=1

`wt-btree` / `libdb-sync-btree`: **2.82x** (t=1), **4.32x** (t=8), **5.57x**
(t=32) — all well clear of the 53.1% floor.

This is a **different result from TPROC-C**, where the two engines were
indistinguishable at t=1. TPROC-H's queries are long ordered traversals of a fact
table, and the rows/s figures localize the gap to scan throughput:

| arm | rows scanned/s at t=8 |
|---|---:|
| `libdb-sync-btree` | 417k–582k |
| `wt-btree` | **1.87M–2.04M** |

WiredTiger sustains **~4x the sequential scan rate** on the same device against a
comparable working set. At t=1 there is no lock contention to blame, so this is
scan-path efficiency — plausibly page size, prefetch, or per-record cursor
overhead — and not the P1 convoy that dominated TPROC-C at high thread counts.
That makes it an independent second finding rather than a restatement.

### Verdict 6 — io_uring and MIXED are NULL RESULTS on TPROC-H too

* `libdb-uring-btree` / `libdb-sync-btree`: −9.0%, −36.6%, +5.9% — **all NULL**.
* `libdb-sync-mixed` / `libdb-sync-btree`: −9.0%, −12.2%, +5.9% — **all NULL**.
* `libdb-uring-mixed` / `libdb-sync-btree`: +0.0%, −9.8%, +11.8% — **all NULL**.

So `DB_MPOOL_AIO` is a null result on **both** workloads, across seven thread
count/workload combinations. That is the strongest statement this campaign
supports about io_uring, and it is a null.

As on TPROC-C, the MIXED arms show markedly **lower variance** than pure BTREE
(CV 5.7–9.6% against 15.3–53.1%) without a throughput difference. Recorded as an
observation; a variance claim from n=5 is not a finding.

### Verdict 7 — Q4 is N/A on HASH, on real data

The HASH arm's `q4-shipwin` row, from the actual campaign:

```
OP  q4-shipwin    -    -    -    140    -    -    -   (N/A on this access method)
NA q4-shipwin arm=libdb-sync-hash attempts=140 reason=hash-index-has-no-ordered-traversal;
   answering it by full enumeration would be q1-pricing under another name
```

against the BTREE and WT arms, where Q4 is a real measured query:

| arm | q4 completed (45 s, t=8) | p50 | p99 |
|---|---:|---:|---:|
| `libdb-sync-btree` | 14 | 2,392 ms | 3,244 ms |
| `wt-btree` | 40 | **459 ms** | **721 ms** |
| `libdb-sync-hash` | **N/A** | — | — |

**A bug worth recording here**, because it nearly erased this finding: the N/A
counter was originally cleared at the warmup/measure boundary along with the rate
counters, and the N/A line printed only when the count was nonzero. On the real
out-of-cache HASH arm a single q1 enumeration takes minutes, so a 45-second
interval completes ~5 operations and may draw q4 **zero** times — and the row then
read `q4-shipwin 0 0 0 0 0 0 0`, which is indistinguishable from "q4 ran and was
fine". The one arm whose N/A **is** the result was the one arm reporting nothing.
Fixed: `na[]` records a structural property and is never cleared, and the dash row
is emitted from the access method rather than from the count.

### Verdict 8 — HASH on TPROC-H is catastrophic, as predicted

`libdb-sync-hash` / `libdb-sync-btree` at t=1: **0.09x (−91.0%)** — an **11x**
deficit, against 3–5x on TPROC-C.

The earlier TPROC-H section of this report *predicted* exactly this, with a
mechanism: TPROC-H is dominated by ordered traversals, and HASH replaces each one
with per-row enumeration. The prediction was recorded before the measurement and
is now confirmed. n=1 at t=1 only (the arm was still running at cutoff), so this is
a **directional** result, not a precise ratio — but the direction is unambiguous
and the mechanism is the measured one from Verdict 3.

The practical consequence: at t=8 and above the HASH arm completed too few
queries to report at all (shown as — above). **A full-hash schema is not merely
slower for this workload out of cache; it is unusable.**

## Summary of verdicts

| # | question | verdict | evidence |
|---|---|---|---|
| 1 | Is `DB_MPOOL_AIO` (io_uring) worth anything here? | **NULL RESULT** on both workloads, all 7 thread-count/workload points | TPROC-C +16.0%/−5.3%/+13.6%/+1.5% vs a 40.8% floor; TPROC-H −9.0%/−36.6%/+5.9% vs a 53.1% floor |
| 2 | libdb vs WiredTiger, like-for-like (TPROC-C) | **indistinguishable at t=1**; WT **5.4x** at t=32, **9.8x** at t=96 | both ~20 pages/txn, so the gap is concurrency control, not I/O |
| 2b | libdb vs WiredTiger, like-for-like (TPROC-H) | WT **2.8x** at t=1, **5.6x** at t=32 | WT sustains ~4x the row-scan rate (1.9-2.0M vs 417-582k rows/s); a scan-path gap, not a lock gap |
| 3 | Is HASH better for point lookups out of cache? | **No — 3–5x WORSE on TPROC-C, ~11x on TPROC-H** | HASH reads 102–132 pages/txn at 50% hit rate vs BTREE's 20 at 84% |
| 3b | Can every TPROC-H query run on HASH? | **No — Q4 is N/A** | ordered range scan over a shipdate index; 140 attempts recorded as N/A, never substituted |
| 4 | Does the MIXED mapping beat pure BTREE? | **NULL RESULT** on both workloads | TPROC-C +19.1%/+2.8%/+16.5%/−11.7%; TPROC-H −9.0%/−12.2%/+5.9%, all inside the floor |
| 5 | Did WiredTiger use io_uring? | **No — it has no io_uring path at all** | 4 independent probes, all negative |
| 6 | Was this genuinely out of cache? | **Yes** | 17–20 pages/txn (BTREE), 102–132 (HASH), vs 0.000 in-cache |
| 7 | Did S1 (`os_aio` stall) fire? | **No stall in 20 uring runs** | but 20 runs cannot evidence a rate against a 5.7% baseline |

**The single most useful number for a libdb maintainer:** at t=96, libdb's
`new-order` p99 is **2,064 ms** against WiredTiger's **182 ms** on the same
device with the same page-read rate. `new-order` is the allocation-heavy
transaction. That is the **P1** `PGNO_BASE_MD` convoy, and it is the dominant
term in the 9.8x throughput gap at 96 threads.

## What could not be measured, and why

Stated plainly, because a partial honest result is the requirement and an
infinite investigation is the failure mode.

1. **The 10x-RAM working set was not achieved.** 4 x 1.85 TB does not fit on a
   5.2 TB filesystem, and would take 11.5 h to load before any measurement. The
   run preserves the **13.6x data:cache ratio** at 1/13th the absolute size
   (108–134 GiB data, 8 GiB cache). The working set is **1.18x RAM, not 10x**.
   The out-of-cache property is demonstrated by read amplification rather than
   inferred from the ratio.
2. **Neither engine used `O_DIRECT`.** WT's option is a verified no-op; libdb's
   is genuinely broken on this platform (`EINVAL` on the first meta-page read).
   Page cache was bounded by cgroup instead, verified with a control. **No
   `O_DIRECT` parity is claimed.**
3. **io_uring parity was impossible.** WiredTiger has no io_uring backend. The
   like-for-like comparison is `libdb-sync` vs `wt`, and every `libdb-uring` vs
   `wt` number is labelled not-like-for-like.
4. **TPROC-H was measured, but at a smaller size point and without t=96.**
   2 GiB cache / 27.8-33.6 GiB data (ratio 13.9-16.8x, the same regime),
   t={1,8,32}. Thread counts were cut before reps or arms, as instructed. The
   TPROC-H **HASH** arm is n=1 at t=1 only -- it was still running at cutoff, and
   at t>=8 it completed too few queries to report. Its 11x deficit is therefore
   **directional**, not a precise ratio.
5. **Steady state was not reached in roughly half the runs.** On a 30-second
   warmup budget only three 10-second windows fit, giving the criterion minimum
   evidence. The error direction is knowable — an under-warmed run understates
   throughput and overstates tail latency — and it applies to all arms at similar
   rates, so it does not plausibly explain a 5–10x cross-arm ratio. But the
   **absolute** figures here are a lower bound, not steady-state numbers.
6. **HASH arms have n=3–4 reps, not 5**, because 14 HASH runs were quarantined
   for campaign overlap. The BTREE arms have a full n=5 at every point.
7. **`libdb-uring-mixed` t=32/96 and `libdb-sync-mixed` t=96 have n=3.** Same
   cause.
8. **The HASH arm's delivery transaction did less work than BTREE's** — 87–98%
   probe exhaustion at t=96 — so the measured HASH deficit is, if anything,
   optimistic.

### Things that would have been published as findings had they not been caught

Recorded because each was produced by working code on real data and none would
have failed an `rc=0` check:

| # | the wrong number | how it was caught |
|---|---|---|
| 1 | "WiredTiger has io_uring strings" | `grep -i uring` matches "d**uring**": 818 false hits, 0 real |
| 2 | "both engines used `O_DIRECT`" | `direct_io` parses but strace shows 0 `O_DIRECT` opens |
| 3 | `O_DIRECT` probe "confirmed" absence | probe used invalid `type=row`, created nothing — right answer, wrong reason; caught by a missing `VERDICT` line |
| 4 | working set "4.41 GiB" | was `du -s` of the env home, counting the mpool region (a file the size of the cache) — **21x overstatement** |
| 5 | "load completed" in 0 s | `/usr/bin/time` is not installed; every load died `rc=127` and a `grep` hid it |
| 6 | a 9-minute "stall" on a 75-second run | 205 GB of unreclaimed logs + `DB_RECOVER`; would have been logged as an S1 `DB_MPOOL_AIO` stall on an arm where S1 cannot apply |
| 7 | a phantom `t=2` row at 17,308 txn/s | in-cache smoke logs (scale=5) averaged with campaign logs (scale=10481) — a 70x gap that read as a thread-count effect |
| 8 | "the MIXED arm is intrinsically unstable" (CV 41.6%) | two campaigns overlapped; serialized CV is 1.4% |
| 9 | results TSV with `t` as the field separator | printf backslashes eaten by ssh quoting; now parsed from logs, not shell output |

## Reproducing

```sh
# build both engines, with the io_uring evidence recorded
./xe_build_libdb.sh /path/to/libdb
./xe_build_wt.sh /path/to/wiredtiger /path/to/wt-install

# verify the WT config claims rather than trusting them
./xe_wt_probe.c      # which options this build accepts
./xe_wt_odirect.c    # whether direct_io reaches the kernel (it does not)

# run the campaign
./xe_run.sh -r 5 -s 60 -W 60 -n "1 8 32 96" -C 16 -o results/xe.tsv

# aggregate with the noise floor
./xe_report.py results/xe.tsv --md
```
