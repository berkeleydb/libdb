# libdb vs TidesDB — measured comparison, September 2026

libdb `5ea3efc2a` (2026.09.6) vs TidesDB `d62d694` (v10.0.1).

## Why this file exists in this form

The first version of this comparison was run on a **shared development host at
load 25**, which produced coefficients of variation of **40–92%** — and the table
was written up anyway. That was a violation of the project's standing rule that all
benchmarks run on a dedicated EC2 instance, and it produced a *wrong ordering*:
it showed libdb ahead on reads and TidesDB ahead by ~1.7× on writes. Re-run
properly, reads are near-parity and the write gap is **3.71×**. The shared-host
numbers were not merely noisy, they were misleading, and they are not reproduced
here.

Rule restated: a CV above the effect being measured means there is no measurement.

## Method

- **Host:** dedicated `c7i.8xlarge` (32 vCPU), Debian 12, gp3 10000 IOPS / 500 MB/s,
  nothing else running. `performance` governor, THP off, ASLR off. Terminated after
  the run.
- **Build:** both engines from source at the commits above, `-O2` / `Release`.
  TidesDB built with its **full compression pipeline** (lz4 + zstd + snappy
  present), not a stripped configuration.
- **Workload:** 200,000 keys, 16-byte keys, 100-byte values, identical
  xorshift-generated key order in both arms. One transaction per put
  (`begin`/`put`/`commit`), which is what a transactional store is actually asked
  to do. Reads are point gets over the same key sequence, **after a warm pass** —
  an unwarmed tree measures a load ramp, the exact artifact that produced a
  retracted 272 ms p99 claim earlier in this project.
- **Durability matched:** libdb `DB_TXN_NOSYNC` vs TidesDB `TDB_SYNC_NONE`. Both
  therefore append to a WAL without an fsync per commit. Verified this is
  apples-to-apples: the box's device fsync is **p50 2790 µs**, which is far above
  every latency below, so no arm is paying for a synchronous flush.
- **Reps:** 6, arms alternating within each rep, `numactl --physcpubind=0-7`.
- **Single-threaded by design.** This measures per-operation cost, not scaling.
  No concurrency claim is made here.
- Each arm prints `db_version()` at startup, because a hardcoded `-ldb-5.3` once
  silently linked a 2013 distro Berkeley DB behind a libdb benchmark.

## Results

| engine / phase | ops/s (median) | CV | p50 µs | p99 µs | p99.9 µs |
|---|---:|---:|---:|---:|---:|
| libdb / read | 625,312 | 1.2% | 1.43 | 2.13 | 3.4 |
| tidesdb / read | **714,216** | 5.2% | 1.21 | 2.65 | 3.7 |
| libdb / write (txn) | 116,392 | 0.9% | 7.69 | 20.66 | 33.5 |
| tidesdb / write (txn) | **432,204** | 1.1% | 1.94 | 6.24 | 11.6 |

**Ratios:** reads **1.14×** to TidesDB (near parity); writes **3.71×** to TidesDB.

CVs of 0.9–5.2% are inside the range where a 14% read difference is real but
small, and where the 3.71× write difference is unambiguous.

## Reading the result

**The write gap is architectural, not a defect.** An LSM commit is a memtable
insert plus a WAL append; a B-tree commit updates a page in place. libdb's p50 of
7.69 µs against TidesDB's 1.94 µs is the expected shape of that difference, and no
amount of tuning inside libdb's B-tree write path will close a structural gap.
Reads being near-parity is also the expected shape: a B-tree point read is one
descent, while an LSM must consult memtable plus levels — TidesDB's hybrid
LSM+B-tree SSTable layout and block cache evidently recover most of that.

**What this comparison does not measure**, and should not be read as covering:

- **Concurrency and scaling.** Single-threaded only. libdb's `db_get_multiple()`
  work (2.47× at 96 threads, cursor mutex 85.3% → 0.70%) is invisible here, as is
  any contention behaviour in either engine.
- **Multi-process.** Not measurable in TidesDB at all: a second `tidesdb_open()`
  on the same directory returns `-12`, verified empirically, while two libdb
  processes both attach successfully. This is the defining structural difference
  between the engines and it is a *capability* gap, not a performance one.
- **Durable-commit throughput.** Both arms ran without per-commit fsync. With
  `SYNC_FULL`/synchronous commit the device floor (2790 µs) dominates and both
  engines are limited by group-commit batching, not by their data structures.
- **Large values, range scans, compaction under sustained write pressure, crash
  recovery time, space amplification.** All unmeasured.

## Reproducing

`/tmp/h2h/h2h.c` in the working session; built twice, `-DUSE_LIBDB` and
`-DUSE_TIDESDB`, against each engine's shared library with an `-rpath`. Two
harness bugs worth knowing if this is rebuilt: libdb's `DB` handle must be opened
inside a transaction (or with `DB_AUTO_COMMIT`) or `put(txn)` fails `BDB0098
Transaction specified for a non-transactional database`; and TidesDB's
`tidesdb_get_column_family()` returns the handle directly rather than through an
out-parameter.
