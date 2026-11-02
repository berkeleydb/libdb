# Group commit: validated under adversarial faults and benchmarked (2026-11)

W7. Does Berkeley DB / libdb group commit actually hold its durability promise
under crash, faults and multiple processes, and does it coalesce fsyncs as
threads rise? Two review packets asserted "group commit already works"
(`review/B/mp-log-os.md §2.1-2.3`, `review/B/lock-txn-mutex.md §4`); this is the
proof, by adversarial test and by measurement.

**Verdict: the claim holds.** Durability, ordering, mixed-mode coverage and
flush-path error propagation are all correct under crash and fault injection,
in one process and across processes; fsyncs per commit fall far below 1 as
threads rise; and the write-path ceiling is the serial log-append latch, not the
flush — exactly as the packets predicted.

## What was already there (not rebuilt)

libdb has a mature Deterministic Simulation Testing suite (`test/sim/`,
`--enable-dst`): a seeded PRNG, a write-back crash model (`__db_sim_wb_crash`
truncates each file to its last-fsync frontier, so an ack-before-fsync bug loses
data), and `test_sim_group_commit.c` — the single-record durability gate with a
MAP_SHARED acknowledged-set oracle and the NODURABLE planted-bug control. Prior
measurement (`WRITE-PATH-2026-09.md`, `WRITE-TAIL-2026-09.md`) had already shown
the batching (fsyncs/commit 1.0→0.02, up to 94 commits/fsync) and that the
write-path tail is `db->put` page locks, not the flush. W7 fills the gaps that
suite did not cover.

## The adversarial tier (new)

Four scenarios, each attacking a property the single-record gate cannot reach.
All use the proven oracle pattern (ack recorded in MAP_SHARED only after
`commit()` returns 0, so the oracle can only under-state what was acknowledged)
and are run with a NODURABLE negative control where applicable.

| # | Scenario | Property | Result |
|---|---|---|---|
| 2 | `test_sim_gc_ordering` | RMW chains: no committed txn depends on an un-acked one; the latest acked value per counter survives | PASS |
| 3 | `test_sim_gc_mixed` | SYNC/WRITE_NOSYNC/NOSYNC interleaved: a SYNC commit makes every earlier-LSN commit durable | PASS |
| 4 | `test_sim_gc_fault eio` | a leader's fsync returns EIO: the error reaches every follower, no acked commit lost | PASS |
| 5 | `test_sim_gc_fault stall` | a leader stalls 300 ms in fsync: no follower returns success before the fsync completes, none hangs | PASS |
| 6 | `test/db/gc_multiproc` | several real processes share one env, SIGKILLed mid-commit: every acked commit survives recovery | PASS |

`gc-adversarial-gate.sh` (clean phase 1 + NODURABLE phase 2), measured on EC2
(Debian 12, c6i.8xlarge dev box, `--enable-dst --enable-debug`):

```
PHASE1: pass=60 fail=0 of 60          (ordering, mixed, EIO, stall x5 seeds)
PHASE2: caught=6 missed=0 of 6        (NODURABLE caught by ordering + mixed)
gc-adversarial-gate: PASS (60 invariant runs, 6 negative-control catches)
```

`run_gc_multiproc.sh` (6 processes, real SIGKILL + recovery): **5287 acked
commits across 6 processes all survived**.

### Why each test has teeth

- The write-back crash model drops every byte written but not fsync'd, so a
  commit acked without a durable fsync is genuinely lost after recovery.
- The NODURABLE planted bug (`__log_flush_int` skips the fsync but still acks)
  makes ordering and mixed lose acked commits — caught at every seed. (It is
  deliberately NOT run against the EIO sub-test: NODURABLE compiles the fsync
  out, so the `db_env_set_func_fsync` hook the EIO test drives never fires; the
  two mechanisms are mutually exclusive. The EIO test's teeth are the injected
  EIO plus the crash.)
- The EIO follower invariant is read straight from the code: on fsync failure
  the leader unwinds WITHOUT advancing `lp->s_lsn` (the assignment is past the
  fsync), so the `done:` wake pass's `LOG_COMPARE(&lp->s_lsn, &commit->lsn) > 0`
  is false for every follower whose record needed that flush — none is woken
  for success; the first gets the `DB_COMMIT_FLUSH` baton. A follower woken
  without coverage re-tests `ALREADY_FLUSHED` and flushes for itself. The test
  makes the whole process fail persistently and crashes, so any follower that
  lied would lose its acked commit.
- The multi-process test is the only one exercising the cross-process
  `lp->commits` queue, where the follower's `mtx_txnwait` is a
  `PTHREAD_PROCESS_SHARED` mutex and the leader whose fsync wakes it is a
  different process — the DST single-process (pthreads) gate cannot reach it.

## The benchmark

### Setup
- Host: `c6id.metal`, 128 vCPU (2 sockets), Debian 12; governor performance,
  THP never, swappiness=1, numa_balancing=0.
- Devices: local NVMe formatted XFS (`/data`); gp3 EBS (`/mnt/gp3data`) — a
  slower, higher-latency fsync, to show batching matters more where fsync is
  expensive.
- Driver: `test/bench/commit_bench.c`, prepopulated 1,000,000-key btree (158 MB,
  fits the 512 MB cache — steady state, not an insert ramp), one put per txn,
  `DB_TXN_SYNC`. fsyncs counted from the log region's `st_scount`
  (flush_per_commit) and cross-checked with `strace -c -f -e fsync,fdatasync`.
- A/B: stock master `e1fe5c2c4` vs Berkeley DB 5.3.28, interleaved, arm order
  rotated each rep, 5 reps, **median** reported; CV where it exceeds 10% a cell
  is marked †. Committed ops only; 0 deadlocks/aborts in every cell.

### Device fsync latency (`fsync_probe`, n=500) — why the device matters
| device | fdatasync p50 | fdatasync p99 | max serial fsync rate |
|---|---:|---:|---:|
| NVMe XFS | 68–77 µs | 200–220 µs | ~13,000–14,700/s |
| gp3 EBS | **2797 µs** | 2900 µs | **357–358/s** |

gp3's fsync is ~41× slower; its serial fsync ceiling is only ~358/s. Any durable
throughput above that is group commit coalescing, nothing else.

### DB_TXN_SYNC commit throughput, fsync coalescing and latency vs threads
Median of 5 reps. `commits/fsync` = `st_maxcommitperflush`; `flush/commit` =
`st_scount / commits`.

| device | threads | arm | ops/s | flush/commit | commits/fsync | p50 µs | p99 µs |
|---|---:|---|---:|---:|---:|---:|---:|
| NVMe | 1 | 5.3.28 | 27,829 | 1.0000 | 1 | 33 | 138 |
| NVMe | 1 | master | 27,723 | 1.0000 | 1 | 33 | 138 |
| NVMe | 2 | 5.3.28 | 31,043 | 0.9998 | 2 | 58 | 175 |
| NVMe | 2 | master | 30,449 | 0.9998 | 2 | 60 | 176 |
| NVMe | 8 | 5.3.28 | 80,468 | 0.2513 | 8 | 76 | 259 |
| NVMe | 8 | master | 83,403 | 0.2502 | 8 | 75 | 219 |
| NVMe | 32 | 5.3.28 | 62,246† | 0.0962 | 32 | 481 | 1107 |
| NVMe | 32 | master | 57,263† | 0.0953 | 32 | 531 | 1102 |
| NVMe | 64 | 5.3.28 | 43,357† | 0.0518 | 64 | 1389 | 3417 |
| NVMe | 64 | master | 44,053 | 0.0510 | 64 | 1366 | 3204 |
| NVMe | 128 | 5.3.28 | 32,723† | 0.0272 | 126 | 3665 | 8850 |
| NVMe | 128 | master | 32,652† | 0.0255 | 127 | 3677 | 8875 |
| gp3 | 1 | 5.3.28 | 1,043 | 1.0000 | 1 | 889 | 3070 |
| gp3 | 1 | master | 1,067 | 1.0000 | 1 | 886 | 3063 |
| gp3 | 2 | 5.3.28 | 1,072 | 0.9929 | 2 | 1814 | 3888 |
| gp3 | 2 | master | 1,079 | 0.9910 | 2 | 1810 | 3787 |
| gp3 | 8 | 5.3.28 | 3,592 | 0.2501 | 8 | 1839 | 3843 |
| gp3 | 8 | master | 3,658 | 0.2501 | 8 | 1836 | 3840 |
| gp3 | 32 | 5.3.28 | 8,882 | 0.0616 | 31 | 3884 | 4351 |
| gp3 | 32 | master | 9,001 | 0.0615 | 30 | 3872 | 4457 |
| gp3 | 64 | 5.3.28 | 11,819 | 0.0313 | 62 | 5899 | 7108 |
| gp3 | 64 | master | 12,085 | 0.0314 | 61 | 5805 | 7110 |
| gp3 | 128 | 5.3.28 | 18,503 | 0.0156 | 126 | 6860 | 9872 |
| gp3 | 128 | master | 19,022 | 0.0157 | 126 | 6671 | 9771 |

Raw data: `results/group-commit-2026-11/gc_bench.tsv`.

### What the numbers say

1. **Fsyncs per commit fall far below 1 as threads rise**, on both devices and
   both versions: 1.0 at t=1 → **0.026 (NVMe) / 0.016 (gp3)** at t=128, with up
   to **127 commits retired per single fsync**. The batching lever is real and
   fully pulled.
2. **Throughput rises with threads up to the device limit.** On gp3 (slow
   fsync), 1,067 → **19,022 ops/s** t=1→128, an **18× gain**, and **53× the
   device's serial fsync rate of 358/s** — a rate unreachable without
   coalescing. On NVMe (fast fsync), throughput peaks at **83,403 ops/s at t=8**
   then falls to ~32,700 at t=128: once fsync is cheap (68 µs), the limiter
   stops being the flush and becomes the serial log-append latch.
3. **strace cross-check** (master, t=32, NVMe): 3,674 `fdatasync` syscalls for
   38,068 commits = 0.097 fsync/commit, matching the library's reported
   `flush_per_commit` of 0.095. The count is real, not a stat artefact. (libdb
   uses `fdatasync` on Linux.)
4. **5.3.28 ≈ master** across the matrix (within noise, and within the marked
   CVs): the group-commit engine is the same in both, and master's lock/txn
   changes neither help nor hurt this single-put `DB_TXN_SYNC` workload — the
   same conclusion `XREL-2026-10.md` reached for TPC-B write-no-sync.

### Profile of one cell (t=128, NVMe, master)
`perf record --call-graph dwarf`, measured phase (129 threads active):

```
96.96%  __db_tas_mutex_lock_int
  |-- 32.44%  __log_put -> __log_put_record_int (LOG_SYSTEM_LOCK, the append latch)
  `-- (rest)  the commit path
```

`__os_fsync` is negligible (NVMe fsync ~68 µs, batched at 0.026 flush/commit).
The bottleneck at full durability is the **serial log-append latch**
(`mtx_regenv`), not the flush — reproducing `P5-LOG-APPEND-2026-09.md` and
confirming the packets' claim that the flush is not the limiter.

## Conclusion

Group commit's durability contract is correct under crash, fsync EIO, leader
stall, mixed durability modes, read-modify-write ordering, and multi-process
concurrency. The batching is real and effective (fsyncs/commit << 1 as threads
rise). The write-path ceiling under full durability is the serial log-append
latch (`mtx_regenv`), which is a separate, already-known scaling item
(RFC 0015 / `P5-LOG-APPEND`), not a group-commit defect. No engine defect was
found; no engine code was changed.

## Reproducing

```sh
# adversarial tier (any Linux, --enable-dst):
cd build_unix && ../dist/configure --enable-dst --enable-debug && make
make test_sim_gc_ordering test_sim_gc_mixed test_sim_gc_fault
sh ../test/sim/gc-adversarial-gate.sh        # phase1 + NODURABLE phase2
# multi-process (stock build):
BUILD=$(pwd) sh ../test/db/run_gc_multiproc.sh .
# benchmark (metal box, both libraries built):
bash gc_bench.sh > gc_bench.tsv              # the harness in results/
```
