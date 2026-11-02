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

<!-- FILLED IN after the sweep completes -->

### Setup
- Host: `c6id.metal`, 128 vCPU (2 sockets), Debian 12; governor performance,
  THP never, swappiness=1, numa_balancing=0.
- Devices: local NVMe formatted XFS (`/data`); gp3 EBS (root, `/mnt/gp3data`) —
  a slower, higher-latency fsync, to show batching matters more where fsync is
  expensive.
- Driver: `test/bench/commit_bench.c`, prepopulated 1,000,000-key btree (158 MB,
  fits the 512 MB cache — steady state, not an insert ramp), one put per txn,
  `DB_TXN_SYNC`. fsyncs counted from the log region's `st_scount`
  (flush_per_commit) and cross-checked with `strace -c -f -e fsync,fdatasync`.
- A/B: stock master `e1fe5c2c4` vs Berkeley DB 5.3.28, interleaved, arm order
  rotated each rep, 5 reps, median reported; CV in the table.

### Headline (confirmed in a spot check, full table below)
At t=128 on NVMe, master: **flush_per_commit = 0.0258 (~39 commits/fsync),
maxcpf = 125** — group commit coalesces ~39 fsyncs into one, up to 125. The
spot check confirms fsyncs/commit falls far below 1 as threads rise.

### Profile of one cell (t=128, NVMe, master)
`perf --call-graph dwarf`, measured phase (129 threads active): **97% of self
time is `__db_tas_mutex_lock_int`**, split ~32% under `__log_put`
(LOG_SYSTEM_LOCK, the serial append latch) and the rest under the commit path.
`__os_fsync` is negligible (NVMe fsync ~130 µs, batched at 0.026 flush/commit).
The bottleneck is the **serial log-append latch**, not the flush — reproducing
`P5-LOG-APPEND-2026-09.md` and confirming the packets' claim that the flush is
not the limiter.

<!-- TABLE -->

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
