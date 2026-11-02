# W7 — adversarial validation of group commit (design note)

Target: master `e1fe5c2c4`. Branch `test/group-commit`.

## The claim under test

Two review packets assert group commit "already works":

- `review/B/mp-log-os.md §2.1/§2.2`: `__log_flush_int` batches committers
  behind one leader's fsync; the follower returns success only after the
  durable watermark `lp->s_lsn` covers its own commit LSN
  (`LOG_COMPARE(&lp->s_lsn, &commit->lsn) > 0`); a flush the leader did not
  cover hands the follower the `DB_COMMIT_FLUSH` baton to flush for itself.
- `review/B/lock-txn-mutex.md §4`: the write path is limited by page-lock
  convoy across the durable commit, not by the flush.

Prior evidence (`WRITE-PATH-2026-09.md`, `WRITE-TAIL-2026-09.md`) measured the
batching (fsyncs/commit 1.0→0.02, up to 94 commits/fsync, 95 parked followers :
1 fsync) and proved the write-path tail is `db->put` page locks, not the flush.
`test/sim/test_sim_group_commit.c` already gates the single-process,
multi-thread durability contract against crash, with the `NODURABLE` planted-bug
control.

## What already exists (do not rebuild)

| W7 requirement | Existing coverage | Gap |
|---|---|---|
| 1 durability under crash (SYNC, many threads, kill, oracle) | `test_sim_group_commit.c` (MAP_SHARED ack oracle, write-back truncation, catches NODURABLE) | software crash model only; no **real kill -9 + real device drop** confirmation |
| 2 ordering (RMW chains) | — | **none** |
| 3 mixed durability (SYNC flushes earlier NOSYNC) | `test_sim_data_log_order` (NOSYNC+ckpt) | the **SYNC-covers-earlier-LSN** invariant with the oracle, interleaved |
| 4 flush-path fault (fsync EIO / short write reaches every follower) | fault knobs exist, no test asserts the invariant | **none for the follower-error invariant** |
| 5 leader death (stall/cancel while followers wait) | — | **none** |
| 6 multi-process group commit | `mp_failchk_pilot` (failchk, not group commit) | **none for multi-process commit batching** |

## Deliverables (fill the gaps, do not duplicate)

All new DST scenarios compile only under `--enable-dst`, zero-overhead off, and
register in `test/MANIFEST` (`sim` tag) + `dist/Makefile.in`. The oracle in
every case is the proven pattern from `test_sim_group_commit.c`: a MAP_SHARED
acknowledged-set written *after* `commit()` returns 0, read by the parent after
the crash. All use the write-back crash model (`__db_sim_wb_crash`) for the
durability truth, and a `timeout` guard so a hang is a visible failure.

1. **`test_sim_gc_ordering`** (req 2). A chain of read-modify-write txns across
   NTHREAD threads: each txn reads a key's current value, writes value+1, commits
   SYNC, acks. After the crash+recovery, assert no key's surviving value implies
   a *successor* that was never acked — i.e. the recovered state is a prefix of
   the acked RMW history, never past it. A committed txn that depended on an
   un-acked predecessor is a FAIL. Planted-bug control: NODURABLE must break it.

2. **`test_sim_gc_mixed`** (req 3). Interleave SYNC, WRITE_NOSYNC and NOSYNC
   committers. Record every commit's mode and its returned LSN in the oracle.
   Invariant: after crash+recovery, **every SYNC-acked commit is present, AND
   every commit (any mode) with an LSN <= the newest surviving SYNC commit's LSN
   is also present** — a SYNC commit makes every earlier-LSN record durable. A
   NOSYNC/WRITE_NOSYNC commit with a *higher* LSN than the last surviving SYNC
   commit may be absent (allowed). Control: NODURABLE loses the SYNC set.

3. **`run_gc_fsync_fault.sh` + `gc_fsync_fault.c`** (req 4 + 5). A standalone
   `test/db` C test, **no DST build needed** — it uses the public
   `db_env_set_func_fsync` hook so it runs on a stock library. Two sub-tests:
   - **(4) fsync EIO reaches every follower.** NTHREAD threads commit SYNC. The
     fsync hook returns EIO on a chosen flush. Assert **no committer whose fsync
     the failing leader owned returned 0**: a leader whose fsync failed must
     propagate the error (its own commit aborts/returns != 0, per
     `__log_flush_commit`'s non-master path), and every follower that leader was
     responsible for must either get the error or be handed the baton and flush
     for itself (and that self-flush must also see the EIO). The code-read
     prediction (packet §2.1 + the `done:` wake pass): on fsync failure
     `lp->s_lsn` is NOT advanced, so the wake pass's `LOG_COMPARE` is false for
     every follower, so no follower returns success; the first gets the baton.
     The test turns that prediction into an assertion.
   - **(5) leader stall.** The fsync hook, on a chosen flush, blocks (sleeps)
     for a bounded interval while followers wait, then returns 0. Assert every
     follower eventually returns (no hang inside the `timeout`) and that none
     returned 0 *before* the leader's fsync completed (checked with a shared
     "leader fsync done" flag the hook sets, read in the oracle). A follower
     returning success while the leader is still stalled would be the defect.

4. **`run_gc_multiproc.sh` + `gc_multiproc.c`** (req 6). Several real processes
   (`fork`) attach one shared (non-`DB_PRIVATE`) env and commit SYNC
   concurrently, writing to a MAP_SHARED oracle each process owns a slice of.
   A coordinator kill -9's the group at a seeded point, recovers, and requires
   every acked commit (across all processes) present. This exercises the
   cross-process `lp->commits` queue and the per-waiter `mtx_txnwait`
   (a `PTHREAD_PROCESS_SHARED` mutex) under a real multi-process crash — the
   axis `test_sim_group_commit` (single process, pthreads) does not reach.

5. **`gc-realcrash.sh`** (req 1, real-device confirmation; EC2 only). On the
   benchmark box: a loop device / `dmsetup` flakey (`drop_writes`) target under
   the env dir, run the multi-process committers, `kill -9` + drop dirty cache
   (`echo 3 > /proc/sys/vm/drop_caches` and the flakey drop), recover, check the
   oracle. This is the real power-cut analogue of the software write-back model,
   run once to confirm the software model's verdict matches a real device.

## Benchmark (EC2 metal; `test/bench/GROUP-COMMIT-2026-11.md`)

Reuse `commit_bench.c` (prepopulated, per-phase timing, reads `st_scount`) and
`fsync_probe.c`. A/B 5.3.28 vs master, interleaved, >=5 reps, CV<=10%:

- SYNC commit throughput + p50/p99 vs threads 1..128, tiny txn (one put),
  prepopulated key range (steady state, not the insert ramp — the WRITE-TAIL
  lesson);
- fsyncs/commit from `st_scount` and cross-checked with
  `strace -c -f -e fdatasync,fsync` on a short run;
- two devices: local NVMe XFS and gp3 EBS (different fsync latency);
- expectation if group commit works: fsyncs/commit << 1 as threads rise,
  throughput rises with threads to the device limit.

## Non-goals / risks

- No RFC: this is validation. If a test FAILS (a real durability defect), stop
  and fix RFC-first if it is a design change, else a separate fix commit with a
  FAIL-before/PASS-after test.
- The software write-back model is the authority for the crash tests; the
  real-device run is a confirmation, not the primary gate (it is less
  deterministic and EC2-only).
- `shortcut:` the multi-process DST (`DB_SIM_RNG_SCHED`) scheduler is v2 and not
  used here; the mp crash test uses a seeded kill point with real OS scheduling
  (deterministic fault, nondeterministic interleaving), the same honest boundary
  as `mp_failchk_pilot`.
