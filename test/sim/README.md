# test/sim — Deterministic Simulation Testing (DST) for libdb

Seed-driven, replayable fault-injection and crash/recovery testing, modeled on
FoundationDB / TigerBeetle and the xtc project's DST. See
See [`DESIGN.md`](DESIGN.md) (or the local canonical copy `.agents/dst-design.md`) for the full architecture
and scenario catalog.

**v1 scope:** deterministic fault-injection + crash/recovery on the single-process
axis. The full deterministic multi-process scheduler is v2 (design doc §0).

## Layout

| File | Role |
|---|---|
| `sim_rng.h` | seeded per-stream PRNG + activation/guard API |
| `sim_fault.h` | fault toggles, buggify, I/O fault knobs, write-back model API |
| `sim_os.h` | declarations of the `__os_*` I/O hooks (included by the os layer) |
| `sim_inject.h` | planted-bug ids (`DB_DST_INJECT_BUG`) — the bug-detection yardstick |
| `sim_scenario.h` | shared crash/recover helpers (fork+crash+recover boilerplate) |
| `sim_core.c` | the DST core (PRNG, guard, buggify, I/O fault knobs, write-back crash model, stale-read ring) — linked into libdb under `--enable-dst` |
| `sim_os_hooks.c` | bridge from `__os_*` (os_rw.c, os_fsync.c) to the core — linked into libdb under `--enable-dst` |
| `test_sim_rng.c` | PRNG determinism / seed-sensitivity / stream independence / guard |
| `test_sim_crash_recover.c` | **capstone**: durable txns survive a crash via the write-back drop; recovery clean |
| `test_sim_torn.c` | corrupt reads caught by checksum or invisible — never silently wrong |
| `test_sim_hash_crash.c` / `test_sim_recno_crash.c` / `test_sim_queue_crash.c` | per-access-method op+crash+recover (hash / recno / queue) |
| `test_sim_ckp_crash.c` | page-flush (checkpoint) durability across a crash |
| `test_sim_torn_log.c` | recovery is safe past a torn log tail |
| `test_sim_enospc.c` | disk-full (ENOSPC) graceful degradation, no corruption |
| `test_sim_abort_atomic.c` | committed present + aborted leave no trace after a crash |
| `test_sim_recover_idempotent.c` | recover twice → identical full-state hash |
| `test_sim_dup_crash.c` | sorted duplicates survive a crash with exact multiplicity |
| `test_sim_overflow_torn.c` | overflow (>page) records: corrupt read caught, never silently wrong |
| `test_sim_split_crash.c` | btree split/merge churn survives a crash, tree clean |
| `test_sim_stale.c` | stale-read ring: a monotonic-version check catches every out-of-date read |
| `test_sim_ckp_enospc.c` | checkpoint under ENOSPC degrades cleanly; committed txns durable |
| `test_sim_split_torn.c` | torn write during a split-heavy flush caught by the page checksum |
| `test_sim_recover_corrupt.c` | corrupt reads DURING recovery never yield silently-wrong data |
| `test_sim_secondary_crash.c` | primary + secondary (associate) index consistent after recover |
| `test_sim_largetxn_crash.c` | a 2000-op single txn is atomic across a crash |
| `test_sim_cursor_crash.c` | cursor mutations durable; post-recovery cursor walk sees exact live set |
| `test_sim_multi_fault.c` | latency + ENOSPC both active across a crash; committed durable |
| `test_sim_latency_load.c` | slow disk makes progress; committed set byte-identical to fast disk |
| `test_sim_ckp_lsn.c` | checkpoint LSN is the correct recovery start point |
| `test_sim_swarm.c` | **swarm**: mixed-fault seed sweep + per-fault activation coverage |
| `dst-sweep.sh` | run one scenario over a seed range, report pass count + failing seeds |
| `dst-swarm.sh` | swarm the FULL scenario set + fault-mix activation, one CI summary |
| `dst-bug-inject.sh` | build a library per planted bug, assert each is caught within K seeds |

## Build

DST is a compile-time option. **Off by default** and **zero-overhead when off**:
with `--enable-dst` absent, `HAVE_DST` is undefined, the `__os_*` hooks compile
to the stock code path, and none of `sim_core.c`/`sim_os_hooks.c` are linked
(verified: the OFF library exports no `__db_sim_*` symbols).

From the nix dev shell:

```sh
cd /home/gburd/ws/libdb
nix develop --command bash -c '
  cd build_unix &&
  ../dist/configure --enable-debug --enable-dst &&
  make -j4 &&           # builds libdb with the DST core
  make dst_tests'       # builds all 24 scenario executables + the swarm
```

Run the pilots (they link the shared lib, so set `LD_LIBRARY_PATH`):

```sh
cd build_unix
LD_LIBRARY_PATH=.libs ./test_sim_rng
LD_LIBRARY_PATH=.libs ./test_sim_crash_recover
LD_LIBRARY_PATH=.libs ./test_sim_torn
```

Expected:

```
test_sim_rng: PASS (determinism, seed-sensitivity, stream-independence, guard)
test_sim_crash_recover: PASS -- 64 committed txns survived, uncommitted did not, DB verifies clean (seed 0xdb5eed)
test_sim_torn: PASS -- no silent corruption; every read was correct or cleanly rejected
```

## Replaying a failing seed

Every scenario takes an optional seed argument (default is a fixed constant).
When a run fails, it prints its seed; rerun with that seed to reproduce the
**exact** run — same workload, same fault schedule:

```sh
LD_LIBRARY_PATH=.libs ./test_sim_crash_recover 0xBEEF
LD_LIBRARY_PATH=.libs ./test_sim_torn 0x701234
```

Determinism is proven directly by `test_sim_rng` (same seed → identical draws)
and by the crash pilot re-deriving its expected committed set from the seed
after recovery.

## Proving DST catches real bugs (planted-bug harness)

`sim_inject.h` defines FIVE planted bugs at REAL library sites. Each is caught
by a specific scenario within **K=1** seeds. Because each bug lives in the
library (not the test), activating one means (re)building the library with
`-DDB_DST_INJECT_BUG=<n>`; `dst-bug-inject.sh` automates a dedicated build tree
per bug and asserts the catch, reporting the catch-latency K:

```sh
sh test/sim/dst-bug-inject.sh 8    # K=8 max seeds; prints "CAUGHT bug N at seed 1"
```

| Bug | Site | Caught by |
|---|---|---|
| 1 NODURABLE | `__log_flush_int` skips the log fsync but acks | `test_sim_crash_recover` (loses every "committed" txn) |
| 2 NOCKSUM | `__db_check_chksum` ignores a checksum mismatch | `test_sim_torn` (SILENT-BAD reads) |
| 3 LOSTUPDATE | `__memp_pgwrite` skips a dirty-page write, acks | `test_sim_ckp_crash` (flushed records lost) |
| 4 ABORTNOUNDO | `__txn_abort` skips the undo rollback pass | `test_sim_abort_atomic` (aborted changes left; recovery errors) |
| 5 CKPBADLSN | `__txn_checkpoint` records a wrong (too-forward) checkpoint LSN | `test_sim_ckp_lsn` (post-ckp committed txns lost) |

Measured: **all five caught at K=1**. A normal build (`DB_DST_INJECT_BUG`
undefined) compiles all five out; every scenario passes and the OFF library
exports **0** `__db_sim_*` symbols.

Sweep a scenario over many seeds (from the build dir):

```sh
sh ../test/sim/dst-sweep.sh test_sim_crash_recover 1 200
```

Or swarm the FULL scenario set + fault-mix activation coverage in one run:

```sh
sh ../test/sim/dst-swarm.sh 30 256    # 30 seeds/scenario + 256-seed fault-mix
# => 24 scenarios x 30 seeds -> N pass, 0 fail
#    fault activation: torn ~74% corrupt ~73% stale ~50% enospc ~50% latency ~75% shorteio ~50%
```

The fault-mix swarm (`test_sim_swarm <count> <base>`) is shardable for a
nightly soak: `./test_sim_swarm 100000 0 & ./test_sim_swarm 100000 100000 & ...`.
Measured 2000-seed soak: 0 invariant violations, every fault class activates.

## Determinism guarantees

- **Per-stream PRNG isolation.** Each decision site (IO, FAULT, BUGGIFY, APP)
  draws from its own splitmix64 stream, so adding a draw at one site never
  shifts another site's sequence. Replays survive code change.
- **Determinism guard.** `__db_sim_nondeterminism()` aborts (strict mode) if a
  sim-reachable path reads a real clock / unseeded RNG / raw pid. A harness can
  assert `__db_sim_nondeterminism_count() == 0` to prove a run was fully
  deterministic. (v1 ships the guard; planting calls at BDB's clock/id
  primitives is a follow-up — design doc §4 item G.)
- **Fault config never perturbs the schedule.** All fault knobs draw on the IO
  stream, so enabling/disabling faults does not change what the APP workload
  produces — the same workload replays regardless of fault config.

## Housekeeping

- Scenarios create scratch env dirs (`TESTDIR_sim_*`) under the working dir and
  remove them at start. If a run is killed mid-way, remove leftovers with
  `trash TESTDIR_sim_*` (this repo blocks `rm -rf`).
- **Always recover before verifying** a crashed transactional env — verifying
  an unrecovered crash falsely looks like corruption (see
  `.agents/concurrent-btree-corruption.md`). The crash pilot does this.
