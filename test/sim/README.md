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
| `sim_core.c` | the DST core (PRNG, guard, buggify, I/O fault knobs, write-back crash model) — linked into libdb under `--enable-dst` |
| `sim_os_hooks.c` | bridge from `__os_*` (os_rw.c, os_fsync.c) to the core — linked into libdb under `--enable-dst` |
| `test_sim_rng.c` | pilot: PRNG determinism / seed-sensitivity / stream independence / guard |
| `test_sim_crash_recover.c` | **capstone** pilot: durable txns survive a crash; recovery is clean |
| `test_sim_torn.c` | pilot: corrupt reads are caught by checksum or invisible — never silently wrong |

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
  make dst_tests'       # builds the three pilot executables
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

`sim_inject.h` defines planted-bug ids. A dedicated build activates exactly one:

```sh
cd build_unix
make test_sim_crash_recover DSTBUG=1   # plant bug 1 (NODURABLE)
LD_LIBRARY_PATH=.libs ./test_sim_crash_recover
```

The bug build is *expected to fail* (an invariant fires) — that failure is the
"DST caught it" signal. A planted bug the sweep does **not** catch is a hole in
the DST coverage of that safety property. v1 ships the harness and the
`NODURABLE` hook; wiring the fsync-skip into `__log_flush` itself (so the
write-back durable frontier truly drops the un-fsynced tail) is the immediate
next step — see design doc §4 item A.

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
