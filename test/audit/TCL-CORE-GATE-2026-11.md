# A11 — TCL core on the PR gate + test/db on a diagnostic build

Review item A11 (Krinsky, Sandstrom): *put the btree/hash/recno + recd* + sdb*
core of the TCL suite on the PR gate (sharded), and run test/db in the
diagnostic build.*

## The gap

Before this change, per PR the TCL signal was five tests — lock001, txn001,
test001, ssi001, ssi002 (`test/MANIFEST` tcl tier, run by
`test/tcl/run_targeted.sh`). The canonical `run_std` suite ran only best-effort
nightly (`ci-extended.yml`, `continue-on-error`). And **no** CI job ran the
`test/db` C regression tier on an `--enable-diagnostic` build, so the engine's
DIAGNOSTIC asserts were never exercised by the fast signal.

So an access-method, recovery or sub-database regression could reach `master`
on the strength of five tests, and a DIAGNOSTIC-only invariant could break
without any gating job noticing.

## What this adds

1. **`test/tcl/run_core_shard.sh`** — runs a shard of the core suite, each test
   in a fresh `tclsh` with a clean `TESTDIR`, emitting one
   `RESULT tcl-core-<shard> <cell> <verdict>` line per cell. The shards:

   | shard | cells | runner |
   |---|---|---|
   | `btree` / `hash` / `recno` | test001–012, test026, test030, test046, test048, test051, test086 (18 each) | `run_method <method> <test>` |
   | `recd-btree` / `recd-hash` | recd001–010 (10 each) | `run_recd <method> <test>` |
   | `sdb` | sdb001–012 (12) | `run_method btree <test>` (sdb sweeps methods internally, skipping queue/heap) |

   Verdict names are `<test>-<method>` for the method sweep and `<test>` for
   sdb, so losing one (test, method) cell is a MISSING in the manifest gate,
   not a silent pass.

2. **`ci.yml` `tcl-core` matrix job** (6 shards) — builds
   `--enable-diagnostic --enable-test --with-tcl`, runs one shard per matrix
   entry, then gates `check_manifest.sh --tier tcl-core-<shard>`. Each shard is
   its own tier because the shards are independent matrix jobs: a per-shard tier
   lets the gate assert exactly this shard's cells without treating the other,
   not-run-in-this-job shards as MISSING.

3. **`ci.yml` `db-diagnostic` job** (Linux gcc) — builds `--enable-diagnostic`
   and runs the `test/db` tier, gating `--tier db`. This is the one place the C
   regression tier runs with DIAGNOSTIC asserts on.

4. **`test/MANIFEST`** — 86 `tcl-core-<shard>` entries (18×3 + 10×2 + 12) so
   `check_manifest.sh` enforces that every cell actually ran.

## TCL harness gotchas handled

- **Fresh tclsh per test, clean TESTDIR.** Each cell is a separate `tclsh`
  invocation sourcing `test.tcl`; `run_recd` already re-execs `tclsh` internally,
  and `test.tcl`'s per-test `cleanup` resets `TESTDIR`.
- **Multi-process tests** (recd uses a child `tclsh`) are fine on Linux; the
  FreeBSD `kern.ipc.shm_use_phys` sysctl note applies only to a FreeBSD runner,
  which this matrix does not use.
- **Subsystem runners vs tests.** The runner invokes the test procs directly
  (`run_method`, `run_recd`), never the `r lock`/`r dead`/`r txn` subsystem
  runners, so no `dead00N`-as-a-command confusion arises.
- **`run_recd` swallows failures** (prints `FAIL:` without re-raising), so the
  runner's verdict is `fail` if either the `catch` caught an error *or* a
  `FAIL`/`FAIL:` line appears in the output.

## Shard timings

Measured on the dev box (c6i.8xlarge, Debian 12, 32 vCPU, `--enable-diagnostic`,
tclsh8.6), each shard run clean (no orphaned processes), gated with
`check_manifest.sh --tier tcl-core-<shard>` (all OK):

| shard | cells | time | verdict |
|---|---|---|---|
| btree | 18 | 75 s | 18 pass |
| hash | 18 | 76 s | 18 pass |
| recno | 18 | 18 s | 18 pass |
| recd-btree | 10 | 894 s (14.9 min) | 10 pass |
| recd-hash | 10 | 893 s (14.9 min) | 10 pass |
| sdb | 12 | 45 s | 12 pass |

The `recd` shards are the long poles (recd001 and recd003 on each method run
many child-process recovery sub-ops: recd001-hash alone is ~370 s, recd003-hash
~356 s; the rest are under 70 s). They are under the ~20-minute target; the
ci.yml job timeout is 45 minutes to leave headroom on a slower hosted runner.
The method and sdb shards are well under two minutes.

Note on measurement: this dev box kills session-backgrounded processes on ssh
disconnect, which once left an orphaned `run_core_shard.sh` appending to a
results file and produced a spurious doubled/failed recd-hash set. Run under
`systemd-run --user --scope`, each shard is clean and passes.
