# RFC 0024 runtime fault-injection harness (`test/dst-rt`)

A third DST layer: faults injected from **outside** the libdb process, on one
Linux host. The in-process DST (`test/sim`, `test/faultinject`) sees only what
libdb's own seams see; this layer covers the three fault classes that reach a
real deployment from below those seams — network, storage and
whole-process memory — using tools that need no engine change.

Spec: [`rfc/0024-runtime-fault-injection.md`](../../rfc/0024-runtime-fault-injection.md).
§2.7 fixes the interfaces. This directory is the F1 single-node half (S1, S2,
S3, S9-lite) plus the shared node binaries; the netns/HA half (S4–S8, F2) adds
`lib/fi_net.sh` and `scenarios/s4_*`/`s5_*`.

## Layout

```
test/dst-rt/
  lib/
    fi_env.sh    tool discovery + SKIP rules (each tool optional)
    fi_fiu.sh    named, seeded libfiu fault plans (io-*, mem-*)
    fi_check.sh  invariant oracle wrappers (single-node rtcheck + HA rules 4-6)
    fi_net.sh    netns/veth + tc/netem topology (F2)
  node/
    fi_acklog.h  the client-side ACK-log writer (raw-syscall, wrapper-bypassing)
    rtload.c     seeded single-node workload generator
    rtnode.c     a long-lived node (local txn env, or a repmgr site for F2)
    rtcheck.c    the invariant oracle (§2.4 rules 1-3, 7)
  scenarios/     one script per scenario; each prints a reproduce line
  tools/
    install.sh         reproducible tool install (apt + pinned source)
    directio_match.c   path-filtered O_DIRECT shim (finding F1-001)
  run.sh         the swarm driver: scenario x seed, per-arm verdicts, k/10 retry
  FINDINGS.md    issue-style write-ups, each with a reproducer
```

## Tools (`tools/install.sh`)

| Tool | Injects | Source |
|---|---|---|
| libfiu (`fiu-run -x`) | per-syscall EIO/ENOSPC/short-count on read/write/pwrite/fsync/fdatasync; malloc/calloc/realloc failure anywhere in the process | `apt: fiu-utils libfiu-dev` |
| `tc` netem + netns | delay/loss/reorder/partition (F2) | `apt: iproute2` |
| libdirectio | blanket `O_DIRECT` on every regular file | pinned source |
| libumem | guard redzones + fill-pattern (use-after-free, overflow) | pinned source |
| FAINT | one allocation failure per profiled call site | pinned source (64-bit only) |

`install.sh` runs on the dev box and on a GitHub `ubuntu-latest` runner; the
source-built tools land under `$PREFIX` (default `/opt/dst-rt`) and CI caches
them keyed on the script's content (which pins the commits). Each tool is
**optional**: a missing tool makes the arms that need it SKIP, never fail, and
the SKIP is visible in the MANIFEST.

Gotchas the install encodes (so you do not rediscover them):

- **libfiu `enable_random` syntax** is ONE comma-joined argument:
  `enable_random name=NAME,probability=P[,failinfo=N]`. A space silently
  disables the probabilistic roll — the plan then never fires.
- **FAINT 64-bit only:** the upstream `all` target also builds a 32-bit
  `fault_inject32.so` (needs the 32-bit multilib). We build the 64-bit objects,
  stub the 32-bit blob, and hand-link; `map_c.o` must be compiled with `gcc`
  (as C++ the `_map_init`/`map` symbols `faint.c` references get mangled).
- **libumem `UMEM_DEBUG=default` aborts** any program at startup on the dev box
  (the `audit` component); S3 falls back to `UMEM_DEBUG=guards,contents`, the
  detection half, which runs clean and still catches overflow + UAF. See
  FINDINGS F1-002.

## The oracle (`rtcheck`) and the ACK log

Every workload writer appends one line per op to an ACK log OUTSIDE the env
homes (§2.7):

```
<seq> <txnid> <op> <db> <key> <vhash> <outcome>
```

`outcome` is `ACK` (commit returned 0), `FAIL` (an error was returned, so it
must not be visible) or inferred `UNKNOWN` (the txn's commit line never
appeared — the process died — so the op may be visible either way). The vhash
is a 64-bit FNV-1a of the value. The ACK-log write uses a raw
`syscall(SYS_write)` + `syscall(SYS_fdatasync)` so libfiu's symbol-level POSIX
wrappers never fail the oracle's own log.

`rtcheck --ack FILE... --home DIR...` opens each home with `DB_RECOVER` and
enforces §2.4 rules 1-3 and 7: every ACKed op present with its value; nothing
FAILed visible; `DB->verify` clean; recovery succeeds. It orders ops by global
append position across ACK files (robust to per-run `seq`/`txnid` resets) and
reports its reach:

```
PASS (checked N acked, M failed, K unknown)
FAIL: X violation(s) (checked N acked, ...)
VACUOUS: 0 acked ops checked (...)    -> exit 1
```

**Non-vacuity:** a PASS over zero acked ops is the vacuous-green shape; rtcheck
exits 1 `VACUOUS` when N == 0, and every scenario arm reports its acked count.

## Scenarios

Each is `scenarios/sN_*.sh <seed>`, prints a reproduce line, and emits one
verdict per arm. Every scenario has a **sabotage arm** that proves the oracle
catches a real failure.

| # | Scenario | Arms | Sabotage (teeth) |
|---|---|---|---|
| S1 | crash-durability | normal (sync), native-directio (`DB_DIRECT_DB`+`DB_LOG_DIRECT`), preload-directio (shim) — all killed at a seeded op count, recovered | `--nosync` commits logged ACK lose libdb's in-process log buffer on the kill; the oracle must catch the lost acked writes |
| S2 | I/O errors | one arm per libfiu io plan: eio-write, fsync-fail, enospc, short | delete an acked key behind the oracle's back; rtcheck must report it missing |
| S3 | allocation failure | FAINT sweep, libfiu malloc-random, libumem redzone | a planted 64-into-16 heap overflow; umem must abort |
| S9-lite | 10-min soak | cycles the S1-S3 plans over one reused env home, watching for unbounded growth | — (survival + the per-cycle oracle) |

## Determinism and replay (§2.5)

One seed drives the workload op schedule (splitmix64), the fault schedule
(`FIU_PRNG_SEED`), and the kill point (`--kill-at` is an op count, not a time).
Single-node scenarios replay exactly. `run.sh` retries a failing seed up to 10
times and reports `k/10`: a `k >= 1` is a confirmed failure; `k = 0` on retry
is a logged flake.

## Running

```sh
# from a configured build dir with libdb.a (build with --enable-diagnostic;
# add --enable-o_direct for the S1 native-directio arm)
sudo PREFIX=/opt/dst-rt sh ../test/dst-rt/tools/install.sh     # once
BUILD=$(pwd) DSTRT_DIR=../test/dst-rt sh ../test/dst-rt/run.sh --tier pr
../test/check_manifest.sh --tier dst-rt
```

CI: `.github/workflows/dst-rt.yml` runs the PR tier (≤ 20 min) on every PR
touching the harness or the engine I/O paths, and the nightly tier (20 seeds +
soak) on a schedule.

## Findings

Real bugs a scenario finds are issue-style write-ups in
[`FINDINGS.md`](FINDINGS.md), each with a reproducer. Engine fixes go on their
own branches from `origin/master`, never on `test/dst-rt`. F1-001 (libdb cannot
open under a *blanket* O_DIRECT; the native direct path is clean) and F1-002
(libumem `audit` mode aborts on the dev box) are recorded there.
