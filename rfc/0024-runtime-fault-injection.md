# RFC 0024: Runtime fault injection as a DST layer

Status: Draft · Owner: lead · Scope: `test/` and CI only (no engine change unless a test finds a bug)

## 1. Why

The DST we have injects faults **inside** libdb:

- `test/sim`: a sim OS seam, in-process, mostly `DB_PRIVATE`;
- `test/faultinject`: fail the Nth `__os_malloc`.

Both are deterministic and fast, and both see only what libdb's own seams see. Three fault classes reach a real deployment from **outside** those seams, and nothing tests them today:

| Class | Real cause | What we lack |
|---|---|---|
| Network | Lossy, slow, reordering or partitioned links between replicas | Every repmgr/rep test runs on a perfect loopback |
| Storage | `EIO`/`ENOSPC`/short writes from the kernel, fsync failure, and data that never reached the page cache | Faults below `__os_*`, and the uncached I/O path (`DB_DIRECT_DB`) under load |
| Memory | `malloc`/`calloc` failure anywhere in the process, including libc and OpenSSL/tss2 called by libdb | `test/faultinject` sees only libdb's allocation seam |

This RFC adds them as a third DST layer: **runtime fault injection from outside the process, on one Linux host**, using four tools that need no engine change:

| Tool | Injects | How |
|---|---|---|
| `tc` netem + network namespaces | delay, jitter, loss, duplication, reordering, corruption, rate limits, partitions | Each node is its own netns joined by veth pairs; qdiscs are changed while the test runs |
| [libfiu](https://github.com/albertito/libfiu) (`fiu-run -x`, `fiu-ctrl`) | per-call failure of `open/read/pread/write/pwrite/fsync/fdatasync/ftruncate/unlink/rename/send/recv/connect/accept`, and `malloc` | `LD_PRELOAD` POSIX wrappers. Points are enabled at start or remotely mid-run. Deterministic with `FIU_PRNG_SEED` |
| [libdirectio](https://github.com/gburd/libdirectio) | sets `O_DIRECT` on every regular file the process opens | `LD_PRELOAD`. Removes the page cache, so a crash loses everything not yet on disk, and I/O alignment bugs surface |
| [libumem](https://codeberg.org/gregburd/libumem) and [FAINT](https://github.com/misc0110/faint) | **libumem:** guard redzones, use-after-free detection, per-buffer audit stacks, leak reports at exit. **FAINT:** systematic "fail the allocation at call site N" over a profiled run | `LD_PRELOAD` for both. FAINT profiles first, then re-runs once per injection position |

### 1.1 Constraints

- Linux only. The other operating systems keep their existing test tiers.
- One host. Everything runs on the shared dev box and on a GitHub Linux runner. Network namespaces need `sudo`; hosted runners have passwordless sudo.
- No change to libdb unless a test finds a bug.
- Every fault test is **reproducible**: it prints the seed and the full fault schedule, and re-running with the same seed replays the same faults. Timing under real concurrency is not deterministic, so the test re-runs a failing seed N times and reports the reproduction rate.

## 2. Design

### 2.1 One harness: `test/dst-rt/`

```
test/dst-rt/
  lib/
    fi_env.sh       tool discovery and SKIP rules (each tool optional; a missing tool SKIPs that arm)
    fi_net.sh       netns/veth topology, netem apply/clear, partition/heal, cleanup trap
    fi_fiu.sh       fiu-run/fiu-ctrl wrappers, seeded schedules, named fault plans
    fi_check.sh     invariant oracles (§2.4)
  node/
    rtnode.c        one libdb node: a repmgr site, a base-API site, or a local app; driven by a command pipe
    rtload.c        a workload generator: seeded txns, writes a client-side ack log for the oracle
  scenarios/        one script per scenario, each with a seed argument
  run.sh            the swarm driver: scenario x seed x fault plan, aggregate verdicts
  README.md
```

**Nodes are real processes.** Each site is a separate `rtnode` process with its own env home. HA runs place each in its own netns: `ns-a`, `ns-b`, `ns-c`, joined by veth pairs to a bridge. netem is applied per veth, so a partition is "drop everything on `veth-c`", and asymmetric faults are possible.

**Workload with a client-side ack log.** `rtload` records every operation it believes committed: key, value, txn id, and for replication the LSN plus the ack policy outcome. After the run, the oracle compares that log with the surviving state. This is the Jepsen pattern, scaled down to one host.

### 2.2 Fault plans

A fault plan is a small, named, seeded schedule. Examples:

| Plan | Faults |
|---|---|
| `net-flaky` | 5–50 ms jitter, 1% loss, 0.5% reorder, on all links |
| `net-partition` | isolate the master for T seconds, then heal; repeat |
| `net-asym` | A→B drops, B→A clean |
| `net-slow-rep` | rate-limit one client to 1 Mbit/s so its replication falls behind |
| `io-eio-write` | `posix/io/rw/pwrite` fails with EIO at probability p, on one node |
| `io-fsync-fail` | `fdatasync` returns EIO once at a seeded point. **Expected: libdb panics or returns the error. It must never report a commit that is not durable** |
| `io-enospc` | `pwrite` returns ENOSPC after N bytes |
| `io-short` | `pwrite`/`pread` return short counts (libfiu `reduce: count`) |
| `direct` | libdirectio on every node: no page cache, so a crash loses all unflushed data |
| `mem-faint` | FAINT systematic sweep over a bounded single-node workload |
| `mem-fiu-random` | `libc/mm/malloc` fails at probability p during an HA run |
| `mem-umem` | libumem `UMEM_DEBUG=default`; any redzone or UAF report fails the test; leaks are reported |
| `kill` | `SIGKILL` a node at a seeded time, restart it with `DB_RECOVER` |

Plans compose: `net-flaky+io-eio-write+kill` is valid. The swarm draws combinations by seed.

### 2.3 Scenarios

| # | Scenario | Topology | Main question |
|---|---|---|---|
| S1 | Local crash-durability | 1 node, `direct` + `kill` | Is every acked commit present after recovery, with the page cache out of the picture? |
| S2 | I/O error handling | 1 node, `io-*` plans | Does an injected EIO/ENOSPC/short I/O return an error or panic, and never corrupt or lose an acked commit? Is the env recoverable afterwards? |
| S3 | Allocation failure, whole process | 1 node, `mem-faint`, `mem-fiu-random` | Does any malloc failure, including inside libc/OpenSSL/tss2, crash, hang or leak a lock? This extends `test/faultinject` past libdb's seam |
| S4 | repmgr 3-site under network faults | 3 netns, `net-*` + `kill` | Is there at most one master per generation? Is every commit acked under `DB_REPMGR_ACKS_QUORUM` present on the elected master after heal? Do all sites converge to an identical log and data? |
| S5 | Partition and election | 3 netns, `net-partition` | Does the minority side stop accepting acked writes? Does the majority elect? Does the old master step down on heal, with no lost acked writes? |
| S6 | Replication + storage fault | 3 netns, `io-eio-write` on a client | Does a client with a failing disk fall out without stalling the group? Does it rejoin after recovery? |
| S7 | repmgr TLS under network faults | 3 netns, TLS (#241), `net-flaky`/`net-partition` | Do handshakes retry, does traffic stay encrypted, and does the group converge? |
| S8 | XA 2PC under faults | XA test TM (#240) + 2 RMs (separate envs), `kill`/`io-fsync-fail` | After a crash between prepare and commit, does recovery report the in-doubt txn, does the TM resolve it, and does no RM commit while another aborts? |
| S9 | Soak | S4 + S2 + S3 plans rotating for hours | No unbounded growth in lockers, mutexes, region memory, log files or RSS; no invariant violation |

### 2.4 Invariant oracles (`fi_check.sh` + `rtcheck`)

After every run, whatever the faults:

1. **Durability:** every operation in the ack log is present with its value, after recovery and, for HA, on the final master.
2. **No phantom commits:** nothing is present that the ack log marks as aborted or failed. A write whose ack was lost is allowed either way.
3. **Integrity:** `DB->verify` passes on every database of every node, and `db_recover` succeeds.
4. **Convergence (HA):** after heal plus quiesce, all sites have the same `log_stat` LSN and identical `db_dump` output.
5. **Single master:** the repmgr event log shows at most one master per election generation.
6. **XA atomicity:** for every global txn, all RMs committed or all aborted.
7. **Error semantics:** an injected fault gives an error code, a `DB_RUNRECOVERY` panic, or a clean retry, never a crash. ASan/umem reports count as failures.

A failed run keeps its env homes, the logs and the fault schedule, and prints the one-line reproduce command.

### 2.5 Determinism and replay

- **One seed drives everything:** the workload ops, the fault schedule (`FIU_PRNG_SEED`, netem parameters, kill times) and the scenario timing.
- **Exact replay:**
  - single-node scenarios with libfiu point counts (`failnum`), FAINT positions and `kill` at an op count, not a time;
  - single-node, a failing seed replays exactly.
- **Statistical replay:**
  - network timing and multi-process interleaving;
  - the runner retries a failing seed 10 times and reports k/10.
  - A k ≥ 1 is a bug. A seed with k = 0 on retry is still logged as a flake for triage.
- **Link to `test/sim`:** any fault that reproduces deterministically gets a sim-level regression test, where the sim seam can express it.

### 2.6 Tiers and CI

| Tier | Where | Budget | Contents |
|---|---|---|---|
| `dst-rt-pr` | GitHub Linux runner, every PR | ≤ 20 min | S1, S2 (2 seeds/plan), S3 (bounded FAINT), S4/S5 (3 seeds each, short) |
| `dst-rt-nightly` | Dev box (or a scheduled runner) | ~2 h | All scenarios × 50 seeds, plus plan combinations |
| `dst-rt-soak` | Dev box, manual or weekly | 8–24 h | S9 |

- Each tool can be skipped on its own. A runner without netns permissions SKIPs S4–S8 and does not fail the job.
- The manifest gate (`test/MANIFEST`) records every scenario cell, so a SKIP is visible.
- Tools are installed from distro packages where they exist: `libfiu`/`fiu-utils` are in Debian; `tc` and `ip` are in `iproute2`.
- libdirectio, libumem and FAINT are built from source in a cached CI step, pinned to a commit.

### 2.7 Interfaces (fixed, so the pieces can be built in parallel)

**Ack log.** Every workload writer (`rtload`, and `rtnode` in HA mode) appends one line per operation to `<run>/ack.<node>.log`, flushed before the op is reported done:

```
<seq> <txnid> <op> <db> <key> <vhash> <outcome> [<lsn-file>/<lsn-off>]
```

| Field | Meaning |
|---|---|
| `op` | `put`, `del` or `txn` (multi-op txns log their member ops with the same txnid, then one `txn` line with the outcome) |
| `vhash` | 64-bit FNV-1a of the value, in hex; `-` for `del` |
| `outcome` | `ACK` (commit returned 0, and for repmgr the ack policy was met: `DB_REP_PERMANENT`), `FAIL` (an error was returned, so it must not be visible), or `UNKNOWN` (the process died or the ack was lost, so it may be visible) |

The log is written with `O_APPEND` and `write(2)` on a file outside the libdb env homes, which libfiu's I/O plans exclude by path (§2.2). That keeps the oracle's own I/O out of the fault scope.

**`rtcheck`.** `rtcheck --ack <files...> --home <env-home> [--home ...]` opens each home with `DB_RECOVER`, then:

- applies the §2.4 rules 1–3 and 7;
- prints `PASS` or a list of violations;
- exits 0, or exits 1.

The HA convergence and single-master rules (4–5) are in `fi_check.sh`, which compares `db_dump` and `db_stat -r` across homes.

**`rtnode` control.** A node reads newline commands on a FIFO `<run>/ctl.<node>` and writes events (`ROLE master|client gen=<n>`, `ACK seq=<n>`, `ERR <code>`, `READY`) to `<run>/ev.<node>.log`:

| Command | Effect |
|---|---|
| `start` / `stop` | start or stop the site |
| `load <n> <seed>` | run n workload ops (master only) |
| `checkpoint` | take a checkpoint |
| `quiesce` | wait for the replication queue to drain |
| `exit` | exit cleanly |

Kills are external (`kill -9 <pid>`).

## 3. Out of scope

- Windows, macOS and the BSDs: their existing tiers stand.
- Byzantine faults (forged messages).
- Clock skew across nodes: `test/sim` already covers clock skew in-process. netns nodes share the host clock, which is a known limit, written down here.

## 4. Phasing

| Phase | Deliverable | Gate |
|---|---|---|
| F1 | `test/dst-rt` scaffold, `fi_env`/`fi_check`, `rtnode`/`rtload`/`rtcheck`; S1, S2, S3 single-node | Each plan's sabotage arm FAILs (e.g. an ack log entry removed, or durability checked against a no-sync commit); PR tier green |
| F2 | `fi_net` netns topology; S4, S5 repmgr | Sabotage: `DB_REPMGR_ACKS_NONE` with kill-master must be **caught** losing acked writes, so the oracle has teeth |
| F3 | S6, S7 (TLS), S8 (XA) | Each scenario has a fault that must produce the expected recovery path |
| F4 | `run.sh` swarm, nightly and soak tiers, CI workflow, docs | Nightly runs clean for 3 consecutive nights, or every finding is a filed issue with a reproducer |

Every bug a scenario finds is fixed through the normal flow: master first, a reproducer test, and an LTS backport if the bug is on that line.
