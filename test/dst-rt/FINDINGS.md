<!--
Copyright (c) 2026 libdb contributors.  All rights reserved.
SPDX-License-Identifier: Sleepycat
-->
# RFC 0024 runtime-fault DST: findings (F2 — multi-node HA and XA)

This file records what the HA and XA scenarios (S4–S8) found on one Linux host
(the shared dev box, Debian 12, 32 vCPU, `sch_netem`, netns, libfiu 1.1,
OpenSSL 3). Each finding has a reproducer and, per RFC 0024 §2.5, a k/10
reproduction rate. Engine bugs get their own branch from `origin/master` (or
the feature branch for TLS/XA), never a fix on `test/dst-rt`.

## Scope and method

- **S4** repmgr 3-site under `net-flaky`, `net-asym`, `net-slow-rep`, `kill`.
- **S5** partition the master into the minority + heal; plus the **sabotage**
  arm (`DB_REPMGR_ACKS_NONE` + partition + kill of the old master), which the
  oracle MUST catch losing acked writes.
- **S6** `io-eio-write` (libfiu) on one client under load.
- **S7** repmgr TLS (RFC 0018) under `net-flaky`/`net-partition`; plus the
  **pin** arm (one site presents an unpinned cert, must never join).
- **S8** XA 2PC (PR #240 harness) under `kill` and `io-fsync-fail`.

Oracle (RFC 0024 §2.4): rule 1 durability, rule 2 no-phantom, rule 3 integrity
(`DB->verify` + `db_recover`), rule 4 convergence (identical `db_dump` across
live sites), rule 5 single-master-per-generation, rule 6 XA atomicity, rule 7
error-semantics (an injected fault gives an error or a controlled panic, never
a crash or a durable lie). The oracle is proven to have teeth two ways: every
run reports `rtcheck: PASS (checked N acked, …)` and FAILs vacuous if N == 0,
and the S5 sabotage / S8 control arms deliberately trip it.

## Harness defects found and fixed (test-only; no libdb bug)

These were found while bringing the oracle up and are fixed in the harness
(`test/dst-rt`, coordinated with F1). None is a libdb defect.

1. **Vacuous oracle pass.** `rtcheck` could print `PASS` having looked up zero
   acked ops. Fixed: it prints `PASS (checked N acked, M failed, K unknown)`
   and FAILs `VACUOUS` when N == 0; the HA durability check also fails on 0
   acked keys.
2. **Cross-node "latest wins" used append/per-batch order.** After a failover
   the old and new masters are different processes, so append order and a
   per-process counter are not comparable. Fixed: ack lines carry the commit
   `(gen, LSN)` from `DB_TXN->set_commit_token`, and `rtcheck` orders HA entries
   by `(gen, LSN)`; single-node entries keep append order. RFC §2.7 updated.
3. **Non-unique write identity → false phantoms.** The workload value and the
   ack-log txnid were per-batch, so a key written in two batches collided and a
   baseline-ACKed key resolved to a later batch's FAIL — a *false* rule-2
   phantom. Fixed: a per-process monotonic counter is both the ack txnid and
   part of the value (`n<node>-g<gen>-w<ctr>-k<key>`), a true per-write
   identity. (Diagnosed with the lead; it looked like an aborted-txn-visible
   libdb bug but was the test.)
4. **ACK vs UNKNOWN labelling.** A put-stage failure (including a demotion
   `BDB0111` read-only) aborts the txn and can never be visible on a replica →
   stays `FAIL`. Only a commit-stage replication error/demotion, where the
   commit record may already be logged and shipped, is `UNKNOWN`. A QUORUM
   master that loses quorum marks its writes `UNKNOWN` via a thread-local flag
   set in the synchronous `DB_EVENT_REP_PERM_FAILED` callback (single committer
   per node; no global counter).
5. **repmgr-in-netns bring-up.** Nodes must start sequentially (the creator
   forms the group and becomes master before any client calls `repmgr_start`,
   else `DB_REP_UNAVAIL` + panic); heartbeat/election timeouts must be set
   (rtnode `--hb-send/--hb-mon/--elect-to/--conn-retry/--ack-to`) or a clean
   partition never triggers an election.

## Scenario results (20 seeds each)

_Filled in as the sweeps complete; see the per-scenario tables below._

### S4 — repmgr 3-site under network faults
_pending sweep_

### S5 — partition + election (normal and sabotage arms)
_pending sweep_

### S6 — replication + storage fault (io-eio-write on a client)
_pending sweep_

### S7 — repmgr TLS under network faults (positive and pin arms)
_pending sweep_

### S8 — XA 2PC under kill + io-fsync-fail
_pending sweep_

## Engine findings (libdb bugs)

None so far. Under `io-fsync-fail` (S8) libdb does the RFC-expected thing: it
returns `DB_RUNRECOVERY`/an XA error and panics rather than report a durable
commit it did not make — rule 7 holds, and no XA split (rule 6) was observed.

## Reproduce

```
# on a Linux host with sudo, iproute2 (tc), netns, libfiu, openssl:
BUILD=<plain build_unix>  bash test/dst-rt/scenarios/s4_repmgr_netfaults.sh <seed>
BUILD=<plain build_unix>  bash test/dst-rt/scenarios/s5_partition_election.sh <seed> [--sabotage]
BUILD=<plain build_unix>  bash test/dst-rt/scenarios/s6_repl_storage_fault.sh <seed>
BUILD=<--enable-tls build> bash test/dst-rt/scenarios/s7_tls_netfaults.sh <seed> [--pin]
BUILD=<test/xa-harness build> bash test/dst-rt/scenarios/s8_xa_faults.sh <seed>
# or the whole HA set through the swarm driver:
DSTRT_HA=1 BUILD=<build> sh test/dst-rt/run.sh --tier nightly --seeds "1 2 3 ... 20"
```
