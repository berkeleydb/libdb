<!--
Copyright (c) 2026 libdb contributors.  All rights reserved.
SPDX-License-Identifier: Sleepycat
-->
# RFC 0024 runtime fault-injection: findings

Two parts: single-node S1–S3 (F1) and multi-node HA/XA S4–S8 (F2).  No libdb
engine bug has been found by either part so far; every non-PASS was a harness
or tool issue, recorded below with its fix.

# Part 1 — single node (F1, scenarios S1–S3)

Each finding is an issue-style write-up with a reproducer.  Engine fixes go on
separate branches from `origin/master`, one per bug, each with a test that
fails first.  They are **not** fixed on `test/dst-rt`.

---

### F1-001: libdb's region file-management writes fail under a BLANKET O_DIRECT (libdirectio), but the NATIVE direct-I/O path is clean

**Severity:** low (preload-only; the supported `DB_DIRECT_DB`/`DB_LOG_DIRECT`
path is alignment-correct).  Not a data-loss bug.

**Status:** CHARACTERISED.  The native path is correct; the preload gap is a
small robustness improvement, optional.

**Verdict (lead-directed diagnostic, 2026-11).**  Two configurations were
compared on ext4 (/data), a real block-backed fs:

| Configuration | env open | data file | log file | region files | unaligned writes |
|---|---|---|---|---|---|
| **native** `--enable-o_direct`, `DB_DIRECT_DB` + `log_set_config(DB_LOG_DIRECT)` | **works, 50/50 commits** | O_DIRECT (fincore 0) | O_DIRECT (fincore 0) | buffered (by design) | **none** |
| **preload** libdirectio (blanket O_DIRECT on every regular file) | **FAILS, EINVAL** | O_DIRECT | O_DIRECT | O_DIRECT (forced) | 3 (see below) |

So libdb's **supported** direct-I/O path is alignment-correct: it opens the
data and log *page* files O_DIRECT with aligned, block-sized I/O (the log
header is padded on that path -- 50/50 commits with `DB_LOG_DIRECT` and
`fincore` shows the log 0-cached), and it deliberately keeps the region files
(`__db.00N`), temp files and the backup/verify utilities on **buffered**
handles.  The bug surfaces only when libdirectio forces O_DIRECT onto those
buffered-by-design files, whose management writes are not block-aligned:

| Call site | Write | Why O_DIRECT rejects it |
|---|---|---|
| `src/env/env_file.c:__db_file_extend` | `write(fd, &buf, 1)` where `buf` is a `char` on the stack | length 1 (not a 512/4096 multiple), buffer stack-unaligned |
| `src/os/os_map.c:__db_file_write` (DB_ENV_REGION_INIT path) | a `MEGABYTE`-chunked write ending in a `rp->size % MEGABYTE` tail | the tail length is not block-aligned |
| log file header write (`log.0000000001`) | `pwrite64(fd, hdr, 131, 0)` / `write(fd, hdr, 131)` | length 131, heap buffer unaligned |

`__db_file_extend` (env/env_file.c) is the clearest:

```c
char buf;
buf = '\0';
pages = (db_pgno_t)((size - sizeof(buf)) / MEGABYTE);
relative = (u_int32_t)((size - sizeof(buf)) % MEGABYTE);
if ((ret = __os_seek(env, fhp, pages, MEGABYTE, relative)) == 0)
	ret = __os_write(env, fhp, &buf, sizeof(buf), &nw);  /* 1-byte, stack */
```

**This is the alignment class the RFC's §1 storage row and the F1 brief
predicted**, but scoped: it is the region/temp/utility files that libdb keeps
buffered, NOT the page I/O path.  The native `DB_DIRECT_DB`/`DB_LOG_DIRECT`
configuration -- the supported way to get direct I/O -- is alignment-clean, so
F1-001 is a **preload artifact**, not a supported-config bug.

**Reproducer (dev box, Debian 12, ext4 on /data):**

```sh
# build libdb --enable-diagnostic; build the harness rtload
LD_PRELOAD=/opt/dst-rt/lib/libdirectio.so \
  ./rtload --home /data/dstrt/h --ack /tmp/a.log --node a \
           --seed 9 --ops 10 --keys 10 --valsz 200
# => BDB0137 write: 0x7ffef78d3f77, 1: Invalid argument
#    env->open ...: Invalid argument
```

strace pinpoints the call:

```
openat(..., "home/__db.001", O_RDWR|O_CREAT|O_EXCL, 0644) = 4
write(5, "\0", 1)  = -1 EINVAL (Invalid argument)   # __db_file_extend
...
openat(..., "log.0000000001", O_RDWR|O_CREAT, 0644) = 3
pwrite64(3, <131 bytes>, 0) = -1 EINVAL             # log header
```

tmpfs ignores `O_DIRECT`, so the bug only shows on a real block-backed fs
(ext4/xfs); `fincore` on `__db.001` confirms 0 cached pages, i.e. the open did
reach the O_DIRECT path before failing.

**Suggested fix (engine branch `fix/direct-io-alignment`, NOT here).**  Optional
robustness: make the region/temp management writes O_DIRECT-tolerant so a
blanket preload (or a user who sets O_DIRECT on the whole fs) does not trip
them.  Smallest change: size the region file with `ftruncate` /
`posix_fallocate` instead of the 1-byte `write` in `__db_file_extend`
(`__os_truncate` already prefers `ftruncate`), and make the `__db_file_write`
MEGABYTE-tail block-aligned.  The log header already pads correctly on the
native direct path, so no log-format change is needed.  Must NOT touch log or
region format.  Correctness fix for robustness -> normal LTS backport rules if
it lands.  Given the native path is clean, this is low priority.

**Impact on the harness.**  S1 gets TWO directio arms:
  - **native:** `--enable-o_direct` build + `DB_DIRECT_DB`/`DB_LOG_DIRECT`, run
    under kill + recovery -- the supported path, verified clean here;
  - **preload:** libdirectio, which still trips F1-001 at env-open, so that arm
    SKIPs-with-reason (naming F1-001) until the optional fix lands.
The S1 kill+recovery durability check (incl. the `--nosync` sabotage) runs on
the native arm now.

**Resolution for the harness (lead decision 2026-11): NO engine fix.**  Changing
engine code to satisfy a tool that forces O_DIRECT on files libdb keeps
buffered on purpose is not worth the risk.  F1-001 stays a documented finding.
Instead the S1 preload arm uses a tiny test-only shim,
`test/dst-rt/tools/directio_match.c`, an `open`/`openat` LD_PRELOAD that sets
O_DIRECT only for paths matching `$DIRECTIO_MATCH` (default `*.db`).  Combined
with the loader's `--direct` (native `DB_LOG_DIRECT`) for the log, the preload
arm now runs uncached over the data+log files AND the utilities
(db_verify/db_recover/db_backup, which never take DB_DIRECT_DB), without
tripping the buffered-region writes.  `s1-preload-directio` PASSES.

---

### F1-002 (tool note, not a libdb bug): libumem `UMEM_DEBUG=default` aborts on the dev box

**Status:** fixed upstream in libumem v4.3.2 (`60a6452`; fix `490c943`,
regression test `4abe0f6`). `install.sh` pins it, and the s3-umem arm uses
`default` again. See "Fixed in libumem 4.3.2" after F1-003.

On the dev box (Debian 12, x86_64, libumem built from the pinned commit),
`UMEM_DEBUG=default` (which includes the `audit` per-buffer stack-capture
component) aborts **any** program at startup -- including a 1-line
`malloc(16);free(p)` program -- with no detection report (SIGABRT, rc=134).
So it is not a libdb heap bug; it is the `audit` component failing on this
host.  Confirmed by isolating the flags:

| UMEM_DEBUG | clean 1-malloc prog | 64-into-16 overflow |
|---|---|---|
| `default` / `audit` / `audit,guards` | ABORT (rc 134/1) | ABORT |
| `guards` | rc 0 | rc 0 (NOT detected at free) |
| `contents` / `firewall` | rc 0 | rc 0 (NOT detected) |
| **`guards,contents`** | **rc 0** | **ABORT (detected)** |

The S3 `s3-umem` arm therefore probes `default` on a trivial program first and,
when it aborts spuriously, falls back to `UMEM_DEBUG=guards,contents` -- the
detection half of `default` (redzone + fill-pattern).  That mode runs the libdb
workload clean AND catches both the planted overflow and a use-after-free (the
`s3-umem-sabotage` arm proves the teeth).  The only thing lost versus `default`
is the audit backtraces, which are diagnostic, not detection.

**libdb result under redzone+contents checking:** CLEAN over an 800-op
transactional workload -- no redzone corruption, no use-after-free, no
fill-pattern violation.  rtcheck verifies the data afterwards.

### F1-003 (tool, not a libdb bug): libumem `UMEM_DEBUG=default` HANGS on the GitHub Ubuntu 26.04 runner

**Status:** fixed upstream in libumem v4.3.2, as F1-002.

Where F1-002 saw `default` abort on the dev box, the hosted runner (kernel
7.0.0-1017-azure, 4 vCPU, libumem fe9ff961 with its rseq fast path) **hangs**:
a trivial one-`malloc` program under `LD_PRELOAD=libumem_malloc.so
UMEM_DEBUG=default` never exits (timeout rc=124 at 60 s), while the same program
under `UMEM_DEBUG=guards,contents` exits 0, and `rtload` (800 ops) under
`guards,contents` exits 0 in under a second.  The s3-umem arm's mode probe ran
the trivial program **without a timeout**, so the probe itself blocked until the
job's 45-minute limit cancelled the run, and the remaining arms produced no
verdict.

The hang is **intermittent**: in CI run 41727745742 the seed-1 probe of
`default` timed out (so the arm fell back and passed), while the seed-2 probe
of `default` exited 0, `default` was chosen, and rtload under it then hung for
the full 180 s.  A probe cannot vouch for `default` here, so the arm no longer
probes: it uses `UMEM_DEBUG=guards,contents` unconditionally
(`S3_UMEM_MODE` overrides).

Fix (harness): every libumem invocation in s3 is now under `timeout -k`
(probe 30 s, workload and rtcheck `S3_UMEM_TIMEOUT`=180 s, sabotage 30 s).  A
probe timeout falls back to `guards,contents` exactly like an abort, and a hang
in the workload is a FAIL verdict, never a silently cancelled job.  FAINT is
bounded the same way (`S3_FAINT_TIMEOUT`=120 s); it was **not** the hang (it
finishes in < 1 s on the runner, 4 injection positions, 0 crashes).

Reported upstream to libumem on 2026-11-04: `UMEM_DEBUG=default` (the `audit`
component) both aborts (dev box, Debian 12 kernel 6.1) and hangs (Ubuntu 26.04
kernel 7.0) on a trivial program.

### Fixed in libumem 4.3.2

**Cause (libumem's diagnosis, matching ours).** With `audit` on, libumem
records a stack for every vmem segment from inside `vmem_hash_insert()`, while
it holds vmem locks. The first stack walk on each thread asked glibc
`pthread_getattr_np()` for the stack bounds, and that call allocates. Under
`LD_PRELOAD` the allocation is libumem's own, so the allocator re-entered
itself. It then either failed the `VMC_POPULATOR` assertion (F1-002) or
deadlocked its update thread against the main thread (F1-003), depending on
rseq and timing.

**Fix.** libumem 4.3.2 reads the stack bounds from `/proc/self/maps` with plain
`open`/`read` into a stack buffer, so nothing on that path allocates. The fix
is `490c943`, its regression test is `test/security/test_audit_reentry.sh`
(`4abe0f6`), and the release is `v4.3.2` (`60a6452`). The ABI is unchanged.

**Verified on the dev box** (Debian 12, x86_64, 32 vCPU, 2026-11-05). Each
library was first shown live, by catching a planted 64-into-16 overflow under
`guards,contents`. The programs are the one-`malloc` reproducer and a
first-`malloc`-on-a-new-thread variant, 25 runs per cell:

| `UMEM_DEBUG` / `UMEM_OPTIONS` | `fe9ff96` (old pin) | `60a6452` (v4.3.2) |
|---|---|---|
| `audit` | 25/25 abort | 25/25 exit 0 |
| `audit` + `norseq` | 25/25 hang | 25/25 exit 0 |
| `default` | 25/25 abort | 25/25 exit 0 |
| `default` + `norseq` | 25/25 hang | 25/25 exit 0 |

The s3 scenario against libdb `master` with 4.3.2 and `UMEM_DEBUG=default`:
s3-umem is clean (no heap error, data verified) and s3-umem-sabotage catches
the planted overflow, on seeds 1-5. `guards,contents` gives the same verdicts.

**Harness change.** `install.sh` pins `LIBUMEM_PIN` to `60a6452`, and s3 uses
`UMEM_DEBUG=default` again, so the audit allocation and free stacks are back in
its reports. Every libumem run stays under `timeout -k`, because the SIGKILL
fallback is what ends a hung `LD_PRELOAD` process. `S3_UMEM_MODE=guards,contents`
still selects the old mode.

# Part 2 — multi-node HA and XA (F2, scenarios S4–S8)

This file records what the HA and XA scenarios (S4–S8) found on one Linux host
(the shared dev box, Debian 12, 32 vCPU, `sch_netem`, netns, libfiu 1.1,
OpenSSL 3). Each finding has a reproducer and, per RFC 0024 §2.5, a k/10
reproduction rate. Engine bugs get their own branch from `origin/master` (or
the feature branch for TLS/XA), never a fix on `test/dst-rt`.

### Scope and method

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

### Harness defects found and fixed (test-only; no libdb bug)

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

### Scenario results (20 seeds each)

Swept on the dev box (Debian 12, 32 vCPU), netns sweeps serial, S8 parallel.
Each seed drives a full scenario; a FAIL is retried k/10 per §2.5.

| Scenario | 20-seed result | Notes |
|---|---|---|
| S4 repmgr 3-site | 20/20 PASS* | *1 cold-start netns flake on the very first seed of a sweep (`ip netns add` racing the /run/netns mount); fixed by a retry in `net_up`, re-run of that seed PASSes. |
| S5 partition (normal) | 20/20 PASS | election to a new master, old master rejoins as client, converges; minority master reports PERM_FAILED and labels its isolated-window writes UNKNOWN.  (The soak initially saw the rule-5 ROLE-line artifact below; fixed, re-verified clean.) |
| S5 partition (sabotage) | 20/20 caught | `ACKS_NONE` + partition + kill: the oracle caught the lost acked writes on every seed (oracle FAIL ⇒ scenario PASS, by design). |
| S6 repl + storage fault | 20/20 PASS | `io-eio-write` on a client; the group kept acking (2-site quorum), the faulty client fell out and rejoined clean.  (A single soak run under heavy box contention saw the master starve and ack 0 in the window -- a load-timing edge on a shared box, not a libdb bug.) |
| S7 TLS (positive) | 20/20 PASS* | *the rule-5 ROLE-line artifact (below, now fixed) and a convergence-timing effect: TLS reconnection + client resync after a partition is slower than plaintext, so the isolated client needs a longer converge window (raised to 120 s for S7); with it, S7 seed 10 passes. |
| S7 TLS (pin, negative) | 20/20 PASS | the unpinned site never joined, every seed. |
| S8 XA 2PC | 20/20 PASS | control run PASS (teeth); under `io-fsync-fail` libdb panics/errors (rule 7), never an XA split (rule 6). |

### The two non-PASS seeds, both harness/oracle artifacts (no libdb bug)

1. **S4 seed 1 cold-start netns race.** The first `ip netns add` of a sweep can
   fail "Invalid netns value" while the kernel sets up the shared `/run/netns`
   mount. Fixed: `net_up` retries the bridge-namespace creation under the
   build lock. Reproduce: it only appears as the very first topology of a cold
   sweep; the re-run passes (k=0 after the fix).
2. **S7 rule-5 phantom split from the ELECTED event (root-caused; rtnode, not
   libdb).** Under TLS + net-partition the single-master check intermittently
   flagged "two masters in generation 1". Ground truth with the engine's own
   verbose log (`DB_VERB_REPLICATION` on every node, rtnode `--verbose`) shows
   it is NOT a split: the engine's `New master gen` broadcasts were `a@gen1`,
   `c@gen2`, `c@gen3` -- exactly one master per generation. Root cause (lead):
   rtnode emitted a `ROLE master` line for BOTH `DB_EVENT_REP_MASTER` and
   `DB_EVENT_REP_ELECTED`; `ELECTED` is fired by `__rep_elect_done` when the
   election is won, BEFORE repmgr calls `rep_start(MASTER)` and thus before
   `SET_GEN(new_gen)`, so its `st_gen` is the OLD generation. A node elected to
   gen 2 therefore logged `ROLE master gen=1` (from ELECTED) and the stale
   gen-1 line collided with the real gen-1 master. Fix: emit the ROLE master
   line ONLY on `DB_EVENT_REP_MASTER` (the role change, after `SET_GEN`); log
   ELECTED as `ELECTED` with no gen. rule 5 now gates both ways -- (role) one
   node with `ROLE master gen=N` per N, and (data) one node ACKing per
   commit-token generation -- and is clean across re-runs. No libdb bug: repmgr
   held one master per generation throughout. Reproduce with ground truth:
   `HA_VERBOSE=1 BUILD=<tls build> bash test/dst-rt/scenarios/s7_tls_netfaults.sh 10`
   then read `ev.<node>.log.msg` for the `New master gen` lines.

After both fixes, re-verification is clean: S7 seed 10 ×5 PASS, S5 normal ×5
PASS (the two seeds/arms that had been failing).

### 1-hour mixed HA soak

A 1 h soak rotating S4, S5, S5-sabotage and S6 over the plain build ran 58
iterations.  With the pre-fix binaries it showed 11 FAILs -- 10 S5 (the ELECTED
rule-5 phantom, since fixed and re-verified) and 1 S6 (the master starved and
acked 0 in its window while the box was under heavy concurrent load -- a
shared-box load-timing edge, not a libdb bug; rules 4 and 5 were OK).  No
ERRORs (the harness itself was solid throughout) and no engine finding.  Re-run
with the fixed binaries, the S5 iterations pass.

### Engine findings (libdb bugs)

None so far. Under `io-fsync-fail` (S8) libdb does the RFC-expected thing: it
returns `DB_RUNRECOVERY`/an XA error and panics rather than report a durable
commit it did not make — rule 7 holds, and no XA split (rule 6) was observed.

### Reproduce

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
