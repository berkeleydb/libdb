# RFC 0006: Chain-replicated WAL as a shared log — multi-master, scale-out libdb

- **Status:** Draft
- **Type:** Prospective
- **Author:** libdb maintainers
- **Date:** 2026-09-14
- **Tracking:** no implementation planned. This RFC exists to record a rigorous
  analysis of the direction — including the reasons it may be un-buildable
  inside libdb's north star — before anyone spends a quarter on it.

---

## Summary

Propose replacing libdb's **single-master log-shipping** replication with a
**shared, totally-ordered WAL replicated along a chain**, and using that total
order as the serialization order for **multi-master** transactions: any node
originates a transaction locally against an MVCC snapshot, commits by
**appending one self-contained intention record** (its redo payload plus its
read/write sets) into the chain, and every node then decides commit-or-abort
**deterministically by replaying the log in order** and applying a certification
rule. No lock coordination between nodes; the log *is* the concurrency-control
mechanism. The design is CORFU's shared log (NSDI 2012) plus Tango's
log-order optimistic commit (SOSP 2013) plus van Renesse & Schneider's chain
replication (OSDI 2004), landed onto an engine that already ships its WAL as
the replication stream and already owns an SSI certification mechanism
(RFC 0003) that is the natural log-order certification rule.

This RFC is **not advocacy**. Three findings below are, in the authors' view,
individually close to fatal: today's redo records carry a *page-LSN
precondition* that assumes a single writer's page-LSN chain (§Risks 1), the
head/sequencer merges the ordering bottleneck and the failure domain into one
node (§Risks 2), and a multi-master group cannot be operated without
membership/epoch infrastructure that libdb, as an embedded library with no
server process, has no business shipping (§North-star check). The value here is
the written analysis and the falsification plan, not the proposal.

## Motivation

### What libdb replication is today

libdb already has the substrate this RFC wants to generalize: **the WAL is the
replication stream.** A master's `__log_put` assigns the LSN under
`LOG_SYSTEM_LOCK`, drops the lock, and broadcasts the *same log record bytes* to
clients (`src/log/log_put.c:264`, `__rep_send_message(... REP_LOG ...)`).
Everything else follows from that:

- A client never generates log records — `__log_put` asserts it
  (`src/log/log_put.c:144`, `DB_ASSERT(env, !IS_REP_CLIENT(env))`), and the
  logging predicate `DBC_LOGGING` excludes clients (`src/common/db_err.c:998`,
  `src/dbinc/db_int.in:1137`). Writes on a client are refused outright
  (`src/common/db_err.c:1020`, "dbc_logging: Client update";
  `src/sequence/sequence.c:243` for sequences).
- A client's log is **byte- and offset-identical** to the master's. The client
  write path is `__log_rep_put`, which asserts the incoming LSN is exactly the
  client's own next log position (`src/log/log_put.c:1683`,
  `DB_ASSERT(env, LOG_COMPARE(lsnp, &lp->lsn) == 0)`). The log is additionally
  back-chained byte-wise (`hdr->prev`, `src/log/log_put.c:840`), so "the same
  bytes at the same offsets" is load-bearing, not incidental.
- Ordering/gap management and apply live in `__rep_apply`
  (`src/rep/rep_record.c:1081`): in-order records go to `__rep_process_rec`,
  out-of-order ones are parked in the `__db.rep.db` bookkeeping database until
  the gap fills. `__rep_process_rec` writes the record to the log *before*
  acting on it (`src/rep/rep_record.c:2088`, `__log_rep_put`) — WAL discipline
  is enforced on the client too.
- Transaction apply is `__rep_process_txn` (`src/rep/rep_record.c:1527`). On a
  `__txn_regop` commit it (a) reacquires the transaction's write locks from the
  **lock list logged inside the commit record** (`src/rep/rep_record.c:1617`,
  `__lock_get_list(env, locker, 0, DB_LOCK_WRITE, lock_dbt)`) at
  `DB_LOCK_MAXPRIORITY` (`:1614`), (b) walks the transaction's records backwards
  through the per-transaction `prev_lsn` chain (`__rep_collect_txn`, `:1697`,
  collecting into `lc->array` at `:1735`), (c) sorts them into forward LSN order
  (`:1623`), and (d) dispatches each through the **recovery** dispatch table
  with `DB_TXN_APPLY` (`:1644`, `env->recover_dtab`).
- Failover is elections plus optional master leases: a simple majority by
  default (`src/rep/rep_elect.c:148` → `ELECTION_MAJORITY`,
  `src/dbinc/rep.h:534`), and a master must hold `config_nsites / 2` valid
  leases to commit or it fails the transaction (`src/rep/rep_lease.c:322`,
  called from `__txn_commit` at `src/txn/txn.c:850` and again at `:1009`, where
  a late failure is a `__env_panic`).
- Membership, connections and threads are `repmgr`'s job, and repmgr is
  **optional**: the Base API (`rep_set_transport`, `src/rep/rep_method.c:2176`,
  `db_rep->send = f_send`) leaves the network to the application, while repmgr
  starts real threads (`src/repmgr/repmgr_method.c:353`) and keeps a group
  membership database (`read_gmdb` / `__repmgr_reload_gmdb`,
  `src/repmgr/repmgr_util.c:1398`, `:1611`). This split matters a great deal in
  §North-star check.

That apply path is **the same redo code recovery uses**: `DB_REDO(op)` is true
for both `DB_TXN_FORWARD_ROLL` and `DB_TXN_APPLY` (`src/dbinc/db.in:820`), and
`__rep_process_txn` dispatches into `env->recover_dtab`. Replication apply is
recovery with a different driver. That is the single most important existing
fact for this RFC — and, as §Risks 1 shows, it is also the trap.

### Why look past single-master at all

1. **One writer is the ceiling.** Every write in a replication group funnels
   through one `__log_put` on one node. Read scale-out exists (clients serve
   reads; `DB_REP_ANYWHERE` even lets clients serve each other's log requests,
   `src/rep/rep_log.c:875`), write scale-out does not. Adding nodes adds
   durability and read capacity, never write throughput.
2. **Failover is a control-plane event, not a data-plane one.** Elections plus
   lease expiry mean a window with no writer; worse, a client that accepted
   records the new master never had must **roll back committed transactions**
   (`__rep_dorecovery`, `src/rep/rep_verify.c:445`, `rollback = 1` at `:525`) or
   refuse to proceed (`DB_REP_WOULDROLLBACK`, `:553`). "Committed then
   un-committed" is a durability wart the shared-log model does not have,
   because in a shared log a record is either in the log at a position or it is
   not.
3. **The existing single-master assumptions are load-bearing in surprising
   places, and the least-tested code in the tree.** `test/coverage/REPLICATION-COVERAGE.md`
   measures `src/rep/` + `src/repmgr/` at **56.6% line coverage over 12,445
   instrumented lines** after deliberate effort, with **`rep_lease.c` at 0%**
   because the lease tests hang under the in-process harness. `test/repiso/` is
   the project's *first executable replication-isolation test* and it exists
   because a real client-apply anomaly (issue #140) was otherwise unobservable.
   Any multi-master work is being proposed on top of the tree's weakest test
   surface. That is an argument for caution, not for enthusiasm.

## North-star check

This is the hardest section in the RFC and it is placed before the design on
purpose.

### Verdict

**A multi-master libdb can preserve "embedded / no server process" *for the
library* only by declaring the shared log to be external infrastructure that
the application supplies.** It cannot preserve the *spirit* of the north star
for the resulting *deployment*: a user running multi-master libdb is running a
distributed system with a sequencer, a chain, and a reconfiguration authority,
whether or not libdb ships those. The honest framing is:

> libdb gains the ability to **participate in** a shared log. It does not gain,
> and must not gain, the ability to **be** one.

Precedent for exactly this split already exists in the tree and should be
followed literally:

| Layer | Today | Under this RFC |
|---|---|---|
| Library, no threads, app-provided transport | Base API: `rep_set_transport` (`src/rep/rep_method.c:2176`), `rep_process_message` | New log-participation callbacks: `reserve/append/read/tail/seal` |
| Optional bundled implementation that starts threads | `repmgr` (`src/repmgr/repmgr_method.c:353`) + group membership DB | out of scope for this RFC; would be a separate proposal |
| Optional helper process | `util/db_replicate.c` | a chain/sequencer daemon would live here or outside the project entirely |

So the additive story is defensible: `DB_INIT_REP`-style opt-in, nothing changes
for the embedded single-process user, and the library still starts no process it
was not asked to start.

### Where it still fails the gate, stated plainly

1. **On-disk/log format: a real break, needs versioning.** Commit records must
   carry read/write-set fingerprints (§Design 4). `__txn_regop` (44) today is
   `opcode, timestamp, envid, locks` (`src/txn/txn.src:38-43`); the `locks` DBT
   is deliberately **write-locks only** — the sizing predicate is
   `IS_WRITELOCK` (`src/lock/lock.c:452` sizing, `:532` populate), which is
   precisely why SSI's `DB_LOCK_SIREAD` markers (`src/dbinc/db.in:327`) are
   excluded. Adding read sets means a new record version, `DB_LOGVERSION` 19 →
   20 (`src/dbinc/db.in:480`), and a `DB_REPVERSION` bump
   (`src/dbinc/rep.h:139`). Doable by the established mechanism
   (`BEGIN_COMPAT`), but it is a format change and must be argued as one.
2. **Multi-process correctness: the append path inverts.** Today `__log_put`
   assigns the LSN locally under `LOG_SYSTEM_LOCK` and only *then* releases the
   lock to send (`src/log/log_put.c:205`). In a shared log the position must be
   obtained *before* the bytes are final, which puts a **network round trip
   inside the log-append critical section** — or requires a redesign so that
   token reservation happens outside any region mutex. A process that dies
   holding the log region latch across a network stall is a `failchk` problem
   for every other process in the environment. This is not a detail; it is a
   structural conflict with libdb's multi-process shared-region model, and it is
   unresolved in this RFC.
3. **ACID: durability improves, isolation gets a new failure mode.** Durability
   is arguably stronger (a committed record is at a chain position acked by the
   tail; no `DB_REP_WOULDROLLBACK` rollback of committed work). Isolation gets
   *aborts that no local conflict explains* — a transaction can be certified
   away by a remote originator it never saw. That is the accepted cost of OCC,
   but it is a behavior change users must opt into.
4. **Access methods: Queue is a special case.** Queue data-page LSNs are
   explicitly **advisory** and deliberately *not* rolled forward
   (`src/qam/qam_rec.c:21-27`: "LSNs in queue data pages are advisory ... They
   should not be rolled forward during recovery"). Whatever page-LSN discipline
   §Risks 1 lands on, Queue is outside it and needs its own answer.
5. **Crash recovery: unchanged in kind, larger in scope.** A node recovers by
   replaying its local log prefix, exactly as today. But "the log" is now
   partly remote: a node that crashed mid-chain must re-derive which positions
   it holds and re-fetch holes — CORFU's hole-filling problem, which today's
   `__rep_apply` gap machinery (`src/rep/rep_record.c:1081`, `waiting_lsn` /
   `ready_lsn` / `__db.rep.db`) is a recognizable ancestor of but not a
   substitute for.

## Design

### 1. The shared log, and what CORFU/Tango actually do

Stated precisely, because the value of the analogy depends on not overstating
it. Both descriptions below are of the **published systems**, cited as
literature; every "libdb would…" sentence is this RFC extrapolating.

**CORFU** (Balakrishnan, Malkhi, Prabhakaran, Wobber, Wei, Davis, NSDI 2012;
extended in ACM TOCS 2013) turns a cluster of flash units into a single
totally-ordered shared log:

- A **sequencer** is a network-attached counter that hands out monotonically
  increasing log positions (tokens). It is an *optimization*, not the source of
  truth: it can be rebuilt by querying the storage units, and a client that
  loses a token leaves a hole that the hole-filling protocol resolves with a
  junk fill.
- A deterministic **projection** maps a log position to the storage units that
  own it. Clients write **directly** to those units — there is no write-through
  server. Within a position, the replica set is written in a fixed order
  (client-driven chaining) so that a position is durable when the last replica
  has it.
- Flash pages are **write-once**; reads go directly to a replica.
- Reconfiguration is by **epoch**: a new projection is installed after *sealing*
  the old one at the storage units, so writes stamped with a stale epoch are
  rejected. The projection sequence lives in an auxiliary service.
- The paper reports a single sequencer serving on the order of a few hundred
  thousand tokens per second. Treat that number as reported, on 2012 hardware,
  not as a prediction for anything here.

**Tango** (Balakrishnan, Malkhi, Wobber, Wu, Prabhakaran, Wei, Davis, Rao, Zou,
Zuck, SOSP 2013) builds replicated transactional **data structures** over such a
log:

- A Tango object is an in-memory **view** plus a **history in the log**. A
  client materializes the view by playing the object's log entries through an
  `apply` upcall. The log is the source of truth; the view is a cache.
- Playback is per-object via **streams** (a `readNext`-style API added over
  CORFU) so a client replays only entries relevant to its objects rather than
  the whole log.
- A transaction is committed by **appending a speculative commit record**
  containing its read set (with the versions read) and its write set. Every
  client that reaches that record in the log evaluates the *same* decision
  function — did anything in the read set change between the read version and
  this commit position? — and therefore reaches the *same* verdict with **no
  further communication and no lock coordination**. The total order is the
  serialization order.
- Linearizable reads require syncing to the current tail (learn the tail, play
  forward) before answering.

**Where the assumptions clash with an embedded library.** (a) Tango clients are
*application processes with in-memory views*; libdb's "view" is a durable page
store under `mpool`, so materialization is an on-disk mutation, not a rebuildable
cache. (b) CORFU clients talk *directly to storage*; libdb's storage is a local
file it owns exclusively, and its "replicas" are peers with their own page
stores. (c) Tango's whole-log-or-stream replay assumes replay is cheap and
side-effect-free; libdb replay is `DB_TXN_APPLY` into a durable B-tree with page
LSN preconditions (§Risks 1). (d) Neither system has anything to say about a
*multi-process* embedded environment sharing regions on one node.

### 2. Why a chain, not a quorum

Chain replication (van Renesse & Schneider, OSDI 2004): writes enter at the
**head**, propagate node-by-node to the **tail**, the tail acknowledges; reads
served by the tail are linearizable. Failure handling is structurally simple —
drop the failed node from the chain and splice — and is coordinated by an
external configuration authority (the paper's "master"). CRAQ (Terrace &
Freedman, USENIX ATC 2009) extends reads to any node by keeping dirty/clean
versions and asking the tail for the committed version when dirty.

For a **WAL** specifically, the fit is good:

- **The workload is append-only and sequential.** Chain replication's weakness
  is write latency proportional to chain length; its strength is that the write
  is a *pipeline*, and a log append pipeline is exactly what a WAL wants. Each
  node performs one sequential write and forwards.
- **Strong consistency with a trivially simple invariant.** Every node's log is
  a prefix of its predecessor's. That is *already* libdb's invariant between
  master and client (`src/log/log_put.c:1683`); chain replication just makes it
  transitive and makes the tail authoritative.
- **Recovery/reconfiguration is prefix arithmetic.** A rejoining node asks its
  new predecessor for everything past its last position. libdb has the machinery
  shape for this already (`__rep_logreq` / `REP_LOG_REQ`,
  `src/rep/rep_log.c:535`).
- **Versus Raft/quorum:** a quorum-replicated log needs a leader that both
  orders *and* replicates, log-matching/truncation logic on divergence, and
  either leader reads or read-index/lease tricks for linearizable reads. libdb's
  present design is closer to the quorum family (majority elections,
  `src/rep/rep_elect.c:148`; majority leases, `src/rep/rep_lease.c:322`) and
  pays for it with the committed-transaction rollback path
  (`__rep_dorecovery`, `src/rep/rep_verify.c:445`). Chain replication has no
  divergence to reconcile: a record is at a position or it is not.

The honest counterweight, spelled out in §Risks 2: chain replication's simple
failure handling is *purchased from an external configuration service*, and its
head is a single ordering point. Choosing a chain does not remove consensus from
the system; it **relocates** it into a small, infrequently-exercised control
plane — which is the standard argument for this shape (and the argument Delos,
OSDI 2020, makes for virtualizing that control plane), and also the standard
place where such systems fail.

### 3. Multi-master: what actually changes

Every node is a full peer: local mpool, local page store, local lock region,
local recovery. What changes is the log.

```
   node A            node B            node C
  (originate)       (originate)       (originate)
      \                 |                 /
       \----> reserve position (head) <--/        [ordering]
                     |
              head -> mid -> tail                 [durability, chain]
                     |
      /--------------+--------------\
     v               v               v
   apply           apply           apply          [materialization]
  (A,B,C each replay the same total order)
```

1. **Originate locally.** A transaction runs on its node against an MVCC
   snapshot (`DB_TXN_SNAPSHOT` / `DB_MULTIVERSION`), taking *local* page locks
   only, as it does today. No cross-node lock traffic — the whole point.
2. **Commit = one append.** Instead of streaming per-operation records as they
   are generated, the transaction's entire redo payload plus its read and write
   sets are appended as **one self-contained intention record** at one log
   position. The reasons this must be one record rather than N are structural,
   not cosmetic (§Risks 1, and it eliminates the per-transaction `prev_lsn`
   back-chain that `__rep_collect_txn` walks today,
   `src/rep/rep_record.c:1697`).
3. **The chain's position order is the serialization order.** No timestamps, no
   vector clocks, no cross-node agreement beyond "what position did the head
   give me".
4. **Every node certifies deterministically.** On reaching an intention record
   at position *p*, a node checks the record's read set against everything
   committed in `(snapshot_position, p)`. Same log, same rule, same verdict,
   everywhere — Tango's move exactly. A committed record's redo is applied; an
   aborted one is skipped. Nothing is un-done, so there is no distributed abort
   protocol.
5. **Materialize.** Each node applies committed redo to its own page store
   through the existing `DB_TXN_APPLY` path. **A node is a deterministic
   function of the log prefix it has applied** — the same property that makes
   recovery work, extended across originators.

### 4. Certification, and the SSI connection (the strongest argument here)

This is the part of the proposal that is *not* speculative plumbing, because
libdb already built the hard half.

RFC 0003 shipped Cahill's Serializable Snapshot Isolation. In doing so it built,
and is now maintaining under test, exactly the three things a log-order
certifier needs:

1. **A materialized read set at page granularity.** An SSI read takes a
   `DB_LOCK_SIREAD` marker instead of no lock
   (`src/db/db_meta.c:1184-1191`: under `MULTIVERSION` with `TXN_SNAPSHOT_SAFE`,
   `mode = DB_LOCK_SIREAD`). The set of pages a transaction read is *already a
   concrete data structure in the lock region*. Tango's commit record needs a
   read set; libdb computes one today for a different purpose.
2. **A materialized write set that is already in the log.** `__txn_commit` asks
   `__lock_vec` for the retained write locks and logs them in `__txn_regop`
   (`src/txn/txn.c:928-946`; the `IS_WRITELOCK` predicate at
   `src/lock/lock.c:452`/`:532`). The commit record already carries the write
   set. It carries it for the *client apply* path
   (`__lock_get_list`, `src/rep/rep_record.c:1617`), but it is the same set a
   certifier wants.
3. **A conflict-abort path with error codes, lifetimes and a test suite.**
   `DB_SNAPSHOT_CONFLICT` / `DB_SNAPSHOT_UNSAFE` (`src/dbinc/db.in:1410-1411`),
   the commit-time pivot check under `TXN_SYSTEM_LOCK` (`src/txn/txn.c:817-830`),
   marker/locker reclamation, `ssi001`–`ssi011`, `test/isolation`, `test/soak`.

So the proposal is: **make the log-order certification rule libdb's existing
certification machinery, moved from "in-memory flags at runtime" to "predicates
evaluated over the log's total order."** Concretely, the commit record gains

```
   BEGIN regop  <new-version>
   ARG   opcode      u_int32_t
   TIME  timestamp   int32_t
   ARG   envid       u_int32_t      /* already present: originator identity */
   ARG   snap_pos    <log position> /* the snapshot this txn read */
   LOCKS locks       DBT            /* write set (as today) */
   LOCKS reads       DBT            /* NEW: read set, i.e. the SIREAD markers */
   END
```

and the certifier is a function of `(snap_pos, reads, writes)` against the
committed writes in the interval — which is a **backward-validation (BOCC)**
rule, i.e. Tango's rule.

**Be precise about the limit of the analogy.** Tango's rule and Cahill's rule
are *not* the same predicate. Backward validation aborts whenever a read-set
item was overwritten in the interval. Cahill's SSI aborts only a transaction
that is the **pivot** of a dangerous structure — two rw-antidependency edges,
one in and one out — which is strictly less conservative and is why SSI beats
BOCC on abort rate. Reconstructing SSI's *edges* (rather than BOCC's
overlaps) from a log requires knowing, for each pair, which transactions were
*concurrent* and in which direction the antidependency ran. With `snap_pos` and
the commit position, concurrency is derivable: transactions *T*₁ and *T*₂ are
concurrent iff each one's commit position exceeds the other's `snap_pos`. So the
edges are, in principle, computable from the log. **Whether Cahill's
dangerous-structure rule remains sound when evaluated over log order by
independent replayers, instead of over wall-clock concurrency by the runtime, is
an open theoretical question this RFC does not resolve** (§Risks 6). The
defensible staging is: ship BOCC first because it is provably right and
mechanically simple, and treat "SSI's pivot rule over log order" as a
lower-abort-rate refinement to be proved separately.

What this connection buys, even at BOCC: the read-set tracking, the abort
plumbing, the page-granularity conflict discipline, the marker lifetime
hardening, and the regression suite are **already paid for**. And what it
inherits is RFC 0003's known weakness, unchanged and amplified: **conflict
tracking is per-page, not per-row.** Two originators writing unrelated keys on
one leaf page conflict. RFC 0003's own follow-up item is "reduce abort rate
under contention (finer-grained conflict tracking)", and a companion RFC on
row-level conflict tracking is where that has to be solved. Page-granularity
certification across nodes is, in the authors' estimate, the difference between
this design being interesting and being useless on real workloads.

### 5. Reads and materialization

- **Read-your-writes on the originating node.** A node that appended at
  position *p* must apply through *p* (and see its own verdict) before answering
  a read that must include it. That is a local wait for the tail ack plus local
  apply, comparable in shape to today's `DB_TXN->set_commit_token` /
  `DB_ENV->txn_applied` handshake (`__txn_build_token`, `src/txn/txn.c:1136-1158`,
  marshals `(version, gen, envid, lsn)`; `__txn_applied` compares them at
  `:2389`).
  That existing token API is very nearly the right primitive already.
- **Stale reads at non-tail nodes.** A node that has applied a shorter prefix
  serves a consistent-but-older snapshot. This is *exactly* today's client-read
  semantics, so it is not a new class of behavior.
- **Linearizable reads** need one of: (i) read at the tail; (ii) CRAQ-style —
  read locally, and if the page is dirty with respect to the applied prefix ask
  the tail for the committed version; (iii) **leases** — a node that holds a
  lease asserting "no committed record exists past position *p* that I have not
  seen" may answer locally. libdb has a lease framework to generalize
  (`src/rep/rep_lease.c`), and it is at **0% test coverage**
  (`test/coverage/REPLICATION-COVERAGE.md`), which is where the real cost of
  option (iii) sits.

### 6. What has to be unwound

| Today | Under this design |
|---|---|
| `REP_F_MASTER` / `REP_F_CLIENT` roles set in `rep_start` (`src/rep/rep_method.c:716`) | no roles; every node originates. `IS_REP_CLIENT` gates (25 call sites across 15 files) must be re-audited one by one |
| Client writes refused (`src/common/db_err.c:1020`; `src/log/log_put.c:144`) | must be *permitted*, which removes the assertion that today protects clients from divergence |
| `DB_TXN_SNAPSHOT` rejected on a client (`src/txn/txn.c:270-272`) | **must be lifted** — and this is the whole isolation story, since certification *requires* every originator to run under snapshot/SSI |
| Write set logged, read set deliberately not (`src/lock/lock.c:452`) | read set must be logged: log-format change |
| Majority elections (`src/rep/rep_elect.c:148`), majority leases (`src/rep/rep_lease.c:322`) | replaced by chain reconfiguration; leases repurposed for linearizable local reads |
| `__rep_process_txn` reacquires write locks from the log (`src/rep/rep_record.c:1617`) and back-walks `prev_lsn` (`:1697`) | one intention record per transaction: no back-walk, and local apply locks are needed only against *local* readers |
| Per-transaction `prev_lsn` chain; per-env `txnid` (`src/txn/txn.c:488`) and `fileid` (`src/dbreg/dbreg.c:331`) counters | `txnid`/`fileid` are **per-environment** and would collide in one shared log; keys become `(envid, id)` everywhere the dispatcher reads a bare txnid (`src/db/db_dispatch.c:88`) |
| Committed-txn rollback on divergence (`__rep_dorecovery`, `src/rep/rep_verify.c:445`; `DB_REP_WOULDROLLBACK`) | gone — nothing to roll back |

The `envid` column is a genuine piece of pre-existing multi-originator
infrastructure worth noting: `__txn_regop` version 44 already logs the
originating environment's id (`src/txn/txn.src:38-43`, `renv->envid` at
`src/txn/txn.c:863`), and commit tokens are already
`(version, gen, envid, lsn)`. Someone already anticipated that "which
environment produced this record" is a question worth being able to answer.

## Alternatives considered

- **Raft/quorum-replicated WAL (single leader).** The mainstream choice, and the
  one libdb is already closest to. Gains: an off-the-shelf, well-understood
  membership/consensus story; no format change; failover measured in
  milliseconds. Loses: still **single-writer** — it fixes failover, not write
  scale-out, so it does not answer this RFC's motivation at all. It also keeps
  divergence reconciliation (log truncation / committed-rollback), which the
  chain does not have. *If the only goal is better HA, this is the correct
  proposal and this RFC should be rejected in its favor.*
- **Keep single-master, make failover fast.** Cheapest by a wide margin: shrink
  election and lease timeouts, pre-warm clients, cover `rep_lease.c`'s 0%. Buys
  most of the *availability* benefit for a fraction of the risk. Buys **zero**
  write scale-out. This is the honest baseline any multi-master work must beat,
  and it should be done regardless.
- **Calvin-style deterministic execution** (Thomson et al., SIGMOD 2012):
  replicate the *input* (transaction requests) in a total order and have every
  node execute deterministically. Attractive because the log gets much smaller.
  Fatal for libdb: it requires transactions to be **declared in advance as
  deterministic units** with known read/write sets, whereas libdb's API is an
  imperative cursor library where the application interleaves arbitrary
  non-deterministic host-language code between `get` and `put`. libdb cannot
  execute the user's program on every node. Deterministic *redo* is the only
  determinism available to us — which is precisely what the WAL already is.
- **Logical / statement replication.** Ships operations, not pages; sidesteps
  the page-LSN precondition problem (§Risks 1) entirely, and allows row-level
  conflict detection naturally. Rejected as a *replacement* because it discards
  libdb's actual asset — a WAL that is already a correct, tested, deterministic
  redo stream with recovery-grade idempotency — and replaces it with a second
  apply path to keep correct for every access method. Worth noting as the
  fallback if §Risks 1's experiment shows physical redo cannot be made
  order-independent.
- **Leader-per-partition sharding.** Split the keyspace into N ranges, each with
  its own single-master group. This gets write scale-out with **no new
  concurrency control at all** and no format change — the cheapest real answer
  to the motivation. Loses: cross-partition transactions require 2PC — and
  libdb's `prepare` **refuses an SSI transaction outright**
  (`src/txn/txn.c:1449-1455`, because the pivot check runs at commit while a
  prepared transaction must be guaranteed committable), so the 2PC path is
  incompatible with the isolation level this design needs — and partitioning is
  exposed to the application. *This is the alternative most
  likely to be the right engineering answer*, and this RFC's advantage over it
  is narrow: a single total order gives cross-shard serializability for free,
  which 2PC-over-shards does not.
- **Hyder (Bernstein, Reid, Das, CIDR 2011) — the closest prior art, and it
  must be named.** Hyder is a data-sharing database in which every server
  executes optimistically against a snapshot, appends an **intention record** to
  a shared log, and every server rolls the log forward applying a **meld**
  function that deterministically detects conflicts and decides each intention's
  fate. That is, in substance, §Design 3-4 of this RFC, published fifteen years
  earlier and with the conflict-detection function worked out over a
  copy-on-write binary-tree index specifically chosen to make meld tractable.
  Two consequences for honesty: (a) this RFC is not novel, and Hyder's
  literature (including the difficulty of meld on a general index) is the best
  available evidence about the design's real cost; (b) Hyder's choice of a
  purpose-built index structure is a warning — it suggests the deterministic
  conflict-decision function is much easier on a structure designed for it than
  on B-tree page redo with page-LSN preconditions.
- **Related shared-log work worth reading before any implementation.** vCorfu
  (NSDI 2017, materialized streams); FuzzyLog (OSDI 2018, deliberately partial
  order to escape the single-sequencer bottleneck — relevant to §Risks 2);
  Scalog (NSDI 2020, high-throughput ordering without one sequencer); Delos
  (OSDI 2020, virtualized/reconfigurable consensus under a shared log API);
  Boki (SOSP 2021). Also FoundationDB, whose sequencer + resolver + transaction
  log split is a production instance of "one order handed out centrally,
  certification done by rule" — and whose deterministic-simulation methodology
  `test/sim/` already imitates.

## Risks & open questions

Numbered so they can be cited. Items 1, 2 and 6 are the blocking ones; the
authors' judgment is that **item 1 alone stops the naive design.**

1. **BLOCKING — today's redo records are not order-independent: they carry a
   page-LSN precondition.** Every page-modifying redo record stores the page's
   *prior* LSN and refuses to apply if the page is not in exactly that state.
   Concretely, `__db_addrem_recover` computes
   `cmp_p = LOG_COMPARE(&LSN(pagep), &argp->pagelsn)` and passes it to
   `CHECK_LSN` (`src/db/db_rec.c:64-65`), which on a replication client is
   *stricter* than in local recovery — the `|| IS_REP_CLIENT(e)` clause in
   `src/dbinc/log.h:387-393` makes it error where local recovery would tolerate
   an unlogged or zero LSN. There are **73 `CHECK_LSN` sites** across
   `bt_rec.c` (25), `db_rec.c` (28), `hash_rec.c` (18), `heap_rec.c` and
   `crdel_rec.c`.

   Today this is safe because there is exactly one writer: a record's
   `pagelsn` was observed by the same node that will be the source of every
   subsequent record for that page, so the client's page LSN chain is a replay
   of the master's. **Under multi-master it is not safe.** If node B generates a
   record whose `pagelsn` reflects B's applied prefix, and the chain orders a
   record from node A for the same page *before* B's, then at apply time
   `LSN(page)` is A's stamp, `cmp_p != 0`, and `CHECK_LSN` returns `EINVAL` on
   *every* node — a hard failure, not a silent divergence, which is at least a
   loud one.

   Certification is a *partial* answer: if the rule aborts any transaction whose
   write-set pages were modified in `(snap_pos, p)`, then a committed record's
   pages were untouched in the interval and `pagelsn` still matches. But that
   only holds if the record's `pagelsn` was captured **as of `snap_pos`**, which
   in turn requires the redo payload to be generated against the snapshot and
   appended as one unit at commit — Hyder's intention record, not today's
   generate-as-you-go stream. And it upgrades certification from "read/write
   conflict" to "**any** write-write page overlap aborts", which at page
   granularity is a severe restriction (see RFC 0003's open follow-up on
   finer-grained tracking). It also does not cover records whose payload depends
   on more than the target page's content — page splits touch three pages plus
   the parent (`src/btree/bt_rec.c:104-118`, `:223`), and page allocation reads
   and rewrites the meta page (`__db_pg_alloc` logs `meta->last_pgno`,
   `src/db/db_meta.c:195-199`), making the **meta page a global serialization
   point for every allocating transaction on every node**.
   *Open question, and the first thing any prototype must answer.*

2. **BLOCKING — the head is both the throughput bottleneck and a failure
   domain.** Chain replication puts ordering at the head; CORFU splits ordering
   (sequencer) from storage precisely so the sequencer can be small, rebuildable
   and bypassed on failure. A design that merges them gets simplicity and a
   single point of both saturation and failure. Sub-questions: does the head
   need to see record *bytes* to assign a position, or only hand out tokens (if
   tokens only, it can be tiny and stateless-ish, at the cost of holes needing
   fills)? Can appends be batched per node to amortize? Is a partial order
   (FuzzyLog) or a sharded ordering layer (Scalog) required to get past the
   single-sequencer ceiling — and if so, does certification still work without a
   total order? *Unresolved.*

3. **Reconfiguration and split-brain.** Chain membership changes need an
   authority, and correctness needs the old configuration **sealed** before the
   new one accepts writes (CORFU's epoch/seal). Get this wrong and two heads
   assign the same position to different records: silent, permanent divergence
   across every node — categorically worse than today's worst case (a
   `DB_REP_WOULDROLLBACK` refusal). libdb has no epoch-fencing primitive; the
   nearest thing is the replication `gen` (carried in commit tokens at
   `src/txn/txn.c:1152-1155`), which is not a seal. *This is
   the item most likely to produce an unrecoverable bug in a first
   implementation.*

4. **Log format and versioning.** `DB_LOGVERSION` 19 → 20
   (`src/dbinc/db.in:480`), `DB_REPVERSION` bump (`src/dbinc/rep.h:139`;
   `DB_REPVERSION_MIN` is 44, `:140`), a new `regop` version keeping the old one
   via `BEGIN_COMPAT` (as `regop 42` is kept today, `src/txn/txn.src:32-36`),
   `db_printlog` support, and a `db_log_verify` update. Mechanical, but it makes
   the whole proposal a format-gated change under `rfc/README.md`'s review rule.

5. **Namespace collisions in one shared log.** `txnid` (`src/txn/txn.c:488`,
   `++region->last_txnid`) and `fileid` (`src/dbreg/dbreg.c:331`,
   `lp->fid_max++`) are **per-environment** counters. In one shared log, two
   originators mint the same ids. Every consumer that keys on a bare txnid —
   starting with the dispatcher, which reads it as the second word of every
   record (`src/db/db_dispatch.c:88`) — needs `(envid, txnid)`. Also: `DB_LSN`
   is `(file, offset)` (`src/dbinc/db.in:495`), a *physical* position. That can
   remain the global log position **only** if every node's log is byte-identical
   — which is today's invariant (`src/log/log_put.c:1683`) and which
   node-local records would break. Node-local, non-replicated content exists
   today (`DB_AM_NOT_DURABLE`, in-memory databases). *Open: does each node keep
   a second, local log, and if so what does `DB_LSN` mean?*

6. **BLOCKING (theory) — is log-order certification with libdb's rule
   provably correct?** BOCC over a total order is textbook. Cahill's
   dangerous-structure rule evaluated by independent replayers over log order
   is **not** something this RFC has proved sound, and RFC 0003's history is a
   direct warning: the commit-window race (issue #136) was a subtle failure of
   *exactly* this kind of reasoning — a check that looked atomic and was not.
   That bug needed `TXN_DTL_SICHECKED` published under `TXN_SYSTEM_LOCK`
   (`src/txn/txn.c:817-830`) and a dedicated test tier to find. A distributed
   restatement of the same rule deserves at least a written proof and a model
   check (`test/cbmc/` exists) before code.

7. **Abort rate at page granularity.** See §Design 4. If two nodes writing
   different keys on one leaf page abort each other, multi-master gives negative
   write scale-out on any workload with locality — which is most of them. This
   is a *quantitative* question with a cheap answer (§Prototype phase 2) and it
   should be answered before anything else is built.

8. **The append critical section.** §North-star check item 2: a network round
   trip inside the log-append path, in a library whose environment is shared by
   multiple processes and policed by `failchk`. Unresolved and structural.

9. **Commit latency.** Today a commit is one local fsync plus an async
   broadcast (with lease checks when configured). Under a chain it is a token
   round trip plus a head→tail traversal with a durable write per hop.
   Single-transaction latency will be *worse*; the case for the design rests
   entirely on aggregate multi-originator throughput. If it does not win there,
   there is nothing to win.

10. **Test surface.** The tier that would have to prove this is
    `test/repiso/` — two real processes, one real socket, currently **two
    sites with fixed roles and no elections** by explicit design. Multi-master
    needs N originators, injected reorderings, and a deterministic scheduler;
    `test/sim/DST-V2-DESIGN.md` scopes the multi-process deterministic
    scheduler as v2, i.e. **not yet built**. Multi-master correctness is not
    testable in this tree today.

## Prototype / evidence

None. No line of this has been measured or built. What follows is the staging
plan and, more importantly, the falsification criteria — each phase is designed
so that a *negative* result kills the direction cheaply.

**Phase 0 — the order-independence experiment (no engine changes, days).**
Take one database image; run two independent single-node workloads from it,
producing logs *L*₍A₎ and *L*₍B₎. Interleave their records into a synthetic
total order and replay against the shared starting image with a
recovery-style driver. Instrument `CHECK_LSN`.
*Prediction:* it fails, with `__db_check_lsn` errors on the first page both
workloads touched (`src/common/db_err.c:1060`).
**Falsifies the naive "just interleave the WALs" story — which is the point.**
If it *unexpectedly* passes for disjoint page sets, that quantifies exactly how
much certification has to guarantee, and is the cheapest possible measurement
of Risk 1.

**Phase 1 — determinism under a single order (weeks).** Two nodes, a trivial
in-process "shared log" shim (no chain, no network), whole-transaction intention
records, write-write page certification only. Success criterion: after applying
identical log prefixes, both nodes produce **identical state hashes** — the same
invariant `test/sim/test_sim_recover_idempotent.c` already asserts across two
recoveries, and which the DST planted bug `REDONOSTAMP` exists precisely to
break (`src/db/db_rec.c:88-99`) — and `db_verify` is clean on both.
*Falsified by* any
divergence that cannot be fixed without regenerating record payloads at apply
time; that outcome means the design is really logical replication and should be
re-framed or dropped (see §Alternatives).

**Phase 2 — certification correctness, and the abort-rate answer (weeks).**
Two originators, concurrent commits. Correctness: extend `test/isolation`'s
write-skew checker so the two transactions originate on *different* nodes; the
anomaly must be prevented, and both nodes must reach the *same* verdict for
every intention record (a divergent verdict is the catastrophic failure mode and
deserves its own assertion). Rate: reuse `ssi_abort_bench`'s write-skew ring —
whose design already encodes the page-granularity lesson that write keys must be
spread far apart in *key order* to land on different leaf pages — to measure
cross-node abort rate as a function of key-spread.
**Falsifier, stated in advance:** if abort rate at realistic locality
(neighbouring keys, i.e. shared leaf pages) makes two-node aggregate write
throughput *lower* than the single-master baseline, the design is dead for
general workloads and should be rejected pending row-level conflict tracking.

**Phase 3 — throughput versus the honest baseline (months).** Only if phases
0-2 pass. Real chain, real network, N ∈ {2,3,5}. The baseline is **today's
single-master group**, measured, not asserted (`test/bench` conventions;
`test/bench/NOISE.md` for the noise band). Report aggregate write throughput,
p99 commit latency, and abort rate together — a throughput win bought with a
20% abort rate is not a win.
**Falsifier:** if aggregate multi-originator write throughput does not exceed
the single-master baseline by a margin outside the noise band at N=3, stop. The
entire justification for the format change, the certification machinery and the
distributed control plane is write scale-out.

**Explicitly out of scope for any prototype:** shipping a sequencer, a chain
daemon, or membership logic in libdb. See §North-star check.

---

## Decision

*(Filled by the reviewer when the RFC is decided.)*

- **Decision:** Pending — Draft. The authors' own recommendation is **do not
  accept as an implementation plan.** Accept it, if at all, as a *research
  direction* whose next step is Phase 0 — a days-long experiment that costs
  nothing and settles Risk 1.
- **Rationale:** —
- **Conditions / follow-ups:** (a) Risk 1 (page-LSN precondition under
  interleaved originators) must be resolved by the Phase 0 experiment before
  any further work; (b) Risk 6 (soundness of log-order certification with
  libdb's rule) needs a written proof or a `test/cbmc/` model; (c) Risk 7
  (page-granularity abort rate) is gated on row-level conflict tracking landing
  first; (d) the north-star framing — libdb *participates in* a shared log,
  never *is* one — must be preserved in any successor RFC, or the successor
  should be rejected on the gate.
