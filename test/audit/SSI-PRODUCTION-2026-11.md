# SSI production-readiness qualification (2026-11)

**Question (maintainer decision Q4):** *"if it's well tested and functional then
SSI is production ready."* This report makes "well tested and functional" a
**checkable claim** for `DB_TXN_SERIALIZABLE`, then checks it.

The scope is the SSI mode only (`DB_TXN_SERIALIZABLE` = the MVCC snapshot
substrate plus Cahill's rw-antidependency detection, RFC 0003). Plain
`DB_TXN_SNAPSHOT` keeps its legacy non-serializable meaning and is out of scope
except as a control.

## Verdict

<!-- filled in at the end, see §Verdict -->

## The checklist

Each item is a claim with a test or a measurement behind it. "Evidence" names
the artifact; "Status" is PASS / FAIL / PARTIAL / documented-limitation.

| # | Claim | Evidence | Status |
|---|---|---|---|
| a | No anomaly ever commits, at scale, over random workloads | `test/isolation/test_ssi_cycle` (Adya cycle checker), `test_iso_anomaly`, `ssi001`–`ssi011` | see §a |
| b | SSI txns survive kill -9 + recovery | `test/isolation/test_ssi_crash_pivot`, `test/sim` swarm | see §b |
| c | SIREAD marker / locker counts stay bounded over a long run | `test/soak/test_soak_resources` | see §c |
| d | The false-abort rate is quantified, with mitigation guidance | `test/bench/SSI-FALSE-ABORT-2026-09.md` + this report | see §d |
| e | SSI reachable from C, C++, Java, Tcl, Rust, Zig | binding survey + new tests | see §e |
| f | DB_TXN_SERIALIZABLE, DB_SNAPSHOT_CONFLICT, retry, MVCC×optread documented | `docs_src/` pages | see §f |

---

## (a) Correctness — no anomaly ever commits

### What already existed

- `test/isolation/test_iso_anomaly` — 10 **fixed** scenarios (write skew in
  three timings, same-btree variants, G2-item, phantom-on-different-pages,
  Fekete read-only anomaly, lost update, read-your-writes). Verdict is
  **computed**: it enumerates every permutation of the committed transactions
  and requires one to reproduce the durable state (and each committed read-only
  transaction's observation). Run under both `DB_TXN_SNAPSHOT` (anomaly
  visible) and `DB_TXN_SERIALIZABLE` (anomaly prevented), and under 1 and
  default lock partitions.
- `ssi001`–`ssi011` (Tcl) — mechanism coverage: write skew, no-false-positive,
  prepare rejection, partitioned lock manager (#136 race gate), incremental
  SIREAD GC, MVCC-version-chain path, region-pressure, recovery, multi-process
  concurrent writers, SI-vs-SSI contrast, prepare-allowed-for-plain-SI.

**The gap this report closes.** The permutation oracle is exact but only
tractable for ≤ 4 transactions and a handful of keys, and the scenarios are
*fixed*. It cannot answer "does an anomaly ever commit under a **random**
workload at **high thread counts** over a **long** run?" — which is the
question Q4's "well tested" demands. An anomaly that only appears under a
particular interleaving of 32 threads over millions of operations is invisible
to a 2–4 transaction deterministic scenario.

### What this report adds: an Adya cycle checker

`test/isolation/test_ssi_cycle.c` — a general serializability oracle that does
**not** enumerate permutations and so scales to arbitrary workloads:

1. Many worker threads run random transactions (read set + write set drawn at
   random) under `DB_TXN_SERIALIZABLE`, over a btree **or** a hash database.
2. Each **committed** transaction records its read set, its write set, its
   begin and commit order, and the version it read for every key (the writer
   that produced it).
3. From those records the checker builds the dependency graph of Adya's
   formalism — ww (write-write order), wr (read-from), and rw-antidependency
   edges — and requires it **acyclic**. A cycle is a non-serializable history:
   a committed anomaly. (This is the same soundness argument SSI itself rests
   on: SSI aborts to keep this graph acyclic; the checker verifies the result
   independently of the mechanism.)
4. Anti-vacuity: the identical harness run under `DB_TXN_SNAPSHOT` **must**
   find a cycle (plain SI admits them), which proves the checker has teeth and
   is actually driving conflicting transactions.

Scale knobs: seeds, threads, duration, key count, access method, and a
multi-process mode (N processes on one shared environment).

Evidence (dev box, `c6i.8xlarge`, 32 vCPU, Debian 12):

<!-- RESULTS a: filled from the run -->

---

## (b) Crash safety

`test/isolation/test_ssi_crash_pivot` drives a deterministic write skew in
which T2 reaches its commit-time pivot check holding **both** conflict ends
while T1's write is already durable, then `SIGKILL`s the process at one of four
points inside `DB_TXN->commit`. The parent reopens with `DB_RECOVER` and
asserts the recovered state is never `(0,0)` — the one state that only a
*committed pivot* could produce. The in-commit kill points are reached by a
`DIAGNOSTIC`-only hook (`SSI_CRASH_AT`), inert in a production build.

The stronger observed result: a pivot never reaches kill point 3 or 4 because
`DB_SNAPSHOT_CONFLICT` returns upstream of the log write — no commit record for
a pivot is ever produced. Teeth: neutering the commit-time pivot check makes
point 4 recover to `(0,0)`.

Evidence (many seeds, DIAGNOSTIC build):

<!-- RESULTS b: filled from the swarm run -->

---

## (c) Resource bounds

`test/soak/test_soak_resources` runs tens of thousands of sequential SSI
transactions and asserts that region slots, mutexes and lockers return to a
bounded baseline — the direct regression for #137 (unreclaimed committed-reader
lockers) and #138 (leaked MVCC mutex slot). The committed-reader footprint is a
bounded sawtooth whose ceiling is `st_objects / SI_CLEANUP_TRIGGER_DIV`.

Evidence (long run, dev box):

<!-- RESULTS c: filled from the run -->

---

## (d) The false-abort rate

Already measured in `test/bench/SSI-FALSE-ABORT-2026-09.md`. Summary and
application guidance here; this report does not re-run the full sweep (the
measurement stands) but states what an application should expect.

**The number.** On a realistic read-modify-write workload, essentially the
entire SSI abort rate is a **page-granularity artifact**, and the rate ranges
from ~0% to **11.4%** of transactions purely by how many records fit on a leaf:

| pagesize | records/leaf | SSI abort rate (uniform RMW) | of which false |
|---:|---:|---:|---:|
| 512 | 4.0 | 0.000% | n/a |
| 1 024 | 10.0 | 0.018% | ~100% |
| 4 096 | 44.9 | 0.294% | ~100% |
| 8 192 | 90.9 | 1.101% | ~100% |
| 16 384 | 183.5 | 3.880% | ~100% |
| 32 768 | 370.4 | 11.352% | ~100% |

The decisive control (decoy vs split: same empty logical conflict graph,
differ only in whether read keys and write keys share a leaf) attributes ~100%
of those aborts to page sharing. The rate is a function of **records-per-leaf**
and collapses to exactly 0 the moment the read key and the write key stop
sharing a leaf.

**What an application should expect, and how to reduce it.**

1. The false-abort rate is a known, measured function of a knob the application
   already controls: `DB->set_pagesize`. Smaller pages → fewer records per leaf
   → fewer false aborts. 512-byte pages drove the realistic workload to 0.000%.
2. The cost of smaller pages is tree height, I/O amplification and cache
   efficiency — a real trade, not a free win. The honest framing: the
   false-abort tax is decoupled from, not eliminated by, the page-size choice.
3. A **read-only** transaction that does not need SSI can run plain
   `DB_TXN_SNAPSHOT` and take **no** SIREAD markers and never abort for a
   conflict — the correct choice for reporting/read-only work that tolerates
   snapshot (not serializable) semantics. (A read-only *serializable* snapshot
   optimisation — Cahill §6 / PostgreSQL's deferrable read-only — is **not**
   implemented; see §Known limitations.)
4. On a skewed (Zipfian) workload, page-level write-write **deadlocks**
   (`DB_LOCK_DEADLOCK`, counted separately) exceed SSI false aborts (~40% vs
   ~13% at 32 KB pages). Key-precise SSI edges (RFC 0005) would not touch those;
   they come from the page-write-lock protocol.

**Benchmark across record/page sizes on metal:** <!-- RESULTS d: metal re-measure if run -->

---

## (e) API completeness

`DB_TXN_SERIALIZABLE` and the `DB_SNAPSHOT_CONFLICT` / `DB_SNAPSHOT_UNSAFE`
return codes must be reachable and idiomatic from every maintained binding.

Survey at the start of this work:

| Binding | `DB_TXN_SERIALIZABLE` | `DB_SNAPSHOT_CONFLICT` | Idiomatic path |
|---|---|---|---|
| C | yes (`api_flags.in`) | yes (`db.in`) | pass the flag to `txn_begin` |
| Java | yes (`TransactionConfig`) | yes | `TransactionConfig.setSerializable` |
| Tcl | yes (`-snapshot_safe`) | yes | `$env txn -snapshot_safe` |
| C++ | **raw flag only** | yes (shared `db.h`) | **no typed `DbTxnConfig`** — user hard-codes the bit |
| Rust | **missing constant** | present | **cannot reach without editing the binding** |
| Zig | **missing constant** | **missing** | **cannot reach** |

Fixes in this branch (each with a FAIL-before / PASS-after test):

<!-- RESULTS e: filled as the fixes land -->

---

## (f) Documentation

New / updated `docs_src/` pages:

<!-- RESULTS f: filled as docs land -->

- `DB_TXN_SERIALIZABLE` semantics on `txn_begin` and `set_flags`.
- `DB_SNAPSHOT_CONFLICT` / `DB_SNAPSHOT_UNSAFE` on the error-return page, with
  **retry guidance** (treat like `DB_LOCK_DEADLOCK`: abort and retry the whole
  transaction).
- The **MVCC × optimistic-read** interaction: RFC 0007's pin-free fast path
  (`bt_search.c:864-870`) is opt-in via `DB_OPTREAD` and is **disabled for
  multiversion files**, so an SSI transaction (which requires `DB_MULTIVERSION`)
  does not take the optimistic descent. This is correct, not a bug, but it must
  be stated.

---

## Known limitations (documented, not blockers)

These are real and named, carried from the Cahill review and RFC 0003. None of
them is a *correctness* failure; they are performance / expectation items.

- **Page-granularity false aborts** (§d): up to 11.4% on large pages, ~100%
  false. Mitigation: page size, or plain SI for read-only work. Key-precise
  tracking (RFC 0005) is Draft.
- **No read-only safe-snapshot optimisation** (Cahill §6 future work): a
  read-only SSI transaction still takes SIREAD markers and can be aborted.
- **MVCC disables the optimistic-read fast path** (§f): SSI transactions pay
  full pin-and-latch descent cost.
- **Fixed victim selection**: whoever is caught at commit (or arrives late)
  aborts; no age/work policy.

## Verdict

<!-- one paragraph, filled at the end -->
