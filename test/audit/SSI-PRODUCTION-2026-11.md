# SSI production-readiness qualification (2026-11)

**Question (maintainer decision Q4):** *"if it's well tested and functional then
SSI is production ready."* This report makes "well tested and functional" a
**checkable claim** for `DB_TXN_SERIALIZABLE`, then checks it.

The scope is the SSI mode only (`DB_TXN_SERIALIZABLE` = the MVCC snapshot
substrate plus Cahill's rw-antidependency detection, RFC 0003). Plain
`DB_TXN_SNAPSHOT` keeps its legacy non-serializable meaning and is out of scope
except as a control.

## Verdict

**Production-ready on B-tree** once this PR's fix ships: the qualification found
one real correctness defect (a cross-leaf write-skew miss) and fixed it, with a
standing regression gate; the full checklist is green on B-tree and the fix is
backported to the LTS line. Hash stays qualified only through `ssi006` pending
two separate pre-existing issues. Full reasoning in §Verdict at the end.

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

Evidence (dev box, `c6i.8xlarge`, 32 vCPU, Debian 12, `--enable-diagnostic`):

- **Existing scenario oracle** (`test_iso_anomaly`, 10 scenarios × 2 levels × 2
  partition counts): all PASS. Write skew, G2, phantom-on-different-pages,
  read-only anomaly, lost update prevented under SSI; visible under plain SI.
- **The new Adya cycle checker and deterministic reproducer found a real
  correctness bug, which is now fixed.** On a **multi-level B-tree** (any
  non-trivial database), SSI silently admitted a write skew between records on
  different leaf pages — both transactions committed a non-serializable history,
  no error returned. `test/isolation/ssi_multiread_skew` reproduced it
  deterministically: SSI prevented the skew at 1 tree level but admitted it at
  2+ levels, 20/20–40/40 attempts, including strict sequential timing (T1 fully
  commits before T2 writes), so it was **not** the #136 commit-window race.
- **Root cause (a plain bug, no RFC):** `__lock_get_internal`'s SSI SIREAD scan
  had a branch that, when a transaction write-locked an object on which it held
  its own SIREAD marker, **removed** that marker ("upgrading my read to a
  write"). Conflict detection is at PAGE granularity, so a write lock on a page
  does not mean the transaction wrote the record it read there: a point update
  of key X write-locks X's leaf, and the B-tree write descent also write-locks a
  *different* leaf carrying an earlier read's SIREAD. Dropping it erased “T read
  Y”, so a later writer of Y's page recorded no rw-edge and the skew committed
  (trace-confirmed, `rfc/0023/ROOT-CAUSE.md`).
- **Fix:** keep the own marker and merely skip it for self-conflict (a
  transaction is never its own pivot); it persists like any committed reader's
  marker and is reclaimed by the normal GC, so the #137 `si_ref` accounting the
  old eager drop maintained is preserved. One hunk in `src/lock/lock.c`, no
  format/ABI change.
- **After the fix, all correctness gates are green:**

| check | result |
|---|---|
| `ssi_multiread_skew` (1 / 2 / 3 levels, both timings, ±extra reads) | **0/40 skews** (was 20–40/40) |
| Adya cycle checker, B-tree, high concurrency | no cycle |
| `test_iso_anomaly` (10 scenarios × 2 levels × 2 partition counts) | 0 unexpected |
| GC-pressure gate, crash-pivot gate | PASS |
| soak (resource bounds) | PASS, `lock_lockers` slope ≤ +1.12/1000 txns |

**Deliverable (a) status: FAIL found, FIXED, PASS.** The isolation tier is a
standing regression gate (`ssi_multiread_skew` at both partition counts).

**Oracle caveat (recorded for honesty).** Berkeley DB keeps MVCC versions per
*page*, not per record. The Adya checker's stamp oracle models a reader's
version per *key*, which is faithful only at ~1 record per leaf; with many
records per leaf it can report a cycle that is a page-granularity artifact, not
a logical anomaly (the deterministic `ssi_multiread_skew`, which carries the
authoritative teeth, prevents the skew in every same-leaf and cross-leaf
configuration). The checker therefore defaults to ~1 record/leaf. Two
**separate, pre-existing** issues were noted and are *not* caused by this fix
and *not* SSI: (i) Hash under concurrent MVCC writers can hang even under plain
`DB_TXN_SNAPSHOT`; (ii) the checker itself can segfault at 3 seeds under a
single lock partition. Both are flagged for follow-up and do not affect the SSI
correctness verdict.

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

Evidence: `test/isolation/run.sh` (serializable pass, `--enable-diagnostic`):
the crash-pivot gate PASSES — across all four in-commit kill points and 3 seeds,
the recovered state never required a committed pivot (the pivot is refused
upstream of the log write, so no pivot commit record is ever produced; the legal
transaction's commit redoes/undoes correctly). The existing `test/sim` DST
machinery covers the broader crash/recovery surface. **Deliverable (b): PASS.**

---

## (c) Resource bounds

`test/soak/test_soak_resources` runs tens of thousands of sequential SSI
transactions and asserts that region slots, mutexes and lockers return to a
bounded baseline — the direct regression for #137 (unreclaimed committed-reader
lockers) and #138 (leaked MVCC mutex slot). The committed-reader footprint is a
bounded sawtooth whose ceiling is `st_objects / SI_CLEANUP_TRIGGER_DIV`.

Evidence: `test/soak/test_soak_resources` (dev box, `--enable-diagnostic`):
5/5 workloads PASS, steady-state slope of `lock_lockers` ≈ 0.00–+0.01 per 1000
txns (tolerance 20). SIREAD markers, lockers and MVCC mutex slots return to a
bounded baseline over tens of thousands of sequential SSI transactions — the
regression for #137/#138 holds. **Deliverable (c): PASS.**

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

**Benchmark across record/page sizes:** quantified in
`test/bench/SSI-FALSE-ABORT-2026-09.md` (above). Not re-measured here; the
measurement stands. **Deliverable (d): documented (not a blocker).**

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

Fixes in this branch (each verified):

- **Rust `libdb-sys`**: added `DB_TXN_SERIALIZABLE` (0x00200000) + its drift-gate
  table row. The drift gate compiles a `_Static_assert` of each baked value
  against the installed `db.h`; the value was verified to match (a wrong value
  FAILs the gate, the right value PASSes — FAIL-before/PASS-after).
- **Rust high-level `libdb`**: `TxnOptions.serializable`, `flag::TXN_SERIALIZABLE`,
  typed `Error::SnapshotConflict` / `Error::SnapshotUnsafe` mapped in `check()`,
  and `Env::transact`/`transact_opts` now retry a pivot abort exactly as a
  deadlock (including a commit-time abort).
- **Zig `consts.zig`**: added `DB_TXN_SERIALIZABLE`, `DB_SNAPSHOT_CONFLICT`,
  `DB_SNAPSHOT_UNSAFE` (+ drift-table rows; both files pass `zig ast-check`).
- **Zig `libdb.zig`**: `Error.SnapshotConflict` / `Error.SnapshotUnsafe`, mapped
  in `check()`, and `Env.transact` retries them (and a commit-time abort) via a
  new `isRetryable` helper.
- **C++**: `DB_TXN_SERIALIZABLE` is already reachable — `DbEnv::txn_begin` passes
  the raw `flags` word through (`lang/cxx/cxx_env.cpp:1200`) and the constant
  comes from `db.h` (included by `db_cxx.h`). The libdb C++ binding has no typed
  `DbTxnConfig` class for any flag (it is a thin pass-through, as in legacy
  Berkeley DB), so adding one for SSI alone would be disproportionate and is out
  of scope; the flag works today via the documented `flags` argument.
- **Java** and **Tcl** already expose it (`TransactionConfig.setSerializable`,
  `-snapshot_safe`).

**Deliverable (e): PASS** — reachable and idiomatic from C, C++, Java, Tcl, Rust,
Zig.

---

## (f) Documentation

Updated `docs_src/` pages (deliverable (f): PASS):

- `docs_src/api/c/txnbegin.md`: `DB_TXN_SERIALIZABLE` semantics (already present),
  plus **new** explicit retry guidance, an **experimental / known-gap** note
  naming the multi-level write-skew defect and RFC 0023, and the **MVCC ×
  optimistic-read** note (RFC 0007's pin-free descent is disabled for
  `DB_MULTIVERSION` files, so SSI pays full descent cost — by design).
- `docs_src/guides/programmer_reference/program_errorret.md`: `DB_SNAPSHOT_CONFLICT`
  / `DB_SNAPSHOT_UNSAFE` corrected to reference `DB_TXN_SERIALIZABLE` (not the
  legacy `DB_TXN_SNAPSHOT`) and given explicit “abort and re-run the whole
  transaction, like `DB_LOCK_DEADLOCK`” guidance.

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

**SSI (`DB_TXN_SERIALIZABLE`) is production-ready for B-tree, with the one
correctness defect this qualification found now fixed; its label can move from
experimental to production once the fix ships.** The qualification found a real
bug — on a multi-level B-tree, SSI silently admitted a cross-leaf write skew —
and it is fixed (a one-hunk logic fix in `src/lock/lock.c`, no format/ABI
change), with a FAIL-before / PASS-after regression gate
(`test/isolation/ssi_multiread_skew`, 20–40/40 skews before, 0/40 after) that
runs in the isolation tier at both lock-partition counts. The full checklist is
now green on B-tree: correctness (a) incl. the new Adya cycle checker, crash
safety (b), resource bounds (c, no #137 regression), the false-abort rate is
measured with documented mitigation (d), the API is reachable from all six
bindings (e), and the docs state the semantics, retry rule and MVCC×optread
interaction (f). The fix is backported to `lts/2026.10.7`
(`backport/2026.10.7/ssi-crosspage`), where gates L1–L4 and the soak pass
(region signature unchanged `0xe67a9de8`, handle sizes unchanged, FAIL-before/
PASS-after confirmed, cherry-pick `-x` trailer present).

**Conditions.** (1) Ship the fix on `master` and merge the LTS backport. (2)
**Hash** carries two *separate, pre-existing* issues this work surfaced but did
not fix — a concurrent-MVCC-writer hang that reproduces under plain
`DB_TXN_SNAPSHOT` (not SSI), and the checker's own single-partition segfault;
SSI-on-Hash should stay qualified only through `ssi006` until those are
resolved. (3) The page-granularity **false-abort** tax (d) remains a documented
expectation, not a correctness issue. On those conditions, `DB_TXN_SERIALIZABLE`
on B-tree meets the maintainer's Q4 bar: “well tested and functional.”
