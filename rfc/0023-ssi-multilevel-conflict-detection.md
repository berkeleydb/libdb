# RFC 0023: SSI conflict detection must survive multi-level B-tree descent

- **Status:** Draft
- **Type:** Normative
- **Author:** libdb maintainers (workstream S1)
- **Date:** 2026-11-02
- **Tracking:** SSI production-readiness (decision Q4); report
  [`test/audit/SSI-PRODUCTION-2026-11.md`](../test/audit/SSI-PRODUCTION-2026-11.md)
- **Prototype:** reproducer `test/isolation/ssi_multiread_skew.c` (FAILs on
  master); Adya cycle checker `test/isolation/test_ssi_cycle.c`

---

## Summary

`DB_TXN_SERIALIZABLE` (SSI) silently admits a write skew — a committed
non-serializable history, with no error, panic, or corrupt page — whenever the
two conflicting records sit on **different leaves of a multi-level B-tree**.
Detection works only while the tree is a single leaf (one level). The cause is
that a committed reader's SIREAD marker is placed on, or coupled off, a page
that the conflicting writer never rescans, so the `WRITE`-meets-`SIREAD` scan in
`__lock_get_internal` finds an empty reader list and records no
rw-antidependency edge. This RFC specifies the correctness requirement — an SSI
read must leave a SIREAD marker on every leaf whose data it depended on, and a
write must find it — and the fix in the B-tree descent/lock-coupling path.

## Motivation

SSI's entire value is that it prevents the snapshot-isolation anomalies
(write skew, the read-only-transaction anomaly). A mode that prevents them on a
one-page table but not on a real, multi-level B-tree is not serializable in
practice: every non-trivial database is multi-level.

**Evidence (master `e1fe5c2c4`, `libdb 2026.10.8`, `--enable-diagnostic`, dev
box).** `test/isolation/ssi_multiread_skew.c` runs the canonical write skew on
one B-tree:

```
T1: read Y, write X=1
T2: read X, write Y=1          (X = min key, Y = max key, different leaves)
```

Both transactions take their snapshot before either writes. No serial order
yields `(X,Y) = (1,1)`; that state means both committed — a write skew SSI must
prevent. Measured:

| tree shape | result |
|---|---|
| 1 leaf, 1 level | PASS — SSI aborts one, 0/40 skews |
| 2 leaves, 2 levels | **FAIL — 20/20 attempts commit the skew** |
| 3 leaves, 2 levels | **FAIL — 20/20** |
| 22 leaves, 3 levels (512-byte pages) | **FAIL — 20/20** |

The transition is exactly single-level → multi-level. It reproduces in strict
**sequential** timing — T1 *fully commits* before T2 even writes — so it is
**not** the issue-#136 commit-window race (that was a live race inside
`__txn_commit`; here there is no concurrency at commit). It is a conflict-
**detection** miss. The general Adya cycle checker
(`test/isolation/test_ssi_cycle.c`) independently finds the same class of
committed cycle under random workloads at high thread counts, on both B-tree and
Hash.

The existing gate `write_skew_samebtree_control` (33 leaves) passes only because
its **control timing** (T2 commits before T1 calls `commit`) resolves the edge
through the #136 `TXN_DTL_SICHECKED` commit-window protocol, not through the
SIREAD marker lookup — so it never exercised the broken path. RFC 0003's note
that "two records on different pages of one B-tree" was "the same #136
commit-window race, not an extra page-granularity hole" is **incorrect for the
sequential/late timing** and is corrected here.

## North-star check

- Embedded / no-server: unchanged.
- ACID: **this restores** the Isolation guarantee SSI claims; A/C/D unaffected.
- Crash recovery: SIREAD markers are in-memory bookkeeping, not logged; the fix
  changes only where a marker is placed/scanned in a live execution. No log or
  recovery change.
- Access methods: the fix is in the B-tree search/lock path; Hash takes SIREAD
  markers through the same `__db_lget` seam and must be checked (the cycle
  checker shows Hash is also affected at scale). Queue/Recno/Heap unaffected by
  the specific btree-descent mechanism but must be re-qualified by the new gate.
- Multi-process correctness: markers live in the shared lock region; the fix
  does not change their lifetime discipline (`si_ref`), only their placement.
- On-disk/log/region/ABI: **no format change.** No new flag. The conflict
  matrix, `DB_LOCK_SIREAD`, and the public API are unchanged.

## Design

### The requirement

For a serializable read of key *k* that resolves to leaf page *p*, SSI must
leave a `DB_LOCK_SIREAD` marker on the lock object for *p* that:

1. **persists** to the reader's commit-or-later (per the `si_ref` lifetime rule,
   RFC 0003 / #137), and
2. is **found** by any concurrent or later writer that write-locks *p*.

Today requirement (2) fails for multi-level trees.

### Where it breaks

A B-tree read descends with lock coupling:

- `BAM_GET_ROOT` read-locks the root page (`src/btree/bt_search.c:613`).
- The descent re-locks each child with `LCK_COUPLE_ALWAYS`
  (`src/btree/bt_search.c:1393-1394`), which acquires the child lock and
  **releases the parent lock**.
- `__db_lget` remaps a `DB_LOCK_READ` under a serializable snapshot txn to
  `DB_LOCK_SIREAD` (`src/db/db_meta.c:1184-1191`).

A write locks the leaf directly: `ACQUIRE_CUR(dbc, DB_LOCK_WRITE, cp->pgno, …)`
(`src/btree/bt_cursor.c`), and the `WRITE`-meets-reader scan walks
`sh_obj->sireaders` for **that leaf's** lock object
(`src/lock/lock.c:1251-1345`).

When the tree is a single leaf, root == leaf == the same `pgno`, so the read's
SIREAD marker and the write's lock object coincide and the scan matches. When
the tree is multi-level, the two diverge, and the reader's marker is either
dropped by the coupling `__LPUT` or left on a page the leaf write never
rescans — so the writer's scan sees no marker and records no edge.

### The fix (specification; implementation to follow)

The SIREAD marker for a serializable read must end up on, and stay on, the
**leaf page that produced the value the reader depends on**, independent of tree
height. Concretely, one of:

1. **Do not couple away a SIREAD marker.** `LCK_COUPLE`/`LCK_COUPLE_ALWAYS`
   release the parent lock when acquiring the child. For a SIREAD marker on a
   leaf (the page whose data was actually read), coupling must *retain* it while
   still releasing interior-page markers that were only traversed. The leaf
   marker is the one the writer scans; interior markers are not needed for
   item-level conflicts (they would only matter for predicate/range locking,
   out of scope).
2. **Acquire the leaf SIREAD after the descent, keyed on the leaf `pgno`**, in
   the same place the equivalent write would lock, so read and write provably
   name the same lock object.

The acceptance test is binary and already written: `ssi_multiread_skew` must go
from 20/20 skews to 0/40 at every tree depth and page size, and
`test_ssi_cycle` must report no cycle under SSI at every thread count on B-tree
and Hash, while both still report the anomaly under plain SI (anti-vacuity).

### Hash

Hash has no multi-level descent, but the cycle checker shows Hash also admits
cycles at scale. The Hash read/write lock-object identity (bucket page vs.
the page a reader actually touched, and overflow-page chains) must be audited
against the same requirement. The fix may be shared (the `__db_lget` seam) or
access-method-specific; the gate covers both.

## Alternatives considered

- **Key-precise (row-level) SIREAD markers (RFC 0005).** A larger change that
  also fixes the *false-abort* problem, but it does not, by itself, guarantee
  the marker is placed and found across tree levels — the placement/coupling bug
  is orthogonal and must be fixed regardless. RFC 0023 is the smaller,
  correctness-only change; RFC 0005 remains the throughput change.
- **Document the limitation and keep SSI experimental.** Acceptable as an
  interim posture (this report recommends exactly that until the fix lands), but
  not a resolution: a silent isolation violation is the one class of defect an
  ACID library must not ship as "production".

## Risks & open questions

- **Which site to fix.** Retaining the leaf SIREAD across coupling
  (`bt_search.c:1393`) is the most localized change but must not accidentally
  retain interior-page markers (region-growth / false-abort regression) or drop
  the leaf marker on the error/retry paths in the same loop.
- **Lock-coupling invariants.** `LCK_COUPLE_ALWAYS` is load-bearing for the 2PL
  read path; the change must be scoped to the SIREAD mode only, leaving ordinary
  read-lock coupling untouched.
- **Hash scope.** Whether one seam fixes both access methods is open until the
  Hash lock-object identity is audited.
- **Performance.** Retaining leaf SIREAD markers is what SSI already intends; no
  new cost beyond markers that *should* have existed. The false-abort rate
  (RFC 0005 / `SSI-FALSE-ABORT`) is unchanged by correct placement.

## Prototype / evidence

- `test/isolation/ssi_multiread_skew.c` — deterministic reproducer; FAIL-before
  gate. Single-level PASS, multi-level FAIL 20/20, sequential timing, every page
  size.
- `test/isolation/test_ssi_cycle.c` — Adya dependency-cycle checker over random
  workloads; corroborates on B-tree and Hash at scale and provides the general
  acceptance oracle (anti-vacuity under SI, acyclic under corrected SSI).

---

## Decision

*(Filled by the reviewer when the RFC is decided.)*

- **Decision:** Pending — 2026-11-02 (Draft).
- **Rationale:**
- **Conditions / follow-ups:** implement the fix on `master`; flip
  `ssi_multiread_skew` and the high-concurrency `test_ssi_cycle` from FAIL to
  PASS as the regression gate; re-evaluate the production/experimental posture
  of `DB_TXN_SERIALIZABLE` once green.
