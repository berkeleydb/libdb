# S1 FINDING: SSI misses a write skew across distant pages of one btree (confirmed)

Reproducer: test/isolation/ssi_multiread_skew.c (independent of the Adya checker;
two threads + barriers + direct state check, no version model).

Shape: one btree, small pages (512), N filler keys; X = min key (key0),
Y = max key (key N+1), so with enough filler they sit on different leaves of a
MULTI-LEVEL tree.
  T1: read Y, write X=1
  T2: read X, write Y=1
Both snapshots taken before either writes. Serial orders give (1,0) or (0,1),
never (1,1). (1,1) with both committed == write skew SSI MUST prevent.

Result (dev box, libdb 2026.10.8 / master e1fe5c2c4, --enable-diagnostic):
  filler <= 10  (shallow tree)  : PASS  0/40 (SSI aborts one)
  filler == 64  (tree splits)   : FAIL 40/40 (both commit)
  filler >= 200                 : FAIL 40/40
  SEQUENTIAL mode (T1 FULLY commits before T2 writes) filler 400: FAIL 40/40
  SEQUENTIAL filler 10: PASS.

SEQUENTIAL is the key: T1 is fully committed before T2 writes, so this is NOT
the #136 commit-window race. It is a pure conflict-DETECTION gap: T2's write to
Y must find T1's SIREAD marker on Y (T1 read Y) and abort; it does not when Y is
far from X in a multi-level tree.

This contradicts RFC 0003's retired claim that the "different pages of one
btree" symptom was only #136. The existing test_iso_anomaly
write_skew_samebtree_control uses 64 filler keys and is documented PASS; my
filler=64 FAILs. Need to reconcile the two constructions (page size, key
layout, cache, timing) to produce a FAIL-before/PASS-after gate.

## ROOT-CAUSE LOCALIZATION (dev box, deterministic)

The trigger is TREE DEPTH, exactly:
  tree: 1 leaf / 1 level  -> PASS (SSI aborts one)
  tree: 2 leaf / 2 levels -> FAIL 20/20
  tree: 3 leaf / 2 levels -> FAIL 20/20
  tree: 4 leaf / 2 levels -> FAIL 20/20

So SSI detects the rw-antidependency within a SINGLE leaf, but misses it the
moment the btree has a root internal page (becomes multi-level). T1 reads Y
(SIREAD marker on Y's leaf); T2 writes Y but does not find/abort on that marker.

STILL TO RECONCILE: existing write_skew_samebtree_control has 33 leaves
(multi-level) and PASSES. Hypothesis: that test's PASS comes from the #136
commit-window path (control timing: T2 commits before T1's commit call), NOT
from the SIREAD marker-on-write lookup. My SEQUENTIAL reproducer (T1 fully
commits first) bypasses the commit-window path and isolates the broken
marker-lookup path. Need to confirm by reading __lock_get_internal SIREAD scan
+ how the read's SIREAD lock object (fileid,pgno,type) is keyed vs the write's
lock object across tree levels.

## MECHANISM (code-localized, bt_search.c)

Reads descend with LOCK COUPLING: BAM_GET_ROOT read-locks the root (bt_search.c
~613), then the descent re-locks each child with LCK_COUPLE_ALWAYS
(bt_search.c:1393-1394) which RELEASES the parent lock as it goes. The
READ->SIREAD remap happens in __db_lget (db_meta.c:1184-1191).

Hypothesis (consistent with "1 level PASS, 2+ levels FAIL"): in a single-level
tree root==leaf==pgno so T1's read SIREAD marker and T2's write WRITE lock hit
the SAME lock object and the SIREAD scan in __lock_get_internal matches. In a
multi-level tree, lock-coupling means the leaf's SIREAD marker is either (a)
dropped by the LCK_COUPLE __LPUT, or (b) left on a page the write never
rescans, so T2's DB_LOCK_WRITE on the leaf finds an EMPTY sh_obj->sireaders and
no rw-edge is recorded -> both commit. The commit-window (#136) path masks this
in the existing write_skew_samebtree_control because its CONTROL timing resolves
the edge via TXN_DTL_SICHECKED, not via the marker lookup; the SEQUENTIAL
reproducer (T1 fully commits first) has no such path and exposes the miss.

## VERDICT IMPACT
SSI is NOT production-ready: a committed write skew is a silent correctness
failure (ACID isolation violated, no error/panic). Q4's "well tested and
functional" is FALSE for multi-level trees == all non-trivial databases.
=> verdict EXPERIMENTAL; fix is RFC 0023 (SIREAD placement under lock coupling),
a design/engine change beyond this workstream's safe scope.
