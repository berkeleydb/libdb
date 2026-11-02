<!--
Copyright (c) 2026 libdb contributors.  All rights reserved.
SPDX-License-Identifier: Sleepycat
-->
# A Tuxedo-free XA test harness for libdb

## Why

libdb ships an X/Open XA resource manager (`src/xa/xa.c`, `xa_map.c`,
`db_xa_switch`).  Its only tests needed Oracle Tuxedo (`chk.xa` + `src1..src5`)
or were an orphaned single-process smoke driver (`xa_direct.c`,
`run_xa_direct.sh`) that was in no `test/MANIFEST` tier and no CI workflow, and
hardcoded `-luring` and `libdb-5.3.so`.  Review 2026-11 D-6 (`A/loverso-bram.md`,
action A5): a shipped integration that nothing proves.

This builds a complete, Tuxedo-free XA test harness that runs in CI.

## What a real TM does, and what we reproduce

A transaction manager (Tuxedo, JTA, Blacktie, ...) never touches DB handles to
drive the protocol: it calls the ten `db_xa_switch` entry points with a global
transaction identifier (`XID`) and the `rmid` it assigned the RM at
`xa_open`.  Our test TM (`tm/`) does exactly that.  The spec sections cited in
code comments are from the **X/Open CAE Specification, Distributed Transaction
Processing: The XA Specification** (X/Open Document C193, 1991):

- §2.2 "The Transaction Identifier XID"  — formatID, gtrid_length, bqual_length.
- §2.3 "The XA Interface" / §6 "The xa_* reference pages" — each entry point.
- §4   "State Tables" — the TM/RM association and transaction-branch states that
         decide which calls are legal and what they return.
- Table 6-4 "flag" values — TMNOFLAGS/TMJOIN/TMRESUME/TMSUSPEND/TMSUCCESS/
         TMFAIL/TMONEPHASE/TMSTARTRSCAN/TMENDRSCAN.

The harness asserts BOTH the XA return code AND the resulting committed data,
because a correct return code with wrong data (or vice versa) is the exact class
of bug XA integration leaks.

## Pieces

1. `tm/xatm.c`, `tm/xatm.h` — a minimal test TM library: build an XID
   (formatID/gtrid/bqual), call a named entry point, drive a recovery scan with
   TMSTARTRSCAN/TMENDRSCAN, and a tiny assert harness that records the failing
   XA code symbolically (XA_OK, XAER_NOTA, ...).  Small and readable.

2. `tm/xa_matrix.c` — the single-process matrix.  Each case drives the state
   machine and checks both the XA code and the data:
   - commit (2PC), rollback, one-phase commit (TMONEPHASE);
   - prepare-then-commit, prepare-then-rollback;
   - read-only branch (observe libdb's actual XA_RDONLY behaviour);
   - two libdb environments as two RMs in ONE global txn — real 2PC;
   - suspend/resume (TMSUSPEND/TMRESUME) and JOIN within a thread;
   - protocol/argument errors: commit without prepare (XAER_PROTO), double
     prepare (XAER_PROTO), unknown XID (XAER_NOTA), XID reused while active
     (XAER_DUPID), bad rmid (XAER_PROTO), TMASYNC (XAER_ASYNC),
     xa_complete (XAER_INVAL), bad flags (XAER_INVAL).
   This folds in all of xa_direct.c's coverage (it is retired into this file).

3. `tm/xa_recover.c` — crash recovery, MULTI-PROCESS.  A child process does
   xa_open -> xa_start -> work -> xa_end -> xa_prepare on several XIDs, then
   `kill -9`s itself (in-doubt txns left behind, locks held in the region).  The
   parent then: reopens with DB_RECOVER, runs xa_recover (TMSTARTRSCAN/TMENDRSCAN)
   to enumerate the in-doubt XIDs, commits some and rolls back others, and checks
   the data matches.  A second arm crashes DURING commit (between the two RMs'
   commits in a 2PC) and checks recovery + xa_recover still reconcile.

## Wiring

- `test/db/run_xa.sh` — library discovery copied from `run_dd_stripes.sh`
  (static libdb first, versioned `.so` fallback, `pkg-config --libs liburing`,
  GNU/BSD/mac `timeout`), no hardcoded soname or `-luring`.  Builds the three
  binaries against the just-built libdb and runs them under a timeout.
- `test/MANIFEST`: one `db	run_xa` line (the db tier already runs in ci.yml via
  `run_all.sh`, which reads the manifest; so XA runs on every push/PR with no
  workflow edit).  `git update-index --chmod=+x` on the runner.
- `test/xa/xa_direct.c` + `run_xa_direct.sh`: retired (coverage folded into the
  matrix).  Tuxedo files `chk.xa`, `src1..src5`, `utilities/` are left in place
  (they document the real-TM path) with a README pointer.

## Coverage target

gcov on `src/xa/*.c` (`--enable-debug CFLAGS=--coverage`), measured before and
after, target > 85% line coverage.  Reported in this file at the end.

## Bugs

Any XA defect found gets its own fix commit with a FAIL-before / PASS-after
test, and D-6 is marked fixed in `test/KNOWN-ISSUES.md`.
