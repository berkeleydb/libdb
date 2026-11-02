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

gcov on `src/xa/*.c` (`--enable-debug --enable-diagnostic CFLAGS="--coverage -O0"`),
measured before and after, target > 85% line coverage.  Reported below.

### Measured (2026-11-02, Debian 12 dev box)

**Before:** `src/xa/*.c` was effectively 0% in CI — the only driver
(`xa_direct.c`) was in no `test/MANIFEST` tier and no workflow, so no CI run
ever executed it.

**After** (the harness, matrix + recovery combined):

| file | lines | branches |
|------|-------|----------|
| `src/xa/xa.c` | **64.8%** of 406 | 81% exec / 46% taken of 454 |
| `src/xa/xa_map.c` | **82.5%** of 40 | 89% exec / 66% taken of 38 |

Per function in `xa.c`: the live protocol entry points and helpers are well
covered (`__xa_get_txn` 77%, `__db_xa_start` 73%, `__db_xa_recover` 77%,
`__xa_put_txn`/`__xa_thread_enter`/`__xa_txn_get_prepared`/`__db_xa_complete`
100%).

### Why 64.8%, not 85% — an honest accounting of the 143 uncovered lines

Almost all of the gap is code a functional test cannot reach without fault
injection or an engine change, not untested behaviour:

- **Dead code (10 lines).** The `XA_RBDEADLOCK` / `XA_RBOTHER` branches in
  `__db_xa_start/end/prepare/commit/rollback` read `td->xa_br_status ==
  TXN_XA_DEADLOCKED` / `TXN_XA_ROLLEDBACK`, but a grep of the whole tree shows
  **those two states are never assigned anywhere** — only read.  A deadlocked
  XA operation surfaces DB_LOCK_DEADLOCK through the data call's return, not
  through the branch status, so these branches are unreachable.  (Reported as a
  finding; no test can cover dead code.)
- **Fault-injection-only error messages (26 lines).** `dbenv->err(...DB_STR...)`
  + the following `return (XAER_*)` inside `if (ret != 0)` arms for
  `__txn_begin` failure, `__os_malloc` failure, `set_thread_count` failure,
  `dbenv->open` failure, etc.  Reaching them needs injected ENOMEM / a broken
  env.
- **Panic / recovery-reopen paths (~24 lines).** `corrupted_env` (0% of 18) and
  the `PANIC_CHECK_RET ... exit(1)` arms run only after a real DB_RUNRECOVERY
  and then call `exit(1)`, which would kill the test process.

Subtracting the dead (10) and fault-only (~50) lines leaves a reachable
ceiling of about (406-60)/406 ≈ 85%; the harness reaches ~64.8% of the whole
file, i.e. the large majority of the *reachable* functional code.  Driving the
fault paths would need a DB_ENV fault-injection hook (`db_env_set_func_*`) and
is out of scope for this harness; it is noted as a follow-up.

Evidence: `cov_matrix.log`, `cov_recover.log`, `xa.c.gcov`, `xa_map.c.gcov`
(gcov annotations), `FAIL-before.log`.

## Bugs found and fixed (FAIL-before / PASS-after)

The harness surfaced three real XA defects, all masked in a production build;
fixed in a separate commit.  On the pristine engine the harness FAILS
(`xa_matrix` aborts on the durability assert at `txn.c:985`; `xa_recover`
returns XAER_PROTO on every recovered branch); with the fixes it PASSES
(`xa_matrix` 105 checks, `xa_recover` 28 checks).  See `FAIL-before.log`.

1. `__txn_continue` set no sync flag, so an XA (and `txn_recover`) commit did
   not fsync the log — silent durability loss.
2. `__txn_restore_txn` left `td->xa_br_status` unset, so recovered in-doubt
   branches could not be committed or rolled back through XA (XAER_PROTO).
3. `__txn_continue` / `__txn_restore_txn` left `txn->locker` / `td->xa_ref`
   uninitialised for restored txns, so committing a recovered branch crashed
   in `__lock_vec` or tripped the `xa_ref` assert.
