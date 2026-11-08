<!--
Copyright (c) 2026 libdb contributors.  All rights reserved.
SPDX-License-Identifier: Sleepycat
-->
# test/xa — the X/Open XA resource-manager tests

libdb ships an XA resource manager (`src/xa/xa.c`, `xa_map.c`, exported as
`db_xa_switch`).  It implements the X/Open XA interface a transaction manager
(Oracle Tuxedo, a JTA provider, or any TM) calls to make libdb one resource in
a distributed, two-phase-committed global transaction.

## What runs in CI (no Tuxedo needed)

`tm/` holds a **Tuxedo-free** test harness, run by `test/db/run_xa.sh` (wired
into `test/MANIFEST`, so the `db` tier runs it on every push/PR):

- `tm/xatm.c`, `tm/xatm.h` — a minimal test transaction manager: build an XID,
  call the `db_xa_switch` entry points, drive the recovery scan, and a small
  assert harness that prints XA return codes symbolically.
- `tm/xa_matrix.c` — single-process state-machine matrix: 2PC commit/rollback,
  one-phase commit, prepare-then-commit/rollback, a read-only branch, **two
  libdb environments as two RMs in one global transaction** (real 2PC),
  suspend/resume and JOIN, and every protocol/argument error code
  (XAER_NOTA/PROTO/INVAL/DUPID/ASYNC).  This replaced the old orphaned
  `xa_direct.c`, folding in all of its coverage.
- `tm/xa_recover.c` — **multi-process crash recovery**: a child prepares
  branches and `kill -9`s itself; the parent reopens (XA opens with
  `DB_REGISTER | DB_RECOVER`, so recovery runs), enumerates the in-doubt
  branches with `xa_recover`, and commits some / rolls back others.  A second
  arm crashes during the commit of a two-branch global transaction.

Spec references in the code are to the **X/Open CAE Specification, Distributed
Transaction Processing: The XA Specification** (X/Open Document C193, 1991).

## The real-TM path (needs Oracle Tuxedo, not in CI)

`chk.xa` plus `src1..src5` and `utilities/` are the original Tuxedo sample
servers and the `chk.xa` driver.  They require a Tuxedo install (`TUXDIR`,
`buildserver`, `tmboot`, `atmi.h`, `fml32.h`) that CI does not have, so they are
not wired into `test/MANIFEST` or any workflow.  They are kept as the reference
for exercising the switch under a production transaction manager.
