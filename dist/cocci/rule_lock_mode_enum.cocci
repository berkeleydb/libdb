/*
 * rule_lock_mode_enum.cocci -- flag the two lock-mode patterns that produced
 * GitHub issue #140.
 *
 * WHY: SSI added DB_LOCK_SIREAD=9 to db_lockmode_t without auditing the
 * pre-existing loops that enumerate lock modes exhaustively.  In __lock_vec's
 * DB_LOCK_PUT_READ path the descriptor array was sized from
 * sh_locker->nwrites, while the population loop released only the modes it
 * named by hand (DB_LOCK_READ, DB_LOCK_READ_UNCOMMITTED).  DB_LOCK_SIREAD
 * matched neither those names nor IS_WRITELOCK, so it fell through to the
 * populate branch and wrote a DBT past the allocation -- a heap overflow in
 * every release build (the only bounds check was a DIAGNOSTIC-only DB_ASSERT)
 * plus a truncated, wrong replication commit lock list.
 *
 * Two shapes are flagged.  Neither is automatically a bug; the point is that
 * adding a lock mode must be a deliberate visit to each one.  Sites judged
 * correct live in dist/cocci/baseline.txt, so a NEW match fails CI
 * (.github/workflows/cocci.yml).  The complementary, exhaustive check -- which
 * catches mode SWITCHES too, and which fires when a mode is added to
 * src/dbinc/db.in at all -- is dist/cocci/lockmode_inventory.sh; Coccinelle
 * cannot express "switch statement missing a case" in this spatch build (a
 * `... when != case X:` inside a switch is a parse error), so the inventory is
 * the authority for that shape.  See rfc/0003/lock-mode-audit.md.
 *
 *   //@LOCK_MODE_SIZING@   An allocation size computed from ->nwrites.  A
 *                          write-lock counter must never size a buffer that a
 *                          mode-enumerating loop then fills: the two can
 *                          disagree.  Count with the SAME predicate the loop
 *                          uses (see __lock_vec) so they agree by
 *                          construction.  Expected: ZERO matches.
 *
 *   //@LOCK_MODE_READTEST@ A hand-written read-mode test naming
 *                          DB_LOCK_READ_UNCOMMITTED.  Such a list is silently
 *                          incomplete the moment another non-write mode
 *                          exists (DB_LOCK_SIREAD did exactly this).  Where
 *                          the intent is "every non-write mode", prefer
 *                          !IS_WRITELOCK(m), which covers present and future
 *                          modes.  Where the intent really is that one mode
 *                          (e.g. the lock-coupling decisions in db_meta.c),
 *                          the site is correct -- baseline it.
 *
 * Source-level EARLY WARNING / convention check -- see README.md.  Written as
 * identity transforms because @script:python@ does not work in this spatch
 * build; the produced diff IS the report.
 */

/*
 * 1. Allocation sized from a write-lock counter.
 */
@lock_mode_sizing@
expression e;
type T;
@@
- e->nwrites * sizeof(T)
+ e->nwrites * sizeof(T) //@LOCK_MODE_SIZING@

/*
 * 2. Hand-enumerated read mode.  Matched one comparison at a time: BDB's
 *    conditions are long `||` chains, which parse left-associated, so a
 *    two-operand pattern would not match a subchain.
 */
@lock_mode_readtest@
expression m;
@@
- m == DB_LOCK_READ_UNCOMMITTED
+ m == DB_LOCK_READ_UNCOMMITTED //@LOCK_MODE_READTEST@
