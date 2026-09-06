# Lock-mode audit: adding a `DB_LOCK_*` mode

Status: enforced (CI)
Applies to: `db_lockmode_t` in `src/dbinc/db.in`

## Why this note exists

GitHub issue #140 was a heap out-of-bounds write in `__lock_vec` and, following
from it, a possible transaction-isolation violation on a replication client.
Neither was a bug in the SSI logic. Both were a bug in the *pre-existing* code
that SSI walked past: RFC 0003 added `DB_LOCK_SIREAD=9` to a 30-year-old enum
without revisiting every site that enumerates lock modes exhaustively.

The concrete failure, in `__lock_vec`'s `DB_LOCK_PUT_READ` handling:

- The temporary `DBT` descriptor array — which becomes the replication commit
  lock list — was **sized** from `sh_locker->nwrites`.
- The loop that **populated** it released (and thus skipped) only the modes it
  named by hand: `DB_LOCK_READ` and `DB_LOCK_READ_UNCOMMITTED`.
- `DB_LOCK_SIREAD` matched neither of those names *nor* `IS_WRITELOCK`. So a
  retained SIREAD marker fell through to the populate branch and consumed a
  descriptor slot the sizing had never allocated.
- The only bounds check was a `DB_ASSERT`, which compiles out of every
  non-`DIAGNOSTIC` build. Release builds corrupted the heap in silence.
- `__lock_fix_list` was then handed `nwrites` rather than the number of
  descriptors actually written, truncating the serialized list. Because newly
  granted locks go to the **head** of the locker's `heldby` list, the SIREAD
  object was visited first and could displace the modified page's write-lock
  object. `__rep_process_txn` reacquires only the listed objects as write locks,
  so apply could change a page a separate client transaction still read-locked.

The generalisable lesson is not "we forgot SIREAD". It is:

> **A write-lock counter must never size a buffer that a mode-enumerating loop
> then fills.** Sizing and population must derive from the *same* predicate, so
> they agree by construction rather than by coincidence.

## The shape to write

Derive the sizing pass and the populate branch from the **same** predicate.
Express that predicate as `IS_WRITELOCK(m)` rather than as a list of mode names:

```c
/* GOOD -- sizing and population cannot disagree, for any future mode. */
nobj = 0;
SH_LIST_FOREACH(lp, &sh_locker->heldby, locker_links, __db_lock)
        if (IS_WRITELOCK(lp->mode))
                nobj++;
objlist->size = nobj * sizeof(DBT);
...
if (objlist != NULL && IS_WRITELOCK(lp->mode))
        ... populate ...
```

```c
/* BAD -- an independent counter sizes what a mode enumeration fills. */
objlist->size = sh_locker->nwrites * sizeof(DBT);
...
if (writes == 1 || lp->mode == DB_LOCK_READ ||
    lp->mode == DB_LOCK_READ_UNCOMMITTED)
        ... release ...
if (objlist != NULL)                    /* everything else falls in here */
        ... populate ...
```

Note what the fix did **not** change: the set of modes `DB_LOCK_PUT_READ`
releases. SIREAD markers must stay on `heldby` past this point so
`__lock_sicommit` can persist or drop them at `__txn_end`; releasing them here
would break SSI. The bug was never "SIREAD is retained" — it was that a retained
non-write lock silently entered a list sized only for write locks. So the fix
narrows the *populate* condition (and the sizing to match) and leaves the
*release* condition alone.

Where the intent genuinely is one specific mode (the lock-coupling decisions in
`__db_lput`, the SIREAD-vs-holders list choice in `__lock_put_internal`), naming
the mode is correct. Record that judgement in the inventory rather than
"fixing" it.

Two further rules:

- **Bound the write at runtime, not just under `DIAGNOSTIC`.** If a mismatch
  would be a memory-safety bug, `DB_ASSERT` is not a bounds check — it is
  documentation. `__lock_vec` now fails the operation via `__env_panic` instead.
- **Pass the count you produced.** Hand downstream serializers the number of
  entries actually populated (`np - (DBT *)objlist->data`), never an
  independently maintained counter that "should" match.

## Checklist: adding a lock mode

CI will not let you skip this — `dist/cocci/lockmode_inventory.sh` fails as soon
as a new `DB_LOCK_*` member appears in `db_lockmode_t`. To make it pass you must:

1. **Conflict matrix.** Bump `DB_LOCK_RIW_N` and add both a **row and a column**
   to `db_riw_conflicts` (`src/lock/lock_region.c`). The matrix is indexed by
   mode value; a missing row is caught at runtime by the
   `lock_mode >= region->nmodes` check in `__lock_get_internal`, but only after
   the mode is already in use. Decide explicitly whether the new mode conflicts
   with each existing one. (SIREAD's row and column are all-zero: a marker never
   blocks and is never blocked, because SSI detects conflicts by walking
   `obj->sireaders`, not through the matrix.)

2. **Read-vs-write classification.** Decide whether the mode is an
   `IS_WRITELOCK` (`src/dbinc/lock.h`). This one decision propagates to
   `nlocks`/`nwrites` accounting, `DB_LOCK_PUT_READ`, `__lock_failchk`'s
   `nlocks == nwrites` test, the deadlock detector's `MINWRITE`/`MAXWRITE`
   policies, and `__txn_doevents`' handle-lock trades. If the answer is "neither
   exactly" — as it is for SIREAD — say so in the inventory note and check that
   every consumer of the classification does the right thing with it.

3. **Which list does it live on?** Every mode but SIREAD lives on
   `obj->holders`. If the new mode needs its own list, audit every walker of
   `holders`/`waiters` (`__lock_promote`, `__lock_put_internal`,
   `__lock_dump_object`, `__dd_build`) for whether it must also walk yours.

4. **Lifetime.** When is the lock released? SIREAD markers deliberately outlive
   `DB_LOCK_PUT_READ` (they are handled by `__lock_sicommit` just before
   `DB_LOCK_PUT_ALL` at `__txn_end`), which is exactly why they were on the
   `heldby` list at the moment `__lock_vec` built the commit lock list.

5. **Exhaustive switches.** Add a `case` arm to every switch marked
   `exhaustive` in the inventory (currently `__lock_printlock` and
   `__db_lockmode_to_string`). CI checks this mechanically.

6. **Every `site` line.** Walk them all and record a verdict. That is the
   deliverable: the inventory is an *auditable* record, not a list of files
   someone glanced at.

7. **Regression test.** Add a case to `test/c/test_lock_sireads.c` (or a sibling)
   that puts the new mode on a locker's `heldby` list at the same time as a write
   lock, and run it under ASan via `test/c/chk.locksireads`. #140 was invisible
   to the entire existing suite because the suite had no test that held two
   different lock modes across a commit-lock-list build.

## What CI enforces

| Guard | File | Failure mode |
| --- | --- | --- |
| New mode in `db_lockmode_t` not recorded | `dist/cocci/lockmode_inventory.sh` | hard fail, not baselined |
| Inventoried enumeration site renamed/removed | same | hard fail |
| `exhaustive` switch missing a `case` for any mode | same | hard fail |
| Allocation sized from `->nwrites` | `dist/cocci/rule_lock_mode_enum.cocci` (`LOCK_MODE_SIZING`) | new violation vs `baseline.txt` |
| Hand-enumerated read-mode test | same (`LOCK_MODE_READTEST`) | new violation vs `baseline.txt` |
| `__lock_vec` overrun at runtime | `src/lock/lock.c` | `__env_panic` in every build, not just `DIAGNOSTIC` |
| The #140 overflow itself | `test/c/chk.locksireads` | ASan heap-buffer-overflow |

Both Coccinelle rules are wired into the existing baseline gate, so they fail on
*new* matches while the sites judged correct stay recorded in
`dist/cocci/baseline.txt`. The inventory script is **not** baselined: it is
absolute.

### Known limitation

Coccinelle cannot express "a `switch` over `db_lockmode_t` that is missing a
`case`" in the spatch build this repo uses — `... when != case X:` inside a
`switch` is a parse error (spatch 1.3.1). That is why the exhaustive-switch check
lives in `lockmode_inventory.sh` (awk over the function body) rather than in
SmPL. The Coccinelle rules cover the two expression-level shapes, where they are
a genuinely better fit than grep.

## Audit performed for #140

See the PR for the full table. Summary: 19 enumeration sites inspected across
`src/lock/`, `src/db/`, and `src/txn/`. One real memory-safety bug
(`__lock_vec`, fixed). Two real reporting bugs (`__lock_printlock` and
`__db_lockmode_to_string` both printed SIREAD as `UNKNOWN`; `__lock_dump_object`
did not walk `obj->sireaders`, so an object pinned only by markers printed as
empty) — fixed. The remaining 15 sites were judged correct, and *why* is recorded
per-site in `dist/cocci/lockmode_inventory.txt` so the judgement can be
re-checked rather than re-derived.
