# R2: equal-key revive for duplicate sets

Design note for the fix that finishes R1 (RFC 0014 §8.3, KNOWN-ISSUES R2).
Not an RFC: a data-integrity bug fix, no on-disk / log / region format change.

## The bug

In a btree whose `bt_compare` equates distinct byte strings ("abc" == "ABC"),
a duplicate set under key "abc" whose members are **all deleted** while a
cursor still pins one of them, followed by a put of an equal key "ABC",
revived the set under the DELETED key's bytes ("abc") instead of storing the
caller's "ABC".

R1 (`50ef9c73f`) fixed the non-duplicate case by rewriting the key item in
place in `__bam_iitem`'s DB_CURRENT path, but `__bam_revive_chk` deliberately
returns no-revive for a key item **shared** by an on-page duplicate set
(`inp[indx] == inp[indx ± P_INDX]`), because a live member might share that
key. That gap is KNOWN-ISSUES R2.

The residual is broader than "DUPSORT on-page": it is every duplicate shape.
Measured on master 2026.10.8 and LTS 2026.10.7.1 (identical):

| case (bt_compare equates, identity dup_compare) | stored key |
|---|---|
| DB_DUP, all members deleted + pin, put equal | "abc" WRONG |
| DB_DUPSORT, all deleted + pin, put equal | "abc" WRONG |
| off-page dup tree (300), all deleted + pin | "abc" WRONG |
| any of the above WITHOUT the pin | "ABC" ok (set reaped, put inserts fresh) |
| delete 1 of 3, 2 live remain | "abc" CORRECT (joins a live set) |

The last row is the documented equal-key / shared-key semantics and must not
change. The rule: **while any member of the set is live the set's key stays;
when every member is deleted the set is logically gone, so an equal-key put
must produce the caller's key bytes, exactly as the no-pin control does.**

## Chosen fix: rewrite the set's shared key item in place

The members of a duplicate set share **one** physical key item: every member
slot's `inp[]` points at the same offset. So when the whole set is deleted and
its stored bytes differ from the caller's, a single in-place rewrite of that
one key item changes the key for the entire set at once, and the new member
then joins by the ordinary duplicate path. The set stays one set, and a
subsequent equal-key put finds the set already carrying the caller's bytes, so
it simply joins too — no second key item is ever created.

`__bamc_put` (bt_cursor.c), after an exact key match on a `DB_AM_DUP`
database, when `__bam_dup_alldeleted` reports the set entirely deleted with
differing stored bytes, calls `__bam_dup_rekey` and then falls through to the
normal duplicate logic:

- **on-page, non-overflow key:** `__bam_ritem` replaces the shared item. It
  shifts the page's data region for a grow/shrink and re-aims **every** `inp[]`
  offset that referenced the moved region — including each member slot sharing
  the key — so all members follow the move. Its `__bam_repl` log record is
  position-indexed and replays through the same `__bam_ritem` on redo/undo, so
  recovery is correct with **no new or changed log record**. A growing key that
  will not fit returns `DB_NEEDSPLIT`; `__bamc_put`'s existing retry splits on
  the caller's key and re-drives the whole put.
- **off-page duplicate tree:** the key is a lone (unshared) slot in the parent
  leaf — the sibling data slot is a `B_DUPLICATE` pointer. It is rewritten by
  delete-and-re-add in the same slot, as R1 does for a non-dup slot, which also
  covers overflow keys and `B_KEYDATA`↔`B_OVERFLOW` transitions there.

While any member is live, `__bam_dup_alldeleted` stops at the first live member
and reports the set present, so the normal shared-key paths run and the set's
key does not move. The dead members stay deleted and the pinning cursor still
sees them as deleted: a key item is never `B_DISSET` (only its data slots are),
and `__bam_ritem` preserves that state.

### Residual: an on-page set whose SHARED key is an overflow item

Left under its old key bytes. There is no logged in-place primitive that
replaces a leaf item with an arbitrary overflow header while keeping the shared
on-page offset the other members reference; a delete-and-re-add would shift the
index table and break the sharers, and `__bam_ritem` only writes a `B_KEYDATA`
item. The put then joins the set under the old key bytes — **a single valid
set** (`DB->verify` clean, `DBC->count` correct), exactly as master behaves,
which is not corruption. The exposure is narrow: a duplicate set of keys each
large enough to be an overflow key, all deleted while pinned, put an
equal-comparing key. Recorded in KNOWN-ISSUES R2.

### Rejected: redirect to a fresh insert (the first attempt)

The first version set `iiop = DB_KEYFIRST` to insert a fresh standalone
key/data pair next to the still-pinned dead set. A **second** equal-key put
then searched, found the dead set again (an exact match, still all-deleted),
and redirected **again**, inserting a second standalone key item. Two
equal-comparing keys then formed two separate non-dup key items, which
`DB->verify` rejects ("non-dup dup key at entry N"); `DBC->count` reported 1
instead of 2 and a reverse walk dropped a trailing record. The tree was
structurally corrupt (lead-reproduced with `r2_walk_corruption.c`).
Equal-comparing keys must form **one** duplicate set, so the key of the
existing set must move — a fresh insert next to it cannot be correct.

### Rejected: split the shared key off the set

More code and more log records than the in-place rewrite, for the same
observable result, and it does not help the overflow residual either.

### Detection

`__bam_dup_alldeleted`, called only on an exact match in a `DB_AM_DUP`
database when the stored key bytes differ from the caller's (so the common
identical-byte case pays nothing):

- on-page: walk the slots sharing `inp[cp->indx]` (both directions), return
  "set is gone" only if every member is `B_DISSET`; a live member stops the
  walk at the first hit.
- off-page: walk the off-page dup tree from its root; return "set is gone"
  only if every data item is `B_DISSET`; short-circuit on the first live one.

The byte-difference gate uses the same keydata/overflow comparison as
`__bam_revive_chk` (memcmp for on-page keys, `__db_moff` for overflow keys),
so a growing/shrinking or overflow key is judged correctly.

## Scope of change

- `src/btree/bt_cursor.c`: `__bam_dup_alldeleted` (detection), `__bam_dup_rekey`
  + `__bam_rekey_slot` (the in-place rewrite), and the call in `__bamc_put`.
- No change to `__bam_iitem` (R1's DB_CURRENT key replace is untouched; a
  comment there is refreshed).
- Tests: `test/db/r2_dup_revive.c` + runner + MANIFEST row.

## Correctness checks (in the test)

Every case does TWO equal-comparing puts (so the corruption path is exercised)
and asserts, always: `DB->verify` clean, `DBC->count` = live set size, and a
forward (DB_NEXT) and reverse (DB_LAST+DB_PREV) walk returning the same
complete mirrored set of live records, with the two new records under one key.
Key bytes are "ABC" for the rewritten shapes and the documented "abc" for the
overflow-shared-key residual. Coverage: DB_DUP and DB_DUPSORT, on-page and
off-page, pin and no-pin, no-env and txn; a tall off-page tree; overflow keys
on-page (residual) and off-page (rewritten); a growing key that forces the
in-place rewrite to split; the live-member control; recovery from the log
alone (DB_RECOVER_FATAL, .db removed); and a sabotage arm that disables the
rewrite and must FAIL. Teeth confirmed on three libraries: the first R2 fix
FAILs (DB_VERIFY_BAD + wrong count), master FAILs (wrong key bytes), the new
fix PASSes.
