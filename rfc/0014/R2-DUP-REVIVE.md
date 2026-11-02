# R2: equal-key revive for duplicate sets

Design note for the fix that finishes R1 (RFC 0014 §8.3, KNOWN-ISSUES R2).
Not an RFC: a data-integrity bug fix, no on-disk / log / region format change.

## The bug

In a btree whose `bt_compare` equates distinct byte strings ("abc" == "ABC"),
a duplicate set under key "abc" whose members are **all deleted** while a
cursor still pins one of them, followed by a put of an equal key "ABC",
revives the set under the DELETED key's bytes ("abc") instead of storing the
caller's "ABC".

R1 (`50ef9c73f`) fixed the non-duplicate case by rewriting the key item in
place in `__bam_iitem`'s DB_CURRENT path, but `__bam_revive_chk` deliberately
returns no-revive for a key item **shared** by an on-page duplicate set
(`inp[indx] == inp[indx ± P_INDX]`), because a live member might share that
key. That gap is KNOWN-ISSUES R2.

The residual is broader than "DUPSORT on-page": it is every duplicate shape.
Measured on master 2026.10.8 and LTS 2026.10.7.1 (identical):

| case (bt_compare equates, identity dup_compare) | stored key | path |
|---|---|---|
| DB_DUP, all members deleted + pin, put equal | "abc" WRONG | DB_AFTER append copies the shared key |
| DB_DUPSORT, all deleted + pin, put equal | "abc" WRONG | DB_CURRENT overwrite keeps the shared key |
| off-page dup tree (300), all deleted + pin | "abc" WRONG | opd put into the dead tree under the parent key |
| any of the above WITHOUT the pin | "ABC" ok | the set is physdel'd first, so the put inserts fresh |
| delete 1 of 3, 2 live remain | "abc" | CORRECT: the new item joins a live set whose key is "abc" |

The last row is the documented equal-key / shared-key semantics and must not
change. The rule: **while any member of the set is live the set's key stays;
when every member is deleted the set is logically gone, so an equal-key put
must produce the caller's key bytes, exactly as the no-pin control does.**

## The paths

`__bamc_put` (bt_cursor.c), after an exact key match (`exact == 1`):

- non-dup: `iiop = DB_CURRENT` → R1's `__bam_revive_chk` + in-place key replace.
- DB_DUP (unsorted) KEYLAST: walk to the last member, `iiop = DB_AFTER` →
  case 2 **appends** a new slot, copying the shared key via `__bam_adjindx`.
- DB_DUPSORT: walk the sorted on-page set; an equal-comparing deleted member
  gives `iiop = DB_CURRENT` → case 5 overwrites its data, keeping the shared
  key; otherwise `iiop = DB_AFTER`/`DB_BEFORE` appends with the shared key.
- off-page (either): `__bam_isopd` → `goto done`; `__dbc_iput` drives an OPD
  cursor that puts the data into the off-page tree. The off-page tree holds
  data only; the key lives in the parent leaf, which is never rewritten.

## Chosen fix: redirect to a fresh insert when the whole set is deleted

When the exact match's **entire** duplicate set is deleted and its stored key
bytes differ from the caller's, treat the put as if the key did not exist: set
`iiop = DB_KEYFIRST` at the matched slot (exactly the `!exact` branch), which
inserts a fresh standalone key/data pair with the caller's bytes (case 1).
The dead set stays pinned on the page and is physically reaped when the pin
releases, which is already how a pinned-deleted item behaves; a subsequent
search skips it (it is deleted). The result equals the no-pin control: one
live record whose key is "ABC".

This reuses case 1, which already handles overflow keys, the split/retry
(DB_NEEDSPLIT), cursor adjustment (`__bam_ca_di`) and logging. No shared-key
surgery, no off-page-tree key rewrite, no new or changed log record, no format
change.

### Weighed alternative: rewrite the shared key in place (R1's approach A)

Rejected. On-page it is not sufficient — the DB_DUP append path adds a *new*
slot that also shares the key, so rewriting the one physical key item still
leaves the append copying the old offset; you would have to rewrite the key
AND redirect the append. Off-page you would rewrite the parent-leaf key item
of a tree that is about to be reaped. Both are more code and more log records
than the redirect, for the same observable result.

### Detection

A helper in `__bamc_put` (`__bam_dup_alldeleted`), called only on an exact
match in a DB_AM_DUP database when the stored key bytes differ from the
caller's key (so the common identical-byte case pays nothing):

- on-page: walk the slots sharing `inp[cp->indx]` (both directions), return
  "set is gone" only if every member is `B_DISSET`; a live member stops the
  walk at the first hit.
- off-page: walk the off-page dup tree from its root; return "set is gone"
  only if every data item is `B_DISSET`; short-circuit on the first live one.

The byte-difference gate uses the same keydata/overflow comparison as
`__bam_revive_chk` (memcmp for on-page keys, `__db_moff` for overflow keys),
so a growing/shrinking or overflow key is judged correctly.

## Scope of change

- `src/btree/bt_cursor.c`: the detection helper + the redirect in `__bamc_put`.
- No change to `__bam_iitem` (R1's DB_CURRENT key replace is untouched).
- Tests: `test/db/r2_dup_revive.c` + runner + MANIFEST row.

## Correctness checks (in the test)

Every row above for DB_DUP and DB_DUPSORT, on-page and off-page, pin and
no-pin, no-env and txn env; overflow keys; a full-page split; recovery from
the log alone (DB_RECOVER_FATAL, .db removed); DB->verify; the live-member
control unchanged; a sabotage arm that disables the redirect and must FAIL.
