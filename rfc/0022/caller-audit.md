# RFC 0022 evidence: `__db_new` / `__db_free` caller audit

> **Note (2026-11-03):** the phase-1 sub-transaction implementation was found
> unsafe and reverted (see the RFC's *Implementation findings*). This caller
> audit stays valid: it establishes *which* callers need the per-page lock held
> to commit, which is unchanged and is a prerequisite for any correct NTA
> design (including the recommended phase-2 split/SMO-as-NTA). The unsafety was
> in the lock-manager mechanics (child-locker inheritance / aliasing), not in
> this caller analysis.

This is the §4.3 safety-argument backing for RFC 0022. For the NTA to preserve
isolation, the newly-allocated page's **page lock** (`*lockp`) must stay with the
parent transaction and be held to commit, while only the **meta-page lock** moves
to the short-lived NTA child. A caller that needs reader-invisibility of the new
page must pass a non-NULL `lockp` and keep it; a caller that passes `NULL` must
allocate a page that is unreachable until the parent links it in under its own
held locks.

Tree: `src/` at the branch base. Audit is of every `__db_new` call site.

## `__db_new` callers

| site | `lockp` | holds to commit? | reader-invisible how | NTA-safe? |
|---|---|---|---|---|
| `bt_split.c:238` (`__bam_root`, left) | `&llock` | yes (split keeps `llock`) | page lock on new leaf | yes — parent keeps `llock` |
| `bt_split.c:239` (`__bam_root`, right) | `&rlock` | yes | page lock on new leaf | yes |
| `bt_split.c:424` (`__bam_page`) | `&rplock` | yes | page lock on new page | yes |
| `bt_put.c:1105` (off-page dup) | `&lock` | yes | page lock on dup page | yes |
| `bt_open.c:659` (initial root) | `NULL` | n/a | DB create path, single-threaded, not yet published | yes — no concurrent reader exists |
| `db.c:345` (subdb meta create) | `NULL` | n/a | subdb not yet in master db until parent links it | yes — unreachable until linked |
| `db_compact.c:442` | `&lock` or `NULL` | yes when locking | page lock; compact runs its own txn | yes |
| `db_overflow.c:320` | `NULL` | n/a | overflow page unreachable until the item that points to it is written under a held page lock | yes |
| `hash_dup.c:280` | `NULL` | n/a | dup page unreachable until linked | **phase 2** (hash on inline path) |
| `hash_page.c:2809` | `NULL` | n/a | new bucket page; hash metapage lock reworked by RFC 0017 | **phase 2** |

Conclusion: every non-hash caller either keeps a page lock to commit (so the NTA
only needs to move the *meta* lock) or allocates a page that is unreachable by
any concurrent transaction until the parent links it under its own held locks.
The NTA is isolation-safe for btree/recno/queue/heap. Hash is deferred to phase 2
(RFC 0022 §3.4) because its metapage lock scope is being reworked by RFC 0017.

## `__db_free` callers

| site | parent holds freed page lock? | NTA-safe? |
|---|---|---|
| `bt_cursor.c:595` | yes (page being deleted) | yes — parent retains lock until commit (RFC 0022 §5.2) |
| `bt_delete.c:342`, `:468` | yes | yes |
| `db.c:251`, `:257` | yes | yes |
| `db_compact.c:465`, `:545` | yes | yes |
| `db_overflow.c:490` | yes | yes |
| `db_reclaim.c:92` | yes (truncate/reclaim) | yes |

For every free site the parent already holds a write lock on the page it is
freeing; RFC 0022 §5.2 keeps that lock (instead of today's behaviour where the
page is nominally free but no new allocation can take it until commit because
`__db_new` re-`__db_lget`s the free page with `DB_LOCK_WRITE` and blocks on the
freeing parent). No caller frees a page it does not already have locked.

## The trap this rules out

`__db_new`'s comment at `db_meta.c:150-156` ("no one will hold a free page
locked") is **false** under the NTA for the free case — the freeing parent holds
it until commit. The audit confirms `__db_new` already defends against this: it
calls `__db_lget(dbc, 0, pgno, DB_LOCK_WRITE, lockp)` on the free page before
using it, which blocks on the freeing parent's retained lock. The comment is
updated in the implementation; no caller relies on the "no one holds a free page
locked" assumption for correctness (verified: the assertion
`DB_ASSERT(dbp->env, h->pgno != next_pgno)` and the `TYPE(h) == P_INVALID` check
are the only guards, both preserved).
