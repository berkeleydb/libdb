/*
 * rule_relaxed_refcount.cocci -- flag relaxed atomic reads of ownership state.
 *
 * The atomics-ordering work (perf/atomics-ordering) established that some
 * atomic fields MUST be read with ACQUIRE ordering (atomic_read), never with
 * atomic_read_relaxed:
 *
 *     - bhp->ref / ->refcount   buffer-header reference counts
 *     - mfp->writers            active-writer counts
 *     - mfp->multiversion       MVCC enable flag gating page visibility
 *
 * A relaxed read of any of these can observe a stale count and free/reuse a
 * buffer another thread still holds, or take the wrong MVCC path.  Pure
 * statistics counters (hash_page_dirty, nsireaders, wired_pages) are allowed
 * to use atomic_read_relaxed and are NOT matched here.
 *
 * This is a source-level EARLY WARNING (a convention check), not the
 * authoritative ABI gate -- see README.md.  Reported as an identity transform
 * tagged //@RELAXED_REFCOUNT@ so the diff IS the report and violations are
 * counted with:  spatch ... | grep '//@RELAXED_REFCOUNT@'
 */

@relaxed_ref@
expression base;
identifier fld =~ "^(ref|refcount|writers|multiversion)$";
@@
- atomic_read_relaxed(&base->fld)
+ atomic_read_relaxed(&base->fld) //@RELAXED_REFCOUNT@
