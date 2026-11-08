# S1 ROOT CAUSE (confirmed by trace): SSI self-upgrade drops a still-needed SIREAD marker

Trace (ssi_multiread_skew, filler=64, 2-level tree, X=key0 leaf pgno2, Y=key65 leaf pgno5):

  SIREAD take pgno=5 (T1 reads Y)
  SIREAD take pgno=2 (T2 reads X)
  WRITE scan pgno=5 sireaders=1 acquirer=T1   <-- T1 (writing X) write-locks pgno5 (Y's leaf)
  SELF-UPGRADE drop SIREAD pgno=5             <-- T1 drops ITS OWN marker on Y's leaf
  WRITE scan pgno=2 sireaders=1 acquirer=T1
  WRITE scan pgno=2 sireaders=1 acquirer=T2
  SELF-UPGRADE drop SIREAD pgno=2             <-- T2 drops its own marker on X's leaf
  WRITE scan pgno=5 sireaders=0 acquirer=T2   <-- T2 writes Y, finds NO marker -> no conflict
  => both commit, write skew.

MECHANISM: __lock_get_internal's SIREAD scan has a branch
  if (lock_mode == DB_LOCK_WRITE && sh_off == sireadlp->holder) { drop the SIREAD }
intended as "I'm upgrading MY OWN read of this key to a write, so the read is
subsumed by the write." That is only valid if the writer writes the SAME datum
it read. With PAGE granularity, T1's write of X (key0) acquires a WRITE lock on
pgno5 (Y's leaf) -- and T1 had only READ Y, never wrote it. Dropping T1's SIREAD
on pgno5 destroys the record of "T1 read Y", so T2's later write of Y finds no
marker and the rw-antidependency T1->T2 is never recorded. Symmetrically T2.

WHY T1's write of X touches pgno5: TBD (btree write stack / lock coupling across
leaves during the SR_WRITE search). But the DEFECT is independent of that: the
self-upgrade must NOT drop a marker for a key the transaction did not write.
Page granularity means "writer holds WRITE on the page carrying my SIREAD" does
NOT imply "I upgraded that read to a write."

This is why single-leaf passes: there, X and Y share the one leaf, so when T1
drops its marker on that leaf it is ALSO the page it writes, and T2's write of Y
on the same page still collides on... no -- single-leaf passes because T1 and T2
both land on the one page and the FIRST writer's marker is still seen by the
second before its own self-upgrade. (Exact single-leaf ordering differs; the
multi-level drop is the bug.)

FIX DIRECTION (plain bug, NO RFC): the self-upgrade drop must be conditional on
the writer actually writing the key/record the SIREAD covers, not merely holding
a WRITE on the same page. At page granularity the safe conservative fix is: do
NOT drop another-key SIREAD on self-upgrade -- keep the marker unless the write
is to the same record. Simplest correct version: remove the early self-drop and
let the marker persist (a self-conflict T1->T1 is harmless: a txn is never its
own pivot because the rw edge needs two DISTINCT txns; the commit-time pivot
check and the scan already skip same-locker via sh_off==holder for CONFLICT
purposes). Must verify this does not reintroduce the #137 si_ref leak the drop
was added to balance.
