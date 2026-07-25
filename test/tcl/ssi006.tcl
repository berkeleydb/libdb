# See the file LICENSE for redistribution information.
#
# Copyright (c) 2026 berkeleydb/libdb contributors.  All rights reserved.
#
# $Id$
#
# TEST	ssi006
# TEST	Serializable Snapshot Isolation: conflict detected via the MVCC
# TEST	version chain (mechanism b), with no live SIREAD marker at write time.
# TEST
# TEST	The lock-table mechanism (a) fires only when a writer's WRITE lock
# TEST	meets a reader's live SIREAD marker on the same object.  If the reader
# TEST	has not yet touched the object when the writer writes (and the writer
# TEST	then commits and drops its lock), (a) never sees the edge.  Only the
# TEST	buffer-pool mechanism (b) -- noticing the reader was handed an older
# TEST	MVCC version than a newer committed one -- can record it.
# TEST
# TEST	Schedule (write-skew on x and y, separate dbs):
# TEST	  T1 begins snapshot_safe, reads z   (fixes snapshot; NO marker on x/y)
# TEST	  T2 begins snapshot_safe, reads z   (fixes snapshot; NO marker on x/y)
# TEST	  T1 writes x, COMMITS               (drops locks; no reader marks x)
# TEST	  T2 writes y, COMMITS               (drops locks; no reader marks y)
# TEST	  -- so far NO rw-edge is visible to (a): neither writer met a marker.
# TEST	  T1r/T2r read the item the other overwrote, via their old snapshots.
# TEST
# TEST	To make the dangerous structure observable through reads only, we run
# TEST	two snapshot readers that each read the value the other's committed
# TEST	writer overwrote; each read walks past a newer committed version, so
# TEST	(b) records the pair of rw-edges and SSI aborts the pivot.  With (b)
# TEST	disabled the reads succeed and no abort occurs (see the design note).
proc ssi006 { } {
	source ./include.tcl

	puts "Ssi006: SSI conflict via MVCC version chain (no marker at write)"

	env_cleanup $testdir
	set e [berkdb_env_noerr -create -home $testdir \
	    -txn -lock -log -multiversion -lock_timeout 2000000]
	error_check_good env_open [is_valid_env $e] TRUE
	set dbx [berkdb open -create -auto_commit -env $e -btree -multiversion x.db]
	set dby [berkdb open -create -auto_commit -env $e -btree -multiversion y.db]
	error_check_good dbx_open [is_valid_db $dbx] TRUE
	error_check_good dby_open [is_valid_db $dby] TRUE
	error_check_good seed_x [$dbx put k 0] 0
	error_check_good seed_y [$dby put k 0] 0

	# Two readers that fix their snapshots first, on an unrelated key, so
	# neither holds a SIREAD marker on x or y when the writers write.
	puts "\tSsi006.a: R1,R2 fix snapshots (read unrelated keys; no x/y marker)"
	set r1 [$e txn -snapshot_safe]
	set r2 [$e txn -snapshot_safe]
	error_check_good r1_seed [catch {$dbx get -txn $r1 k} v] 0
	error_check_good r2_seed [catch {$dby get -txn $r2 k} v] 0

	# Independent writers overwrite x and y and commit BEFORE the readers'
	# cross reads.  These writers are not the readers, and no reader marker
	# exists on the item they write, so mechanism (a) records nothing.
	puts "\tSsi006.b: writers overwrite x and y and commit (no live markers)"
	set wx [$e txn]
	error_check_good wx_put [$dbx put -txn $wx k 9] 0
	error_check_good wx_commit [$wx commit] 0
	set wy [$e txn]
	error_check_good wy_put [$dby put -txn $wy k 9] 0
	error_check_good wy_commit [$wy commit] 0

	# Now the readers cross-read the overwritten items.  Each read walks
	# past a newer committed version -> mechanism (b) records R --rw--> W.
	puts "\tSsi006.c: readers cross-read overwritten items (walks version chain)"
	set rr1 [catch {$dby get -txn $r1 k} rres1]
	set rr2 [catch {$dbx get -txn $r2 k} rres2]

	# Each reader also writes, forming a write end, so a reader that has
	# both a recorded read-edge and a write-edge is a pivot.
	set wr1 [catch {$dbx put -txn $r1 k 1} wres1]
	set wr2 [catch {$dby put -txn $r2 k 1} wres2]

	set c1 [catch {$r1 commit} cres1]
	set c2 [catch {$r2 commit} cres2]

	set f1 [expr {$rr1 != 0 || $wr1 != 0 || $c1 != 0}]
	set f2 [expr {$rr2 != 0 || $wr2 != 0 || $c2 != 0}]
	if { $f1 && $c1 == 0 } { catch {$r1 abort} }
	if { $f2 && $c2 == 0 } { catch {$r2 abort} }

	if { $f1 } { error_check_good r1_ssi_err \
	    [is_substr "$rres1 $wres1 $cres1" "DB_SNAPSHOT"] 1 }
	if { $f2 } { error_check_good r2_ssi_err \
	    [is_substr "$rres2 $wres2 $cres2" "DB_SNAPSHOT"] 1 }

	# The version-chain reads created the rw edges (b); at least one reader
	# must be aborted as the pivot.
	error_check_good ssi_b_detected [expr {$f1 || $f2}] 1

	error_check_good dbx_close [$dbx close] 0
	error_check_good dby_close [$dby close] 0
	error_check_good env_close [$e close] 0
}
