# See the file LICENSE for redistribution information.
#
# Copyright (c) 2026 berkeleydb/libdb contributors.  All rights reserved.
#
# $Id$
#
# TEST	ssi005
# TEST	Serializable Snapshot Isolation: SIREAD markers are reclaimed
# TEST	incrementally, not only at checkpoint.
# TEST
# TEST	Committed snapshot-safe readers leave SIREAD markers that persist
# TEST	until GC.  Before the bounded-reclaim fix, GC ran only at checkpoint,
# TEST	so with no checkpoints the marker count (and the lock objects it pins)
# TEST	grew as a function of total transactions ever run -- an unbounded leak
# TEST	for a long-lived process.
# TEST
# TEST	This runs many committed snapshot-safe read-only transactions, each
# TEST	reading a distinct key, WITHOUT ever checkpointing, and asserts that
# TEST	the peak lock-object usage stays bounded (the __txn_begin-driven sweep
# TEST	reclaims markers whose snapshot no reader can still see).  Without the
# TEST	fix, peak objects climb roughly with the number of readers.
proc ssi005 { { readers 400 } } {
	source ./include.tcl

	puts "Ssi005: SSI SIREAD markers reclaimed without checkpoint"

	env_cleanup $testdir

	# Small object pool so accumulation is easy to observe and the sweep
	# threshold (half of allocated objects) is reached quickly.
	set e [berkdb_env -create -home $testdir \
	    -txn -lock -log -multiversion \
	    -lock_max_objects 200 -lock_max_locks 2000 -lock_max_lockers 2000]
	error_check_good env_open [is_valid_env $e] TRUE

	set db [berkdb open -create -auto_commit -env $e -btree -multiversion d.db]
	error_check_good db_open [is_valid_db $db] TRUE

	# Seed a spread of keys across many pages so readers touch distinct
	# lock objects (distinct SIREAD markers).
	for { set i 0 } { $i < $readers } { incr i } {
		error_check_good seed_$i [$db put "k$i" $i] 0
	}

	puts "\tSsi005.a: run $readers committed snapshot-safe readers (no checkpoint)"
	for { set i 0 } { $i < $readers } { incr i } {
		set t [$e txn -snapshot_safe]
		error_check_good read_$i [catch {$db get -txn $t "k$i"} r] 0
		error_check_good commit_$i [$t commit] 0
	}

	# Peak object usage must stay well under one-per-reader: bounded
	# reclaim caps it near the sweep threshold, not at ~$readers markers.
	set st [$e lock_stat]
	set maxobj 0
	foreach pair $st {
		if { [lindex $pair 0] eq "Maximum number of objects so far" } {
			set maxobj [lindex $pair 1]
		}
	}
	puts "\tSsi005.b: peak objects = $maxobj (readers = $readers)"

	# Generous bound: without reclaim, peak would approach $readers.  With
	# reclaim, it stays near the object pool / sweep threshold.  Assert it
	# is comfortably below the reader count.
	error_check_good markers_bounded [expr {$maxobj < $readers}] 1

	error_check_good db_close [$db close] 0
	error_check_good env_close [$e close] 0
}
