# See the file LICENSE for redistribution information.
#
# Copyright (c) 2026 berkeleydb/libdb contributors.  All rights reserved.
#
# $Id$
#
# TEST	bt_rsnap001
# TEST	Lock-free root-snapshot read descent correctness under concurrency.
# TEST
# TEST	The read fast path (option B, __bam_rsnap_child in bt_search.c) begins
# TEST	a plain point lookup at a child taken from a private, wired snapshot of
# TEST	the tree root, validated only by the live root LSN (a seqlock).  This
# TEST	test stresses that invariant: several reader processes point-get known
# TEST	resident keys -- each value is a fixed function of its key -- while a
# TEST	churner process repeatedly splits and merges the root by inserting and
# TEST	deleting a large high-key range.  Every read of a resident key MUST
# TEST	return the correct value; a stale/reused child from the optimistic path
# TEST	would surface as a wrong value, a missing key, or a crash.  The database
# TEST	must verify clean afterward.
proc bt_rsnap001 { { readers 5 } { nkeys 2000 } { iters 4000 } } {
	source ./include.tcl

	puts "Bt_rsnap001: lock-free root-snapshot descent correctness"

	env_cleanup $testdir

	proc valfor { i } { return "v[format %08d $i]payload" }

	# Small pages so nkeys resident keys build a multi-level tree with a
	# P_IBTREE root -- that is what makes the optimistic descent fire.
	set e [berkdb_env -create -home $testdir -txn -lock -log \
	    -lock_detect default \
	    -lock_max_locks 200000 -lock_max_objects 200000 \
	    -lock_max_lockers 200000]
	error_check_good env_open [is_valid_env $e] TRUE
	set db [berkdb open -create -auto_commit -env $e -btree \
	    -pagesize 512 bt.db]
	error_check_good db_open [is_valid_db $db] TRUE
	for { set i 0 } { $i < $nkeys } { incr i } {
		error_check_good seed_$i \
		    [$db put k[format %08d $i] [valfor $i]] 0
	}
	error_check_good db_close [$db close] 0
	error_check_good env_close [$e close] 0

	puts "\tBt_rsnap001.a: spawn $readers readers + 1 churner"
	set pidlist {}
	set p [exec $tclsh_path $test_path/wrap.tcl \
	    btrsnapscript.tcl $testdir/bt_rsnap001.churn.out \
	    churner $testdir $nkeys [expr $iters / 40] &]
	lappend pidlist $p
	for { set i 0 } { $i < $readers } { incr i } {
		set p [exec $tclsh_path $test_path/wrap.tcl \
		    btrsnapscript.tcl $testdir/bt_rsnap001.$i.out \
		    reader $testdir $nkeys $iters &]
		lappend pidlist $p
	}

	puts "\tBt_rsnap001.b: [expr $readers + 1] procs running"
	# Generous cap: workers must finish on their own.  If watch_procs ever
	# SIGKILLs a worker mid-transaction, the shared txn environment is left
	# crashed and the post-run verify would report false corruption (WAL not
	# yet applied) until recovery runs -- so never let a worker be killed.
	watch_procs $pidlist 5 3600

	# Any wrong value / missing key / crash is a FAIL string in a .out file.
	set errstrings [eval findfail \
	    [glob -nocomplain $testdir/bt_rsnap001.*.out]]
	foreach str $errstrings {
		error_check_good "clean worker exit ($str)" 0 1
	}
	foreach f [glob -nocomplain $testdir/bt_rsnap001.*.out] {
		fileremove -f $f
	}

	puts "\tBt_rsnap001.c: database verifies clean after the run"
	# Open with -recover first: if any worker died mid-transaction the
	# on-disk tree is WAL-consistent but not yet applied; recovery makes it
	# whole so verify sees the real (clean) state, not a crash artifact.
	set e [berkdb_env -create -recover -home $testdir -txn -lock -log]
	error_check_good verify [verify_dir $testdir "" 0 0 1] 0
	error_check_good reopen_close [$e close] 0
}
