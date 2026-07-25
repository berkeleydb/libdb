# See the file LICENSE for redistribution information.
#
# Copyright (c) 2026 berkeleydb/libdb contributors.  All rights reserved.
#
# $Id$
#
# TEST	ssi009
# TEST	Serializable Snapshot Isolation: concurrent writers do not crash.
# TEST
# TEST	Regression for a family of pre-existing SIREAD marker/locker/detail
# TEST	lifetime bugs (see the SSI hardening work) that crashed the engine
# TEST	under multiple concurrent snapshot-safe writers -- most importantly
# TEST	a lock object being reclaimed while it still had SIREAD markers on its
# TEST	sireaders list, and markers/details freed while still referenced.
# TEST
# TEST	Spawns several processes that hammer a shared multiversion database
# TEST	with snapshot-safe read-then-write transactions on a small hot key set
# TEST	(maximizing SIREAD-marker churn and conflict/deadlock aborts).  The
# TEST	test passes if every process exits cleanly (no crash/panic) and the
# TEST	database verifies afterward.
proc ssi009 { { procs 6 } { nkeys 8 } { iters 3000 } } {
	source ./include.tcl

	puts "Ssi009: SSI concurrent writers must not crash"

	env_cleanup $testdir

	# Create the env and seed the hot set.  Size the lock/mutex regions for
	# many concurrent SIREAD markers across several processes.
	set e [berkdb_env -create -home $testdir \
	    -txn -lock -log -multiversion -lock_detect default \
	    -mutex_set_max 500000 \
	    -lock_max_locks 200000 -lock_max_objects 200000 \
	    -lock_max_lockers 200000]
	error_check_good env_open [is_valid_env $e] TRUE
	set db [berkdb open -create -auto_commit -env $e -btree -multiversion ssi.db]
	error_check_good db_open [is_valid_db $db] TRUE
	for { set i 0 } { $i < $nkeys } { incr i } {
		error_check_good seed_$i [$db put k$i 0] 0
	}
	error_check_good db_close [$db close] 0
	error_check_good env_close [$e close] 0

	puts "\tSsi009.a: spawn $procs concurrent snapshot-safe writers"
	set pidlist {}
	for { set i 0 } { $i < $procs } { incr i } {
		set p [exec $tclsh_path $test_path/wrap.tcl \
		    ssiscript.tcl $testdir/ssi009.$i.out \
		    $testdir $nkeys $iters &]
		lappend pidlist $p
	}

	puts "\tSsi009.b: $procs writers running"
	watch_procs $pidlist 5 300

	# Any crash/panic/assert shows up as an error string in a .out file.
	set errstrings [eval findfail [glob -nocomplain $testdir/ssi009.*.out]]
	foreach str $errstrings {
		error_check_good "clean worker exit ($str)" 0 1
	}
	for { set i 0 } { $i < $procs } { incr i } {
		fileremove -f $testdir/ssi009.$i.out
	}

	puts "\tSsi009.c: database verifies clean after the run"
	set e [berkdb_env -create -home $testdir -txn -lock -log -multiversion]
	error_check_good verify [verify_dir $testdir "" 0 0 1] 0
	error_check_good reopen_close [$e close] 0
}
