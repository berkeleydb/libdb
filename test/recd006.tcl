# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996, 1997, 1998
#	Sleepycat Software.  All rights reserved.
#
#	@(#)recd006.tcl	8.3 (Sleepycat) 1/16/99
#
# Recovery Test 6.
# Test nested transactions.
proc recd006 { method {select 0} } {
global kvals
	set method [convert_method $method]
	if { [string compare $method DB_RECNO] == 0 } {
		puts "Recd006 skipping for method RECNO"
		return
	}
	puts "Recd006: $method nested transactions"

	# Get global declarations since tcl doesn't support
	# any useful equivalent to #defines!
	source ./include.tcl

	# Create the database and environment.
	cleanup $testdir

	set testfile recd006.db
	puts "\tRecd006.a: create database"
	set db [dbopen $testfile [expr $DB_CREATE | $DB_TRUNCATE] 0644 $method]
	error_check_good dbopen [is_valid_db $db] TRUE

	# Make sure that we have enough entries to span a couple of
	# different pages.
	set did [open $dict]
	set count 0
	while { [gets $did str] != -1 && $count < 1000 } {
		if { [string compare $method DB_RECNO] == 0 } {
			set key [expr $count + 1]
		} else {
			set key $str
		}

		set ret [$db put 0 $key $str $DB_NOOVERWRITE]
		error_check_good put $ret 0

		incr count
	}
	close $did

	# Variables used below:
	# p1: a pair of keys that are likely to be on the same page.
	# p2: a pair of keys that are likely to be on the same page,
	# but on a page different than those in p1.
	set dbc [$db cursor 0]
	error_check_good dbc [is_valid_widget $dbc $db.cursor] TRUE

	set ret [$dbc get 0 $DB_FIRST]
	error_check_bad dbc_get:DB_FIRST [string length $ret] 0
	set p1 [lindex $ret 0]
	set kvals($p1) [lindex $ret 1]

	set ret [$dbc get 0 $DB_NEXT]
	error_check_bad dbc_get:DB_NEXT [string length $ret] 0
	lappend p1 [lindex $ret 0]
	set kvals([lindex $ret 0]) [lindex $ret 1]

	set ret [$dbc get 0 $DB_LAST]
	error_check_bad dbc_get:DB_LAST [string length $ret] 0
	set p2 [lindex $ret 0]
	set kvals($p2) [lindex $ret 1]

	set ret [$dbc get 0 $DB_PREV]
	error_check_bad dbc_get:DB_PREV [string length $ret] 0
	lappend p2 [lindex $ret 0]
	set kvals([lindex $ret 0]) [lindex $ret 1]

	error_check_good dbc_close [$dbc close] 0
	error_check_good db_close [$db close] 0

	# Now create the full transaction environment.
	set flags [expr $DB_CREATE | $DB_THREAD | \
	    $DB_INIT_LOG | $DB_INIT_LOCK | $DB_INIT_MPOOL | $DB_INIT_TXN]

	puts "\tRecd006.b: creating environment"
	set env_cmd "dbenv -dbhome $testdir -dbflags $flags"
	set dbenv [eval $env_cmd]
	error_check_bad dbenv $dbenv NULL

	# Close the environment.
	reset_env $dbenv


	# List of recovery tests: {CMD MSG} pairs
	set rlist {
	{ {nesttest DB TMGR TXNID 1 $p1 $p2 commit commit} 
		"Recd006.c: children (commit commit)"}
	{ {nesttest DB TMGR TXNID 0 $p1 $p2 commit commit} 
		"Recd006.d: children (commit commit)"}
	{ {nesttest DB TMGR TXNID 1 $p1 $p2 commit abort}
		"Recd006.e: children (commit abort)"}
	{ {nesttest DB TMGR TXNID 0 $p1 $p2 commit abort}
		"Recd006.f: children (commit abort)"}
	{ {nesttest DB TMGR TXNID 1 $p1 $p2 abort abort}
		"Recd006.g: children (abort abort)"}
	{ {nesttest DB TMGR TXNID 0 $p1 $p2 abort abort}
		"Recd006.h: children (abort abort)"}
	{ {nesttest DB TMGR TXNID 1 $p1 $p2 abort commit}
		"Recd006.i: children (abort commit)"}
	{ {nesttest DB TMGR TXNID 0 $p1 $p2 abort commit}
		"Recd006.j: children (abort commit)"}
	}

	foreach pair $rlist {
		set cmd [my_subst [lindex $pair 0]]
		set msg [lindex $pair 1]
		if { $select != 0 } {
			set tag [lindex $msg 0]
			set tail [expr [string length $tag] - 2]
			set tag [string range $tag $tail $tail]
			if { [lsearch $select $tag] == -1 } {
				continue
			}
		}
		op_recover abort $testdir $env_cmd $testfile $cmd $msg
		op_recover commit $testdir $env_cmd $testfile $cmd $msg
	}

}

# Do the nested transaction test.
# We want to make sure that children inherit properly from their
# parents and that locks are properly handed back to parents
# and that the right thing happens on commit/abort.
# In particular:
#	Write lock on parent, properly acquired by child.
#	Committed operation on child gives lock to parent so that
#		other child can also get the lock.
#	Aborted op by child releases lock so other child can get it.
#	Correct database state if child commits
#	Correct database state if child aborts
proc nesttest { db tmgr parent do p1 p2 child1 child2} {
source ./include.tcl
global kvals
	if { $do == 1 } {
		set func toupper
	} else {
		set func tolower
	}

	# Do an RMW on the parent to get a write lock.
	set p10 [lindex $p1 0]
	set p11 [lindex $p1 1]
	set p20 [lindex $p2 0]
	set p21 [lindex $p2 1]

	set ret [$db get $parent $p10 $DB_RMW]
	set res $ret
	if { [string compare $ret $kvals($p10)] == 0 ||
	    [string compare $ret [string toupper $kvals($p10)]] == 0 } {
		set val 0
	} else {
		set val $ret
	}
	error_check_good get_parent_RMW $val 0

	# OK, do child 1
	set kid1 [$tmgr begin $parent]
	error_check_good kid1 [is_valid_widget $kid1 $tmgr.txn] TRUE

	# Reading write-locked parent object should be OK
	set ret [$db get $kid1 $p10 0]
	error_check_good kid1_get10 $ret $res

	# Now update this child
	set data [string $func $ret]
	set ret [$db put $kid1 $p10 $data 0]
	error_check_good kid1_put10 $ret 0

	# Now start child2
	set kid2 [$tmgr begin $parent]
	error_check_good kid2 [is_valid_widget $kid2 $tmgr.txn] TRUE

	# Getting anything in the p1 set should deadlock, so let's
	# work on the p2 set.
	set data [string $func $kvals($p20)]
	set ret [$db put $kid2 $p20 $data 0]
	error_check_good kid2_put20 $ret 0

	# Now let's do the right thing to kid1
	if { [string compare $child1 "commit"] == 0 } {
		error_check_good kid1_commit [$kid1 commit] 0
	} else {
		error_check_good kid1_abort [$kid1 abort] 0
	}

	# In either case, child2 should now be able to get the
	# lock, either because it is inherited by the parent
	# (commit) or because it was released (abort).
	set data [string $func $kvals($p11)]
	set ret [$db put $kid2 $p11 $data 0]
	error_check_good kid2_put11 $ret 0

	# Now let's do the right thing to kid2
	if { [string compare $child2 "commit"] == 0 } {
		error_check_good kid2_commit [$kid2 commit] 0
	} else {
		error_check_good kid2_abort [$kid2 abort] 0
	}

	# Now, let parent check that the right things happened.
	# First get all four values
	set p10_check [$db get $parent $p10 0]
	set p11_check [$db get $parent $p11 0]
	set p20_check [$db get $parent $p20 0]
	set p21_check [$db get $parent $p21 0]
	if { [string compare $child1 "commit"] == 0 } {
		error_check_good parent_kid1 $p10_check \
		    [string $func $kvals($p10)]
	} else {
		error_check_good parent_kid1 $p10_check $kvals($p10)
	}
	if { [string compare $child2 "commit"] == 0 } {
		error_check_good parent_kid2 $p11_check \
		    [string $func $kvals($p11)]
		error_check_good parent_kid2 $p20_check \
		    [string $func $kvals($p20)]
	} else {
		error_check_good parent_kid2 $p11_check $kvals($p11)
		error_check_good parent_kid2 $p20_check $kvals($p20)
	}

	# Now do a write on the parent for 21 whose lock it should
	# either have or should be available.

	set ret [$db put $parent $p21 [string $func $kvals($p21)] 0]
	error_check_good parent_put21 $ret 0

	return 0
}
