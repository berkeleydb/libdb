# See the file LICENSE for redistribution information.
#
# Copyright (c) 1999
#	Sleepycat Software.  All rights reserved.
#
#	@(#)test072.tcl	11.1 (Sleepycat) 10/26/99
#
# DB Test 72: Test that of cursor stability when duplicates are moved off-page.
proc test072 { method {pagesize 512} {ndups 20} {tnum 72} args } {
	source ./include.tcl
	global alphabet

	set omethod [convert_method $method]
	set args [convert_args $method $args]

	cleanup $testdir
	set testfile ./TESTDIR/test0$tnum.db

	set key "the key"

	append args " -pagesize $pagesize -dup"

	puts -nonewline "Test0$tnum $omethod ($args): "
	if { [is_record_based $method] || [is_rbtree $method] } {
		puts "Skipping for method $method."
		return
	} else {
		puts "\n    Test of cursor stability when\
		    duplicates are moved off-page."
	}

	set db [eval {berkdb \
	    open -create -truncate -mode 0644} $omethod $args $testfile]
	error_check_good "db open" [is_valid_db $db] TRUE

	puts "\tTest0$tnum.a: Put/create cursor/verify all cursor loop."

	for { set i 0 } { $i < $ndups } { incr i } {
		set datum [format "%4d$alphabet" [expr $i + 1000]]
		set data($i) $datum

		error_check_good "db put ($i)" [$db put $key $datum] 0

		set dbc($i) [$db cursor]
		error_check_good "db cursor ($i)"\
		    [is_valid_cursor $dbc($i) $db] TRUE

		error_check_good "dbc get -get_both ($i)"\
		    [$dbc($i) get -get_both $key $datum]\
		    [list [list $key $datum]]

		for { set j 0 } { $j < $i } { incr j } {
	    	    set dbt [$dbc($j) get -current]
	    	    set k [lindex [lindex $dbt 0] 0]
	    	    set d [lindex [lindex $dbt 0] 1]

		    #puts "cursor $j after $i: $d"

		    eval {$db sync}

	    	    error_check_good\
			"cursor $j key correctness after $i puts" $k $key
		    error_check_good\
			"cursor $j data correctness after $i puts" $d $data($j)
		}
	}

	# Close cursors.
	puts "\tTest0$tnum.b: Closing cursors."
	for { set i 0 } { $i < $ndups } { incr i } {
		error_check_good "dbc close ($i)" [$dbc($i) close] 0
	}
	error_check_good "db close" [$db close] 0
}
