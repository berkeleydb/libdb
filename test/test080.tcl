# See the file LICENSE for redistribution information.
#
# Copyright (c) 2000
#	Sleepycat Software.  All rights reserved.
#
#	$Id: test080.tcl,v 11.5 2000/05/09 14:36:15 sue Exp $
#
# DB Test 80 {access method}
# Test of dbremove
proc test080 { method {tnum 80} args } {
	source ./include.tcl

	set args [convert_args $method $args]
	set omethod [convert_method $method]

	puts "Test0$tnum: Test of DB->remove()"

	cleanup $testdir

	set eindex [lsearch -exact $args "-env"]
	if { $eindex != -1 } {
		puts "\tTest0$tnum: Skipping in the presence of an environment"
		return
	}

	set testfile $testdir/test0$tnum.db
	set db [eval {berkdb_open -create -truncate -mode 0644} $omethod \
	    $args {$testfile}]
	error_check_good db_open [is_valid_db $db] TRUE
	error_check_good db_close [$db close] 0

	error_check_good file_exists_before [file exists $testfile] 1

	error_check_good db_remove [berkdb dbremove $testfile] 0
	error_check_good file_exists_after [file exists $testfile] 0

	puts "\tTest0$tnum succeeded."
}
