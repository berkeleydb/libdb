# See the file LICENSE for redistribution information.
#
# Copyright (c) 2000-2001
#	Sleepycat Software.  All rights reserved.
#
# $Id: test080.tcl,v 11.12 2001/08/03 16:39:46 bostic Exp $
#
# TEST	test080
# TEST	Test of DB->remove()
proc test080 { method {tnum 80} args } {
	source ./include.tcl

	set args [convert_args $method $args]
	set omethod [convert_method $method]

	puts "Test0$tnum: Test of DB->remove()"

	set eindex [lsearch -exact $args "-env"]
	if { $eindex != -1 } {
		puts "\tTest0$tnum: Skipping in the presence of an environment"
		return
	}
	cleanup $testdir NULL

	# Test relative pathnames
	set testfile $testdir/test0$tnum.db
	puts "\tTesting with relative pathnames."
	set db [eval {berkdb_open -create -mode 0644} $omethod \
		$args {$testfile}]
	error_check_good db_open [is_valid_db $db] TRUE
	for {set i 1} { $i < 1000 } {incr i} {
		$db put $i $i
	}
	error_check_good db_close [$db close] 0
	error_check_good file_exists_before [file exists $testfile] 1
	error_check_good db_remove [berkdb dbremove $testfile] 0
	error_check_good file_exists_after [file exists $testfile] 0

	cleanup $testdir NULL

	# Set up absolute pathname
	set curdir [pwd]
	cd $testdir
	set fulldir [pwd]
	cd $curdir

	# Test absolute pathnames
	set testfile $fulldir/test0$tnum.db
	puts "\tTesting with absolute pathnames."
	set db [eval {berkdb_open -create -mode 0644} $omethod \
		$args {$testfile}]
	error_check_good db_open [is_valid_db $db] TRUE
	for {set i 1} { $i < 1000 } {incr i} {
		$db put $i $i
	}
	error_check_good db_close [$db close] 0
	error_check_good file_exists_before [file exists $testfile] 1
	error_check_good db_remove [berkdb dbremove $testfile] 0
	error_check_good file_exists_after [file exists $testfile] 0

	puts "\tTest0$tnum succeeded."
}
