# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996, 1997, 1998, 1999
#	Sleepycat Software.  All rights reserved.
#
#	@(#)test060.tcl	11.1 (Sleepycat) 8/19/99
#
# Test060: Test of the DB_EXCL flag to DB->open.
#     1) Attempt to open and create a nonexistent database; verify success.
#     2) Attempt to reopen it;  verify failure.
proc test060 { method args } {
	global errorCode
	source ./include.tcl

	set args [convert_args $method $args]
	set omethod [convert_method $method]

	puts "Test060: $method ($args) Test of the DB_EXCL flag to DB->open"

	# Set the database location and make sure the db doesn't exist yet
	set testfile $testdir/test060.db
	cleanup $testdir

	# Create the database and check success
	puts "\tTest060.a: open and close non-existent file with DB_EXCL"
	set db [eval {berkdb \
	    open -create  -excl -mode 0644} $args {$omethod $testfile}]
	error_check_good dbopen:excl [is_valid_db $db] TRUE

	# Close it and check success
	error_check_good db_close [$db close] 0

	# Try to open it again, and make sure the open fails
	puts "\tTest060.b: open it again with DB_EXCL and make sure it fails"
	set errorCode NONE
	error_check_good open:excl:catch [catch { \
	    set db [eval {berkdb \
	    open -create  -excl -mode 0644} $args {$omethod $testfile}]
	    } ret ] 1

	error_check_good dbopen:excl [is_substr $errorCode EEXIST] 1
}
