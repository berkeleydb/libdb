# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996, 1997
#	Sleepycat Software.  All rights reserved.
#
#	@(#)test029.tcl	10.3 (Sleepycat) 8/24/97
#
proc test029 { method args } {
	source ./include.tcl
	set method [convert_method $method]
	puts "Test029: $method interface test"
	set testfile test029.db
	cleanup $testdir

	# Create an initial database on which we can test
	set flags \
	    [expr $DB_INIT_MPOOL | $DB_INIT_TXN | $DB_INIT_LOCK | $DB_INIT_LOG]
	set dbenv [dbenv -dbflags $flags -dbhome $testdir $args]
	set db [eval dbopen \
	    $testfile [expr $DB_CREATE | $DB_TRUNCATE] 0644 $method \
	    -dbenv $dbenv -flags $DB_DUP $args]

	# Put a bunch of records in (including some duplicates) so that
	# we can exercise all the various flags to get/put/delete and
	# the cursor ops.
	error_check_good db_close [$db close] 0
}
