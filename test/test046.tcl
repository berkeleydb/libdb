# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996, 1997, 1998
#	Sleepycat Software.  All rights reserved.
#
#	@(#)test046.tcl	8.3 (Sleepycat) 12/11/98
#
proc test046 { method args } {
	source ./include.tcl
	set omethod $method
	set method [convert_method $method]
	if { [is_rbtree $omethod] == 1 } {
		puts "Test046 skipping for method $omethod"
		return
	}
	puts "Test046: $method interface test"
	set testfile test046.db
	cleanup $testdir

	# Create an initial database on which we can test
	set flags \
	    [expr $DB_INIT_MPOOL | $DB_INIT_TXN | $DB_INIT_LOCK | $DB_INIT_LOG]
	set dbenv [dbenv -dbflags $flags -dbhome $testdir $args]
	set db [eval dbopen \
	    $testfile [expr $DB_CREATE | $DB_TRUNCATE] 0644 $method \
	    -dbenv $dbenv -flags $DB_DUP $args]
	error_check_good dbopen [is_valid_db $db] TRUE

	# Put a bunch of records in (including some duplicates) so that
	# we can exercise all the various flags to get/put/delete and
	# the cursor ops.
	error_check_good db_close [$db close] 0
}
