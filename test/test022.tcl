# See the file LICENSE for redistribution information.
#
# Copyright (c) 1999
#	Sleepycat Software.  All rights reserved.
#
#	@(#)test022.tcl	11.3 (Sleepycat) 9/10/99
#
# Test022: Test of DB->get_byteswapped
proc test022 { method args } {
	source ./include.tcl

	set args [convert_args $method $args]
	set omethod [convert_method $method]

	puts "Test022 ($args) $omethod: DB->getbyteswapped()"

	set testfile1 "$testdir/test022a.db"
	set testfile2 "$testdir/test022b.db"
	cleanup $testdir

	# Create two databases, one in each byte order.
	set db1 [eval {berkdb open -create \
	    -mode 0644} $omethod $args {-lorder 1234} $testfile1]
	error_check_good db1_open [is_valid_db $db1] TRUE

	set db2 [eval {berkdb open -create \
	    -mode 0644} $omethod $args {-lorder 4321} $testfile2]
	error_check_good db2_open [is_valid_db $db2] TRUE

	# Call DB->get_byteswapped on both of them.
	set db1_order [$db1 is_byteswapped]
	set db2_order [$db2 is_byteswapped]

	# Make sure that both answers are either 1 or 0,
	# and that exactly one of them is 1.
	error_check_good is_byteswapped_sensible_1 \
	    [expr ($db1_order == 1 && $db2_order == 0) || \
	          ($db1_order == 0 && $db2_order == 1)] 1

	puts "\tTest022 complete."
}
