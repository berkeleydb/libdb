# See the file LICENSE for redistribution information.
#
# Copyright (c) 1998, 1999
#	Sleepycat Software.  All rights reserved.
#
#	@(#)test034.tcl	11.2 (Sleepycat) 8/3/99
#
# DB Test 34 {access method}
# DB_GET_BOTH functionality with off-page duplicates.
proc test034 { method {nentries 10000} args} {
	# Test with off-page duplicates
	eval {test032 $method $nentries 20 34 -pagesize 512} $args

	# Test with multiple pages of off-page duplicates
	eval {test032 $method [expr $nentries / 10] 100 34 -pagesize 512} $args
}
