# See the file LICENSE for redistribution information.
#
# Copyright (c) 1998
#	Sleepycat Software.  All rights reserved.
#
#	@(#)test040.tcl	10.1 (Sleepycat) 10/3/98
#
# DB Test 40 {access method}
# DB_GET_BOTH functionality with off-page duplicates.
proc test040 { method {nentries 10000} args} {
	# Test with off-page duplicates
	test038 $method $nentries 20 40 -psize 512 $args

	# Test with multiple pages of off-page duplicates
	test038 $method [expr $nentries / 10] 100 40 -psize 512 $args
}
