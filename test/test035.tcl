# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996, 1997, 1998
#	Sleepycat Software.  All rights reserved.
#
#	@(#)test035.tcl	8.2 (Sleepycat) 9/12/98
#
# DB Test 35 {access method}
# DB_GET_BOTH functionality with off-page duplicates.
proc test035 { method {nentries 10000} args} {
	# Test with off-page duplicates
	test033 $method $nentries 20 35 -psize 512 $args

	# Test with multiple pages of off-page duplicates
	test033 $method [expr $nentries / 10] 100 35 -psize 512 $args
}
