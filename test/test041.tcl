# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996, 1997, 1998
#	Sleepycat Software.  All rights reserved.
#
#	@(#)test041.tcl	10.1 (Sleepycat) 10/3/98
#
# DB Test 41 {access method}
# DB_GET_BOTH functionality with off-page duplicates.
proc test041 { method {nentries 10000} args} {
	# Test with off-page duplicates
	test039 $method $nentries 20 41 -psize 512 $args

	# Test with multiple pages of off-page duplicates
	test039 $method [expr $nentries / 10] 100 41 -psize 512 $args
}
