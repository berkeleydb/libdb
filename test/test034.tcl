# See the file LICENSE for redistribution information.
#
# Copyright (c) 1998-2001
#	Sleepycat Software.  All rights reserved.
#
# $Id: test034.tcl,v 11.7 2001/08/13 19:11:43 bostic Exp $
#
# TEST	test034
# TEST	test032 with off-page duplicates
# TEST	DB_GET_BOTH, DB_GET_BOTH_RANGE functionality with off-page duplicates.
proc test034 { method {nentries 10000} args} {
	# Test with off-page duplicates
	eval {test032 $method $nentries 20 34 -pagesize 512} $args

	# Test with multiple pages of off-page duplicates
	eval {test032 $method [expr $nentries / 10] 100 34 -pagesize 512} $args
}
