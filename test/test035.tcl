# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996-2001
#	Sleepycat Software.  All rights reserved.
#
# $Id: test035.tcl,v 11.5 2001/08/03 16:39:39 bostic Exp $
#
# TEST	test035
# TEST	Test033 with off-page duplicates
# TEST	DB_GET_BOTH functionality with off-page duplicates.
proc test035 { method {nentries 10000} args} {
	# Test with off-page duplicates
	eval {test033 $method $nentries 20 35 -pagesize 512} $args

	# Test with multiple pages of off-page duplicates
	eval {test033 $method [expr $nentries / 10] 100 35 -pagesize 512} $args
}
