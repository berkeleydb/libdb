# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996, 1997
#	Sleepycat Software.  All rights reserved.
#
#	@(#)test027.tcl	8.1 (Sleepycat) 9/27/97
#
# DB Test 27 {access method}
# Check that delete operations work.  Create a database; close database and
# reopen it.  Then issues delete by key for each entry.
proc test027 { method {nentries 100} args} {
	test026 $method $nentries 100 27 $args
}
