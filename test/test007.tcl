# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996, 1997, 1998, 1999
#	Sleepycat Software.  All rights reserved.
#
#	@(#)test007.tcl	11.1 (Sleepycat) 7/25/99
#
# DB Test 7 {access method}
# Check that delete operations work.  Create a database; close database and
# reopen it.  Then issues delete by key for each entry.
proc test007 { method {nentries 10000} args} {
   	eval {test006 $method $nentries 7} $args
}
