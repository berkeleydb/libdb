# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996-2001
#	Sleepycat Software.  All rights reserved.
#
# $Id: test005.tcl,v 11.5 2001/01/25 18:23:08 bostic Exp $
#
# DB Test 5 {access method}
# Check that cursor operations work.  Create a database; close database and
# reopen it.  Then read through the database sequentially using cursors and
# delete each element.
proc test005 { method {nentries 10000} args } {
	eval {test004 $method $nentries 5 0} $args
}
