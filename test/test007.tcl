# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996-2001
#	Sleepycat Software.  All rights reserved.
#
# $Id: test007.tcl,v 11.6 2001/01/25 18:23:08 bostic Exp $
#
# DB Test 7 {access method}
# Check that delete operations work.  Create a database; close database and
# reopen it.  Then issues delete by key for each entry.
proc test007 { method {nentries 10000} {tnum 7} args} {
	eval {test006 $method $nentries 1 $tnum} $args
}
