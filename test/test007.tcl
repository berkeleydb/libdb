# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996-2001
#	Sleepycat Software.  All rights reserved.
#
# $Id: test007.tcl,v 11.7 2001/08/03 16:39:33 bostic Exp $
#
# TEST	test007
# TEST	Small keys/medium data
# TEST		Put/get per key
# TEST		Close, reopen
# TEST		Keyed delete
# TEST
# TEST	Check that delete operations work.  Create a database; close
# TEST	database and reopen it.  Then issues delete by key for each
# TEST	entry.
proc test007 { method {nentries 10000} {tnum 7} args} {
	eval {test006 $method $nentries 1 $tnum} $args
}
