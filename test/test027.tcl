# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996-2001
#	Sleepycat Software.  All rights reserved.
#
# $Id: test027.tcl,v 11.6 2001/08/03 16:39:38 bostic Exp $
#
# TEST	test027
# TEST	Off-page duplicate test
# TEST		Test026 with parameters to force off-page duplicates.
# TEST
# TEST	Check that delete operations work.  Create a database; close
# TEST	database and reopen it.  Then issues delete by key for each
# TEST	entry.
proc test027 { method {nentries 100} args} {
	eval {test026 $method $nentries 100 27} $args
}
