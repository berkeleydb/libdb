# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996-2001
#	Sleepycat Software.  All rights reserved.
#
# $Id: test009.tcl,v 11.6 2001/08/03 16:39:33 bostic Exp $
#
# TEST	test009
# TEST	Small keys/large data
# TEST		Same as test008; close and reopen database
# TEST
# TEST	Check that we reuse overflow pages.  Create database with lots of
# TEST	big key/data pairs.  Go through and delete and add keys back
# TEST	randomly.  Then close the DB and make sure that we have everything
# TEST	we think we should.
proc test009 { method {nentries 10000} args} {
	eval {test008 $method $nentries 9 0} $args
}
