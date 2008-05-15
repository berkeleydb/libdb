# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996,2008 Oracle.  All rights reserved.
#
# $Id: test009.tcl,v 12.6 2008/01/08 20:58:53 bostic Exp $
#
# TEST	test009
# TEST	Small keys/large data
# TEST		Same as test008; close and reopen database
# TEST
# TEST	Check that we reuse overflow pages.  Create database with lots of
# TEST	big key/data pairs.  Go through and delete and add keys back
# TEST	randomly.  Then close the DB and make sure that we have everything
# TEST	we think we should.
proc test009 { method args} {
	eval {test008 $method "009" 0} $args
}
