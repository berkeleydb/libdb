# See the file LICENSE for redistribution information.
#
# Copyright (c) 1999-2001
#	Sleepycat Software.  All rights reserved.
#
# $Id: test081.tcl,v 11.5 2001/08/03 16:39:46 bostic Exp $
#
# TEST	test081
# TEST	Test off-page duplicates and overflow pages together with
# TEST	very large keys (key/data as file contents).
proc test081 { method {ndups 13} {tnum 81} args} {
	source ./include.tcl

	eval {test017 $method 1 $ndups $tnum} $args
}
