# See the file LICENSE for redistribution information.
#
# Copyright (c) 2000-2001
#	Sleepycat Software.  All rights reserved.
#
# $Id: test082.tcl,v 11.4 2001/08/03 16:39:47 bostic Exp $
#
# TEST	test082
# TEST	Test of DB_PREV_NODUP (uses test074).
proc test082 { method {dir -prevnodup} {nitems 100} {tnum 82} args} {
	source ./include.tcl

	eval {test074 $method $dir $nitems $tnum} $args
}
