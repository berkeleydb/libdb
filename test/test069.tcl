# See the file LICENSE for redistribution information.
#
# Copyright (c) 1999
#	Sleepycat Software.  All rights reserved.
#
#	@(#)test069.tcl	11.2 (Sleepycat) 9/21/99
#
# DB Test 69: Run DB Test 67 with a small number of dups,
# to ensure that partial puts to DB_CURRENT work correctly in
# the absence of duplicate pages.

proc test069 { method {ndups 50} {tnum 69} args } {
	eval test067 $method $ndups $tnum $args
}
