# See the file LICENSE for redistribution information.
#
# Copyright (c) 1999-2001
#	Sleepycat Software.  All rights reserved.
#
# $Id: test069.tcl,v 11.5 2001/01/25 18:23:12 bostic Exp $
#
# DB Test 69: Run DB Test 67 with a small number of dups,
# to ensure that partial puts to DB_CURRENT work correctly in
# the absence of duplicate pages.

proc test069 { method {ndups 50} {tnum 69} args } {
	eval test067 $method $ndups $tnum $args
}
