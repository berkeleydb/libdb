# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996, 1997, 1998, 1999
#	Sleepycat Software.  All rights reserved.
#
#	@(#)test017.tcl	11.1 (Sleepycat) 7/25/99
#
# DB Test 17 {access method}
# Run duplicates with small page size so that we test off page duplicates.
proc test017 { method {nentries 10000} args} {
	puts "Test017: Off page duplicate tests"
	eval {test010 $method $nentries 19 17 -pagesize 512} $args
}
