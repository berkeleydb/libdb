# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996, 1997
#	Sleepycat Software.  All rights reserved.
#
#	@(#)test017.tcl	10.1 (Sleepycat) 4/12/97
#
# DB Test 17 {access method}
# Run duplicates with small page size so that we test off page duplicates.
proc test017 { method {nentries 10000} args} {
	puts "Test017: Off page duplicate tests"
	test010 $method $nentries 19 17 -psize 512 $args
}
