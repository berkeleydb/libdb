# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996-2001
#	Sleepycat Software.  All rights reserved.
#
# $Id: test018.tcl,v 11.5 2001/08/03 16:39:36 bostic Exp $
#
# TEST	test018
# TEST	Offpage duplicate test
# TEST	Key_{first,last,before,after} offpage duplicates.
# TEST	Run duplicates with small page size so that we test off page
# TEST	duplicates.
proc test018 { method {nentries 10000} args} {
	puts "Test018: Off page duplicate tests"
	eval {test011 $method $nentries 19 18 -pagesize 512} $args
}
