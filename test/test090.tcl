# See the file LICENSE for redistribution information.
#
# Copyright (c) 2000-2001
#	Sleepycat Software.  All rights reserved.
#
# $Id: test090.tcl,v 11.6 2001/08/03 16:39:48 bostic Exp $
#
# TEST	test090
# TEST	Test for functionality near the end of the queue.
proc test090 { method {nentries 1000} {txn -txn} {tnum "90"} args} {
	if { [is_queueext $method ] == 0 } {
		puts "Skipping test0$tnum for $method."
		return;
	}
	eval {test001 $method $nentries 4294967000 $tnum} $args
	eval {test025 $method $nentries 4294967000 $tnum} $args
	eval {test070 $method 4 2 $nentries WAIT 4294967000 $txn $tnum} $args
}
