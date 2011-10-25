# See the file LICENSE for redistribution information.
#
# Copyright (c) 2007, 2011 Oracle and/or its affiliates.  All rights reserved.
#
# $Id$
#
# TEST	repmgr021
# TEST	Basic repmgr election test with region files in memory. 
# TEST  This is identical to repmgr002 except the env is opened with
# TEST	"-private".
# TEST
# TEST	Open three clients of different priorities and make sure repmgr 
# TEST	elects expected master. Shut master down, make sure repmgr elects 
# TEST	expected remaining client master, make sure former master can join 
# TEST	as client.
# TEST
# TEST	Run for btree only because access method shouldn't matter.
# TEST
proc repmgr021 { { niter 100 } { tnum "021" } args } {

	source ./include.tcl

	if { $is_freebsd_test == 1 } {
		puts "Skipping replication manager test on FreeBSD platform."
		return
	}

	set method "btree"
	set args [convert_args $method $args]

	puts "Repmgr$tnum ($method):\
	    Basic repmgr election test with region files in memory."
	basic_repmgr_election_test $method $niter $tnum 0 1 $args
}
