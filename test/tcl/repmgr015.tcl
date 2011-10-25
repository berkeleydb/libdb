# See the file LICENSE for redistribution information.
#
# Copyright (c) 2007, 2011 Oracle and/or its affiliates.  All rights reserved.
#
# $Id$
#
# TEST	repmgr015
# TEST	Basic repmgr in-memory election test. 
# TEST
# TEST	Open three clients of different priorities and make sure repmgr 
# TEST	elects expected master. Shut master down, make sure repmgr elects 
# TEST	expected remaining client master, make sure former master can join 
# TEST	as client. Replication files are in-memory; databases, logs and
# TEST	environment regions are on-disk.
# TEST
# TEST	Run for btree only because access method shouldn't matter.
# TEST
proc repmgr015 { { niter 100 } { tnum "015" } args } {

	source ./include.tcl

	if { $is_freebsd_test == 1 } {
		puts "Skipping replication manager test on FreeBSD platform."
		return
	}

	set method "btree"
	set args [convert_args $method $args]

	puts -nonewline "Repmgr$tnum ($method): Basic repmgr election test "
	puts "with rep files in-memory."
	basic_repmgr_election_test $method $niter $tnum 1 0 $args
}
