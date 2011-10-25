# See the file LICENSE for redistribution information.
#
# Copyright (c) 2007, 2011 Oracle and/or its affiliates.  All rights reserved.
#
# $Id$
#
# TEST	repmgr005
# TEST	Basic repmgr test with in-memory databases. 
# TEST
# TEST	Create an appointed master and two clients, process some records and 
# TEST	verify resulting databases.
# TEST
# TEST	Run for btree only because access method shouldn't matter.
# TEST
proc repmgr005 { { niter 100 } { tnum "005" } args } {

	source ./include.tcl

	if { $is_freebsd_test == 1 } {
		puts "Skipping replication manager test on FreeBSD platform."
		return
	}

	set method "btree"
	set args [convert_args $method $args]

	puts "Repmgr$tnum ($method): Basic repmgr test in-memory databases."
	basic_repmgr_test $method $niter $tnum 1 0 0 0 0 0 $args
}

