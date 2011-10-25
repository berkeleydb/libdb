# See the file LICENSE for redistribution information.
#
# Copyright (c) 2007, 2011 Oracle and/or its affiliates.  All rights reserved.
#
# $Id$
#
# TEST	repmgr008
# TEST	Basic repmgr test with client-to-client configuration.
# TEST
# TEST	Create a master and two clients, process some records and verify 
# TEST	resulting databases.
# TEST
# TEST	Run for btree only because access method shouldn't matter.
# TEST
proc repmgr008 { { niter 100 } { tnum "008" } args } {

	source ./include.tcl

	if { $is_freebsd_test == 1 } {
		puts "Skipping replication manager test on FreeBSD platform."
		return
	}

	set method "btree"
	set args [convert_args $method $args]

	puts "Repmgr$tnum ($method): Basic repmgr test client-to-client."
	basic_repmgr_test $method $niter $tnum 0 0 1 0 0 0 $args
}

