# See the file LICENSE for redistribution information.
#
# Copyright (c) 2007, 2011 Oracle and/or its affiliates.  All rights reserved.
#
# $Id$
#
# TEST	repmgr022
# TEST	Basic repmgr internal init test with region files in-memory.
# TEST	This is the same as repmgr003 except the env is opened with 
# TEST	"-private".
# TEST
# TEST	Start an appointed master site and two clients, processing 
# TEST	transactions between each additional site. Verify all expected 
# TEST	transactions are replicated.
# TEST
# TEST	Run for btree only because access method shouldn't matter.
# TEST
proc repmgr022 { { niter 100 } { tnum "022" } args } {

	source ./include.tcl

	if { $is_freebsd_test == 1 } {
		puts "Skipping replication manager test on FreeBSD platform."
		return
	}

	set method "btree"
	set args [convert_args $method $args]

	puts "Repmgr$tnum ($method):\
	    Basic repmgr internal init test with region files in memory."
	basic_repmgr_init_test $method $niter $tnum 0 1 $args
}
