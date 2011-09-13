# See the file LICENSE for redistribution information.
#
# Copyright (c) 2010, 2011 Oracle and/or its affiliates.  All rights reserved.
#
# $Id$
#
# TEST	test128
# TEST	Test database bulk update for sub database and duplicate database.
# TEST
# TEST	This is essentially test126 with sub database and secondary database.

proc test128 {method { nentries 10000 } {callback 1} args } {
	source ./include.tcl

	if  { [is_partition_callback $args] == 1 } {
		set nodump 1
	} else {
		set nodump 0
	}	
	# Test using sub database
	eval {test126 $method $nentries "128" $callback 1 0} $args
	eval {verify_dir $testdir "" 1 0 $nodump}
	eval {salvage_dir $testdir "" 1}

	# Test using secondary database
	eval {test126 $method $nentries "128" $callback 0 1} $args
	eval {verify_dir $testdir "" 1 0 $nodump}
	eval {salvage_dir $testdir "" 1}

	# Test using both sub database and secondary database
	eval {test126 $method $nentries "128" $callback 1 1} $args

}


