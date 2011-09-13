# See the file LICENSE for redistribution information.
#
# Copyright (c) 2010, 2011 Oracle and/or its affiliates.  All rights reserved.
#
# $Id$
#
# TEST	test129
# TEST	Test database bulk update for duplicate sub database.
# TEST
# TEST	This is essentially test127 with sub database.

proc test129 {method { nentries 10000 } { ndups 5} args } {
	
	# Test using both sub database and secondary database
	eval {test127 $method $nentries $ndups "129" 1} $args

}


