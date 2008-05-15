# See the file LICENSE for redistribution information.
#
# Copyright (c) 2005,2008 Oracle.  All rights reserved.
#
# $Id: rep057.tcl,v 1.9 2008/01/08 20:58:53 bostic Exp $
#
# TEST  rep057
# TEST	Replication test of internal initialization with
# TEST	in-memory named databases.
# TEST
# TEST	Rep057 is just a driver to run rep029 with in-memory
# TEST	named databases.

proc rep057 { method args } {
	source ./include.tcl

	# Valid for all access methods.
	if { $checking_valid_methods } {
		return "ALL"
	}

	eval { rep029 $method 1000 "057" } $args
}
