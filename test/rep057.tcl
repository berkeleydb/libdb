# See the file LICENSE for redistribution information.
#
# Copyright (c) 2005
#	Sleepycat Software.  All rights reserved.
#
# $Id: rep057.tcl,v 1.1 2005/10/19 01:06:51 carol Exp $
#
# TEST  rep057
# TEST	Replication test of internal initialization with 
# TEST	in-memory named databases.
# TEST
# TEST	Rep057 is just a driver to run rep029 with in-memory
# TEST	named databases.

proc rep057 { method args } {
	eval { rep029 $method 1000 "057" } $args
} 
