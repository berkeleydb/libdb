# See the file LICENSE for redistribution information.
#
# Copyright (c) 2005
#	Sleepycat Software.  All rights reserved.
#
# $Id: rep056.tcl,v 1.1 2005/10/19 01:06:51 carol Exp $
#
# TEST  rep056
# TEST	Replication test with in-memory named databases.
# TEST
# TEST	Rep056 is just a driver to run rep001 with in-memory
# TEST	named databases.

proc rep056 { method args } {
	eval { rep001 $method 1000 "056" } $args
} 
