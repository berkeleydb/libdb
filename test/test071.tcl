# See the file LICENSE for redistribution information.
#
# Copyright (c) 1999, 2000
#	Sleepycat Software.  All rights reserved.
#
#	$Id: test071.tcl,v 11.4 2000/05/22 12:51:40 bostic Exp $
#
# DB Test 71: Test of DB_CONSUME.
#	This is DB Test 70, with one consumer, one producers, and 10000 items.
proc test071 { method {nconsumers 1} {nproducers 1}\
    {nitems 10000} {tnum 71} args } {

	eval test070 $method $nconsumers $nproducers $nitems $tnum $args
}
