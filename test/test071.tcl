# See the file LICENSE for redistribution information.
#
# Copyright (c) 1999
#	Sleepycat Software.  All rights reserved.
#
#	@(#)test071.tcl	11.1 (Sleepycat) 9/30/99
#
# DB Test 71: Test of DB_CONSUME.
# 	This is DB Test 70, with one consumer, one producers, and 10000 items.
proc test071 { method {nconsumers 1} {nproducers 1}\
    {nitems 10000} {tnum 71} args } {

	eval test070 $method $nconsumers $nproducers $nitems $tnum $args
}
