# See the file LICENSE for redistribution information.
#
# Copyright (c) 1999
#	Sleepycat Software.  All rights reserved.
#
#	@(#)test070.tcl	11.2 (Sleepycat) 9/30/99
#
# DB Test 70: Test of DB_CONSUME.
# Fork off six processes, four consumers and two producers.
# The producers will each put 20000 records into a queue;
# the consumers will each get 10000.
# Then, verify that no record was lost or retrieved twice.
proc test070 { method {nconsumers 4} {nproducers 2}\
    {nitems 1000} {tnum 70} args } {
	source ./include.tcl
	global alphabet

	set omethod [convert_method $method]
	set args [convert_args $method $args]

	puts "Test0$tnum: $method ($args) Test\
		of DB_CONSUME flag to DBcursor->c_get."

	error_check_good enough_consumers [expr $nconsumers > 0] 1
	error_check_good enough_producers [expr $nproducers > 0] 1

	if { [is_queue $method] != 1 } {
		puts "\tSkipping Test0$tnum for method $method."
		return
	}

	cleanup $testdir
	set testfile test0$tnum.db

	# Create environment
	set dbenv [eval {berkdb env -create -mpool -lock -home} $testdir]
	error_check_good dbenv_create [is_valid_env $dbenv] TRUE

	# Create database
	# set db [eval {berkdb open -create -truncate -mode 0644 -queue}\
	#	$args $testfile]
	# error_check_good db_open [is_valid_db $db] TRUE

	set pidlist {}

	# Divvy up the total number of records amongst the consumers and
	# producers.
	error_check_good cons_div_evenly [expr $nitems % $nconsumers] 0
	error_check_good prod_div_evenly [expr $nitems % $nproducers] 0
	set nperconsumer [expr $nitems / $nconsumers]
	set nperproducer [expr $nitems / $nproducers]

	set consumerlog $testdir/CONSUMERLOG.

	# Fork consumer processes (we want them to be hungry)
	for { set ndx 0 } { $ndx < $nconsumers } { incr ndx } {
		set output $consumerlog$ndx
		set p [exec $tclsh_path $test_path/conscript.tcl \
		    $testdir $testfile CONSUME $nperconsumer $output $tnum \
		    $args &]
		lappend pidlist $p
	}
	for { set ndx 0 } { $ndx < $nproducers } { incr ndx } {
		set p [exec $tclsh_path $test_path/conscript.tcl \
		    $testdir $testfile PRODUCE $nperproducer "" $tnum \
		    $args &]
		lappend pidlist $p
	}

	# Wait for all children.
	watch_procs $pidlist 10

	# Verify: slurp all record numbers into list, sort, and make
	# sure each appears exactly once.
	puts "\tTest0$tnum: Verifying results."
	set reclist {}
	for { set ndx 0 } { $ndx < $nconsumers } { incr ndx } {
		set input $consumerlog$ndx
		set iid [open $input r]
		while { [gets $iid str] != -1 } {
			lappend reclist $str
		}
		close $iid
	}
	set sortreclist [lsort -integer $reclist]

	for { set ndx 1 } { $ndx <= $nitems } { incr ndx } {
		error_check_good pop_num [lindex $sortreclist 0] $ndx
		set sortreclist [lreplace $sortreclist 0 0]
	}
	error_check_good list_ends_empty $sortreclist {}
	error_check_good dbenv_close [$dbenv close] 0

	puts "\tTest0$tnum completed successfully."
}
