# See the file LICENSE for redistribution information.
#
# Copyright (c) 2004-2005
#	Sleepycat Software.  All rights reserved.
#
# $Id: rep041.tcl,v 12.5 2005/10/18 19:05:54 carol Exp $
#
# TEST	rep041
# TEST  Turn replication on and off at run-time.
# TEST
# TEST  Start a master with replication OFF (noop transport function).
# TEST  Run rep_test to advance log files and archive.
# TEST  Start up client; change master to working transport function.
# TEST  Now replication is ON.
# TEST  Do more ops, make sure client is up to date.
# TEST  Close client, turn replication OFF on master, do more ops.
# TEST  Repeat from point A.
#
proc rep041 { method { niter 500 } { tnum "041" } args } {

	source ./include.tcl
	if { $is_windows9x_test == 1 } { 
		puts "Skipping replication test on Win 9x platform."
		return
	} 
	set args [convert_args $method $args]
	set saved_args $args

	# This test needs to set its own pagesize.
	set pgindex [lsearch -exact $args "-pagesize"]
        if { $pgindex != -1 } {
                puts "Rep$tnum: skipping for specific pagesizes"
                return
        }

	# Run the body of the test with and without recovery.
	set recopts { "" " -recover " }
	foreach r $recopts {
		set envargs ""
		set args $saved_args
		puts "Rep$tnum ($method $envargs $r $args):\
		    Turn replication on and off."
		rep041_sub $method $niter $tnum $envargs \
		    $r $args
	}
}

proc rep041_sub { method niter tnum envargs recargs largs } {
	global testdir
	global util_path

	env_cleanup $testdir

	replsetup $testdir/MSGQUEUEDIR

	set masterdir $testdir/MASTERDIR
	set clientdir $testdir/CLIENTDIR

	file mkdir $masterdir
	file mkdir $clientdir

	# Log size is small so we quickly create more than one.
	# The documentation says that the log file must be at least
	# four times the size of the in-memory log buffer.
	set pagesize 4096
	append largs " -pagesize $pagesize "
	set log_buf [expr $pagesize * 2]
	set log_max [expr $log_buf * 4]

	# Open a master.
	puts "\tRep$tnum.a: Open master with replication OFF."
	repladd 1
	set ma_envcmd "berkdb_env_noerr -create -txn nosync \
	    -log_buffer $log_buf -log_max $log_max $envargs \
	    -home $masterdir -rep"
#	set ma_envcmd "berkdb_env_noerr -create -txn nosync \
#	    -log_buffer $log_buf -log_max $log_max $envargs \
#	    -verbose {rep on} -errpfx MASTER \
#	    -home $masterdir -rep"
	set masterenv [eval $ma_envcmd $recargs]
	error_check_good master_env [is_valid_env $masterenv] TRUE

        # Run rep_test in the master to advance log files.
	puts "\tRep$tnum.b: Running rep_test to create some log files."
	set start 0
	eval rep_test $method $masterenv NULL $niter $start $start 0 0 $largs
	set start [expr $start + $niter]	

	# Reset transport function to replnoop, and specify that
	# this env will be master.
	error_check_good \
	    transport_noop [$masterenv rep_transport {1 replnoop}] 0
	error_check_good rep_on [$masterenv rep_start -master] 0

	# Archive.
	puts "\tRep$tnum.c: Run log_archive - some logs should be removed."
	set res [eval exec $util_path/db_archive -l -h $masterdir]
	error_check_bad log.1.present [lsearch -exact $res log.0000000001] -1
	set res [eval exec $util_path/db_archive -d -h $masterdir]
	set res [eval exec $util_path/db_archive -l -h $masterdir]
	error_check_good log.1.gone [lsearch -exact $res log.0000000001] -1

        # Run rep_test some more - this simulates running without clients.
	puts "\tRep$tnum.d: Running rep_test."
	eval rep_test $method $masterenv NULL $niter $start $start 0 0 $largs
	set start [expr $start + $niter] 

	# Open a client
	puts "\tRep$tnum.e: Open client."
	repladd 2
	set cl_envcmd "berkdb_env_noerr -create -txn nosync \
	    -log_buffer $log_buf -log_max $log_max $envargs \
	    -home $clientdir \
	    -rep_transport \[list 2 replsend\]"
#	set cl_envcmd "berkdb_env_noerr -create -txn nosync \
#	    -log_buffer $log_buf -log_max $log_max $envargs \
#	    -verbose {rep on} -errpfx CLIENT \
#	    -home $clientdir \
#	    -rep_transport \[list 2 replsend\]"
	set clientenv [eval $cl_envcmd $recargs -rep_client]
	error_check_good client_env [is_valid_env $clientenv] TRUE

	# Set up envlist for processing messages later.
	set envlist "{$masterenv 1} {$clientenv 2}"

	# Turn replication on and off more than once. 
	set repeats 2
	for { set i 0 } { $i < $repeats } { incr i } { 

		puts "\tRep$tnum.f.$i: Turn replication ON."
		# Reset master transport function to replsend.
		error_check_good transport_on \
		    [$masterenv rep_transport {1 replsend}] 0
		# Have the master announce itself so messages will pass.
		error_check_good rep_on [$masterenv rep_start -master] 0
	
		# Process messages.
		process_msgs $envlist
	
		puts "\tRep$tnum.g.$i: Verify that client is up to date."
		# Check that master and client contents match, to verify
		# that client is up to date. 
		set dbname "test.db"
		if { [is_hash $method] == 0 } {
			set db1 \
			    [berkdb_open -env $masterenv -auto_commit $dbname]
			set db2 \
			    [berkdb_open -env $clientenv -auto_commit $dbname]
	
			error_check_good compare1and2 [db_compare \
			    $db1 $db2 $masterdir/$dbname $clientdir/$dbname] 0
			error_check_good db1_close [$db1 close] 0
			error_check_good db2_close [$db2 close] 0
		}
	
		puts "\tRep$tnum.h.$i: Turn replication OFF on master."
		error_check_good \
		    transport_off [$masterenv rep_transport {1 replnoop}] 0
	
		puts "\tRep$tnum.i.$i: Running rep_test in replicated env."
		eval rep_test \
		    $method $masterenv NULL $niter $start $start 0 0 $largs
		set start [expr $start + $niter]	

		puts "\tRep$tnum.j.$i:\
		    Process messages; none should be available."
		process_msgs $envlist

		# Client and master should NOT match. 
		puts "\tRep$tnum.k.$i: Master and client should NOT match."
		if { [is_hash $method] == 0 } {
			set db1 \
			    [berkdb_open -env $masterenv -auto_commit $dbname]
			set db2 \
			    [berkdb_open -env $clientenv -auto_commit $dbname]
	
			error_check_good db_compare [db_compare $db1 $db2 \
			    $masterdir/$dbname $clientdir/$dbname] 1
			error_check_good db1_close [$db1 close] 0
			error_check_good db2_close [$db2 close] 0
		}

	}
	error_check_good clientenv_close [$clientenv close] 0
	error_check_good masterenv_close [$masterenv close] 0
	replclose $testdir/MSGQUEUEDIR
}
