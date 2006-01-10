# See the file LICENSE for redistribution information.
#
# Copyright (c) 2004-2005
#	Sleepycat Software.  All rights reserved.
#
# $Id: rep038.tcl,v 12.6 2005/10/18 19:04:17 carol Exp $
#
# TEST	rep038
# TEST	Test of internal initialization and ongoing master updates.
# TEST
# TEST	One master, one client.
# TEST	Generate several log files.
# TEST	Remove old master log files.
# TEST	Delete client files and restart client.
# TEST	Put more records on master while initialization is in progress.
#
proc rep038 { method { niter 200 } { tnum "038" } args } {

	source ./include.tcl
	if { $is_windows9x_test == 1 } { 
		puts "Skipping replication test on Win 9x platform."
		return
	} 
	set args [convert_args $method $args]

	# This test needs to set its own pagesize.
	set pgindex [lsearch -exact $args "-pagesize"]
        if { $pgindex != -1 } {
                puts "Rep$tnum: skipping for specific pagesizes"
                return
        }

	# Run the body of the test with and without recovery,
	# and with and without cleaning.
	set recopts { "" " -recover " }
	set cleanopts { clean noclean }
	foreach r $recopts {
		foreach c $cleanopts {
			puts "Rep$tnum ($method $c $r $args):\
			    Test of internal initialization with new records."
			rep038_sub $method $niter $tnum $r $c $args
		}
	}
}

proc rep038_sub { method niter tnum recargs clean largs } {
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
	repladd 1
	set ma_envcmd "berkdb_env_noerr -create -txn nosync \
	    -log_buffer $log_buf -log_max $log_max \
	    -home $masterdir -rep_transport \[list 1 replsend\]"
#	set ma_envcmd "berkdb_env_noerr -create -txn nosync \
#	    -log_buffer $log_buf -log_max $log_max \
#	    -verbose {rep on} -errpfx MASTER \
#	    -home $masterdir -rep_transport \[list 1 replsend\]"
	set masterenv [eval $ma_envcmd $recargs -rep_master]
	error_check_good master_env [is_valid_env $masterenv] TRUE

	# Open a client
	repladd 2
	set cl_envcmd "berkdb_env_noerr -create -txn nosync \
	    -log_buffer $log_buf -log_max $log_max \
	    -home $clientdir -rep_transport \[list 2 replsend\]"
#	set cl_envcmd "berkdb_env_noerr -create -txn nosync \
#	    -log_buffer $log_buf -log_max $log_max \
#	    -verbose {rep on} -errpfx CLIENT \
#	    -home $clientdir -rep_transport \[list 2 replsend\]"
	set clientenv [eval $cl_envcmd $recargs -rep_client]
	error_check_good client_env [is_valid_env $clientenv] TRUE

	# Bring the clients online by processing the startup messages.
	set envlist "{$masterenv 1} {$clientenv 2}"
	process_msgs $envlist

	# Run rep_test in the master (and update client).
	puts "\tRep$tnum.a: Running rep_test in replicated env."
	eval rep_test $method $masterenv NULL $niter 0 0 0 0 $largs
	process_msgs $envlist

	puts "\tRep$tnum.b: Close client."
	error_check_good client_close [$clientenv close] 0

	# Find out what exists on the client.  We need to loop until
	# the first master log file > last client log file.
	set res [eval exec $util_path/db_archive -l -h $clientdir]

	set last_client_log [lindex [lsort $res] end]

	set stop 0
	while { $stop == 0 } {
		# Run rep_test in the master (don't update client).
		puts "\tRep$tnum.c: Running rep_test in replicated env."
		eval rep_test $method $masterenv NULL $niter 0 0 0 0 $largs
		replclear 2

		puts "\tRep$tnum.d: Run db_archive on master."
		set res [eval exec $util_path/db_archive -d -h $masterdir]
		set res [eval exec $util_path/db_archive -l -h $masterdir]
		if { [lsearch -exact $res $last_client_log] == -1 } {
			set stop 1
		}
	}

	puts "\tRep$tnum.e: Reopen client ($clean)."
	if { $clean == "clean" } {
		env_cleanup $clientdir
	}
	set clientenv [eval $cl_envcmd $recargs -rep_client]
	error_check_good client_env [is_valid_env $clientenv] TRUE
	set envlist "{$masterenv 1} {$clientenv 2}"
	#
	# We want to simulate a master continually getting new
	# records while an update is going on.  Simulate that
	# for several iterations and then let the messages finish
	# all their processing.
	#
	set loop 10
	set i 0
	set entries 10
	set start $niter
	while { $i < $loop } {
		set nproced 0
		set start [expr $start + $entries]
		eval rep_test $method $masterenv NULL $entries $start 0 0 0 $largs
		incr nproced [proc_msgs_once $envlist NONE err]
		error_check_bad nproced $nproced 0
		incr i
	}
	process_msgs $envlist

	puts "\tRep$tnum.f: Verify logs and databases"
	rep_verify $masterdir $masterenv $clientdir $clientenv 1

	# Add records to the master and update client.
	puts "\tRep$tnum.g: Add more records and check again."
	set entries 10
	eval rep_test $method $masterenv NULL $entries $niter 0 0 0 $largs
	process_msgs $envlist 0 NONE err

	# Check again that master and client logs and dbs are identical.
	rep_verify $masterdir $masterenv $clientdir $clientenv 1

	error_check_good masterenv_close [$masterenv close] 0
	error_check_good clientenv_close [$clientenv close] 0
	replclose $testdir/MSGQUEUEDIR
}
