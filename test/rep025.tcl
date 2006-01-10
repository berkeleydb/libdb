# See the file LICENSE for redistribution information.
#
# Copyright (c) 2004
#       Sleepycat Software.  All rights reserved.
#
# $Id: rep025.tcl,v 12.6 2005/10/20 14:30:01 carol Exp $
#
# TEST  rep025
# TEST  Test of DB_REP_JOIN_FAILURE.
# TEST
# TEST  One master, one client.  
# TEST  Generate several log files. 
# TEST  Remove old master log files. 
# TEST  Delete client files and restart client. 
# TEST  Put one more record to the master.  At the next 
# TEST  processing of messages, the client should get JOIN_FAILURE.
# TEST  Recover with a hot failover. 
#
proc rep025 { method { niter 200 } { tnum "025" } args } {
	set args [convert_args $method $args]

	# This test needs to set its own pagesize.
	set pgindex [lsearch -exact $args "-pagesize"]
	if { $pgindex != -1 } {
		puts "Rep$tnum: skipping for specific pagesizes"
		return
	}

	# Run the body of the test with and without recovery. 
	set recopts { "" " -recover " }
	foreach r $recopts {
		set recargs $r
		puts "Rep$tnum ($method $recargs):\
		    Test of manual initialization and join failure."
		rep025_sub $method $niter $tnum $recargs $args
	}
}

proc rep025_sub { method niter tnum recargs largs } {
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

	# Run a modified test001 in the master (and update client).
	puts "\tRep$tnum.a: Running rep_test in replicated env."
	set start 0
	eval rep_test $method $masterenv NULL $niter $start $start 0 0 $largs
	set start [expr $start + $niter]
	process_msgs $envlist

	puts "\tRep$tnum.b: Close client."
	error_check_good client_close [$clientenv close] 0

	# Run master, allowing client to become outdated.
	puts "\tRep$tnum.c: Running rep_test with client closed."
	eval rep_test $method $masterenv NULL $niter $start $start 0 0 $largs
	set start [expr $start + $niter]

	puts "\tRep$tnum.d: Run db_archive on master."
	set res [eval exec $util_path/db_archive -l -h $masterdir]
	error_check_bad log.1.present [lsearch -exact $res log.0000000001] -1        
	set res [eval exec $util_path/db_archive -d -h $masterdir]
	set res [eval exec $util_path/db_archive -l -h $masterdir]
	error_check_good log.1.gone [lsearch -exact $res log.0000000001] -1

	puts "\tRep$tnum.e: Clean client and reopen."
	env_cleanup $clientdir
	set clientenv [eval $cl_envcmd $recargs -rep_client]
	error_check_good client_env [is_valid_env $clientenv] TRUE
	set envlist "{$masterenv 1} {$clientenv 2}"

	# Set initialization to manual.
	$clientenv rep_config {noautoinit on}
	process_msgs $envlist 0 NONE err
	error_check_good error_on_right_env [lindex $err 0] $clientenv
	error_check_good right_error [is_substr $err DB_REP_JOIN_FAILURE] 1

	# Add records to the master and update client. 
	puts "\tRep$tnum.f: Update master; client should return error."
	set entries 100
	eval rep_test $method $masterenv NULL $entries $start $start 0 0 $largs
	set start [expr $start + $entries]
	process_msgs $envlist 0 NONE err
	error_check_good error_on_right_env [lindex $err 0] $clientenv
	error_check_good right_error [is_substr $err DB_REP_JOIN_FAILURE] 1

	puts "\tRep$tnum.g: Hot failover from master to client."
	error_check_good client_close [$clientenv close] 0  
	env_cleanup $clientdir
	set files [glob $masterdir/log.* $masterdir/*.db]
	foreach f $files {
		set filename [file tail $f]
		file copy -force $f $clientdir/$filename
	}

	puts "\tRep$tnum.h: Run catastrophic recovery on client."
	set clientenv [eval $cl_envcmd -recover_fatal -rep_client]
	error_check_good client_env [is_valid_env $clientenv] TRUE
	set envlist "{$masterenv 1} {$clientenv 2}"
	process_msgs $envlist 0 NONE err
	error_check_good no_errors1 $err 0

	# Adding another entry should not flush out an error. 
	eval rep_test $method $masterenv NULL $entries $start $start 0 0 $largs
	process_msgs $envlist 0 NONE err
	error_check_good no_errors2 $err 0

	error_check_good masterenv_close [$masterenv close] 0
	error_check_good clientenv_close [$clientenv close] 0
	replclose $testdir/MSGQUEUEDIR
}
