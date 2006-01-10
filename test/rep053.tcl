# See the file LICENSE for redistribution information.
#
# Copyright (c) 2001-2005
#	Sleepycat Software.  All rights reserved.
#
# $Id: rep053.tcl,v 12.6 2005/10/18 19:05:54 carol Exp $
#
# TEST	rep053
# TEST	Replication and basic client-to-client synchronization.
# TEST
# TEST	Open and start up master and 1 client.
# TEST	Start up a second client later and verify it sync'ed from
# TEST	the original client, not the master.
#
proc rep053 { method { niter 200 } { tnum "053" } args } {
	source ./include.tcl

	if { $is_windows9x_test == 1 } { 
		puts "Skipping replication test on Win 9x platform."
		return
	} 
	set args [convert_args $method $args]

	# Run the body of the test with and without recovery.
	set recopts { "" "-recover" }
	set throttle { "throttle" "" } 
	foreach r $recopts {
		foreach t $throttle {
			puts "Rep$tnum ($method $r $t):\
			    Replication and basic client-to-client syncup."
			rep053_sub $method $niter $tnum $r $t $args
		}
	}
}

proc rep053_sub { method niter tnum recargs throttle largs } {
	global anywhere
	global testdir
	global util_path
	env_cleanup $testdir
	set orig_tdir $testdir

	replsetup $testdir/MSGQUEUEDIR

	set masterdir $testdir/MASTERDIR
	set clientdir $testdir/CLIENTDIR
	set delaycldir1 $testdir/DELAYCLDIR.1
	file mkdir $masterdir
	file mkdir $clientdir
	file mkdir $delaycldir1

	# Open a master.
	repladd 1
	set ma_envcmd "berkdb_env_noerr -create -txn nosync \
	    -lock_max 2500 -errpfx MASTER \
	    -home $masterdir -rep_transport \[list 1 replsend\]"
#	set ma_envcmd "berkdb_env_noerr -create -txn nosync \
#	    -lock_max 2500 \
#	    -errpfx MASTER -verbose {rep on} -errfile /dev/stderr \
#	    -home $masterdir -rep_transport \[list 1 replsend\]"
	set masterenv [eval $ma_envcmd $recargs -rep_master]
	error_check_good master_env [is_valid_env $masterenv] TRUE

	# Open two clients
	repladd 2
	set cl_envcmd "berkdb_env_noerr -create -txn nosync \
	    -lock_max 2500 -errpfx CLIENT \
	    -home $clientdir -rep_transport \[list 2 replsend\]"
#	set cl_envcmd "berkdb_env_noerr -create -txn nosync \
#	    -lock_max 2500 \
#	    -errpfx CLIENT -verbose {rep on} -errfile /dev/stderr \
#	    -home $clientdir -rep_transport \[list 2 replsend\]"
	set clientenv [eval $cl_envcmd $recargs -rep_client]
	error_check_good client_env [is_valid_env $clientenv] TRUE

	# If throttling is specified, turn it on here.  Throttle the
	# client, since this is a test of client-to-client sync up. 
	if { $throttle == "throttle" } {
		error_check_good \
		    throttle [$clientenv rep_limit 0 [expr 32 * 1024]] 0
	}

	#
	# Set up delayed client command, but don't eval until later.
	# !!! Do NOT put the 'repladd' call here because we don't
	# want this client to already have the backlog of records
	# when it starts.
	#
	set dc1_envcmd "berkdb_env_noerr -create -txn nosync \
	    -lock_max 2500 -errpfx DELAYCL \
	    -home $delaycldir1 -rep_transport \[list 3 replsend\]"
#	set dc1_envcmd "berkdb_env_noerr -create -txn nosync \
#	    -lock_max 2500 \
#	    -errpfx DELAYCL -verbose {rep on} -errfile /dev/stderr \
#	    -home $delaycldir1 -rep_transport \[list 3 replsend\]"

	# Bring the client online by processing the startup messages.
	set envlist "{$masterenv 1} {$clientenv 2}"
	process_msgs $envlist

	puts "\tRep$tnum.a: Run rep_test in master env."
	set start 0
	eval rep_test $method $masterenv NULL $niter $start $start 0 0 $largs
	process_msgs $envlist

	puts "\tRep$tnum.b: Start new client."
	set anywhere 1
	repladd 3
	set newclient [eval $dc1_envcmd $recargs -rep_client]
	error_check_good client2_env [is_valid_env $newclient] TRUE

	set envlist "{$masterenv 1} {$clientenv 2} {$newclient 3}"
	process_msgs $envlist

	puts "\tRep$tnum.c: Verify sync-up from client."
	set req [stat_field $clientenv rep_stat "Client service requests"]
	set miss [stat_field $clientenv rep_stat "Client service req misses"]
	set rereq [stat_field $newclient rep_stat "Client rerequests"]
	#
	# The original client should have received at least one request for
	# service from the new client.  Since this is a fully operational
	# client, there should be no misses and more than one request only
	# if we are throttling. 
	#
	if { $throttle == "throttle" } {
		error_check_good req [expr $req > 1] 1
	} else {
		error_check_good req $req 1
	}
	error_check_good miss $miss 0
	error_check_good rereq $rereq 0

	# Check for throttling.
	if { $throttle == "throttle" } {
		set num_throttles \
		    [stat_field $clientenv rep_stat "Transmission limited"]
		error_check_bad client_throttling $num_throttles 0
	}

	rep_verify $masterdir $masterenv $clientdir $clientenv
	# Process messages again in case we are running with debug_rop.
	process_msgs $envlist
	rep_verify $masterdir $masterenv $delaycldir1 $newclient

	puts "\tRep$tnum.d: Run rep_test more in master env and verify."
	set start $niter
	set niter 10
	eval rep_test $method $masterenv NULL $niter $start $start 0 0 $largs
	process_msgs $envlist
	rep_verify $masterdir $masterenv $clientdir $clientenv
	process_msgs $envlist
	rep_verify $masterdir $masterenv $delaycldir1 $newclient

	puts "\tRep$tnum.e: Closing"
	error_check_good master_close [$masterenv close] 0
	error_check_good client_close [$clientenv close] 0
	error_check_good dc1_close [$newclient close] 0
	replclose $testdir/MSGQUEUEDIR
	set testdir $orig_tdir
	set anywhere 0
	return
}
