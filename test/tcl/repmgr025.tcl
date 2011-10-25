# See the file LICENSE for redistribution information.
#
# Copyright (c) 2007, 2011 Oracle and/or its affiliates.  All rights reserved.
#
# $Id$
#
# TEST	repmgr025
# TEST	repmgr rerequest thread test.
# TEST
# TEST	Start an appointed master site and one client.  Generate some
# TEST 	uncommitted updates on the master to lock some database pages.
# TEST	Start a second client that gets stuck in internal init.  Wait
# TEST	long enough to rely on the rerequest thread to request the
# TEST	missing pages, commit the master updates and verify that all
# TEST	data appears on both clients.
# TEST
# TEST	Run for btree only because access method shouldn't matter.
# TEST
proc repmgr025 { { niter 100 } { tnum "025" } args } {

	source ./include.tcl

	if { $is_freebsd_test == 1 } {
		puts "Skipping replication manager test on FreeBSD platform."
		return
	}

	set method "btree"
	set args [convert_args $method $args]

	puts "Repmgr$tnum ($method): repmgr rerequest thread test."
	repmgr025_sub $method $niter $tnum $args
}

proc repmgr025_sub { method niter tnum largs } {
	global testdir
	global rep_verbose
	global verbose_type
	set nsites 3

	set verbargs ""
	if { $rep_verbose == 1 } {
		set verbargs " -verbose {$verbose_type on} "
	}

	env_cleanup $testdir
	set ports [available_ports $nsites]
	set omethod [convert_method $method]

	set masterdir $testdir/MASTERDIR
	set clientdir $testdir/CLIENTDIR
	set clientdir2 $testdir/CLIENTDIR2

	file mkdir $masterdir
	file mkdir $clientdir
	file mkdir $clientdir2

	# Use different connection retry timeout values to handle any
	# collisions from starting sites at the same time by retrying
	# at different times.

	# Open a master.
	puts "\tRepmgr$tnum.a: Start a master."
	set ma_envcmd "berkdb_env_noerr -create $verbargs \
	    -errpfx MASTER -home $masterdir -txn -rep -thread"
	set masterenv [eval $ma_envcmd]
	$masterenv repmgr -ack all -nsites $nsites \
	    -timeout {connection_retry 20000000} \
	    -local [list localhost [lindex $ports 0]] \
	    -start master

	# Open first client
	puts "\tRepmgr$tnum.b: Start first client."
	set cl_envcmd "berkdb_env_noerr -create $verbargs \
	    -errpfx CLIENT -home $clientdir -txn -rep -thread"
	set clientenv [eval $cl_envcmd]
	$clientenv repmgr -ack all -nsites $nsites \
	    -timeout {connection_retry 10000000} \
	    -local [list localhost [lindex $ports 1]] \
	    -remote [list localhost [lindex $ports 0]] \
	    -remote [list localhost [lindex $ports 2]] \
	    -start client
	await_startup_done $clientenv

	puts "\tRepmgr$tnum.c: Add some data to master and commit."
	set dbname test.db
	set mdb [eval {berkdb_open_noerr -create $omethod -auto_commit \
	    -env $masterenv} $largs {$dbname}]
	set numtxns 3
	set t [$masterenv txn]
	for { set i 1 } { $i <= $numtxns } { incr i } {
		error_check_good db_put \
		    [eval $mdb put -txn $t $i [chop_data $method data$i]] 0
	}
	error_check_good init_txn_commit [$t commit] 0

	puts "\tRepmgr$tnum.d: Start updates on master but don't commit."
	# This locks some database pages on the master until these updates
	# are committed later in the test.
	set t2 [$masterenv txn]
	for { set i 1 } { $i <= $numtxns } { incr i } {
		set ret \
		    [$mdb get -get_both -txn $t2 $i [pad_data $method data$i]]
		error_check_good db_put \
		    [$mdb put -txn $t2 $i [chop_data $method newdata$i]] 0
	}

	# Open second client.  The uncommitted master updates will cause
	# this client to be stuck in internal init until the updates
	# are committed, so do not await_startup_done here.
	puts "\tRepmgr$tnum.e: Start second client."
	set cl2_envcmd "berkdb_env_noerr -create $verbargs \
	    -errpfx CLIENT2 -home $clientdir2 -txn -rep -thread"
	set clientenv2 [eval $cl2_envcmd]
	# Set client retransmission max time to 1 second.
	$clientenv2 rep_request 40000 1000000
	$clientenv2 repmgr -ack all -nsites $nsites \
	    -timeout {connection_retry 5000000} \
	    -local [list localhost [lindex $ports 2]] \
	    -remote [list localhost [lindex $ports 0]] \
	    -remote [list localhost [lindex $ports 1]] \
	    -start client

	puts "\tRepmgr$tnum.f: Test for page requests from rerequest thread."
	# Wait 5 seconds (significantly longer than client retransmission
	# max time) to process all page requests resulting from master
	# transactions.
	set max_wait 5
	tclsleep $max_wait
	set init_pagereq [stat_field $clientenv2 rep_stat "Pages requested"]
	# Any further page requests can only be from the rerequest thread
	# because we processed all other lingering page requests above.
	await_condition {[stat_field $clientenv2 rep_stat \
	    "Pages requested"] > $init_pagereq} $max_wait

	puts "\tRepmgr$tnum.g: Commit master updates, finish client startup."
	error_check_good update_txn_commit [$t2 commit] 0
	await_startup_done $clientenv2

	puts "\tRepmgr$tnum.h: Verifying client database contents."
	rep_verify $masterdir $masterenv $clientdir $clientenv 1 1 1
	rep_verify $masterdir $masterenv $clientdir2 $clientenv2 1 1 1

	error_check_good mdb_close [$mdb close] 0
	error_check_good client2_close [$clientenv2 close] 0
	error_check_good client_close [$clientenv close] 0
	error_check_good masterenv_close [$masterenv close] 0
}
