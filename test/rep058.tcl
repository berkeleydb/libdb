# See the file LICENSE for redistribution information.
#
# Copyright (c) 2005
#	Sleepycat Software.  All rights reserved.
#
# $Id: rep058.tcl,v 12.1 2005/10/14 18:39:21 sue Exp $
#
# TEST	rep058
# TEST	
# TEST	Replication with early databases
# TEST
# TEST	Mimic an application where they create a database before
# TEST	calling rep_start, thus writing log records on a client
# TEST	before it is a client.  Verify we cannot join repl group.

proc rep058 { method { tnum "058" } args } {

	source ./include.tcl
	if { $is_windows9x_test == 1 } { 
		puts "Skipping replication test on Win 9x platform."
		return
	} 
	#
	# There should be no difference with methods.  Just use btree.
	#
	if { [is_btree $method] == 0 } {
		puts "Rep058: Skipping for method $method."
		return
	}

	set args [convert_args $method $args]

	set recopts { "" " -recover " }
	foreach r $recopts {
		puts "Rep$tnum ($method $r): Replication with pre-created \
		    databases."
		rep058_sub $method $tnum $r $args
	}
}

proc rep058_sub { method tnum recargs largs } {
	global testdir
	source ./include.tcl
	set orig_tdir $testdir

	set masterdir $testdir/MASTERDIR
	set clientdir $testdir/CLIENTDIR

	env_cleanup $testdir
	replsetup $testdir/MSGQUEUEDIR
	file mkdir $masterdir
	file mkdir $clientdir
	
	set omethod [convert_method $method]

	# Open a master.
	repladd 1
	set envcmd(M) "berkdb_env_noerr -create -txn nosync\
	    -lock_max 2500 -lock_detect default \
	    -home $masterdir -rep_transport \[list 1 replsend\]"
#	set envcmd(M) "berkdb_env_noerr -create -txn nosync \
#	    -lock_max 2500 -lock_detect default \
#	    -errpfx ENV.M -verbose {rep on} -errfile /dev/stderr \
#	    -home $masterdir -rep_transport \[list 1 replsend\]"
	set menv [eval $envcmd(M) $recargs]
	error_check_good master_env0 [is_valid_env $menv] TRUE

	# Open a client
	repladd 2
	set envcmd(C) "berkdb_env_noerr -create -txn nosync \
	    -lock_max 2500 -lock_detect default \
	    -home $clientdir -rep_transport \[list 2 replsend\]"
#	set envcmd(C) "berkdb_env_noerr -create -txn nosync \
# 	    -lock_max 2500 -lock_detect default \
#	    -errpfx ENV.C -verbose {rep on} -errfile /dev/stderr \
#	    -home $clientdir -rep_transport \[list 2 replsend\]"
	set cenv [eval $envcmd(C) $recargs]
	error_check_good client_env [is_valid_env $cenv] TRUE

	puts "\tRep$tnum.a: Create same database in both envs."
	set dbname "test.db"
	set mdb [eval {berkdb_open_noerr -env $menv -create \
	    -auto_commit -mode 0644} -btree $dbname]
	error_check_good open [is_valid_db $mdb] TRUE
	set cdb [eval {berkdb_open_noerr -env $cenv -create \
	    -auto_commit -mode 0644} -btree $dbname]
	error_check_good open [is_valid_db $cdb] TRUE

	puts "\tRep$tnum.b: Start master and client now."
	error_check_good master [$menv rep_start -master] 0
	error_check_good client [$cenv rep_start -client] 0

	set envlist "{$menv 1} {$cenv 2}"
	process_msgs $envlist 0 NONE err
	error_check_good msg_err [is_substr $err "never part of"] 1

	puts "\tRep$tnum.c: Clean up."
	error_check_good cdb_close [$cdb close] 0
	error_check_good cdb_close [$mdb close] 0

	error_check_good menv_close [$menv close] 0
	error_check_good cenv_close [$cenv close] 0

	replclose $testdir/MSGQUEUEDIR
	set testdir $orig_tdir
	return
}

