# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996, 1997, 1998, 1999, 2000
#	Sleepycat Software.  All rights reserved.
#
#	$Id: rpc001.tcl,v 11.14 2000/05/22 15:34:37 sue Exp $
#
# Test RPC specifics, primarily that unsupported functions return
# errors and such.
#
proc rpc001 { } {
	global __debug_on
	global __debug_print
	global errorInfo
	source ./include.tcl

	set curdir [pwd]
	cd $testdir
	set fulltestdir [pwd]
	cd $curdir
	#
	# First test timeouts on server.
	#
	set ttime 5
	set itime 10
	puts "Rpc001: Server timeouts: resource $ttime sec, idle $itime sec"
	if { [string compare $rpc_server "localhost"] == 0 } {
	       set dpid [exec ./berkeley_db_svc -h $fulltestdir -t $ttime \
		   -I $itime &]
	} else {
	       set dpid [exec rsh $curdir/berkeley_db_svc -h $fulltestdir \
		   -t $ttime -I $itime&]
	}
	puts "\tRpc001.a0: Started server, pid $dpid"

	tclsleep 1
	puts "\tRpc001.a1: Creating environment"

	cleanup $testdir
	set testfile "rpc001.db"
	set home [file tail $testdir]

	set env [eval {berkdb env -create -mode 0644 -home $home \
	    -server $rpc_server -client_timeout 10000 -txn}]
	error_check_good lock_env:open [is_valid_env $env] TRUE

	#
	# We need the 2nd env to just do an op to timeout the env.
	#
	set env1 [eval {berkdb env -create -mode 0644 -home $home \
	    -server $rpc_server -client_timeout 10000 -txn}]
	error_check_good lock_env:open [is_valid_env $env1] TRUE

	puts "\tRpc001.a2: Opening a database"
	#
	# NOTE: the type of database doesn't matter, just use btree.
	set db [eval {berkdb_open -create -btree -mode 0644} \
	    -env $env $testfile]
	error_check_good dbopen [is_valid_db $db] TRUE

	puts "\tRpc001.a3: Create a cursor"
	set dbc [$db cursor]
	error_check_good db_cursor [is_valid_cursor $dbc $db] TRUE

	puts "\tRpc001.a4: Starting a transaction"
	set txn [$env txn]
	error_check_good txn_begin [is_valid_txn $txn $env] TRUE

	puts "\tRpc001.a5: Timeout cursor and transactions"
	set sleeptime [expr $ttime + 2]
	tclsleep $sleeptime

	#
	# Perform a generic db operation to cause the timeout routine
	# to trigger.
	#
	set stat [catch {$db stat} ret]
	error_check_good dbstat $stat 0

	#
	# Make sure cursors and txn's are timed out, but db handles are not.
	#
	set stat [catch {$dbc close} ret]
	error_check_good dbc_close $stat 1
	error_check_good dbc_timeout \
	    [is_substr $errorInfo "DB_NOSERVER_ID"] 1

	set stat [catch {$txn commit} ret]
	error_check_good txn_commit $stat 1
	error_check_good txn_timeout \
	    [is_substr $errorInfo "DB_NOSERVER_ID"] 1

	set stat [catch {$db close} ret]
	error_check_good db_close $stat 0

	puts "\tRpc001.a6: Timeout idle env handle"
	set sleeptime [expr $itime + 2]
	tclsleep $sleeptime

	set stat [catch {$env1 close} ret]
	error_check_good env1_close $stat 0

	set stat [catch {$env close} ret]
	error_check_good env_close $stat 1
	error_check_good env_timeout \
	    [is_substr $errorInfo "DB_NOSERVER_ID"] 1

	exec $KILL $dpid
}
