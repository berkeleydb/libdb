# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996, 1997, 1998
#	Sleepycat Software.  All rights reserved.
#
#	@(#)test037.tcl	8.4 (Sleepycat) 11/25/98
#
# Test037: RMW functionality.
proc test037 { method {nentries 100} } {
source ./include.tcl
	puts "Test037: RMW $method"
	set method [convert_method $method]

	# Create the database
	cleanup $testdir
	set testfile test037.db
	set db [dbopen $testfile [expr $DB_CREATE | $DB_TRUNCATE] 0644 $method]
	error_check_good dbopen [is_valid_db $db] TRUE

	set did [open $dict]
	set count 0
	puts "\tTest037.a: Creating database"
	# Here is the loop where we put and get each key/data pair
	while { [gets $did str] != -1 && $count < $nentries } {
		if { [string compare $method DB_RECNO] == 0 } {
			global kvals

			set key [expr $count + 1]
			set kvals($key) $str
		} else {
			set key $str
		}
		set ret [$db put 0 $key $str 0]
		error_check_good put $ret 0

		set ret [$db get txn $key 0]
		error_check_good get $ret $str
		incr count
	}
	close $did
	error_check_good dbclose [$db close] 0

	puts "\tTest037.b: Setting up environments"

	# Open the environment and transaction manager
	set env_cmd "dbenv -dbflags \
	    [expr $DB_CREATE | $DB_INIT_TXN | $DB_INIT_LOCK | $DB_INIT_MPOOL]"
	set e [eval $env_cmd]
	error_check_good dbenv [is_valid_widget $e env] TRUE
	set tmgr [txn "" 0 0 -dbenv $e]
	error_check_good txn_open [is_valid_widget $tmgr mgr] TRUE

	# Open environment remotely
	set f1 [open |./dbtest r+]
	set remote_env [send_cmd $f1 $env_cmd]
	error_check_good remote:env_open [is_valid_widget $remote_env env] TRUE

	# Open transaction manager
	set rmgr [send_cmd $f1 "txn \"\" 0 0 -dbenv $remote_env"]
	error_check_good remote:txn_open [is_valid_widget $rmgr mgr] TRUE

	# Now try put test without RMW.  Gets on one site should not
	# lock out gets on another.

	# Open databases and dictionary
	puts "\tTest037.c: Opening databases"
	set did [open $dict]
	set rkey 0

	set db [dbopen $testfile 0 0 $method -dbenv $e]
	error_check_good dbopen [is_valid_db $db] TRUE
	set rdb [send_cmd $f1 \
	    "dbopen $testfile 0 0 $method -dbenv $remote_env"]
	error_check_good remote:dbopen [is_valid_widget $rdb db] TRUE

	puts "\tTest037.d: Testing without RMW"
	# Now begin transactions locally and remotely.
	set t [$tmgr begin]
	error_check_good txn_begin [is_valid_widget $t $tmgr.txn] TRUE

	set remote_t [send_cmd $f1 "$rmgr begin"]
	error_check_good remote:txn_begin \
	    [is_valid_widget $remote_t $rmgr.txn] TRUE

	# Now, get a key and try to "get" it from both DBs.
	error_check_bad "gets on new open" [gets $did str] -1
	incr rkey
	if { [string compare $method DB_RECNO] == 0 } {
		set key $rkey
	} else {
		set key $str
	}
	set rec [$db get $t $key 0]
	error_check_good local_get $rec $str

	set r [send_timed_cmd $f1 0 "$rdb get $remote_t $key 0"]
	error_check_good remote_send $r 0

	# Now sleep before releasing local record lock
	exec $SLEEP 5
	error_check_good local_commit [$t commit] 0

	# Now get the remote result
	set remote_time [rcv_result $f1]
	error_check_good no_rmw_get:remote_time [expr $remote_time <= 1] 1

	# Commit the remote
	set r [send_cmd $f1 "$remote_t commit"]
	error_check_good remote_commit $r 0

	puts "\tTest037.e: Testing with RMW"

	# Now begin transactions locally and remotely.
	set t [$tmgr begin]
	error_check_good txn_begin [is_valid_widget $t $tmgr.txn] TRUE

	set remote_t [send_cmd $f1 "$rmgr begin"]
	error_check_good remote:txn_begin \
	    [is_valid_widget $remote_t $rmgr.txn] TRUE

	# Now, get a key and try to "get" it from both DBs.
	error_check_bad "gets on new open" [gets $did str] -1
	incr rkey
	if { [string compare $method DB_RECNO] == 0 } {
		set key $rkey
	} else {
		set key $str
	}

	set rec [$db get $t $key $DB_RMW]
	error_check_good local_get $rec $str

	set r [send_timed_cmd $f1 0 "$rdb get $remote_t $key 0"]
	error_check_good remote_send $r 0

	# Now sleep before releasing local record lock
	exec $SLEEP 5
	error_check_good local_commit [$t commit] 0

	# Now get the remote result
	set remote_time [rcv_result $f1]
	error_check_good rmw_get:remote_time [expr $remote_time > 4] 1

	# Commit the remote
	set r [send_cmd $f1 "$remote_t commit"]
	error_check_good remote_commit $r 0

	# Close everything up: remote first
	set r [send_cmd $f1 "$rdb close"]
	error_check_good remote_db_close $r 0

	set r [send_cmd $f1 "$rmgr close"]
	set r [send_cmd $f1 "reset_env $remote_env"]

	# Close locally
	error_check_good db_close [$db close] 0
	error_check_good txn_close [$tmgr close] 0
	reset_env $e
	close $did
	close $f1
}

proc send_cmd { fd cmd } {
source ./include.tcl

	puts $fd "set v \[$cmd\]"
	puts $fd "puts \$v"
	puts $fd "flush stdout"
	flush $fd
	debug_check
	exec $SLEEP 2

	set r [rcv_result $fd]
	return $r
}

proc rcv_result { fd } {

	set r [gets $fd result]
	error_check_bad remote_read $r -1

	return $result
}

proc send_timed_cmd { fd rcv_too cmd } {

	set c1 "set start \[timestamp -r\]; "
	set c2 "puts \[expr \[timestamp -r\] - \$start\]"
	set full_cmd [concat $c1 $cmd ";" $c2]

	puts $fd $full_cmd
	puts $fd "flush stdout"
	flush $fd
	return 0
}
