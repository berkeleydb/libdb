# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996, 1997, 1998, 1999
#	Sleepycat Software.  All rights reserved.
#
#	@(#)test025.tcl	11.4 (Sleepycat) 8/17/99
#
# DB Test 25 {method nentries}
# Test the DB_APPEND flag.
proc test025 { method {nentries 10000} args} {
	global kvals
	source ./include.tcl

	set args [convert_args $method $args]
	set omethod [convert_method $method]
	puts "Test025: $method ($args)"

	if { [string compare $omethod "-btree"] == 0 } {
		puts "Test025 skipping for method BTREE"
		return
	}
	if { [string compare $omethod "-hash"] == 0 } {
		puts "Test025 skipping for method HASH"
		return
	}

	# Create the database and open the dictionary
	set testfile $testdir/test025.db
	set t1 $testdir/t1

	cleanup $testdir
	set db [eval {berkdb \
	    open -create -truncate -mode 0644} $args {$omethod $testfile}]
	error_check_good dbopen [is_valid_db $db] TRUE
	set did [open $dict]

	puts "\tTest025.a: put/get loop"
	set gflags " -recno"
	set pflags " -append"
	set txn ""
	set checkfunc test025_check

	# Here is the loop where we put and get each key/data pair
	set count 0
	while { [gets $did str] != -1 && $count < $nentries } {
		set k [expr $count + 1]
		set kvals($k) [pad_data $method $str]
		set ret [eval {$db put} $txn $pflags {[chop_data $method $str]}]
		error_check_good db_put $ret $k

		set ret [eval {$db get} $txn $gflags {$k}]
		error_check_good \
		    get $ret [list [list $k [pad_data $method $str]]]
		incr count
	}
	close $did

	# Now we will get each key from the DB and compare the results
	# to the original.
	puts "\tTest025.b: dump file"
	dump_file $db $txn $t1 $checkfunc
	error_check_good db_close [$db close] 0

	puts "\tTest025.c: close, open, and dump file"
	# Now, reopen the file and run the last test again.
	open_and_dump_file $testfile NULL $txn $t1 $checkfunc \
	    dump_file_direction -first -next

	# Now, reopen the file and run the last test again in the
	# reverse direction.
	puts "\tTest001.d: close, open, and dump file in reverse direction"
	open_and_dump_file $testfile NULL $txn $t1 $checkfunc \
		dump_file_direction -last -prev
}

proc test025_check { key data } {
	global kvals

	error_check_good key"$key"_exists [info exists kvals($key)] 1
	error_check_good " key/data mismatch for |$key|" $data $kvals($key)
}
