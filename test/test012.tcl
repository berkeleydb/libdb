# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996, 1997, 1998, 1999
#	Sleepycat Software.  All rights reserved.
#
#	@(#)test012.tcl	11.5 (Sleepycat) 8/22/99
#
# DB Test 12 {access method}
# Take the source files and dbtest executable and enter their contents as
# the key with their names as data.  After all are entered, retrieve all;
# compare output to original. Close file, reopen, do retrieve and re-verify.
proc test012 { method args} {
	global names
	source ./include.tcl

	set args [convert_args $method $args]
	set omethod [convert_method $method]

	if { [is_record_based $method] == 1 } {
		puts "Test012 skipping for method $method"
		return
	}

	puts "Test012: $method filename=data filecontents=key pairs"

	# Create the database and open the dictionary
	set testfile $testdir/test012.db
	set t1 $testdir/t1
	set t2 $testdir/t2
	set t3 $testdir/t3
	set t4 $testdir/t4

	cleanup $testdir

	set db [eval {berkdb \
	    open -create -truncate -mode 0644} $args {$omethod $testfile}]
	error_check_good dbopen [is_valid_db $db] TRUE

	set pflags ""
	set gflags ""
	set txn ""

	# Here is the loop where we put and get each key/data pair
	set file_list [glob $test_path/../\[a-z\]*/*.c \
	    $test_path/build_*/*.lo $test_path/build_*/*.exe]

	puts "\tTest012.a: put/get loop"
	set count 0
	foreach f $file_list {
		put_file_as_key $db $txn $pflags $f

		get_file_as_key $db $txn $gflags $f

		incr count
	}

	# Now we will get each key from the DB and compare the results
	# to the original.
	puts "\tTest012.b: dump file"
	dump_binkey_file $db $txn $t1 test012.check
	error_check_good db_close [$db close] 0

	# Now compare the data to see if they match the .o and dbtest files
	set oid [open $t2.tmp w]
	foreach f $file_list {
		puts $oid $f
	}
	close $oid
	exec $SORT $t2.tmp > $t2
	exec $RM $t2.tmp
	exec $SORT $t1 > $t3

	error_check_good Test012:diff($t3,$t2) \
	    [catch { exec $CMP $t3 $t2 } res] 0

	# Now, reopen the file and run the last test again.
	puts "\tTest012.c: close, open, and dump file"
	open_and_dump_file $testfile NULL $txn $t1 test012.check \
	    dump_binkey_file_direction "-first" "-next"

	exec $SORT $t1 > $t3

	error_check_good Test012:diff($t3,$t2) \
	    [catch { exec $CMP $t3 $t2 } res] 0

	# Now, reopen the file and run the last test again in reverse direction.
	puts "\tTest012.d: close, open, and dump file in reverse direction"
	open_and_dump_file $testfile NULL $txn $t1 test012.check\
	    dump_binkey_file_direction "-last" "-prev"

	exec $SORT $t1 > $t3

	error_check_good Test012:diff($t3,$t2) \
	    [catch { exec $CMP $t3 $t2 } res] 0
}

# Check function for test012; key should be file name; data should be contents
proc test012.check { binfile tmpfile } {
	source ./include.tcl

	error_check_good Test012:diff($binfile,$tmpfile) \
	    [catch { exec $CMP $binfile $tmpfile } res] 0
}
