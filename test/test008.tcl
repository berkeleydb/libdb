# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996, 1997, 1998, 1999
#	Sleepycat Software.  All rights reserved.
#
#	@(#)test008.tcl	11.5 (Sleepycat) 8/17/99
#
# DB Test 8 {access method}
# Take the source files and dbtest executable and enter their names as the
# key with their contents as data.  After all are entered, begin looping
# through the entries; deleting some pairs and then readding them.
proc test008 { method {nentries 10000} {reopen 8} {debug 0} args} {
	source ./include.tcl

	set tnum Test00$reopen
	set args [convert_args $method $args]
	set omethod [convert_method $method]

	if { [is_record_based $method] == 1 } {
		puts "Test00$reopen skipping for method $method"
		return
	}

	puts -nonewline "$tnum: $method filename=key filecontents=data pairs"
	if {$reopen == 9} {
		puts "(with close)"
	} else {
		puts ""
	}

	# Create the database and open the dictionary
	set testfile $testdir/$tnum.db
	set t1 $testdir/t1
	set t2 $testdir/t2
	set t3 $testdir/t3
	set t4 $testdir/t4

	cleanup $testdir

	set db [eval {berkdb open -create -truncate -mode 0644} $args {$omethod $testfile}]
	error_check_good dbopen [is_valid_db $db] TRUE

	set pflags ""
	set gflags ""
	set txn ""

	# Here is the loop where we put and get each key/data pair
	set file_list [glob ../*/*.c ../build_*/*.o ../build_*/*.lo ../build_*/*.exe]

	set count 0
	puts "\tTest00$reopen.a: Initial put/get loop"
	foreach f $file_list {
		set names($count) $f
		set key $f

		put_file $db $txn $pflags $f

		get_file $db $txn $gflags $f $t4

		error_check_good Test00$reopen:diff($f,$t4) \
		    [catch { exec $CMP $f $t4 } res] 0

		incr count
	}

	if {$reopen == 9} {
		error_check_good db_close [$db close] 0

		set db [berkdb open $testfile]
		error_check_good dbopen [is_valid_db $db] TRUE
	}

	# Now we will get step through keys again (by increments) and
	# delete all the entries, then re-insert them.

	puts "\tTest00$reopen.b: Delete re-add loop"
	foreach i "1 2 4 8 16" {
		for {set ndx 0} {$ndx < $count} { incr ndx $i} {
         set r [eval {$db del} $txn {$names($ndx)}]
			error_check_good db_del:$names($ndx) $r 0
		}
		for {set ndx 0} {$ndx < $count} { incr ndx $i} {
         put_file $db $txn $pflags $names($ndx)
		}
	}

	if {$reopen == 9} {
		error_check_good db_close [$db close] 0
      set db [berkdb open $testfile]
	   error_check_good dbopen [is_valid_db $db] TRUE
	}

	# Now, reopen the file and make sure the key/data pairs look right.
	puts "\tTest00$reopen.c: Dump contents forward"
	dump_bin_file $db $txn $t1 test008.check

	set oid [open $t2.tmp w]
	foreach f $file_list {
		puts $oid $f
	}
	close $oid
	exec $SORT $t2.tmp > $t2
	exec $RM $t2.tmp
	exec $SORT $t1 > $t3

	error_check_good Test00$reopen:diff($t3,$t2) \
	    [catch { exec $CMP $t3 $t2 } res] 0

	# Now, reopen the file and run the last test again in reverse direction.
	puts "\tTest00$reopen.d: Dump contents backward"
	dump_bin_file_direction $db $txn $t1 test008.check "-last" "-prev"

	exec $SORT $t1 > $t3

	error_check_good Test00$reopen:diff($t3,$t2) \
	    [catch { exec $CMP $t3 $t2 } res] 0
	error_check_good close:$db [$db close] 0
}

proc test008.check { binfile tmpfile } {
	global tnum
	source ./include.tcl

	error_check_good diff($binfile,$tmpfile) \
	    [catch { exec $CMP $binfile $tmpfile } res] 0
}
