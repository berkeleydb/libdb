# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996, 1997, 1998, 1999
#	Sleepycat Software.  All rights reserved.
#
#	@(#)test036.tcl	11.6 (Sleepycat) 9/24/99
#
# DB Test 36 {access method}
# Put nentries key/data pairs (from the dictionary) using a cursor
# and KEYFIRST and KEYLAST (this tests the case where use use cursor
# put for non-existent keys).
proc test036 { method {nentries 10000} args } {
	source ./include.tcl

	set args [convert_args $method $args]
	set omethod [convert_method $method]

	puts "Test036: $method ($args) $nentries equal key/data pairs"
	if { [is_record_based $method] == 1 } {
		puts "Test036 skipping for method recno"
		return
	}

	# Create the database and open the dictionary
	set testfile $testdir/test036.db
	set t1 $testdir/t1
	set t2 $testdir/t2
	set t3 $testdir/t3
	cleanup $testdir
	set db [eval {berkdb \
	    open -create -truncate -mode 0644} $args {$omethod $testfile}]
	error_check_good dbopen [is_valid_db $db] TRUE
	set did [open $dict]

	set pflags ""
	set gflags ""
	set txn ""
	set count 0

	if { [is_record_based $method] == 1 } {
		set checkfunc test036_recno.check
		append gflags " -recno"
	} else {
		set checkfunc test036.check
	}
	puts "\tTest036.a: put/get loop KEYFIRST"
	# Here is the loop where we put and get each key/data pair
	set dbc [eval {$db cursor} $txn]
	error_check_good cursor [is_substr $dbc $db] 1
	while { [gets $did str] != -1 && $count < $nentries } {
		if { [is_record_based $method] == 1 } {
			global kvals

			set key [expr $count + 1]
			set kvals($key) $str
		} else {
			set key $str
		}
		set ret [eval {$dbc put} $txn $pflags {-keyfirst $key $str}]
		error_check_good put $ret 0

		set ret [eval {$db get} $txn $gflags {$key}]
		error_check_good get [lindex [lindex $ret 0] 1] $str
		incr count
	}
	error_check_good dbc_close [$dbc close] 0

	puts "\tTest036.a: put/get loop KEYLAST"
	set dbc [eval {$db cursor} $txn]
	error_check_good cursor [is_substr $dbc $db] 1
	while { [gets $did str] != -1 && $count < $nentries } {
		if { [is_record_based $method] == 1 } {
			global kvals

			set key [expr $count + 1]
			set kvals($key) $str
		} else {
			set key $str
		}
		set ret [eval {$dbc put} $txn $pflags {-keylast $key $str}]
		error_check_good put $ret 0

		set ret [eval {$db get} $txn $gflags {$key}]
		error_check_good get [lindex [lindex $ret 0] 1] $str
		incr count
	}
	error_check_good dbc_close [$dbc close] 0
	close $did

	# Now we will get each key from the DB and compare the results
	# to the original.
	puts "\tTest036.c: dump file"
	dump_file $db $txn $t1 $checkfunc
	error_check_good db_close [$db close] 0

	# Now compare the keys to see if they match the dictionary (or ints)
	if { [is_record_based $method] == 1 } {
		set oid [open $t2 w]
		for {set i 1} {$i <= $nentries} {set i [incr i]} {
			puts $oid $i
		}
		close $oid
		exec $MV $t1 $t3
	} else {
		set q q
		exec $SED $nentries$q $dict > $t3
		exec $SORT $t3 > $t2
		exec $SORT $t1 > $t3
	}

}

# Check function for test036; keys and data are identical
proc test036.check { key data } {
	error_check_good "key/data mismatch" $data $key
}

proc test036_recno.check { key data } {
	global dict
	global kvals

	error_check_good key"$key"_exists [info exists kvals($key)] 1
	error_check_good "key/data mismatch, key $key" $data $kvals($key)
}
