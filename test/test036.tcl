# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996, 1997, 1998
#	Sleepycat Software.  All rights reserved.
#
#	@(#)test036.tcl	8.2 (Sleepycat) 9/16/98
#
# DB Test 36 {access method}
# Put nentries key/data pairs (from the dictionary) using a cursor
# and KEYFIRST and KEYLAST (this tests the case where use use cursor
# put for non-existent keys).
proc test036 { method {nentries 10000} args } {
	set args [convert_args $method $args]
	set method [convert_method $method]
	puts "Test036: $method ($args) $nentries equal key/data pairs"
	if { [string compare $method DB_RECNO] == 0 } {
		puts "Test036 skipping for method recno"
		return
	}

	# Get global declarations since tcl doesn't support
	# any useful equivalent to #defines!
	source ./include.tcl

	# Create the database and open the dictionary
	set testfile test036.db
	set t1 $testdir/t1
	set t2 $testdir/t2
	set t3 $testdir/t3
	cleanup $testdir
	set db [eval [concat dbopen \
	    $testfile [expr $DB_CREATE | $DB_TRUNCATE] 0644 $method $args]]
	error_check_good dbopen [is_valid_db $db] TRUE
	set did [open $dict]

	set flags 0
	set txn 0
	set count 0

	if { [string compare $method DB_RECNO] == 0 } {
		set checkfunc test036_recno.check
		set put putn
	} else {
		set checkfunc test036.check
		set put put
	}
	puts "\tTest036.a: put/get loop KEYFIRST"
	# Here is the loop where we put and get each key/data pair
	set dbc [$db cursor $txn]
	error_check_good cursor [is_valid_widget $dbc $db.cursor] TRUE
	while { [gets $did str] != -1 && $count < $nentries } {
		if { [string compare $method DB_RECNO] == 0 } {
			global kvals

			set key [expr $count + 1]
			set kvals($key) $str
		} else {
			set key $str
		}
		set ret [$dbc $put $key $str $DB_KEYFIRST]
		error_check_good put $ret 0

		set ret [$db get $txn $key $flags]
		error_check_good get $ret $str
		incr count
	}
	error_check_good dbc_close [$dbc close] 0

	puts "\tTest036.a: put/get loop KEYLAST"
	set dbc [$db cursor $txn]
	error_check_good cursor [is_valid_widget $dbc $db.cursor] TRUE
	while { [gets $did str] != -1 && $count < $nentries } {
		if { [string compare $method DB_RECNO] == 0 } {
			global kvals

			set key [expr $count + 1]
			set kvals($key) $str
		} else {
			set key $str
		}
		set ret [$dbc $put $key $str $DB_KEYLAST]
		error_check_good put $ret 0

		set ret [$db get $txn $key $flags]
		error_check_good get $ret $str
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
	if { [string compare $method DB_RECNO] == 0 } {
		set oid [open $t2 w]
		for {set i 1} {$i <= $nentries} {set i [incr i]} {
			puts $oid $i
		}
		close $oid
		exec $MV $t1 $t3
	} else {
		set q q
		exec $SED $nentries$q $dict > $t2
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
