# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996, 1997, 1998
#	Sleepycat Software.  All rights reserved.
#
#	@(#)test033.tcl	10.2 (Sleepycat) 9/14/98
#
# DB Test 33 {access method}
# Use the first 10,000 entries from the dictionary.
# Insert each with self as key and data; add duplicate
# records for each.
# After all are entered, retrieve all; verify output by doing
# DB_GET_BOTH on existing and non-existing keys.
# This does not work for recno
proc test033 { method {nentries 10000} {ndups 5} {tnum 33} args } {
	set omethod $method
	set method [convert_method $method]
	set args [convert_args $method $args]
	puts "Test0$tnum: $method ($args) $nentries small dup key/data pairs"
	if { [string compare $method DB_RECNO] == 0 || \
	    [is_rbtree $omethod] == 1 } {
		puts "Test0$tnum skipping for method $omethod"
		return
	}

	# Get global declarations since tcl doesn't support
	# any useful equivalent to #defines!
	source ./include.tcl

	# Create the database and open the dictionary
	set testfile test0$tnum.db
	set t1 $testdir/t1
	set t2 $testdir/t2
	set t3 $testdir/t3
	cleanup $testdir
	set args [add_to_args $DB_DUP $args]
	set db [eval [concat dbopen $testfile \
	    [expr $DB_CREATE | $DB_TRUNCATE] 0644 $method $args]]
	error_check_good dbopen [is_valid_db $db] TRUE
	set did [open $dict]

	set flags 0
	set txn 0
	set count 0

	puts "Test0$tnum.a: Put/get loop."
	# Here is the loop where we put and get each key/data pair
	while { [gets $did str] != -1 && $count < $nentries } {
		for { set i 1 } { $i <= $ndups } { incr i } {
			set datastr $i:$str
			set ret [$db put $txn $str $datastr $flags]
			error_check_good db_put $ret 0
		}

		# Now retrieve all the keys matching this key and dup
		for {set i 1} {$i <= $ndups } { incr i } {
			set datastr $i:$str
			set ret [$db bget $txn $str $datastr $DB_GET_BOTH]
			error_check_good "Test0$tnum:dup#" $ret $datastr
		}

		# Now retrieve non-existent dup (i is ndups + 1)
		set datastr $i:$str
		set ret [$db bget $txn $str $datastr $DB_GET_BOTH]
		error_check_good Test0$tnum:dupfailure \
		    $ret "Key $str not found."
		incr count
	}
	close $did

	set did [open $dict]
	set count 0
	puts "Test0$tnum.b: Verifying DB_GET_BOTH after creation."
	while { [gets $did str] != -1 && $count < $nentries } {
		# Now retrieve all the keys matching this key and dup
		for {set i 1} {$i <= $ndups } { incr i } {
			set datastr $i:$str
			set ret [$db bget $txn $str $datastr $DB_GET_BOTH]
			error_check_good "Test0$tnum:dup#" $ret $datastr
		}

		# Now retrieve non-existent dup (i is ndups + 1)
		set datastr $i:$str
		set ret [$db bget $txn $str $datastr $DB_GET_BOTH]
		error_check_good Test0$tnum:dupfailure $ret "Key $str not found."
		incr count
	}
	close $did

	error_check_good db_close [$db close] 0
}
