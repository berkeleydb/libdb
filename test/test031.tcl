# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996, 1997, 1998
#	Sleepycat Software.  All rights reserved.
#
#	@(#)test031.tcl	8.3 (Sleepycat) 10/29/98
#
# DB Test 31 {access method}
# Use the first 10,000 entries from the dictionary.
# Insert each with self as key and "ndups" duplicates
# For the data field, prepend the letters of the alphabet
# in a random order so that we force the duplicate sorting
# code to do something.
# By setting ndups large, we can make this an off-page test
# After all are entered, retrieve all; verify output.
# Close file, reopen, do retrieve and re-verify.
# This does not work for recno
proc test031 { method {nentries 10000} {ndups 5} {tnum 31} args } {
global alphabet
srand 1234
	set omethod $method
	set method [convert_method $method]
	set args [convert_args $method $args]

	# Get global declarations since tcl doesn't support
	# any useful equivalent to #defines!
	source ./include.tcl

	# Create the database and open the dictionary
	set testfile test0$tnum.db
	set t1 $testdir/t1
	set t2 $testdir/t2
	set t3 $testdir/t3
	cleanup $testdir
	set args [add_to_args [expr $DB_DUP | $DB_DUPSORT] $args]
	puts "Test0$tnum: $method ($args) $nentries small sorted dup key/data pairs"
	if { [string compare $method DB_RECNO] == 0 || \
	    [is_rbtree $omethod] == 1 } {
		puts "Test0$tnum skipping for method $omethod"
		return
	}
	set db [eval [concat dbopen $testfile \
	    [expr $DB_CREATE | $DB_TRUNCATE] 0644 $method $args]]
	error_check_good dbopen [is_valid_db $db] TRUE
	set did [open $dict]

	set check_db [dbopen checkdb.db [expr $DB_CREATE | $DB_TRUNCATE] \
	    0644 DB_HASH]
	error_check_good dbopen:check_db [is_valid_db $check_db] TRUE

	set flags 0
	set txn 0
	set count 0

	# Here is the loop where we put and get each key/data pair
	puts "\tTest0$tnum.a: Put/get loop"
	set dbc [$db cursor $txn]
	error_check_good cursor_open [is_valid_widget $dbc $db.cursor] TRUE
	while { [gets $did str] != -1 && $count < $nentries } {
		set dups ""
		for { set i 1 } { $i <= $ndups } { incr i } {
			set pref [string index $alphabet [random_int 0 25]]
			set dups $dups$pref
			set datastr $pref:$str
			set ret [$db put $txn $str $datastr $flags]
			error_check_good put $ret 0
		}
		set ret [$check_db put $txn $str $dups $flags]
		error_check_good checkdb_put $ret 0

		# Now retrieve all the keys matching this key
		set x 0
		set lastdup ""
		for {set ret [$dbc get $str $DB_SET]} \
		    {[string length $ret] != 0} \
		    {set ret [$dbc get 0 $DB_NEXT_DUP] } {
			set k [lindex $ret 0]
			if { [string compare $k $str] != 0 } {
				break
			}
			set datastr [lindex $ret 1]
			if {[string length $datastr] == 0} {
				break
			}
			if {[string compare $lastdup $datastr] > 0} {
				error_check_good sorted_dups($lastdup,$datastr)\
				    0 1
			}
			incr x
			set lastdup $datastr
		}
		error_check_good "Test0$tnum:ndups:$str" $x $ndups
		incr count
	}
	error_check_good cursor_close [$dbc close] 0
	close $did

	# Now we will get each key from the DB and compare the results
	# to the original.
	puts "\tTest0$tnum.b: Checking file for correct duplicates"
	set dbc [$db cursor $txn]
	error_check_good cursor_open(2) [is_valid_widget $dbc $db.cursor] TRUE

	set lastkey ""
	for {set ret [$dbc get $str $DB_FIRST]} \
	    {[string length $ret] != 0} \
	    {set ret [$dbc get 0 $DB_NEXT] } {
		set k [lindex $ret 0]
		set d [lindex $ret 1]
		error_check_bad key_check:$k [string length $k] 0
		error_check_bad data_check:$d [string length $d] 0

		if { [string compare $k $lastkey] != 0 } {
			# Remove last key from the checkdb
			if { [string length $lastkey] != 0 } {
				error_check_good check_db:del:$lastkey \
				    [$check_db del $txn $lastkey 0] 0
			}
			set lastdup ""
			set lastkey $k
			set dups [$check_db get $txn $k 0]
			error_check_good check_db:get:$k \
			    [string length $dups] $ndups
		}

		if { [string compare $lastdup $d] > 0 } {
			error_check_good dup_check:$k:$d 0 1
		}
		set lastdup $d

		set pref [string range $d 0 0]
		set ndx [string first $pref $dups]
		error_check_good valid_duplicate [expr $ndx >= 0] 1
		set a [string range $dups 0 [expr $ndx - 1]]
		set b [string range $dups [expr $ndx + 1] end]
		set dups $a$b
	}
	# Remove last key from the checkdb
	if { [string length $lastkey] != 0 } {
		error_check_good check_db:del:$lastkey \
		    [$check_db del $txn $lastkey 0] 0
	}

	# Make sure there is nothing left in check_db

	set check_c [$check_db cursor $txn]
	set ret [$check_c get 0 $DB_FIRST]
	error_check_good check_c:get:$ret [string length $ret] 0
	error_check_good check_c:close [$check_c close] 0
	error_check_good check_db:close [$check_db close] 0

	error_check_good dbc_close [$dbc close] 0
	error_check_good db_close [$db close] 0
}
