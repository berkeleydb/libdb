# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996, 1997, 1998
#	Sleepycat Software.  All rights reserved.
#
#	@(#)test043.tcl	10.2 (Sleepycat) 12/12/98
#
# DB Test 43 {method nentries}
# Test the Record number implicit creation and renumbering options.

proc test043 { method {nentries 10000} args} {
	set do_renumber [is_rrecno $method]
	set args [convert_args $method $args]
	set method [convert_method $method]
	puts "Test043: $method ($args)"

	if { [string compare $method DB_RECNO] != 0 } {
		puts "Test043 skipping for method $method"
		return
	}

	# Get global declarations since tcl doesn't support
	# any useful equivalent to #defines!
	source ./include.tcl

	# Create the database and open the dictionary
	set testfile test043.db
	cleanup $testdir

	# Create the database
	set db [eval [concat dbopen \
	    $testfile [expr $DB_CREATE | $DB_TRUNCATE] 0644 $method $args]]
	error_check_good dbopen [is_valid_db $db] TRUE

	set flags 0
	set txn 0

	# First test implicit creation and retrieval
	set count 1
	set interval 5
	if { $nentries < $interval } {
		set nentries [expr $interval + 1]
	}
	puts "\tTest043.a: insert keys at $interval record intervals"
	while { $count <= $nentries } {
		set ret [$db putn $txn $count $count 0]
		error_check_good "$db putn $count" $ret 0
		set last $count
		incr count $interval
	}

	puts "\tTest043.b: get keys using DB_FIRST/DB_NEXT"
	set dbc [$db cursor $txn]
	error_check_good "$db cursor" [is_valid_widget $dbc $db.cursor] TRUE

	set check 1
	for { set rec [$dbc getn 0 $DB_FIRST] } { [llength $rec] != 0 } {
	    set rec [$dbc getn 0 $DB_NEXT] } {
		set k [lindex $rec 0]
		set d [lindex $rec 1]
		error_check_good "$dbc get key==data" $k $d
		error_check_good "$dbc get sequential" $k $check
		if { $k > $nentries } {
			error_check_good "$dbc get key too large" $k $nentries
		}
		incr check $interval
	}

	# Now make sure that we get DB_KEYEMPTY for non-existent keys
	puts "\tTest043.c: Retrieve non-existent keys"

	set check 1
	for { set rec [$dbc getn 0 $DB_FIRST] } { [llength $rec] != 0 } {
	    set rec [$dbc getn 0 $DB_NEXT] } {
		set k [lindex $rec 0]
		set ret [$db getn $txn [expr $k + 1] 0]
		error_check_good "$db getn [expr $k + 1]" $ret -1
		incr check $interval
		# Make sure we don't do a retrieve past the end of file
		if { $check >= $last }  {
			break
		}
	}

	# Now try deleting and make sure the right thing happens.
	puts "\tTest043.d: Delete tests"
	set rec [$dbc getn 0 $DB_FIRST]
	error_check_bad "$dbc get 0 $DB_FIRST" [llength $rec] 0
	error_check_good  "$dbc get 0 $DB_FIRST key" [lindex $rec 0] 1
	error_check_good  "$dbc get 0 $DB_FIRST data" [lindex $rec 1] 1

	# Delete the first item
	error_check_good "$dbc del" [$dbc del 0] 0

	# Retrieving 1 should always fail
	set ret [$db getn $txn 1 0]
	error_check_good "$db getn 1" $ret -1

	# Now, retrieving other keys should work; keys will vary depending
	# upon renumbering.
	if { $do_renumber == 1 } {
		set count [expr 0 + $interval]
		set max [expr $nentries - 1]
	} else {
		set count [expr 1 + $interval]
		set max $nentries
	}

	while { $count <= $max } {
		set rec [$db getn $txn $count 0]
		if { $do_renumber == 1 } {
			set data [expr $count + 1]
		} else {
			set data $count
		}
		error_check_good "$db getn $count" $data $rec
		incr count $interval
	}
	set max [expr $count - $interval]

	puts "\tTest043.e: Verify LAST/PREV functionality"
	set count $max
	for { set rec [$dbc getn 0 $DB_LAST] } { [llength $rec] != 0 } {
	    set rec [$dbc getn 0 $DB_PREV] } {
		set k [lindex $rec 0]
		set d [lindex $rec 1]
		if { $do_renumber == 1 } {
			set data [expr $k + 1]
		} else {
			set data $k
		}
		error_check_good "$dbc get key==data" $data $d
		error_check_good "$dbc get sequential" $k $count
		if { $k > $nentries } {
			error_check_good "$dbc get key too large" $k $nentries
		}
		set count [expr $count - $interval]
		if { $count < 1 } {
			break
		}
	}
	error_check_good dbc_close [$dbc close] 0
	error_check_good db_close [$db close] 0
}

