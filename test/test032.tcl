# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996, 1997, 1998, 1999
#	Sleepycat Software.  All rights reserved.
#
#	@(#)test032.tcl	11.5 (Sleepycat) 10/25/99
#
# DB Test 32 {access method}
# Use the first 10,000 entries from the dictionary.
# Insert each with self as key and "ndups" duplicates
# For the data field, prepend the letters of the alphabet
# in a random order so that we force the duplicate sorting
# code to do something.
# By setting ndups large, we can make this an off-page test
# After all are entered; test the DB_GET_BOTH functionality
# first by retrieving each dup in the file explicitly.  Then
# test the failure case.
proc test032 { method {nentries 10000} {ndups 5} {tnum 32} args } {
	global alphabet
	global rand_init
	source ./include.tcl

	berkdb srand $rand_init

	set args [convert_args $method $args]
	set omethod [convert_method $method]

	# Create the database and open the dictionary
	set testfile $testdir/test0$tnum.db
	set t1 $testdir/t1
	set t2 $testdir/t2
	set t3 $testdir/t3
	cleanup $testdir

	puts "Test0$tnum:\
	    $method ($args) $nentries small sorted dup key/data pairs"
	if { [is_record_based $method] == 1 || \
	    [is_rbtree $method] == 1 } {
		puts "Test0$tnum skipping for method $omethod"
		return
	}
	set db [eval {berkdb open -create -truncate -mode 0644 \
	    $omethod -dup -dupsort} $args {$testfile} ]
	error_check_good dbopen [is_valid_db $db] TRUE
	set did [open $dict]

	set check_db [berkdb \
	    open -create -truncate -mode 0644 -hash $testdir/checkdb.db]
	error_check_good dbopen:check_db [is_valid_db $check_db] TRUE

	set pflags ""
	set gflags ""
	set txn ""
	set count 0

	# Here is the loop where we put and get each key/data pair
	puts "\tTest0$tnum.a: Put/get loop"
	set dbc [eval {$db cursor} $txn]
	error_check_good cursor_open [is_substr $dbc $db] 1
	while { [gets $did str] != -1 && $count < $nentries } {
		set dups ""
		for { set i 1 } { $i <= $ndups } { incr i } {
			set pref \
			    [string index $alphabet [berkdb random_int 0 25]]
			set dups $dups$pref
			set datastr $pref:$str
                        set ret [eval {$db put} \
			    $txn $pflags {$str [chop_data $method $datastr]}]
			error_check_good put $ret 0
		}
                set ret [eval {$check_db put} \
		    $txn $pflags {$str [chop_data $method $dups]}]
		error_check_good checkdb_put $ret 0

		# Now retrieve all the keys matching this key
		set x 0
		set lastdup ""
		for {set ret [$dbc get -set $str]} \
		    {[llength $ret] != 0} \
		    {set ret [$dbc get -nextdup] } {
			set k [lindex [lindex $ret 0] 0]
			if { [string compare $k $str] != 0 } {
				break
			}
			set datastr [lindex [lindex $ret 0] 1]
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
	puts "\tTest0$tnum.b: Checking file for correct duplicates (no cursor)"
        set check_c [eval {$check_db cursor} $txn]
        error_check_good check_c_open(2) \
	    [is_substr $check_c $check_db] 1

	for {set ndx 0} {$ndx < $ndups} {incr ndx} {
		for {set ret [$check_c get -first]} \
		    {[llength $ret] != 0} \
		    {set ret [$check_c get -next] } {
			set k [lindex [lindex $ret 0] 0]
			set d [lindex [lindex $ret 0] 1]
			error_check_bad key_check:$k [string length $k] 0
			error_check_bad data_check:$d [string length $d] 0

			set pref [string range $d $ndx $ndx]
			set data $pref:$k
                        set ret [eval {$db get} $txn {-get_both $k $data}]
			error_check_good \
			    get_both_data:$k $ret [list [list $k $data]]
		}
	}

	# Now repeat the above test using cursor ops
	puts "\tTest0$tnum.c: Checking file for correct duplicates (cursor)"
	set dbc [eval {$db cursor} $txn]
	error_check_good cursor_open [is_substr $dbc $db] 1

	for {set ndx 0} {$ndx < $ndups} {incr ndx} {
		for {set ret [$check_c get -first]} \
      		    {[llength $ret] != 0} \
		    {set ret [$check_c get -next] } {
			set k [lindex [lindex $ret 0] 0]
			set d [lindex [lindex $ret 0] 1]
			error_check_bad key_check:$k [string length $k] 0
			error_check_bad data_check:$d [string length $d] 0

			set pref [string range $d $ndx $ndx]
			set data $pref:$k
                        set ret [eval {$dbc get} {-get_both $k $data}]
			error_check_good \
			    get_both_key:$k $ret [list [list $k $data]]
		}
	}

	# Now check the error case
	puts "\tTest0$tnum.d: Check error case (no cursor)"
	for {set ret [$check_c get -first]} \
	    {[llength $ret] != 0} \
	    {set ret [$check_c get -next] } {
		set k [lindex [lindex $ret 0] 0]
		set d [lindex [lindex $ret 0] 1]
		error_check_bad key_check:$k [string length $k] 0
		error_check_bad data_check:$d [string length $d] 0

		set data XXX$k
		set ret [eval {$db get} $txn {-get_both $k $data}]
		error_check_good error_case:$k [llength $ret] 0
	}

	# Now check the error case
	puts "\tTest0$tnum.e: Check error case (cursor)"
	for {set ret [$check_c get -first]} \
	    {[llength $ret] != 0} \
	    {set ret [$check_c get -next] } {
		set k [lindex [lindex $ret 0] 0]
		set d [lindex [lindex $ret 0] 1]
		error_check_bad key_check:$k [string length $k] 0
		error_check_bad data_check:$d [string length $d] 0

		set data XXX$k
		set ret [eval {$dbc get} {-get_both $k $data}]
		error_check_good error_case:$k [llength $ret] 0
	}

	error_check_good check_c:close [$check_c close] 0
	error_check_good check_db:close [$check_db close] 0

	error_check_good dbc_close [$dbc close] 0
	error_check_good db_close [$db close] 0
}
