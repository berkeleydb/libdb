# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996, 1997, 1998, 1999
#	Sleepycat Software.  All rights reserved.
#
#	@(#)test031.tcl	11.7 (Sleepycat) 11/8/99
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

	puts "Test0$tnum: \
	    $method ($args) $nentries small sorted dup key/data pairs"
	if { [is_record_based $method] == 1 || \
	    [is_rbtree $method] == 1 } {
		puts "Test0$tnum skipping for method $omethod"
		return
	}
	set db [eval {berkdb open -create -truncate \
		-mode 0644} $args {$omethod -dup -dupsort $testfile}]
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
			if {[string compare \
			    $lastdup [pad_data $method $datastr]] > 0} {
				error_check_good \
				    sorted_dups($lastdup,$datastr) 0 1
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
	set dbc [eval {$db cursor} $txn]
	error_check_good cursor_open(2) [is_substr $dbc $db] 1

	set lastkey ""
	for {set ret [$dbc get -first]} \
	    {[llength $ret] != 0} \
	    {set ret [$dbc get -next] } {
		set k [lindex [lindex $ret 0] 0]
		set d [lindex [lindex $ret 0] 1]
		error_check_bad key_check:$k [string length $k] 0
		error_check_bad data_check:$d [string length $d] 0

		if { [string compare $k $lastkey] != 0 } {
			# Remove last key from the checkdb
			if { [string length $lastkey] != 0 } {
				error_check_good check_db:del:$lastkey \
				    [eval {$check_db del} $txn {$lastkey}] 0
			}
			set lastdup ""
			set lastkey $k
			set dups [lindex [lindex [eval {$check_db get} \
				$txn {$k}] 0] 1]
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
		[eval {$check_db del} $txn {$lastkey}] 0
	}

	# Make sure there is nothing left in check_db

	set check_c [eval {$check_db cursor} $txn]
	set ret [$check_c get -first]
	error_check_good check_c:get:$ret [llength $ret] 0
	error_check_good check_c:close [$check_c close] 0
	error_check_good check_db:close [$check_db close] 0

	error_check_good dbc_close [$dbc close] 0
	error_check_good db_close [$db close] 0
}
