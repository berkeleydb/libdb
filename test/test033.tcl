# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996-2001
#	Sleepycat Software.  All rights reserved.
#
# $Id: test033.tcl,v 11.15 2001/08/13 19:11:43 bostic Exp $
#
# TEST	test033
# TEST	DB_GET_BOTH without comparison function
# TEST
# TEST	Use the first 10,000 entries from the dictionary.  Insert each with
# TEST	self as key and data; add duplicate records for each.  After all are
# TEST	entered, retrieve all and verify output using DB_GET_BOTH (on DB and
# TEST	DBC handles) and DB_GET_BOTH_RANGE (on a DBC handle) on existant and
# TEST	nonexistant keys.
# TEST
# TEST	XXX
# TEST	This does not work for Recno.
proc test033 { method {nentries 10000} {ndups 5} {tnum 33} args } {
	source ./include.tcl

	set args [convert_args $method $args]
	set omethod [convert_method $method]

	puts "Test0$tnum: $method ($args) $nentries small dup key/data pairs"
	if { [is_record_based $method] == 1 || \
	    [is_rbtree $method] == 1 } {
		puts "Test0$tnum skipping for method $omethod"
		return
	}

	# Create the database and open the dictionary
	set eindex [lsearch -exact $args "-env"]
	#
	# If we are using an env, then testfile should just be the db name.
	# Otherwise it is the test directory and the name.
	if { $eindex == -1 } {
		set testfile $testdir/test0$tnum.db
		set env NULL
	} else {
		set testfile test0$tnum.db
		incr eindex
		set env [lindex $args $eindex]
	}
	set t1 $testdir/t1
	set t2 $testdir/t2
	set t3 $testdir/t3
	cleanup $testdir $env

	set db [eval {berkdb_open -create -mode 0644 \
		$omethod -dup} $args {$testfile}]
	error_check_good dbopen [is_valid_db $db] TRUE

	set pflags ""
	set gflags ""
	set txn ""

	puts "\tTest0$tnum.a: Put/get loop."

	# Allocate a cursor for DB_GET_BOTH_RANGE.
	set dbc [eval {$db cursor} $txn]
	error_check_good cursor_open [is_substr $dbc $db] 1

	# Here is the loop where we put and get each key/data pair
	set count 0
	set did [open $dict]
	while { [gets $did str] != -1 && $count < $nentries } {
		for { set i 1 } { $i <= $ndups } { incr i } {
			set datastr $i:$str
			set ret [eval {$db put} \
			    $txn $pflags {$str [chop_data $method $datastr]}]
			error_check_good db_put $ret 0
		}

		# Now retrieve all the keys matching this key and dup
		for {set i 1} {$i <= $ndups } { incr i } {
			set datastr $i:$str
			set ret [eval {$db get} $txn {-get_both $str $datastr}]
			error_check_good "db_get:dup#" [lindex \
			    [lindex $ret 0] 1] [pad_data $method $datastr]

			set ret [eval \
			    {$dbc get} $txn {-get_both $str $datastr}]
			error_check_good "dbc_get_both:dup#" [lindex \
			    [lindex $ret 0] 1] [pad_data $method $datastr]

			set ret [eval \
			    {$dbc get} $txn {-get_both_range $str $datastr}]
			error_check_good "dbc_get_both_range:dup#" [lindex \
			    [lindex $ret 0] 1] [pad_data $method $datastr]
		}

		# Now retrieve non-existent dup (i is ndups + 1)
		set datastr $i:$str
		set ret [eval {$db get} $txn {-get_both $str $datastr}]
		error_check_good db_get_both:dupfailure [llength $ret] 0
		set ret [eval {$dbc get} $txn {-get_both $str $datastr}]
		error_check_good dbc_get_both:dupfailure [llength $ret] 0
		set ret [eval {$dbc get} $txn {-get_both_range $str $datastr}]
		error_check_good dbc_get_both_range:dupfailure [llength $ret] 0

		incr count
	}
	close $did

	puts "\tTest0$tnum.b: Verifying DB_GET_BOTH after creation."
	set count 0
	set did [open $dict]
	while { [gets $did str] != -1 && $count < $nentries } {
		# Now retrieve all the keys matching this key and dup
		for {set i 1} {$i <= $ndups } { incr i } {
			set datastr $i:$str
			set ret [eval {$db get} $txn {-get_both $str $datastr}]
			error_check_good "db_get_both:dup#" \
			    [lindex [lindex $ret 0] 1] $datastr

			set ret [eval \
			    {$dbc get} $txn {-get_both $str $datastr}]
			error_check_good "dbc_get_both:dup#" \
			    [lindex [lindex $ret 0] 1] $datastr

			set ret [eval \
			    {$dbc get} $txn {-get_both_range $str $datastr}]
			error_check_good "dbc_get_both_range:dup#" \
			    [lindex [lindex $ret 0] 1] $datastr
		}

		# Now retrieve non-existent dup (i is ndups + 1)
		set datastr $i:$str
		set ret [eval {$db get} $txn {-get_both $str $datastr}]
		error_check_good db_get_both:dupfailure [llength $ret] 0
		set ret [eval {$dbc get} $txn {-get_both $str $datastr}]
		error_check_good dbc_get_both:dupfailure [llength $ret] 0
		set ret [eval {$dbc get} $txn {-get_both_range $str $datastr}]
		error_check_good dbc_get_both_range [llength $ret] 0

		incr count
	}
	close $did

	error_check_good dbc_close [$dbc close] 0
	error_check_good db_close [$db close] 0
}
