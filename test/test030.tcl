# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996, 1997, 1998
#	Sleepycat Software.  All rights reserved.
#
#	@(#)test030.tcl	8.3 (Sleepycat) 9/10/98
#
# DB Test 30: Test DB_NEXT_DUP Functionality.

proc test030 { method {nentries 10000} args } {
source ./include.tcl

	set omethod $method
	set method [convert_method $method]
	if { [string compare $method DB_RECNO] == 0 ||
	    [is_rbtree $omethod] == 1 } {
		puts "Test030 skipping for method $omethod"
		return
	}
	set args [convert_args $method $args]

	puts "Test030: $method ($args) $nentries DB_NEXT_DUP testing"
	srand 30

	# Create the database and open the dictionary
	set testfile test030.db
	set t1 $testdir/t1
	set t2 $testdir/t2
	set t3 $testdir/t3
	cleanup $testdir
	set args [add_to_args $DB_DUP $args]
	set db [eval [concat dbopen $testfile \
	    [expr $DB_CREATE | $DB_TRUNCATE] 0644 $method $args]]
	error_check_good dbopen [is_valid_db $db] TRUE

	# Use a second DB to keep track of how many duplicates
	# we enter per key

	set cntdb [dbopen cntfile.db [expr $DB_CREATE | $DB_TRUNCATE] \
		0644 DB_BTREE]
	error_check_good dbopen:cntfile [is_valid_db $db] TRUE

	set flags 0
	set txn 0
	set count 0

	# Here is the loop where we put and get each key/data pair
	# We will add between 1 and 10 dups with values 1 ... dups
	# We'll verify each addition.

	set did [open $dict]
	puts "\tTest030.a: put and get duplicate keys."
	set dbc [$db cursor $txn]

	while { [gets $did str] != -1 && $count < $nentries } {
		set ndup [random_int 1 10]

		for { set i 1 } { $i <= $ndup } { incr i 1 } {
			set ret [$cntdb put $txn $str $ndup $flags]
			error_check_good put_cnt $ret 0
			set datastr $i:$str
			set ret [$db put $txn $str $datastr $flags]
			error_check_good put $ret 0
		}

		# Now retrieve all the keys matching this key
		set x 0
		for {set ret [$dbc get $str $DB_SET]} \
		    {[string length $ret] != 0} \
		    {set ret [$dbc get 0 $DB_NEXT_DUP] } {
			incr x
			set k [lindex $ret 0]
			if { [string compare $k $str] != 0 } {
				break
			}
			set datastr [lindex $ret 1]
			set d [data_of $datastr]

			if {[string length $d] == 0} {
				break
			}
			error_check_good Test030:put $d $str
			set id [ id_of $datastr ]
			error_check_good Test030:dup# $id $x
		}
		error_check_good Test030:numdups $x $ndup
		incr count
	}
	close $did

	# Verify on sequential pass of entire file
	puts "\tTest030.a: sequential check"

	set lastkey ""
	for {set ret [$dbc get 0 $DB_FIRST]} \
	    {[string length $ret] != 0} \
	    {set ret [$dbc get 0 $DB_NEXT] } {

	    	# Outer loop should always get a new key

		set k [lindex $ret 0]
		error_check_bad outer_get_loop:key $k $lastkey

		set datastr [lindex $ret 1]
		set d [data_of $datastr]
		set id [ id_of $datastr ]

		error_check_good outer_get_loop:data $d $k
		error_check_good outer_get_loop:id $id 1

		set lastkey $k
		# Figure out how may dups we should have
		set ndup [$cntdb get $txn $k $flags]

		set howmany 1
		for { set ret [$dbc get 0 $DB_NEXT_DUP] } \
		    { [string length $ret] != 0 } \
		    { set ret [$dbc get 0 $DB_NEXT_DUP] } {
			incr howmany

			set k [lindex $ret 0]
			error_check_good inner_get_loop:key $k $lastkey

			set datastr [lindex $ret 1]
			set d [data_of $datastr]
			set id [ id_of $datastr ]

			error_check_good inner_get_loop:data $d $k
			error_check_good inner_get_loop:id $id $howmany

		}
		error_check_good ndups_found $howmany $ndup
	}

	# Verify on key lookup
	puts "\tTest030.c: keyed check"
	set cnt_dbc [$cntdb cursor 0]
	for {set ret [$cnt_dbc get 0 $DB_FIRST]} \
	    {[string length $ret] != 0} \
	    {set ret [$cnt_dbc get 0 $DB_NEXT] } {
		set k [lindex $ret 0]
		error_check_bad cnt_seq:key [string length $k] 0

		set howmany [lindex $ret 1]
		error_check_bad cnt_seq:data [string length $howmany] 0

		set i 0

		for {set ret [$dbc get $k $DB_SET]} \
		    {[string length $ret] != 0} \
		    {set ret [$dbc get 0 $DB_NEXT_DUP] } {

			incr i

			set k [lindex $ret 0]
			error_check_bad keyed_loop:key [string length $k] 0

			set datastr [lindex $ret 1]
			set d [data_of $datastr]
			set id [ id_of $datastr ]

			error_check_good inner_get_loop:data $d $k
			error_check_good inner_get_loop:id $id $i
		}
		error_check_good keyed_count $i $howmany

	}
	error_check_good cnt_curs_close [$cnt_dbc close] 0
	error_check_good db_curs_close [$dbc close] 0
	error_check_good cnt_file_close [$cntdb close] 0
	error_check_good db_file_close [$db close] 0
}
