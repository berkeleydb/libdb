# See the file LICENSE for redistribution information.
#
# Copyright (c) 2005, 2011 Oracle and/or its affiliates.  All rights reserved.
#
# $Id$
#
# TEST	test127
# TEST	Test database bulk update.
# TEST
# TEST	This is essentially test126 with duplicates.
# TEST	To make it simple we use numerical keys all the time.
# TEST
# TEST	Put with -multiple, then with -multiple_key,
# TEST	and make sure the items in database are what we want.
# TEST  Later, delete some items with -multiple, then with -multiple_key,
# TEST	and make sure if the correct items are deleted.

proc test127 {method { nentries 10000 } { ndups 5} { tnum "127" } args } {
	source ./include.tcl
	global alphabet

	if {[is_btree $method] != 1 && [is_hash $method] != 1} {
		puts "Skipping test$tnum for $method."
		return 
	}

	set save_args ""

	set subdb ""
	set subindex [lsearch -exact $args "-subdb"]
	if { $subindex != -1 } {
		if {[is_partitioned $args]} {
			puts "Skipping test$tnum with sub database\
		       	    for partitioned $method test."
			    return		
		}
		set subdb "subdb"
		set args [lreplace $args $subindex $subindex]
		set save_args "$save_args -subdb"
	}

	# If we are using an env, then testfile should just be the db name.
	# Otherwise it is the test directory and the name.
	set eindex [lsearch -exact $args "-env"]
	set txnenv 0
	set txn ""
	if { $eindex == -1 } {
		set testfile $testdir/test$tnum.db
		set env NULL
	} else {
		set testfile test$tnum.db
		incr eindex
		set env [lindex $args $eindex]
		set txnenv [is_txnenv $env]
		if { $txnenv == 1 } {
			append args " -auto_commit "
		}
		set testdir [get_home $env]
	}

	set args [convert_args $method $args]
	set omethod [convert_method $method]	

	cleanup $testdir $env

	puts "Test$tnum: $method ($save_args $args) Database bulk update with duplicates."

	set db [eval {berkdb_open_noerr -create -dup -dupsort -mode 0644} \
	    $args $omethod $testfile $subdb]
	error_check_good dbopen [is_valid_db $db] TRUE

	if { $txnenv == 1 } {
		set t [$env txn]
		error_check_good txn [is_valid_txn $t $env] TRUE
		set txn "-txn $t"
	}

	# Do bulk put.
	# First, we put half the entries using put -multiple.
	# Then, we put the rest half using put -multiple_key.

	puts "\tTest$tnum.a: Bulk put data using -multiple."
	set key_list1 {}
	set data_list1 {}
	for { set i 1 } { $i < $nentries / 2} { incr i } {
		for { set j 1 } { $j <= $ndups } { incr j } {
			set str $i.$j.$alphabet
			lappend key_list1 $i
			lappend data_list1 [chop_data $method $str]
		}
	}
	set ret [eval {$db put} $txn -multiple {$key_list1 $data_list1}]
	error_check_good {put(-multiple)} $ret 0

	# Put again without -overwritedup, should return DB_KEYEXIST.	
	set ret [catch {
	    eval {$db put} $txn -multiple {$key_list1 $data_list1}} res]
	error_check_good {Check DB_KEYEXIST} [is_substr $res DB_KEYEXIST] 1

	# Put again with -overwritedup, should succeed
	set ret [eval \
	    {$db put} $txn -multiple -overwritedup {$key_list1 $data_list1}]
	error_check_good {put_again(-multiple -overwritedup)} $ret 0

	puts "\tTest$tnum.b: Bulk put data using -multiple_key."
	set pair_list1 {}
	for { set i [expr $nentries / 2 ]} { $i <= $nentries} { incr i } {
		for { set j 1 } { $j <= $ndups } { incr j } {
			set str $i.$j.$alphabet
			lappend pair_list1 $i [chop_data $method $str]
		}
	}
	set ret [eval {$db put} $txn -multiple_key {$pair_list1}]
	error_check_good {put(-multiple_key)} $ret 0
	
	# Put again without -overwritedup, should return DB_KEYEXIST.	
	set ret [catch {
	    eval {$db put} $txn -multiple_key {$pair_list1}} res]
	error_check_good {Check DB_KEYEXIST} [is_substr $res DB_KEYEXIST] 1

	# Put again with -overwritedup, should succeed
	set ret [eval \
	    {$db put} $txn -multiple_key -overwritedup {$pair_list1}]
	error_check_good {put_again(-multiple_key -overwritedup)} $ret 0

	puts "\tTest$tnum.c: Verify the data after bulk put."
	set len [llength $pair_list1]
	for {set indx1 0; set indx2 1} {$indx2 < $len} \
	    {incr indx1 2; incr indx2 2} {
		lappend key_list1 [lindex $pair_list1 $indx1]
		lappend data_list1 [lindex $pair_list1 $indx2]
	}

	# Check if the data items are what we want to put.
	set dbc [eval $db cursor $txn]
	error_check_good $dbc [is_valid_cursor $dbc $db] TRUE
	for {set pair [$dbc get -first]} {[llength $pair] > 0} \
	    {set pair [$dbc get -next]} {
		set key [lindex [lindex $pair 0] 0]
		set data [lindex [lindex $pair 0] 1]		
		set index [lsearch -exact $data_list1 $data]
		error_check_bad data_index $index -1
		error_check_good key $key [lindex $key_list1 $index]
	}
	
	# Check if all the items we want to put are in the database.
	set len [llength $key_list1]
	for {set i 0} {$i < $len} {incr i} {
		set pair [$dbc get -get_both [lindex $key_list1 $i] \
		    [lindex $data_list1 $i]]
		error_check_bad pair [llength $pair] 0
	}

	error_check_good $dbc.close [$dbc close] 0

	puts "\tTest$tnum.d: Bulk delete data using -multiple."
	set key_list2 {}
	for { set i 1 } { $i <= $nentries} { incr i 2 } {
		lappend key_list2 $i
	}
	set ret [eval {$db del} $txn -multiple {$key_list2}]
	error_check_good {del(-multiple)} $ret 0

	# Delete again, should return DB_NOTFOUND.
	set ret [catch {eval {$db del} $txn -multiple {$key_list2}} res]
	error_check_good {Check DB_NOTFOUND} [is_substr $res DB_NOTFOUND] 1

	puts "\tTest$tnum.e: Bulk delete using -multiple_key."
	set pair_list2 {}
	for { set i 2 } { $i <= $nentries} { incr i  2} {
		for { set j 1 } { $j <= $ndups / 2 } { incr j } {
			set str $i.$j.$alphabet
			lappend pair_list2 $i [chop_data $method $str]
		}
	}

	set ret [eval {$db del} $txn -multiple_key {$pair_list2}]
	error_check_good {del(-multiple_key)} $ret 0

	# Delete again, should return DB_NOTFOUND.
	set ret [catch {eval {$db del} $txn -multiple_key {$pair_list2}} res]
	error_check_good {Check DB_NOTFOUND} [is_substr $res DB_NOTFOUND] 1

	puts "\tTest$tnum.f: Verify the data after bulk delete."	

	# Check if the specified items are deleted
	set dbc [eval $db cursor $txn]
	error_check_good $dbc [is_valid_cursor $dbc $db] TRUE
	set len [llength $key_list2]
	for {set i 0} {$i < $len} {incr i} {
		set key [lindex $key_list2 $i]
		set pair [$dbc get -set $key]
		error_check_good pair [llength $pair] 0
	}

	set len [llength $pair_list2]	
	for {set indx1 0; set indx2 1} {$indx2 < $len} \
	    {incr indx1 2; incr indx2 2} {
		set key [lindex $pair_list2 $indx1]
		set data [lindex $pair_list2 $indx2]
		set pair [$dbc get -get_both $key $data]
		error_check_good pair [llength $pair] 0
	}

	# Check all items to mare sure we do not delete other items.
	for {set pair [$dbc get -first]} {[llength $pair] > 0} \
	    {set pair [$dbc get -next]} {
		set key [lindex [lindex $pair 0] 0]
		set data [lindex [lindex $pair 0] 1]		
		set index [lsearch -exact $data_list1 $data]
		error_check_bad data_index $index -1
		error_check_good key $key [lindex $key_list1 $index]
	}
	
	set len [llength $key_list1]
	for {set i 0} {$i < $len} {incr i} {
		set key  [lindex $key_list1 $i]
		set data [lindex $data_list1 $i]
		if {[lsearch -exact $key_list2 $key] >= 0} {
			continue
		}
		if {[lsearch -exact $pair_list2 $data] >= 0} {
			continue
		}
		set pair [$dbc get -get_both $key $data]
		error_check_bad pair [llength $pair] 0
	}

	error_check_good $dbc.close [$dbc close] 0	

	if { $txnenv == 1 } {
		error_check_good txn_commit [$t commit] 0
	}
	error_check_good db_close [$db close] 0
}

