# See the file LICENSE for redistribution information.
#
# Copyright (c) 2005, 2011 Oracle and/or its affiliates.  All rights reserved.
#
# $Id$
#
# TEST	test126
# TEST	Test database bulk update for non-duplicate databases.
# TEST
# TEST	Put with -multiple, then with -multiple_key,
# TEST	and make sure the items in database are what we put.
# TEST  Later, delete some items with -multiple, then with -multiple_key,
# TEST	and make sure if the correct items are deleted.

proc test126 {method { nentries 10000 } { tnum "126" } args } {
	source ./include.tcl

	# For rrecno, when keys are deleted, the ones after will move forward,
	# and the keys change, which is not good to verify after delete.
	# So, we skip rrecno temporarily.
	if {[is_rrecno $method]} {
		puts "Skipping test$tnum for $method test."
		return
	}

	set save_args ""

	# Check if we use sub database.
	set subdb ""
	set subindex [lsearch -exact $args "-subdb"]
	if { $subindex != -1 } {
		if {[is_queue $method]} {
			puts "Skipping test$tnum with sub database for $method."
			    return
		}
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

	# Check if we use secondary database.
	set secindex [lsearch -exact $args "-secondary"]
	if { $secindex != -1} {
		set args [lreplace $args $secindex $secindex]
		set sec_args $args
		set save_args "$save_args -secondary"
	}

	set args [convert_args $method $args]
	set omethod [convert_method $method]

	cleanup $testdir $env

	puts "Test$tnum: $method ($save_args $args) Database bulk update."

	set db [eval {berkdb_open_noerr -create -mode 0644} \
	    $args $omethod $testfile $subdb]
	error_check_good dbopen [is_valid_db $db] TRUE

	# Open the secondary database and do association. 
	# This is the test for [#18878].
	if { $secindex != -1 } {
		if { $subindex != -1 } {
			set sec_subdb "subdb-secondary"
			set sec_testfile $testfile
		} else {
			set sec_subdb ""
			if { $eindex == -1 } {
				set sec_testfile $testdir/test$tnum-secondary.db
			} else {
				set sec_testfile test$tnum-secondary.db
			}
		}
		# Open a simple dupsort btree database.
		# In order to be consistent, we need to use all the passed-in 
		# am-unrelated flags.
		set sec_db [eval {berkdb_open_noerr -create -mode 0644} $sec_args \
		    -dup -dupsort -btree $sec_testfile $sec_subdb]
		error_check_good secdb_open [is_valid_db $sec_db] TRUE
		set ret [$db associate -create [callback_n 4] $sec_db]
		error_check_good db_associate $ret 0
	}
	
	if { $txnenv == 1 } {
		set t [$env txn]
		error_check_good txn [is_valid_txn $t $env] TRUE
		set txn "-txn $t"
	}

	set did [open $dict]
	set count 0

	
	# Do bulk put.
	# First, we put half the entries using put -multiple.
	# Then, we put the rest half using put -multiple_key.

	puts "\tTest$tnum.a: Bulk put data using -multiple."
	set key_list1 {}
	set data_list1 {}
	while { [gets $did str] != -1 && $count < $nentries / 2 } {
		if { [is_record_based $method] == 1 } {
			set key [expr $count + 1]
		} else {
			set key $str
			set str [reverse $str]
		}
		lappend key_list1 $key
		lappend data_list1 [make_fixed_length $method $str]
		incr count
	}

	set ret [eval {$db put} $txn -multiple {$key_list1 $data_list1}]
	error_check_good {put(-multiple)} $ret 0

	# Put again, should succeed
	set ret [eval {$db put} $txn -multiple  {$key_list1 $data_list1}]
	error_check_good {put_again(-multiple)} $ret 0

	puts "\tTest$tnum.b: Bulk put data using -multiple_key."
	set pair_list1 {}
	while { [gets $did str] != -1 && $count < $nentries } {
		if { [is_record_based $method] == 1 } {
			set key [expr $count + 1]
		} else {
			set key $str
			set str [reverse $str]
		}
		lappend pair_list1 $key [make_fixed_length $method $str]
		incr count	
	}

	set ret [eval {$db put} $txn -multiple_key {$pair_list1}]
	error_check_good {put(-multiple_key)} $ret 0	
	
	# Put again, should succeed
	set ret [eval {$db put} $txn -multiple_key {$pair_list1}]
	error_check_good {put_again(-multiple_key)} $ret 0

	puts "\tTest$tnum.c: Verify the data after bulk put."
	set len [llength $pair_list1]
	for {set indx1 0; set indx2 1} {$indx2 < $len} \
	    {incr indx1 2; incr indx2 2} {
		lappend key_list1 [lindex $pair_list1 $indx1]
		lappend data_list1 [lindex $pair_list1 $indx2]
	}

	# Check if the data items are correct.
	set dbc [eval $db cursor $txn]
	error_check_good $dbc [is_valid_cursor $dbc $db] TRUE
	for {set pair [$dbc get -first]} {[llength $pair] > 0} \
	    {set pair [$dbc get -next]} {
		set key [lindex [lindex $pair 0] 0]
		set data [lindex [lindex $pair 0] 1]		
		set index [lsearch -exact $key_list1 $key]
		error_check_bad key_index $index -1
		error_check_good data $data [lindex $data_list1 $index]
	}
	
	# Check if all the items we want to put are in the database.
	set len [llength $key_list1]
	for {set i 0} {$i < $len} {incr i} {
		set pair [$dbc get -get_both [lindex $key_list1 $i] \
		    [lindex $data_list1 $i]]
		error_check_bad pair [llength $pair] 0
	}

	error_check_good $dbc.close [$dbc close] 0
	close $did

	puts "\tTest$tnum.d: Bulk delete data using -multiple."
	set key_list2 {}
	for { set i 0 } { $i < $nentries} { incr i 3 } {
		lappend key_list2 [lindex $key_list1 $i]
	}
	set ret [eval {$db del} $txn -multiple {$key_list2}]
	error_check_good {del(-multiple)} $ret 0

	# Delete again, should return DB_NOTFOUND/DB_KEYEMPTY.
	set ret [catch {eval {$db del} $txn -multiple {$key_list2}} res]
	error_check_good {Check DB_NOTFOUND/DB_KEYEMPTY} \
	    [expr [is_substr $res DB_NOTFOUND] || \
	    [is_substr $res DB_KEYEMPTY]] 1

	puts "\tTest$tnum.e: Bulk delete data using -multiple_key."
	set pair_list2 {}
	for { set i 1 } { $i < $nentries} { incr i 3} {
		lappend pair_list2 [lindex $key_list1 $i] \
		    [lindex $data_list1 $i]
	}

	set ret [eval {$db del} $txn -multiple_key {$pair_list2}]
	error_check_good {del(-multiple_key)} $ret 0

	# Delete again, should return DB_NOTFOUND/DB_KEYEMPTY.
	set ret [catch {eval {$db del} $txn -multiple_key {$pair_list2}} res]
	error_check_good {Check DB_NOTFOUND/DB_KEYEMPTY} \
	    [expr [is_substr $res DB_NOTFOUND] || \
	    [is_substr $res DB_KEYEMPTY]] 1


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
		lappend key_list2 $key
		lappend data_list2 $data
		set pair [$dbc get -get_both $key $data]
		error_check_good pair [llength $pair] 0
	}

	# Check all items to make sure we did not delete other items.
	for {set pair [$dbc get -first]} {[llength $pair] > 0} \
	    {set pair [$dbc get -next]} {
		set key [lindex [lindex $pair 0] 0]
		set data [lindex [lindex $pair 0] 1]		
		set index [lsearch -exact $key_list1 $key]
		error_check_bad key_index $index -1
		error_check_good data $data [lindex $data_list1 $index]
	}
	
	set len [llength $key_list1]
	for {set i 0} {$i < $len} {incr i} {
		set key  [lindex $key_list1 $i]
		set data [lindex $data_list1 $i]
		if {[lsearch -exact $key_list2 $key] >= 0} {
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
	if {$secindex != -1} {
		error_check_good secdb_close [$sec_db close] 0
	}
}

