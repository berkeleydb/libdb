# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996, 1997, 1998, 1999
#	Sleepycat Software.  All rights reserved.
#
#	@(#)test006.tcl	11.4 (Sleepycat) 8/17/99
#
# DB Test 6 {access method}
# Keyed delete test.
# Create database.
# Go through database, deleting all entries by key.
proc test006 { method {nentries 10000} {reopen 6} args} {
	source ./include.tcl

	set do_renumber [is_rrecno $method]
	set args [convert_args $method $args]
	set omethod [convert_method $method]

	set tnum Test00$reopen
	puts -nonewline "$tnum: \
	    $method ($args) $nentries equal small key; medium data pairs"
	if {$reopen == 7} {
		puts "(with close)"
	} else {
		puts ""
	}

	# Create the database and open the dictionary
	set testfile $testdir/$tnum.db

	set pflags ""
	set gflags ""
	set txn ""
	set count 0
	if { [is_record_based $method] == 1 } {
	   append gflags " -recno"
	}

	# Here is the loop where we put and get each key/data pair

	cleanup $testdir
	set db [eval {berkdb \
	    open -create -truncate -mode 0644} $args {$omethod $testfile}]
	error_check_good dbopen [is_valid_db $db] TRUE

	set did [open $dict]
	while { [gets $did str] != -1 && $count < $nentries } {
		if { [is_record_based $method] == 1 } {
			set key [expr $count + 1 ]
		} else {
			set key $str
		}

		set datastr [make_data_str $str]

		set ret [eval {$db put} \
		    $txn $pflags {$key [chop_data $method $datastr]}]
		error_check_good put $ret 0

		set ret [eval {$db get} $gflags {$key}]
		error_check_good "$tnum: put $datastr got $ret" \
		    $ret [list [list $key [pad_data $method $datastr]]]
		incr count
	}
	close $did

	if { $reopen == 7 } {
		error_check_good db_close [$db close] 0

		set db [eval {berkdb open} {$testfile}]
		error_check_good dbopen [is_valid_db $db] TRUE
	}

	# Now we will get each key from the DB and compare the results
	# to the original, then delete it.
	set count 0
	set did [open $dict]
	set key 0
	while { [gets $did str] != -1 && $count < $nentries } {
		if { $do_renumber == 1 } {
			set key 1
		} elseif { [is_record_based $method] == 1 } {
			incr key
		} else {
			set key $str
		}

		set datastr [make_data_str $str]

		set ret [eval {$db get} $gflags {$key}]
		error_check_good "$tnum: get $datastr got $ret" \
		    $ret [list [list $key [pad_data $method $datastr]]]

		set ret [eval {$db del} $txn {$key}]
		error_check_good db_del:$key $ret 0
		incr count
	}
	close $did

	error_check_good db_close [$db close] 0
}
