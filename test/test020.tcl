# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996, 1997
#	Sleepycat Software.  All rights reserved.
#
#	@(#)test020.tcl	10.4 (Sleepycat) 8/22/97
#
# DB Test 20 {access method}
# Test in-memory databases.
proc test020 { method {nentries 10000} args } {
	set args [convert_args $method $args]
	set method [convert_method $method]
	puts "Test020: $method ($args) $nentries equal key/data pairs"

	# Get global declarations since tcl doesn't support
	# any useful equivalent to #defines!
	source ./include.tcl

	# Create the database and open the dictionary
	set t1 $testdir/t1
	set t2 $testdir/t2
	set t3 $testdir/t3
	cleanup $testdir
	set db [eval [concat dbopen NULL \
	    [expr $DB_CREATE | $DB_TRUNCATE] 0644 $method $args]]
	set did [open $dict]

	set flags 0
	set txn 0
	set count 0

	if { [string compare $method DB_RECNO] == 0 } {
		set checkfunc test020_recno.check
		set put putn
	} else {
		set checkfunc test020.check
		set put put
	}
	puts "\tTest020.a: put/get loop"
	# Here is the loop where we put and get each key/data pair
	while { [gets $did str] != -1 && $count < $nentries } {
		if { [string compare $method DB_RECNO] == 0 } {
			global kvals

			set key [expr $count + 1]
			set kvals($key) $str
		} else {
			set key $str
		}
		$db $put $txn $key $str $flags
		set ret [$db get $txn $key $flags]
		if { [string compare $ret $str] != 0 } {
			puts "Test020: put $key got $ret"
			return
		}
		incr count
	}
	close $did
	# Now we will get each key from the DB and compare the results
	# to the original.
	puts "\tTest020.b: dump file"
	dump_file $db $txn $t1 $checkfunc
	error_check_good db_close [$db close] 0

	# Now compare the keys to see if they match the dictionary (or ints)
	if { [string compare $method DB_RECNO] == 0 } {
		set oid [open $t2 w]
		for {set i 1} {$i <= $nentries} {set i [incr i]} {
			puts $oid $i
		}
		close $oid
		exec $MV $t1 $t3
	} else {
		set q q
		exec $SED $nentries$q $dict > $t2
		exec $SORT $t1 > $t3
	}

	error_check_good Test020:diff($t3,$t2) \
	    [catch { exec $DIFF $t3 $t2 } res] 0
}

# Check function for test020; keys and data are identical
proc test020.check { key data } {
	if { [string compare $key $data] != 0 } {
		error "Test020: key/data mismatch: |$key| |$data|"
	}
}

proc test020_recno.check { key data } {
global dict
global kvals
	error_check_good key"$key"_exists [info exists kvals($key)] 1
	if { [string compare $kvals($key) $data] != 0 } {
		error "Test020: data mismatch: key $key |$kvals($key)| |$data|"
	}
}
