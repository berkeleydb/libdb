# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996, 1997, 1998, 1999
#	Sleepycat Software.  All rights reserved.
#
#	@(#)dbm.tcl	11.5 (Sleepycat) 9/24/99
#
# Historic DBM interface test.
# Use the first 1000 entries from the dictionary.
# Insert each with self as key and data; retrieve each.
# After all are entered, retrieve all; compare output to original.
# Then reopen the file, re-retrieve everything.
# Finally, delete everything.
proc dbm { { nentries 1000 } } {
	source ./include.tcl

	puts "DBM interfaces test: $nentries"

	# Create the database and open the dictionary
	set testfile $testdir/dbmtest
	set t1 $testdir/t1
	set t2 $testdir/t2
	set t3 $testdir/t3
	cleanup $testdir

	error_check_good dbminit [berkdb dbminit $testfile] 0
	set did [open $dict]

	set flags ""
	set txn ""
	set count 0

	puts "\tDBM.a: put/get loop"
	# Here is the loop where we put and get each key/data pair
	while { [gets $did str] != -1 && $count < $nentries } {
		set ret [berkdb store $str $str]
		error_check_good dbm_store $ret 0

		set d [berkdb fetch $str]
		error_check_good dbm_fetch $d $str
		incr count
	}
	close $did

	# Now we will get each key from the DB and compare the results
	# to the original.
	puts "\tDBM.b: dump file"
	set oid [open $t1 w]
	for { set key [berkdb firstkey] } { $key != -1 } {\
		 set key [berkdb nextkey $key] } {
		puts $oid $key
		set d [berkdb fetch $key]
		error_check_good dbm_refetch $d $key
	}
	close $oid

	# Now compare the keys to see if they match the dictionary (or ints)
	set q q
	exec $SED $nentries$q $dict > $t3
	exec $SORT $t3 > $t2
	exec $SORT $t1 > $t3

	error_check_good DBM:diff($t3,$t2) \
	    [catch { exec $CMP $t3 $t2 } res] 0

	puts "\tDBM.c: close, open, and dump file"

	# Now, reopen the file and run the last test again.
	error_check_good dbminit2 [berkdb dbminit $testfile] 0
	set oid [open $t1 w]

	for { set key [berkdb firstkey] } { $key != -1 } {\
		 set key [berkdb nextkey $key] } {
		puts $oid $key
		set d [berkdb fetch $key]
		error_check_good dbm_refetch $d $key
	}
	close $oid

	# Now compare the keys to see if they match the dictionary (or ints)
	exec $SORT $t1 > $t3

	error_check_good DBM:diff($t2,$t3) \
	    [catch { exec $CMP $t2 $t3 } res] 0

	# Now, reopen the file and delete each entry
	puts "\tDBM.d: sequential scan and delete"

	error_check_good dbminit3 [berkdb dbminit $testfile] 0
	set oid [open $t1 w]

	for { set key [berkdb firstkey] } { $key != -1 } {\
		 set key [berkdb nextkey $key] } {
		puts $oid $key
		set ret [berkdb delete $key]
		error_check_good dbm_delete $ret 0
	}
	close $oid

	# Now compare the keys to see if they match the dictionary (or ints)
	exec $SORT $t1 > $t3

	error_check_good DBM:diff($t2,$t3) \
	    [catch { exec $CMP $t2 $t3 } res] 0

	error_check_good "dbm_close" [berkdb dbmclose] 0
}
