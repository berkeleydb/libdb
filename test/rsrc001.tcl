# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996, 1997, 1998, 1999
#	Sleepycat Software.  All rights reserved.
#
#	@(#)rsrc001.tcl	11.7 (Sleepycat) 9/8/99
#
# Recno backing file test.
# Try different patterns of adding records and making sure that the
# corresponding file matches
proc rsrc001 { } {
	source ./include.tcl

	puts "Rsrc001: Basic recno backing file writeback tests"

	# We run this test essentially twice, once with a db file
	# and once without (an in-memory database).
	foreach testfile { "$testdir/rsrc001.db" "" } {

		cleanup $testdir

		# Create the starting files
		set oid1 [open $testdir/rsrc.txt w]
		set oid2 [open $testdir/check.txt w]
		puts $oid1 "This is record 1"
		puts $oid2 "This is record 1"
		puts $oid1 "This is record 2 This is record 2"
		puts $oid2 "This is record 2 This is record 2"
		puts $oid1 "This is record 3 This is record 3 This is record 3"
		puts $oid2 "This is record 3 This is record 3 This is record 3"
		close $oid1
		close $oid2

		sanitize_textfile $testdir/rsrc.txt
		sanitize_textfile $testdir/check.txt

		if { $testfile == "" } {
			puts "Rsrc001: Testing with in-memory database."
		} else {
			puts "Rsrc001: Testing with disk-backed database."
		}

		puts -nonewline "\tRsrc001.a: Read file, rewrite last record;"
		puts " write it out and diff"
		set db [eval {berkdb open -create -mode 0644\
		    -recno -source $testdir/rsrc.txt} $testfile]
		error_check_good dbopen [is_valid_db $db] TRUE

		# Read the last record; replace it (but we won't change it).
		# Then close the file and diff the two files.
		set txn ""
		set dbc [eval {$db cursor} $txn]
		error_check_good db_cursor [is_valid_cursor $dbc $db] TRUE

		set rec [$dbc get -last]
		error_check_good get_last [llength [lindex $rec 0]] 2
		set key [lindex [lindex $rec 0] 0]
		set data [lindex [lindex $rec 0] 1]

		# Get the last record from the text file
		set oid [open $testdir/rsrc.txt]
		set laststr ""
		while { [gets $oid str] != -1 } {
			set laststr $str
		}
		close $oid
		error_check_good getlast $data $laststr

		set ret [eval {$db put} $txn {$key $data}]
		error_check_good replace_last $ret 0

		error_check_good curs_close [$dbc close] 0
		error_check_good db_sync [$db sync] 0
		error_check_good db_sync [$db sync] 0
		error_check_good \
		    Rsrc001:diff($testdir/rsrc.txt,$testdir/check.txt) \
		    [catch { exec $CMP $testdir/rsrc.txt \
		    $testdir/check.txt } res] 0

		puts -nonewline "\tRsrc001.b: "
		puts "Append some records in tree and verify in file."
		set oid [open $testdir/check.txt a]
		for {set i 1} {$i < 10} {incr i} {
			set rec [replicate "New Record $i" $i]
			puts $oid $rec
			incr key
			set ret [eval {$db put} $txn {-append $rec}]
			error_check_good put_append $ret $key
		}
		error_check_good db_sync [$db sync] 0
		error_check_good db_sync [$db sync] 0
		close $oid
		sanitize_textfile $testdir/check.txt
		set ret [catch { exec $CMP $testdir/rsrc.txt \
		    $testdir/check.txt } res]
		error_check_good \
		    Rsrc001:diff($testdir/{rsrc.txt,check.txt}) $ret 0

		puts "\tRsrc001.c: Append by record number"
		set oid [open $testdir/check.txt a]
		for {set i 1} {$i < 10} {incr i} {
			set rec [replicate "New Record (set 2) $i" $i]
			puts $oid $rec
			incr key
			set ret [eval {$db put} $txn {$key $rec}]
			error_check_good put_byno $ret 0
		}

		error_check_good db_sync [$db sync] 0
		error_check_good db_sync [$db sync] 0
		close $oid
		sanitize_textfile $testdir/check.txt
		set ret [catch { exec $CMP $testdir/rsrc.txt \
		    $testdir/check.txt } res]
		error_check_good \
		    Rsrc001:diff($testdir/{rsrc.txt,check.txt}) $ret 0

		puts "\tRsrc001.d: Put beyond end of file."
		set oid [open $testdir/check.txt a]
		for {set i 1} {$i < 10} {incr i} {
			puts $oid ""
			incr key
		}
		set rec "Last Record"
		puts $oid $rec
		incr key
		set ret [eval {$db put} $txn {$key $rec}]
		error_check_good put_byno $ret 0

		error_check_good db_sync [$db sync] 0
		error_check_good db_sync [$db sync] 0
		close $oid
		sanitize_textfile $testdir/check.txt
		set ret [catch { exec $CMP $testdir/rsrc.txt \
		    $testdir/check.txt } res]
		error_check_good \
		    Rsrc001:diff($testdir/{rsrc.txt,check.txt}) $ret 0

		puts "\tRsrc001.e: Verify proper syncing of changes on close."
		error_check_good Rsrc001:db_close [$db close] 0
		set db [eval {berkdb open -create -mode 0644 -recno \
		    -source $testdir/rsrc.txt} $testfile]
		set oid [open $testdir/check.txt a]
		for {set i 1} {$i < 10} {incr i} {
			set rec [replicate "New Record $i" $i]
			puts $oid $rec
			set ret [eval {$db put} $txn {-append $rec}]
			# Don't bother checking return;  we don't know what
			# the key number is, and we'll pick up a failure
			# when we compare.
		}
		error_check_good Rsrc001:db_close [$db close] 0
		close $oid
		sanitize_textfile $testdir/check.txt
		set ret [catch { exec $CMP $testdir/rsrc.txt \
		    $testdir/check.txt } res]
		error_check_good Rsrc001:diff($testdir/{rsrc,check}.txt) $ret 0
	}
}

# convert CR/LF to just LF.
# Needed on Windows when a file is created as text but read as binary.
proc sanitize_textfile { filename } {
	source ./include.tcl

	if { $is_windows_test == 1 } {
		catch { exec $TR -d '\015' <$filename > $testdir/nonl.tmp } res
		catch { exec $MV $testdir/nonl.tmp $filename } res
	}
}
