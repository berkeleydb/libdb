# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996, 1997, 1998
#	Sleepycat Software.  All rights reserved.
#
#	@(#)test042.tcl	10.13 (Sleepycat) 11/25/98
#
# DB Test 42 {access method}
# Multiprocess DB test; verify that locking is basically working
# for the concurrent access method product.
#
# Use the first "nentries" words from the dictionary.
# Insert each with self as key and a fixed, medium length data string.
# Then fire off multiple processes that bang on the database.  Each
# one should try to read and write random keys.  When they rewrite,
# they'll append their pid to the data string (sometimes doing a rewrite
# sometimes doing a partial put).  Some will use cursors to traverse
# through a few keys before finding one to write.

set datastr abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz

proc test042 { method {nentries 1000} args } {
global datastr
source ./include.tcl

	set method [convert_method $method]
	puts "Test042: CDB Test $method $nentries"

	# Parse options
	set iter 10000
	set procs 5
	set seeds {}
	set do_exit 0
	for { set i 0 } { $i < [llength $args] } {incr i} {
		switch -regexp -- [lindex $args $i] {
			-d.* { incr i; set testdir [lindex $args $i] }
			-i.* { incr i; set iter [lindex $args $i] }
			-p.* { incr i; set procs [lindex $args $i] }
			-s.* { incr i; set seeds [lindex $args $i] }
			-x.* { set do_exit 1 }
			default {
				test042_usage
				return
			}
		}
	}

	if { [file exists $testdir] != 1 } {
		exec $MKDIR $testdir
	} elseif { [file isdirectory $testdir ] != 1 } {
		error "FAIL: $testdir is not a directory"
	}

	# Create the database and open the dictionary
	set testfile test042.db
	set t1 $testdir/t1
	set t2 $testdir/t2
	set t3 $testdir/t3
	cleanup $testdir
	set db [eval [concat dbopen \
	    $testfile [expr $DB_CREATE | $DB_TRUNCATE] 0644 $method $args]]
	error_check_good dbopen [is_valid_db $db] TRUE
	set did [open $dict]

	set flags 0
	set txn 0
	set count 0

	# Here is the loop where we put each key/data pair
	puts "\tTest042.a: put/get loop"
	while { [gets $did str] != -1 && $count < $nentries } {
		if { [string compare $method DB_RECNO] == 0 } {
			set key [expr $count + 1]
			set put putn
		} else {
			set key $str
			set put put
		}
		set ret [$db $put $txn $key $datastr $flags]
		error_check_good put:$db $ret 0
		incr count
	}
	close $did
	error_check_good close:$db [$db close] 0

	# Database is created, now set up environment

	# Remove old mpools and Open/create the lock and mpool regions
	set ret [ lock_unlink $testdir 1 ]
#	error_check_good lock_unlink $ret 0
	set ret [ memp_unlink $testdir 1 ]
#	error_check_good memp_unlink $ret 0

	set env \
	    [dbenv -dbflags [expr $DB_CREATE | $DB_INIT_CDB | $DB_INIT_MPOOL]]
	error_check_good dbenv [is_valid_widget $env env] TRUE

	if { $do_exit == 1 } {
		return
	}

	# Now spawn off processes
	puts "\tTest042.b: forking off $procs children"
	set pidlist {}
	for { set i 0 } {$i < $procs} {incr i} {
		set s -1
		if { [llength $seeds] == $procs } {
			set s [lindex $seeds $i]
		}
		puts "exec ./dbtest ../test/mdbscript.tcl $method $testdir \
		    $testfile $nentries $iter $i $procs $s > \
		    $testdir/test042.$i.log &"
		set p [exec ./dbtest ../test/mdbscript.tcl $method $testdir \
		    $testfile $nentries $iter $i $procs $s > \
		    $testdir/test042.$i.log & ]
		lappend pidlist $p
	}
	puts "Test042: $procs independent processes now running"
	watch_procs $pidlist

	# Test is done, blow away lock and mpool region
	reset_env $env
	set ret [ lock_unlink $testdir 0 ]
#	error_check_good lock_unlink $ret 0
	set ret [ memp_unlink $testdir 0 ]
#	error_check_good memp_unlink $ret 0
}

proc test042_usage { } {
	puts -nonewline "test042 method nentries [-d directory] [-i iterations]"
	puts " [-p procs] [-s {seeds} ] -x"
}
