# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996, 1997, 1998, 1999
#	Sleepycat Software.  All rights reserved.
#
#	@(#)test045.tcl	11.7 (Sleepycat) 10/25/99
#
# DB Test 45 Run the random db tester on the specified access method.
# Options are:
#	-adds <maximum number of keys before you disable adds>
#	-cursors <number of cursors>
#	-dataavg <average data size>
#	-delete <minimum number of keys before you disable deletes>
#	-dups <allow duplicates in file>
#	-errpct <Induce errors errpct of the time>
#	-init <initial number of entries in database>
#	-keyavg <average key size>
proc test045 { method {nops 10000} args } {
	source ./include.tcl

	set args [convert_args $method $args]
	set omethod [convert_method $method]

	if { [is_record_based $method] == 1 } {
		puts "Test045: skipping for method $method"
		return
	}

	puts "Test045: Random tester on $method for $nops operations"

	# Set initial parameters
	set adds 100000
	set cursors 5
	set dataavg 40
	set delete 10000
	set dups 0
	set errpct 0
	set init 0
	set keyavg 25

	# Process arguments
	set oargs ""
	for { set i 0 } { $i < [llength $args] } {incr i} {
		switch -regexp -- [lindex $args $i] {
			-adds	 { incr i; set adds [lindex $args $i] }
			-cursors { incr i; set cursors [lindex $args $i] }
			-dataavg { incr i; set dataavg [lindex $args $i] }
			-delete	 { incr i; set delete [lindex $args $i] }
			-dups	 { incr i; set dups [lindex $args $i] }
			-errpct	 { incr i; set errpct [lindex $args $i] }
			-init	 { incr i; set init [lindex $args $i] }
			-keyavg	 { incr i; set keyavg [lindex $args $i] }
			default	 { append oargs [lindex $args $i] }
		}
	}

	# Create the database and and initialize it.
	set root $testdir/test045
	set f $root.db
	cleanup $testdir

	# Run the script with 3 times the number of initial elements to
	# set it up.
	set db [eval {berkdb \
	    open -create -truncate -mode 0644 $omethod} $oargs {$f}]
	error_check_good dbopen:$f [is_valid_db $db] TRUE

	set r [$db close]
	error_check_good dbclose:$f $r 0

	# We redirect standard out, but leave standard error here so we
	# can see errors.

	puts "\tTest045.a: Initializing database"
	if { $init != 0 } {
		set n [expr 3 * $init]
		exec $tclsh_path \
		    $test_path/dbscript.tcl $f $n \
		    1 $init $n $keyavg $dataavg $dups 0 -1 \
		    > $testdir/test045.init
	}

	puts "\tTest045.b: Now firing off berkdb rand dbscript, running: "
	# Now the database is initialized, run a test
	puts "$tclsh_path\
	    $test_path/dbscript.tcl $f $nops $cursors $delete $adds \
	    $keyavg $dataavg $dups $errpct > $testdir/test045.log"

	exec $tclsh_path \
	    $test_path/dbscript.tcl $f \
	    $nops $cursors $delete $adds $keyavg \
	    $dataavg $dups $errpct \
	    > $testdir/test045.log
}
