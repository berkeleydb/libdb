# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996, 1997, 1998, 1999
#	Sleepycat Software.  All rights reserved.
#
#	@(#)lock003.tcl	11.8 (Sleepycat) 10/25/99
#
# Exercise multi-process aspects of lock.  Generate a bunch of parallel
# testers that try to randomly obtain locks.
proc lock003 { dir {iter 500} {max 1000} {procs 5} {ldegree 5} {objs 75} \
        {reads 65} {wait 1} {conflicts { 3 0 0 0 0 0 1 0 1 1}} {seeds {}} } {
        source ./include.tcl

        puts "Lock003: Multi-process random lock test"

        # Clean up after previous runs
	cleanup $dir

	# Open/create the lock region
        set e [berkdb env -create -lock -mpool -home $dir]
        error_check_good env_open [is_substr $e env] 1

        set ret [$e close]
        error_check_good env_close $ret 0

	# Now spawn off processes
	set pidlist {}
	for { set i 0 } {$i < $procs} {incr i} {
		if { [llength $seeds] == $procs } {
			set s [lindex $seeds $i]
		}
		puts "$tclsh_path\
		    $test_path/lockscript.tcl\
		    $dir $iter $objs $wait $ldegree $reads \
		    > $dir/$i.lockout &"
                set p [exec $tclsh_path $test_path/lockscript.tcl \
		    $dir $iter $objs $wait $ldegree $reads \
                    > $testdir/lock003.$i.out &]
                lappend pidlist $p
	}

	puts "Lock003: $procs independent processes now running"
	watch_procs $pidlist 30 10800
	# Remove log files
	for { set i 0 } {$i < $procs} {incr i} {
		exec $RM -f $dir/$i.lockout
	}
}
