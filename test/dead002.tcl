# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996, 1997, 1998, 1999
#	Sleepycat Software.  All rights reserved.
#
#	@(#)dead002.tcl	11.5 (Sleepycat) 9/21/99
#
# Deadlock Test 2.
# Identical to Test 1 except that instead of running a standalone deadlock
# detector, we create the region with "detect on every wait"
proc dead002 { { procs "2 4 10" } {tests "ring clump" } } {
	source ./include.tcl

	puts "Dead002: Deadlock detector tests"

	cleanup $testdir

	# Create the environment.
	puts "\tDead002.a: creating environment"
	set env [berkdb env \
	    -create -mode 0644 -home $testdir -mpool -lock -lock_detect default]
	#	set lm [lock_open "" $DB_CREATE 0644 -detect 1]
	error_check_good lock_env:open [is_valid_env $env] TRUE
	error_check_good lock_env:close [$env close] 0

	foreach t $tests {
		set pidlist ""
		foreach n $procs {
			# Fire off the tests
			puts "\tDead002: $n procs of test $t"
			for { set i 0 } { $i < $n } { incr i } {
				puts "$tclsh_path\
				    $test_path/ddscript.tcl $testdir \
				    $t $i $i $n >& $testdir/dead002.log.$i"
				set p [exec $tclsh_path \
				    $test_path/ddscript.tcl $testdir $t $i \
				    $i $n >& $testdir/dead002.log.$i &]
				lappend pidlist $p
			}
			watch_procs $pidlist 5

			# Now check output
			set dead 0
			set clean 0
			set other 0
			for { set i 0 } { $i < $n } { incr i } {
				set did [open $testdir/dead002.log.$i]
				while { [gets $did val] != -1 } {
					switch $val {
						DEADLOCK { incr dead }
						1 { incr clean }
						default { incr other }
					}
				}
				close $did
			}
			dead_check $t $n $dead $clean $other
		}
	}

	exec $RM -f $testdir/dd.out
	# Remove log files
	for { set i 0 } { $i < $n } { incr i } {
		exec $RM -f $testdir/dead002.log.$i
	}
}
