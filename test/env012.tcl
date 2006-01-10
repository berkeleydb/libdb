# See the file LICENSE for redistribution information.
#
# Copyright (c) 2004-2005
#       Sleepycat Software.  All rights reserved.
#
# $Id: env012.tcl,v 12.6 2005/11/02 15:11:04 carol Exp $
#
# TEST	env012
# TEST	Test DB_REGISTER.
# TEST
# TEST	DB_REGISTER will fail on systems without fcntl.  If it
# TEST	fails, make sure we got the expected DB_OPNOTSUP return.
# TEST
# TEST	Then, the real tests:
# TEST	1. 	Process 1 enters with -register -recover.
# TEST		Process 2 enters successfully with just -register. 
# TEST	
# TEST  2. 	Process 1 enters with -register -recover.
# TEST		Process 1 is killed.
# TEST		Process 2 enters successfully with -register -recover.
# TEST
# TEST	3. 	Process 1 enters with -register -recover.
# TEST		Process 1 is killed.
# TEST		Process 2 fails to enter with -register.
# TEST	
# TEST	4.  	Process 1 enters with -register -recover.
# TEST		Process 2 enters with -register.
# TEST		Process 1 is killed.
# TEST		Process 3 enters with -register -recover.
# TEST		Process 2 fails with DB_RUNRECOVERY.
# TEST
proc env012 { } {
	source ./include.tcl
	set tnum "012"

	puts "Env$tnum: Test of DB_REGISTER."

	puts "\tEnv$tnum.a: Platforms without fcntl fail with DB_OPNOTSUP."
	env_cleanup $testdir
	if {[catch {eval {berkdb_env} \
	    -create -home $testdir -txn -register -recover} env]} {
		error_check_good fail_OPNOTSUP [is_substr $env DB_OPNOTSUP] 1
		puts "Skipping env$tnum; DB_REGISTER is not supported."
	}
	error_check_good env_close [$env close] 0
		    
	puts "\tEnv$tnum.b: Second process can join with -register."
	env_cleanup $testdir
	set testfile TESTFILE
	set key KEY
	set data DATA1

	puts "\t\tEnv$tnum.b1: Start process 1."
	set p1 [exec $tclsh_path $test_path/wrap.tcl envscript.tcl \
	    $testdir/env$tnum.log.p1 \
	    $testdir $testfile PUT $key $data RECOVER 10 &]

	# Wait a while so process 1 has a chance to get going.
	tclsleep 2

	puts "\t\tEnv$tnum.b2: Start process 2."
	set p2 [exec $tclsh_path $test_path/wrap.tcl envscript.tcl \
	    $testdir/env$tnum.log.p2 \
	    $testdir $testfile GET $key $data 0 0 &]

	watch_procs $p1 1 120
	watch_procs $p2 1 120

	# Check log files for failures.
	logcheck $testdir/env$tnum.log.p1
	logcheck $testdir/env$tnum.log.p2

	puts "\tEnv$tnum.c: Second process can join with -register\
	    -recover after first process is killed."
	env_cleanup $testdir
	
	puts "\t\tEnv$tnum.c1: Start process 1."
	set pids {}
	set p1 [exec $tclsh_path $test_path/wrap.tcl envscript.tcl \
	    $testdir/env$tnum.log.p1 \
	    $testdir $testfile PUT $key $data RECOVER 10 &]
	lappend pids $p1
	tclsleep 2

	puts "\t\tEnv$tnum.c2: Kill process 1."
	set pids [findprocessids $testdir $pids]
	foreach pid $pids {
		tclkill $pid 
	}

	puts "\t\tEnv$tnum.c3: Start process 2."
	set p2 [exec $tclsh_path $test_path/wrap.tcl envscript.tcl \
	    $testdir/env$tnum.log.p2 \
	    $testdir $testfile GET $key $data RECOVER 0 &]

	watch_procs $p2 1 120

	# Check log files for failures.
	logcheck $testdir/env$tnum.log.p1
	logcheck $testdir/env$tnum.log.p2

	puts "\tEnv$tnum.d: Second process cannot join without -recover\
	    after first process is killed."
	env_cleanup $testdir
	
	puts "\t\tEnv$tnum.d1: Start process 1."
	set pids {}
	set p1 [exec $tclsh_path $test_path/wrap.tcl envscript.tcl \
	    $testdir/env$tnum.log.p1 \
	    $testdir $testfile PUT $key $data RECOVER 10 &]
	lappend pids $p1
	tclsleep 2

	puts "\t\tEnv$tnum.d2: Kill process 1."
	set pids [findprocessids $testdir $pids]
	foreach pid $pids {
		tclkill $pid 
	}

	puts "\t\tEnv$tnum.d3: Start process 2."
	set p2 [exec $tclsh_path $test_path/wrap.tcl envscript.tcl \
	    $testdir/env$tnum.log.p2 \
	    $testdir $testfile GET $key $data 0 0 &]
	tclsleep 2
	watch_procs $p2 1 120

	# Check log files.  Log p1 should be clean, but we 
	# expect DB_RUNRECOVERY in log p2.
	logcheck $testdir/env$tnum.log.p1
	logcheckfails $testdir/env$tnum.log.p2 DB_RUNRECOVERY

	puts "\tEnv$tnum.e: Running registered process detects failure."
	env_cleanup $testdir
	
	puts "\t\tEnv$tnum.e1: Start process 1."
	set pids {}
	set p1 [exec $tclsh_path $test_path/wrap.tcl envscript.tcl \
	    $testdir/env$tnum.log.p1 \
	    $testdir $testfile PUT $key $data RECOVER 10 &]
	lappend pids $p1
	tclsleep 2

	# Identify child process to kill later. 
	set pids [findprocessids $testdir $pids]

	puts "\t\tEnv$tnum.e2: Start process 2."
	set p2 [exec $tclsh_path $test_path/wrap.tcl envscript.tcl \
	    $testdir/env$tnum.log.p2 \
	    $testdir $testfile LOOP $key $data 0 10 &]

	puts "\t\tEnv$tnum.e3: Kill process 1."
	foreach pid $pids {
		tclkill $pid
	}

	puts "\t\tEnv$tnum.e4: Start process 3."
	set p3 [exec $tclsh_path $test_path/wrap.tcl envscript.tcl \
	    $testdir/env$tnum.log.p3 \
	    $testdir $testfile GET $key $data RECOVER 0 &]
	tclsleep 2

	watch_procs $p2 1 120
	watch_procs $p3 1 120
	
	# Check log files.  Logs p1 and p3 should be clean, but we 
	# expect DB_RUNRECOVERY in log p2.
	logcheck $testdir/env$tnum.log.p1
	logcheckfails $testdir/env$tnum.log.p2 DB_RUNRECOVERY
	logcheck $testdir/env$tnum.log.p3

}

# Check log file and report failures with FAIL.  Use this when
# we don't expect failures.
proc logcheck { logname } {
	set errstrings [eval findfail $logname]
	foreach errstring $errstrings {
		puts "FAIL: error in $logname : $errstring"
	}
}

# When we expect a failure, verify we find the one we expect.
proc logcheckfails { logname message }  {
	set f [open $logname r]
	while { [gets $f line] >= 0 } {
		if { [is_substr $line $message] == 1 } {
			close $f
			return 0
		}
	}		
	close $f
	puts "FAIL: Did not find expected error $message."
}

# The script wrap.tcl creates a parent and a child process.  We
# can't see the child pids, so find them by their sentinel files. 
# This creates a list where the parent pid is always listed 
# before the child pid.
proc findprocessids { testdir plist }  {
	set beginfiles [glob $testdir/begin.*]
	foreach b $beginfiles {
		regsub $testdir/begin. $b {} pid
		if { [lsearch -exact $plist $pid] == -1 } {
			lappend plist $pid	
		}
	}
	return $plist
}

