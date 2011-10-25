# See the file LICENSE for redistribution information.
#
# Copyright (c) 2009, 2011 Oracle and/or its affiliates.  All rights reserved.
#
# TEST	repmgr026
# TEST	Test of "full election" timeouts.
# TEST	1. Cold boot with all sites present.
# TEST	2. Cold boot with some sites missing.
# TEST	3. Partial-participation election with one client having seen a master,
# TEST	   but another just starting up fresh.
# TEST	4. Partial participation, with all participants already having seen a
# TEST	   master.
# TEST

proc repmgr026 { { tnum 026 } } {
	source ./include.tcl

	if { $is_freebsd_test == 1 } {
		puts "Skipping replication manager test on FreeBSD platform."
		return
	}

	foreach use_leases {no yes} {
		foreach client_down {no yes} {
			puts "Repmgr$tnum: Full election test, \
			    client_down: $client_down; leases: $use_leases"
			repmgr026_sub $tnum $client_down $use_leases
		}
	}
}

proc repmgr026_sub { tnum client_down use_leases } {
	global testdir
	global repfiles_in_memory
	global rep_verbose
	global verbose_type
	
	set verbargs ""
	if { $rep_verbose == 1 } {
		set verbargs " -verbose {$verbose_type on} "
	}

	set repmemargs ""
	if { $repfiles_in_memory } {
		set repmemargs "-rep_inmem_files "
	}

	env_cleanup $testdir
	file mkdir [set dira $testdir/SITE_A]
	file mkdir [set dirb $testdir/SITE_B]
	file mkdir [set dirc $testdir/SITE_C]
	foreach { porta portb portc } [available_ports 3] {}

	# Cold boot the group (with or without site C), giving site A a
	# high priority.
	# 
	set common "-create -txn $verbargs $repmemargs \
	    -rep -thread -event"
	if { $use_leases } {
		append common " -rep_lease {[list 3 3000000]} "
	}
	set common_mgr "-nsites 3 -start elect \
	    -timeout {connection_retry 5000000} \
	    -timeout {election_retry 2000000}"
	set times "-timeout {full_election 60000000} \
	    -timeout {election 5000000} -timeout {ack 3000000}"

	# The wait_limit's are intended to be an amount that is way more than
	# the expected timeout, used for nothing more than preventing the test
	# from hanging forever.  The leeway amount should be enough less than
	# the timeout to allow for any imprecision introduced by the test
	# mechanism.
	# 
	set elect_wait_limit 25
	set full_secs_leeway 59
	set full_wait_limit 85

	puts "\tRepmgr$tnum.a: Start first two sites."
	set cmda "berkdb_env_noerr $common -errpfx SITE_A -home $dira"
	set enva [eval $cmda]
	eval $enva repmgr $common_mgr $times -pri 200 \
	    -local {[list localhost $porta]}

	set cmdb "berkdb_env_noerr $common -errpfx SITE_B -home $dirb"
	set envb [eval $cmdb]
	eval $envb repmgr $common_mgr $times -pri 100 \
	    -local {[list localhost $portb]} -remote {[list localhost $porta]}

	set cmdc "berkdb_env_noerr $common -errpfx SITE_C -home $dirc"
	if { $client_down } {
		set envc NONE
	} else {
		puts "\tRepmgr$tnum.b: Start third site."
		set envc [eval $cmdc]
		eval $envc repmgr $common_mgr $times -pri 50 \
		    -local {[list localhost $portc]} \
		    -remote {[list localhost $porta]}
	}

	# wait for results, and make sure they're correct
	#
	set envlist [list $enva $envb]
	if { $envc != "NONE" } {
		lappend envlist $envc
	}
	set limit $full_wait_limit
	puts "\tRepmgr$tnum.c: wait (up to $limit seconds) for first election."
	set t [repmgr026_await_election_result $envlist $limit]
	if { $client_down } {
		error_check_good slow_election [expr $t > $full_secs_leeway] 1
	} else {
		# When all sites participate, the election should finish in way
		# less than 60 seconds.
		# 
		error_check_good timely_election [expr $t < $full_secs_leeway] 1
	}
	puts "\tRepmgr$tnum.d: first election completed in $t seconds"

	puts "\tRepmgr$tnum.e: wait for start-up done"
	$enva event_info -clear
	await_startup_done $envb
	$envb event_info -clear
	if { $envc != "NONE" } {
		await_startup_done $envc
		$envc event_info -clear
	}

	# Shut down site A, in order to test elections with less than the whole
	# group voting.  However, normally repmgr's reaction to losing master
	# connection is to try a "fast election" (the n-1 trick).  So we must do
	# something to mitigate that (see below).
	# 
	puts "\tRepmgr$tnum.f: shut down master site A"
	if { $client_down } {

		# The third site is already down, so now there's only site B
		# running.  In its first election attempt it won't be able to
		# succeed.  In subsequent attempts it won't bother with the
		# "fast election" trick, so we can avoid that just by waiting.
		#
		$enva close
		set initial_egen \
		    [stat_field $envb rep_stat "Election generation number"]
		
		# Just to be safe, skip over 2 egens.
		# 
		puts "\tRepmgr$tnum.f: skip over \"fast election\" egen"
		set goal [expr $initial_egen + 2]
		await_condition {[expr \
		    [stat_field $envb rep_stat "Election generation number"] \
		    >= $goal]}

		puts "\tRepmgr$tnum.g: Start third client"
		set envc [eval $cmdc]
		eval $envc repmgr $common_mgr $times -pri 50 \
		    -local {[list localhost $portc]} \
		    -remote {[list localhost $portb]}
	} else {

		# Here the third client is not initially down, so waiting (as we
		# did above) won't work.  Instead we'll fake a higher group size
		# in order to compensate for the "n-1" trick.  However, that
		# doesn't work when leases are in effect, since leases currently
		# disallow group size changes.  So just skip this part of the
		# test in this case.
		# 
		if { $use_leases } {
			$envc close
			$envb close
			$enva close
			return
		}
		$envb repmgr -nsites 4
		$envc repmgr -nsites 4

		$enva close
		set initial_egen \
		    [stat_field $envb rep_stat "Election generation number"]
		set goal [expr $initial_egen + 2]
		await_condition {[expr \
		    [stat_field $envb rep_stat "Election generation number"] \
		    >= $goal]}
		$envb repmgr -nsites 3
		$envc repmgr -nsites 3
	}

	# wait for results, and check them
	# 
	set envlist [list $envb $envc]
	set limit $elect_wait_limit
	puts "\tRepmgr$tnum.h: wait (up to $limit seconds) for second election."
	set t [repmgr026_await_election_result $envlist $limit]
	error_check_good normal_election [expr $t < $full_secs_leeway] 1
	puts "\tRepmgr$tnum.i: second election completed in $t seconds"

	await_startup_done $envc
	$envc close
	$envb close
}

# Wait (a limited amount of time) for the election to finish.  The first env
# handle in the list is the expected winner, and the others are the remaining
# clients.  Returns the approximate amount of time (in seconds) that the
# election took.
# 
proc repmgr026_await_election_result { envlist limit } {
	set begin [clock seconds]
	set deadline [expr $begin + $limit]
	while { true } {
		set t [clock seconds]
		if { $t > $deadline } {
			error "FAIL: time limit exceeded"
		}

		if { [repmgr026_is_ready $envlist] } {
			return [expr $t - $begin]
		}

		tclsleep 1
	}
}

proc repmgr026_is_ready { envlist } {
	set winner [lindex $envlist 0]
	if {![is_elected $winner]} {
		return false
	}

	foreach client [lrange $envlist 1 end] {
		if {![is_event_present $client newmaster]} {
			return false
		}
	}
	return true
}
