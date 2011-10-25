# See the file LICENSE for redistribution information.
#
# Copyright (c) 2009, 2011 Oracle and/or its affiliates.  All rights reserved.
#
# TEST	repmgr027
# TEST	Test of "full election" timeouts, where a client starts up and joins the
# TEST	group during the middle of an election.
# TEST

proc repmgr027 { { tnum 027 } } {
	source ./include.tcl

	if { $is_freebsd_test == 1 } {
		puts "Skipping replication manager test on FreeBSD platform."
		return
	}
	puts -nonewline "Repmgr$tnum: Full election test,"
	puts " with client joining halfway through election"
	repmgr027_sub $tnum
}

proc repmgr027_sub { tnum } {
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
	file mkdir [set dirc $testdir/SITE_D]
	foreach { porta portb portc portd } [available_ports 4] {}

	# The election times are arbitrary, but the full election timeout should
	# be long enough to allow the test to start two sites, wait for them to
	# be in an election, and then have the third site start and join
	# (including the leeway time, in seconds), before it times out.
	#
	set common "-create -txn $verbargs $repmemargs \
	    -rep -thread -event"
	set common_mgr "-nsites 3 -start elect \
	    -timeout {connection_retry 5000000} \
	    -timeout {election_retry 2000000}"
	set times "-timeout {full_election 180000000} \
	    -timeout {election 5000000}"
	set leeway 5

	# Cold boot, at first just 2 sites.
	# 
	puts "\tRepmgr$tnum.a: Start first two sites."
	set cmda "berkdb_env_noerr $common -errpfx SITE_A -home $dira"
	set enva [eval $cmda]
	eval $enva repmgr $common_mgr $times -pri 200 \
	    -local {[list localhost $porta]}

	set cmdb "berkdb_env_noerr $common -errpfx SITE_B -home $dirb"
	set envb [eval $cmdb]
	eval $envb repmgr $common_mgr $times -pri 100 \
	    -local {[list localhost $portb]} -remote {[list localhost $porta]}

	# Wait until both sites recognize that they're in an election, plus a
	# few extra seconds just for good measure.
	# 
	await_condition {[expr \
	    [stat_field $enva rep_stat "Election phase"] == 1 && \
	    [stat_field $envb rep_stat "Election phase"] == 1]}
	tclsleep $leeway

	# At this point we should not have completed an election yet, even
	# though we have a majority, because we don't have full participation.
	# 
	error_check_bad site_a_elected [is_elected $enva] 1
	error_check_bad site_b_elected [is_elected $envb] 1

	puts "\tRepmgr$tnum.c: Start 3rd site."

	set cmdc "berkdb_env_noerr $common -errpfx SITE_C -home $dirc"
	set envc [eval $cmdc]
	eval $envc repmgr $common_mgr $times -pri 100 \
	    -local {[list localhost $portc]} \
	    -remote {[list localhost $porta]}

	# Wait for results, and make sure they're correct.  The election should
	# complete right away, once the third client has joined, regardless of
	# the election timeout values.  We wait an arbitrary maximum of 60
	# seconds, merely so that the test doesn't hang forever if something
	# goes horribly wrong.
	#
	set envlist [list $enva $envb $envc]
	set limit 60
	puts "\tRepmgr$tnum.c: wait (up to $limit seconds) for election."
	set t [repmgr026_await_election_result $envlist $limit]
	error_check_good timely_election [expr $t < 2 * $leeway] 1
	puts "\tRepmgr$tnum.d: first election completed in $t seconds"

	puts "\tRepmgr$tnum.e: wait for start-up done"
	await_startup_done $envb
	await_startup_done $envc

	$envb close
	$envc close
	$enva close
}
