# See the file LICENSE for redistribution information.
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# $Id$
#
# TEST	rep200
# TEST	ADVISORY reproduction for the __rep_cmp_vote election-comparator defect
# TEST	(see test/rep/E1-FINDINGS.md).  It builds, against the REAL engine, the
# TEST	precondition of the data_gen-bypass bug: three electable sites with an
# TEST	EQUAL last-commit LSN but DIFFERENT data_gen (A stale, B/C newer), each
# TEST	carrying those values in a real VOTE1.  It prints the measured LSNs and
# TEST	data generations and attempts an election.
# TEST
# TEST	This test is ADVISORY, not a pass/fail gate: the precondition reproduces
# TEST	every run, but forcing the asynchronous election to settle on the stale
# TEST	site deterministically is not possible through the TCL harness (advancing
# TEST	a site's data_gen requires an egen pre-bump that desynchronises the
# TEST	election's egen; a construction that keeps egens synced yields equal
# TEST	data_gens -- the two requirements conflict).  The DETERMINISTIC gate for
# TEST	this defect is at the comparator level: test/db/run_rep_cmpvote_order.sh
# TEST	and the CBMC harness test/cbmc/harness_cmpvote.c, which drive the REAL
# TEST	__rep_cmp_vote over the vote vectors captured here, in every arrival
# TEST	order.  See test/rep/E1-FINDINGS.md for the full reachability write-up.
proc rep200 { method args } {
	source ./include.tcl
	if { $checking_valid_methods } { return "btree" }
	rep200_run $method
}
proc last_regop_lsn { home } {
	source ./include.tcl
	set out [eval exec $util_path/db_printlog -h $home]
	set last ""
	foreach line [split $out "\n"] {
		if { [regexp {\[(\d+)\]\[(\d+)\]__txn_regop} $line -> f o] } { set last "$f/$o" }
	}
	return $last
}
proc mk_master_then_client { env } {
	foreach peer {1 2 3 4} { catch {replclear $peer} }
	$env rep_start -master
	foreach peer {1 2 3 4} { catch {replclear $peer} }
	$env rep_start -client
	foreach peer {1 2 3 4} { catch {replclear $peer} }
}
proc bump_egen { env eid qdir } {
	global elect_serial
	incr elect_serial
	start_election "bump.$elect_serial" $qdir [$env get_home] $eid 5 5 100 [expr 2*1000000] none 0
	tclsleep 3
	set he 0; catch {replprocessqueue $env $eid 0 he}
	catch {cleanup_elections}
	foreach peer {1 2 3 4} { catch {replclear $peer} }
}
# Deliver queued messages to $env (eid $machid) in queue order; if $holdfrom >= 0, leave
# queued (do not deliver) any VOTE1 whose sender == $holdfrom.  Returns count delivered.
proc deliver_except_vote1_from { dbenv machid holdfrom } {
	global queuedbs queueenv
	# Snapshot (recno -> {ctrl rec from}) in order, deciding keep/deliver, deleting delivered.
	set deliver {}
	set txn [$queueenv txn]
	set dbc [$queuedbs($machid) cursor -txn $txn]
	set dels {}
	for { set dbt [$dbc get -first] } { [llength $dbt] != 0 } { set dbt [$dbc get -next] } {
		set recno [lindex [lindex $dbt 0] 0]
		set data  [lindex [lindex $dbt 0] 1]
		set ctrl [lindex $data 0]; set rec [lindex $data 1]; set from [lindex $data 2]
		if { $holdfrom >= 0 && [berkdb msgtype $ctrl] eq "vote1" && $from == $holdfrom } {
			continue
		}
		lappend deliver [list $ctrl $rec $from]
		lappend dels $recno
	}
	$dbc close
	# Delete the ones we will deliver (so they are not reprocessed).
	set dc [$queuedbs($machid) cursor -txn $txn]
	foreach r $dels {
		if { [llength [$dc get -set $r]] != 0 } { $dc del }
	}
	$dc close
	$txn commit
	set n 0
	foreach msg $deliver {
		catch {$dbenv rep_process_message [lindex $msg 0] [lindex $msg 1] [lindex $msg 2]}
		incr n
	}
	return $n
}
proc rep200_run { method } {
	source ./include.tcl
	global testdir elect_serial queuedbs queueenv
	env_cleanup $testdir
	set qdir $testdir/MSGQUEUEDIR
	replsetup $qdir
	foreach d {M A B C} { file mkdir $testdir/$d }
	repladd 1; repladd 2; repladd 3; repladd 4
	set m [eval berkdb_env_noerr -create -txn -event -home $testdir/M -rep_master "-rep_transport {1 replsend}"]
	set a [eval berkdb_env_noerr -create -txn -event -home $testdir/A -errpfx A -rep_client "-rep_transport {2 replsend}"]
	set b [eval berkdb_env_noerr -create -txn -event -home $testdir/B -errpfx B -rep_client "-rep_transport {3 replsend}"]
	set c [eval berkdb_env_noerr -create -txn -event -home $testdir/C -errpfx C -rep_client "-rep_transport {4 replsend}"]
	set envlist "{$m 1} {$a 2} {$b 3} {$c 4}"
	process_msgs $envlist
	eval rep_test $method $m NULL 5 0 0 0
	process_msgs $envlist
	$m close

	set elect_serial 50
	mk_master_then_client $a
	bump_egen $b 3 $qdir
	mk_master_then_client $b
	bump_egen $c 4 $qdir
	mk_master_then_client $c

	set al [last_regop_lsn $testdir/A]; set bl [last_regop_lsn $testdir/B]; set cl [last_regop_lsn $testdir/C]
	set ag [stat_field $a rep_stat {Generation number}]
	set bg [stat_field $b rep_stat {Generation number}]
	set cg [stat_field $c rep_stat {Generation number}]
	puts "\tRep200.precond: A(2) regop=$al dg=$ag ; B(3) regop=$bl dg=$bg ; C(4) regop=$cl dg=$cg"
	error_check_good precond_equal_lsn_ab $al $bl
	error_check_good precond_equal_lsn_ac $al $cl
	error_check_good precond_a_stale [expr {$ag < $bg && $ag < $cg}] 1
	error_check_good precond_bc_newer_same [expr {$bg == $cg && $bg > $ag}] 1

	# Converge egens: run ONE joint throwaway election (full delivery) so all three agree on
	# an egen, then clear events.  This does NOT change data_gen (REPLSNHIST is untouched).
	foreach e [list $a $b $c] { $e event_info -clear }
	foreach peer {1 2 3 4} { catch {replclear $peer} }
	set elect_serial 90
	start_election "sync.90" $qdir $testdir/A 2 3 3 100 [expr 3*1000000] none 0
	start_election "sync.91" $qdir $testdir/B 3 3 3 10  [expr 3*1000000] none 0
	start_election "sync.92" $qdir $testdir/C 4 3 3 10  [expr 3*1000000] none 0
	tclsleep 2
	for {set i 0} {$i < 10} {incr i} {
		set he 0
		foreach {env id} [list $a 2 $b 3 $c 4] { catch {replprocessqueue $env $id 0 he} }
		tclsleep 1
	}
	catch {cleanup_elections}
	foreach e [list $a $b $c] { $e event_info -clear }
	foreach peer {1 2 3 4} { catch {replclear $peer} }
	puts "\tRep200.egens: A=[stat_field $a rep_stat {Election generation number}] B=[stat_field $b rep_stat {Election generation number}] C=[stat_field $c rep_stat {Election generation number}]"

	# Start an election on all three at the same egen.  A pri 100, B/C pri 10.
	set to [expr 30*1000000]
	set elect_serial 100
	start_election "A.100" $qdir $testdir/A 2 3 2 100 $to none 0 ; set pA 100
	start_election "B.101" $qdir $testdir/B 3 3 2 10  $to none 0 ; set pB 101
	start_election "C.102" $qdir $testdir/C 4 3 2 10  $to none 0 ; set pC 102
	tclsleep 2

	# Controlled delivery rounds: to B and C deliver everything EXCEPT A's VOTE1 (hold it),
	# so B and C tally the newer votes first.  To A deliver everything.  Repeat so all VOTE1s
	# (except A->B, A->C) propagate.  THEN deliver A's held VOTE1 to B and C last.
	for {set i 0} {$i < 8} {incr i} {
		catch {deliver_except_vote1_from $a 2 -1}
		catch {deliver_except_vote1_from $b 3 2}
		catch {deliver_except_vote1_from $c 4 2}
		foreach {env id pipe} [list $a 2 $pA $b 3 $pB $c 4 $pC] {
			catch {check_election $pipe u e}
		}
		tclsleep 1
	}
	puts "\tRep200.predeliver: A_elected=[is_elected $a] B_elected=[is_elected $b] C_elected=[is_elected $c]"
	# Now deliver A's stale VOTE1 to B and C LAST (arm D should flip them to A).
	for {set i 0} {$i < 10} {incr i} {
		catch {deliver_except_vote1_from $b 3 -1}
		catch {deliver_except_vote1_from $c 4 -1}
		catch {deliver_except_vote1_from $a 2 -1}
		foreach {env id pipe} [list $a 2 $pA $b 3 $pB $c 4 $pC] {
			catch {check_election $pipe u e}
		}
		if {[is_elected $a] || [is_elected $b] || [is_elected $c]} break
		tclsleep 1
	}
	set awon [is_elected $a]; set bwon [is_elected $b]; set cwon [is_elected $c]
	puts "\tRep200.winner: A_elected=$awon B_elected=$bwon C_elected=$cwon"
	catch {cleanup_elections}
	#
	# ADVISORY outcome.  The precondition (asserted above) is the real result
	# of this test and always holds.  The election below may or may not settle
	# through the TCL harness; we only REPORT what happened.  The deterministic
	# pass/fail gate is test/db/run_rep_cmpvote_order.sh + the CBMC harness.
	#
	if {$awon} {
		puts "\tRep200: OBSERVED the bug -- the STALE data_gen-$ag site (eid2) was"
		puts "\tRep200: elected over newer data_gen-$bg sites at equal LSN (arm D"
		puts "\tRep200: bypassed data_gen).  Expected on the pre-fix comparator."
	} elseif {$bwon || $cwon} {
		puts "\tRep200: a newer-data site was elected (not the stale A) -- the"
		puts "\tRep200: outcome the fixed total order guarantees."
	} else {
		puts "\tRep200: election did not settle through the harness (advisory;"
		puts "\tRep200: see the comparator-level gate for the deterministic check)."
	}
	puts "\tRep200 RESULT: PASS (advisory -- precondition reproduced)."
	catch {$a close}; catch {$b close}; catch {$c close}
	replclose $qdir
}
