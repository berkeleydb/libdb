# rep200 v12: DURABILITY reproduction. 3 sites, equal last-commit LSN:
#   A (eid2) STALE data_gen 2, HIGH priority.   B (eid3), C (eid4) NEWER data_gen 3, LOW priority.
# A genuine total order (data_gen first) must elect a NEWER site (B or C) -- never the stale A.
# Arm D (cmp==0 on priority) makes the newer electors B,C pick the stale A: each tallies its
# own newer vote first (winner), then A's stale-but-higher-priority vote fires arm D -> A.
# A majority (B,C) then votes A -> the STALE site is elected and the newer data (data_gen 3)
# is rolled back on sync (memcmp, rep_verify.c:77): a durable commit is lost.
# VERDICT: the elected master must NOT be the stale A (eid2).  Master elects A -> FAIL; fix -> PASS.
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
# One master cycle (writes exactly one REPLSNHIST commit). peers' queues cleared so the
# transient mastership is invisible; identical byte additions on every site.
proc mk_master_then_client { env } {
	foreach peer {1 2 3 4} { catch {replclear $peer} }
	$env rep_start -master
	foreach peer {1 2 3 4} { catch {replclear $peer} }
	$env rep_start -client
	foreach peer {1 2 3 4} { catch {replclear $peer} }
}
# Bump this site's egen (so its next master cycle lands at a higher gen) via a throwaway
# election it cannot win (needs more votes than exist).
proc bump_egen { env eid qdir } {
	global elect_serial
	incr elect_serial
	start_election "bump.$elect_serial" $qdir [$env get_home] $eid 5 5 100 [expr 2*1000000] none 0
	tclsleep 3
	set he 0; catch {replprocessqueue $env $eid 0 he}
	catch {cleanup_elections}
	foreach peer {1 2 3 4} { catch {replclear $peer} }
}
# Full election over cenvlist that RETURNS the winning eid (or -1).  Each site runs its own
# rep_elect child; we pump every queue and restart a site's election on HOLDELECTION so egens
# converge, exactly like run_election, but we observe the winner rather than assert it.
proc rep200_run_election { cenvlist priname qdir nsites nvotes to } {
	global elect_serial
	upvar $priname pri
	array set ep {}
	foreach pair $cenvlist {
		set env [lindex $pair 0]; set eid [lindex $pair 1]
		$env event_info -clear
		replclear $eid
	}
	incr elect_serial
	set base $elect_serial
	foreach pair $cenvlist {
		set env [lindex $pair 0]; set eid [lindex $pair 1]
		incr elect_serial
		start_election "S$eid.$elect_serial" $qdir [$env get_home] $eid \
		    $nsites $nvotes $pri($eid) $to none 0
		set ep($eid) $elect_serial
	}
	tclsleep 2
	set winner -1
	set tries [expr ($to * 4) / 1000000]
	while { $tries > 0 && $winner < 0 } {
		set nproced 0
		foreach pair $cenvlist {
			set env [lindex $pair 0]; set eid [lindex $pair 1]
			set he 0
			catch {check_election $ep($eid) unavail celected}
			if {[info exists celected] && $celected} { set winner $eid; break }
			incr nproced [replprocessqueue $env $eid 0 he]
			if {[is_elected $env]} { set winner $eid; break }
			if {$he == 1} {
				catch {close_election $ep($eid)}
				incr elect_serial
				start_election "S$eid.$elect_serial" $qdir [$env get_home] \
				    $eid $nsites $nvotes $pri($eid) $to none 0
				set ep($eid) $elect_serial
			}
		}
		if {$winner >= 0} break
		if {$nproced == 0} { incr tries -1; tclsleep 1 }
	}
	catch {cleanup_elections}
	return $winner
}
proc rep200_run { method } {
	source ./include.tcl
	global testdir elect_serial
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

	# A -> data_gen 2 (one master cycle at gen 2).
	set elect_serial 50
	mk_master_then_client $a
	# B,C -> data_gen 3: pre-bump their egen above 2, then one master cycle each.
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
	error_check_good precond_bc_same_newer [expr {$bg == $cg && $bg > $ag}] 1

	# A (stale) HIGH priority; B,C (newer) LOW priority.  Correct order elects a NEWER
	# site; arm D makes a majority elect the stale A.  We use run_election EXPECTING the
	# stale A (index 0) as winner: that is what the arm-D bug produces.  On master A is
	# elected -> run_election succeeds -> BUG present -> we FAIL the test.  With the fix a
	# newer site wins -> run_election's right_winner mismatches/does not elect A -> we PASS.
	set cenvlist "{$a 2} {$b 3} {$c 4}"
	set pri(0) 100; set pri(1) 10; set pri(2) 10
	set err(0) none; set err(1) none; set err(2) none
	set crash(0) 0; set crash(1) 0; set crash(2) 0
	set rc [catch {
		run_election cenvlist err pri crash $qdir \
		    Rep200 0 3 2 3 0 0 NULL 0 1 [expr 20*1000000]
	} msg]
	# timeout_ok=1 so a non-settling election is not a hard error; rc!=0 or a non-A winner
	# both mean A was NOT elected.
	set awon [is_elected $a]
	puts "\tRep200.winner: run_election rc=$rc  A_elected=$awon  B_elected=[is_elected $b] C_elected=[is_elected $c]"
	if {$awon} {
		puts "\tRep200 RESULT: FAIL -- the STALE data_gen-$ag site (eid2) was elected over"
		puts "\tRep200: newer data_gen-$bg sites at equal LSN -- arm D bypassed data_gen."
		error_check_good rep200_stale_not_elected $awon 0
	}
	puts "\tRep200 RESULT: PASS -- the stale site was NOT elected."
	catch {$a close}; catch {$b close}; catch {$c close}
	replclose $qdir
}
