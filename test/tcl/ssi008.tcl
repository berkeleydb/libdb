# See the file LICENSE for redistribution information.
#
# Copyright (c) 2026 berkeleydb/libdb contributors.  All rights reserved.
#
# $Id$
#
# TEST	ssi008
# TEST	Serializable Snapshot Isolation: state across environment recovery.
# TEST
# TEST	SIREAD markers and the SSI conflict flags are in-memory lock-region
# TEST	state, not logged.  After a crash and recovery they are gone -- which
# TEST	is correct: recovered transactions are resolved by the log, and no
# TEST	live SSI conflict can span the crash.  Verify that:
# TEST	  (a) an environment that had active/committed snapshot-safe txns
# TEST	      recovers cleanly, and
# TEST	  (b) SSI works normally after recovery -- it still catches a fresh
# TEST	      write-skew and still allows non-conflicting transactions (no
# TEST	      stale marker state causes spurious conflicts).
proc ssi008 { } {
	source ./include.tcl

	puts "Ssi008: SSI state across recovery"

	env_cleanup $testdir
	set env_cmd "berkdb_env -create -home $testdir -txn -lock -log -multiversion"

	# ---- populate and leave SSI activity behind, then "crash" ---------
	puts "\tSsi008.a: run snapshot-safe txns, then close without cleanup"
	set e [eval $env_cmd]
	error_check_good env_open [is_valid_env $e] TRUE
	set dbx [berkdb open -create -auto_commit -env $e -btree -multiversion x.db]
	set dby [berkdb open -create -auto_commit -env $e -btree -multiversion y.db]
	error_check_good seed_x [$dbx put k 0] 0
	error_check_good seed_y [$dby put k 0] 0

	# A committed snapshot-safe reader (leaves a persisted SIREAD marker).
	set tr [$e txn -snapshot_safe]
	error_check_good tr_read [catch {$dbx get -txn $tr k} r] 0
	error_check_good tr_commit [$tr commit] 0
	# An in-flight snapshot-safe txn that we never commit (simulated crash).
	set tinflight [$e txn -snapshot_safe]
	error_check_good tin_write [$dbx put -txn $tinflight k 7] 0

	# Close the databases and env WITHOUT resolving tinflight -- the next
	# open with -recover must roll it back from the log.
	error_check_good dbx_close [$dbx close] 0
	error_check_good dby_close [$dby close] 0
	# Force the handles/txn away without a clean commit path.
	catch {$tinflight abort}
	error_check_good env_close [$e close] 0

	# ---- recover ------------------------------------------------------
	puts "\tSsi008.b: reopen with -recover (must succeed)"
	set e [eval $env_cmd "-recover"]
	error_check_good env_recover [is_valid_env $e] TRUE

	set dbx [berkdb open -auto_commit -env $e -btree -multiversion x.db]
	set dby [berkdb open -auto_commit -env $e -btree -multiversion y.db]
	error_check_good dbx_reopen [is_valid_db $dbx] TRUE
	error_check_good dby_reopen [is_valid_db $dby] TRUE

	# ---- SSI still works after recovery -------------------------------
	puts "\tSsi008.c: a fresh write-skew is still caught after recovery"
	set t1 [$e txn -snapshot_safe]
	set t2 [$e txn -snapshot_safe]
	catch {$dby get -txn $t1 k}
	catch {$dbx get -txn $t2 k}
	set w1 [catch {$dbx put -txn $t1 k 1} e1]
	set w2 [catch {$dby put -txn $t2 k 1} e2]
	set c1 [catch {$t1 commit} c1res]
	set c2 [catch {$t2 commit} c2res]
	set f1 [expr {$w1 || $c1}]
	set f2 [expr {$w2 || $c2}]
	if { $f1 && $c1 == 0 } { catch {$t1 abort} }
	if { $f2 && $c2 == 0 } { catch {$t2 abort} }
	error_check_good skew_still_caught [expr {$f1 || $f2}] 1
	error_check_good not_both_aborted [expr {$f1 && $f2}] 0

	puts "\tSsi008.d: non-conflicting snapshot-safe txns still commit"
	set t3 [$e txn -snapshot_safe]
	error_check_good t3_read [catch {$dbx get -txn $t3 k} r] 0
	error_check_good t3_commit [$t3 commit] 0

	error_check_good dbx_close2 [$dbx close] 0
	error_check_good dby_close2 [$dby close] 0
	error_check_good env_close2 [$e close] 0
}
