# See the file LICENSE for redistribution information.
#
# Copyright (c) 2026 berkeleydb/libdb contributors.  All rights reserved.
#
# $Id$
#
# TEST	ssi001
# TEST	Serializable Snapshot Isolation: the canonical write-skew anomaly.
# TEST
# TEST	Two snapshot-safe transactions each read the datum the other writes
# TEST	(T1: read y, write x;  T2: read x, write y).  Under plain snapshot
# TEST	isolation both commit, producing a write-skew anomaly.  Under SSI
# TEST	exactly one must be aborted with DB_SNAPSHOT_CONFLICT/UNSAFE.
# TEST
# TEST	x and y live in separate databases so the two writes never contend
# TEST	on the same page -- the only contention we want to exercise is the
# TEST	read/write antidependency tracked by SSI, not page locks.
proc ssi001 { } {
	source ./include.tcl

	puts "Ssi001: Serializable Snapshot Isolation write-skew anomaly"

	env_cleanup $testdir

	# Multiversion (snapshot) + txn + lock environment.  A short lock
	# timeout guarantees the single-threaded interleave can never hang on
	# an unexpected lock wait; it fails fast instead.
	set e [berkdb_env -create -home $testdir \
	    -txn -lock -log -multiversion -lock_timeout 2000000]
	error_check_good env_open [is_valid_env $e] TRUE

	set dbx [berkdb open -create -auto_commit -env $e -btree -multiversion x.db]
	error_check_good dbx_open [is_valid_db $dbx] TRUE
	set dby [berkdb open -create -auto_commit -env $e -btree -multiversion y.db]
	error_check_good dby_open [is_valid_db $dby] TRUE

	error_check_good seed_x [$dbx put k 0] 0
	error_check_good seed_y [$dby put k 0] 0

	puts "\tSsi001.a: Interleave two snapshot-safe transactions"
	set t1 [$e txn -snapshot_safe]
	error_check_good t1_begin [is_valid_txn $t1 $e] TRUE
	set t2 [$e txn -snapshot_safe]
	error_check_good t2_begin [is_valid_txn $t2 $e] TRUE

	# Each reads the item the other will write (records the rw edges).
	error_check_good t1_read_y [catch {$dby get -txn $t1 k} r1] 0
	error_check_good t2_read_x [catch {$dbx get -txn $t2 k} r2] 0

	# Cross writes, in different databases (no page contention).
	set w1 [catch {$dbx put -txn $t1 k 1} wres1]
	set w2 [catch {$dby put -txn $t2 k 1} wres2]

	puts "\tSsi001.b: Commit both; SSI must abort exactly one"
	set c1 [catch {$t1 commit} cres1]
	set c2 [catch {$t2 commit} cres2]

	set fail1 [expr {$w1 != 0 || $c1 != 0}]
	set fail2 [expr {$w2 != 0 || $c2 != 0}]

	# If a write failed, the txn handle is dead only after commit; clean up
	# any txn whose write failed but whose commit we never reached.
	if { $w1 != 0 && $c1 == 0 } { catch {$t1 abort} }
	if { $w2 != 0 && $c2 == 0 } { catch {$t2 abort} }

	if { $fail1 } {
		error_check_good t1_is_ssi_err \
		    [is_substr "$wres1 $cres1" "DB_SNAPSHOT"] 1
	}
	if { $fail2 } {
		error_check_good t2_is_ssi_err \
		    [is_substr "$wres2 $cres2" "DB_SNAPSHOT"] 1
	}

	# The SSI guarantee: the write-skew must be prevented (>=1 abort) ...
	error_check_good ssi_no_write_skew [expr {$fail1 || $fail2}] 1
	# ... and we must not have spuriously aborted both.
	error_check_good ssi_not_both_aborted [expr {$fail1 && $fail2}] 0

	error_check_good dbx_close [$dbx close] 0
	error_check_good dby_close [$dby close] 0
	error_check_good env_close [$e close] 0
}
