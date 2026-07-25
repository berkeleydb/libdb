# See the file LICENSE for redistribution information.
#
# Copyright (c) 2026 berkeleydb/libdb contributors.  All rights reserved.
#
# $Id$
#
# TEST	ssi004
# TEST	Serializable Snapshot Isolation under a partitioned lock manager.
# TEST
# TEST	Regression for the commit-time pivot-flag race (docs/design/
# TEST	ssi-pivot-race.md).  The pivot flags TXN_DTL_RCONF/TXN_DTL_WCONF are
# TEST	set on a transaction detail by writers in the lock manager and read
# TEST	by __txn_commit.  Under multiple lock partitions the flag writes hold
# TEST	only a per-partition mutex, so before the fix a write from a writer in
# TEST	a different partition -- or the lock-free commit-time read -- was
# TEST	unsynchronized, and a real pivot could commit.
# TEST
# TEST	This runs the canonical write-skew (as ssi001) but forces many lock
# TEST	partitions so the two conflicting objects land in different partitions,
# TEST	and repeats the interleave so a reintroduced race has repeated chances
# TEST	to admit a non-serializable schedule.  The SSI invariant -- at least
# TEST	one of each conflicting pair aborts, never both -- must hold every
# TEST	iteration.
proc ssi004 { { iterations 50 } } {
	source ./include.tcl

	puts "Ssi004: SSI write-skew under a partitioned lock manager"

	env_cleanup $testdir

	# Force many lock partitions: this is what makes LOCK_SYSTEM_LOCK a
	# no-op and exercises the per-partition flag-write path the fix
	# serializes on the txn region.
	set e [berkdb_env -create -home $testdir \
	    -txn -lock -log -multiversion -lock_partitions 64 \
	    -lock_timeout 2000000]
	error_check_good env_open [is_valid_env $e] TRUE

	set dbx [berkdb open -create -auto_commit -env $e -btree -multiversion x.db]
	error_check_good dbx_open [is_valid_db $dbx] TRUE
	set dby [berkdb open -create -auto_commit -env $e -btree -multiversion y.db]
	error_check_good dby_open [is_valid_db $dby] TRUE

	set aborts 0
	for { set i 0 } { $i < $iterations } { incr i } {
		# Fresh seed each round with distinct keys, so successive rounds
		# touch different lock objects (spreading across partitions).
		set k "k$i"
		error_check_good seed_x [$dbx put $k 0] 0
		error_check_good seed_y [$dby put $k 0] 0

		set t1 [$e txn -snapshot_safe]
		error_check_good t1_begin [is_valid_txn $t1 $e] TRUE
		set t2 [$e txn -snapshot_safe]
		error_check_good t2_begin [is_valid_txn $t2 $e] TRUE

		# Each reads the item the other will write (records rw edges).
		error_check_good t1_read_y [catch {$dby get -txn $t1 $k} r1] 0
		error_check_good t2_read_x [catch {$dbx get -txn $t2 $k} r2] 0

		# Cross writes in different databases (no page contention;
		# only the SSI antidependency should conflict).
		set w1 [catch {$dbx put -txn $t1 $k 1} wres1]
		set w2 [catch {$dby put -txn $t2 $k 1} wres2]

		set c1 [catch {$t1 commit} cres1]
		set c2 [catch {$t2 commit} cres2]

		set fail1 [expr {$w1 != 0 || $c1 != 0}]
		set fail2 [expr {$w2 != 0 || $c2 != 0}]

		# Clean up any txn whose write failed but never reached commit.
		if { $w1 != 0 && $c1 == 0 } { catch {$t1 abort} }
		if { $w2 != 0 && $c2 == 0 } { catch {$t2 abort} }

		if { $fail1 } {
			error_check_good t1_ssi_err \
			    [is_substr "$wres1 $cres1" "DB_SNAPSHOT"] 1
		}
		if { $fail2 } {
			error_check_good t2_ssi_err \
			    [is_substr "$wres2 $cres2" "DB_SNAPSHOT"] 1
		}

		# The invariant, every iteration: the write-skew is prevented
		# (>=1 abort) and we did not spuriously abort both.
		error_check_good no_write_skew_$i [expr {$fail1 || $fail2}] 1
		error_check_good not_both_$i [expr {$fail1 && $fail2}] 0
		if { $fail1 || $fail2 } { incr aborts }
	}

	# Every iteration is a genuine dangerous structure, so every one must
	# have produced exactly one abort.
	error_check_good all_prevented $aborts $iterations
	puts "\tSsi004: $aborts/$iterations write-skews prevented"

	error_check_good dbx_close [$dbx close] 0
	error_check_good dby_close [$dby close] 0
	error_check_good env_close [$e close] 0
}
