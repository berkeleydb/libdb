# See the file LICENSE for redistribution information.
#
# Copyright (c) 2026 berkeleydb/libdb contributors.  All rights reserved.
#
# $Id$
#
# TEST	ssi010
# TEST	DB_TXN_SNAPSHOT (plain SI) vs DB_TXN_SERIALIZABLE (SSI): the write-skew
# TEST	anomaly is VISIBLE under plain snapshot isolation and PREVENTED under
# TEST	serializable snapshot isolation.
# TEST
# TEST	This is the whole point of restoring DB_TXN_SNAPSHOT to plain snapshot
# TEST	isolation and adding DB_TXN_SERIALIZABLE for SSI.  The exact same
# TEST	interleave (T1 reads y writes x; T2 reads x writes y) commits BOTH
# TEST	transactions under -snapshot (the classic SI write skew, exactly as
# TEST	Oracle Berkeley DB behaved) but must abort at least one under
# TEST	-snapshot_safe (SSI).
proc ssi010 { } {
	source ./include.tcl

	puts "Ssi010: plain SI shows write skew; SSI prevents it"

	# Part 1: plain DB_TXN_SNAPSHOT (-snapshot) -- both must commit.
	puts "\tSsi010.a: -snapshot (plain SI): write skew COMMITS (anomaly)"
	set skew_a [ssi010_writeskew "-snapshot"]
	error_check_good si_both_commit $skew_a 0

	# Part 2: DB_TXN_SERIALIZABLE (-snapshot_safe) -- at least one aborts.
	puts "\tSsi010.b: -snapshot_safe (SSI): write skew PREVENTED (>=1 abort)"
	set skew_b [ssi010_writeskew "-snapshot_safe"]
	error_check_good ssi_prevents_skew [expr {$skew_b >= 1}] 1
}

# Run the write-skew interleave with the given txn isolation flag.  Returns the
# number of transactions that failed (write error or commit error).  0 means
# both committed (a write-skew anomaly was allowed); >=1 means it was blocked.
proc ssi010_writeskew { txnflag } {
	source ./include.tcl

	env_cleanup $testdir

	set e [berkdb_env_noerr -create -home $testdir \
	    -txn -lock -log -multiversion -lock_timeout 2000000]
	error_check_good env_open [is_valid_env $e] TRUE

	set dbx [berkdb open -create -auto_commit -env $e -btree -multiversion x.db]
	error_check_good dbx_open [is_valid_db $dbx] TRUE
	set dby [berkdb open -create -auto_commit -env $e -btree -multiversion y.db]
	error_check_good dby_open [is_valid_db $dby] TRUE

	error_check_good seed_x [$dbx put k 0] 0
	error_check_good seed_y [$dby put k 0] 0

	set t1 [eval {$e txn} $txnflag]
	error_check_good t1_begin [is_valid_txn $t1 $e] TRUE
	set t2 [eval {$e txn} $txnflag]
	error_check_good t2_begin [is_valid_txn $t2 $e] TRUE

	# Each reads the item the other will write.
	error_check_good t1_read_y [catch {$dby get -txn $t1 k} r1] 0
	error_check_good t2_read_x [catch {$dbx get -txn $t2 k} r2] 0

	# Cross writes, in different databases (no page contention).
	set w1 [catch {$dbx put -txn $t1 k 1} wres1]
	set w2 [catch {$dby put -txn $t2 k 1} wres2]

	set c1 [catch {$t1 commit} cres1]
	set c2 [catch {$t2 commit} cres2]

	set fail1 [expr {$w1 != 0 || $c1 != 0}]
	set fail2 [expr {$w2 != 0 || $c2 != 0}]

	# Clean up any txn whose write failed but whose commit we never reached.
	if { $w1 != 0 && $c1 == 0 } { catch {$t1 abort} }
	if { $w2 != 0 && $c2 == 0 } { catch {$t2 abort} }

	# Any failure under SSI must be a DB_SNAPSHOT error, not something else.
	if { $fail1 } {
		error_check_good t1_err \
		    [is_substr "$wres1 $cres1" "DB_SNAPSHOT"] 1
	}
	if { $fail2 } {
		error_check_good t2_err \
		    [is_substr "$wres2 $cres2" "DB_SNAPSHOT"] 1
	}
	# Never spuriously abort both.
	error_check_good not_both_aborted [expr {$fail1 && $fail2}] 0

	error_check_good dbx_close [$dbx close] 0
	error_check_good dby_close [$dby close] 0
	error_check_good env_close [$e close] 0

	return [expr {($fail1 ? 1 : 0) + ($fail2 ? 1 : 0)}]
}
