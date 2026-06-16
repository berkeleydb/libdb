# See the file LICENSE for redistribution information.
#
# Copyright (c) 2026 berkeleydb/libdb contributors.  All rights reserved.
#
# $Id$
#
# TEST	ssi002
# TEST	Serializable Snapshot Isolation: no false-positive aborts.
# TEST
# TEST	Two snapshot-safe transactions that touch disjoint data, and a
# TEST	read-only snapshot-safe transaction, must all commit.  SSI must only
# TEST	abort genuine dangerous structures, never independent transactions.
proc ssi002 { } {
	source ./include.tcl

	puts "Ssi002: SSI must not abort non-conflicting transactions"

	env_cleanup $testdir
	set e [berkdb_env -create -home $testdir \
	    -txn -lock -log -multiversion -lock_timeout 2000000]
	error_check_good env_open [is_valid_env $e] TRUE
	set dba [berkdb open -create -auto_commit -env $e -btree -multiversion a.db]
	set dbb [berkdb open -create -auto_commit -env $e -btree -multiversion b.db]
	error_check_good a_open [is_valid_db $dba] TRUE
	error_check_good b_open [is_valid_db $dbb] TRUE
	error_check_good seed_a [$dba put ka 0] 0
	error_check_good seed_b [$dbb put kb 0] 0

	puts "\tSsi002.a: disjoint read/write sets both commit"
	set t1 [$e txn -snapshot_safe]
	set t2 [$e txn -snapshot_safe]
	# t1 works only on a.db, t2 only on b.db -- no shared items.
	error_check_good t1_ra [catch {$dba get -txn $t1 ka} r] 0
	error_check_good t2_rb [catch {$dbb get -txn $t2 kb} r] 0
	error_check_good t1_wa [$dba put -txn $t1 ka 1] 0
	error_check_good t2_wb [$dbb put -txn $t2 kb 1] 0
	error_check_good t1_commit [$t1 commit] 0
	error_check_good t2_commit [$t2 commit] 0

	puts "\tSsi002.b: a read-only snapshot-safe txn commits"
	set t3 [$e txn -snapshot_safe]
	error_check_good t3_ra [catch {$dba get -txn $t3 ka} r] 0
	error_check_good t3_rb [catch {$dbb get -txn $t3 kb} r] 0
	error_check_good t3_commit [$t3 commit] 0

	puts "\tSsi002.c: read-then-write on the same item by one txn commits"
	set t4 [$e txn -snapshot_safe]
	error_check_good t4_r [catch {$dba get -txn $t4 ka} r] 0
	error_check_good t4_w [$dba put -txn $t4 ka 2] 0
	error_check_good t4_commit [$t4 commit] 0

	error_check_good a_close [$dba close] 0
	error_check_good b_close [$dbb close] 0
	error_check_good env_close [$e close] 0
}
