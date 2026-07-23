# See the file LICENSE for redistribution information.
#
# Copyright (c) 2026 berkeleydb/libdb contributors.  All rights reserved.
#
# $Id$
#
# TEST	ssi003
# TEST	Serializable Snapshot Isolation: prepare() is rejected.
# TEST
# TEST	SSI's pivot check runs at commit time, but a prepared transaction
# TEST	must be guaranteed committable (upstream panics the environment if a
# TEST	prepared txn cannot commit).  Until SSI's conflict status is frozen at
# TEST	prepare time, DB_TXN_SNAPSHOT_SAFE + prepare() must be refused with an
# TEST	error -- never allowed to reach the commit-time panic path.
proc ssi003 { } {
	source ./include.tcl

	puts "Ssi003: SSI prepare() must be rejected, not panic the env"

	env_cleanup $testdir
	# _noerr: this test deliberately triggers an error (prepare rejection),
	# so open the env without the harness's FAIL errpfx/errfile.
	set e [berkdb_env_noerr -create -home $testdir \
	    -txn -lock -log -multiversion]
	error_check_good env_open [is_valid_env $e] TRUE
	set db [berkdb open -create -auto_commit -env $e -btree -multiversion a.db]
	error_check_good db_open [is_valid_db $db] TRUE
	error_check_good seed [$db put ka 0] 0

	puts "\tSsi003.a: prepare() on a snapshot-safe txn returns an error"
	set t1 [$e txn -snapshot_safe]
	error_check_good t1_w [$db put -txn $t1 ka 1] 0
	# prepare must fail; the txn is still live and must abort cleanly.
	set gid [make_gid ssi003:t1]
	set ret [catch {$t1 prepare $gid} res]
	error_check_good prepare_rejected $ret 1
	error_check_good t1_abort [$t1 abort] 0

	puts "\tSsi003.b: a plain (non-SSI) txn can still prepare"
	set t2 [$e txn]
	error_check_good t2_w [$db put -txn $t2 ka 2] 0
	error_check_good t2_prepare [$t2 prepare [make_gid ssi003:t2]] 0
	error_check_good t2_commit [$t2 commit] 0

	# The environment must still be alive (no panic).
	set t3 [$e txn -snapshot_safe]
	error_check_good t3_r [catch {$db get -txn $t3 ka} r] 0
	error_check_good t3_commit [$t3 commit] 0

	error_check_good db_close [$db close] 0
	error_check_good env_close [$e close] 0
}
