# See the file LICENSE for redistribution information.
#
# Copyright (c) 2026 berkeleydb/libdb contributors.  All rights reserved.
#
# $Id$
#
# TEST	ssi011
# TEST	A plain DB_TXN_SNAPSHOT (-snapshot) transaction can be prepared for
# TEST	two-phase commit, exactly as legacy Berkeley DB allowed.  Only
# TEST	DB_TXN_SERIALIZABLE (-snapshot_safe, SSI) is rejected by prepare().
# TEST
# TEST	This guards the restore of DB_TXN_SNAPSHOT to plain snapshot isolation:
# TEST	when snapshot silently meant SSI, prepare() rejected it; now it must be
# TEST	preparable again.
proc ssi011 { } {
	source ./include.tcl

	puts "Ssi011: plain -snapshot txn is preparable; -snapshot_safe is not"

	env_cleanup $testdir
	set e [berkdb_env_noerr -create -home $testdir \
	    -txn -lock -log -multiversion]
	error_check_good env_open [is_valid_env $e] TRUE
	set db [berkdb open -create -auto_commit -env $e -btree -multiversion a.db]
	error_check_good db_open [is_valid_db $db] TRUE
	error_check_good seed [$db put ka 0] 0

	puts "\tSsi011.a: prepare() on a plain -snapshot txn succeeds"
	set t1 [$e txn -snapshot]
	error_check_good t1_w [$db put -txn $t1 ka 1] 0
	error_check_good t1_prepare [$t1 prepare [make_gid ssi011:t1]] 0
	error_check_good t1_commit [$t1 commit] 0

	puts "\tSsi011.b: prepare() on a -snapshot_safe (SSI) txn is rejected"
	set t2 [$e txn -snapshot_safe]
	error_check_good t2_w [$db put -txn $t2 ka 2] 0
	set ret [catch {$t2 prepare [make_gid ssi011:t2]} res]
	error_check_good prepare_rejected $ret 1
	error_check_good t2_abort [$t2 abort] 0

	error_check_good db_close [$db close] 0
	error_check_good env_close [$e close] 0
}
