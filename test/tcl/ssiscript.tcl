# See the file LICENSE for redistribution information.
#
# Copyright (c) 2026 berkeleydb/libdb contributors.  All rights reserved.
#
# $Id$
#
# Worker for ssi009: hammer a shared multiversion database with snapshot-safe
# read-then-write transactions from one process.  Usage:
#   ssiscript.tcl <dir> <nkeys> <iters>
source ./include.tcl
source $test_path/testutils.tcl

set usage "ssiscript.tcl dir nkeys iters"
if { $argc != 3 } { puts stderr "FAIL: usage: $usage"; exit 1 }
set dir   [lindex $argv 0]
set nkeys [lindex $argv 1]
set iters [lindex $argv 2]

set e [berkdb_env -home $dir -txn -lock -log -multiversion \
    -lock_detect default]
error_check_good ssiscript_env [is_valid_env $e] TRUE
set db [berkdb open -auto_commit -env $e -btree -multiversion ssi.db]
error_check_good ssiscript_db [is_valid_db $db] TRUE

# Each iteration: begin snapshot_safe, read one key, write another as a
# function of the read (a real read/write dependency), commit.  SSI conflicts
# and deadlocks are expected and simply retried; the point is that concurrent
# writers never crash or corrupt the environment.
for { set i 0 } { $i < $iters } { incr i } {
	set rk k[berkdb random_int 0 [expr $nkeys - 1]]
	set wk k[berkdb random_int 0 [expr $nkeys - 1]]
	set t [$e txn -snapshot_safe]
	if { [catch { $db get -txn $t $rk } r] } { catch {$t abort}; continue }
	set v 0
	if { [llength $r] > 0 } { set v [lindex [lindex $r 0] 1] }
	if { [catch { $db put -txn $t $wk [expr $v + 1] } ] } {
		catch {$t abort}; continue
	}
	catch { $t commit }
}

error_check_good ssiscript_db_close [$db close] 0
error_check_good ssiscript_env_close [$e close] 0
