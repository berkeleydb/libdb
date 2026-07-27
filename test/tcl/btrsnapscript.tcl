# See the file LICENSE for redistribution information.
#
# Copyright (c) 2026 berkeleydb/libdb contributors.  All rights reserved.
#
# $Id$
#
# Worker for bt_rsnap001: exercise the lock-free root-snapshot read descent.
# Two roles:
#   reader  -- point-get known keys under read txns; every value must equal
#              f(key).  A stale child from the optimistic path shows up as a
#              wrong value (or a crash).
#   churner -- repeatedly grow then shrink the tree so the root splits and
#              merges, changing the live root LSN under the readers.
# Usage: btrsnapscript.tcl <role> <dir> <nkeys> <iters>
source ./include.tcl
source $test_path/testutils.tcl

set usage "btrsnapscript.tcl role dir nkeys iters"
if { $argc != 4 } { puts stderr "FAIL: usage: $usage"; exit 1 }
set role  [lindex $argv 0]
set dir   [lindex $argv 1]
set nkeys [lindex $argv 2]
set iters [lindex $argv 3]

# f(key): the value stored for a resident key is a fixed function of the key,
# so a reader can check correctness without coordination.
proc valfor { i } { return "v[format %08d $i]payload" }

set e [berkdb_env -home $dir -txn -lock -log -lock_detect default]
error_check_good bs_env [is_valid_env $e] TRUE
set db [berkdb open -auto_commit -env $e -btree bt.db]
error_check_good bs_db [is_valid_db $db] TRUE

if { $role == "reader" } {
	for { set i 0 } { $i < $iters } { incr i } {
		# Only the low, always-resident keys are checked; the churner
		# adds/removes high keys.  A read of a resident key must return
		# exactly valfor(key) -- the optimistic descent must never route
		# a lookup to a stale/reused child.
		set k [berkdb random_int 0 [expr $nkeys - 1]]
		set t [$e txn]
		if { [catch { $db get -txn $t k[format %08d $k] } r] } {
			catch { $t abort }; continue
		}
		catch { $t commit }
		if { [llength $r] == 0 } {
			puts stderr "FAIL: missing resident key $k"; exit 1
		}
		set got [lindex [lindex $r 0] 1]
		set want [valfor $k]
		if { $got != $want } {
			puts stderr "FAIL: key $k got '$got' want '$want'"
			exit 1
		}
	}
} else {
	# Churn high keys in and out to force root splits/merges.  Each batch
	# is one txn so every commit bumps the root LSN.  A deadlock (or any
	# error) on an operation MUST abort the whole txn and retry it -- using
	# or committing a txn after a failed operation is illegal and corrupts
	# the tree.
	# Churn a bounded high-key range in and out to force root splits/merges.
	# The range is sized to push the tree between 3 and 4 levels each cycle
	# (a genuine root split/merge) while still completing quickly -- a
	# runaway range (e.g. nkeys*20) makes a single batch take minutes, so
	# watch_procs would SIGKILL the churner mid-transaction and post-run
	# verify would then see a crashed, unrecovered tree (false corruption).
	set hi [expr $nkeys + 4000]
	proc batch { e db op lo hi } {
		while { 1 } {
			set t [$e txn]
			set failed 0
			for { set k $lo } { $k < $hi } { incr k } {
				if { $op == "put" } {
					set rc [catch { $db put -txn $t \
					    k[format %08d $k] [valfor $k] } ]
				} else {
					set rc [catch { $db del -txn $t \
					    k[format %08d $k] } ]
				}
				if { $rc != 0 } { set failed 1; break }
			}
			if { $failed } {
				catch { $t abort }
				continue
			}
			if { [catch { $t commit }] } { continue }
			return
		}
	}
	for { set i 0 } { $i < $iters } { incr i } {
		batch $e $db put $nkeys $hi
		batch $e $db del $nkeys $hi
	}
}

error_check_good bs_db_close [$db close] 0
error_check_good bs_env_close [$e close] 0
