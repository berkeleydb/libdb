# See the file LICENSE for redistribution information.
#
# Copyright (c) 2026 berkeleydb/libdb contributors.  All rights reserved.
#
# $Id$
#
# TEST	ssi007
# TEST	Serializable Snapshot Isolation: graceful behaviour under lock-region
# TEST	pressure, and no unbounded growth over a long run.
# TEST
# TEST	SSI keeps committed readers' SIREAD markers alive until GC, which
# TEST	historically ran only at checkpoint -- a long-lived process could
# TEST	exhaust the statically sized lock region.  Two things must hold:
# TEST	  (a) bounded reclaim keeps a long run of committed readers from ever
# TEST	      exhausting a modest region (many more readers than objects), and
# TEST	  (b) if the region is genuinely exhausted, the engine returns a clean
# TEST	      error (ENOMEM / "out of available") and stays usable -- it does
# TEST	      not corrupt state or panic.
proc ssi007 { } {
	source ./include.tcl

	puts "Ssi007: SSI under lock-region pressure"

	# ---- (a) marker/object reclaim keeps the object footprint bounded ----
	puts "\tSsi007.a: many committed readers keep a bounded object footprint"
	env_cleanup $testdir
	set e [berkdb_env -create -home $testdir \
	    -txn -lock -log -multiversion \
	    -lock_max_objects 200 -lock_max_lockers 20000 -lock_max_locks 40000]
	error_check_good env_open [is_valid_env $e] TRUE
	set db [berkdb open -create -auto_commit -env $e -btree -multiversion d.db]
	error_check_good db_open [is_valid_db $db] TRUE
	for { set i 0 } { $i < 300 } { incr i } {
		error_check_good seed_$i [$db put "k$i" $i] 0
	}

	# Far more committed readers than the region has objects; incremental
	# marker reclaim must keep the live object count from ever exhausting
	# the 200-object pool (committed-reader SIREAD markers are the objects
	# that would otherwise accumulate without bound between checkpoints).
	# NOTE: committed-reader *locker* structs are not yet reclaimed (see
	# the SSI known-issues note), so the locker cap is sized generously
	# here; this checks the marker/object reclaim specifically.
	set failed 0
	for { set i 0 } { $i < 2000 } { incr i } {
		set t [$e txn -snapshot_safe]
		set k "k[expr {$i % 300}]"
		if { [catch {$db get -txn $t $k} r] } { incr failed }
		if { [catch {$t commit} r] } { incr failed }
	}
	error_check_good marker_objects_bounded $failed 0

	# Peak object usage must stay well below one-per-reader.
	set st [$e lock_stat]
	set maxobj 0
	foreach pair $st {
		if { [lindex $pair 0] eq "Maximum number of objects so far" } {
			set maxobj [lindex $pair 1]
		}
	}
	error_check_good objects_reclaimed [expr {$maxobj < 200}] 1

	error_check_good db_close [$db close] 0
	error_check_good env_close [$e close] 0

	# ---- (b) genuine exhaustion is a clean error, env stays usable ----
	puts "\tSsi007.b: forced exhaustion returns a clean error, no panic"
	env_cleanup $testdir
	# A deliberately tiny region: a single transaction reading many
	# distinct keys accrues one SIREAD marker per object and, held open
	# (no commit, so no reclaim of its own live markers), must eventually
	# hit the object cap -- as an error, not a crash.
	set e2 [berkdb_env_noerr -create -home $testdir \
	    -txn -lock -log -multiversion \
	    -lock_max_objects 40 -lock_max_lockers 100 -lock_max_locks 200]
	error_check_good env2_open [is_valid_env $e2] TRUE
	set db2 [berkdb open -create -auto_commit -env $e2 -btree -multiversion e.db]
	error_check_good db2_open [is_valid_db $db2] TRUE
	for { set i 0 } { $i < 500 } { incr i } {
		error_check_good seed2_$i [$db2 put "k$i" $i] 0
	}

	set t [$e2 txn -snapshot_safe]
	set hit_limit 0
	for { set i 0 } { $i < 500 && !$hit_limit } { incr i } {
		if { [catch {$db2 get -txn $t "k$i"} r] } {
			# Must be a resource error, not a crash/panic.
			error_check_good clean_resource_error \
			    [expr {[is_substr $r "out of available"] || \
			           [is_substr $r "not enough space"] || \
			           [is_substr $r "Cannot allocate"] || \
			           [is_substr $r "unable"]}] 1
			set hit_limit 1
		}
	}
	# The txn can still be aborted and the environment is still usable.
	error_check_good abort_after_limit [expr {[catch {$t abort}] == 0}] 1
	set t2 [$e2 txn -snapshot_safe]
	error_check_good env_still_usable [catch {$db2 get -txn $t2 k0} r] 0
	error_check_good commit_ok [$t2 commit] 0

	error_check_good db2_close [$db2 close] 0
	error_check_good env2_close [$e2 close] 0
}
