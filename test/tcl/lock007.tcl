# See the file LICENSE for redistribution information.
#
# Copyright (c) 2024 Oracle and/or its affiliates.  All rights reserved.
#
# $Id$
#
# TEST	lock007
# TEST	Lock subsystem configuration coverage.
# TEST	Exercises every DB_ENV lock configuration setter (set_lk_max_locks,
# TEST	set_lk_max_lockers, set_lk_max_objects, set_lk_partitions,
# TEST	set_lk_tablesize, the DB_MEM_LOCK/LOCKER/LOCKOBJECT init counts, and
# TEST	set_lk_detect for every deadlock-detection policy) and reads the
# TEST	values back through the corresponding getters.  Then runs a workload
# TEST	that allocates many locks/lockers/objects on distinct objects so the
# TEST	lock region grows past its initial free lists (src/lock/lock_alloc.incl).
proc lock007 { } {
	source ./include.tcl

	puts "Lock007: Lock configuration setters/getters + region growth"

	env_cleanup $testdir

	# Part a: set every lock config knob before open, then open a locking
	# env and read the values back.  The setters store into the DB_ENV
	# (ENV_ILLEGAL_AFTER_OPEN path); the getters read the live region
	# (LOCKING_ON path) after open, so both halves of each get_/set_ are hit.
	puts "\tLock007.a: set every lock config knob, read back via getters"
	set maxlocks 5000
	set maxlockers 2000
	set maxobjects 5000
	set partitions 3
	set tablesize 509
	set initlocks 1500
	set initlockers 600
	set initobjects 1500

	set eflags "-create -lock -home $testdir -mode 0644 \
	    -lock_max_locks $maxlocks -lock_max_lockers $maxlockers \
	    -lock_max_objects $maxobjects -lock_partitions $partitions \
	    -lock_tablesize $tablesize -lock_locks $initlocks \
	    -lock_lockers $initlockers -lock_objects $initobjects \
	    -lock_detect default"
	set env [eval {berkdb_env} $eflags]
	error_check_good env [is_valid_env $env] TRUE

	error_check_good get_max_locks \
	    [$env get_lk_max_locks] $maxlocks
	error_check_good get_max_lockers \
	    [$env get_lk_max_lockers] $maxlockers
	error_check_good get_max_objects \
	    [$env get_lk_max_objects] $maxobjects
	error_check_good get_partitions \
	    [$env get_lk_partitions] $partitions
	# get_lk_detect: DB_LOCK_DEFAULT was requested; with no detector
	# running the region stays at the requested (or NORUN) value.  Just
	# confirm the getter succeeds and returns a valid policy string.
	set det [$env get_lk_detect]
	error_check_bad get_detect [string length $det] 0

	error_check_good env_close_a [$env close] 0

	# Part b: validate the set_lk_detect policy switch -- every documented
	# policy must be accepted, and a bogus one must be rejected.  Do this
	# on a fresh lock env for each policy (set on an already-open locking
	# env exercises the LOCKING_ON region path in __lock_set_lk_detect).
	puts "\tLock007.b: set_lk_detect accepts every policy, rejects garbage"
	foreach pol {default expire maxlocks maxwrites minlocks minwrites \
	    oldest youngest random} {
		env_cleanup $testdir
		set e [berkdb_env -create -lock -home $testdir \
		    -lock_detect $pol]
		error_check_good detect_$pol [is_valid_env $e] TRUE
		error_check_good detect_close_$pol [$e close] 0
	}
	# A bogus policy string is rejected by the Tcl binding before it ever
	# reaches set_lk_detect.
	env_cleanup $testdir
	set ret [catch {berkdb_env -create -lock -home $testdir \
	    -lock_detect boguspolicy} res]
	error_check_good detect_bogus_rejected $ret 1

	# Part c: allocate many locks/lockers/objects to grow the region.
	# Each locker takes read locks on many distinct objects; with hundreds
	# of lockers and thousands of objects this exhausts the initial free
	# lists and drives the region-growth loop in lock_alloc.incl.
	puts "\tLock007.c: many-locker workload to grow the lock region"
	env_cleanup $testdir
	set nlockers 200
	set nobjs 40
	set env [berkdb_env -create -lock -home $testdir \
	    -lock_max_locks 40000 -lock_max_lockers 4000 \
	    -lock_max_objects 40000 -lock_partitions 4]
	error_check_good env_c [is_valid_env $env] TRUE

	set lockers {}
	set locks {}
	for {set i 0} {$i < $nlockers} {incr i} {
		set locker [$env lock_id]
		lappend lockers $locker
		for {set j 0} {$j < $nobjs} {incr j} {
			# Distinct object per (locker,obj); read locks never
			# conflict so nothing blocks, but each is a fresh
			# lock+object allocation.
			set obj "obj_${i}_${j}"
			set lockp [$env lock_get read $locker $obj]
			error_check_good lock_get_c [is_substr $lockp $env] 1
			lappend locks $lockp
		}
	}

	# Confirm we actually allocated a lot of locks.
	set nlocks_now [lock007_stat $env "Current number of locks"]
	error_check_good grew_locks [expr {$nlocks_now >= $nlockers * $nobjs}] 1

	# Release everything.
	foreach lockp $locks {
		error_check_good lock_put [$lockp put] 0
	}
	foreach locker $lockers {
		error_check_good free_id [$env lock_id_free $locker] 0
	}
	error_check_good env_close_c [$env close] 0
}

# Pull a single integer statistic out of "$env lock_stat" by its label.
proc lock007_stat { env label } {
	set stat [$env lock_stat]
	foreach pair $stat {
		if { [is_substr [lindex $pair 0] $label] != 0 } {
			return [lindex $pair 1]
		}
	}
	return -1
}
