# See the file LICENSE for redistribution information.
#
# Copyright (c) 2026 berkeleydb/libdb contributors.  All rights reserved.
#
# $Id$
#
# TEST	mvcc001
# TEST	Force the MVCC freeze/thaw and cache-resize (grow) code paths.
# TEST
# TEST	Two cold subsystems that the functional suite never reaches:
# TEST	  1. src/mp/mp_mvcc.c __memp_bh_freeze / __memp_bh_thaw / __pgno_cmp.
# TEST	     These fire only when a multiversion cache fills up while old
# TEST	     page versions are still pinned by long-lived snapshot readers:
# TEST	     the old versions are spilled ("frozen") to a __db.freezer file
# TEST	     and read back ("thawed") when a reader needs them.  test120-122
# TEST	     exercise MVCC but never apply cache pressure, so freeze/thaw
# TEST	     stay at 0% even in a full test run.
# TEST	  2. src/mp/mp_resize.c region growth (__memp_add_region /
# TEST	     __memp_add_bucket / __memp_merge_buckets / __memp_map_regions /
# TEST	     __memp_resize / get/set_cache_max).  These fire when the cache
# TEST	     is grown past a region boundary after open, via resize_cache
# TEST	     (DB_ENV->set_cachesize post-open) with cache_max > 1 region.
# TEST
# TEST	mvcc001.a drives freeze/thaw: tiny multiversion cache, snapshot
# TEST	readers pinning the original versions, a writer churning the pages
# TEST	so old versions must freeze, then the readers read them back (thaw)
# TEST	and we assert they still see their original snapshot.
# TEST	mvcc001.b drives cache growth: a small multi-region cache with a
# TEST	larger cache_max, grown in steps so buffers are re-hashed into new
# TEST	regions, asserting the data survives every reshuffle.
# TEST
# TEST	NOTE: cache SHRINK (resize_cache to fewer regions) is deliberately
# TEST	NOT exercised -- it crashes (SIGSEGV) due to an off-by-one in
# TEST	__memp_remove_region (see test/coverage/MVCC-RESIZE-COVERAGE.md).

proc mvcc001 { } {
	puts "Mvcc001: MVCC freeze/thaw and cache-resize (grow) coverage"
	mvcc001_freeze_thaw
	mvcc001_resize_grow
}

# mp_mvcc.c: force __memp_bh_freeze / __memp_bh_thaw / __pgno_cmp.
proc mvcc001_freeze_thaw { } {
	source ./include.tcl

	puts "\tMvcc001.a: Force MVCC freeze/thaw under cache pressure"
	env_cleanup $testdir

	# Tiny cache + multiversion.  The cache must be small enough that the
	# writer's new page versions cannot coexist with the old versions the
	# snapshot readers pin (forcing freeze), but large enough that a frozen
	# buffer header and the currently-referenced buffers still fit (so
	# freeze can allocate).  512K with 512-byte pages is the sweet spot.
	# A large mutex set is required: every frozen buffer takes a mutex.
	set e [eval {berkdb_env_noerr -create -txn -multiversion \
	    -mutex_set_max 60000 -cachesize {0 524288 1} -home} $testdir]
	error_check_good env_open [is_valid_env $e] TRUE

	set db [eval {berkdb_open_noerr -create -auto_commit -env} $e \
	    {-btree -pagesize 512 mvcc.db}]
	error_check_good db_open [is_valid_db $db] TRUE

	set nentries 150
	set origdata [string repeat D 200]

	puts "\t\tMvcc001.a1: Seed $nentries records"
	for { set i 0 } { $i < $nentries } { incr i } {
		error_check_good put($i) [$db put key$i $origdata] 0
	}

	# Long-lived snapshot readers.  Each reads a sampling of the keys,
	# pinning the ORIGINAL version of those pages for the life of the txn.
	puts "\t\tMvcc001.a2: Start snapshot readers pinning old versions"
	set readers {}
	for { set r 0 } { $r < 2 } { incr r } {
		set t [$e txn -snapshot]
		for { set i 0 } { $i < $nentries } { incr i 10 } {
			eval {$db get -txn $t} key$i
		}
		lappend readers $t
	}

	# Writer churns every page many times in small committed transactions.
	# The readers still pin the originals, so each overwrite creates a new
	# version and the old one must be frozen out of the full cache.
	puts "\t\tMvcc001.a3: Churn pages so old versions freeze"
	for { set pass 0 } { $pass < 20 } { incr pass } {
		for { set base 0 } { $base < $nentries } { incr base 5 } {
			set wt [$e txn]
			for { set i $base } \
			    { $i < $base + 5 && $i < $nentries } { incr i } {
				error_check_good churn($pass.$i) \
				    [eval {$db put -txn $wt} \
				    key$i [string repeat V 180]] 0
			}
			error_check_good churn_commit($pass.$base) [$wt commit] 0
		}
	}

	set st [$e mpool_stat]
	set frozen [getstats $st {Buffers frozen}]
	set thawed [getstats $st {Buffers thawed}]
	puts "\t\tMvcc001.a4: $frozen buffers frozen, $thawed thawed so far"
	error_check_good froze_something [expr {$frozen > 0}] 1

	# Readers read their pinned keys back.  Any frozen old version must be
	# thawed from the freezer file, and each reader MUST still see its
	# original snapshot -- this is the correctness assertion under freeze.
	puts "\t\tMvcc001.a5: Readers thaw and re-verify their snapshot"
	foreach t $readers {
		for { set i 0 } { $i < $nentries } { incr i 10 } {
			set ret [eval {$db get -txn $t} key$i]
			set got [lindex [lindex $ret 0] 1]
			error_check_good snapshot_data($i) $got $origdata
		}
		error_check_good reader_commit [$t commit] 0
	}

	set st [$e mpool_stat]
	set thawed [getstats $st {Buffers thawed}]
	puts "\t\tMvcc001.a6: $thawed buffers thawed total"
	error_check_good thawed_something [expr {$thawed > 0}] 1

	# Current handle should now see the churned data.
	set ret [$db get key0]
	error_check_good current_data \
	    [lindex [lindex $ret 0] 1] [string repeat V 180]

	error_check_good db_close [$db close] 0
	error_check_good env_close [$e close] 0
}

# mp_resize.c: force cache growth (region add) + get/set_cache_max.
proc mvcc001_resize_grow { } {
	source ./include.tcl

	puts "\tMvcc001.b: Grow a multi-region cache after open"
	env_cleanup $testdir

	# Start with a small 2-region cache but allow growth to many regions.
	set e [eval {berkdb_env_noerr -create -txn \
	    -cache_max {0 16777216} -cachesize {0 1048576 2} -home} $testdir]
	error_check_good env_open [is_valid_env $e] TRUE

	set st [$e mpool_stat]
	set nc0 [getstats $st {Number of caches}]
	set maxc [getstats $st {Maximum number of caches}]
	puts "\t\tMvcc001.b1: Opened with $nc0 caches (max $maxc)"
	error_check_good multi_region [expr {$nc0 >= 2}] 1

	# get_cache_max returns the configured maximum (exercises the
	# MPOOL_ON branch of __memp_get_cache_max).
	set gcm [$e get_cache_max]
	error_check_good get_cache_max [expr {[llength $gcm] == 2}] 1

	set db [eval {berkdb_open_noerr -create -auto_commit -env} $e \
	    {-btree -pagesize 512 resize.db}]
	error_check_good db_open [is_valid_db $db] TRUE

	set nentries 800
	set data [string repeat D 200]
	puts "\t\tMvcc001.b2: Seed $nentries records"
	for { set i 0 } { $i < $nentries } { incr i } {
		error_check_good put($i) [$db put key$i $data] 0
	}

	# Grow the cache in steps.  Each region added forces the hash buckets
	# of the last region to be split and the buffers merged/copied into the
	# new region (__memp_add_region -> __memp_add_bucket ->
	# __memp_merge_buckets).  After every grow the data must be intact.
	puts "\t\tMvcc001.b3: Grow cache and verify data survives each step"
	foreach target { 2097152 3145728 4194304 8388608 } {
		error_check_good resize($target) \
		    [$e resize_cache [list 0 $target]] 0
		set st [$e mpool_stat]
		set nc [getstats $st {Number of caches}]
		puts "\t\t\tGrew to $nc caches (target $target bytes)"
		for { set i 0 } { $i < $nentries } { incr i } {
			set ret [$db get key$i]
			error_check_good resized_data($target.$i) \
			    [lindex [lindex $ret 0] 1] $data
		}
	}

	# Error path: asking for more regions than cache_max allows must fail
	# cleanly (the EINVAL branch of __memp_resize), not crash.
	puts "\t\tMvcc001.b4: Over-max resize fails cleanly"
	set ret [catch {$e resize_cache {1 0}} res]
	error_check_good overmax_fails $ret 1

	error_check_good db_close [$db close] 0
	error_check_good env_close [$e close] 0
}
