# See the file LICENSE for redistribution information.
#
# Copyright (c) 2024 Oracle and/or its affiliates.  All rights reserved.
#
# $Id$
#
# TEST	test143
# TEST	Compressed-integer codec coverage (src/common/db_compint.c).
# TEST	Stores records into a -compress btree whose data sizes span the
# TEST	compressed-integer size classes the codec uses to marshal record
# TEST	lengths: 1-byte (size <= 127), 2-byte (size <= 16511) and 3-byte
# TEST	(size <= ~2M).  __bam_compress_marshal_data() runs __db_compress_int
# TEST	over data->size on write and __db_decompress_int32 on read, so a
# TEST	round-trip over these sizes drives the codec's 1/2/3-byte encode and
# TEST	decode paths.  Keys share long common prefixes so the prefix/suffix
# TEST	length compression is exercised too.  (The full 4-9 byte size classes
# TEST	are unit-tested exhaustively by test/pbt/pbt_compint.c, a separate
# TEST	property-based tier not run under the Tcl coverage subset.)
proc test143 { method {tnum "143"} args } {
	source ./include.tcl

	# Compression is btree-only.
	if { [is_btree $method] == 0 } {
		puts "Test$tnum skipping for method $method (btree only)."
		return
	}

	set args [convert_args $method $args]
	set omethod [convert_method $method]

	# Work in an env so we get a page/mpool big enough for overflow data.
	env_cleanup $testdir
	set env [berkdb_env -create -home $testdir -mode 0644 -cachesize {0 4194304 1}]
	error_check_good env [is_valid_env $env] TRUE

	set testfile test$tnum.db
	puts "Test$tnum: $method compressed-integer codec (varying record sizes)"

	set db [eval {berkdb_open -create -env $env -mode 0644} \
	    $args {-compress} $omethod $testfile]
	error_check_good dbopen [is_valid_db $db] TRUE

	# Data sizes chosen to land in each compressed-int size class for
	# data->size: 1 (1-byte), 100 (1-byte boundary), 200 (2-byte),
	# 16000 (2-byte near max), 20000 (3-byte), 100000 (3-byte).
	set sizes {1 50 100 127 128 200 5000 16000 16511 16512 20000 100000}

	puts "\tTest$tnum.a: put records spanning codec size classes"
	set n 0
	foreach sz $sizes {
		# Long shared key prefix so key prefix/suffix lengths compress.
		set key [format "commonkeyprefix_%08d" $n]
		set data [repeat "x" $sz]
		set ret [eval {$db put} {$key $data}]
		error_check_good put_$n $ret 0
		set expect($key) $data
		incr n
	}

	puts "\tTest$tnum.b: read back and verify (decompress path)"
	foreach key [array names expect] {
		set ret [$db get $key]
		error_check_good get_$key [llength $ret] 1
		set pair [lindex $ret 0]
		error_check_good key_$key [lindex $pair 0] $key
		error_check_good len_$key \
		    [string length [lindex $pair 1]] [string length $expect($key)]
		error_check_good data_$key [lindex $pair 1] $expect($key)
	}

	# Full cursor scan forces decompression of every compressed page.
	puts "\tTest$tnum.c: cursor scan of all compressed records"
	set dbc [$db cursor]
	error_check_good cursor [is_valid_cursor $dbc $db] TRUE
	set count 0
	for {set ret [$dbc get -first]} {[llength $ret] > 0} \
	    {set ret [$dbc get -next]} {
		set pair [lindex $ret 0]
		set k [lindex $pair 0]
		error_check_good scan_$k \
		    [string length [lindex $pair 1]] [string length $expect($k)]
		incr count
	}
	error_check_good scan_count $count [llength $sizes]
	error_check_good dbc_close [$dbc close] 0

	error_check_good db_close [$db close] 0
	error_check_good env_close [$env close] 0
}
