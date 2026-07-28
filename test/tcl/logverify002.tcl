# See the file LICENSE for redistribution information.
#
# Copyright (c) 2026 berkeleydb/libdb contributors.  All rights reserved.
#
# $Id$
#
# TEST	logverify002
# TEST	Log verification must DETECT a corrupted log file.
# TEST
# TEST	Generates a healthy multi-file log, then corrupts the body of an
# TEST	interior physical log file (flipping bytes past the file header so
# TEST	live log-record checksums and headers no longer validate) and runs
# TEST	db_log_verify.  The verifier must NOT crash and must emit the
# TEST	corruption-detection diagnostics ("checksum mismatch" / "invalid log
# TEST	record header" / "Invalid log file"), exercising the error-detection
# TEST	branches in src/log/log_verify_int.c and the logc/get checksum path.
# TEST	Also runs one clean baseline verification for contrast.
proc logverify002 { } {
	source ./include.tcl
	global util_path

	puts "Logverify002: Log verification detects a corrupted log"

	env_cleanup $testdir

	# Small log_max so we roll several files; corrupt an interior one.
	set e [berkdb_env -create -home $testdir -txn -lock -log -log_max 65536]
	error_check_good env_open [is_valid_env $e] TRUE
	set db [berkdb open -create -auto_commit -env $e -btree corrupt.db]
	error_check_good db_open [is_valid_db $db] TRUE

	set val [string repeat "logverify002-payload-" 3]
	for { set i 0 } { $i < 1500 } { incr i } {
		set t [$e txn]
		error_check_good t_begin [is_valid_txn $t $e] TRUE
		error_check_good put [$db put -txn $t key$i $val$i] 0
		error_check_good commit [$t commit] 0
		if { $i % 150 == 0 } {
			error_check_good ckp [$e txn_checkpoint] 0
		}
	}
	error_check_good db_close [$db close] 0
	error_check_good env_close [$e close] 0

	set logs [lsort [glob -nocomplain $testdir/log.*]]
	error_check_good multiple_logfiles [expr {[llength $logs] > 3}] 1

	puts "\tLogverify002.a: Clean log verifies SUCCESSfully (baseline)"
	error_check_good clean_ok [verify_log $testdir] 0

	puts "\tLogverify002.b: Corrupt the body of an interior log file"
	# Pick an interior file (not the first, whose header/metadata the
	# verifier reads specially; not the last, which may be a short tail).
	set target [lindex $logs [expr {[llength $logs] / 2}]]
	corrupt_logfile_body $target

	puts "\tLogverify002.c: db_log_verify must DETECT the corruption"
	# db_log_verify skips unreadable records and can still print its final
	# "SUCCEEDED" banner, so success here is measured by the presence of
	# corruption diagnostics on stderr, not by the exit status.
	set tmp_dir lgverify_dir2
	file delete -force $tmp_dir
	file mkdir $tmp_dir
	set logfile lgvrfy002.log
	set ret [catch {eval exec $util_path/db_log_verify \
	    {-C 10} {-h $testdir} {-H $tmp_dir} >& $logfile} msg]

	# Whatever the exit status, it must not have crashed/segfaulted.
	if { $ret } {
		error_check_good no_crash \
		    [expr {[is_substr $msg "child killed"] == 0 && \
		    [is_substr $msg "SIGSEGV"] == 0 && \
		    [is_substr $msg "core dumped"] == 0}] 1
	}

	set fh [open $logfile r]
	set out [read $fh]
	close $fh

	set detected [expr { \
	    [is_substr $out "checksum mismatch"] || \
	    [is_substr $out "invalid log record header"] || \
	    [is_substr $out "Invalid log file"] || \
	    [is_substr $out "magic number"] }]
	error_check_good corruption_detected $detected 1

	# It must NOT falsely claim clean success amid the corruption:
	# the corruption diagnostics above are the pass condition.
	puts "\tLogverify002.d: Corruption diagnostics present"
}

# Corrupt the body of a log file: leave the persistent file header intact
# (first 512 bytes) so the file is still opened, but perturb the record
# region so live checksums/headers fail to validate.
proc corrupt_logfile_body { path } {
	set fh [open $path r+]
	fconfigure $fh -translation binary
	set data [read $fh]
	set len [string length $data]
	# Corrupt from offset 512 (past the log file header) up to ~40KB.
	set start 512
	set end [expr {$len < 40000 ? $len : 40000}]
	if { $end <= $start } {
		set start 0
		set end $len
	}
	# Read the byte range as an integer list, perturb each byte, write back.
	# binary scan/format keeps this byte-accurate (no Unicode mangling).
	binary scan [string range $data $start [expr {$end - 1}]] cu* codes
	set new {}
	foreach code $codes {
		lappend new [expr {($code + 0x37) & 0xFF}]
	}
	seek $fh $start start
	puts -nonewline $fh [binary format cu* $new]
	close $fh
}
