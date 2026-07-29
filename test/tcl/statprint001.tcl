# See the file LICENSE for redistribution information.
#
# Copyright (c) 2026 berkeleydb/libdb contributors.  All rights reserved.
#
# $Id$
#
# TEST	statprint001
# TEST	Exercise every stat_print (verbose / DB_STAT_ALL) code path.
# TEST
# TEST	env020 already covers the Tcl stat_print bindings, but two cold
# TEST	spots remain and the db_stat *utility* entry point (util/db_stat.c ->
# TEST	__*_stat_print) is never exercised by the Tcl suite at all:
# TEST	  1. heap_stat.c (0%): env020 opens no heap database.
# TEST	  2. dbreg_stat.c __dbreg_print_all (cold): reached only with
# TEST	     DB_STAT_ALL | DB_STAT_SUBSYSTEM set together and databases open;
# TEST	     env020 passes those flags separately, never combined.
# TEST	  3. the db_stat utility drives the same __*_stat_print functions
# TEST	     through a separate (read-only, on-disk) code path.
# TEST
# TEST	This test closes those gaps: a heap DB stat_print sweep, an
# TEST	env stat_print -all -subsystem call with live open DBs (dbreg), and a
# TEST	db_stat utility sweep over a populated all-subsystems env with every
# TEST	meaningful flag combination.

proc statprint001 { } {
	puts "Statprint001: stat_print verbose / DB_STAT_ALL coverage sweep"
	statprint001_heap
	statprint001_dbreg
	statprint001_dbstat_util
}

# heap_stat.c: env020 never opens a heap DB.  Drive stat_print with the
# default, -fast (DB_FAST_STAT) and -all (DB_STAT_ALL) flags.
proc statprint001_heap { } {
	source ./include.tcl
	# Substrings emitted by __heap_stat_print (src/heap/heap_stat.c).
	set pattern "Heap magic number"

	puts "\tStatprint001: DB->stat_print for heap"
	env_cleanup $testdir
	set env [eval berkdb_env_noerr -create -home $testdir]
	error_check_good is_valid_env [is_valid_env $env] TRUE

	foreach {opt tag} {"" default -fast fast -all all} {
		puts "\t\tUsing the $tag option"
		set db [eval berkdb_open_noerr -create -env $env -heap \
		    -msgfile $testdir/msgfile_$tag heap_$tag.db]
		error_check_good is_valid_db [is_valid_db $db] TRUE
		# Put a few records so the region/page counts are non-trivial.
		for {set i 1} {$i <= 20} {incr i} {
			set ret [$db put -append [chop_data heap "data$i"]]
			error_check_good "heap put $i" [expr {$ret > 0}] 1
		}
		error_check_good heap_stat_print [eval $db stat_print $opt] 0
		error_check_good "$db close" [$db close] 0
		# Confirm __heap_stat_print emitted its output.
		set found 0
		set f [open $testdir/msgfile_$tag r]
		while {[gets $f line] >= 0} {
			if {[string first $pattern $line] >= 0} { set found 1; break }
		}
		close $f
		error_check_good heap_stat_output_$tag $found 1
		file delete -force $testdir/msgfile_$tag
	}
	error_check_good "$env close" [$env close] 0
}

# dbreg_stat.c __dbreg_print_all: reached only via env stat_print with
# DB_STAT_ALL | DB_STAT_SUBSYSTEM set together AND databases open (so the
# LOG FNAME list has entries to iterate).  env020 passes -all and -subsystem
# separately, never combined -- this leaves __dbreg_print_all cold.
proc statprint001_dbreg { } {
	source ./include.tcl
	# Substring that only appears in __dbreg_print_all's output.
	set pattern "LOG FNAME list:"

	puts "\tStatprint001: env stat_print -all -subsystem (dbreg)"
	env_cleanup $testdir
	set env [eval berkdb_env_noerr -create -txn -lock -log \
	    -home $testdir -msgfile $testdir/dbregmsg]
	error_check_good is_valid_env [is_valid_env $env] TRUE

	# Open (and keep open) several DBs so they are registered with dbreg.
	set dbs {}
	foreach am {btree hash recno} {
		set db [eval berkdb_open_noerr -create -env $env -$am \
		    -auto_commit dbreg_$am.db]
		error_check_good is_valid_db_$am [is_valid_db $db] TRUE
		set k [expr {$am eq "recno" ? 1 : "k1"}]
		error_check_good put_$am \
		    [$db put $k [chop_data $am data1]] 0
		lappend dbs $db
	}

	# The combined flags that hit __dbreg_print_all.
	error_check_good env_stat_all_sub \
	    [$env stat_print -all -subsystem] 0
	$env msgfile /dev/stdout

	# Confirm __dbreg_print_all actually ran and iterated the FNAME list.
	set found 0
	set f [open $testdir/dbregmsg r]
	while {[gets $f line] >= 0} {
		if {[string first $pattern $line] >= 0} { set found 1; break }
	}
	close $f
	error_check_good dbreg_print_all_ran $found 1

	foreach db $dbs { error_check_good db_close [$db close] 0 }
	error_check_good "$env close" [$env close] 0
	file delete -force $testdir/dbregmsg
}

# db_stat utility sweep: drives util/db_stat.c -> __*_stat_print through the
# read-only on-disk entry path with every meaningful flag combination.  This
# is the cheapest way to hit all the __*_stat_print branches at once (env,
# lock, log, mpool, mutex, rep, txn) including DB_STAT_ALL/SUBSYSTEM (-E) which
# reaches __dbreg_stat_print, DB_STAT_MEMP_HASH (-Mh) and DB_STAT_CLEAR (-Z).
proc statprint001_dbstat_util { } {
	source ./include.tcl
	global util_path

	puts "\tStatprint001: db_stat utility flag sweep"
	env_cleanup $testdir

	# Build a fully-populated all-subsystems env (rep enabled so rep_stat
	# has something to print), then close the DBs so db_stat can open the
	# env read-only.
	set env [eval berkdb_env_noerr -create -txn -lock -log -rep -home $testdir]
	error_check_good is_valid_env [is_valid_env $env] TRUE
	foreach am {btree hash recno queue} {
		if {$am eq "queue"} {
			set db [eval berkdb_open_noerr -create -env $env -$am \
			    -len 64 -pad 0 -auto_commit stat_$am.db]
		} else {
			set db [eval berkdb_open_noerr -create -env $env -$am \
			    -auto_commit stat_$am.db]
		}
		error_check_good is_valid_db_$am [is_valid_db $db] TRUE
		for {set i 1} {$i <= 50} {incr i} {
			set k [expr {$am eq "queue" || $am eq "recno" ? $i : "key$i"}]
			error_check_good put_$am \
			    [$db put $k [chop_data $am "data$i"]] 0
		}
		error_check_good db_close_$am [$db close] 0
	}
	error_check_good txn_chkpt [$env txn_checkpoint] 0
	error_check_good "$env close" [$env close] 0

	# Every db_stat flag combination that drives a __*_stat_print.  Each is
	# expected to exit 0 and emit non-empty output.
	set flagsets {
		{-e}				{-E}
		{-c}				{-C A}		{-C clop}
		{-l}				{-L A}
		{-m}				{-M A}		{-M h}
		{-x}				{-X A}
		{-r}				{-R A}
		{-t}
		{-Z -e}				{-Z -c}		{-Z -l}
		{-Z -m}				{-Z -t}
		{-d stat_btree.db}		{-d stat_btree.db -f}
		{-d stat_hash.db}		{-d stat_recno.db}
		{-d stat_queue.db}
	}
	foreach fs $flagsets {
		set ret [catch {eval exec $util_path/db_stat \
		    -h $testdir $fs} output]
		error_check_good "db_stat $fs rc" $ret 0
		error_check_good "db_stat $fs output" \
		    [expr {[string length $output] > 0}] 1
	}
}
