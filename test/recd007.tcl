# See the file LICENSE for redistribution information.
#
# Copyright (c) 1999
#	Sleepycat Software.  All rights reserved.
#
#	@(#)recd007.tcl	11.16 (Sleepycat) 11/10/99
#
# Recovery Test 7.
# This is a recovery test for create/delete of databases.  We have
# hooks in the database so that we can abort the process at various
# points and make sure that the transaction doesn't commit.  We
# then need to recover and make sure the file is correctly existing
# or not, as the case may be.
proc recd007 { method {select 0} } {
	global fixed_len
	source ./include.tcl

	set orig_fixed_len $fixed_len
	set fixed_len 1024
	set opts [convert_args $method ""]
	set omethod [convert_method $method]

	puts "Recd007: $method operation/transaction tests"

	# Create the database and environment.
	cleanup $testdir

	set testfile recd007.db
	set flags "-create -log -lock -mpool -txn -home $testdir"

	puts "\tRecd007.a: creating environment"
	set env_cmd "berkdb env $flags"

	#
	# List of recovery tests: {HOOKS MSG} pairs
	# Where each HOOK is a list of {COPY ABORT}
	#
	set rlist {
	{ {"none" "preopen"}		"Recd007.b0: none/preopen"}
	{ {"none" "postopen"}		"Recd007.b1: none/postopen"}
	{ {"none" "postlogmeta"}	"Recd007.b2: none/postlogmeta"}
	{ {"none" "postlog"}		"Recd007.b3: none/postlog"}
	{ {"none" "postsync"}		"Recd007.b4: none/postsync"}
	{ {"postopen" "none"}		"Recd007.c0: postopen/none"}
	{ {"postlogmeta" "none"}	"Recd007.c1: postlogmeta/none"}
	{ {"postlog" "none"}		"Recd007.c2: postlog/none"}
	{ {"postsync" "none"}		"Recd007.c3: postsync/none"}
	{ {"postopen" "postopen"}	"Recd007.d: postopen/postopen"}
	{ {"postopen" "postlogmeta"}	"Recd007.e: postopen/postlogmeta"}
	{ {"postopen" "postlog"}	"Recd007.f: postopen/postlog"}
	{ {"postlog" "postlog"}		"Recd007.g: postlog/postlog"}
	{ {"postlogmeta" "postlogmeta"}	"Recd007.h: postlogmeta/postlogmeta"}
	{ {"postlogmeta" "postlog"}	"Recd007.i: postlogmeta/postlog"}
	{ {"postlog" "postsync"}	"Recd007.j: postlog/postsync"}
	{ {"postsync" "postsync"}	"Recd007.k: postsync/postsync"}
	}

	# These are all the data values that we're going to need to read
	# through the operation table and run the recovery tests.

	foreach pair $rlist {
		set cmd [lindex $pair 0]
		set msg [lindex $pair 1]
		file_recover_create $testdir $env_cmd $omethod \
		    $opts $testfile $cmd $msg
	}

	set rlist {
	{ {"none" "prerename"}		"Recd007.l0: none/prerename"}
	{ {"none" "postrename"}		"Recd007.l1: none/postrename"}
	{ {"prerename" "none"}		"Recd007.m0: prerename/none"}
	{ {"postrename" "none"}		"Recd007.m1: postrename/none"}
	{ {"prerename" "prerename"}	"Recd007.n: prerename/prerename"}
	{ {"prerename" "postrename"}	"Recd007.o: prerename/postrename"}
	{ {"postrename" "postrename"}	"Recd007.p: postrename/postrename"}
	}
	foreach pair $rlist {
		set cmd [lindex $pair 0]
		set msg [lindex $pair 1]
		file_recover_delete $testdir $env_cmd $omethod \
		    $opts $testfile $cmd $msg
	}

	puts "\tRecd007.q: Verify db_printlog can read logfile"
	set tmpfile $testdir/printlog.out
	set stat [catch {exec ./db_printlog -h $testdir > $tmpfile} ret]
	error_check_good db_printlog $stat 0
	exec $RM $tmpfile
}

# Run a recovery test for a particular operation
# Notice that we catch the return from CP and do not do anything with it.
# This is because Solaris CP seems to exit non-zero on occasion, but
# everything else seems to run just fine.
proc file_recover_create { dir env_cmd method opts dbfile cmd msg } {
	#
	# We run this test on each of these scenarios:
	# 1.  Creating just a database
	# 2.  Creating a database with a subdb
	# 3.  Creating a 2nd subdb in a database
	puts "\t$msg create with a database"
	do_file_recover_create $dir $env_cmd $method $opts $dbfile \
	    0 $cmd $msg
	if { [is_queue $method] == 1 } {
		puts "\tSkipping subdatabase tests for method $method"
		return
	}
	puts "\t$msg create with a database and subdb"
	do_file_recover_create $dir $env_cmd $method $opts $dbfile \
	    1 $cmd $msg
	puts "\t$msg create with a database and 2nd subdb"
	do_file_recover_create $dir $env_cmd $method $opts $dbfile \
	    2 $cmd $msg

}


proc do_file_recover_create { dir env_cmd method opts dbfile sub cmd msg } {
	global log_log_record_types
	source ./include.tcl

	# Keep track of the log types we've seen
	if { $PERL5 != "" && \
	    $log_log_record_types == 1} {
		set err \
		    [catch {exec $PERL5 "$test_path/log.pl" "--read" $dir} ret]
		error_check_good "Saving log record types" $err 0
	}


	cleanup $dir
	# Open the environment and set the copy/abort locations
	set env [eval $env_cmd]
	set copy [lindex $cmd 0]
	set abort [lindex $cmd 1]
	error_check_good copy_location [is_valid_create_loc $copy] 1
	error_check_good abort_location [is_valid_create_loc $abort] 1

	if {([string first "logmeta" $copy] != -1 || \
	    [string first "logmeta" $abort] != -1) && \
	    [is_btree $method] == 0 } {
		puts "\tSkipping for method $method"
		$env test copy none
		$env test abort none
		error_check_good env_close [$env close] 0
		return
	}

	#
	# Basically non-existence is our initial state.  When we
	# abort, it is also our final state.
	#
	switch $sub {
		0 {
			set oflags "-create $method -mode 0644 \
			    -env $env $opts $dbfile"
		}
		1 {
			set oflags "-create $method -mode 0644 \
			    -env $env $opts $dbfile sub0"
		}
		2 {
			#
			# If we are aborting here, then we need to
			# create a first subdb, then create a second
			#
			set oflags "-create $method -mode 0644 \
			    -env $env $opts $dbfile sub0"
			set db [eval {berkdb open} $oflags]
			error_check_good db_open [is_valid_db $db] TRUE
			error_check_good db_close [$db close] 0
			set init_file $dir/$dbfile.init
			catch { exec $CP $dir/$dbfile $init_file } res
			set oflags "-create $method -mode 0644 \
			    -env $env $opts $dbfile sub1"
		}
		default {
			puts "\tBad value $sub for sub"
			return
		}
	}
	#
	# Set our locations to copy and abort
	#
	set ret [eval $env test copy $copy]
	error_check_good test_copy $ret 0
	set ret [eval $env test abort $abort]
	error_check_good test_abort $ret 0

	set ret [catch {eval {berkdb open} $oflags} db]
	puts "\t\tCommand executed"

	# Sync the mpool so any changes to the file that are
	# in mpool get written to the disk file before the
	# diff.
	$env mpool_sync "0 0"

	#
	# If we don't abort, then we expect success.
	# If we abort, we expect no file created.
	#
	if {[string first "none" $abort] == -1} {
		#
		# Operation was aborted, verify it does
		# not exist.
		#
		puts "\t\tCommand executed and aborted."
		error_check_bad db_open ret 0

		#
		# Check that the file does not exist.  Final state.
		#
		if { $sub != 2 } {
			error_check_good db_open:exists \
			    [file exists $dir/$dbfile] 0
		} else {
			error_check_good \
			    diff(init,postcreate):diff($init_file,$dir/$dbfile)\
		    	    [dbdump_diff $init_file $dir/$dbfile] 0
		}
	} else {
		#
		# Operation was committed, verify it exists.
		#
		puts "\t\tCommand executed and committed."
		error_check_good db_open [is_valid_db $db] TRUE
		error_check_good db_close [$db close] 0

		#
		# Check that the file exists.
		#
		error_check_good db_open [file exists $dir/$dbfile] 1
		set init_file $dir/$dbfile.init
		catch { exec $CP $dir/$dbfile $init_file } res
	}
	error_check_good env_close [$env close] 0

	#
	# Run recovery here.  Should be a no-op.  Verify that
	# the file still doesn't exist or change (depending on sub)
	# when we are done.
	#
	berkdb debug_check
	puts -nonewline "\t\tAbout to run recovery ... "
	flush stdout

	set stat [catch {exec ./db_recover -h $dir -c} result]
	if { $stat == 1 } {
		error "FAIL: Recovery error: $result."
		return
	}
	puts "complete"
	if { $sub != 2 && [string first "none" $abort] == -1} {
		#
		# Operation was aborted, verify it still does
		# not exist.  Only done with file creations.
		#
		error_check_good after_recover1 [file exists $dir/$dbfile] 0
	} else {
		#
		# Operation was committed or just a subdb was aborted.
		# Verify it did not change.
		#
		error_check_good \
		    diff(initial,post-recover1):diff($init_file,$dir/$dbfile) \
		    [dbdump_diff $init_file $dir/$dbfile] 0
		#
		# Need a new copy to get the right LSN into the file.
		#
		catch { exec $CP $dir/$dbfile $init_file } res
	}

	#
	# If we didn't make a copy, then we are done.
	#
	if {[string first "none" $copy] != -1} {
		return
	}

	#
	# Now move the .afterop file to $dbfile.  Run recovery again.
	#
	exec $MV $dir/$dbfile.afterop $dir/$dbfile
	berkdb debug_check
	puts -nonewline "\t\tAbout to run recovery ... "
	flush stdout

	set stat [catch {exec ./db_recover -h $dir -c} result]
	if { $stat == 1 } {
		error "FAIL: Recovery error: $result."
		return
	}
	puts "complete"
	if { $sub != 2 && [string first "none" $abort] == -1} {
		#
		# Operation was aborted, verify it still does
		# not exist.  Only done with file creations.
		#
		error_check_good after_recover2 [file exists $dir/$dbfile] 0
	} else {
		#
		# Operation was committed or just a subdb was aborted.
		# Verify it did not change.
		#
		error_check_good \
		    diff(initial,post-recover2):diff($init_file,$dir/$dbfile) \
		    [dbdump_diff $init_file $dir/$dbfile] 0
	}

}

# Run a recovery test for a particular operation
# Notice that we catch the return from CP and do not do anything with it.
# This is because Solaris CP seems to exit non-zero on occasion, but
# everything else seems to run just fine.
proc file_recover_delete { dir env_cmd method opts dbfile cmd msg } {
	#
	# We run this test on each of these scenarios:
	# 1.  Deleting just a database
	# 2.  Deleting a database with a subdb
	# 3.  Deleting a 2nd subdb in a database
	puts "\t$msg delete with a database"
	do_file_recover_delete $dir $env_cmd $method $opts $dbfile \
	    0 $cmd $msg
	if { [is_queue $method] == 1 } {
		puts "\tSkipping subdatabase tests for method $method"
		return
	}
	puts "\t$msg delete with a database and subdb"
	do_file_recover_delete $dir $env_cmd $method $opts $dbfile \
	    1 $cmd $msg
	puts "\t$msg delete with a database and 2nd subdb"
	do_file_recover_delete $dir $env_cmd $method $opts $dbfile \
	    2 $cmd $msg

}

proc do_file_recover_delete { dir env_cmd method opts dbfile sub cmd msg } {
	global log_log_record_types
	source ./include.tcl

	# Keep track of the log types we've seen
	if { $PERL5 != "" && \
	    $log_log_record_types == 1} {
		set err \
		    [catch {exec $PERL5 "$test_path/log.pl" "--read" $dir} ret]
		error_check_good "Saving log record types" $err 0
	}


	cleanup $dir
	# Open the environment and set the copy/abort locations
	set env [eval $env_cmd]
	set copy [lindex $cmd 0]
	set abort [lindex $cmd 1]
	error_check_good copy_location [is_valid_delete_loc $copy] 1
	error_check_good abort_location [is_valid_delete_loc $abort] 1

	if { [is_record_based $method] == 1 } {
		set key 1
	} else {
		set key recd007_key
	}
	set data1 recd007_data
	set data2 NEWrecd007_data2

	#
	# Depending on what sort of subdb we want, if any, our
	# args to the open call will be different (and if we
	# want a 2nd subdb, we create the first here.
	#
	switch $sub {
		0 {
			set oflags "-create $method -mode 0644 \
			    -env $env $opts $dbfile"
		}
		1 {
			set oflags "-create $method -mode 0644 \
			    -env $env $opts $dbfile sub0"
		}
		2 {
			#
			# If we are aborting here, then we need to
			# create a first subdb, then create a second
			#
			set oflags "-create $method -mode 0644 \
			    -env $env $opts $dbfile sub0"
			set db [eval {berkdb open} $oflags]
			error_check_good db_open [is_valid_db $db] TRUE
			set ret [$db put $key $data2]
			error_check_good db_put $ret 0
			error_check_good db_close [$db close] 0
			set oflags "-create $method -mode 0644 \
			    -env $env $opts $dbfile sub1"
		}
		default {
			puts "\tBad value $sub for sub"
			return
		}
	}

	#
	# Set our locations to copy and abort
	#
	set ret [eval $env test copy $copy]
	error_check_good test_copy $ret 0
	set ret [eval $env test abort $abort]
	error_check_good test_abort $ret 0

	#
	# Open our db, add some data, close and copy as our
	# init file.
	#
	set db [eval {berkdb open} $oflags]
	error_check_good db_open [is_valid_db $db] TRUE
	set ret [$db put $key $data1]
	error_check_good db_put $ret 0
	error_check_good db_close [$db close] 0

	set init_file $dir/$dbfile.init
	catch { exec $CP $dir/$dbfile $init_file } res

	#
	# If we don't abort, then we expect success.
	# If we abort, we expect no file removed.
	#
	set ret [catch { berkdb dbremove -env $env $dbfile } remret]
	if {[string first "none" $abort] == -1} {
		#
		# Operation was aborted, verify it did not change.
		#
		puts "\t\tCommand executed and aborted."
		error_check_good dbremove $ret 1

		#
		# Check that the file exists.  Final state.
		# Compare against initial file.
		#
		error_check_good postdbremove1 [file exists $dir/$dbfile] 1
		error_check_good \
		    diff(init,postdbremove2):diff($init_file,$dir/$dbfile)\
		    [dbdump_diff $init_file $dir/$dbfile] 0
	} else {
		#
		# Operation was committed, verify it does
		# not exist.
		#
		puts "\t\tCommand executed and committed."
		error_check_good dbremove $ret 0
		#
		# Check that the file does not exist.
		#
		error_check_good dbremove [file exists $dir/$dbfile] 0
	}
	error_check_good env_close [$env close] 0
	catch { exec $CP $dir/$dbfile $init_file } res

	#
	# Run recovery here.  Should be a no-op.  Verify that
	# the file still doesn't exist or change (depending on abort)
	# when we are done.
	#
	berkdb debug_check
	puts -nonewline "\t\tAbout to run recovery ... "
	flush stdout

	set stat [catch {exec ./db_recover -h $dir -c} result]
	if { $stat == 1 } {
		error "FAIL: Recovery error: $result."
		return
	}
	puts "complete"
	if { [string first "none" $abort] != -1} {
		#
		# Operation was committed, verify it still does
		# not exist.
		#
		error_check_good after_recover1 [file exists $dir/$dbfile] 0
	} else {
		#
		# Operation was aborted, verify it did not change.
		#
		error_check_good \
		    diff(initial,post-recover1):diff($init_file,$dir/$dbfile) \
		    [dbdump_diff $init_file $dir/$dbfile] 0
	}

	#
	# If we didn't make a copy, then we are done.
	#
	if {[string first "none" $copy] != -1} {
		return
	}

	#
	# Now move the .afterop file to $dbfile.  Run recovery again.
	#
	set filecopy [glob $dir/*.afterop]
	if { [llength $filecopy] != 1 } {
		error "FAIL: Could not find 1 copy file.  Got: $filecopy"
		return
	}
	set afterop [lindex $filecopy 0]
	exec $MV $afterop $dir/$dbfile

	berkdb debug_check
	puts -nonewline "\t\tAbout to run recovery ... "
	flush stdout

	set stat [catch {exec ./db_recover -h $dir -c} result]
	if { $stat == 1 } {
		error "FAIL: Recovery error: $result."
		return
	}
	puts "complete"

	if { [string first "none" $abort] != -1} {
		#
		# Operation was committed, verify it still does
		# not exist.
		#
		error_check_good after_recover2 [file exists $dir/$dbfile] 0
	} else {
		#
		# Operation was aborted, verify it did not change.
		#
		error_check_good \
		    diff(initial,post-recover2):diff($init_file,$dir/$dbfile) \
		    [dbdump_diff $init_file $dir/$dbfile] 0
	}

}

proc is_valid_create_loc { loc } {
	switch $loc {
		none 		-
		preopen		-
		postopen	-
		postlogmeta	-
		postlog		-
		postsync
			{ return 1 }
		default
			{ return 0 }
	}
}

proc is_valid_delete_loc { loc } {
	switch $loc {
		none 		-
		prerename	-
		postrename	-
		postremcall
			{ return 1 }
		default
			{ return 0 }
	}
}

# Do a logical diff on the db dump files.  We expect that either
# the files are identical, or if they differ, that it is exactly
# just a free/invalid page.
# Return 1 if they are different, 0 if logically the same (or identical).
#
proc dbdump_diff { initfile dbfile } {
	source ./include.tcl

	set initdump $initfile.dump
	set dbdump $dbfile.dump

	set stat [catch {exec ./db_dump -dar -f $initdump $initfile} ret]
	error_check_good dbdump.init $stat 0

	# Do a dump without the freelist which should eliminate any
	# recovery differences.
	set stat [catch {exec ./db_dump -dar -f $dbdump $dbfile} ret]
	error_check_good dbdump.db $stat 0

	set stat [catch {exec $CMP $dbdump $initdump} ret]

	if {$stat == 0} {
		return 0
	}
	puts "diff: $dbdump $initdump gives:\n$ret"
	return 1
}
