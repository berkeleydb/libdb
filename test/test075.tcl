# See the file LICENSE for redistribution information.
#
# Copyright (c) 2000-2001
#	Sleepycat Software.  All rights reserved.
#
# $Id: test075.tcl,v 11.14 2001/09/10 13:02:35 bostic Exp $
#
# TEST	test075
# TEST	Test of DB->rename().
# TEST	(formerly test of DB_TRUNCATE cached page invalidation [#1487])
proc test075 { method { tnum 75 } args } {
	global errorCode
	source ./include.tcl
	set omethod [convert_method $method]
	set args [convert_args $method $args]

	puts "Test0$tnum: $method ($args): Test of DB->rename()"

	# Define absolute pathnames
	set curdir [pwd]
	cd $testdir
	set fulldir [pwd]
	cd $curdir
	set reldir $testdir

	# Set up absolute and relative pathnames for test
	set paths [list $fulldir $reldir]
	foreach path $paths {
		# If we are using an env, then testfile should just be the
		# db name.  Otherwise it is the test directory and the name.
		set eindex [lsearch -exact $args "-env"]
		if { $eindex == -1 } {
			set oldfile $path/test0$tnum-old.db
			set newfile $path/test0$tnum.db
			set env NULL
			set renargs ""
		} else {
			set oldfile test0$tnum-old.db
			set newfile test0$tnum.db
			# File existence checks won't work in an env, since
			# $oldfile and $newfile won't be in the current
			# working directory.  We use this to skip them, and
			# turn our secondary check (opening the dbs and seeing
			# that all is well) into the main one.
			incr eindex
			set env [lindex $args $eindex]
			set renargs " -env $env"
		}

		# Make sure we're starting with a clean slate.
		cleanup $path $env

		puts "\tTest0$tnum: starting test of $path path"
		if { $env == "NULL" } {
			error_check_bad \
			   "$oldfile exists" [file exists $oldfile] 1
			error_check_bad \
			   "$newfile exists" [file exists $newfile] 1
		}

		puts "\tTest0$tnum.a: Create/rename file"
		puts "\t\tTest0$tnum.a.1: create"
		set db [eval \
		   {berkdb_open -create -mode 0644} $omethod $args $oldfile]
		error_check_good dbopen [is_valid_db $db] TRUE

		if { $env == "NULL" } {
			error_check_bad \
			   "$oldfile exists" [file exists $oldfile] 0
			error_check_bad \
			   "$newfile exists" [file exists $newfile] 1
		}

		# The nature of the key and data are unimportant; use numeric
		# key so record-based methods don't need special treatment.
		set key 1
		set data [pad_data $method data]

		error_check_good dbput [$db put $key $data] 0
		error_check_good dbclose [$db close] 0

		puts "\t\tTest0$tnum.a.2: rename"
		if { $env == "NULL" } {
			error_check_bad \
			    "$oldfile exists" [file exists $oldfile] 0
			error_check_bad \
			    "$newfile exists" [file exists $newfile] 1
		}
		error_check_good rename_file [eval {berkdb dbrename} \
			$renargs $oldfile $newfile] 0
		if { $env == "NULL" } {
			error_check_bad \
			    "$oldfile exists" [file exists $oldfile] 1
			error_check_bad \
			    "$newfile exists" [file exists $newfile] 0
		}

		puts "\t\tTest0$tnum.a.3: check"
		# Open again with create to make sure we're not caching or
		# anything silly.  In the normal case (no env), we already
		# know the file doesn't exist.
		set odb [eval \
		    {berkdb_open -create -mode 0644} $omethod $args $oldfile]
		set ndb [eval \
		    {berkdb_open -create -mode 0644} $omethod $args $newfile]
		error_check_good odb_open [is_valid_db $odb] TRUE
		error_check_good ndb_open [is_valid_db $ndb] TRUE

		set odbt [$odb get $key]
		set ndbt [$ndb get $key]

		# The DBT from the "old" database should be empty, not the
		# "new" one.
		error_check_good odbt_empty [llength $odbt] 0
		error_check_bad ndbt_empty [llength $ndbt] 0

		error_check_good ndbt [lindex [lindex $ndbt 0] 1] $data

		error_check_good odb_close [$odb close] 0
		error_check_good ndb_close [$ndb close] 0

		if { $env != "NULL" } {
			puts "\tTest0$tnum: External environment present; \
			    skipping remainder"
			continue
		}

		# Now there's both an old and a new.  Rename the "new" to
		# the "old" and make sure that fails.
		#
		# XXX Ideally we'd do this test even when there's an external
		# environment, but that env has errpfx/errfile set now.  :-(
		puts \
	"\tTest0$tnum.b: Make sure rename fails instead of overwriting"
		set ret [catch \
		    {eval {berkdb dbrename} $renargs $newfile $oldfile} res]
		error_check_bad rename_overwrite $ret 0
		error_check_good \
		    rename_overwrite_ret [is_substr $errorCode EEXIST] 1

		# Verify and then start over from a clean slate.
		verify_dir $path "\tTest0$tnum.c: "
		cleanup $path $env
		error_check_bad "$oldfile exists" [file exists $oldfile] 1
		error_check_bad "$newfile exists" [file exists $newfile] 1

		set oldfile test0$tnum-old.db
		set newfile test0$tnum.db

		puts "\tTest0$tnum.d: Create/rename file in environment"

		env_cleanup $path
		set env [berkdb env -create -home $path]
		error_check_good env_open [is_valid_env $env] TRUE
		error_check_bad "$oldfile exists" [file exists $oldfile] 1
		error_check_bad "$newfile exists" [file exists $newfile] 1

		puts "\t\tTest0$tnum.d.1: create"
		set db [eval {berkdb_open -create -mode 0644} -env $env\
			$omethod $args $oldfile]
		error_check_good dbopen [is_valid_db $db] TRUE

		# We need to make sure that it didn't create/rename into the
		# current directory.
		error_check_bad "$oldfile exists" [file exists $oldfile] 1
		error_check_bad "$newfile exists" [file exists $newfile] 1
		error_check_bad "$path/$oldfile exists"\
		    [file exists $path/$oldfile] 0
		error_check_bad "$path/$newfile exists"\
		    [file exists $path/$newfile] 1

		error_check_good dbput [$db put $key $data] 0
		error_check_good dbclose [$db close] 0

		puts "\t\tTest0$tnum.d.2: rename"

		error_check_good rename_file [berkdb dbrename -env $env\
		    $oldfile $newfile] 0
		error_check_bad "$oldfile exists" [file exists $oldfile] 1
		error_check_bad "$newfile exists" [file exists $newfile] 1
		error_check_bad "$path/$oldfile exists"\
		    [file exists $path/$oldfile] 1
		error_check_bad "$path/$newfile exists"\
		    [file exists $path/$newfile] 0

		puts "\t\tTest0$tnum.d.3: check"
		# Open again with create to make sure we're not caching or
		# anything silly.
		set odb [eval {berkdb_open -create -mode 0644} -env $env\
		    $omethod $args $oldfile]
		set ndb [eval {berkdb_open -create -mode 0644} -env $env\
		    $omethod $args $newfile]
		error_check_good odb_open [is_valid_db $odb] TRUE
		error_check_good ndb_open [is_valid_db $ndb] TRUE

		set odbt [$odb get $key]
		set ndbt [$ndb get $key]

		# The DBT from the "old" database should be empty, not the
		# "new" one.
		error_check_good odbt_empty [llength $odbt] 0
		error_check_bad ndbt_empty [llength $ndbt] 0

		error_check_good ndbt [lindex [lindex $ndbt 0] 1] $data

		error_check_good odb_close [$odb close] 0
		error_check_good ndb_close [$ndb close] 0

		# XXX
		# We need to close and reopen the env since berkdb_open has
		# set its errfile/errpfx, and we can't unset that.
		error_check_good env_close [$env close] 0
		set env [berkdb env -home $path]
		error_check_good env_open2 [is_valid_env $env] TRUE

		puts "\tTest0$tnum.e:\
		    Make sure rename fails instead of overwriting in env"
		set ret [catch \
		    {eval {berkdb dbrename} -env $env $newfile $oldfile} res]
		error_check_bad rename_overwrite $ret 0
		error_check_good \
		    rename_overwrite_ret [is_substr $errorCode EEXIST] 1
		cleanup $path $env
		error_check_good env_close [$env close] 0
	}
	puts "\tTest0$tnum succeeded."
}
