# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996, 1997
#	Sleepycat Software.  All rights reserved.
#
#	@(#)mpool.tcl	10.3 (Sleepycat) 10/4/97
#
# Options are:
# -cachesize <bytes>
# -nfiles <files>
# -iterations <iterations>
# -pagesize <page size in bytes>
# -dir <directory in which to store memp>
# -stat
proc memp_usage {} {
	puts "memp -cachesize <bytes> -nfiles <files> -iterations <iterations>"
	puts "\t-pagesize <page size in bytes> -dir <memp directory>"
	return
}
proc mpool { args } {
	source ./include.tcl

# Set defaults
	set cachesize [expr 50 * 1024]
	set nfiles 5
	set iterations 500
	set pagesize "512 1024 2048 4096 8192"
	set npages 100
	set procs 4
	set seeds ""
	set dostat 0
	for { set i 0 } { $i < [llength $args] } {incr i} {
		switch -regexp -- [lindex $args $i] {
			-c.* { incr i; set cachesize [lindex $args $i] }
			-d.* { incr i; set testdir [lindex $args $i] }
			-i.* { incr i; set iterations [lindex $args $i] }
			-nf.* { incr i; set nfiles [lindex $args $i] }
			-np.* { incr i; set npages [lindex $args $i] }
			-pa.* { incr i; set pagesize [lindex $args $i] }
			-pr.* { incr i; set procs [lindex $args $i] }
			-se.* { incr i; set seeds [lindex $args $i] }
			-st.* { set dostat 1 }
			default {
				memp_usage
				return
			}
		}
	}
	if { [file exists $testdir] != 1 } {
		exec $MKDIR $testdir
	} elseif { [file isdirectory $testdir ] != 1 } {
		error "$testdir is not a directory"
	}

	# Clean out old directory
	cleanup $testdir

	# Open the memp (figure out how to handle DBINFO)

	set mp [ memp "" 0644 $DB_CREATE -cachesize $cachesize ]
	memp001 $mp $testdir $nfiles $iterations [lindex $pagesize 0] $dostat
	error_check_good memp_close [$mp close] 0
	error_check_good memp_unlink:$testdir [memp_unlink $testdir 1] 0
	set mp [ memp "" 0644 $DB_CREATE -cachesize $cachesize ]
	error_check_good memp [is_substr $mp mp] 1
	error_check_good memp_close [$mp close] 0
	memp002 $testdir $procs $pagesize $iterations $npages $seeds $dostat
	error_check_good memp_unlink:$testdir [memp_unlink $testdir 1] 0
	memp003 $iterations
	error_check_good memp_unlink:$testdir [memp_unlink $testdir 1] 0
}

proc memp001 {mp dir n iter psize dostat} {
	source ./include.tcl

	puts "Memp001: random update $iter iterations on $n files."
	# Open N memp files
	for {set i 1} {$i <= $n} {incr i} {
		set fname "data_file.$i"
		file_create $dir/$fname 50 $psize
		set files($i) [$mp open $fname $psize 0 0644]
	}
	srand 0xf0f0f0f0

	# Now, loop, picking files at random
	for {set i 0} {$i < $iter} {incr i} {
		set f $files([random_int 1 $n])
		set p1 [get_range $f 10]
		set p2 [get_range $f 10]
		set p3 [get_range $f 10]
		replace $p1
		replace $p3
		set p4 [get_range $f 20]
		replace $p4
		set p5 [get_range $f 10]
		set p6 [get_range $f 20]
		set p7 [get_range $f 10]
		set p8 [get_range $f 20]
		replace $p5
		replace $p6
		set p9 [get_range $f 40]
		replace $p2
		set p10 [get_range $f 40]
		replace $p7
		replace $p8
		replace $p9
		replace $p10
	}
	if { $dostat == 1 } {
		$mp stat
		for {set i 1} {$i <= $n} {incr i} {
			error_check_good mp_sync [$files($i) sync] 0
#			error_check_good mp_close [$files($i) close] 0
		}
	}

	# Close N memp files
	for {set i 1} {$i <= $n} {incr i} {
		error_check_good memp_close:$files($i) [$files($i) close] 0
		exec $RM -rf $dir/data_file.$i
	}
}

proc file_create { fname nblocks blocksize } {
	set fid [open $fname w]
	for {set i 0} {$i < $nblocks} {incr i} {
		seek $fid [expr $i * $blocksize] start
		puts -nonewline $fid $i
	}
	seek $fid [expr $nblocks * $blocksize - 1]
	puts $fid ""
	close $fid
}


proc get_range { file max } {
	set pno [random_int 0 $max]
	set p [$file get $pno 0 ]
	set got [$p get]
	if { $got != $pno } {
#	if {[string compare $got $pno] != 0} \{
		puts "Get_range: Page mismatch page |$pno| val |$got|"
	}
	$p init "Page is pinned by [pid]"
	return $p
}

proc replace { p } {
global DB_MPOOL_DIRTY
	$p init "Page is unpinned by [pid]"
	$p put $DB_MPOOL_DIRTY
}

proc memp002 { dir procs psizes iterations npages seeds dostat } {
	source ./include.tcl

	puts "Memp002: Multiprocess mpool tester"
	set iter [expr $iterations / $procs]

	# Clean up old stuff and create new.
	lock_unlink $dir 1
	for { set i 0 } { $i < [llength $psizes] } { incr i } {
		exec $RM -rf $dir/file$i
	}
	set lp [lock_open "" $DB_CREATE 0644]
	$lp close

	set pidlist {}
	for { set i 0 } { $i < $procs } {incr i} {
		if { [llength $seeds] == $procs } {
			set seed [lindex $seeds $i]
		} else {
			set seed -1
		}
		puts "./dbtest ../test/mpoolscript.tcl $dir $i $procs \
		    $iter \"$psizes\" $npages 3 $seed  > $i.mpoolout &"
		set p [exec ./dbtest ../test/mpoolscript.tcl $dir $i $procs \
		    $iter $psizes $npages 3 $seed  > $i.mpoolout & ]
		lappend pidlist $p
	}
	puts "Memp002: $procs independent processes now running"
	watch_procs $pidlist
	# Remove output logs
	for { set i 0 } { $i < $procs } {incr i} {
		exec $RM $i.mpoolout
	}
	lock_unlink $dir 1
}

# Test reader-only/writer process combinations; we use the access methods
# for testing.
proc memp003 { {nentries 10000} } {
global alphabet
	source ./include.tcl
	puts "Memp003: Reader/Writer tests"

	cleanup $testdir
	set env_flags [expr $DB_CREATE | $DB_INIT_LOCK | $DB_INIT_MPOOL]
	set psize 1024
	set testfile mpool.db
	set t1 $testdir/t1

	# Create an environment that the two processes can share
	set dbenv [dbenv -dbflags $env_flags \
	    -dbhome $testdir -cachesize [expr $psize * 10]]

	# First open and create the file.

	set db [dbopen $testfile [expr $DB_CREATE | $DB_TRUNCATE] \
	    0644 DB_BTREE -dbenv $dbenv -psize $psize]
	error_check_good dbopen/RW [is_substr $db db] 1

	set did [open $dict]
	set flags 0
	set txn 0
	set count 0

	puts "\tMemp003.a: create database"
	set keys ""
	# Here is the loop where we put and get each key/data pair
	while { [gets $did str] != -1 && $count < $nentries } {
		lappend keys $str

		set ret [$db put $txn $str $str $flags]
		error_check_good put $ret 0

		set ret [$db get $txn $str $flags]
		error_check_good get $ret $str

		incr count
	}
	close $did
	error_check_good close [$db close] 0

	# Now open the file for read-only
	set db [dbopen $testfile $DB_RDONLY 0 DB_UNKNOWN -dbenv $dbenv]
	error_check_good dbopen/RO [is_substr $db db] 1

	puts "\tMemp003.b: verify a few keys"
	# Read and verify a couple of keys; saving them to check later
	set testset ""
	for { set i 0 } { $i < 10 } { incr i } {
		set ndx [random_int 0 [expr $nentries - 1]]
		set key [lindex $keys $ndx]
		if { [lsearch $testset $key] != -1 } {
			incr i -1
			continue;
		}
		lappend testset $key

		set ret [$db get $txn $key $flags]
		error_check_good get/RO $ret $key
	}

	puts "\tMemp003.c: retrieve and modify keys in remote process"
	# Now open remote process where we will open the file RW
	set f1 [open |./dbtest r+]
	puts $f1 "set dbenv \[dbenv -dbflags $env_flags -dbhome $testdir\
	    -cachesize [expr $psize * 10]\]"
	puts $f1 "puts \$dbenv"
	puts $f1 "flush stdout"
	flush $f1

	set r [gets $f1 result]
	error_check_bad gets $r -1
	error_check_good remote_dbenv [is_substr $result env] 1

	puts $f1 "set db \[dbopen $testfile 0 0 DB_UNKNOWN -dbenv \$dbenv\]"
	puts $f1 "puts \$db"
	puts $f1 "flush stdout"
	flush $f1

	set r [gets $f1 result]
	error_check_bad gets $r -1
	error_check_good remote_dbopen [is_substr $result db] 1

	foreach k $testset {
		# Get the key
		puts $f1 "set ret \[\$db get 0 $k 0\]"
		puts $f1 "puts \$ret"
		puts $f1 "flush stdout"
		flush $f1

		set r [gets $f1 result]
		error_check_bad gets $r -1
		error_check_good remote_get $result $k

		# Now replace the key
		puts $f1 "set ret \[\$db put 0 $k $k$k 0\]"
		puts $f1 "puts \$ret"
		puts $f1 "flush stdout"
		flush $f1

		set r [gets $f1 result]
		error_check_bad gets $r -1
		error_check_good remote_put $result 0
	}

	puts "\tMemp003.d: verify changes in local process"
	foreach k $testset {
		set ret [$db get $txn $key $flags]
		error_check_good get_verify/RO $ret $key$key
	}

	puts "\tMemp003.e: Fill up the cache with dirty buffers"
	foreach k $testset {
		# Now rewrite the keys with BIG data
		set data [replicate $alphabet 32]
		puts $f1 "set ret \[\$db put 0 $k $data 0\]"
		puts $f1 "puts \$ret"
		puts $f1 "flush stdout"
		flush $f1

		set r [gets $f1 result]
		error_check_bad gets $r -1
		error_check_good remote_put $result 0
	}

	puts "\tMemp003.f: Get more pages for the read-only file"
	dump_file $db $txn $t1 nop

	puts "\tMemp003.g: Sync from the read-only file"
	error_check_good db_sync [$db sync 0] 0
	error_check_good db_close [$db close] 0

	puts $f1 "set ret \[\$db close\]"
	puts $f1 "puts \$ret"
	puts $f1 "flush stdout"
	flush $f1

	set r [gets $f1 result]
	error_check_bad gets $r -1
	error_check_good remote_get $result 0
	close $f1
}
