# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996, 1997, 1998
#	Sleepycat Software.  All rights reserved.
#
#	@(#)join.tcl	8.8 (Sleepycat) 10/27/98
#
# We'll test 2-way, 3-way, and 4-way joins and figure that if those work,
# everything else does as well.  We'll create test databases called
# join1.db, join2.db, join3.db, and join4.db.  The number on the database
# describes the duplication -- duplicates are of the form 0, N, 2N, 3N, ...
# where N is the number of the database.  Primary.db is the primary database,
# and null.db is the database that has no matching duplicates.
#
# We should test this on all btrees, all hash, and a combination thereof
# Join test.
proc jointest { {psize 8192} {flags 0} } {
source ./include.tcl
	foreach m "DB_HASH DB_BTREE DB_BOTH" {
		cleanup $testdir
		build_all $m $psize

		# Build the primary
		puts "Jointest: Building the primary database $m"
		set db [dbopen primary.db [expr $DB_CREATE | $DB_TRUNCATE] \
		    0644 [conv $m [random_int 1 2]]]
		error_check_good dbopen [is_valid_db $db] TRUE
		for { set i 0 } { $i < 1000 } { incr i } {
			set key [format "%04d" $i]
			set ret [$db put 0 $key stub 0]
			error_check_good "primary put" $ret 0
		}
		error_check_good "primary close" [$db close] 0
		set did [open $dict]
		gets $did str
		do_join primary.db "1 0" $str $flags
		gets $did str
		do_join primary.db "2 0" $str $flags
		gets $did str
		do_join primary.db "3 0" $str $flags
		gets $did str
		do_join primary.db "4 0" $str $flags
		gets $did str
		do_join primary.db "1" $str $flags
		gets $did str
		do_join primary.db "2" $str $flags
		gets $did str
		do_join primary.db "3" $str $flags
		gets $did str
		do_join primary.db "4" $str $flags
		gets $did str
		do_join primary.db "1 2" $str $flags
		gets $did str
		do_join primary.db "1 2 3" $str $flags
		gets $did str
		do_join primary.db "1 2 3 4" $str $flags
		gets $did str
		do_join primary.db "2 1" $str $flags
		gets $did str
		do_join primary.db "3 2 1" $str $flags
		gets $did str
		do_join primary.db "4 3 2 1" $str $flags
		gets $did str
		do_join primary.db "1 3" $str $flags
		gets $did str
		do_join primary.db "3 1" $str $flags
		gets $did str
		do_join primary.db "1 4" $str $flags
		gets $did str
		do_join primary.db "4 1" $str $flags
		gets $did str
		do_join primary.db "2 3" $str $flags
		gets $did str
		do_join primary.db "3 2" $str $flags
		gets $did str
		do_join primary.db "2 4" $str $flags
		gets $did str
		do_join primary.db "4 2" $str $flags
		gets $did str
		do_join primary.db "3 4" $str $flags
		gets $did str
		do_join primary.db "4 3" $str $flags
		gets $did str
		do_join primary.db "2 3 4" $str $flags
		gets $did str
		do_join primary.db "3 4 1" $str $flags
		gets $did str
		do_join primary.db "4 2 1" $str $flags
		gets $did str
		do_join primary.db "0 2 1" $str $flags
		gets $did str
		do_join primary.db "3 2 0" $str $flags
		gets $did str
		do_join primary.db "4 3 2 1" $str $flags
		gets $did str
		do_join primary.db "4 3 0 1" $str $flags

		close $did
	}
}

proc build_all { method psize {nentries 100}} {
	db_build join1.db $nentries 50 1 [conv $method 1] $psize
	db_build join2.db $nentries 25 2 [conv $method 2] $psize
	db_build join3.db $nentries 16 3 [conv $method 3] $psize
	db_build join4.db $nentries 12 4 [conv $method 4] $psize
	db_build null.db $nentries 0 5 [conv $method 5] $psize
}

proc conv { m i } {
	switch $m {
		DB_HASH { return DB_HASH }
		DB_BTREE { return DB_BTREE }
		DB_BOTH {
			if { [expr $i % 2] == 0 } {
				return DB_HASH;
			} else {
				return DB_BTREE;
			}
		}
	}
}

proc db_build { name nkeys ndups dup_interval method psize} {
source ./include.tcl
	# Create the database and open the dictionary
	set db [dbopen $name [expr $DB_CREATE | $DB_TRUNCATE] 0644 $method \		    -flags [expr $DB_DUP | $DB_DUPSORT] -psize $psize]
	error_check_good dbopen [is_valid_db $db] TRUE
	set did [open $dict]
	set count 0
	puts "\tBuilding $name.  $nkeys keys with $ndups duplicates at interval of $dup_interval"
	for { set count 0 } { [gets $did str] != -1 && $count < $nkeys } {
	    incr count} {
		for { set i 0 } { $i < $ndups } { incr i } {
			set data [format "%04d" [expr $i * $dup_interval]]
			set ret [$db put 0 $str $data 0]
			error_check_good put $ret 0
		}

		if { $ndups == 0 } {
			set ret [$db put 0 $str NODUP 0]
			error_check_good put $ret 0
		}
	}
	close $did
	error_check_good close:$name [$db close] 0
}

proc do_join { primary dbs key flags } {
source include.tcl
	puts "\tJoining: $dbs on $key"

	# Open all the databases
	set p [dbopen $primary 0 0 DB_UNKNOWN]
	error_check_good "primary open" [is_valid_db $p] TRUE

	set dblist ""
	set curslist ""

	foreach i $dbs {
		set db [dbopen [n_to_name $i] 0 0 DB_UNKNOWN]
		error_check_good "[n_to_name $i] open" [is_valid_db $db] TRUE
		set curs [$db cursor 0]
		error_check_good "$db cursor" \
		    [is_valid_widget $curs $db.cursor] TRUE
		lappend dblist $db
		lappend curslist $curs

		set pair [$curs get $key $DB_SET]
		error_check_good cursor_set:$key:$pair [llength $pair] 2
	}

	set join_curs [$p join $curslist 0]
	error_check_good join_cursor \
	    [is_valid_widget $join_curs join.cursor] TRUE

	# Calculate how many dups we expect.
	# We go through the list of indices.  If we find a 0, then we
	# expect 0 dups.  For everything else, we look at pairs of numbers,
	# if the are relatively prime, multiply them and figure out how
	# many times that goes into 50.  If they aren't relatively prime,
	# take the number of times the larger goes into 50.
	set expected 50
	set last 1
	foreach n $dbs {
		if { $n == 0 } {
			set expected 0
			break
		}
		if { $last == $n } {
			continue
		}

		if { [expr $last % $n] == 0 || [expr $n % $last] == 0 } {
			if { $n > $last } {
				set last $n
				set expected [expr 50 / $last]
			}
		} else {
			set last [expr $n * $last / [gcd $n $last]]
			set expected [expr 50 / $last]
		}
	}

	set ndups 0
	if { $flags == $DB_JOIN_ITEM } {
		set l 1
	} else {
		set l 2
	}
	for { set pair [$join_curs get 0 $flags] } { [llength $pair] == $l } {
	    set pair [$join_curs get 0 $flags] } {
		set k [lindex $pair 0]
		foreach i $dbs {
			error_check_bad valid_dup:$i:$dbs $i 0
			set kval [string trimleft $k 0]
			if { [string length $kval] == 0 } {
				set kval 0
			}
			error_check_good valid_dup:$i:$dbs [expr $kval % $i] 0
		}
		incr ndups
	}
	error_check_good number_of_dups:$dbs $ndups $expected

	error_check_good close_primary [$p close] 0
	foreach i $curslist {
		error_check_good close_cursor:$i [$i close] 0
	}
	foreach i $dblist {
		error_check_good close_index:$i [$i close] 0
	}
}

proc n_to_name { n } {
	if { $n == 0 } {
		return null.db;
	} else {
		return join$n.db;
	}
}

proc gcd { a b } {
	set g 1

	for { set i 2 } { $i <= $a } { incr i } {
		if { [expr $a % $i] == 0 && [expr $b % $i] == 0 } {
			set g $i
		}
	}
	return $g
}
