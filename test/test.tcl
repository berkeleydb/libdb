# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996, 1997, 1998, 1999
#	Sleepycat Software.  All rights reserved.
#
#	@(#)test.tcl	11.57 (Sleepycat) 10/28/99

source ./include.tcl

# Load DB's TCL API.
load $tcllib

if { [file exists $testdir] != 1 } {
	exec $MKDIR $testdir
}

global __debug_print
global __debug_on

set __debug_print 0
set __debug_on 0

set parms(subdb002) 10000
set parms(subdb003) 2000
set parms(subdb004) 10000
set parms(subdb005) 100
set parms(subdb006) 100
set parms(subdb007) 10000
set parms(test001) 10000
set parms(test002) 10000
set parms(test003) ""
set parms(test004) {10000 4 0}
set parms(test005) 10000
set parms(test006) {10000 6}
set parms(test007) 10000
set parms(test008) {10000 8 0}
set parms(test009) 10000
set parms(test010) {10000 5 10}
set parms(test011) {10000 5 11}
set parms(test012)  ""
set parms(test013) 10000
set parms(test014) 10000
set parms(test015) {7500 0}
set parms(test016) 10000
set parms(test017) 10000
set parms(test018) 10000
set parms(test019) 10000
set parms(test020) 10000
set parms(test021) 10000
set parms(test022) ""
set parms(test023) ""
set parms(test024) 10000
set parms(test025) 10000
set parms(test026) {2000 5 26}
set parms(test027) {100}
set parms(test028) ""
set parms(test029) 10000
set parms(test030) 10000
set parms(test031) {10000 5 31}
set parms(test032) {10000 5 32}
set parms(test033) {10000 5 33}
set parms(test034) 10000
set parms(test035) 10000
set parms(test036) 10000
set parms(test037) 100
set parms(test038) 10000
set parms(test039) 10000
set parms(test040) 10000
set parms(test041) 100
set parms(test042) 1000
set parms(test043) 10000
set parms(test044) {5 10 0}
set parms(test045) 10000
set parms(test046) ""
set parms(test047) ""
set parms(test048) ""
set parms(test049) ""
set parms(test050) ""
set parms(test051) ""
set parms(test052) ""
set parms(test053) ""
set parms(test054) ""
set parms(test055) ""
set parms(test056) ""
set parms(test057) ""
set parms(test058) ""
set parms(test059) ""
set parms(test060) ""
set parms(test061) ""
set parms(test062) {200 200 62}
set parms(test063) ""
set parms(test064) ""
set parms(test065) ""
set parms(test066) ""
set parms(test067) {1000 67}
set parms(test068) ""
set parms(test069) {50 69}
set parms(test070) {4 2 1000 70}
set parms(test071) {1 1 10000 71}
set parms(test072) ""

set dict $test_path/wordlist
set alphabet "abcdefghijklmnopqrstuvwxyz"

# Random number seed.
global rand_init
set rand_init 12345

# Default record length and padding character for
# fixed record length access method(s)
set fixed_len 20
set fixed_pad 0

set recd_debug	0
set log_log_record_types 0

set deadtests	 2
set envtests	 6
set recdtests	 7
set rsrctests	 2
set runtests	72
set subdbtests	 8

for { set i 1 } { $i <= $deadtests } {incr i} {
	set name [format "dead%03d.tcl" $i]
	source $test_path/$name
}
for { set i 1 } { $i <= $envtests } {incr i} {
	set name [format "env%03d.tcl" $i]
	source $test_path/$name
}
for { set i 1 } { $i <= $recdtests } {incr i} {
	set name [format "recd%03d.tcl" $i]
	source $test_path/$name
}
for { set i 1 } { $i <= $rsrctests } {incr i} {
	set name [format "rsrc%03d.tcl" $i]
	source $test_path/$name
}
for { set i 1 } { $i <= $runtests } {incr i} {
	set name [format "test%03d.tcl" $i]
	source $test_path/$name
}
for { set i 1 } { $i <= $subdbtests } {incr i} {
	set name [format "sdb%03d.tcl" $i]
	source $test_path/$name
}

source $test_path/archive.tcl
source $test_path/byteorder.tcl
source $test_path/dbm.tcl
source $test_path/hsearch.tcl
source $test_path/join.tcl
source $test_path/lock001.tcl
source $test_path/lock002.tcl
source $test_path/lock003.tcl
source $test_path/log.tcl
source $test_path/mpool.tcl
source $test_path/mutex.tcl
source $test_path/ndbm.tcl
source $test_path/sdbtest001.tcl
source $test_path/sdbtest002.tcl
source $test_path/sdbutils.tcl
source $test_path/testutils.tcl
source $test_path/txn.tcl
source $test_path/upgrade.tcl

proc run_all { } {
	global runtests
	global subdbtests
	source ./include.tcl

	exec $RM -rf ALL.OUT

	set o [open ALL.OUT a]
	puts $o [berkdb version -string]
	close $o

	set test_list {
	{"environment"		"env"}
	{"archive"		"archive"}
	{"locking"		"lock"}
	{"logging"		"log"}
	{"memory pool"		"mpool"}
	{"mutex"		"mutex"}
	{"transaction"		"txn"}
	{"deadlock detection"	"dead"}
	{"subdatabase"		"subdb_gen"}
	{"byte-order"		"byte"}
	{"recno backing file"	"rsrc"}
	{"DBM interface"	"dbm"}
	{"NDBM interface"	"ndbm"}
	{"Hsearch interface"	"hsearch"}
	}

	foreach pair $test_list {
		set msg [lindex $pair 0]
		set cmd [lindex $pair 1]
		puts "Running $msg tests"
		if [catch {exec $tclsh_path \
		    << "source $test_path/test.tcl; r $cmd" >>& ALL.OUT } res] {
			set o [open ALL.OUT a]
			puts $o "FAIL: $cmd test"
			close $o
		}
	}

	# Run recovery tests.
	puts "Running recovery tests"
	if [catch {exec $tclsh_path \
	    << "source $test_path/test.tcl; r recd" >>& ALL.OUT } res] {
		set o [open ALL.OUT a]
		puts $o "FAIL: recd test"
		close $o
	}

	# Run join test
	#
	# XXX
	# Broken up into separate tclsh instantiations so we don't require
	# so much memory.
	puts "Running join test"
	foreach i "join1 join2 join3 join4 join5 join6" {
		if [catch {exec $tclsh_path \
		    << "source $test_path/test.tcl; r $i" >>& ALL.OUT } res] {
			set o [open ALL.OUT a]
			puts $o "FAIL: $i test"
			close $o
		}
	}

	# Access method tests.
	#
	# XXX
	# Broken up into separate tclsh instantiations so we don't require
	# so much memory.
	foreach i "btree rbtree hash queue recno frecno rrecno" {
		puts "Running $i tests"
		for { set j 1 } { $j <= $runtests } {incr j} {
			if [catch {exec $tclsh_path \
			    << "source $test_path/test.tcl; \
			    run_method -$i $j $j" >>& ALL.OUT } res] {
				set o [open ALL.OUT a]
				puts $o "FAIL: [format "test%03d" $j] $i"
				close $o
			}
		}
		if [catch {exec $tclsh_path \
		    << "source $test_path/test.tcl; \
		    subdb -$i" >>& ALL.OUT } res] {
			set o [open ALL.OUT a]
			puts $o "FAIL: subdb -$i test"
			close $o
		}
	}

	catch { exec $SED -e /^FAIL/p -e d ALL.OUT } res
	set o [open ALL.OUT a]
	if { [string length $res] == 0 } {
		puts "Regression Tests Succeeded"
		puts $o "Regression Tests Succeeded"
	} else {
		puts "Regression Tests Failed; see ALL.OUT for log"
		puts $o "Regression Tests Failed"
	}
	close $o
}

proc r { args } {
	global envtests
	global log_log_record_types
	global recdtests
	global subdbtests
	source ./include.tcl

	if {[catch {
		set l [ lindex $args 0 ]
		switch $l {
			archive { eval archive [lrange $args 1 end] }
			byte {
				foreach method \
				    "-hash -btree -recno -queue -frecno" {
					byteorder $method
				}
			}
			dbm { eval dbm }
			dead {
				eval dead001 [lrange $args 1 end]
				eval dead002 [lrange $args 1 end]
			}
			env {
				for { set i 1 } { $i <= $envtests } {incr i} {
					eval env00$i
				}
			}
			hsearch { eval hsearch }
			join {
				eval r join1
				eval r join2
				eval r join3
				eval r join4
				eval r join5
				eval r join6
			}
			join1 { eval jointest }
			join2 { eval jointest 512 }
			join3 {	eval jointest 8192 0 $DB_JOIN_ITEM }
			join4 { eval jointest 8192 2 }
			join5 { eval jointest 8192 3 }
			join6 { eval jointest 512 3 }
			lock { eval locktest [lrange $args 1 end] }
			log { eval logtest [lrange $args 1 end] }
			mpool {
				eval r mpool1
				eval r mpool2
				eval r mpool3
			}
			mpool1 { eval mpool [lrange $args 1 end] }
			mpool2 { eval mpool -mem system [lrange $args 1 end] }
			mpool3 { eval mpool -mem private [lrange $args 1 end] }
			mutex { eval mutex [lrange $args 1 end] }
			ndbm { eval ndbm }
			recd {
				if { $PERL5 != "" } {
					set log_log_record_types 1
					set err [catch {exec $PERL5 \
					    "$test_path/log.pl" "--init"} ret]
					error_check_good \
					    "Initializing log record tracker" \
					    $err 0
				}
				foreach method \
			"btree rbtree hash queue recno frecno rrecno" {
					if { [catch \
					    {run_recd -$method} ret] != 0 } {
						puts $ret
					}
				}
				if { $log_log_record_types == 1 } {
					catch {exec $PERL5 "$test_path/log.pl" \
					    "--summary" $test_path} ret
					puts $ret
					set log_log_record_types 0
				}
			}
			rsrc {
				eval rsrc001
				eval rsrc002
			}
			subdb {
				eval subdbtest001
				eval subdbtest002

				foreach method \
			"btree rbtree hash queue recno frecno rrecno" {
					eval subdb [ -$method ]
				}
			}
			subdb_gen {
				eval subdbtest001
				eval subdbtest002
			}
			txn { eval txntest [lrange $args 1 end] }

			btree -
			rbtree -
			hash -
			queue -
			recno -
			frecno -
			rrecno { eval run_method $args }

			default {
				error \
				    "FAIL:[timestamp] r: $args: unknown command"
			}
		}
		flush stdout
		flush stderr
	} res] != 0} {
		global errorInfo;

		set fnl [string first "\n" $errorInfo]
		set theError [string range $errorInfo 0 [expr $fnl - 1]]
		if {[string first FAIL $errorInfo] == -1} {
			error "FAIL:[timestamp] r: $args: $theError"
		} else {
			error $theError;
		}
	}
}

proc run_method { method {start 1} {stop 0} args } {
	global __debug_on
	global __debug_print
	global parms
	global runtests

	if { $stop == 0 } {
		set stop $runtests
	}
	puts "run_method: $method $start $stop $args"

	if {[catch {
		for { set i $start } { $i <= $stop } {incr i} {
			puts "[timestamp]"
			set name [format "test%03d" $i]
			eval $name $method $parms($name) $args
			if { $__debug_print != 0 } {
				puts ""
			}
			if { $__debug_on != 0 } {
				debug
			}
			flush stdout
			flush stderr
		}
	} res] != 0} {
		global errorInfo;

		set fnl [string first "\n" $errorInfo]
		set theError [string range $errorInfo 0 [expr $fnl - 1]]
		if {[string first FAIL $errorInfo] == -1} {
			error "FAIL:[timestamp]\
			    run_method: $method $i: $theError"
		} else {
			error $theError;
		}
	}
}

proc subdb { method } {
	global subdbtests

	for { set i 1 } {$i <= $subdbtests} {incr i} {
		set name [format "subdb%03d" $i]
		eval $name $method
	}
}

proc run_recd { method {start 1} {stop 0} args } {
	global __debug_on
	global __debug_print
	global parms
	global recdtests

	if { $stop == 0 } {
		set stop $recdtests
	}
	puts "run_recd: $method $start $stop $args"

	if {[catch {
		for { set i $start } { $i <= $stop } {incr i} {
			puts "[timestamp]"
			set name [format "recd%03d" $i]
			eval $name $method
			if { $__debug_print != 0 } {
				puts ""
			}
			if { $__debug_on != 0 } {
				debug
			}
			flush stdout
			flush stderr
		}
	} res] != 0} {
		global errorInfo;

		set fnl [string first "\n" $errorInfo]
		set theError [string range $errorInfo 0 [expr $fnl - 1]]
		if {[string first FAIL $errorInfo] == -1} {
			error "FAIL:[timestamp]\
			    run_recd: $method $i: $theError"
		} else {
			error $theError;
		}
	}
}

proc convert_method { method } {
	switch -- $method {
		-btree -
		-dbtree -
		-ddbtree -
		-rbtree -
		BTREE -
		DB_BTREE -
		DB_RBTREE -
		RBTREE -
		bt -
		btree -
		db_btree -
		db_rbtree -
		rbt -
		rbtree { return "-btree" }

		-dhash -
		-hash -
		DB_HASH -
		HASH -
		db_hash -
		h -
		hash { return "-hash" }

		-queue -
		DB_QUEUE -
		QUEUE -
		db_queue -
		q -
		qam -
		queue { return "-queue" }

		-frecno -
		-recno -
		-rrecno -
		DB_FRECNO -
		DB_RECNO -
		DB_RRECNO -
		FRECNO -
		RECNO -
		RRECNO -
		db_frecno -
		db_recno -
		db_rrecno -
		frec -
		frecno -
		rec -
		recno -
		rrec -
		rrecno { return "-recno" }

		default { error "FAIL:[timestamp] $method: unknown method" }
	}
}

# If recno-with-renumbering or btree-with-renumbering is specified, then
# fix the arguments to specify the DB_RENUMBER/DB_RECNUM option for the
# -flags argument.
proc convert_args { method {largs ""} } {
	global fixed_len
	global fixed_pad
	global gen_upgrade
	global upgrade_be
	source ./include.tcl

	if { $gen_upgrade == 1 && $upgrade_be == 1 } {
		append largs " -lorder 4321 "
	}

	if { [is_rrecno $method] == 1 } {
		append largs " -renumber "
	} elseif { [is_rbtree $method] == 1 } {
		append largs " -recnum "
	} elseif { [is_dbtree $method] == 1 } {
		append largs " -dup "
	} elseif { [is_ddbtree $method] == 1 } {
		append largs " -dup "
		append largs " -dupsort "
	} elseif { [is_dhash $method] == 1 } {
		append largs " -dup "
	}

	if {[is_fixed_length $method] == 1} {
		append largs " -len $fixed_len -pad $fixed_pad"
	}
	return $largs
}

# Make sure the DB_RECNUM flag is set if we are doing btree.
proc number_btree { method {largs ""} } {
	source ./include.tcl

	if { [string compare $method "DB_BTREE"] == 0 } {
		append largs " -recnum "
	}
	return $largs
}

proc is_btree { method } {
	set names { -btree BTREE DB_BTREE bt btree }
	if { [lsearch $names $method] >= 0 } {
		return 1
	} else {
		return 0
	}
}

proc is_dbtree { method } {
	set names { -dbtree }
	if { [lsearch $names $method] >= 0 } {
		return 1
	} else {
		return 0
	}
}

proc is_ddbtree { method } {
	set names { -ddbtree }
	if { [lsearch $names $method] >= 0 } {
		return 1
	} else {
		return 0
	}
}

proc is_rbtree { method } {
	set names { -rbtree rbtree RBTREE db_rbtree DB_RBTREE rbt }
	if { [lsearch $names $method] >= 0 } {
		return 1
	} else {
		return 0
	}
}

proc is_recno { method } {
	set names { -recno DB_RECNO RECNO db_recno rec recno}
	if { [lsearch $names $method] >= 0 } {
		return 1
	} else {
		return 0
	}
}

proc is_rrecno { method } {
	set names { -rrecno rrecno RRECNO db_rrecno DB_RRECNO rrec }
	if { [lsearch $names $method] >= 0 } {
		return 1
	} else {
		return 0
	}
}

proc is_frecno { method } {
	set names { -frecno frecno frec FRECNO db_frecno DB_FRECNO}
	if { [lsearch $names $method] >= 0 } {
		return 1
	} else {
		return 0
	}
}

proc is_hash { method } {
	set names { -hash DB_HASH HASH db_hash h hash }
	if { [lsearch $names $method] >= 0 } {
		return 1
	} else {
		return 0
	}
}

proc is_dhash { method } {
	set names { -dhash }
	if { [lsearch $names $method] >= 0 } {
		return 1
	} else {
		return 0
	}
}

proc is_queue { method } {
	set names { -queue DB_QUEUE QUEUE db_queue q queue qam }
	if { [lsearch $names $method] >= 0 } {
		return 1
	} else {
		return 0
	}
}

proc is_record_based { method } {
	if { [is_recno $method] || [is_frecno $method] ||
	    [is_rrecno $method] || [is_queue $method] } {
		return 1
	} else {
		return 0
	}
}

proc is_fixed_length { method } {
	if { [is_queue $method] || [is_frecno $method] } {
		return 1
	} else {
		return 0
	}
}
