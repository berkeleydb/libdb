# See the file LICENSE for redistribution information.
#
# Copyright (c) 1996, 1997, 1998
#	Sleepycat Software.  All rights reserved.
#
#	@(#)mdbscript.tcl	10.5 (Sleepycat) 12/2/98
#
# Process script for the multi-process db tester.
# Usage: mdbscript dir file nentries iter procid procs seed
# dir: DBHOME directory
# file: db file on which to operate
# nentries: number of entries taken from dictionary
# iter: number of operations to run
# procid: this processes' id number
# procs: total number of processes running
# seed: Random number generator seed (-1 means use pid)
source ./include.tcl
source ../test/testutils.tcl
set datastr abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz
set usage "mdbscript method dir file nentries iter procid procs seed"

# Verify usage
if { $argc != 8 } {
	puts stderr $usage
	exit
}

# Initialize arguments
set method [lindex $argv 0]
set dir [lindex $argv 1]
set file [lindex $argv 2]
set nentries [ lindex $argv 3 ]
set iter [ lindex $argv 4 ]
set procid [ lindex $argv 5 ]
set procs [ lindex $argv 6 ]
set seed [ lindex $argv 7 ]

if { [string compare $method DB_RECNO] == 0 } {
	set put putn
} else {
	set put put
}

# Initialize seed
if { $seed == -1 } {
	set seed [pid]
}
srand $seed

puts "Beginning execution for [pid] $method"
puts "$dir db_home"
puts "$file database"
puts "$nentries data elements"
puts "$iter iterations"
puts "$procid process id"
puts "$procs processes"
puts "$seed seed"

set klock NOLOCK
flush stdout

set dbenv [dbenv -dbhome $dir -dbflags [expr $DB_INIT_CDB | $DB_INIT_MPOOL]]
error_check_good dbenv [is_valid_widget $dbenv env] TRUE

set db [record dbopen $file 0 0 DB_UNKNOWN -dbenv $dbenv]
error_check_bad dbopen $db NULL
error_check_good dbopen [is_valid_widget $db db] TRUE

# Get lock manager so we can lock keys
set lmgr [lock_open "" 0 0 -dbenv $dbenv]
error_check_good "lock_open -dbenv $dbenv" [is_valid_widget $lmgr lockmgr] TRUE

# Init globals (no data)
set nkeys [db_init $db 0]
puts "Initial number of keys: $nkeys"
error_check_good db_init $nkeys $nentries
exec $SLEEP 5

proc get_lock { k } {
global lmgr
global procid
global klock
global DB_LOCK_WRITE
global DB_LOCK_NOWAIT
	# Make sure that the key isn't in the middle of
	# a delete operation
	set klock [$lmgr get $procid $k $DB_LOCK_WRITE $DB_LOCK_NOWAIT]
	if { [string compare $klock BLOCKED] == 0 } {
		puts "Warning: key $k locked"
		set klock NOLOCK
		return 1
	}
	return 0
}
set txn 0

# On each iteration we're going to randomly pick a key.
# 1. We'll either get it (verifying that its contents are reasonable).
# 2. Put it (using an overwrite to make the data be datastr:ID).
# 3. Get it and do a put through the cursor, tacking our ID on to
# 4. Get it, read forward some random number of keys.
# 5. Get it, read forward some random number of keys and do a put (replace).
# 6. Get it, read forward some random number of keys and do a del.  And then
#	do a put of the key.
set gets 0
set getput 0
set overwrite 0
set seqread 0
set seqput 0
set seqdel 0
set dlen [string length $datastr]
for { set i 0 } { $i < $iter } { incr i } {
	set op [random_int 0 5]
	puts "iteration $i operation $op"
	flush stdout
	switch $op {
		0 {
			incr gets
			set k [random_int 0 [expr $nkeys - 1]]
			set key [lindex $l_keys $k]
			if { [get_lock  $key] == 1 } {
				incr i -1
				continue;
			}
			set rec [record $db get $txn $key 0 ]
			error_check_bad "$db get $key" [string length $rec] 0
			set partial [string range $rec 0 [expr $dlen - 1]]
			error_check_good "$db get $key" $partial $datastr
		}
		1 {
			incr overwrite
			set k [random_int 0 [expr $nkeys - 1]]
			set key [lindex $l_keys $k]
			set data $datastr:$procid
			set ret [record $db $put $txn $key $data 0]
			error_check_good "$db $put $key" $ret 0
		}
		2 {
			incr getput
			set dbc [record $db cursor 0 $DB_RMW]
			set k [random_int 0 [expr $nkeys - 1]]
			set key [lindex $l_keys $k]
			if { [get_lock  $key] == 1 } {
				incr i -1
				error_check_good "$dbc close" \
				    [record $dbc close] 0
				continue;
			}
			set ret [record $dbc get $key $DB_SET]
			error_check_good "$dbc get $key" [llength $ret] 2
			set rec [lindex $ret 1]
			set partial [string range $rec 0 [expr $dlen - 1]]
			error_check_good "$dbc get $key" $partial $datastr
			append rec ":$procid"
			set ret [record $dbc $put $key $rec $DB_CURRENT]
			error_check_good "$dbc $put $key" $ret 0
			error_check_good "$dbc close" [record $dbc close] 0
		}
		3 -
		4 -
		5 {
			if { $op == 3 } {
				set flags 0
			} else {
				set flags $DB_RMW
			}
			set dbc [record $db cursor 0 $flags]
			set close_cursor 1
			set k [random_int 0 [expr $nkeys - 1]]
			set key [lindex $l_keys $k]
			if { [get_lock  $key] == 1 } {
				incr i -1
				error_check_good "$dbc close" \
				    [record $dbc close] 0
				continue;
			}
			set ret [record $dbc get $key $DB_SET]
			error_check_good "$dbc get $key" [llength $ret] 2

			# Now read a few keys sequentially
			set nloop [random_int 0 10]
			if { [random_int 0 1] == 0 } {
				set flags $DB_NEXT
			} else {
				set flags $DB_PREV
			}
			while { $nloop > 0 } {
				set lastret $ret
				set ret [record $dbc get 0 $flags]
				# Might read beginning/end of file
				if { [string length $ret] == 0} {
					set ret $lastret
					break
				}
				incr nloop -1
			}
			switch $op {
				3 {
					incr seqread
				}
				4 {
					incr seqput
					set rec [lindex $ret 1]
					set partial [string range $rec 0 \
					    [expr $dlen - 1]]
					error_check_good "$dbc get $key" \
					    $partial $datastr
					append rec ":$procid"
					set ret [record $dbc \
					    put $key $rec $DB_CURRENT]
					error_check_good "$dbc put $key" $ret 0
				}
				5 {
					incr seqdel
					set k [lindex $ret 0]
					# We need to lock the item we're
					# deleting so that someone else can't
					# try to do a get while we're
					# deleting
					error_check_good "$klock put" \
					    [$klock put] 0
					set cur [$dbc get 0 $DB_CURRENT]
					if { [get_lock [lindex $cur 0]] == 1 } {
						incr i -1
						error_check_good "$dbc close" \
						    [record $dbc close] 0
						continue
					}
					set ret [record $dbc del 0]
					error_check_good "$dbc del 0" $ret 0
					set rec $datastr
					append rec ":$procid"
					if { [string compare $method DB_RECNO]
					    == 0 } {
						error_check_good "$dbc close" \
						    [record $dbc close] 0
						set close_cursor 0
						set ret [record $db $put 0 \
						    $k $rec 0]
						error_check_good "$db $put $k" \
						    $ret 0
					} else {
						set ret [record $dbc \
						    $put $k $rec $DB_KEYLAST]
						error_check_good "$dbc $put $k"\
						    $ret 0
					}
				}
			}
			if { $close_cursor == 1 } {
				error_check_good "$dbc close" \
				    [record $dbc close] 0
			}
		}
	}
	flush stdout
	if { [string compare $klock NOLOCK] != 0 } {
		error_check_good "$klock put" [$klock put] 0
		set klock NOLOCK
	}
}

error_check_good db_close:$db [record $db close] 0
error_check_good "$lmgr close" [$lmgr close] 0
reset_env $dbenv

puts "[timestamp] [pid] Complete"
puts "Successful ops: "
puts "\t$gets gets"
puts "\t$overwrite overwrites"
puts "\t$getput getputs"
puts "\t$seqread seqread"
puts "\t$seqput seqput"
puts "\t$seqdel seqdel"
flush stdout
