#
#    May you do good and not evil.
#    May you find forgiveness for yourself and forgive others.
#    May you share freely, never taking more than you give.
#
#***********************************************************************
# Utility functions for bdb tests.

source $testdir/tester.tcl

# 
# Functions for threads that return SQLITE_LOCK error when caught
set ::bdb_thread_procs { 
  proc execsql {sql} {
    set rc SQLITE_OK
    set err [catch {
      set ::STMT [sqlite3_prepare_v2 $::DB $sql -1 dummy_tail]
    } msg]

    if {$err == 0} {
      while {[set rc [sqlite3_step $::STMT]] eq "SQLITE_ROW"} {}
      set rc [sqlite3_finalize $::STMT]
    } else {
      if {[lindex $msg 0]=="(6)"} {
        set rc SQLITE_LOCKED
      } else {
        set rc SQLITE_ERROR
      }
    }

    if {[string first locked [sqlite3_errmsg $::DB]]>=0} {
      set rc SQLITE_LOCKED
    }
    if {$rc ne "SQLITE_OK" && $rc ne "SQLITE_LOCKED"} {
      set errtxt "$rc - [sqlite3_errmsg $::DB] (debug1)"
    }
    set rc
  }

  proc do_test {name script result} {
    set res [eval $script]
    if {$res ne $result} {
      puts "$name failed: expected \"$result\" got \"$res\""
      error "$name failed: expected \"$result\" got \"$res\""
    }
  }
}

# NOTE: This routine is copied from ../test/tcl/reputils.tcl
# and changes to it should be made in both places because the SQL
# tests are currently independent of the core tests.
#
# Return a list of TCP port numbers that are not currently in use on
# the local system.  Note that this doesn't actually reserve the
# ports, so it's possible that by the time the caller tries to use
# them, another process could have taken one of them.  But for our
# purposes that's unlikely enough that this is still useful: it's
# still better than trying to find hard-coded port numbers that will
# always be available.
#
# Using a starting baseport value that falls in the non-ephemeral port
# range on most platforms.  Can override starting baseport by setting
# environment variable BDBBASEPORT. 
#
proc available_ports { n { rangeincr 10 } } {
	global env

	if { [info exists env(BDBBASEPORT)] } {
		set baseport $env(BDBBASEPORT)
	} else {
		set baseport 30100
	}

	# Try sets of contiguous ports ascending from baseport.
	for { set i $baseport } { $i < $baseport + $rangeincr * 100 } \
	    { incr i $rangeincr } {
		set ports {}
		set socks {}
		set numports $n
		set curport $i

		# Try one set of contiguous ports.
		while { [incr numports -1] >= 0 } {
			incr curport
			if [catch { socket -server Unused \
			    -myaddr localhost $curport } sock] {
				# A port is unavailable, try another set.
				break
			}
			lappend socks $sock
			lappend ports $curport
		}
		foreach sock $socks {
			close $sock
		}
		if { $numports == -1 } {
			# We have all the ports we need.
			break
		}
	}
	if { $numports == -1 } {
		return $ports
	} else {
		error "available_ports: could not get ports for $baseport"
	}
}

#
# This procedure sets up three sites and databases suitable for replication
# testing.  The databases are created in separate subdirectories of the
# current working directory.
#
# This procedure populates global variables for each site's network
# address (host:port) and each site's directory for later use in tests.
# It uses the standard sqlite testing databases: db, db2 and db3.
#
proc setup_rep_sites {} {
	global site1addr site2addr site3addr site1dir site2dir site3dir

	# Get free ports in safe range for most platforms.
	set ports [available_ports 3]

	# Set up site1 directory and database.
	set site1dir ./repsite1
	catch {db close}
	file delete -force $site1dir/rep.db
	file delete -force $site1dir/rep.db-journal
	file delete -force $site1dir
	file mkdir $site1dir
	sqlite3 db $site1dir/rep.db
	set site1addr "localhost:[lindex $ports 0]"

	# Set up site2 directory and database.
	set site2dir ./repsite2
	catch {db2 close}
	file delete -force $site2dir/rep.db
	file delete -force $site2dir/rep.db-journal
	file delete -force $site2dir
	file mkdir $site2dir
	sqlite3 db2 $site2dir/rep.db
	set site2addr "localhost:[lindex $ports 1]"

	# Set up site3 directory and database.
	set site3dir ./repsite3
	catch {db3 close}
	file delete -force $site3dir/rep.db
	file delete -force $site3dir/rep.db-journal
	file delete -force $site3dir
	file mkdir $site3dir
	sqlite3 db3 $site3dir/rep.db
	set site3addr "localhost:[lindex $ports 2]"
}
