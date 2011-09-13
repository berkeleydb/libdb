# See the file LICENSE for redistribution information.
#
# Copyright (c) 1999, 2011 Oracle and/or its affiliates.  All rights reserved.
#
# $Id$
#
# TEST	db_reptest
# TEST	Wrapper to configure and run the db_reptest program.

#
# TODO:
# late client start.
# Number of message proc threads.
#

global last_nsites
set last_nsites 0

#
# There are several user-level procs that the user may invoke.
# 1. db_reptest - Runs randomized configurations in a loop.
# 2. basic_db_reptest - Runs a simple set configuration once,
#	as a smoke test.
# 3. restore_db_reptest 'dir' - Runs the configuration given in 'dir'
#	in a loop.  The purpose is either to reproduce a problem
#	that some configuration encountered, or test a fix.
# 4. db_reptest_prof - Runs a single randomized configuration
#	and generates gprof profiling information for that run.
# 5. basic_db_reptest_prof - Runs a simple set configuration and
#	generates gprof profiling information for that run.
# 6. restore_db_reptest_prof - Runs the configuration given in 'dir' and
#	generates gprof profiling information for one run.
#

#
# db_reptest - Run a randomized configuration.  Run the test
# 'count' times in a loop, or until 'stopstr' is seen in the OUTPUT
# files or if no count or string is given, it is an infinite loop.
#
proc db_reptest { { stopstr "" } {count -1} } {
	berkdb srand [pid]
	set cmd "db_reptest_int random"
	db_reptest_loop $cmd $stopstr $count
}

#
# Run a basic reptest.  The types are:
# Basic 0 - Two sites, start with site 1 as master, 5 worker threads, btree,
#	run 100 seconds, onesite remote knowledge.
# Basic 1 - Three sites, all sites start as client, 5 worker threads, btree
#	run 150 seconds, full remote knowledge.
#
proc basic_db_reptest { { basic 0 } } {
	global util_path

	if { [file exists $util_path/db_reptest] == 0 } {
		puts "Skipping db_reptest.  Is it built?"
		return
	}
	if { $basic == 0 } {
		db_reptest_int basic0
	}
	if { $basic == 1 } {
		db_reptest_int basic1
	}
}

proc basic_db_reptest_prof { { basic 0 } } {
	basic_db_reptest $basic
	generate_profiles
}

#
# Restore a configuration from the given directory and
# run that configuration in a loop 'count' times or until
# 'stopstr' is seen in the OUTPUT files or if no count or
# string is given, it is an infinite loop.
#
proc restore_db_reptest { restoredir { stopstr "" } { count -1 } } {
	set cmd "db_reptest_int restore $restoredir/SAVE_RUN"
	db_reptest_loop $cmd $stopstr $count
}

proc restore_db_reptest_prof { restoredir } {
	restore_db_reptest $restoredir "" 1
	generate_profiles
}

#
# Run a single randomized iteration and then generate the profile
# information for each site.
#
proc db_reptest_prof { } {
	berkdb srand [pid]
	set cmd "db_reptest_int random"
	db_reptest_loop $cmd "" 1
	generate_profiles
}

proc generate_profiles {} {
	global envdirs
	global num_sites
	global util_path

	#
	# Once it is complete, generate profile information.
	#
	for { set i 1 } { $i <= $num_sites } { incr i } {
		set gmon NULL
		set known_gmons \
		    { $envdirs($i)/db_reptest.gmon $envdirs($i)/gmon.out }
		foreach gfile $known_gmons {
			if { [file exists $gfile] } {
				set gmon $gfile
				break
			}
		}
		if { $gmon == "NULL" } {
			puts "No gmon file.  Was it built with profiling?"
			return
		}
		set prof_out db_reptest.$i.OUT
		set stat [catch {exec gprof $util_path/db_reptest \
		    $gmon >>& $prof_out} ret]
		if { $stat != 0 } {
			puts "FAIL: gprof: $ret"
		}
		error_check_good gprof $stat 0
		puts "Profiled output for site $i: $prof_out"
	}
}

proc db_reptest_profile { } {
	db_reptest_prof
}

#
# Wrapper to run the command in a loop, 'count' times.
#
proc db_reptest_loop { cmd stopstr count } {
	global util_path

	if { [file exists $util_path/db_reptest] == 0 } {
		puts "Skipping db_reptest.  Is it built?"
		return
	}
	set iteration 1
	set start_time [clock format [clock seconds] -format "%H:%M %D"]
	while { 1 } {
		puts -nonewline "ITERATION $iteration: "
		puts -nonewline \
		    [clock format [clock seconds] -format "%H:%M %D"]
		puts " (Started: $start_time)"

		#
		eval $cmd

		puts -nonewline "COMPLETED $iteration: "
		puts [clock format [clock seconds] -format "%H:%M %D"]
		incr iteration
		#
		# If we've been given a string to look for, run until we
		# see it.  Or if not, skip to the count check.
		#
		if { [string length $stopstr] > 0 } {
			set found [search_output $stopstr]
			if { $found } {
				break
			}
		}
		if { $count > 0 && $iteration > $count } {
			break
		}
	}
}

#
# Internal version of db_reptest that all user-level procs
# eventually call.  It will configure a single run of
# db_reptest based on the configuration type specified
# in 'cfgtype'.  This proc will:
# Configure a run of db_reptest
# Run db_reptest
# Verify the sites after db_reptest completes.
#
proc db_reptest_int { cfgtype { restoredir NULL } } {
	source ./include.tcl
	global envdirs
	global num_sites

	env_cleanup $testdir

	set savedir TESTDIR/SAVE_RUN
	reptest_cleanup $savedir

	#
	# Get all the default or random values needed for the test
	# and its args first.
	#
	set runtime 0
	#
	# Get number of sites first because pretty much everything else
	# after here depends on how many sites there are.
	#
	set num_sites [get_nsites $cfgtype $restoredir]
	set use_lease [get_lease $cfgtype $restoredir]
	set use_peers [get_peers $cfgtype]
	#
	# Only use kill if we have > 2 sites.
	# Returns a list.  An empty list means this will not be a kill test.
	# Otherwise the list has 3 values, the kill type and 2 kill sites.
	# See the 'get_kill' proc for a description of kill types.
	#
	set kill_type 0
	set kill_site 0
	set kill_remove 0
	set site_remove 0
	if { $num_sites > 2 } {
		set kill [get_kill $cfgtype $restoredir $num_sites]
		if { [llength $kill] > 0 } {
			set kill_type [lindex $kill 0]
			set kill_site [lindex $kill 1]
			set kill_remove [lindex $kill 2]
		} else {
			# If we are not doing a kill test, determine if
			# we are doing a remove test.
			set site_remove [get_remove $cfgtype $num_sites]
		}
	}
	if { $cfgtype != "restore" } {
		if { $use_lease } {
			set use_master 0
		} else {
			set use_master [get_usemaster $cfgtype]
			if { $site_remove == $use_master } {
				set site_remove 0
			}
		}
		set master_site [get_mastersite $cfgtype $use_master $num_sites]
		set noelect [get_noelect $use_master]
		set master2_site [get_secondary_master \
		    $noelect $master_site $kill_site $num_sites]
		set workers [get_workers $cfgtype $use_lease]
		set dbtype [get_dbtype $cfgtype]
		set runtime [get_runtime $cfgtype]
		puts -nonewline "Running: $num_sites sites, $runtime seconds "
		if { $kill_site } {
			puts -nonewline "kill site $kill_site "
			if { $kill_remove } {
				puts -nonewline "removed by site $kill_remove "
			}
		} elseif { $site_remove } {
			puts -nonewline "remove site $site_remove "
		}
		if { $use_lease } {
			puts "with leases"
		} elseif { $use_master } {
			set master_text "master site $master_site"
			if { $noelect } {
				set master_text [concat $master_text \
				    "no elections"]
			}
			if { $master2_site } {
				set master_text [concat $master_text \
				    "secondary master site $master2_site"]
			}
			puts "$master_text"
		} else {
			puts "no master"
		}
	}
	#
	# This loop sets up the args to the invocation of db_reptest
	# for each site.
	#
	set portlist [available_ports $num_sites]
	for { set i 1 } {$i <= $num_sites } { incr i } {
		set envdirs($i) TESTDIR/ENV$i
		set homedirs($i) ../ENV$i
		reptest_cleanup $envdirs($i)
		#
		# If we are restoring the args, just read them from the
		# saved location for this sites.  Otherwise build up
		# the args for each piece we need.
		#
		if { $cfgtype == "restore" } {
			set cid [open $restoredir/DB_REPTEST_ARGS.$i r]
			set prog_args($i) [read $cid]
			close $cid
			if { $runtime == 0 } {
				set runtime [parse_runtime $prog_args($i)]
				puts "Runtime: $runtime"
			}
		} else {
			set nmsg [berkdb random_int 1 [expr $num_sites * 2]]
			set prog_args($i) \
			    "-v -c $workers -t $dbtype -T $runtime -m $nmsg "
			set prog_args($i) \
			    [concat $prog_args($i) "-h $homedirs($i)"]
			set prog_args($i) \
			    [concat $prog_args($i) "-o $num_sites"]
			#
			# Add in if this site should remove itself.
			#
			if { $site_remove == $i } {
				set prog_args($i) [concat $prog_args($i) "-r"]
			}
			#
			# Add in if this site should kill itself.
			#
			if { $kill_site == $i } {
				set prog_args($i) [concat $prog_args($i) "-k"]
			}
			#
			# Add in if this site should remove a killed site.
			#
			if { $kill_remove == $i } {
				set kport [lindex $portlist \
				    [expr $kill_site - 1]]
				set prog_args($i) [concat $prog_args($i) \
				    "-K $kport"]
			}
			#
			# Add in if this site starts as a master or client.
			#
			if { $i == $master_site } {
				set state($i) MASTER
				set prog_args($i) [concat $prog_args($i) "-M"]
			} else {
				set state($i) CLIENT
				#
				# If we have a master, then we just want to
				# start as a client.  Otherwise start with
				# elections.
				#
				if { $use_master } {
					set prog_args($i) \
					    [concat $prog_args($i) "-C"]
				} else {
					set prog_args($i) \
					    [concat $prog_args($i) "-E"]
				}
			}
			#
			# Add in if we are in no elections mode and if we are 
			# the secondary master.
			#
			if { $noelect } {
				set prog_args($i) [concat $prog_args($i) "-n"]
				if { $i == $master2_site } {
					set prog_args($i) \
					    [concat $prog_args($i) "-s"]
				}
			}
		}
		save_db_reptest $savedir ARGS $i $prog_args($i)
	}

	# Now make the DB_CONFIG file for each site.
	reptest_make_config $savedir $num_sites envdirs state \
	    $use_lease $use_peers $kill_site $portlist $cfgtype $restoredir

	# Run the test
	run_db_reptest $savedir envdirs $num_sites $runtime $use_lease
	puts "Test run complete.  Verify."

	# Verify the test run.
	verify_db_reptest $num_sites envdirs $kill_site

	# Show the summary files
	print_summary

}

#
# Make a DB_CONFIG file for all sites in the group
#
proc reptest_make_config { savedir nsites edirs st lease peers kill \
    portlist cfgtype restoredir } {
	upvar $edirs envdirs
	upvar $st state
	global rporttype

	#
	# Generate global config values that should be the same
	# across all sites, such as number of sites and log size, etc.
	#
	set rporttype NULL
	set default_cfglist {
	{ "set_flags" "DB_TXN_NOSYNC" }
	{ "rep_set_request" "150000 2400000" }
	{ "rep_set_timeout" "db_rep_checkpoint_delay 0" }
	{ "rep_set_timeout" "db_rep_connection_retry 2000000" }
	{ "rep_set_timeout" "db_rep_heartbeat_monitor 1000000" }
	{ "rep_set_timeout" "db_rep_heartbeat_send 500000" }
	{ "set_cachesize"  "0 4194304 1" }
	{ "set_lg_max" "131072" }
	{ "set_lk_detect" "db_lock_default" }
	{ "set_verbose" "db_verb_recovery" }
	{ "set_verbose" "db_verb_replication" }
	}

	set acks { db_repmgr_acks_all db_repmgr_acks_all_peers \
	    db_repmgr_acks_none db_repmgr_acks_one db_repmgr_acks_one_peer \
	    db_repmgr_acks_quorum }

	#
	# 2site strict and ack policy must be the same on all sites.
	#
	if { $cfgtype == "random" } {
		if { $nsites == 2 } {
			set strict [berkdb random_int 0 1]
		} else {
			set strict 0
		}
		if { $lease } {
			#
			# 2site strict with leases must have ack policy of
			# one because quorum acks are ignored in this case,
			# resulting in lease expired panics on some platforms.
			#
			if { $strict } {
				set ackpolicy db_repmgr_acks_one
			} else {
				set ackpolicy db_repmgr_acks_quorum
			}
		} else {
			set done 0
			while { $done == 0 } {
				set acksz [expr [llength $acks] - 1]
				set myack [berkdb random_int 0 $acksz]
				set ackpolicy [lindex $acks $myack]
				#
				# Only allow the "none" policy with 2 sites
				# otherwise it can overwhelm the system and
				# it is a rarely used option.
				#
				if { $ackpolicy == "db_repmgr_acks_none" && \
				    $nsites > 2 } {
					continue
				}
				#
				# Only allow "all" or "all_peers" policies
				# if not killing a site, otherwise the
				# unavailable site will cause the master
				# to ignore acks and blast the clients with
				# log records.
				#
				if { $kill && \
				    ($ackpolicy == "db_repmgr_acks_all" || \
				    $ackpolicy == 
				    "db_repmgr_acks_all_peers") } {
					continue
				}
				set done 1
			}
		}
	} else {
		set ackpolicy db_repmgr_acks_one
	}
	#
	# Set known_master to the initial master or if one is not
	# assigned, randomly assign the group creator.
	#
	set known_master 0
	if { $cfgtype != "restore" } {
		for { set i 1 } { $i <= $nsites } { incr i } {
			if { $state($i) == "MASTER" } {
				set known_master $i
			}
		}
		if { $known_master == 0 } {
			set known_master [berkdb random_int 1 $nsites]
		}
	}
	for { set i 1 } { $i <= $nsites } { incr i } {
		#
		# If we're restoring we just need to copy it.
		#
		if { $cfgtype == "restore" } {
			file copy $restoredir/DB_CONFIG.$i \
			    $envdirs($i)/DB_CONFIG
			file copy $restoredir/DB_CONFIG.$i \
			    $savedir/DB_CONFIG.$i
			continue
		}
		#
		# Otherwise set up per-site config information
		#
		set cfglist $default_cfglist

		#
		# Add lease configuration if needed.  We're running all
		# locally, so there is no clock skew.
		#
		set allist [get_ack_lease_timeouts $lease]
		if { $lease } {
			#
			# We need to have an ack timeout > lease timeout.
			# Otherwise txns can get committed without waiting
			# long enough for leases to get granted.
			#
			lappend cfglist { "rep_set_config" "db_rep_conf_lease" }
			lappend cfglist { "rep_set_timeout" \
			    "db_rep_lease_timeout [lindex $allist 1]" }
			lappend cfglist { "rep_set_timeout" \
			    "db_rep_ack_timeout [lindex $allist 0]" }
		} else {
			lappend cfglist { "rep_set_timeout" \
			    "db_rep_ack_timeout [lindex $allist 0]" }
		}

		#
		# Priority
		#
		if { $state($i) == "MASTER" } {
			lappend cfglist { "rep_set_priority" 100 }
		} else {
			if { $cfgtype == "random" } {
				set pri [berkdb random_int 10 25]
			} else {
				set pri 20
			}
			set litem [list rep_set_priority $pri]
			lappend cfglist $litem
		}
		#
		# Others: limit size, bulk, 2site strict
		#
		if { $cfgtype == "random" } {
			set limit_sz [berkdb random_int 15000 1000000]
			set bulk [berkdb random_int 0 1]
			if { $bulk } {
				lappend cfglist \
				    { "rep_set_config" "db_rep_conf_bulk" }
			}
			#
			# 2site strict was set above for all sites but
			# should only be used for sites in random configs.
			#
			if { $strict } {
				lappend cfglist { "rep_set_config" \
				    "db_repmgr_conf_2site_strict" }
			}
		} else {
			set limit_sz 100000
		}
		set litem [list rep_set_limit "0 $limit_sz"]
		lappend cfglist $litem
		set litem [list repmgr_set_ack_policy $ackpolicy]
		lappend cfglist $litem
		#
		# Now set up the local and remote ports.  If we are the
		# known_master (either master or group creator) set the
		# group creator flag on.
		#
		set lport($i) [lindex $portlist [expr $i - 1]]
		if { $i == $known_master } {
			set litem [list repmgr_site \
			    "localhost $lport($i) db_local_site on \
			    db_group_creator on"]
		} else {
			set litem [list repmgr_site \
			    "localhost $lport($i) db_local_site on"]
		}
		lappend cfglist $litem
		set rport($i) [get_rport $portlist $i $nsites \
		    $known_master $cfgtype]
		#
		# Declare all sites bootstrap helpers.
		#
		foreach p $rport($i) {
			if { $peers } {
				set litem [list repmgr_site "localhost $p \
				    db_bootstrap_helper on db_repmgr_peer on"]
			} else {
				set litem [list repmgr_site "localhost $p \
				    db_bootstrap_helper on"]
			}
			#
			# If we have full knowledge, assume a legacy system.
			#
			if { $cfgtype == "full" } {
				lappend litem "db_legacy on"
			}
			lappend cfglist $litem
		}
		#
		# Now write out the DB_CONFIG file.
		#
		set cid [open $envdirs($i)/DB_CONFIG a]
		foreach c $cfglist {
			set carg [subst [lindex $c 0]]
			set cval [subst [lindex $c 1]]
			puts $cid "$carg $cval"
		}
		close $cid
		set cid [open $envdirs($i)/DB_CONFIG r]
		set cfg [read $cid]
		close $cid
	
		save_db_reptest $savedir CONFIG $i $cfg
	}

}

proc reptest_cleanup { dir } {
	#
	# For now, just completely remove it all.  We might want
	# to use env_cleanup at some point in the future.
	#
	fileremove -f $dir
	file mkdir $dir
}


proc save_db_reptest { savedir op site savelist } {
	#
	# Save a copy of the configuration and args used to run this
	# instance of the test.
	#
	if { $op == "CONFIG" } {
		set outfile $savedir/DB_CONFIG.$site
	} else {
		set outfile $savedir/DB_REPTEST_ARGS.$site
	}
	set cid [open $outfile a]
	puts -nonewline $cid $savelist
	close $cid
}

proc run_db_reptest { savedir edirs numsites runtime use_lease } {
	source ./include.tcl
	upvar $edirs envdirs
	global killed_procs

	set pids {}
	#
	# Wait three times workload run time plus an ack_timeout for each site
	# to kill a run.  The ack_timeout is especially significant for runs
	# where leases are in use because they take much longer to get started.
	#
	set ack_timeout [lindex [get_ack_lease_timeouts $use_lease] 0]
	set watch_time [expr $runtime * 3 + \
	    [expr $ack_timeout / 1000000] * $numsites]
	for {set i 1} {$i <= $numsites} {incr i} {
		lappend pids [exec $tclsh_path $test_path/wrap_reptest.tcl \
		    $savedir/DB_REPTEST_ARGS.$i $envdirs($i) \
		    $savedir/site$i.log &]
		tclsleep 1
	}
	watch_procs $pids 15 $watch_time
	set killed [llength $killed_procs]
	if { $killed > 0 } {
		error "Processes $killed_procs never finished"
	}
}

proc verify_db_reptest { num_sites edirs kill } {
	upvar $edirs envdirs

	set startenv 1
	set cmpeid 2
	if { $kill == 1 } {
		set startenv 2
		set cmpeid 3
	}
	set envbase [berkdb_env_noerr -home $envdirs($startenv)]
	for { set i $cmpeid } { $i <= $num_sites } { incr i } {
		if { $i == $kill } {
			continue
		}
		set cmpenv [berkdb_env_noerr -home $envdirs($i)]
		puts "Compare $envdirs($startenv) with $envdirs($i)"
		#
		# Compare 2 envs.  We assume the name of the database that
		# db_reptest creates and know it is 'am1.db'.
		# We want as other args:
		# 0 - compare_shared_portion
		# 1 - match databases
		# 0 - don't compare logs (for now)
		rep_verify $envdirs($startenv) $envbase $envdirs($i) $cmpenv \
		    0 1 0 am1.db
		$cmpenv close
	}
	$envbase close
}

proc get_nsites { cfgtype restoredir } {
	global last_nsites

	#
	# Figure out the number of sites.  We use 'glob' to get all of
	# the valid DB_CONFIG files in the restoredir.  That command uses
	# a single digit match, so the maximum number of sites must be <= 9.
	# Match DB_CONFIG.# so that it does not consider anything like an
	# emacs save file.
	#
	set maxsites 5
	#
	# If someone changes maxsites to be too big, it will break the
	# 'glob' below.  Catch that now.
	#
	if { $maxsites > 9 } {
		error "Max sites too large."
	}
	if { $cfgtype == "restore" } {
		set ret [catch {glob $restoredir/DB_CONFIG.\[1-$maxsites\]} \
		    result]
		if { $ret != 0 } {
			error "Could not get config list: $result"
		}
		return [llength $result]
	}
	if { $cfgtype == "random" } {
		#
		# Sometimes 'random' doesn't seem to do a good job.  I have
		# seen on all iterations after the first one, nsites is
		# always 2, 100% of the time.  Add this bit to make sure
		# this nsites values is different from the last iteration.
		#
		set n [berkdb random_int 2 $maxsites]
		while { $n == $last_nsites } {
			set n [berkdb random_int 2 $maxsites]
puts "Getting random nsites between 2 and $maxsites.  Got $n, last_nsites $last_nsites"
		}
		set last_nsites $n
		return $n
	}
	if { $cfgtype == "basic0" } {
		return 2
	}
	if { $cfgtype == "basic1" } {
		return 3
	}
	return -1
}

#
# Run with master leases?  25%/75% (use a master lease 25% of the time).
#
proc get_lease { cfgtype restoredir } {
	#
	# The number of sites must be the same for all.  Read the
	# first site's saved DB_CONFIG file if we're restoring since
	# we only know we have at least 1 site.
	#
	if { $cfgtype == "restore" } {
		set use_lease 0
		set cid [open $restoredir/DB_CONFIG.1 r]
		while { [gets $cid cfglist] } {
#			puts "Read in: $cfglist"
			if { [llength $cfglist] == 0 } {
				break;
			}
			set cfg [lindex $cfglist 0]
			if { $cfg == "rep_set_config" } {
				set lease [lindex $cfglist 1]
				if { $lease == "db_rep_conf_lease" } {
					set use_lease 1
					break;
				}
			}
		}
		close $cid
		return $use_lease
	}
	if { $cfgtype == "random" } {
		set leases { 1 0 0 0 }
		set len [expr [llength $leases] - 1]
		set i [berkdb random_int 0 $len]
		return [lindex $leases $i]
	}
	if { $cfgtype == "basic0" } {
		return 0
	}
	if { $cfgtype == "basic1" } {
		return 0
	}
}

#
# Do a kill test about half the time.  We randomly choose a
# site number to kill, it could be a master or a client.  If
# we want to remove the site from the group, randomly choose
# a site to do the removal.
#
# We return a list with the kill type and the sites.  Return
# an empty list if we don't kill any site.  There are 2 variants:
#
# 1: Die - A site just kills itself but remains part of the group.
# Return a list {1 deadsite# 0}.
# 2: Removal - A site kills itself, and some site will also remove
# the dead site from the group. (Could be the same site that is dying).
# {2 deadsite# removalsite#}.
#
proc get_kill { cfgtype restoredir num_sites } {
	set nokill ""
	if { $cfgtype == "restore" } {
		set ksite 0
		set ktype 0
		set rsite 0
		for { set i 1 } { $i <= $num_sites } { incr i } {
			set cid [open $restoredir/DB_REPTEST_ARGS.$i r]
			# !!!
			# We currently assume the args file is 1 line.
			#
			gets $cid arglist
			close $cid
#			puts "Read in: $arglist"
			set dokill [lsearch $arglist "-k"]
			set dorem [lsearch $arglist "-K"]
			#
			# Only 1 of those 3 should ever be set.  If we
			# find -K, we have all the information we need
			# and can break the loop.  If we find a -k we might
			# find a later -K so we keep looking.
			#
			if { $dokill != -1 } {
				set ksite $i
				set ktype 1
			}
			#
			# If it is a remote removal kill type, we are
			# the site doing the removing and we need to get
			# the site to remove from the arg.  $dorem is the
			# index of the arg, so + 1 is the site number.
			# The site in the arg is the port number so grab
			# the site number out of it.
			#
			if { $dorem != -1 } {
				set ktype 2
				set kport [lindex $arglist [expr $dorem + 1]]
				set ksite [site_from_port $kport $num_sites]
				set rsite $i
				break
			}
		}
		if { $ktype == 0 } {
			return $nokill
		} else {
			return [list $ktype $ksite $rsite]
		}
	}
	if { $cfgtype == "random" } {
		# Do a kill test half the time.
		set k { 0 0 0 1 1 1 0 1 1 0 }
		set len [expr [llength $k] - 1]
		set i [berkdb random_int 0 $len]
		if { [lindex $k $i] == 1 } {
			set ktype 1
			set ksite [berkdb random_int 1 $num_sites]
			set rsite 0
			# Do a removal half the time we do a kill.
			set k { 0 0 0 1 1 1 0 1 1 0 }
			set len [expr [llength $k] - 1]
			set i [berkdb random_int 0 $len]
			if { [lindex $k $i] == 1 } {
				set ktype 2
				set rsite [berkdb random_int 1 $num_sites]
			}
			set klist [list $ktype $ksite $rsite]
		} else {
			set klist $nokill
		}
		return $klist
	}
	if { $cfgtype == "basic0" || $cfgtype == "basic1" } {
		return $nokill
	} else {
		error "Get_kill: Invalid config type $cfgtype"
	}
}

#
# If we want to run a remove/rejoin, which site?  This proc
# will return a site number of a site to remove/rejoin or
# it will return 0 if no removal test.  Sites are numbered
# starting at 1.
#
proc get_remove { cfgtype nsites } {
#
# For now, until the "restart a dead carcass" work is done
# post-5.2, we don't use this option.  5.2 requires a site
# to shutdown if it gets removed while it is alive.
#
return 0
	if { $cfgtype == "random" } {
		# Do a remove test half the time we're called.
		set k { 0 0 0 1 1 1 0 1 1 0 }
		set len [expr [llength $k] - 1]
		set i [berkdb random_int 0 $len]
		if { [lindex $k $i] == 1 } {
			set rsite [berkdb random_int 1 $nsites]
		} else {
			set rsite 0
		}
		return $rsite
	} else {
		return 0
	}
}

#
# Use peers or only the master for requests? 25%/75% (use a peer 25%
# of the time and master 75%)
#
proc get_peers { cfgtype } {
	if { $cfgtype == "random" } {
		set peer { 0 0 0 1 }
		set len [expr [llength $peer] - 1]
		set i [berkdb random_int 0 $len]
		return [lindex $peer $i]
	} else {
		return 0
	}
}

#
# Start with a master or all clients?  25%/75% (use a master 25%
# of the time and have all clients 75%)
#
proc get_usemaster { cfgtype } {
	if { $cfgtype == "random" } {
		set mst { 1 0 0 0 }
		set len [expr [llength $mst] - 1]
		set i [berkdb random_int 0 $len]
		return [lindex $mst $i]
	}
	if { $cfgtype == "basic0" } {
		return 1
	}
	if { $cfgtype == "basic1" } {
		return 0
	}
}

#
# If we use a master, which site?  This proc will return
# the site number of the mastersite, or it will return
# 0 if no site should start as master.  Sites are numbered
# starting at 1.
#
proc get_mastersite { cfgtype usemaster nsites } {
	if { $usemaster == 0 } {
		return 0
	}
	if { $cfgtype == "random" } {
		return [berkdb random_int 1 $nsites]
	}
	if { $cfgtype == "basic0" } {
		return 1
	}
	if { $cfgtype == "basic1" } {
		return 0
	}
}

#
# If we are using a master, use no elections 20% of the time.
#
proc get_noelect { usemaster } {
	if { $usemaster } {
		set noelect { 0 0 1 0 0 }
		set len [expr [llength $noelect] - 1]
		set i [berkdb random_int 0 $len]
		return [lindex $noelect $i]
	} else {
		return 0
	}
}

#
# If we are using no elections mode and we are going to kill the initial
# master, select a different site to start up as master after the initial
# master is killed.
#
proc get_secondary_master { noelect master_site kill nsites } {
	if { $noelect == 0 || $kill != $master_site} {
		return 0
	}
	set master2_site [berkdb random_int 1 $nsites]
	while { $master2_site == $master_site } {
		set master2_site [berkdb random_int 1 $nsites]		
	}
	return $master2_site
}

#
# This is the number of worker threads performing the workload.
# This is not the number of message processing threads.
#
# Scale back the number of worker threads if leases are in use.
# The timing with leases can be fairly sensitive and since all sites
# run on the local machine, too many workers on every site can
# overwhelm the system, causing lost messages and delays that make
# the tests fail.  Rather than try to tweak timeouts, just reduce
# the workloads a bit.
#
proc get_workers { cfgtype lease } {
	if { $cfgtype == "random" } {
		if { $lease } {
			return [berkdb random_int 2 4]
		} else {
			return [berkdb random_int 2 8]
		}
	}
	if { $cfgtype == "basic0" || $cfgtype == "basic1" } {
		return 5
	}
}

proc get_dbtype { cfgtype } {
	if { $cfgtype == "random" } {
		#
		# 50% btree, 25% queue 12.5% hash 12.5% recno
		# We favor queue only because there is special handling
		# for queue in internal init.
		#
#		set methods {btree btree btree btree queue queue hash recno}
		set methods {btree btree btree btree hash recno}
		set len [expr [llength $methods] - 1]
		set i [berkdb random_int 0 $len]
		return [lindex $methods $i]
	}
	if { $cfgtype == "basic0" || $cfgtype == "basic1" } {
		return btree
	}
}

proc get_runtime { cfgtype } {
	if { $cfgtype == "random" } {
		return [berkdb random_int 100 500]
	}
	if { $cfgtype == "basic0" } {
		return 100
	}
	if { $cfgtype == "basic1" } {
		return 150
	}
}

proc get_rport { portlist i num_sites known_master cfgtype} {
	global rporttype

	if { $cfgtype == "random" && $rporttype == "NULL" } {
		set types {backcirc forwcirc full onesite}
		set len [expr [llength $types] - 1]
		set rindex [berkdb random_int 0 $len]
		set rporttype [lindex $types $rindex]
	}
	if { $cfgtype == "basic0" } {
		set rporttype onesite
	}
	if { $cfgtype == "basic1" } {
		set rporttype full
	}
	#
	# This produces a circular knowledge ring.  Either forward
	# or backward.  In the forwcirc, ENV1 knows (via -r) about
	# ENV2, ENV2 knows about ENV3, ..., ENVX knows about ENV1.
	#
	if { $rporttype == "forwcirc" } {
		if { $i != $num_sites } {
			return [list [lindex $portlist $i]]
		} else {
			return [list [lindex $portlist 0]]
		}
	}
	if { $rporttype == "backcirc" } {
		if { $i != 1 } {
			return [list [lindex $portlist [expr $i - 2]]]
		} else {
			return [list [lindex $portlist [expr $num_sites - 1]]]
		}
	}
	#
	# This produces a configuration where site N does not know
	# about any other site and every other site knows about site N.
	# Site N must either be the master or group creator.
	# NOTE: Help_site_i subtracts one because site numbers
	# are 1-based and list indices are 0-based.
	#
	if { $rporttype == "onesite" } {
		set helper_site [expr $known_master - 1]
		if { $i == $known_master } {
			return {}
		}
		return [lindex $portlist $helper_site]
	}
	#
	# This produces a fully connected configuration
	#
	if { $rporttype == "full" } {
		set rlist {}
		for { set site 1 } { $site <= $num_sites } { incr site } {
			if { $site != $i } {
				lappend rlist \
				    [lindex $portlist [expr $site - 1]]
			}
		}
		return $rlist
	}
}

#
# We need to have an ack timeout > lease timeout. Otherwise txns can get 
# committed without waiting long enough for leases to get granted.  We
# return a list {acktimeout# leasetimeout#}, with leasetimeout#=0 if leases
# are not in use.
#
proc get_ack_lease_timeouts { useleases } {
	if { $useleases } {
		return [list 20000000 10000000]
	} else {
		return [list 5000000 0]
	}
}

proc parse_runtime { progargs } {
	set i [lsearch $progargs "-T"]
	set val [lindex $progargs [expr $i + 1]]
	return $val
}

proc print_summary { } {
	source ./include.tcl
	global envdirs

	set ret [catch {glob $testdir/summary.*} result]
	if { $ret == 0 } {
		set sumfiles $result
	} else {
		puts "Could not get summary list: $result"
		return 1
	}
	foreach f $sumfiles {
		puts "====   $f   ===="
		set ret [catch {open $f} fd]
		if { $ret != 0 } {
			puts "Error opening $f: $fd"
			continue
		}
		while { [gets $fd line] >= 0 } {
			puts "$line"
		}
		close $fd
	}
	return 0
}

proc search_output { stopstr } {
	source ./include.tcl

	set ret [catch {glob $testdir/E*/OUTPUT} result]
	if { $ret == 0 } {
		set outfiles $result
	} else {
		puts "Could not find any OUTPUT files: $result"
		return 0
	}
	set found 0
	foreach f $outfiles {
		set ret [catch {exec grep $stopstr $f > /dev/null} result]
		if { $ret == 0 } {
			puts "$f: Match found: $stopstr"
			set found 1
		}
	}
	return $found
}
