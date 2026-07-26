# This awk script parses C input files looking for lines marked "PUBLIC:"
# "EXTERN:", and "DB_LOG_RECSPEC".  (PUBLIC lines are DB internal function
# prototypes and #defines, EXTERN lines are DB external function prototypes
# and #defines, and DB_LOG_RECSPEC lines are the definition of log record
# templates.)
#
# PUBLIC lines are put into two versions of per-directory include files:
# one file that contains the prototypes, and one file that contains a
# #define for the name to be processed during configuration when creating
# unique names for every global C-language symbol in the DB library.
#
# The EXTERN lines are put into two files: one of which contains prototypes
# which are always appended to the db.h file, and one of which contains a
# #define list for use when creating unique symbol names.
#
# DB_LOG_RECSPEC lines are put into PUBLIC's internal #define file.
#
# Four arguments:
#	e_dfile		list of EXTERN #defines
#	e_pfile		include file that contains EXTERN prototypes
#	i_dfile		list of internal (PUBLIC) #defines
#	i_pfile		include file that contains internal (PUBLIC) prototypes

# Reset the per-file preprocessor-guard depth at each new input file.  #if/
# #endif are balanced within a file, but reset defensively so a stray
# imbalance in one file cannot corrupt the guard stack for the next.
FNR == 1 { pp_pub_d = 0; pp_ext_d = 0 }

/PUBLIC:/ {
	sub(/^.*PUBLIC:[	 ][	 ]*/, "")
	if ($0 ~ /^#if|^#ifdef|^#ifndef|^#else|^#endif/) {
		# Accumulate continuation lines (ending with \) into
		# a single preprocessor directive.
		ppline = $0
		while (ppline ~ /\\$/) {
			sub(/\\$/, "", ppline)
			if (getline <= 0)
				break
			sub(/^.*PUBLIC:[	 ][	 ]*/, "")
			ppline = ppline $0
		}
		# Track the active preprocessor guard so the de-dup below can
		# distinguish the SAME prototype under DIFFERENT (mutually
		# exclusive) #if arms -- e.g. __os_atomic_read is declared once
		# per atomic tier (GCC_BUILTIN, SYNC_BUILTIN, ...).  Those must
		# all survive; only a true duplicate within one guard is dropped.
		if (ppline ~ /^#if|^#ifdef|^#ifndef/)
			pp_pub[++pp_pub_d] = ppline
		else if (ppline ~ /^#else/)
			pp_pub[pp_pub_d] = pp_pub[pp_pub_d] "!"
		else if (ppline ~ /^#endif/ && pp_pub_d > 0)
			pp_pub_d--
		print ppline >> i_pfile
		print ppline >> i_dfile
		next
	}
	pline = sprintf("%s %s", pline, $0)
	if (pline ~ /\)\);/) {
		sub(/^[	 ]*/, "", pline)
		# Defensive de-dup keyed on (active guard stack + prototype):
		# a symbol may carry a PUBLIC: prototype in more than one
		# #if/#else arm (a real impl plus a platform stub with the same
		# signature) -- drop those true duplicates so the header is
		# idempotent -- while the same prototype under a DIFFERENT guard
		# (a different tier) is kept.
		guard = ""
		for (gi = 1; gi <= pp_pub_d; gi++) guard = guard "|" pp_pub[gi]
		key = guard "::" pline
		if (key in seen_pub) { pline = ""; next }
		seen_pub[key] = 1
		print pline >> i_pfile
		if (pline !~ db_version_unique_name) {
			gsub(/[	 ][	 ]*__P.*/, "", pline)
			sub(/^.*[	 ][*]*/, "", pline)
			printf("#define	%s %s@DB_VERSION_UNIQUE_NAME@\n",
			    pline, pline) >> i_dfile
		}
		pline = ""
	}
}

/EXTERN:/ {
	sub(/^.*EXTERN:[	 ][	 ]*/, "")
	if ($0 ~ /^#if|^#ifdef|^#ifndef|^#else|^#endif/) {
		ppline = $0
		while (ppline ~ /\\$/) {
			sub(/\\$/, "", ppline)
			if (getline <= 0)
				break
			sub(/^.*EXTERN:[	 ][	 ]*/, "")
			ppline = ppline $0
		}
		if (ppline ~ /^#if|^#ifdef|^#ifndef/)
			pp_ext[++pp_ext_d] = ppline
		else if (ppline ~ /^#else/)
			pp_ext[pp_ext_d] = pp_ext[pp_ext_d] "!"
		else if (ppline ~ /^#endif/ && pp_ext_d > 0)
			pp_ext_d--
		print ppline >> e_pfile
		print ppline >> e_dfile
		next
	}
	eline = sprintf("%s %s", eline, $0)
	if (eline ~ /\)\);/) {
		sub(/^[	 ]*/, "", eline)
		guard = ""
		for (gi = 1; gi <= pp_ext_d; gi++) guard = guard "|" pp_ext[gi]
		key = guard "::" eline
		if (key in seen_ext) { eline = ""; next }
		seen_ext[key] = 1
		print eline >> e_pfile
		if (eline !~ db_version_unique_name) {
			gsub(/[	 ][	 ]*__P.*/, "", eline)
			sub(/^.*[	 ][*]*/, "", eline)
			printf("#define	%s %s@DB_VERSION_UNIQUE_NAME@\n",
			    eline, eline) >> e_dfile
		}
		eline = ""
	}
}

/^DB_LOG_RECSPEC.*_desc\[\]/ {
    sub(/DB_LOG_RECSPEC[ 	]*/, "");
    sub(/\[][ 	]*=[ 	]*{.*$/, "");
    printf("#define\t%s %s@DB_VERSION_UNIQUE_NAME@\n", $0, $0) >> i_dfile
}
