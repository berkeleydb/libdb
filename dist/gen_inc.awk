# This awk script parses C input files looking for lines marked "PUBLIC:"
# and "EXTERN:".  (PUBLIC lines are DB internal function prototypes and
# #defines, EXTERN are DB external function prototypes and #defines.)  The
# PUBLIC lines are put into two versions of per-directory include files:
# one version for normal use, and one version to be post-processed based
# on creating unique file names for every global symbol in the DB library.
# The EXTERN lines are put into two versions of the db.h file, again, one
# version for normal use, and one version to be post-processed for unique
# naming.
/PUBLIC:/ {
	sub("^.*PUBLIC:[	 ][	 ]*", "")
	if ($0 ~ "^#if|^#else|^#endif") {
		print $0 >> inc_file
		print $0 >> uinc_file
		next
	}
	if ($0 ~ "^#define.*[(]") {
		print $0 >> inc_file
		def = gensub("[(]", "@DB_VERSION_UNIQUE_NAME@(", 2)
		print def >> uinc_file
		next
	}
	if ($0 ~ "^#define") {
		print $0 >> inc_file
		sub("[	 ]*$", "@DB_VERSION_UNIQUE_NAME@")
		print $0 >> uinc_file
		next
	}
	pline = sprintf("%s %s", pline, $0)
	if (pline ~ "));") {
		sub("^[	 ]*", "", pline)
		print pline >> inc_file
		if (pline ~ db_version_unique_name)
			print pline >> uinc_file;
		else {
			def = gensub("[	 ][	 ]*__P.*", "", 1, pline)
			sub("^.*[	 ][*]*", "", def)
			printf("#define	%s %s@DB_VERSION_UNIQUE_NAME@\n%s\n",
			    def, def, pline) >> uinc_file
		}
		pline = ""
	}
}

# When we switched to methods in 4.0, we guessed txn_{abort,begin,commit}
# were the interfaces applications would likely use and not be willing to
# change, due to the sheer volume of the calls.  Provide wrappers -- we
# could do txn_abort and txn_commit using macros, but not txn_begin, as
# the name of the field is txn_begin, we didn't want to modify it.
#
# The issue with txn_begin hits us in another way.  If configured with the
# --with-uniquename option, we use #defines to re-define DB's interfaces
# to unique names.  We can't do that for these functions because txn_begin
# is also a field name in the DB_ENV structure, and the #defines we use go
# at the end of the db.h file -- we get control too late to #define a field
# name.  So, modify the script that generates the unique names #defines to
# not generate them for these three functions, and don't include the three
# functions in libraries built with that configuration option.
/EXTERN:/ {
	sub("^.*EXTERN:[	 ][	 ]*", "")
	if ($0 ~ "^#if|^#else|^#endif") {
		print $0 >> ext_file
		print $0 >> uext_file
		next
	}
	if ($0 ~ "^#define.*[(]") {
		print $0 >> ext_file
		def = gensub("[(]", "@DB_VERSION_UNIQUE_NAME@(", 2)
		print def >> uext_file
		next
	}
	if ($0 ~ "^#define") {
		print $0 >> ext_file
		sub("[	 ]*$", "@DB_VERSION_UNIQUE_NAME@")
		print $0 >> uext_file
		next
	}
	eline = sprintf("%s %s", eline, $0)
	if (eline ~ "));") {
		sub("^[	 ]*", "", eline)
		print eline >> ext_file
		if (eline ~ db_version_unique_name || eline ~ "^int txn_")
			print eline >> uext_file;
		else {
			def = gensub("[	 ][	 ]*__P.*", "", 1, eline)
			sub("^.*[	 ][*]*", "", def)
			printf("#define	%s %s@DB_VERSION_UNIQUE_NAME@\n%s\n",
			    def, def, eline) >> uext_file
		}
		eline = ""
	}
}
