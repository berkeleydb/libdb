# $Id$

# Determine what kind of application-specified event tracing support is
# available for this platform. The options are:
#	Solaris	DTrace 		--enable-dtrace	
#	Linux	SystemTap	--enable-systemtap

AC_DEFUN(AM_DEFINE_PERFMON, [

AH_TEMPLATE(HAVE_PERFMON,
    [Define to 1 to enable some kind of performance event tracing.])
AH_TEMPLATE(HAVE_STATISTICS_PERFMON,
    [Define to 1 to enable performance event tracing of *_stat() statistics.])
AH_TEMPLATE(HAVE_DTRACE,
    [Define to 1 to use dtrace for performance event tracing.])
AH_TEMPLATE(HAVE_SYSTEMTAP,
    [Define to 1 to use stap for performance event tracing.])

if test "$db_cv_dtrace" = "yes" || test "$db_cv_systemtap" = "yes" ; then
	db_cv_perfmon="yes"
fi

if test "$db_cv_perfmon" = "yes" ; then
	if test "$DTRACE" = "dtrace" ; then
		# Generate the DTrace provider header file.
		# This is duplicated in Makefile.in, in case
		# custom events are added.
		if ! dtrace -h -s ../dist/db_provider.d; then
		    AC_MSG_ERROR([Could not build db_provider.d: dtrace failed])
		elif ! mv db_provider.h db_provider.h.tmp ; then
		    AC_MSG_ERROR([Could not build db_provider.d: mv failed])
		elif ! sed -e \
'/^#define[ 	]*BDB_[A-Z_]*(.*)/y/ABCDEFGHIJKLMNOPQRSTUVWXYZ/abcdefghijklmnopqrstuvwxyz/' \
db_provider.h.tmp > db_provider.h ; then
		    AC_MSG_ERROR([Could not build db_provider.d: sed failed])
		fi
		rm -f db_provider.h.tmp

		# DTrace on Solaris needs to post-process .o files to both
		# generate an additional .o as well as resolving the
		# __dtrace___bdb___<xxx> symbols before putting them into
		# libraries;  Mac OS X does not. Treat a failing dtrace -G
		# command as the indicator sign that dtrace -G is unnecessary.
		# If it is needed then create an empty .c file to be a
		# placeholder for the PIC & non-PIC versions of the dtrace -G
		# output file.  The root of this .c file must be the same as
		# the root of the .d file -- i.e. db_provider -- for the
		# dtrace -G lines at the end of Makefile.in to work correctly. 
		if dtrace -G -s ../dist/db_provider.d 2> /dev/null ; then
			FINAL_OBJS="$FINAL_OBJS db_provider${o}"
			rm -f db_provider.c
			echo "" > db_provider.c
		fi
		AC_DEFINE(HAVE_PERFMON)
		AC_DEFINE(HAVE_STATISTICS_PERFMON)
		AC_DEFINE(HAVE_DTRACE)
	elif test "$STAP" = "stap" ; then
		# The dtrace-compatible statically defined tracing header file
		# is needed.
		AC_CHECK_HEADERS(sys/sdt.h)
		if test "$ac_cv_header_sys_sdt_h" != "yes" ; then
			AC_MSG_ERROR([<sys/sdt.h> is missing. Please install the SystemTap sdt development package (e.g., systemtap-sdt-devel) to obtain it.])
		fi
		# Would it be good to add a check that the kernel has the
		# utrace patches needed for systemtap's userspace markers?
		# A warning message would be useful.
		AC_DEFINE(HAVE_PERFMON)
		AC_DEFINE(HAVE_STATISTICS_PERFMON)
		AC_DEFINE(HAVE_SYSTEMTAP)
	else
		AC_MSG_ERROR([No supported performance utility (dtrace or stap) found.])
	fi
fi
])
