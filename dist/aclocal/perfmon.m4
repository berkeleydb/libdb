# $Id$

# Determine what kind of application-specified performance event monitoring
# support is available for this platform.  --enable-dtrace builds the `bdb'
# USDT provider (dist/db_provider.d) with whichever dtrace(1) is on the PATH:
#
#	native, -G	FreeBSD, illumos/Solaris.  The objects are post-processed
#			with `dtrace -G', which rewrites each probe call site and
#			emits a DOF object (db_provider.o) that is linked in.
#	native		macOS.  The linker resolves the probes; no `dtrace -G'.
#	sdt		Linux.  /usr/bin/dtrace is SystemTap's python wrapper
#			(Debian/Ubuntu systemtap-sdt-dev, Fedora/RHEL
#			systemtap-sdt-devel) and <sys/sdt.h> turns each probe
#			into an ELF .note.stapsdt entry that bpftrace, perf and
#			stap all read.  The SystemTap runtime (stap) is NOT
#			needed to build.
#
# --enable-systemtap is accepted as a synonym for --enable-dtrace.

AC_DEFUN(AM_DEFINE_PERFMON, [

AH_TEMPLATE(HAVE_PERFMON,
    [Define to 1 to enable some kind of performance event monitoring.])
AH_TEMPLATE(HAVE_PERFMON_STATISTICS,
    [Define to 1 to enable performance event monitoring of *_stat() statistics.])
AH_TEMPLATE(HAVE_DTRACE,
    [Define to 1 to use dtrace for performance monitoring.])

if test "$db_cv_systemtap" = "yes" ; then
	db_cv_dtrace="yes"
fi
if test "$db_cv_dtrace" = "yes" ; then
	db_cv_perfmon="yes"
fi

AC_SUBST(DTRACE_CPP)
DTRACE_CPP=-C
if test "$db_cv_perfmon" = "yes" ; then
	if test "$db_cv_build_statistics" != "yes"; then
		AC_MSG_ERROR([--enable-dtrace cannot be combined with --disable-statistics])
	fi
	AC_CHECK_HEADERS(sys/sdt.h)
	AC_MSG_CHECKING([which kind of dtrace $DTRACE is])
	if $DTRACE --help 2>&1 | grep -i systemtap > /dev/null; then
		db_cv_dtrace_kind=sdt
		# The wrapper parses the provider itself.  Running cpp first pulls
		# in util/dtrace/dbdefs.d, whose D`type syntax the wrapper cannot
		# parse; it then falls back to a regex parser and writes a header
		# whose macros are named after a garbage provider.
		DTRACE_CPP=
	else
		case "$host_os" in
		darwin*)	db_cv_dtrace_kind=native;;
		*)		db_cv_dtrace_kind="native, -G";;
		esac
	fi
	AC_MSG_RESULT([$db_cv_dtrace_kind])
	if test "$db_cv_dtrace_kind" = sdt -a "$ac_cv_header_sys_sdt_h" != yes; then
		AC_MSG_ERROR([<sys/sdt.h> not found: install systemtap-sdt-dev (Debian, Ubuntu) or systemtap-sdt-devel (Fedora, RHEL)])
	fi

	# Generate the DTrace provider header file. This is duplicated
	# in Makefile.in, to allow custom events to be added.
	$RM db_provider.h db_provider.h.tmp
	if ! $DTRACE -h $DTRACE_CPP -I"$topdir/util/dtrace" \
	    -s "$topdir/dist/db_provider.d" -o db_provider.h.tmp ; then
		AC_MSG_ERROR([Could not build db_provider.h: $DTRACE -h failed])
	fi
	if ! sed -e \
'/^#define[ 	]*BDB_[A-Z_]*(.*)/y/ABCDEFGHIJKLMNOPQRSTUVWXYZ/abcdefghijklmnopqrstuvwxyz/' \
	    db_provider.h.tmp > db_provider.h ; then
		AC_MSG_ERROR([Could not build db_provider.h: sed failed])
	fi
	$RM db_provider.h.tmp
	# Exit 0 is not proof: the systemtap wrapper reports success after
	# writing a header with no usable bdb macros.
	if ! grep '^#define[ 	]*bdb_txn_begin(' db_provider.h > /dev/null; then
		AC_MSG_ERROR([$DTRACE -h wrote a db_provider.h with no bdb_txn_begin() macro])
	fi

	# Every kind except macOS links an extra object made by dtrace -G:
	# the DOF for native DTrace, the probe semaphores for sdt.  It used to
	# be detected by running `dtrace -G' over the provider alone, but
	# FreeBSD and illumos (since illumos 4248, 2013) refuse to build DOF
	# with no probe sites, so the test failed there, the object was left
	# out, and libdb did not link.  db_provider.c is an empty placeholder
	# whose root must match db_provider.d for the Makefile.in rule.
	if test "$db_cv_dtrace_kind" != native ; then
		FINAL_OBJS="$FINAL_OBJS db_provider${o}"
		$RM db_provider.c
		echo "" > db_provider.c
	fi
	AC_DEFINE(HAVE_DTRACE)
	AC_DEFINE(HAVE_PERFMON)
	if test "$db_cv_perfmon_statistics" != "no" ; then
		AC_DEFINE(HAVE_PERFMON_STATISTICS)
	fi
	# `make listprobes'.  sdt probes are ELF notes and can be read without
	# privileges; native DTrace needs a process that has loaded libdb.
	AC_SUBST(LISTPROBES_DEPENDENCY)
	AC_SUBST(LISTPROBES_COMMAND)
	if test "$db_cv_dtrace_kind" = sdt; then
		LISTPROBES_DEPENDENCY=.libs/libdb-$DB_VERSION_MAJOR.$DB_VERSION_MINOR$SOSUFFIX
		LISTPROBES_COMMAND="readelf -n $LISTPROBES_DEPENDENCY | grep -A1 'Provider: bdb'"
	else
		LISTPROBES_DEPENDENCY=db_load
		LISTPROBES_COMMAND="LD_LIBRARY_PATH=.libs $DTRACE -l -n 'bdb\$\$target:::' -c '.libs/db_load -V'"
	fi
elif test "$db_cv_perfmon_statistics" = "yes" ; then
	AC_MSG_ERROR([Enabling perfmon statistics requires --enable-dtrace])
fi
])
