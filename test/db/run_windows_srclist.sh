#!/bin/sh
# W1 regression: every libdb source compiled on Unix must also be listed in the
# Visual Studio project files.
#
# src/mutex/mut_order.c and src/log/log_handoff_trace.c were absent from all four
# projects, so the DIAGNOSTIC lock-order checker and --enable-handoff-trace could
# not be built on Windows at all.  Neither was a build break -- both files are
# whole-file #ifdef-gated and compile to nothing when their option is off, which
# is the default -- so nothing complained.
#
# This compares the source list the autoconf build uses (dist/srcfiles.in, the
# same list that drives the Unix Makefile) against each project file, and reports
# any source the projects do not mention.  It is a LIST comparison, not a build,
# so it runs anywhere.
#
# Pre-existing omissions are allowlisted individually below with a reason, so a
# NEW omission still fails.

set -u

HERE=$(cd "$(dirname "$0")" && pwd)
SRC=$(cd "$HERE/../.." && pwd)
SRCFILES=$SRC/dist/srcfiles.in

[ -f "$SRCFILES" ] || {
	echo "run_windows_srclist.sh: SKIP no dist/srcfiles.in"
	exit 0
}

PROJECTS="build_windows/VS10/db.vcxproj build_windows/VS8/db.vcproj"

# Sources the Windows projects legitimately do not build.  Each needs a reason,
# so that a NEW omission still fails rather than being absorbed.
#   os/*            POSIX implementations; os_windows/ is used instead
#   *_stub.c        replaced by the real subsystem on a full build
#   clib/rand.c     build_windows/db_config.h defines HAVE_RAND -- the platform
#   clib/snprintf.c and HAVE_SNPRINTF, so these replacements are not compiled
#   mutex/mut_tas.c Windows selects HAVE_MUTEX_WIN32, not a TAS mutex, so this
#                   file is #ifdef'd out in its entirety there
skip_src() {
	case "$1" in
	src/os/*|src/os_vxworks/*|src/os_qnx/*)	return 0 ;;
	*_stub.c)				return 0 ;;
	src/dbinc_auto/*)			return 0 ;;
	src/clib/rand.c|src/clib/snprintf.c)	return 0 ;;
	src/mutex/mut_tas.c)			return 0 ;;
	esac
	return 1
}

rc=0
for proj in $PROJECTS ; do
	[ -f "$SRC/$proj" ] || continue
	missing=
	# srcfiles.in lists "path  tag1 tag2 ..."; take C sources tagged for the
	# main library build.
	for f in $(awk '/^src\/.*\.c[ \t]/ { print $1 }' "$SRCFILES" | sort -u) ; do
		skip_src "$f" && continue
		base=$(basename "$f")
		grep -q -- "$base" "$SRC/$proj" || missing="$missing $base"
	done
	if [ -n "$missing" ] ; then
		echo "  $proj does not list:$missing"
		rc=1
	else
		echo "  $proj: every library source is listed"
	fi
done

if [ "$rc" -eq 0 ] ; then
	echo "run_windows_srclist.sh: PASS"
else
	echo "run_windows_srclist.sh: FAIL"
fi
exit $rc
