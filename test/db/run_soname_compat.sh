#!/bin/sh -
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# run_soname_compat.sh --
#	rfc/0020: a process at the new compatibility level refuses the previous
#	level's region with DB_VERSION_MISMATCH, and opens and recovers the
#	previous level's databases and logs once that region is gone.
#
#	Needs a static library of the previous level.  Point OLD_BUILD at a
#	configured+built tree of the previous level (for example v2026.10.8).  If
#	OLD_BUILD is unset the test SKIPs: CI provides it, a local run need not.
#
#	Usage (from the build directory):  OLD_BUILD=/path/to/old/build_unix \
#	    sh ../test/db/run_soname_compat.sh
#	Env: BUILD (default .), OLD_BUILD, TIMEOUT (default 300).

set -e

BUILD=${BUILD:-.}
_HERE=$(cd "$(dirname "$0")" && pwd)
SRC=$_HERE/soname_compat.c
HOME_DIR=${HOME_DIR:-$BUILD/SONAME_COMPAT_TESTDIR}

if [ -z "${OLD_BUILD:-}" ]; then
	echo "run_soname_compat.sh: SKIP OLD_BUILD (a build of the previous compatibility level) is not set"
	exit 0
fi
for b in "$BUILD" "$OLD_BUILD"; do
	[ -f "$b/libdb.a" ] || { echo "run_soname_compat.sh: FAIL no $b/libdb.a"; exit 1; }
done
lvl() { sed -n 's/^#define[	 ]*DB_VERSION_\(MAJOR\|MINOR\)[	 ]*\([0-9]*\).*/\2/p' "$1/db.h" | tr '\n' . ; }
NEW_LVL=$(lvl "$BUILD"); OLD_LVL=$(lvl "$OLD_BUILD")
if [ "$NEW_LVL" = "$OLD_LVL" ]; then
	echo "run_soname_compat.sh: FAIL both builds are level $NEW_LVL; OLD_BUILD must be the previous level"
	exit 1
fi
echo "Compiling soname_compat: new level $NEW_LVL, old level $OLD_LVL"
"${CC:-cc}" -g -O1 -I"$BUILD" "$SRC" "$BUILD/libdb.a" -lpthread -o "$BUILD/soname_compat_new"
"${CC:-cc}" -g -O1 -I"$OLD_BUILD" "$SRC" "$OLD_BUILD/libdb.a" -lpthread -o "$BUILD/soname_compat_old"

[ -d "$HOME_DIR" ] && find "$HOME_DIR" -mindepth 1 -delete
mkdir -p "$HOME_DIR"
rc=0
"$BUILD/soname_compat_old" create "$HOME_DIR" || rc=1
"$BUILD/soname_compat_new" attach "$HOME_DIR" || rc=1
"$BUILD/soname_compat_old" close "$HOME_DIR" || rc=1
"$BUILD/soname_compat_new" open "$HOME_DIR" || rc=1
if [ $rc -eq 0 ]; then
	echo "run_soname_compat.sh: PASS"
else
	echo "run_soname_compat.sh: FAIL"
fi
exit $rc
