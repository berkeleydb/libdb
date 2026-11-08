#!/bin/sh -
#
# $Id$
#
# run_null_method_slots.sh --
#	Build and run null_method_slots.c, the regression test for
#	https://github.com/berkeleydb/libdb/issues/149 -- two NULL
#	dereferences reachable from documented public APIs:
#
#	1. DB_ENV->rep_get_nsites() on a repmgr-configured but unopened
#	   environment dereferenced db_rep->region (attached only at open).
#	2. __cdsgroup_begin() left 4 of DB_TXN's 12 method slots NULL, so
#	   calling one was an indirect call through a NULL pointer.
#
# Exits non-zero on failure or hang.

set -e

. "$(cd "$(dirname "$0")" && pwd)/runner_lib.sh"
rl_default_build
_HERE=$(cd "$(dirname "$0")" && pwd)
SRC=${SRC:-$_HERE/null_method_slots.c}
HOME_DIR=${HOME_DIR:-NULL_METHOD_TESTDIR}
TIMEOUT=${TIMEOUT:-180}

rl_find_lib

rl_compile "$SRC" "$BUILD/null_method_slots"

rl_fresh_home "$HOME_DIR"

echo "Running null_method_slots (timeout ${TIMEOUT}s)"
if run_with_timeout "$TIMEOUT" "$BUILD/null_method_slots"; then
	echo "run_null_method_slots.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_null_method_slots.sh: FAIL (rc=$rc)"
	exit $rc
fi
