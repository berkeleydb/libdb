#!/bin/sh -
#
# $Id$
#
# run_qam_readpath_bound.sh --
#	Regression gate for issue #159 -- the queue cursor read path probed the
#	filesystem once per record, so a corrupt meta page claiming a wrapped
#	near-UINT32_MAX range turned a cursor walk into billions of stat(2)
#	calls.
#
#	Asserts both directions, since a bound that is too tight silently
#	truncates real data (worse than the DoS): a legitimate 770-extent queue
#	reads back every record and survives consume + re-append, AND a hostile
#	wrapped meta page is bounded promptly.
#
# Exits non-zero on failure or hang.

set -e

. "$(cd "$(dirname "$0")" && pwd)/runner_lib.sh"
rl_default_build
_HERE=$(cd "$(dirname "$0")" && pwd)
SRC=${SRC:-$_HERE/qam_readpath_bound.c}
HOME_DIR=${HOME_DIR:-QAM_READPATH_TESTDIR}
TIMEOUT=${TIMEOUT:-180}

rl_find_lib

rl_compile "$SRC" "$BUILD/qam_readpath_bound"

rl_fresh_home "$HOME_DIR"

echo "Running qam_readpath_bound (timeout ${TIMEOUT}s)"
if run_with_timeout "$TIMEOUT" "$BUILD/qam_readpath_bound"; then
	echo "run_qam_readpath_bound.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_qam_readpath_bound.sh: FAIL (rc=$rc)"
	exit $rc
fi
