#!/bin/sh -
#
# $Id$
#
# run_qam_extent_vrfy.sh --
#	Build and run qam_extent_vrfy.c, the regression gate for queue extent
#	verification.  PR #160 bounded __qam_vrfy_walkqueue()'s scan by
#	vdp->last_pgno, which is the last page of the MAIN .db file -- but a
#	queue with extents keeps only its meta page there, so the bound
#	collapsed to 0 and the extent walk was skipped: db_verify reported
#	success on a queue with a corrupted extent page (shipped in 5.3.35).
#
#	The test asserts BOTH directions, because a verifier that silently does
#	nothing also passes "legitimate queues still verify OK":
#	  1. a legitimate many-extent queue verifies clean, and
#	  2. a corrupted extent page is still DETECTED.
#
# Exits non-zero on failure or hang.

set -e

. "$(cd "$(dirname "$0")" && pwd)/runner_lib.sh"
rl_default_build
_HERE=$(cd "$(dirname "$0")" && pwd)
SRC=${SRC:-$_HERE/qam_extent_vrfy.c}
HOME_DIR=${HOME_DIR:-QAM_EXTENT_VRFY_TESTDIR}
# 600 s, not the usual 180 s: building the many-extent queue fdatasyncs each of
# its ~775 extent files (eviction-discard during the fill and the per-file sync at
# close), so the run time is the host's fsync latency times 775.  That was
# 162 s on GitHub's ubuntu-24.04 runners and 181 s on ubuntu-26.04, which timed
# out at 180 s.  The I/O pattern is what the test exercises; do not shorten it by
# skipping syncs (DB_NOSYNC + DB_NOFLUSH changed case 2's verdict from
# DB_VERIFY_BAD to EINVAL).
TIMEOUT=${TIMEOUT:-600}

rl_find_lib

rl_compile "$SRC" "$BUILD/qam_extent_vrfy"

rl_fresh_home "$HOME_DIR"

echo "Running qam_extent_vrfy (timeout ${TIMEOUT}s)"
if run_with_timeout "$TIMEOUT" "$BUILD/qam_extent_vrfy"; then
	echo "run_qam_extent_vrfy.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_qam_extent_vrfy.sh: FAIL (rc=$rc)"
	exit $rc
fi
