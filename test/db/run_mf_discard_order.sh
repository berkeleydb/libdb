#!/bin/sh -
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# run_mf_discard_order.sh --
#	Build and run mf_discard_order.c: a DB_TRUNCATE open of an existing,
#	written file discards its MPOOLFILE.  The discard took the file's hash
#	bucket under the mpool region (allocator) latch, the inverse of the
#	global lock order; a --enable-diagnostic build's checker PANICs there
#	(BDB2077).  Meaningful as a lock-order test in a diagnostic build; in a
#	release build it checks the functional result only.
#
# Exits non-zero on failure.

set -e

. "$(cd "$(dirname "$0")" && pwd)/runner_lib.sh"
rl_default_build
_HERE=$(cd "$(dirname "$0")" && pwd)
SRC=${SRC:-$_HERE/mf_discard_order.c}
HOME_DIR=${HOME_DIR:-MF_DISCARD_ORDER_TESTDIR}
TIMEOUT=${TIMEOUT:-180}

rl_find_lib

rl_compile "$SRC" "$BUILD/mf_discard_order"

rl_fresh_home "$HOME_DIR"

echo "Running mf_discard_order (timeout ${TIMEOUT}s)"
if run_with_timeout "$TIMEOUT" "$BUILD/mf_discard_order" "$HOME_DIR"; then
	echo "run_mf_discard_order.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_mf_discard_order.sh: FAIL (rc=$rc)"
	exit $rc
fi
