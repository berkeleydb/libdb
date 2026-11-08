#!/bin/sh -
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# run_dd_stripes.sh --
#	Build and run dd_stripes.c.  The deadlock detector skips its graph
#	when no cycle is possible and aborts a victim under one locker stripe.
#	The driver runs guaranteed two-transaction cycles, single-object
#	upgrade deadlocks, churning transactions and bystanders at once, and
#	requires every cycle to be resolved (a watchdog catches a missed one as
#	a hang) and no bystander to be aborted.  Run it on a
#	--enable-diagnostic build to also exercise the lock-order checker.
#
#	Usage (from build_unix):  sh ../test/db/run_dd_stripes.sh
#	Env: SECS (default 30), TIMEOUT (default 180).

set -e

. "$(cd "$(dirname "$0")" && pwd)/runner_lib.sh"
rl_default_build
_HERE=$(cd "$(dirname "$0")" && pwd)
SRC=${SRC:-$_HERE/dd_stripes.c}
HOME_DIR=${HOME_DIR:-DD_STRIPES_TESTDIR}
TIMEOUT=${TIMEOUT:-180}
SECS=${SECS:-30}

rl_find_lib

rl_compile "$SRC" "$BUILD/dd_stripes"

rl_fresh_home "$HOME_DIR"

echo "Running dd_stripes (timeout ${TIMEOUT}s)"
if run_with_timeout "$TIMEOUT" "$BUILD/dd_stripes" "$HOME_DIR" "$SECS"; then
	echo "run_dd_stripes.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_dd_stripes.sh: FAIL (rc=$rc)"
	exit $rc
fi
