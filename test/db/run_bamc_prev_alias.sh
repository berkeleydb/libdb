#!/bin/sh -
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# run_bamc_prev_alias.sh --
#	Build and run bamc_prev_alias.c, the regression test for the
#	strict-aliasing miscompile in __bamc_prev: IS_CUR_DELETED read the
#	cursor index through DBC_INTERNAL * while the walk wrote it through
#	BTREE_CURSOR *, and clang -O2 on aarch64 reordered the two, so DB_PREV
#	skipped live records next to deleted ones.
#
#	The test is compiled -O1 but the LIBRARY is the build's own -O2 one,
#	which is where the reorder happens, so this checks the shipped code.
#
# Exits non-zero on failure.

set -e

. "$(cd "$(dirname "$0")" && pwd)/runner_lib.sh"
rl_default_build
_HERE=$(cd "$(dirname "$0")" && pwd)
SRC=${SRC:-$_HERE/bamc_prev_alias.c}
HOME_DIR=${HOME_DIR:-BAMC_PREV_ALIAS_TESTDIR}
TIMEOUT=${TIMEOUT:-180}

rl_find_lib

rl_compile "$SRC" "$BUILD/bamc_prev_alias"

rl_fresh_home "$HOME_DIR"

echo "Running bamc_prev_alias (timeout ${TIMEOUT}s)"
if run_with_timeout "$TIMEOUT" "$BUILD/bamc_prev_alias" "$HOME_DIR"; then
	echo "run_bamc_prev_alias.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_bamc_prev_alias.sh: FAIL (rc=$rc)"
	exit $rc
fi
