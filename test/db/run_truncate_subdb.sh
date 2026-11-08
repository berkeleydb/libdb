#!/bin/sh -
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# run_truncate_subdb.sh --
#	Build and run truncate_subdb.c: DB->open with DB_TRUNCATE and a
#	subdatabase name must be refused (EINVAL) without touching the file.
#	Unrefused, it truncated the whole file and destroyed every other
#	subdatabase in it.
#
# Exits non-zero on failure.

set -e

. "$(cd "$(dirname "$0")" && pwd)/runner_lib.sh"
rl_default_build
_HERE=$(cd "$(dirname "$0")" && pwd)
SRC=${SRC:-$_HERE/truncate_subdb.c}
HOME_DIR=${HOME_DIR:-TRUNCATE_SUBDB_TESTDIR}
TIMEOUT=${TIMEOUT:-180}

rl_find_lib

rl_compile "$SRC" "$BUILD/truncate_subdb"

rl_fresh_home "$HOME_DIR"

echo "Running truncate_subdb (timeout ${TIMEOUT}s)"
if run_with_timeout "$TIMEOUT" "$BUILD/truncate_subdb" "$HOME_DIR"; then
	echo "run_truncate_subdb.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_truncate_subdb.sh: FAIL (rc=$rc)"
	exit $rc
fi
