#!/bin/sh -
#
# $Id$
#
# run_curadj_dup_partition.sh --
#	Build and run curadj_dup_partition.c, the regression test for the
#	cursor-adjustment retry protocol on a DB_THREAD handle.
#
#	__db_walk_cursors() holds a cq_parts[] partition mutex across its
#	callback, but __bam_ca_dup_func / __bam_ca_undodup_func /
#	__ham_chgpg_recover_func kept dropping dbp->mutex -- the mutex the walk
#	held before the cursor queues were sharded.  On a DB_THREAD handle that
#	unlocks a mutex the thread does not hold and then re-enters the held
#	partition mutex through __db_cursor_int / __dbc_close, which deadlocks.
#
#	THIS IS PRIMARILY A HANG TEST, hence the timeout below: against the
#	defect the test process blocks forever in __db_cursor_int rather than
#	failing.  Non-threaded handles leave these mutexes MUTEX_INVALID, which
#	is why the TCL cursor suites cannot see this and this test uses
#	DB_THREAD deliberately.
#
# Exits non-zero on failure or hang.

set -e

. "$(cd "$(dirname "$0")" && pwd)/runner_lib.sh"
rl_default_build
_HERE=$(cd "$(dirname "$0")" && pwd)
SRC=${SRC:-$_HERE/curadj_dup_partition.c}
HOME_DIR=${HOME_DIR:-CURADJ_DUP_TESTDIR}
TIMEOUT=${TIMEOUT:-180}

rl_find_lib

rl_compile "$SRC" "$BUILD/curadj_dup_partition"

rl_fresh_home "$HOME_DIR"

echo "Running curadj_dup_partition (timeout ${TIMEOUT}s)"
if run_with_timeout "$TIMEOUT" "$BUILD/curadj_dup_partition" "$HOME_DIR"; then
	echo "run_curadj_dup_partition.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_curadj_dup_partition.sh: FAIL (rc=$rc)"
	exit $rc
fi
