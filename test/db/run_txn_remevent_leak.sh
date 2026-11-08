#!/bin/sh -
#
# $Id$
#
# run_txn_remevent_leak.sh --
#	Build and run txn_remevent_leak.c, the regression test for the leak of
#	a child transaction's TXN_REMOVE event (DB_ENV->dbremove inside a
#	child txn) when the child or its parent aborts: __txn_doevents
#	(src/txn/txn_util.c) kept the event "for the parent" on abort as well
#	as on commit, but only a commit hands it over, so it was in no list.
#	The test counts libdb's allocations through db_env_set_func_*.
#
# Exits non-zero on failure or hang.

set -e

. "$(cd "$(dirname "$0")" && pwd)/runner_lib.sh"
rl_default_build
_HERE=$(cd "$(dirname "$0")" && pwd)
SRC=${SRC:-$_HERE/txn_remevent_leak.c}
HOME_DIR=${HOME_DIR:-TXN_REMEVENT_TESTDIR}
TIMEOUT=${TIMEOUT:-180}

rl_find_lib

rl_compile "$SRC" "$BUILD/txn_remevent_leak"

rl_fresh_home "$HOME_DIR"

echo "Running txn_remevent_leak (timeout ${TIMEOUT}s)"
if run_with_timeout "$TIMEOUT" "$BUILD/txn_remevent_leak"; then
	echo "run_txn_remevent_leak.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_txn_remevent_leak.sh: FAIL (rc=$rc)"
	exit $rc
fi
