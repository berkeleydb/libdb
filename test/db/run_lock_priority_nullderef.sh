#!/bin/sh -
#
# $Id$
#
# run_lock_priority_nullderef.sh --
#	Build and run lock_priority_nullderef.c, the regression test for
#	https://github.com/berkeleydb/libdb/issues/148 --
#	DB_ENV->set_lk_priority() and DB_ENV->get_lk_priority()
#	(src/lock/lock_method.c) dereferenced a NULL locker.
#
#	__lock_getlocker() with create == 0 reports "no such locker" by
#	returning 0 with a NULL locker rather than by returning an error,
#	because callers such as __lock_vec_pp() legitimately expect a locker
#	that holds no locks.  Both priority accessors adopted that idiom
#	without the NULL check, so a plain public-API call naming an id with no
#	live locker raised SIGSEGV.  They now return EINVAL.
#
# Exits non-zero on failure or hang.

set -e

. "$(cd "$(dirname "$0")" && pwd)/runner_lib.sh"
rl_default_build
_HERE=$(cd "$(dirname "$0")" && pwd)
SRC=${SRC:-$_HERE/lock_priority_nullderef.c}
HOME_DIR=${HOME_DIR:-LOCK_PRIORITY_TESTDIR}
TIMEOUT=${TIMEOUT:-180}

rl_find_lib

rl_compile "$SRC" "$BUILD/lock_priority_nullderef"

rl_fresh_home "$HOME_DIR"

echo "Running lock_priority_nullderef (timeout ${TIMEOUT}s)"
if run_with_timeout "$TIMEOUT" "$BUILD/lock_priority_nullderef"; then
	echo "run_lock_priority_nullderef.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_lock_priority_nullderef.sh: FAIL (rc=$rc)"
	exit $rc
fi
