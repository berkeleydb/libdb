#!/bin/sh -
#
# $Id$
#
# run_log_emfile_nopanic.sh --
#	Build and run log_emfile_nopanic.c, the regression test for the
#	environment panic when a log-file open fails with EMFILE/ENFILE.
#	__log_name (src/log/log_put.c) treated any non-ENOENT open error as
#	fatal and panicked the environment (abort, in diagnostic builds); a
#	transient descriptor shortage must instead be returned to the caller.
#	The condition is injected through db_env_set_func_open, so no real
#	descriptor exhaustion is needed.
#
# Exits non-zero on failure or hang.

set -e

. "$(cd "$(dirname "$0")" && pwd)/runner_lib.sh"
rl_default_build
_HERE=$(cd "$(dirname "$0")" && pwd)
SRC=${SRC:-$_HERE/log_emfile_nopanic.c}
HOME_DIR=${HOME_DIR:-LOG_EMFILE_NOPANIC}
TIMEOUT=${TIMEOUT:-180}

rl_find_lib

rl_compile "$SRC" "$BUILD/log_emfile_nopanic.t"

# Start from an EMPTY home, regions included. Removing only *.db left the
# previous run's __db.00N region files behind, so a second run (another arm,
# another build) attached a region it did not create: on FreeBSD that is a
# PANIC (a pshared mutex whose initialiser has exited, tracker row B3), and
# between builds it is a DB_VERSION_MISMATCH. Either way a false red.
# Clear any leftover of the home, whether a dir OR a stray file: on a reused
# CI workspace (seen on the macOS runners) a prior run can leave the path as
# something mkdir -p then refuses with "File exists", aborting under set -e.
# This driver homes itself under /tmp via mkdtemp; it ignores $HOME_DIR.
# Do NOT create a dir named like the binary here: on a case-insensitive
# filesystem (macOS) "COMPACT_SUBDB_REMOVE" folds onto "./compact_subdb_remove",
# shadowing the compiled binary with a directory (rc=126, "is a directory").

echo "Running log_emfile_nopanic (timeout ${TIMEOUT}s)"
if run_with_timeout "$TIMEOUT" "$BUILD/log_emfile_nopanic.t"; then
	echo "run_log_emfile_nopanic.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_log_emfile_nopanic.sh: FAIL (rc=$rc)"
	exit $rc
fi
