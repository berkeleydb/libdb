#!/bin/sh -
#
# $Id$
#
# run_compact_subdb_remove.sh --
#	Build and run compact_subdb_remove.c, the regression test for the
#	compaction hang when a sub-database is removed and a sibling's
#	metadata page is moved into the freed page in the same transaction.
#	__db_move_metadata (src/db/db_compact.c) asked for the handle lock on
#	the moved page under the dbp's own handle locker rather than the
#	transaction locker, self-deadlocking against the WRITE handle lock the
#	transaction already held from the pending remove.  The test itself is
#	SIGALRM-guarded: a hang is reported as a failure, never a hung suite.
#
# Exits non-zero on failure or hang.

set -e

. "$(cd "$(dirname "$0")" && pwd)/runner_lib.sh"
rl_default_build
_HERE=$(cd "$(dirname "$0")" && pwd)
SRC=${SRC:-$_HERE/compact_subdb_remove.c}
HOME_DIR=${HOME_DIR:-COMPACT_SUBDB_REMOVE}
TIMEOUT=${TIMEOUT:-180}

rl_find_lib

rl_compile "$SRC" "$BUILD/compact_subdb_remove.t"

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

echo "Running compact_subdb_remove (timeout ${TIMEOUT}s)"
if run_with_timeout "$TIMEOUT" "$BUILD/compact_subdb_remove.t"; then
	echo "run_compact_subdb_remove.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_compact_subdb_remove.sh: FAIL (rc=$rc)"
	exit $rc
fi
