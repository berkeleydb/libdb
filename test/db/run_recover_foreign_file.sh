#!/bin/sh -
#
# $Id$
#
# run_recover_foreign_file.sh --
#	Build and run recover_foreign_file.c, the regression test for recovery
#	meeting a data file referenced in the log that is now a non-Berkeley
#	DB file (empty/foreign).  __dbreg_open_file / __dbreg_register_recover
#	(src/dbreg) failed all of recovery on the resulting EINVAL; the
#	environment-open path turned that into a panic (abort, in diagnostic
#	builds).  A missing file was already tolerated; a foreign one must be
#	too.  The test kills a registered peer so recovery runs, replaces one
#	database with an empty file, reopens, and checks a second database
#	kept its committed record and the environment was not panicked.
#
# Exits non-zero on failure or hang.

set -e

. "$(cd "$(dirname "$0")" && pwd)/runner_lib.sh"
rl_default_build
_HERE=$(cd "$(dirname "$0")" && pwd)
SRC=${SRC:-$_HERE/recover_foreign_file.c}
HOME_DIR=${HOME_DIR:-RECOVER_FOREIGN_FILE}
TIMEOUT=${TIMEOUT:-180}

rl_find_lib

rl_compile "$SRC" "$BUILD/recover_foreign_file.t"

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

echo "Running recover_foreign_file (timeout ${TIMEOUT}s)"
if run_with_timeout "$TIMEOUT" "$BUILD/recover_foreign_file.t"; then
	echo "run_recover_foreign_file.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_recover_foreign_file.sh: FAIL (rc=$rc)"
	exit $rc
fi
