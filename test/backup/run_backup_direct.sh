#!/bin/sh -
#
# $Id$
#
# run_backup_direct.sh --
#	Build and run the standalone hot-backup API driver (backup_direct.c),
#	which exercises Berkeley DB's backup config + callback API
#	(src/env/env_backup.c) and the DB_ENV->backup / DB_ENV->dbbackup engine
#	(src/db/db_backup.c) via the callback path.  The Tcl backup.tcl test
#	drives the db_hotbackup utility only, which leaves env_backup.c at 0%
#	and the backup->open/write/close callback branches of db_backup.c cold.
#
# Usage (from build_unix):
#	sh ../test/backup/run_backup_direct.sh
#
# It compiles backup_direct against the just-built libdb in ./.libs, runs it
# in a guaranteed-clean home under a timeout, and reports PASS/FAIL.  Exits
# non-zero on failure or hang.

set -e

BUILD=${BUILD:-.}			# build_unix dir (cwd by default)
SRC=${SRC:-../test/backup/backup_direct.c}
HOME_DIR=${HOME_DIR:-BACKUP_TESTDIR}
TIMEOUT=${TIMEOUT:-90}

LIB="$BUILD/.libs/libdb-5.3.so"
if [ ! -f "$LIB" ]; then
	# Fall back to the versioned .so actually present.
	LIB=$(ls "$BUILD"/.libs/libdb-*.so 2>/dev/null | head -1)
fi
[ -n "$LIB" ] || { echo "FAIL: libdb .so not found in $BUILD/.libs"; exit 1; }

echo "Compiling backup_direct against $LIB"
gcc -g -O1 ${CFLAGS:-} -I"$BUILD" "$SRC" "$LIB" \
    -lpthread -Wl,-rpath,"$(cd "$BUILD/.libs" && pwd)" \
    -o "$BUILD/backup_direct"

# Guaranteed-clean home: remove env/log/db artifacts (not rm -rf).
rm -f "$HOME_DIR"/__db.* "$HOME_DIR"/log.* "$HOME_DIR"/*.db \
    "$HOME_DIR"/DB_CONFIG "$HOME_DIR"/bak/* 2>/dev/null || true
mkdir -p "$HOME_DIR"/bak

echo "Running backup_direct (timeout ${TIMEOUT}s)"
if timeout "$TIMEOUT" "$BUILD/backup_direct"; then
	echo "run_backup_direct.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_backup_direct.sh: FAIL (rc=$rc)"
	exit $rc
fi
