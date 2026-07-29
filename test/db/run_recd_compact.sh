#!/bin/sh -
#
# $Id$
#
# run_recd_compact.sh --
#	Build and run recd_compact.c, which drives the btree-compaction and
#	page-truncation recovery handlers in src/db/db_rec.c
#	(__db_merge_recover, __db_pgno_recover, __db_pg_trunc_recover) that
#	the Tcl recd0NN suite never reaches (no recd test runs compaction
#	under recovery).  It builds a txn env, fills+sparsifies a btree,
#	compacts with DB_FREE_SPACE (logging merge/pgno/pg_trunc records), then
#	re-opens under DB_RECOVER_FATAL so recovery replays those records.
#
# Usage (from build_unix):
#	sh ../test/db/run_recd_compact.sh
#
# Exits non-zero on failure or hang.

set -e

BUILD=${BUILD:-.}
SRC=${SRC:-../test/db/recd_compact.c}
HOME_DIR=${HOME_DIR:-RECD_COMPACT_TESTDIR}
TIMEOUT=${TIMEOUT:-180}

LIB="$BUILD/.libs/libdb-5.3.so"
if [ ! -f "$LIB" ]; then
	LIB=$(ls "$BUILD"/.libs/libdb-*.so 2>/dev/null | head -1)
fi
[ -n "$LIB" ] || { echo "FAIL: libdb .so not found in $BUILD/.libs"; exit 1; }

echo "Compiling recd_compact against $LIB"
gcc -g -O1 ${CFLAGS:-} -I"$BUILD" "$SRC" "$LIB" \
    -lpthread -Wl,-rpath,"$(cd "$BUILD/.libs" && pwd)" \
    -o "$BUILD/recd_compact"

rm -f "$HOME_DIR"/__db.* "$HOME_DIR"/log.* "$HOME_DIR"/*.db \
    "$HOME_DIR"/DB_CONFIG 2>/dev/null || true
mkdir -p "$HOME_DIR"

echo "Running recd_compact (timeout ${TIMEOUT}s)"
if timeout "$TIMEOUT" "$BUILD/recd_compact"; then
	echo "run_recd_compact.sh: PASS"
	exit 0
else
	rc=$?
	echo "run_recd_compact.sh: FAIL (rc=$rc)"
	exit $rc
fi
