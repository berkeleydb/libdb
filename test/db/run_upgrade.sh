#!/bin/sh -
#
# $Id$
#
# run_upgrade.sh --
#	Exercise the on-disk-format upgrade path (src/db/db_upg.c,
#	db_upg_opd.c) by running the db_upgrade utility + DB->upgrade over an
#	old-format Berkeley DB file.
#
#	The full Tcl "upgrade" group (test/tcl/upgrade.tcl) needs a large
#	per-version/per-method fixture tree under test/tcl/upgrade/databases/
#	that is NOT present in this fork.  This script instead uses the one
#	committed old-format fixture (test/csharp/bdb4.7.db, a btree meta
#	version-9 database) to drive the utility's open/version-check path and
#	DB->upgrade's no-op-when-current branch, then verifies the result.
#
# Usage (from build_unix):
#	sh ../test/db/run_upgrade.sh
#
# Exits non-zero on failure.  Runs under a timeout so it cannot hang.

set -e

BUILD=${BUILD:-.}
FIXTURE=${FIXTURE:-../test/csharp/bdb4.7.db}
WORK=${WORK:-UPGTEST}
TIMEOUT=${TIMEOUT:-60}

[ -f "$FIXTURE" ] || { echo "FAIL: fixture $FIXTURE not found"; exit 1; }
[ -x "$BUILD/db_upgrade" ] || { echo "FAIL: $BUILD/db_upgrade missing"; exit 1; }

ABS_UPGRADE=$(cd "$BUILD" && pwd)/db_upgrade
ABS_VERIFY=$(cd "$BUILD" && pwd)/db_verify
ABS_FIXTURE=$(cd "$(dirname "$FIXTURE")" && pwd)/$(basename "$FIXTURE")

rm -f "$WORK"/__db.* "$WORK"/log.* "$WORK"/*.db 2>/dev/null || true
mkdir -p "$WORK"
cp "$ABS_FIXTURE" "$WORK/t.db"

echo "db_upgrade on $(basename "$FIXTURE")"
( cd "$WORK" && timeout "$TIMEOUT" "$ABS_UPGRADE" -h . t.db )
echo "db_verify on upgraded file"
( cd "$WORK" && timeout "$TIMEOUT" "$ABS_VERIFY" t.db )

echo "run_upgrade.sh: PASS"
exit 0
