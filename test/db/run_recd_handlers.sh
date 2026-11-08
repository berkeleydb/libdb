#!/bin/sh -
#
# $Id$
#
# run_recd_handlers.sh --
#	Build and run recd_handlers.c, which drives four recovery-record
#	handlers the Tcl recd0NN suite never reaches:
#	  bt_rec.c: __bam_irep_recover, __bam_root_recover, __bam_rcuradj_recover
#	  db_rec.c: __db_ovref_recover
#	Each is fed a scenario that logs its record (subdb-create root update,
#	compaction internal-record replace, rrecno child-txn cursor adjust,
#	truncate of a btree with overflow items) and then replayed under
#	DB_RECOVER / DB_RECOVER_FATAL (and txn abort for the undo paths).
#	(__db_cksum_recover is documented-but-uncovered: its marker record is
#	unreachable by recovery -- redo dies on the corrupt page first.)
#
# Usage (from build_unix):
#	sh ../test/db/run_recd_handlers.sh
#
# Exits non-zero on failure or hang.

set -e

. "$(cd "$(dirname "$0")" && pwd)/runner_lib.sh"
rl_default_build
_HERE=$(cd "$(dirname "$0")" && pwd)
SRC=${SRC:-$_HERE/recd_handlers.c}
HOME_DIR=${HOME_DIR:-RECD_HANDLERS_TESTDIR}
TIMEOUT=${TIMEOUT:-300}

rl_find_lib

rl_compile "$SRC" "$BUILD/recd_handlers"

rm -f "$HOME_DIR"/__db.* "$HOME_DIR"/log.* "$HOME_DIR"/*.db \
    "$HOME_DIR"/DB_CONFIG 2>/dev/null || true
mkdir -p "$HOME_DIR"

echo "Running recd_handlers (timeout ${TIMEOUT}s)"
if run_with_timeout "$TIMEOUT" "$BUILD/recd_handlers"; then
	echo "run_recd_handlers.sh: PASS"
	rm -f "$HOME_DIR"/__db.* "$HOME_DIR"/log.* "$HOME_DIR"/*.db \
	    "$HOME_DIR"/DB_CONFIG 2>/dev/null || true
	exit 0
else
	rc=$?
	echo "run_recd_handlers.sh: FAIL (rc=$rc)"
	exit $rc
fi
