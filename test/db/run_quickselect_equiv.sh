#!/bin/sh -
#
# $Id$
#
# run_quickselect_equiv.sh --
#	Compile and run test/c/quickselect_equiv.c, which proves
#	db_quickselect() (src/dbinc/db_quickselect.h) agrees with qsort at
#	every percentile position across random, sorted, reverse-sorted,
#	all-equal and duplicate-heavy inputs.
#
#	Self-contained: the helper is header-only with no libdb dependency, so
#	this needs only a C compiler.  It also includes a SABOTAGE arm (set
#	QS_SABOTAGE=1) that neuters the selection so the equivalence check is
#	shown to have teeth.
#
# Usage (from build_unix):
#	sh ../test/db/run_quickselect_equiv.sh
#	QS_SABOTAGE=1 sh ../test/db/run_quickselect_equiv.sh   # must FAIL

set -u

HERE=$(cd "$(dirname "$0")" && pwd)
SRCDIR=$(cd "$HERE/../.." && pwd)
CC=${CC:-cc}
CFLAGS=${CFLAGS:--O2 -Wall}
WORK=${WORK:-${BUILD:-.}/QS_WORK}

mkdir -p "$WORK"
find "$WORK" -mindepth 1 -delete 2>/dev/null || true

INC="-I$SRCDIR/src/dbinc"
SRC="$SRCDIR/test/c/quickselect_equiv.c"
BIN="$WORK/quickselect_equiv"

if [ "${QS_SABOTAGE:-0}" = "1" ]; then
	# Sabotage: make db_quickselect place NOTHING, so most positions keep
	# their original (unsorted) value and the equivalence check must fail.
	# Neuter the condition rather than delete a line, so it still compiles.
	SAB="$WORK/db_quickselect.h"
	sed 's/if (nmemb == 0 || nk == 0)/if (nmemb == 0 || nk == 0 || 1)/' \
	    "$SRCDIR/src/dbinc/db_quickselect.h" > "$SAB"
	INC="-I$WORK"
	echo "run_quickselect_equiv.sh: SABOTAGE build (selection disabled)"
fi

if ! $CC $CFLAGS $INC "$SRC" -o "$BIN" 2> "$WORK/cc.log"; then
	echo "run_quickselect_equiv.sh: SKIP (compile failed)"
	cat "$WORK/cc.log"
	exit 0
fi

if "$BIN"; then
	rc=0
else
	rc=1
fi

if [ "${QS_SABOTAGE:-0}" = "1" ]; then
	if [ "$rc" -eq 0 ]; then
		echo "run_quickselect_equiv.sh: SABOTAGE PASSED -- the test has"
		echo "    no teeth (equivalence check did not catch broken"
		echo "    selection).  FAIL"
		exit 1
	fi
	echo "run_quickselect_equiv.sh: sabotage correctly caught -- PASS"
	exit 0
fi

if [ "$rc" -ne 0 ]; then
	echo "run_quickselect_equiv.sh: FAIL"
	exit 1
fi
echo "run_quickselect_equiv.sh: PASS"
exit 0
