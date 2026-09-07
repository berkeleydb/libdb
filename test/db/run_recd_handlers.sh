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

BUILD=${BUILD:-.}
SRC=${SRC:-../test/db/recd_handlers.c}
HOME_DIR=${HOME_DIR:-RECD_HANDLERS_TESTDIR}
TIMEOUT=${TIMEOUT:-300}

# Prefer the STATIC library: on macOS the .dylib carries a baked-in install name
# (/usr/local/BerkeleyDB.5.3/lib/...) which takes precedence over -rpath, so a
# shared link runs against an uninstalled path and dyld aborts.  Static linking
# avoids the dynamic loader entirely, which is what the other CI-wired suites
# (e.g. test/fuzz) already do.  Fall back to the shared library if no static one
# was built.
LIB=""
LIBRPATH=""
for cand in "$BUILD"/libdb.a "$BUILD"/.libs/libdb-5.3.a "$BUILD"/.libs/libdb-*.a; do
	if [ -f "$cand" ]; then LIB="$cand"; break; fi
done
if [ -z "$LIB" ]; then
	for cand in "$BUILD"/.libs/libdb-5.3.so "$BUILD"/.libs/libdb-5.3.dylib \
	    "$BUILD"/.libs/libdb-*.so "$BUILD"/.libs/libdb-*.dylib; do
		if [ -f "$cand" ]; then LIB="$cand"; break; fi
	done
	[ -n "$LIB" ] && LIBRPATH="-Wl,-rpath,$(cd "$BUILD/.libs" && pwd)"
fi
[ -n "$LIB" ] || { echo "FAIL: no libdb library (static or shared) found under $BUILD"; exit 1; }

# A static libdb needs its transitive deps named explicitly.
EXTRALIBS=""
for l in $(pkg-config --libs liburing 2>/dev/null); do EXTRALIBS="$EXTRALIBS $l"; done

echo "Compiling recd_handlers against $LIB"
"${CC:-cc}" -g -O1 ${CFLAGS:-} -I"$BUILD" "$SRC" "$LIB" \
    -lpthread $LIBRPATH $EXTRALIBS \
    -o "$BUILD/recd_handlers"

rm -f "$HOME_DIR"/__db.* "$HOME_DIR"/log.* "$HOME_DIR"/*.db \
    "$HOME_DIR"/DB_CONFIG 2>/dev/null || true
mkdir -p "$HOME_DIR"

# `timeout` is GNU coreutils: present on Linux, absent on stock macOS (where it
# is `gtimeout` if coreutils is installed).  Resolve it once; if neither exists,
# run without a timeout rather than failing with rc=127.
if command -v timeout >/dev/null 2>&1; then
	TIMEOUT_CMD="timeout"
elif command -v gtimeout >/dev/null 2>&1; then
	TIMEOUT_CMD="gtimeout"
else
	TIMEOUT_CMD=""
fi
run_with_timeout() {
	if [ -n "$TIMEOUT_CMD" ]; then
		"$TIMEOUT_CMD" "$@"
	else
		shift	# drop the seconds argument
		"$@"
	fi
}
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
