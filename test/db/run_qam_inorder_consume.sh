#!/bin/sh
# Regression for P7: DB_INORDER + DB_CONSUME over a deleted record spins forever.
#
# The defect is a HANG, so rc alone cannot judge it -- a timeout kill and a clean
# exit both need distinguishing from a real verdict.  The driver prints exactly
# one VERDICT line and this script requires it; absence is failure.
#
# Both arms run.  inorder=0 is the control that always worked, so a regression in
# the shared consume path shows up as the control failing too, which is what
# separates "this defect came back" from "consume is broken generally".

set -u

HERE=$(cd "$(dirname "$0")" && pwd)
BUILD=${1:-.}
BUILD=$(cd "$BUILD" && pwd)
SRC="$HERE/qam_inorder_consume.c"

# Library discovery, .dylib handling and the static-link dependency list all
# follow test/db/run_qam_readpath_bound.sh rather than being reinvented here.
LIB=""
LIBRPATH=""
for cand in "$BUILD"/libdb.a "$BUILD"/.libs/libdb-*.a ; do
	if [ -f "$cand" ] ; then LIB="$cand" ; break ; fi
done
if [ -z "$LIB" ] ; then
	for cand in "$BUILD"/.libs/libdb-*.so "$BUILD"/.libs/libdb-*.dylib ; do
		if [ -f "$cand" ] ; then LIB="$cand" ; break ; fi
	done
	[ -n "$LIB" ] && LIBRPATH="-Wl,-rpath,$(cd "$BUILD/.libs" && pwd)"
fi
[ -n "$LIB" ] || { echo "qam_inorder_consume.sh: FAIL no libdb library under $BUILD" ; exit 1 ; }

# A static libdb needs its transitive deps named explicitly (io_uring).
EXTRALIBS=""
for l in $(pkg-config --libs liburing 2>/dev/null) ; do EXTRALIBS="$EXTRALIBS $l" ; done

BIN="$BUILD/qam_inorder_consume"
rm -f "$BIN"
"${CC:-cc}" -g -O1 ${CFLAGS:-} -I"$BUILD" "$SRC" "$LIB" \
    -lpthread $LIBRPATH $EXTRALIBS -o "$BIN" 2>&1 || {
	echo "qam_inorder_consume.sh: FAIL compile" ; exit 1 ; }
# A stale binary from an earlier build would give a false PASS.
test -x "$BIN" || { echo "qam_inorder_consume.sh: FAIL no binary produced" ; exit 1 ; }

# `timeout` is GNU coreutils: present on Linux, absent on stock macOS (where it
# is `gtimeout` if coreutils is installed).  Unlike the other runners we cannot
# fall back to running untimed -- the defect under test is an INFINITE LOOP, so
# an unbounded run would wedge CI instead of reporting.  With neither command
# available, background the driver and poll, which needs only the shell.
if command -v timeout >/dev/null 2>&1 ; then
	TIMEOUT_CMD=timeout
elif command -v gtimeout >/dev/null 2>&1 ; then
	TIMEOUT_CMD=gtimeout
else
	TIMEOUT_CMD=""
fi

# run_limited <secs> <cmd...> -- returns 124 on timeout, like timeout(1).
run_limited() {
	_secs=$1 ; shift
	if [ -n "$TIMEOUT_CMD" ] ; then
		"$TIMEOUT_CMD" "$_secs" "$@"
		return $?
	fi
	"$@" & _pid=$!
	_i=0
	while [ "$_i" -lt "$_secs" ] ; do
		kill -0 "$_pid" 2>/dev/null || { wait "$_pid" ; return $? ; }
		sleep 1
		_i=$((_i + 1))
	done
	kill -9 "$_pid" 2>/dev/null
	wait "$_pid" 2>/dev/null
	return 124
}

rc=0
for arm in 1 0 ; do
	d="$BUILD/QAM_INORDER_$arm"
	rm -f "$d"/* 2>/dev/null
	mkdir -p "$d"
	out=$(run_limited 60 "$BIN" "$d" "$arm" 2>&1)
	kill_rc=$?
	v=$(echo "$out" | grep '^VERDICT p7' | head -1)
	if [ "$kill_rc" -eq 124 ] ; then
		echo "  inorder=$arm: HUNG (no verdict, killed at 60s) -- P7 regression"
		rc=1
	elif [ -z "$v" ] ; then
		echo "  inorder=$arm: no VERDICT line (rc=$kill_rc) -- driver did not report"
		echo "$out" | tail -3 | sed 's/^/    /'
		rc=1
	else
		echo "  inorder=$arm: $v"
		echo "$v" | grep -q PASS || rc=1
	fi
done

if [ "$rc" -eq 0 ] ; then
	echo "qam_inorder_consume.sh: PASS"
else
	echo "qam_inorder_consume.sh: FAIL"
fi
exit $rc
