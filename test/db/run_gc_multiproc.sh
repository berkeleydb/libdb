#!/bin/sh
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# See the file LICENSE for redistribution information.
#
# run_gc_multiproc.sh -- adversarial MULTI-PROCESS group-commit durability
# (W7 req 6).  Several real processes share ONE environment and commit
# DB_TXN_SYNC; the whole process group is SIGKILLed mid-commit (a real power
# cut, with followers parked on another process's fsync); after recovery every
# acknowledged commit, recorded in a MAP_SHARED oracle AFTER commit() returned
# 0, must be present.  Uses a REAL kill and REAL fsync durability -- the
# hardware confirmation of the DST write-back model's verdict.
#
# Exits non-zero on a lost acked commit, a hang, or a signal death in verify.

set -u
HERE=$(cd "$(dirname "$0")" && pwd)
BUILD=${1:-${BUILD:-.}}
BUILD=$(cd "$BUILD" && pwd)
NAME=gc_multiproc
SRC=$HERE/$NAME.c
NPROC=${NPROC:-6}
RUNSECS=${RUNSECS:-3}

# Library discovery follows test/db/run_typed_unique_mp.sh.
LIB=""
LIBRPATH=""
for cand in "$BUILD"/libdb.a "$BUILD"/.libs/libdb-*.a ; do
	[ -f "$cand" ] && { LIB="$cand" ; break ; }
done
if [ -z "$LIB" ] ; then
	for cand in "$BUILD"/.libs/libdb-*.so "$BUILD"/.libs/libdb-*.dylib ; do
		[ -f "$cand" ] && { LIB="$cand" ; break ; }
	done
	[ -n "$LIB" ] && LIBRPATH="-Wl,-rpath,$(cd "$BUILD/.libs" && pwd)"
fi
[ -n "$LIB" ] || { echo "VERDICT $NAME FAIL no libdb library under $BUILD" ; exit 1 ; }

EXTRALIBS=""
for l in $(pkg-config --libs liburing 2>/dev/null) ; do EXTRALIBS="$EXTRALIBS $l" ; done

BIN=$BUILD/$NAME
rm -f "$BIN"
"${CC:-cc}" -g -O1 ${CFLAGS:-} -I"$BUILD" "$SRC" "$LIB" \
    -lpthread $LIBRPATH $EXTRALIBS -o "$BIN" 2>&1 || {
	echo "VERDICT $NAME FAIL compile" ; exit 1 ; }
test -x "$BIN" || { echo "VERDICT $NAME FAIL no binary produced" ; exit 1 ; }

work=$BUILD/TESTDIR_$NAME
[ -d "$work" ] && find "$work" -mindepth 1 -delete 2>/dev/null
mkdir -p "$work"

# Start the committer group in its OWN session/process group so one signal
# kills the setup process AND every child it forked, all at once -- a real
# simultaneous power cut, no orderly shutdown.
setsid "$BIN" "$work" setup "$NPROC" >"$work/setup.out" 2>&1 &
grp=$!

# Let the processes commit for a while so group commit batches across
# processes (followers park on another process's fsync).
sleep "$RUNSECS"

# SIGKILL the whole group (negative pid = process group).  -9 so there is no
# chance of a clean close or a final flush.
kill -9 -"$grp" 2>/dev/null
# Reap so the group is gone before recovery touches the env.
wait "$grp" 2>/dev/null
# A belt-and-braces sweep in case any grandchild outlived the group kill.
pkill -9 -f "$BIN $work child" 2>/dev/null
sleep 1

acks=$(od -An -tu4 -N4 "$work/acked.map" 2>/dev/null | tr -d ' ')
echo "  acked commits recorded before the kill: ${acks:-0}"
if [ -z "${acks:-}" ] || [ "${acks:-0}" -eq 0 ] ; then
	echo "VERDICT $NAME FAIL no commits acked before the kill (raise RUNSECS)"
	exit 1
fi

TO=""
command -v timeout >/dev/null 2>&1 && TO="timeout 180"
out=$($TO "$BIN" "$work" verify "$NPROC" 2>&1)
rc=$?
echo "$out" | sed 's/^/  /'
[ "$rc" -eq 124 ] && { echo "VERDICT $NAME FAIL recovery/verify hung" ; exit 1 ; }
[ "$rc" -ge 128 ] && { echo "VERDICT $NAME FAIL died on signal $((rc - 128))" ; exit 1 ; }

case "$out" in
*"gc_multiproc: PASS"*) [ "$rc" -eq 0 ] && { echo "VERDICT $NAME PASS" ; exit 0 ; } ;;
esac
echo "VERDICT $NAME FAIL (rc=$rc)"
exit 1
