#!/bin/sh
# Copyright (c) 2026 libdb contributors.  All rights reserved.
# SPDX-License-Identifier: Sleepycat
#
# run_dtrace_probes.sh -- the `bdb' USDT provider is really in libdb, and fires.
#
# On an --enable-dtrace build (HAVE_DTRACE in db_config.h):
#   Linux (SystemTap SDT): every probe in dist/db_provider.d that has a call
#     site must appear as a `Provider: bdb' note in libdb's .note.stapsdt, and
#     if bpftrace can run (root or sudo -n) a txn-commit probe must fire.
#   native DTrace (FreeBSD, illumos, macOS): `dtrace -l' against a running
#     driver must list the probes, and a txn-commit probe must fire.
# A missing probe or a probe that never fires is a FAIL.
#
# SKIP, with the reason, when: the build has no HAVE_DTRACE; the host has no
# tracer for the firing step (bpftrace / dtrace); or tracing is not permitted
# (no root, no passwordless sudo).  The note check on Linux needs no privilege,
# so a DTrace build there never skips it.
#
# Usage (from build_unix): sh ../test/db/run_dtrace_probes.sh

set -u

HERE=$(cd "$(dirname "$0")" && pwd)
SRC=$(cd "$HERE/../.." && pwd)
BUILD=${1:-${BUILD:-.}}
BUILD=$(cd "$BUILD" && pwd)
N=run_dtrace_probes.sh

grep -q '^#define HAVE_DTRACE 1' "$BUILD/db_config.h" 2>/dev/null || {
	echo "$N: SKIP not an --enable-dtrace build (no HAVE_DTRACE in $BUILD/db_config.h)"
	exit 0
}

SO=""
for c in "$BUILD"/.libs/libdb-*.so "$BUILD"/.libs/libdb-*.dylib ; do
	[ -f "$c" ] && [ ! -h "$c" ] && { SO=$c ; break ; }
done
[ -n "$SO" ] || { echo "$N: FAIL no shared libdb under $BUILD/.libs" ; exit 1 ; }

# The probes that have call sites.  STAT_PERFMON (--enable-perfmon-statistics)
# probes and the few declared-but-unused ones are not expected in the library.
# PERFMONn(env, cat, id, ...) -> cat-id.  The two error/warning macros pass a
# symbolic errcode, so they are skipped here.
EXPECT="$BUILD/dtrace_expect.txt"
grep -rhoE '(^|[^_A-Z])PERFMON[0-9]\([^,]+,[[:space:]]*[a-z]+,[[:space:]]*[a-z_]+' \
    "$SRC/src" --include='*.c' |
    sed -E 's/.*,[[:space:]]*([a-z]+),[[:space:]]*([a-z_]+)$/\1-\2/' |
    grep -v '^cat-id$' | sort -u > "$EXPECT"
nexp=$(wc -l < "$EXPECT" | tr -d ' ')
[ "$nexp" -gt 20 ] || { echo "$N: FAIL found only $nexp probe call sites in src -- the scan is broken" ; exit 1 ; }

# Privilege for the firing step: root, or passwordless sudo.
if [ "$(id -u)" -eq 0 ] ; then
	SUDO=""
elif sudo -n true 2>/dev/null ; then
	SUDO="sudo -n"
else
	SUDO=none
fi

TO=""
command -v timeout >/dev/null 2>&1 && TO="timeout 120"

DRV="$BUILD/dtrace_drv"
rm -f "$DRV"
"${CC:-cc}" -g -O1 -I"$BUILD" "$HERE/dtrace_drv.c" "$SO" -lpthread \
    -Wl,-rpath,"$(dirname "$SO")" -o "$DRV" || { echo "$N: FAIL compile" ; exit 1 ; }
H="$BUILD/DTRACE_PROBES"
mkdir -p "$H" && find "$H" -mindepth 1 -delete

# Which kind of build this is comes from the build, not from what the library
# happens to contain: an SDT build with no notes must FAIL, not fall through to
# the native branch and SKIP for want of a DTrace kernel.
rc=0
if grep -q 'Systemtap' "$BUILD/db_provider.h" 2>/dev/null ; then
	# --- Linux SDT ------------------------------------------------------
	GOT="$BUILD/dtrace_got.txt"
	readelf -n "$SO" | awk '/Provider: bdb/ { getline; sub(/.*Name: /, ""); print }' |
	    sed 's/__/-/' | sort -u > "$GOT"
	nnote=$(readelf -n "$SO" | grep -c 'Provider: bdb')
	echo "  stapsdt notes for provider bdb: $nnote ($(wc -l < "$GOT" | tr -d ' ') distinct probes, $nexp expected)"
	missing=$(comm -23 "$EXPECT" "$GOT")
	if [ -n "$missing" ] ; then
		echo "  MISSING from $SO:" ; echo "$missing" | sed 's/^/    /'
		rc=1
	fi
	if ! command -v bpftrace >/dev/null 2>&1 ; then
		echo "  firing check: SKIP no bpftrace on this host"
	elif [ "$SUDO" = none ] ; then
		echo "  firing check: SKIP bpftrace needs root and sudo -n is not available"
	else
		# Attach to the library path, not with -c: bpftrace (0.17) cannot
		# resolve a probe in a library the -c child has not loaded yet.
		BT="$BUILD/dtrace_bt.out"
		$TO $SUDO bpftrace -q -e "usdt:$SO:bdb:txn__commit { @n = count(); }
		    interval:s:8 { exit(); }" > "$BT" 2>&1 &
		sleep 3
		"$DRV" "$H" 2 50 > /dev/null || rc=1
		wait
		n=$(sed -n 's/^@n: *//p' "$BT")
		echo "  bpftrace usdt:bdb:txn__commit fired ${n:-0} times (100 commits run)"
		[ "${n:-0}" -ge 100 ] || { tail -5 "$BT" | sed 's/^/    /' ; rc=1 ; }
	fi
else
	# --- native DTrace --------------------------------------------------
	if ! command -v dtrace >/dev/null 2>&1 ; then
		echo "$N: SKIP native DTrace build, but this host has no dtrace(1)"
		exit 1
	fi
	[ "$SUDO" = none ] && {
		echo "$N: SKIP dtrace needs root and sudo -n is not available"
		exit 0
	}
	# FreeBSD loads the DTrace modules on demand only for root's first use.
	[ "$(uname -s)" = FreeBSD ] && { $SUDO kldstat -q -m dtrace || $SUDO kldload dtraceall ; } 2>/dev/null
	$SUDO dtrace -l -n 'dtrace:::BEGIN' > /dev/null 2>&1 || {
		echo "$N: SKIP dtrace(1) cannot run here (no DTrace kernel support, or not permitted)"
		exit 0
	}
	GOT="$BUILD/dtrace_got.txt"
	$TO $SUDO dtrace -l -n 'bdb$target:::' -c "$DRV $H 1 1" 2>/dev/null |
	    awk 'NR > 1 { print $NF }' | sort -u > "$GOT"
	echo "  dtrace -l: $(wc -l < "$GOT" | tr -d ' ') distinct bdb probes listed, $nexp expected"
	missing=$(comm -23 "$EXPECT" "$GOT")
	if [ -n "$missing" ] ; then
		echo "  MISSING:" ; echo "$missing" | sed 's/^/    /'
		rc=1
	fi
	find "$H" -mindepth 1 -delete
	n=$($TO $SUDO dtrace -q -n 'bdb$target:::txn-commit { n++; } END { printf("fired=%d\n", n); }' \
	    -c "$DRV $H 2 50" 2>&1 | sed -n 's/^fired=//p')
	echo "  dtrace bdb\$target:::txn-commit fired ${n:-0} times (100 commits run)"
	[ "${n:-0}" -ge 100 ] || rc=1
fi

if [ "$rc" -eq 0 ] ; then echo "$N: PASS" ; else echo "$N: FAIL" ; fi
exit $rc
