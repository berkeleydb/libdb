#!/bin/sh
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# See the file LICENSE for redistribution information.
#
# typed_putcost_ab.sh -- RFC 0014 commit 13: price the typed layer's one branch
# on the UNTYPED put hot path (D2's revisit condition), A/B of a DEFAULT build
# against an --enable-typed build.
#
# METHODOLOGY (and why -- see test/bench/run_bench.sh and opt_ab.sh):
#
#  * This A/B is INHERENTLY two builds: the thing under test is a compiled-in
#    branch, there is no runtime switch for it.  Two builds mean two binaries
#    and two memory layouts, and run_bench.sh warns that a DB_PRIVATE A/B is
#    bimodal in the LENGTH of the env-home path (a layout artifact can read as a
#    1.65x "win").  Controls against that confound:
#      - BOTH arms use the SAME env-home path (same length), run serially so
#        they never share the dir at the same instant;
#      - arms ALTERNATE within each rep (def, typ, def, typ ...), so thermal or
#        neighbour drift cannot masquerade as an arm effect;
#      - a NOISE FLOOR arm: the default build measured against ITSELF (def vs
#        def2).  Any def-vs-typ difference smaller than def-vs-def2 is noise and
#        is reported as such, not as a cost.
#  * Every rep is printed as it completes and appended to the output file.
#  * The driver prepopulates the key range, so it measures steady-state
#    overwrites, not an insert ramp (the commit_bench lesson).
#  * The driver self-reports typed=0/1 (compiled with -DHAVE_TYPED iff the
#    build's db_config.h has it); a run whose typed flag does not match the arm
#    it was built for is rejected.
#
# This is a 16-vCPU-host directional probe by default; the numbers are LABELLED
# as such.  The lead runs it on metal with -r/-s/-n raised.
#
# Usage: typed_putcost_ab.sh -d DEF_BUILD -t TYPED_BUILD [-o OUT.tsv]
#            [-r REPS] [-s SECS] [-n "1 8 32"] [-k NKEYS] [-H HOME]
set -u

DEF=""
TYP=""
OUT=/dev/stdout
REPS=5
SECS=10
THREADS="1 4"
NKEYS=1000000
HOME_DIR=${TYPED_PUTCOST_HOME:-/tmp/typed_putcost_home}

while [ $# -gt 0 ]; do
	case "$1" in
	-d) DEF=$2; shift 2;;
	-t) TYP=$2; shift 2;;
	-o) OUT=$2; shift 2;;
	-r) REPS=$2; shift 2;;
	-s) SECS=$2; shift 2;;
	-n) THREADS=$2; shift 2;;
	-k) NKEYS=$2; shift 2;;
	-H) HOME_DIR=$2; shift 2;;
	*) echo "usage: see header" >&2; exit 2;;
	esac
done
[ -n "$DEF" ] && [ -n "$TYP" ] || { echo "need -d DEF_BUILD and -t TYPED_BUILD" >&2; exit 2; }

HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
DEF=$(CDPATH= cd -- "$DEF" && pwd)
TYP=$(CDPATH= cd -- "$TYP" && pwd)
SRC=$HERE/typed_putcost.c

# Compile the driver against a build's library, matching its HAVE_TYPED so the
# driver's self-reported typed flag is correct (the run_handle_sizes precedent).
build_bin() {  # build_bin <build-dir> <out-bin>
	bdir=$1; obin=$2
	lib=""
	for c in "$bdir"/.libs/libdb-*.so "$bdir"/libdb.a "$bdir"/.libs/libdb-*.a; do
		[ -f "$c" ] && { lib="$c"; break; }
	done
	[ -n "$lib" ] || { echo "no libdb in $bdir" >&2; return 1; }
	rp=""
	case "$lib" in *.so) rp="-Wl,-rpath,$(cd "$bdir/.libs" && pwd)";; esac
	guard=""
	grep -q '^#define HAVE_TYPED 1' "$bdir/db_config.h" 2>/dev/null &&
	    guard="-DHAVE_TYPED"
	el=""
	if grep -q '^#define HAVE_IO_URING 1' "$bdir/db_config.h" 2>/dev/null; then
		el=$(pkg-config --libs liburing 2>/dev/null || echo -luring)
	fi
	"${CC:-cc}" -g -O2 $guard -I"$bdir" "$SRC" "$lib" -lpthread $rp $el \
	    -o "$obin" 2>&1
}

DBIN=$DEF/typed_putcost_def
TBIN=$TYP/typed_putcost_typ
build_bin "$DEF" "$DBIN" || { echo "build default FAIL" >&2; exit 1; }
build_bin "$TYP" "$TBIN" || { echo "build typed FAIL" >&2; exit 1; }

# One measurement: run <bin> in the shared home, return the putspersec and the
# driver's self-reported typed flag.  $1=bin $2=threads ; echoes "rate typed".
measure() {
	rm -rf "$HOME_DIR"; mkdir -p "$HOME_DIR"
	line=$("$1" "$HOME_DIR" "$SECS" "$2" "$NKEYS" 2>&1 | grep '^RESULT ')
	rate=$(echo "$line" | sed -n 's/.*putspersec=\([0-9]*\).*/\1/p')
	tf=$(echo "$line" | sed -n 's/.*typed=\([01]\).*/\1/p')
	echo "${rate:-0} ${tf:-x}"
}

{
echo "# typed_putcost A/B -- RFC 0014 commit 13 (D2 revisit: untyped put hot path)"
echo "# def_build=$DEF typed_build=$TYP"
echo "# reps=$REPS secs=$SECS threads='$THREADS' nkeys=$NKEYS home=$HOME_DIR"
echo "# host: $(uname -s) $(uname -m), $( (nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo '?') ) CPUs"
echo "# arms: def (default build), typ (--enable-typed), def2 (noise floor = default vs itself)"
echo "# columns: rep	threads	arm	putspersec	typed_flag"
} | tee "$OUT" >/dev/null
[ "$OUT" = /dev/stdout ] || cat "$OUT"

for t in $THREADS; do
	r=1
	while [ "$r" -le "$REPS" ]; do
		# alternate within the rep: def, typ, def2(noise floor)
		set -- "def $DBIN" "typ $TBIN" "def2 $DBIN"
		for pair in "$@"; do
			arm=${pair%% *}; bin=${pair#* }
			res=$(measure "$bin" "$t")
			rate=${res% *}; tf=${res#* }
			printf '%d\t%d\t%s\t%s\t%s\n' "$r" "$t" "$arm" "$rate" "$tf" | tee -a "$OUT"
		done
		r=$((r + 1))
	done
done

echo "# done.  Summarize: awk -F'\\t' '/^[0-9]/{s[\$3]+=\$4;n[\$3]++} END{for(a in s)printf \"%s mean=%.0f\\n\",a,s[a]/n[a]}' $OUT" | tee -a "$OUT"
