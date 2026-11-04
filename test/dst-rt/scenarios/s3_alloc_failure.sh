#!/bin/sh -
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# test/dst-rt/scenarios/s3_alloc_failure.sh <seed> -- RFC 0024 S3.
#
# Allocation failure across the WHOLE process (one node), extending
# test/faultinject past libdb's own __os_malloc seam (§2.3).  Three arms:
#
#   s3-faint        FAINT profiles a bounded rtload run, then injects ONE
#                   allocation failure per call site.  Any crash is a finding.
#   s3-fiu-malloc   libfiu fails libc/mm/malloc|calloc|realloc at probability p
#                   anywhere in the process (incl. libc), over the workload.
#                   The env must stay recoverable and acked data must survive.
#   s3-umem         libumem UMEM_DEBUG=default (guard redzones + UAF detection)
#                   over the workload; any redzone/UAF report fails the arm.
#
# SABOTAGE arm (s3-umem-sabotage): a tiny program with a deliberate heap
# buffer overflow run under libumem -- umem MUST abort (teeth: the tool detects
# the planted corruption).
#
# Usage:  sh scenarios/s3_alloc_failure.sh <seed>

set -u
HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
DSTRT_DIR=${DSTRT_DIR:-$(CDPATH= cd -- "$HERE/.." && pwd)}
. "$DSTRT_DIR/lib/fi_env.sh"
. "$DSTRT_DIR/lib/fi_fiu.sh"
. "$DSTRT_DIR/../harness.sh"

SEED=${1:-1}
echo "# reproduce: BUILD=$BUILD DSTRT_DIR=$DSTRT_DIR sh $0 $SEED"
# When driven by run.sh (DSTRT_KEEP_RESULTS=1) do NOT truncate the shared
# results file -- run.sh did the single hi_init so every scenario appends.
if [ "${DSTRT_KEEP_RESULTS:-0}" = 1 ]; then
	hi_tier="dst-rt"
	hi_file="${LIBDB_RESULTS_DIR:-$DSTRT_DIR/../.results}/dst-rt.results"
else
	hi_init "dst-rt" "$DSTRT_DIR/.."
fi

fi_have libdb || { echo "SKIP: no libdb under $BUILD"; exit 77; }
[ -x "$BUILD/rtload" ] || fi_build_node rtload || { echo "SKIP: rtload build"; exit 77; }
[ -x "$BUILD/rtcheck" ] || fi_build_node rtcheck || { echo "SKIP: rtcheck build"; exit 77; }

RUN=$(fi_mkrun s3)
trap 'rm -rf "$RUN" 2>/dev/null' 0
rc=0

# ---- s3-faint: systematic one-fault-per-call-site sweep -------------------
echo "== s3-faint =="
if fi_have faint && command -v setarch >/dev/null 2>&1; then
	_home="$RUN/faint/home"; mkdir -p "$_home"
	# FAINT disables ASLR (needs setarch -R) and profiles, then injects one
	# fault per call site.  Bounded workload so the sweep stays short.
	# Bounded: the sweep takes < 1 s on the dev box.  A hang (seen on the
	# Ubuntu 26.04 runner, 41 min until the job was cancelled) must be a
	# visible verdict, not a silently cancelled job.  -k: SIGKILL if the
	# SIGTERM is ignored by a child FAINT forked.
	out=$(timeout -k 10 "${S3_FAINT_TIMEOUT:-120}" setarch -R "$FAINT_BIN" \
	    "$BUILD/rtload" --home "$_home" \
	    --ack "$RUN/faint.ack" --node f --seed "$SEED" \
	    --ops 20 --keys 10 --valsz 100 2>&1)
	_frc=$?
	echo "$out" | grep -iE "injection position|Crashed at|Unique crashes" | sed 's/^/    /'
	crashes=$(echo "$out" | sed -n 's/.*Crashed at \([0-9]*\) from.*/\1/p')
	positions=$(echo "$out" | sed -n 's/.*Found \([0-9]*\) different.*/\1/p')
	if [ "$_frc" = 124 ] || [ "$_frc" = 137 ]; then
		echo "    FAIL: FAINT did not finish in ${S3_FAINT_TIMEOUT:-120}s (hung)"
		echo "$out" | tail -8 | sed 's/^/      | /'
		hi_emit s3-faint fail; rc=1
	elif [ "${positions:-0}" = 0 ]; then
		echo "    SKIP: FAINT found no injection positions"
		hi_emit s3-faint skip
	elif [ "${crashes:-0}" = 0 ]; then
		echo "    no crash across $positions injection positions"
		hi_emit s3-faint pass
	else
		echo "    FINDING: $crashes crash(es) from a single injected malloc failure"
		hi_emit s3-faint fail; rc=1
	fi
else
	echo "    SKIP: FAINT or setarch unavailable"
	hi_emit s3-faint skip
fi

# ---- s3-fiu-malloc: random malloc failure over the workload ---------------
echo "== s3-fiu-malloc =="
if fi_have fiu; then
	_home="$RUN/fiu/home"; mkdir -p "$_home"
	# Pre-create the env clean so the fault lands on the workload.
	"$BUILD/rtload" --home "$_home" --ack "$RUN/fiu.pre" --node m \
	    --seed "$SEED" --ops 5 --keys 100 --valsz 150 >/dev/null 2>&1
	cmd=$(fi_fiu_cmd mem-fiu-random "$SEED" 0.0005)
	# shellcheck disable=SC2086
	eval "$cmd" "$BUILD/rtload" --home "$_home" --ack "$RUN/fiu.ack" \
	    --node m --seed "$SEED" --ops 800 --keys 100 --valsz 150 \
	    >/dev/null 2>&1
	echo "    loader rc=$? (non-zero ok: a malloc fault surfaced as an error)"
	out=$("$BUILD/rtcheck" --home "$_home" --ack "$RUN/fiu.ack" 2>&1)
	echo "$out" | sed 's/^/    /'		# prints acked/failed/unknown counts
	if echo "$out" | grep -q '^PASS'; then
		hi_emit s3-fiu-malloc pass
	elif echo "$out" | grep -q '^VACUOUS'; then
		echo "    VACUOUS: no acked op reached the oracle; arm failed"
		hi_emit s3-fiu-malloc fail; rc=1
	else
		echo "    FINDING: a malloc failure corrupted/lost acked data or wedged the env"
		hi_emit s3-fiu-malloc fail; rc=1
	fi
else
	echo "    SKIP: libfiu unavailable"; hi_emit s3-fiu-malloc skip
fi

# ---- s3-umem: guard redzones + contents (fill-pattern) over the workload --
echo "== s3-umem =="
if fi_have umem; then
	# UMEM_DEBUG mode: the brief asks for `default` (guards + audit).  On some
	# hosts the `audit` component (per-buffer stack capture) aborts even a
	# trivial program at startup, with no detection report -- a libumem/host
	# limitation, NOT a libdb heap bug (a 1-malloc program aborts too).  Probe
	# `default` with a trivial program; if it aborts spuriously, fall back to
	# `guards,contents`, which is the DETECTION half of default (redzone +
	# fill-pattern): it still catches an overflow and a use-after-free (the
	# sabotage arm proves this), just without the audit backtraces.
	# UMEM_DEBUG mode: guards,contents, the detection half of `default`
	# (redzones + fill pattern).  It catches the planted overflow (the
	# sabotage arm below) and runs the libdb workload clean.  `default` adds
	# the audit component, which is not usable here: on the dev box it aborts
	# a one-malloc program (F1-002), and on the GitHub Ubuntu 26.04 runner it
	# HANGS intermittently (F1-003) -- a probe passed and the workload then
	# hung, so no probe can vouch for it.  S3_UMEM_MODE overrides.
	UMODE=${S3_UMEM_MODE:-guards,contents}

	_home="$RUN/umem/home"; mkdir -p "$_home"
	# Every libumem run is bounded: on the Ubuntu 26.04 runner this arm hung
	# until the 45-minute job limit cancelled it, leaving no verdict.
	timeout -k 10 "${S3_UMEM_TIMEOUT:-180}" env LD_PRELOAD="$UMEM_SO" \
	    UMEM_DEBUG="$UMODE" UMEM_OPTIONS=abort \
	    "$BUILD/rtload" --home "$_home" --ack "$RUN/umem.ack" --node u \
	    --seed "$SEED" --ops 800 --keys 100 --valsz 200 >/dev/null 2>&1
	urc=$?
	if [ "$urc" = 124 ] || [ "$urc" = 137 ]; then
		echo "    FAIL: rtload under libumem (UMEM_DEBUG=$UMODE) did not finish"
		echo "          in ${S3_UMEM_TIMEOUT:-180}s (hung)"
		hi_emit s3-umem fail; rc=1
	elif [ "$urc" = 0 ]; then
		out2=$(timeout -k 10 "${S3_UMEM_TIMEOUT:-180}" env \
		    LD_PRELOAD="$UMEM_SO" UMEM_DEBUG="$UMODE" "$BUILD/rtcheck" \
		    --home "$_home" --ack "$RUN/umem.ack" 2>&1)
		echo "$out2" | sed 's/^/    /'		# prints acked/failed/unknown counts
		if echo "$out2" | grep -q '^PASS'; then
			echo "    clean (UMEM_DEBUG=$UMODE): no heap error, data verified"
			hi_emit s3-umem pass
		else
			# includes VACUOUS: a umem run that checked 0 acked ops
			hi_emit s3-umem fail; rc=1
		fi
	else
		echo "    FINDING: libumem (UMEM_DEBUG=$UMODE) aborted over the libdb workload"
		echo "             (rc=$urc) -- investigate before trusting PASS"
		hi_emit s3-umem fail; rc=1
	fi
else
	echo "    SKIP: libumem unavailable"; hi_emit s3-umem skip
fi

# ---- SABOTAGE: a planted heap overflow umem must catch --------------------
echo "== s3-umem-sabotage =="
if fi_have umem; then
	cat > "$RUN/sab_oob.c" <<'CEOF'
#include <stdlib.h>
#include <string.h>
int main(void){
	char *p = malloc(16);
	/* deliberate overflow past the 16-byte allocation into the redzone */
	memset(p, 0x41, 64);
	free(p);
	return 0;
}
CEOF
	"${CC:-cc}" -O0 "$RUN/sab_oob.c" -o "$RUN/sab_oob" 2>/dev/null
	# Use the SAME mode the s3-umem arm settled on, so the teeth match the
	# detection actually in force.
	UMODE=${UMODE:-guards,contents}
	sout=$(timeout -k 5 30 env LD_PRELOAD="$UMEM_SO" UMEM_DEBUG="$UMODE" \
	    UMEM_OPTIONS=abort "$RUN/sab_oob" 2>&1)
	src2=$?
	if [ "$src2" = 124 ] || [ "$src2" = 137 ]; then
		echo "    FAIL: the sabotage program hung under libumem"
		hi_emit s3-umem-sabotage fail; rc=1
	elif [ "$src2" != 0 ] || echo "$sout" | grep -qiE "redzone|modified|buffer|corrupt"; then
		echo "    umem (UMEM_DEBUG=$UMODE) caught the planted heap overflow (teeth)"
		hi_emit s3-umem-sabotage pass
	else
		echo "    SABOTAGE NOT CAUGHT: umem missed a 64-into-16 overflow"
		hi_emit s3-umem-sabotage fail; rc=1
	fi
else
	echo "    SKIP: libumem unavailable"; hi_emit s3-umem-sabotage skip
fi

exit $rc
