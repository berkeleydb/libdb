#!/bin/sh -
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# run_repmgr_tls_wirecap.sh --
#	The core FAIL-before / PASS-after test for RFC 0018 (repmgr TLS).
#	Runs a two-site repmgr group that replicates a unique marker value,
#	capturing every byte sent to a socket with an LD_PRELOAD shim.
#
#	  - PLAINTEXT run: the marker MUST appear in the capture (proves the
#	    cleartext leak the review found).  On a library WITHOUT TLS, or
#	    with TLS off, this is the only run; it is expected to show the
#	    marker.
#	  - TLS run (only when the library has HAVE_REPMGR_SSL): the marker
#	    MUST NOT appear in the capture (proves TLS fixes it), and the
#	    client must still observe the replicated marker (proves TLS does
#	    not break replication).
#	  - TLS_REQUIRE run: a plaintext client must be refused (the client
#	    cannot replicate the marker).
#
#	Usage (from the build dir):  sh ../test/db/run_repmgr_tls_wirecap.sh
#	Env: BUILD (default .), TLS=1 to force the TLS arms (requires the lib
#	     to export __repmgr_ssl_* -- i.e. an --enable-tls build).

set -e

BUILD=${BUILD:-.}
_HERE=$(cd "$(dirname "$0")" && pwd)
SRC=${SRC:-$_HERE/repmgr_tls.c}
SHIM=${SHIM:-$_HERE/wirecap.c}
WORK=${WORK:-REPMGR_TLS_TESTDIR}
MPORT=${MPORT:-6210}
CPORT=${CPORT:-6211}

# Pick the library the other db-tier suites use.
LIB=""
LIBRPATH=""
for cand in "$BUILD"/.libs/libdb-*.so "$BUILD"/libdb.a "$BUILD"/.libs/libdb-*.a; do
	if [ -f "$cand" ]; then LIB="$cand"; break; fi
done
[ -n "$LIB" ] || { echo "FAIL: no libdb library found under $BUILD"; exit 1; }
case "$LIB" in
*.so) LIBRPATH="-Wl,-rpath,$(cd "$BUILD/.libs" && pwd)";;
esac

# Does this library have TLS compiled in?
HAS_TLS=0
if nm -D "$BUILD"/.libs/libdb-*.so 2>/dev/null | grep -q __repmgr_ssl_init; then
	HAS_TLS=1
fi

EXTRALIBS=""
for l in $(pkg-config --libs liburing 2>/dev/null); do EXTRALIBS="$EXTRALIBS $l"; done

echo "Compiling repmgr_tls against $LIB (HAS_TLS=$HAS_TLS)"
"${CC:-cc}" -g -O1 ${CFLAGS:-} -I"$BUILD" "$SRC" "$LIB" \
    -lpthread $LIBRPATH $EXTRALIBS -o "$BUILD/repmgr_tls_test"
"${CC:-cc}" -g -O1 -fPIC -shared "$SHIM" -ldl -o "$BUILD/libwirecap.so"

[ -d "$WORK" ] && find "$WORK" -mindepth 1 -delete 2>/dev/null
mkdir -p "$WORK/m" "$WORK/c" "$WORK/certs"

# Generate a self-signed CA and one leaf cert the two sites share (localhost
# loopback; we only need to prove encryption vs cleartext here).
CERTS="$WORK/certs"
openssl req -x509 -newkey rsa:2048 -nodes -days 2 \
    -keyout "$CERTS/ca.key" -out "$CERTS/ca.crt" \
    -subj "/CN=libdb-test-ca" >/dev/null 2>&1
openssl req -newkey rsa:2048 -nodes \
    -keyout "$CERTS/site.key" -out "$CERTS/site.csr" \
    -subj "/CN=127.0.0.1" >/dev/null 2>&1
openssl x509 -req -in "$CERTS/site.csr" -CA "$CERTS/ca.crt" \
    -CAkey "$CERTS/ca.key" -CAcreateserial -days 2 \
    -out "$CERTS/site.crt" >/dev/null 2>&1
# A cert signed by a DIFFERENT CA, to test rejection.
openssl req -x509 -newkey rsa:2048 -nodes -days 2 \
    -keyout "$CERTS/rogue.key" -out "$CERTS/rogue.crt" \
    -subj "/CN=rogue" >/dev/null 2>&1

# A second leaf from the SAME CA: valid by chain, but not pinned (pinning arms).
openssl req -newkey rsa:2048 -nodes \
    -keyout "$CERTS/other.key" -out "$CERTS/other.csr" \
    -subj "/CN=127.0.0.1" >/dev/null 2>&1
openssl x509 -req -in "$CERTS/other.csr" -CA "$CERTS/ca.crt" \
    -CAkey "$CERTS/ca.key" -CAcreateserial -days 2 \
    -out "$CERTS/other.crt" >/dev/null 2>&1
SITE_PIN=$(openssl x509 -in "$CERTS/site.crt" -noout -fingerprint -sha256 |
    sed 's/.*=//')

run_group() {
	# $1 = capture file, $2 = master port, $3 = client port.
	# $PIN_M / $PIN_C, if set, become a repmgr_tls_pin line in that home.
	rm -f "$1"
	: > "$1"
	find "$WORK/m" -mindepth 1 -delete 2>/dev/null || true
	find "$WORK/c" -mindepth 1 -delete 2>/dev/null || true
	[ -n "$PIN_M" ] && echo "repmgr_tls_pin $PIN_M" > "$WORK/m/DB_CONFIG"
	[ -n "$PIN_C" ] && echo "repmgr_tls_pin $PIN_C" > "$WORK/c/DB_CONFIG"
	LD_PRELOAD="$BUILD/libwirecap.so" WIRECAP_FILE="$1" \
	    "$BUILD/repmgr_tls_test" "$WORK/m" "$WORK/c" "$2" "$3"
}

MARKER="REPMGR_TLS_MARKER_abcdef0123456789_VALUE"
rc=0

# ---- Arm 1: PLAINTEXT -- the marker MUST be on the wire. ----
echo "=== Arm 1: plaintext (expect marker IN capture) ==="
unset REP_TLS_CERT REP_TLS_KEY REP_TLS_CA REP_TLS_REQUIRE REP_TLS_NO_VERIFY
if run_group "$WORK/cap_plain.bin" "$MPORT" "$CPORT"; then
	cstat=0
else
	cstat=$?
fi
if grep -qa "$MARKER" "$WORK/cap_plain.bin"; then
	echo "  PASS: marker found in plaintext capture (leak reproduced)"
else
	echo "  FAIL: marker NOT in plaintext capture -- test cannot prove anything"
	rc=1
fi
[ "$cstat" = 0 ] || { echo "  FAIL: plaintext replication did not converge"; rc=1; }

if [ "$HAS_TLS" = 0 ]; then
	echo "Library built WITHOUT TLS; skipping TLS arms."
	if [ "$rc" = 0 ]; then echo "run_repmgr_tls_wirecap.sh: PASS (plaintext only)"; else echo "run_repmgr_tls_wirecap.sh: FAIL"; fi
	exit $rc
fi

export REP_TLS_CERT="$CERTS/site.crt"
export REP_TLS_KEY="$CERTS/site.key"
export REP_TLS_CA="$CERTS/ca.crt"

# ---- Arm 2: TLS -- the marker MUST NOT be on the wire, and must replicate. ----
echo "=== Arm 2: TLS (expect marker ABSENT from capture, replication OK) ==="
unset REP_TLS_REQUIRE REP_TLS_NO_VERIFY
if run_group "$WORK/cap_tls.bin" $((MPORT+2)) $((CPORT+2)); then
	cstat=0
else
	cstat=$?
fi
if grep -qa "$MARKER" "$WORK/cap_tls.bin"; then
	echo "  FAIL: marker FOUND in TLS capture -- traffic not encrypted"
	rc=1
else
	echo "  PASS: marker absent from TLS capture (encrypted)"
fi
if [ "$cstat" = 0 ]; then
	echo "  PASS: replication converged over TLS"
else
	echo "  FAIL: replication did not converge over TLS"
	rc=1
fi

# ---- Arm 3: wrong CA -- handshake must fail, no replication. ----
echo "=== Arm 3: wrong CA (expect handshake failure, no replication) ==="
REP_TLS_CA="$CERTS/rogue.crt"
if run_group "$WORK/cap_badca.bin" $((MPORT+4)) $((CPORT+4)); then
	echo "  FAIL: replication converged with a wrong CA"
	rc=1
else
	echo "  PASS: replication refused with a wrong CA"
fi
REP_TLS_CA="$CERTS/ca.crt"

# ---- Arm 4: pinning, right pin -- both sites pin site.crt, must replicate. ----
echo "=== Arm 4: pinned leaf (expect replication OK) ==="
PIN_M=$SITE_PIN PIN_C=$SITE_PIN
if run_group "$WORK/cap_pin.bin" $((MPORT+6)) $((CPORT+6)); then
	echo "  PASS: replication converged with the peer's leaf pinned"
else
	echo "  FAIL: replication refused although the peer's leaf is pinned"
	rc=1
fi

# ---- Arm 5: pinning, unpinned leaf -- the client presents other.crt (valid
# under the CA, not pinned by the master).  Must be refused EVERY time: the
# verify callback once read a process-global set per handshake, and concurrent
# handshakes made it see NULL and skip the pin check.  Repeat to catch a race.
echo "=== Arm 5: CA-valid but unpinned leaf (expect refusal, x${PIN_REPS:-5}) ==="
i=0
while [ $i -lt "${PIN_REPS:-5}" ]; do
	PIN_M=$SITE_PIN PIN_C=
	export REP_TLS_CERT_CLIENT="$CERTS/other.crt" REP_TLS_KEY_CLIENT="$CERTS/other.key"
	if run_group "$WORK/cap_unpin.bin" $((MPORT+8+2*i)) $((CPORT+8+2*i)); then
		echo "  FAIL: run $i: an unpinned leaf was accepted"
		rc=1
	fi
	i=$((i+1))
done
unset REP_TLS_CERT_CLIENT REP_TLS_KEY_CLIENT PIN_M PIN_C
[ "$rc" = 0 ] && echo "  PASS: unpinned leaf refused in all runs"

echo
if [ "$rc" = 0 ]; then
	echo "run_repmgr_tls_wirecap.sh: PASS"
else
	echo "run_repmgr_tls_wirecap.sh: FAIL"
fi
exit $rc
