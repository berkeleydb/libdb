#!/bin/sh
# RFC 0019 (review D-3): the env password must never be stored in the shared
# region.  This runs two arms:
#
#   CORRECT  -- the library as built: the password must appear in NO file under
#               the env home, a wrong password is rejected (EPERM), the right
#               one is accepted.  Expect PASS.
#   SABOTAGE -- crypto.o recompiled with -DCRYPTO_REGION_SECRET_SABOTAGE (the
#               region holds the secret again) linked ahead of libdb.a.  The
#               leak scan MUST then find the password.  Expect the test to FAIL,
#               which is the teeth: a scan that cannot detect the planted secret
#               is worthless.
#
# Library discovery follows test/db/run_handle_sizes.sh.
set -u
HERE=$(cd "$(dirname "$0")" && pwd)
BUILD=${1:-${BUILD:-"$HERE/../../build_unix"}}
[ -d "$BUILD" ] && BUILD=$(cd "$BUILD" && pwd)
SRC=$HERE/crypto_secret_leak.c
CRYPTO_SRC=$(cd "$HERE/../.." && pwd)/src/crypto/crypto.c

# Must be a crypto build, else the test is vacuous.
if ! grep -q '^#define HAVE_CRYPTO 1' "$BUILD/db_config.h" 2>/dev/null ; then
	echo "run_crypto_secret_leak.sh: SKIP not-a-crypto-build"
	exit 0
fi

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
[ -n "$LIB" ] || { echo "run_crypto_secret_leak.sh: FAIL no libdb under $BUILD" ; exit 1 ; }

EXTRALIBS=""
for l in $(pkg-config --libs liburing 2>/dev/null) ; do EXTRALIBS="$EXTRALIBS $l" ; done
CC=${CC:-cc}

# ---- CORRECT arm --------------------------------------------------------
BIN=$BUILD/crypto_secret_leak
rm -f "$BIN"
"$CC" -g -O1 ${CFLAGS:-} -I"$BUILD" "$SRC" "$LIB" \
    -lpthread $LIBRPATH $EXTRALIBS -o "$BIN" 2>&1 || {
	echo "run_crypto_secret_leak.sh: FAIL compile correct arm" ; exit 1 ; }
out=$("$BIN" 2>&1)
echo "$out" | sed 's/^/  [correct] /'
v=$(echo "$out" | grep '^VERDICT crypto_secret_leak' | head -1)
case "$v" in
*PASS*)	: ;;
*)	echo "run_crypto_secret_leak.sh: FAIL correct arm did not PASS" ; exit 1 ;;
esac

# ---- SABOTAGE arm -------------------------------------------------------
# Recompile crypto.o with the secret put back into the region and link it
# AHEAD of libdb.a so its __crypto_region_init wins over the archive copy.
SAB_O=$BUILD/crypto_sabotage.o
SAB_BIN=$BUILD/crypto_secret_leak_sab
rm -f "$SAB_O" "$SAB_BIN"
# Reuse the build's own include path and HAVE_* macros via db_config.h.
"$CC" -g -O1 ${CFLAGS:-} -DCRYPTO_REGION_SECRET_SABOTAGE \
    -I"$BUILD" -I"$(cd "$HERE/../.." && pwd)/src" \
    -I"$(cd "$HERE/../.." && pwd)" \
    -c "$CRYPTO_SRC" -o "$SAB_O" 2>sab_cc.log || {
	echo "  [sabotage] could not recompile crypto.o:" ; sed 's/^/    /' sab_cc.log
	echo "run_crypto_secret_leak.sh: SKIP sabotage-build" ; rm -f sab_cc.log ; exit 0 ; }
rm -f sab_cc.log
"$CC" -g -O1 ${CFLAGS:-} -I"$BUILD" "$SRC" "$SAB_O" "$LIB" \
    -lpthread $LIBRPATH $EXTRALIBS -o "$SAB_BIN" 2>&1 || {
	echo "run_crypto_secret_leak.sh: SKIP sabotage-link" ; exit 0 ; }
sout=$("$SAB_BIN" 2>&1)
echo "$sout" | sed 's/^/  [sabotage] /'
sv=$(echo "$sout" | grep '^VERDICT crypto_secret_leak' | head -1)
case "$sv" in
*FAIL*)	echo "  [sabotage] correctly FAILED (planted secret detected) -- teeth OK" ;;
*)	echo "run_crypto_secret_leak.sh: FAIL sabotage arm did NOT detect the leak" ; exit 1 ;;
esac

echo "run_crypto_secret_leak.sh: PASS"
exit 0
