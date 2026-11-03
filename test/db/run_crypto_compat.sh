#!/bin/sh
# RFC 0019: the region-verifier change must NOT alter the page encryption key
# derivation, so an encrypted database created by PRE-change master must still
# open with the same password.  test/db/fixtures/enc_master_base.db was created
# by the base tree (origin/master before this change) with the password below;
# this opens it with the library under test and reads every record back.
#
# Library discovery follows test/db/run_handle_sizes.sh.
set -u
HERE=$(cd "$(dirname "$0")" && pwd)
BUILD=${1:-${BUILD:-"$HERE/../../build_unix"}}
[ -d "$BUILD" ] && BUILD=$(cd "$BUILD" && pwd)
SRC=$HERE/crypto_compat.c
FIX=$HERE/fixtures/enc_master_base.db
PW="fixture-passwd-RFC0019-compat"

if ! grep -q '^#define HAVE_CRYPTO 1' "$BUILD/db_config.h" 2>/dev/null ; then
	echo "run_crypto_compat.sh: SKIP not-a-crypto-build"
	exit 0
fi
[ -f "$FIX" ] || { echo "run_crypto_compat.sh: FAIL missing fixture $FIX" ; exit 1 ; }

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
[ -n "$LIB" ] || { echo "run_crypto_compat.sh: FAIL no libdb under $BUILD" ; exit 1 ; }

# An --enable-tpm libdb.a needs libtss2 at link time: take it from the build.
TPMLIBS=""
grep -q '^#define HAVE_TPM 1' "$BUILD/db_config.h" 2>/dev/null && TPMLIBS="-ltss2-esys -ltss2-mu"
EXTRALIBS=""
for l in $(pkg-config --libs liburing 2>/dev/null) ; do EXTRALIBS="$EXTRALIBS $l" ; done

BIN=$BUILD/crypto_compat
rm -f "$BIN"
"${CC:-cc}" -g -O1 ${CFLAGS:-} -I"$BUILD" "$SRC" "$LIB" \
    -lpthread $LIBRPATH $EXTRALIBS $TPMLIBS -o "$BIN" 2>&1 || {
	echo "run_crypto_compat.sh: FAIL compile" ; exit 1 ; }

# Work on a copy so an RDONLY open leaves the committed fixture untouched.
TMP=$BUILD/enc_master_base_copy.db
cp "$FIX" "$TMP"

# 1. Right password opens the base-created db and reads every record.
out=$("$BIN" read "$TMP" "$PW" 2>&1)
echo "$out" | sed 's/^/  [compat] /'
case "$out" in
*"VERDICT crypto_compat OK read"*) ok=1 ;;
*) ok=0 ;;
esac

# 2. Wrong password must be rejected (not silently open).
wout=$("$BIN" read "$TMP" "wrong-password-should-be-rejected!" 2>&1)
case "$wout" in
*"VERDICT crypto_compat FAIL"*) wrong_rejected=1 ;;
*) wrong_rejected=0 ;;
esac
echo "  [compat] wrong-password rejected: $([ $wrong_rejected = 1 ] && echo yes || echo NO)"

rm -f "$TMP"
if [ "$ok" = 1 ] && [ "$wrong_rejected" = 1 ] ; then
	echo "run_crypto_compat.sh: PASS"
	exit 0
fi
echo "run_crypto_compat.sh: FAIL"
exit 1
