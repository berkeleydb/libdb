#!/bin/sh
# RFC 0019: the built-in key-file provider.  A 0600 raw 256-bit key file opens
# an encrypted env and data round-trips; a group/world-accessible or
# wrong-length key file is refused (EINVAL).
#
# Library discovery follows test/db/run_handle_sizes.sh.
set -u
HERE=$(cd "$(dirname "$0")" && pwd)
BUILD=${1:-${BUILD:-"$HERE/../../build_unix"}}
[ -d "$BUILD" ] && BUILD=$(cd "$BUILD" && pwd)
SRC=$HERE/crypto_keyfile.c

if ! grep -q '^#define HAVE_CRYPTO 1' "$BUILD/db_config.h" 2>/dev/null ; then
	echo "run_crypto_keyfile.sh: SKIP not-a-crypto-build"
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
[ -n "$LIB" ] || { echo "run_crypto_keyfile.sh: FAIL no libdb under $BUILD" ; exit 1 ; }

EXTRALIBS=""
for l in $(pkg-config --libs liburing 2>/dev/null) ; do EXTRALIBS="$EXTRALIBS $l" ; done

BIN=$BUILD/crypto_keyfile
rm -f "$BIN"
"${CC:-cc}" -g -O1 ${CFLAGS:-} -I"$BUILD" "$SRC" "$LIB" \
    -lpthread $LIBRPATH $EXTRALIBS -o "$BIN" 2>&1 || {
	echo "run_crypto_keyfile.sh: FAIL compile" ; exit 1 ; }

out=$("$BIN" 2>&1)
echo "$out" | sed 's/^/  /'
case "$out" in
*"VERDICT crypto_keyfile PASS"*) echo "run_crypto_keyfile.sh: PASS" ; exit 0 ;;
esac
echo "run_crypto_keyfile.sh: FAIL"
exit 1
