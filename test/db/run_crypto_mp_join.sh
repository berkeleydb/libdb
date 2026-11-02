#!/bin/sh
# RFC 0019: multi-process join with the verifier.  A creator makes an encrypted
# env; N child processes join concurrently (each recomputes the salted verifier
# and compares it in constant time) and read the record back; a wrong-key joiner
# is rejected (EPERM).  Run for the password and key-file providers.
#
# Library discovery follows test/db/run_handle_sizes.sh.
set -u
HERE=$(cd "$(dirname "$0")" && pwd)
BUILD=${1:-${BUILD:-"$HERE/../../build_unix"}}
[ -d "$BUILD" ] && BUILD=$(cd "$BUILD" && pwd)
SRC=$HERE/crypto_mp_join.c

if ! grep -q '^#define HAVE_CRYPTO 1' "$BUILD/db_config.h" 2>/dev/null ; then
	echo "run_crypto_mp_join.sh: SKIP not-a-crypto-build"
	exit 0
fi

LIB=""
LIBRPATH=""
for cand in "$BUILD"/libdb.a "$BUILD"/.libs/libdb-*.a ; do
	[ -f "$cand" ] && { LIB="$cand" ; break ; }
done
if [ -z "$LIB" ] ; then
	for cand in "$BUILD"/.libs/libdb-*.so ; do
		[ -f "$cand" ] && { LIB="$cand" ; break ; }
	done
	[ -n "$LIB" ] && LIBRPATH="-Wl,-rpath,$(cd "$BUILD/.libs" && pwd)"
fi
[ -n "$LIB" ] || { echo "run_crypto_mp_join.sh: FAIL no libdb under $BUILD" ; exit 1 ; }

EXTRALIBS=""
for l in $(pkg-config --libs liburing 2>/dev/null) ; do EXTRALIBS="$EXTRALIBS $l" ; done

BIN=$BUILD/crypto_mp_join
rm -f "$BIN"
"${CC:-cc}" -g -O1 ${CFLAGS:-} -I"$BUILD" "$SRC" "$LIB" \
    -lpthread $LIBRPATH $EXTRALIBS -o "$BIN" 2>&1 || {
	echo "run_crypto_mp_join.sh: FAIL compile" ; exit 1 ; }

rc=0

echo "  --- password provider, 8 joiners ---"
out=$(cd "$BUILD" && "$BIN" passwd 8 2>&1)
echo "$out" | sed 's/^/  /'
case "$out" in *"VERDICT crypto_mp_join PASS"*) : ;; *) rc=1 ;; esac

# Key-file provider: generate a 0600 raw key outside the env home.
KF=$BUILD/mp_key.raw
rm -f "$KF"
head -c 32 /dev/urandom > "$KF" 2>/dev/null
chmod 600 "$KF"
if [ -s "$KF" ] ; then
	echo "  --- key-file provider, 8 joiners ---"
	out=$(cd "$BUILD" && "$BIN" keyfile 8 "$KF" 2>&1)
	echo "$out" | sed 's/^/  /'
	case "$out" in *"VERDICT crypto_mp_join PASS"*) : ;; *) rc=1 ;; esac
	rm -f "$KF"
fi

if [ "$rc" -eq 0 ] ; then echo "run_crypto_mp_join.sh: PASS" ; else echo "run_crypto_mp_join.sh: FAIL" ; fi
exit $rc
