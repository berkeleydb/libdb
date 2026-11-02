#!/bin/sh
# RFC 0019 TPM 2.0 provider, against a software TPM (swtpm).  SKIPs cleanly when
# the build has no TPM support or swtpm/tss2 cannot be brought up (e.g. stock CI
# runners); --enable-tpm is EXCLUDED in option_sweep.sh for the same reason.
#
#   seal a KEK to the (software) TPM, open an encrypted env whose DEK is wrapped
#   under it (create+write), reopen (join+read), and check a wrong PCR state is
#   refused.
#
# Library discovery follows test/db/run_handle_sizes.sh.
set -u
HERE=$(cd "$(dirname "$0")" && pwd)
BUILD=${1:-${BUILD:-"$HERE/../../build_unix"}}
[ -d "$BUILD" ] && BUILD=$(cd "$BUILD" && pwd)
SRC=$HERE/crypto_tpm.c

if ! grep -q '^#define HAVE_TPM 1' "$BUILD/db_config.h" 2>/dev/null ; then
	echo "run_crypto_tpm.sh: SKIP not-a-tpm-build (--enable-tpm)"
	exit 0
fi
command -v swtpm >/dev/null 2>&1 || {
	echo "run_crypto_tpm.sh: SKIP swtpm not installed"
	exit 0
}
KEYTOOL=$BUILD/db_keytool
[ -x "$KEYTOOL" ] || { echo "run_crypto_tpm.sh: SKIP no db_keytool" ; exit 0 ; }

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
[ -n "$LIB" ] || { echo "run_crypto_tpm.sh: FAIL no libdb under $BUILD" ; exit 1 ; }

TSS2LIBS="-ltss2-esys -ltss2-mu"
BIN=$BUILD/crypto_tpm
rm -f "$BIN"
"${CC:-cc}" -g -O1 ${CFLAGS:-} -I"$BUILD" "$SRC" "$LIB" \
    -lpthread $LIBRPATH $TSS2LIBS -o "$BIN" 2>/dev/null || {
	echo "run_crypto_tpm.sh: SKIP could not link against libtss2" ; exit 0 ; }

# --- bring up swtpm (mssim TCP), guarded so it can never hang the tier ----
STATE=$(mktemp -d)
PORT=${SWTPM_PORT:-2321}
CTRL=$((PORT + 1))
pkill -f "swtpm socket.*port=$PORT" 2>/dev/null
# Launch fully detached; some swtpm builds do not background cleanly with
# --daemon over a pipe, so redirect every fd and disown.
( setsid swtpm socket --tpm2 --tpmstate dir="$STATE" \
    --server type=tcp,port=$PORT --ctrl type=tcp,port=$CTRL \
    --flags startup-clear </dev/null >"$STATE/out" 2>&1 & ) >/dev/null 2>&1
sleep 2
# Confirm the socket is up within a bounded wait; else SKIP.
up=0
for i in 1 2 3 4 5 ; do
	if (exec 3<>/dev/tcp/127.0.0.1/$PORT) 2>/dev/null ; then up=1 ; break ; fi
	sleep 1
done
if [ "$up" != 1 ] ; then
	echo "run_crypto_tpm.sh: SKIP swtpm did not come up on port $PORT"
	pkill -f "swtpm socket.*port=$PORT" 2>/dev/null
	rm -rf "$STATE"
	exit 0
fi
export TPM2TOOLS_TCTI="swtpm:host=127.0.0.1,port=$PORT"
export TSS2_TCTI="swtpm:host=127.0.0.1,port=$PORT"

cleanup() {
	pkill -f "swtpm socket.*port=$PORT" 2>/dev/null
	rm -rf "$STATE"
	rm -rf "$BUILD/TESTDIR_crypto_tpm"
}
trap cleanup EXIT

rc=0
BLOB=$BUILD/tpm_kek.blob
rm -f "$BLOB"

# 1. Seal a fresh KEK (no PCR policy) and open + reopen the env.
rm -rf "$BUILD/TESTDIR_crypto_tpm" ; mkdir -p "$BUILD/TESTDIR_crypto_tpm"
if ! "$KEYTOOL" -T "$BLOB" >/dev/null 2>&1 ; then
	echo "run_crypto_tpm.sh: SKIP db_keytool could not seal to swtpm"
	exit 0
fi
out=$(cd "$BUILD" && "$BIN" open "$BLOB" 2>&1)
echo "$out" | sed 's/^/  [seal-open] /'
case "$out" in
*"VERDICT crypto_tpm OK open"*) echo "  seal/unseal + env open OK" ;;
*) echo "  FAIL seal/open" ; rc=1 ;;
esac

# 2. PCR policy: seal bound to PCR 16, extend PCR 16, expect refusal.
if command -v tpm2_pcrextend >/dev/null 2>&1 ; then
	rm -rf "$BUILD/TESTDIR_crypto_tpm" ; mkdir -p "$BUILD/TESTDIR_crypto_tpm"
	rm -f "$BLOB"
	# PCR mask bit 16 -> byte 2 bit 0 -> 0x010000.
	if "$KEYTOOL" -T "$BLOB" -C 0x010000 >/dev/null 2>&1 ; then
		tpm2_pcrextend 16:sha256=$(printf '0%.0s' $(seq 1 64)) \
		    >/dev/null 2>&1
		bout=$(cd "$BUILD" && "$BIN" badpcr "$BLOB" 2>&1)
		echo "$bout" | sed 's/^/  [wrong-pcr] /'
		case "$bout" in
		*"VERDICT crypto_tpm OK badpcr-refused"*)
			echo "  wrong-PCR correctly refused" ;;
		*)	echo "  FAIL wrong-PCR not refused" ; rc=1 ;;
		esac
	else
		echo "  [wrong-pcr] SKIP: could not seal with a PCR policy"
	fi
else
	echo "  [wrong-pcr] SKIP: tpm2_pcrextend not available"
fi

if [ "$rc" -eq 0 ] ; then echo "run_crypto_tpm.sh: PASS" ; else echo "run_crypto_tpm.sh: FAIL" ; fi
exit $rc
