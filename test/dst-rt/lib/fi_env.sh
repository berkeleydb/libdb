#!/bin/sh -
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# test/dst-rt/lib/fi_env.sh -- tool discovery and SKIP rules for RFC 0024.
# Sourced by every scenario and by run.sh.  Each tool is OPTIONAL: a missing
# tool makes the arms that need it SKIP (never fail), so the harness runs on a
# bare runner and reports honestly what it could not exercise.
#
# After sourcing, these are set:
#   DSTRT_DIR     the test/dst-rt directory
#   BUILD         the libdb build dir (default .), must hold libdb.a + db.h
#   LIBDB_LIB     the resolved libdb static (or shared) library
#   TOOLS_PREFIX  where install.sh put the source-built tools (default /opt/dst-rt)
#   HAVE_FIU HAVE_TC HAVE_NETNS HAVE_DIRECTIO HAVE_UMEM HAVE_FAINT  (1/0)
#   DIRECTIO_SO UMEM_SO FAINT_BIN  paths (empty if absent)
# and the helpers fi_build_node / fi_build_tool / fi_skip / fi_have.

set -u

DSTRT_DIR=${DSTRT_DIR:-$(CDPATH= cd -- "$(dirname -- "${0:-.}")/.." && pwd)}
case "$DSTRT_DIR" in
*/dst-rt) : ;;			# already the dst-rt dir
*) [ -d "$DSTRT_DIR/dst-rt" ] && DSTRT_DIR="$DSTRT_DIR/dst-rt" ;;
esac

BUILD=${BUILD:-.}
TOOLS_PREFIX=${TOOLS_PREFIX:-/opt/dst-rt}
CC=${CC:-cc}

# --- libdb library (same pattern as test/db/run_dd_stripes.sh) -------------
LIBDB_LIB=""
LIBDB_RPATH=""
for cand in "$BUILD"/libdb.a "$BUILD"/.libs/libdb-5.3.a "$BUILD"/.libs/libdb-*.a; do
	[ -f "$cand" ] && { LIBDB_LIB="$cand"; break; }
done
if [ -z "$LIBDB_LIB" ]; then
	for cand in "$BUILD"/.libs/libdb-5.3.so "$BUILD"/.libs/libdb-*.so; do
		[ -f "$cand" ] && { LIBDB_LIB="$cand"
			LIBDB_RPATH="-Wl,-rpath,$(cd "$BUILD/.libs" && pwd)"; break; }
	done
fi

# transitive deps a static libdb needs (liburing, and OpenSSL if --enable-tls).
LIBDB_EXTRA="-lpthread"
for l in $(pkg-config --libs liburing 2>/dev/null); do LIBDB_EXTRA="$LIBDB_EXTRA $l"; done
# repmgr mode may pull in TLS; name OpenSSL if the lib references it.
if [ -n "$LIBDB_LIB" ] && nm "$LIBDB_LIB" 2>/dev/null | grep -q SSL_connect; then
	LIBDB_EXTRA="$LIBDB_EXTRA -lssl -lcrypto"
fi

# --- tool discovery --------------------------------------------------------
fi_have_cmd() { command -v "$1" >/dev/null 2>&1; }

HAVE_FIU=0; fi_have_cmd fiu-run && HAVE_FIU=1

TC_BIN=""
if fi_have_cmd tc; then TC_BIN=tc
elif [ -x /usr/sbin/tc ]; then TC_BIN=/usr/sbin/tc; fi
HAVE_TC=0; [ -n "$TC_BIN" ] && HAVE_TC=1

# netns needs CAP_NET_ADMIN -- effectively root or passwordless sudo.
HAVE_NETNS=0
if [ "$(id -u)" -eq 0 ]; then
	ip netns list >/dev/null 2>&1 && HAVE_NETNS=1
elif sudo -n true 2>/dev/null; then
	sudo -n ip netns list >/dev/null 2>&1 && HAVE_NETNS=1
fi

DIRECTIO_SO=""
for so in "$TOOLS_PREFIX"/lib/libdirectio*.so /usr/lib/libdirectio*.so \
    /usr/local/lib/libdirectio*.so; do
	[ -f "$so" ] && { DIRECTIO_SO="$so"; break; }
done
HAVE_DIRECTIO=0; [ -n "$DIRECTIO_SO" ] && HAVE_DIRECTIO=1

UMEM_SO=""
for so in "$TOOLS_PREFIX"/lib/libumem_malloc.so /usr/lib/libumem_malloc.so \
    /usr/local/lib/libumem_malloc.so; do
	[ -f "$so" ] && { UMEM_SO="$so"; break; }
done
HAVE_UMEM=0; [ -n "$UMEM_SO" ] && HAVE_UMEM=1

FAINT_BIN=""
for b in "$TOOLS_PREFIX"/bin/faint /usr/bin/faint /usr/local/bin/faint; do
	[ -x "$b" ] && { FAINT_BIN="$b"; break; }
done
HAVE_FAINT=0; [ -n "$FAINT_BIN" ] && HAVE_FAINT=1

# fi_have TOOL -- 1 if available.  TOOL in: fiu tc netns directio umem faint libdb
fi_have() {
	case "$1" in
	fiu) [ "$HAVE_FIU" = 1 ] ;;
	tc) [ "$HAVE_TC" = 1 ] ;;
	netns) [ "$HAVE_NETNS" = 1 ] ;;
	directio) [ "$HAVE_DIRECTIO" = 1 ] ;;
	umem) [ "$HAVE_UMEM" = 1 ] ;;
	faint) [ "$HAVE_FAINT" = 1 ] ;;
	libdb) [ -n "$LIBDB_LIB" ] ;;
	*) return 1 ;;
	esac
}

# fi_skip REASON -- print a SKIP line and exit 77 (the run.sh/CI skip code).
fi_skip() {
	echo "SKIP: $*"
	exit 77
}

# fi_build_node OUT -- compile node/rtnode.c, rtload.c, or rtcheck.c (OUT is the
# basename, e.g. rtnode) against libdb into $BUILD/<OUT>.
fi_build_node() {
	_out=$1
	_src="$DSTRT_DIR/node/$_out.c"
	[ -f "$_src" ] || { echo "fi_build_node: no $_src" >&2; return 1; }
	[ -n "$LIBDB_LIB" ] || { echo "fi_build_node: no libdb library under $BUILD" >&2; return 1; }
	"$CC" -g -O1 ${CFLAGS:-} -I"$BUILD" "$_src" "$LIBDB_LIB" \
		$LIBDB_RPATH $LIBDB_EXTRA -o "$BUILD/$_out"
}

# fi_mkrun PREFIX -- make a fresh run directory outside any env home and echo it.
# The ack log lives here (RFC 0024 §2.7), excluded from libfiu I/O plans.
fi_mkrun() {
	_p=${1:-dstrt}
	_r="${DSTRT_RUN_ROOT:-${TMPDIR:-/tmp}}/$_p.$$.$(date +%s)"
	[ -d "$_r" ] && find "$_r" -mindepth 1 -delete 2>/dev/null
	mkdir -p "$_r"
	echo "$_r"
}

export DSTRT_DIR BUILD TOOLS_PREFIX CC
export LIBDB_LIB LIBDB_RPATH LIBDB_EXTRA
export HAVE_FIU HAVE_TC HAVE_NETNS HAVE_DIRECTIO HAVE_UMEM HAVE_FAINT
export DIRECTIO_SO UMEM_SO FAINT_BIN TC_BIN
