#!/bin/sh
# Cross-compile the core libdb C library for Android via the NDK's clang and
# the existing root meson.build. Produces build-android/libdb.so (aarch64 by
# default). No emulator/device needed -- this is a cross-BUILD test.
#
# The NDK clang is just a cross-compiler: meson's HAVE_* probes are all
# compile/link checks (which work when cross-compiling) and sizeof/alignment
# are resolved by compile-only probes, so no target execution is required.
#
# Usage:
#   ANDROID_NDK_ROOT=/path/to/ndk dist/android/build_android.sh [ABI] [API]
#
#   ABI : android ABI          -> aarch64 (default) | arm | x86_64 | x86
#   API : android API level    -> 24 (default)
#
# ANDROID_NDK_ROOT is exported by nttld/setup-ndk and android-actions/setup-android.
# ANDROID_NDK_HOME / NDK are accepted as fallbacks.
set -eu

ABI="${1:-aarch64}"
API="${2:-24}"

NDK="${ANDROID_NDK_ROOT:-${ANDROID_NDK_HOME:-${NDK:-}}}"
if [ -z "$NDK" ]; then
  echo "error: set ANDROID_NDK_ROOT (or ANDROID_NDK_HOME / NDK) to the NDK root" >&2
  exit 2
fi

# NDK layout: toolchains/llvm/prebuilt/<host>/bin/<triple><API>-clang
# nixpkgs' ndk-bundle nests the real NDK under libexec/android-sdk/ndk/<ver>/.
if [ ! -d "$NDK/toolchains" ]; then
  nested=$(find "$NDK" -maxdepth 5 -type d -path '*/toolchains/llvm/prebuilt/*/bin' 2>/dev/null | head -1)
  [ -n "$nested" ] && NDK=$(dirname "$(dirname "$(dirname "$(dirname "$(dirname "$nested")")")")")
fi

# NDK r27+ still used linux-x86_64; do not assume it, because when the guess is
# wrong every path below is silently empty and the failure surfaces as meson's
# "Unknown compiler(s)".  Prefer the host tag that actually has a bin/ directory.
case "$(uname -s)" in
  Linux)  HOSTTAGS="linux-x86_64 linux-aarch64" ;;
  Darwin) HOSTTAGS="darwin-x86_64 darwin-arm64" ;;
  *) echo "error: unsupported build host $(uname -s)" >&2; exit 2 ;;
esac

BIN=""
for h in $HOSTTAGS ; do
	if [ -d "$NDK/toolchains/llvm/prebuilt/$h/bin" ] ; then
		BIN="$NDK/toolchains/llvm/prebuilt/$h/bin" ; HOSTTAG=$h ; break
	fi
done
# Last resort: find any bin/ holding a clang, whatever the layout is called.
if [ -z "$BIN" ] ; then
	found=$(find "$NDK" -type f -name 'clang' -perm -u+x 2>/dev/null | head -1)
	if [ -n "$found" ] ; then
		BIN=$(dirname "$found") ; HOSTTAG="(discovered)"
	else
		BIN=""
	fi
fi
if [ -z "$BIN" ] || [ ! -d "$BIN" ] ; then
	echo "error: no NDK toolchain bin/ under $NDK" >&2
	echo "  tried host tags: $HOSTTAGS" >&2
	echo "  prebuilt/ contains:" >&2
	ls "$NDK/toolchains/llvm/prebuilt" 2>/dev/null | sed 's/^/    /' >&2 ||
	    echo "    (no toolchains/llvm/prebuilt at all)" >&2
	echo "  NDK root contains:" >&2
	ls "$NDK" 2>/dev/null | head -12 | sed 's/^/    /' >&2
	exit 2
fi

case "$ABI" in
  aarch64) TRIPLE=aarch64-linux-android;   CPUFAM=aarch64; CPU=aarch64 ;;
  arm)     TRIPLE=armv7a-linux-androideabi; CPUFAM=arm;     CPU=armv7a ;;
  x86_64)  TRIPLE=x86_64-linux-android;    CPUFAM=x86_64;  CPU=x86_64 ;;
  x86)     TRIPLE=i686-linux-android;      CPUFAM=x86;     CPU=i686 ;;
  *) echo "error: unknown ABI '$ABI' (aarch64|arm|x86_64|x86)" >&2; exit 2 ;;
esac

# Resolve the actual compiler.  Three layouts have shipped:
#
#   a) bin/<triple><API>-clang works directly (r25-r27 and earlier).
#   b) the wrapper is present but its sibling `clang' is not, so running it dies
#      with "bin/clang: No such file or directory" -- meson reports this only as
#      the unhelpful "Unknown compiler(s)".  NDK r29 is in this shape: bin/ holds
#      the per-API wrappers for every API level and NO bare clang.
#   c) the real driver is present under a versioned name (clang-NN) or in a
#      sibling directory, and must be driven with an explicit --target.
#
# So testing -x is never sufficient: the compiler has to be RUN.
CC="$BIN/${TRIPLE}${API}-clang"
CXX="$BIN/${TRIPLE}${API}-clang++"
TARGETFLAG=""
if ! "$CC" --version >/dev/null 2>&1 ; then
	real=""
	# Prefer an unversioned clang, then clang-NN (highest first), looking in
	# bin/ and then anywhere under the NDK.
	for cand in "$BIN/clang" $(ls -1 "$BIN"/clang-[0-9]* 2>/dev/null | sort -Vr) ; do
		if [ -x "$cand" ] && "$cand" --version >/dev/null 2>&1 ; then
			real="$cand" ; break
		fi
	done
	if [ -z "$real" ] ; then
		for cand in $(find "$NDK" -type f \( -name 'clang' -o -name 'clang-[0-9]*' \) \
		    -perm -u+x 2>/dev/null | sort -Vr) ; do
			if "$cand" --version >/dev/null 2>&1 ; then real="$cand" ; break ; fi
		done
	fi
	if [ -n "$real" ] ; then
		echo "note: $CC is not runnable; using $real --target=${TRIPLE}${API}" >&2
		CC="$real"
		case "$real" in
		*/clang) CXX="${real}++" ;;
		*)       CXX="$real" ;;   # clang-NN also compiles C++ via --target
		esac
		[ -x "$CXX" ] || CXX="$real"
		TARGETFLAG="--target=${TRIPLE}${API}"
	else
		echo "error: no runnable NDK compiler found." >&2
		echo "  tried: $BIN/${TRIPLE}${API}-clang (exists but did not run)" >&2
		echo "  tried: $BIN/clang, $BIN/clang-NN, and any clang under $NDK" >&2
		echo "  why the wrapper failed:" >&2
		"$BIN/${TRIPLE}${API}-clang" --version 2>&1 | head -3 | sed 's/^/    /' >&2
		echo "  non-wrapper entries in $BIN:" >&2
		ls -1 "$BIN" 2>/dev/null | grep -vE -- '-(clang|clang\+\+)$' |
		    head -25 | sed 's/^/    /' >&2
		exit 2
	fi
fi

ROOT=$(cd "$(dirname "$0")/../.." && pwd)
BUILDDIR="${BUILDDIR:-$ROOT/build-android}"
CROSS="$BUILDDIR.cross.txt"
mkdir -p "$BUILDDIR"

# llvm-ar / llvm-strip / llvm-ranlib normally sit beside clang, but r29 has
# already shown that "beside clang" is not a safe assumption -- so resolve each
# one the same way: try $BIN, then a versioned name, then anywhere under the NDK,
# and only fail once nothing answers.  Values land in AR/STRIP/RANLIB.
find_tool() {
	_want=$1
	for _c in "$BIN/$_want" $(ls -1 "$BIN/$_want"-[0-9]* 2>/dev/null | sort -Vr) ; do
		[ -x "$_c" ] && { echo "$_c" ; return 0 ; }
	done
	_c=$(find "$NDK" -type f \( -name "$_want" -o -name "$_want-[0-9]*" \) \
	    -perm -u+x 2>/dev/null | sort -Vr | head -1)
	[ -n "$_c" ] && { echo "$_c" ; return 0 ; }
	# llvm-* are drop-in for the plain names on a clang toolchain.
	_c=$(find "$NDK" -type f -name "${_want#llvm-}" -perm -u+x 2>/dev/null | head -1)
	[ -n "$_c" ] && { echo "$_c" ; return 0 ; }
	return 1
}

AR=$(find_tool llvm-ar) || { echo "error: NDK tool not found: llvm-ar" >&2 ; MISSING=1 ; }
STRIP=$(find_tool llvm-strip) || { echo "error: NDK tool not found: llvm-strip" >&2 ; MISSING=1 ; }
RANLIB=$(find_tool llvm-ranlib) || { echo "error: NDK tool not found: llvm-ranlib" >&2 ; MISSING=1 ; }
if [ -n "${MISSING:-}" ] ; then
	echo "  searched $BIN and all of $NDK" >&2
	echo "  bin/ non-wrapper entries:" >&2
	ls -1 "$BIN" 2>/dev/null | grep -vE -- '-(clang|clang\+\+)$' |
	    head -25 | sed 's/^/    /' >&2
	exit 2
fi

# meson accepts a LIST for a binary, so when we fell back to the untargeted
# clang the --target goes here rather than into c_args -- that way it applies to
# meson's own compiler sanity check too, not just to our translation units.
if [ -n "$TARGETFLAG" ] ; then
	CBIN="['$CC', '$TARGETFLAG']"
	CPPBIN="['$CXX', '$TARGETFLAG']"
else
	CBIN="'$CC'"
	CPPBIN="'$CXX'"
fi

cat > "$CROSS" <<EOF
# Generated by dist/android/build_android.sh -- do not commit.
[binaries]
c = $CBIN
cpp = $CPPBIN
ar = '$AR'
strip = '$STRIP'
ranlib = '$RANLIB'

[host_machine]
system = 'android'
cpu_family = '$CPUFAM'
cpu = '$CPU'
endian = 'little'
EOF

echo "== NDK:  $NDK"
echo "== CC:   $CC $TARGETFLAG"
echo "== out:  $BUILDDIR/libdb.so"

meson setup "$BUILDDIR" "$ROOT" --cross-file "$CROSS" --wipe 2>/dev/null \
  || meson setup "$BUILDDIR" "$ROOT" --cross-file "$CROSS"
ninja -C "$BUILDDIR"

# The library builds under $BUILDDIR/dist/ (dist/meson.build owns library()).
# Copy it to $BUILDDIR/libdb.so so the artifact path stays stable.
cp "$BUILDDIR/dist/libdb.so" "$BUILDDIR/libdb.so"

"$STRIP" -o "$BUILDDIR/libdb.so.stripped" "$BUILDDIR/libdb.so" 2>/dev/null || true
file "$BUILDDIR/libdb.so"
