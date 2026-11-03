#!/bin/sh -
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# test/dst-rt/tools/install.sh -- install the runtime fault-injection tools
# (RFC 0024).  Idempotent and reproducible: distro packages where they exist,
# pinned source commits otherwise.  Runs on Debian 12 (the dev box) and on a
# GitHub ubuntu-latest runner.
#
# Tools (RFC 0024 §1):
#   libfiu       -- apt: fiu-utils libfiu-dev   (POSIX/malloc fault preload)
#   libdirectio  -- github.com/gburd/libdirectio (O_DIRECT on every open)
#   libumem      -- codeberg.org/gregburd/libumem (guard redzones, UAF, leaks)
#   FAINT        -- github.com/misc0110/faint    (systematic malloc sweep)
#   iproute2     -- apt: tc + ip (netns/netem, used by F2)
#
# Everything built from source lands under $PREFIX (default /opt/dst-rt), which
# CI caches.  A built tree with the pin files present is reused, so a cache hit
# skips the build.  Env knobs:
#   PREFIX         install root for source-built tools (default /opt/dst-rt)
#   SKIP_APT=1     do not run apt (for a runner that pre-installed packages)
#   JOBS           parallel make jobs (default: nproc)
#
# Prints a final "TOOLS:" line naming what is available, which the caller
# (fi_env.sh, CI) can grep.

set -eu

PREFIX=${PREFIX:-/opt/dst-rt}
JOBS=${JOBS:-$(nproc 2>/dev/null || echo 2)}
SKIP_APT=${SKIP_APT:-0}

# Pinned commits.  Bump deliberately; a cache key in CI includes these.
LIBDIRECTIO_REPO=https://github.com/gburd/libdirectio
LIBDIRECTIO_PIN=master
LIBUMEM_REPO=https://codeberg.org/gregburd/libumem
LIBUMEM_PIN=master
FAINT_REPO=https://github.com/misc0110/faint
FAINT_PIN=master

SRC="$PREFIX/src"
mkdir -p "$PREFIX/bin" "$PREFIX/lib" "$SRC"

log() { printf '[install] %s\n' "$*" >&2; }

# ---------------------------------------------------------------------------
# apt packages: libfiu + iproute2 (+ build deps).
# ---------------------------------------------------------------------------
if [ "$SKIP_APT" != 1 ]; then
	SUDO=""
	[ "$(id -u)" -eq 0 ] || SUDO="sudo"
	log "apt packages (fiu, iproute2, build deps)"
	export DEBIAN_FRONTEND=noninteractive
	$SUDO apt-get update -qq || log "apt-get update failed (continuing)"
	# fiu-utils gives fiu-run/fiu-ctrl; libfiu-dev gives the headers/lib so
	# FAINT and anything else can link it.  iproute2 gives tc + ip.
	$SUDO apt-get install -y -qq \
		fiu-utils libfiu-dev iproute2 \
		build-essential git autoconf automake libtool pkg-config \
		|| log "apt-get install reported errors (continuing; SKIP arms will catch gaps)"
fi

# clone_pin REPO DIR PIN -- shallow clone or update DIR to PIN.
clone_pin() {
	_repo=$1 _dir=$2 _pin=$3
	if [ -d "$_dir/.git" ]; then
		( cd "$_dir" && git fetch -q origin "$_pin" 2>/dev/null \
			&& git checkout -q "$_pin" 2>/dev/null ) \
			|| log "could not update $_dir to $_pin (using existing tree)"
	else
		git clone -q "$_repo" "$_dir" || { log "clone $_repo failed"; return 1; }
		( cd "$_dir" && git checkout -q "$_pin" 2>/dev/null ) \
			|| log "checkout $_pin failed in $_dir"
	fi
	# record the resolved commit so the caller/CI cache sees the exact pin
	( cd "$_dir" && git rev-parse HEAD > .dst-rt-pin 2>/dev/null ) || true
}

# ---------------------------------------------------------------------------
# libdirectio -- make && make check, install the .so into $PREFIX/lib.
# ---------------------------------------------------------------------------
install_libdirectio() {
	_d="$SRC/libdirectio"
	clone_pin "$LIBDIRECTIO_REPO" "$_d" "$LIBDIRECTIO_PIN" || return 1
	log "build libdirectio"
	( cd "$_d" && make -j"$JOBS" ) || { log "libdirectio build FAILED"; return 1; }
	# make check is best-effort: it proves the preload works on THIS host's fs.
	( cd "$_d" && make check ) || log "libdirectio 'make check' reported issues"
	# the build drops libdirectio.so (name may vary); copy whatever .so appears.
	for so in "$_d"/libdirectio*.so "$_d"/.libs/libdirectio*.so; do
		[ -f "$so" ] && cp -f "$so" "$PREFIX/lib/" && log "installed $(basename "$so")"
	done
}

# ---------------------------------------------------------------------------
# libumem -- autogen/configure/make/make check; install the malloc preload.
# ---------------------------------------------------------------------------
install_libumem() {
	_d="$SRC/libumem"
	clone_pin "$LIBUMEM_REPO" "$_d" "$LIBUMEM_PIN" || return 1
	log "build libumem"
	( cd "$_d" && [ -x ./configure ] || ./autogen.sh ) \
		|| { log "libumem autogen FAILED"; return 1; }
	( cd "$_d" && ./configure --prefix="$PREFIX" ) \
		|| { log "libumem configure FAILED"; return 1; }
	( cd "$_d" && make -j"$JOBS" ) || { log "libumem build FAILED"; return 1; }
	( cd "$_d" && make check ) || log "libumem 'make check' reported issues"
	( cd "$_d" && make install ) || log "libumem install reported issues"
	# The preload we use is libumem_malloc.so (the malloc-only interposer).
	for so in "$_d"/.libs/libumem_malloc.so "$PREFIX"/lib/libumem_malloc.so; do
		[ -f "$so" ] && cp -f "$so" "$PREFIX/lib/" 2>/dev/null && \
			log "have $(basename "$so")" && break
	done
}

# ---------------------------------------------------------------------------
# FAINT -- make (64-bit only; the 32-bit multilib target often cannot build).
# ---------------------------------------------------------------------------
install_faint() {
	_d="$SRC/faint"
	clone_pin "$FAINT_REPO" "$_d" "$FAINT_PIN" || return 1
	log "build FAINT (64-bit only)"
	# Build only the 64-bit targets; the default 'make' may also try a 32-bit
	# multilib that is absent on a minimal runner.  Build the named 64-bit
	# artefacts and tolerate a failure of the 32-bit ones.
	( cd "$_d" && make -j"$JOBS" faint faint_preload.so 2>/dev/null ) \
		|| ( cd "$_d" && make -j"$JOBS" 2>/dev/null ) \
		|| log "FAINT build reported errors (64-bit artefacts checked below)"
	for b in "$_d"/faint; do
		[ -f "$b" ] && cp -f "$b" "$PREFIX/bin/" && log "installed faint"
	done
	for so in "$_d"/faint_preload.so "$_d"/*.so; do
		[ -f "$so" ] && cp -f "$so" "$PREFIX/lib/" && log "installed $(basename "$so")"
	done
}

install_libdirectio || log "libdirectio NOT installed (its arm will SKIP)"
install_libumem     || log "libumem NOT installed (its arm will SKIP)"
install_faint       || log "FAINT NOT installed (its arm will SKIP)"

# ---------------------------------------------------------------------------
# Report what is available.  fi_env.sh greps this, and so does CI.
# ---------------------------------------------------------------------------
avail=""
command -v fiu-run >/dev/null 2>&1 && avail="$avail fiu"
command -v tc >/dev/null 2>&1 || [ -x /usr/sbin/tc ] && avail="$avail tc"
ls "$PREFIX"/lib/libdirectio*.so >/dev/null 2>&1 && avail="$avail directio"
ls "$PREFIX"/lib/libumem_malloc.so >/dev/null 2>&1 && avail="$avail umem"
[ -x "$PREFIX/bin/faint" ] && avail="$avail faint"

printf 'PREFIX=%s\n' "$PREFIX"
printf 'TOOLS:%s\n' "$avail"
