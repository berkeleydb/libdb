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
LIBDIRECTIO_PIN=67e57c3064f2d56273376d41bb759320c9e93110
LIBUMEM_REPO=https://codeberg.org/gregburd/libumem
LIBUMEM_PIN=60a64527e8710d955062d0c9f5fd44ce335fa7c1	# tag v4.3.2: audit re-entrancy fix (F1-002/F1-003)
FAINT_REPO=https://github.com/misc0110/faint
FAINT_PIN=b24cf2d573d245a3eb53fe47ffe3296355c09676
# libfiu: apt where packaged (Debian 12: 1.1); Ubuntu 26.04 dropped it, so
# build the 1.2 tag from source into $PREFIX when apt cannot provide it.
LIBFIU_REPO=https://github.com/albertito/libfiu
LIBFIU_PIN=04af4dbe44e4f8fa74b24913b09789af54440589	# tag 1.2

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
		iproute2 build-essential git autoconf automake libtool \
		pkg-config util-linux \
		|| log "apt-get install reported errors (continuing; SKIP arms will catch gaps)"
	# Separate install: a distro without fiu packages must not lose the rest.
	$SUDO apt-get install -y -qq fiu-utils libfiu-dev 2>/dev/null \
		|| log "fiu-utils/libfiu-dev not packaged here; building libfiu from source"
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

# libfiu from source, only when fiu-run is not already on PATH (apt).
# Installs fiu-run/fiu-ctrl and the preload libraries under $PREFIX.
if ! command -v fiu-run >/dev/null 2>&1 && [ ! -x "$PREFIX/bin/fiu-run" ]; then
	_d="$SRC/libfiu"
	if clone_pin "$LIBFIU_REPO" "$_d" "$LIBFIU_PIN"; then
		( cd "$_d" && make -s -j"$JOBS" PREFIX="$PREFIX" \
		    && make -s PREFIX="$PREFIX" install ) >/dev/null 2>&1 \
		    && log "built libfiu from source into $PREFIX" \
		    || log "libfiu source build failed (fiu arms will SKIP)"
	fi
fi

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
	# The build drops a versioned soname (libdirectio.so.MAJOR.MINOR); copy it
	# and make the unversioned symlink fi_env.sh globs for.
	_got=""
	for so in "$_d"/libdirectio.so* "$_d"/.libs/libdirectio.so*; do
		[ -f "$so" ] || continue
		cp -f "$so" "$PREFIX/lib/" && log "installed $(basename "$so")"
		_got=$so
	done
	if [ -n "$_got" ] && [ ! -e "$PREFIX/lib/libdirectio.so" ]; then
		ln -sf "$(basename "$_got")" "$PREFIX/lib/libdirectio.so"
	fi
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
# FAINT -- build the 64-bit driver only.  The upstream 'all' target also builds
# a 32-bit fault_inject32.so (via -m32) and embeds BOTH preload blobs into the
# driver with -Wl,--format=binary; on a host without the 32-bit multilib the
# -m32 compile fails.  Build every 64-bit object, a 64-bit fault_inject.so,
# stub fault_inject32.so (only embedded, never run on a 64-bit target), and
# link the driver by hand.  map_c.o MUST be compiled with gcc (C): as C++ the
# symbols _map_init/map that faint.c references get mangled and the link fails.
# ---------------------------------------------------------------------------
install_faint() {
	_d="$SRC/faint"
	clone_pin "$FAINT_REPO" "$_d" "$FAINT_PIN" || return 1
	log "build FAINT (64-bit only)"
	_V='-Wall -g -fno-builtin-log -DVERSION="0.3"'
	(
		cd "$_d" && mkdir -p obj bin &&
		gcc -c src/faint.c -O2 $_V -Wunused-result -o obj/faint.o &&
		gcc $_V -O2 -c src/usage.c -o obj/usage.o &&
		gcc $_V -O2 src/utils.c -c -o obj/utils.o &&
		gcc $_V -O2 src/log.c -fno-builtin-log -c -o obj/log.o &&
		gcc $_V -O2 -c src/modules.c -o obj/modules.o &&
		g++ $_V -O2 src/map.c -fPIC -DPIC -c -o obj/map.o &&
		g++ $_V -O2 src/modules.c -fPIC -DPIC -c -o obj/modules_s.o &&
		g++ $_V -O0 -fPIC -DPIC -c -fno-stack-protector -funwind-tables \
			-fpermissive src/fault_inject.cpp -o obj/fault_inject.o &&
		g++ -O0 -shared -o obj/fault_inject.so \
			obj/map.o obj/modules_s.o obj/fault_inject.o -ldl &&
		printf '' > obj/fault_inject32.so &&
		gcc $_V -O2 -c src/map.c -o obj/map_c.o &&
		( cd obj && gcc -O2 faint.o map_c.o usage.o utils.o log.o \
			modules.o -Wl,--format=binary -Wl,fault_inject.so \
			-Wl,--format=binary -Wl,fault_inject32.so \
			-Wl,--format=default -o faint ) &&
		mv obj/faint bin/faint
	) || { log "FAINT 64-bit build FAILED"; return 1; }
	cp -f "$_d/bin/faint" "$PREFIX/bin/" && log "installed faint"
	cp -f "$_d/obj/fault_inject.so" "$PREFIX/lib/" 2>/dev/null || true
}

install_libdirectio || log "libdirectio NOT installed (its arm will SKIP)"
install_libumem     || log "libumem NOT installed (its arm will SKIP)"
install_faint       || log "FAINT NOT installed (its arm will SKIP)"

# directio_match.so: our test-only path-filtered O_DIRECT shim (finding
# F1-001).  Lives beside install.sh; built here so the S1 preload arm can
# O_DIRECT only the data/log files and leave libdb's buffered region files
# alone.  Small and dependency-light, so always build it.
_HERE=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
if [ -f "$_HERE/directio_match.c" ]; then
	if ${CC:-cc} -shared -fPIC -D_GNU_SOURCE "$_HERE/directio_match.c" \
	    -ldl -o "$PREFIX/lib/directio_match.so" 2>/dev/null; then
		log "built directio_match.so"
	else
		log "directio_match.so build FAILED (preload arm will use raw libdirectio or SKIP)"
	fi
fi

# ---------------------------------------------------------------------------
# Report what is available.  fi_env.sh greps this, and so does CI.
# ---------------------------------------------------------------------------
avail=""
command -v fiu-run >/dev/null 2>&1 && avail="$avail fiu"
if command -v tc >/dev/null 2>&1 || [ -x /usr/sbin/tc ]; then avail="$avail tc"; fi
ls "$PREFIX"/lib/libdirectio*.so >/dev/null 2>&1 && avail="$avail directio"
ls "$PREFIX"/lib/directio_match.so >/dev/null 2>&1 && avail="$avail directio-match"
ls "$PREFIX"/lib/libumem_malloc.so >/dev/null 2>&1 && avail="$avail umem"
[ -x "$PREFIX/bin/faint" ] && avail="$avail faint"

printf 'PREFIX=%s\n' "$PREFIX"
printf 'TOOLS:%s\n' "$avail"
