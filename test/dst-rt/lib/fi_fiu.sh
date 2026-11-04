#!/bin/sh -
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# test/dst-rt/lib/fi_fiu.sh -- libfiu wrappers and named, seeded fault plans
# (RFC 0024 §2.2).  Sourced after fi_env.sh.
#
# A fault PLAN is a set of libfiu points enabled with a probability and a
# failinfo (errno or short-count), all driven by one FIU_PRNG_SEED so a seed
# replays the same schedule.  We enable points at PROCESS START with fiu-run
# -x so the whole run is under the plan; libfiu's posix preload has no path
# filter, so the ack log is kept out of scope by the raw-syscall writer in
# fi_acklog.h (it never reaches the wrapped write(2)) -- see §2.7.
#
# Each plan maps to fiu-run -c arguments.  fi_fiu_cmd PLAN echoes the
# "fiu-run ... --" prefix to put in front of a node command; the caller
# appends the binary and its args.
#
# libfiu point names (the Debian 1.1 posix preload):
#   posix/io/rw/write  posix/io/rw/pwrite  posix/io/rw/read  posix/io/rw/pread
#   posix/io/rw/{p,}{read,write}/reduce  (short count)
#   posix/io/sync/fsync  posix/io/sync/fdatasync
#   posix/io/oc/open  posix/io/oc/close
#   libc/mm/malloc  libc/mm/calloc  libc/mm/realloc
# SYNTAX: the fiu-run -c argument is ONE comma-separated string:
#   'enable_random name=NAME,probability=P[,failinfo=N]'
# (a space between name= and probability= silently disables the random roll;
# learned the hard way -- it then never fires).

# fi_fiu_cmd PLAN SEED [PROB] -- echo the fiu-run prefix for PLAN.
# PROB overrides the plan default (0..1 float).  Needs HAVE_FIU.
fi_fiu_cmd() {
	_plan=$1 _seed=${2:-1} _prob=${3:-}
	[ "${HAVE_FIU:-0}" = 1 ] || { echo ""; return 1; }

	# FIU_PRNG_SEED makes enable_random deterministic.
	_pre="env FIU_PRNG_SEED=$_seed fiu-run"

	case "$_plan" in
	io-eio-write)
		# pwrite/write fail EIO (errno 5) at probability p on one node.
		_p=${_prob:-0.02}
		echo "$_pre -x \
-c 'enable_random name=posix/io/rw/pwrite,probability=$_p,failinfo=5' \
-c 'enable_random name=posix/io/rw/write,probability=$_p,failinfo=5'"
		;;
	io-fsync-fail)
		# fdatasync/fsync return EIO.  Expected: libdb panics or returns
		# the error; it must NEVER report a durable commit it did not.
		_p=${_prob:-0.05}
		echo "$_pre -x \
-c 'enable_random name=posix/io/sync/fdatasync,probability=$_p,failinfo=5' \
-c 'enable_random name=posix/io/sync/fsync,probability=$_p,failinfo=5'"
		;;
	io-enospc)
		# pwrite/write return ENOSPC (errno 28).
		_p=${_prob:-0.02}
		echo "$_pre -x \
-c 'enable_random name=posix/io/rw/pwrite,probability=$_p,failinfo=28' \
-c 'enable_random name=posix/io/rw/write,probability=$_p,failinfo=28'"
		;;
	io-short)
		# Short read/write: libfiu's reduce action returns a partial
		# count.  The posix preload exposes it as the .../reduce point;
		# failinfo is the number of bytes to subtract from the request.
		_p=${_prob:-0.05}
		echo "$_pre -x \
-c 'enable_random name=posix/io/rw/pwrite/reduce,probability=$_p,failinfo=512' \
-c 'enable_random name=posix/io/rw/pread/reduce,probability=$_p,failinfo=512'"
		;;
	mem-fiu-random)
		# malloc/calloc/realloc fail (return NULL) at probability p,
		# anywhere in the process incl. libc/OpenSSL/tss2.
		_p=${_prob:-0.001}
		echo "$_pre -x \
-c 'enable_random name=libc/mm/malloc,probability=$_p' \
-c 'enable_random name=libc/mm/calloc,probability=$_p' \
-c 'enable_random name=libc/mm/realloc,probability=$_p'"
		;;
	none)
		echo "$_pre -x"
		;;
	*)
		echo "fi_fiu_cmd: unknown plan '$_plan'" >&2
		echo ""
		return 1
		;;
	esac
}

# fi_fiu_selftest -- prove fiu-run -x can preload a libdb binary at all.
# Runs BIN under fiu-run with no points enabled; a 0 exit means the preload
# did not break the process (the brief's "fiu-run -x works on a libdb binary"
# check).  Returns non-zero only if fiu-run itself cannot wrap BIN.
fi_fiu_selftest() {
	_bin=$1; shift
	[ "${HAVE_FIU:-0}" = 1 ] || return 1
	fiu-run -x "$_bin" "$@" >/dev/null 2>&1
}
