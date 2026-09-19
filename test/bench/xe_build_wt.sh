#!/bin/sh
#
# xe_build_wt.sh -- build WiredTiger from source for the cross-engine TPROC
# benchmark, and record whether the resulting build has an io_uring path.
#
# WiredTiger's block manager reaches the device through its os_posix layer
# (pread/pwrite/preadv/pwritev) or, on Linux, optional O_DIRECT.  There is no
# io_uring backend in the mainline source; this script proves that from the
# built tree rather than asserting it, because the report has to state what WT
# actually used.  The evidence lands in $PREFIX/IO-URING-EVIDENCE.txt.
#
# usage: xe_build_wt.sh [srcdir] [prefix]
set -e

SRC=${1:-/nvme/wiredtiger}
PREFIX=${2:-/nvme/wt-install}
REF=${WT_REF:-develop}

if [ ! -d "$SRC/.git" ]; then
	git clone --depth 50 https://github.com/wiredtiger/wiredtiger.git "$SRC"
fi
cd "$SRC"
git fetch --depth 50 origin "$REF" 2>/dev/null || true
git checkout "$REF" 2>/dev/null || true
WT_SHA=$(git rev-parse --short HEAD)

mkdir -p "$SRC/build"
cd "$SRC/build"
# type=row B-tree only; we deliberately do NOT enable any LSM option.
cmake -G Ninja \
	-DCMAKE_BUILD_TYPE=Release \
	-DCMAKE_INSTALL_PREFIX="$PREFIX" \
	-DENABLE_PYTHON=0 \
	-DENABLE_STATIC=0 \
	..
ninja -j "$(nproc)"
ninja install

# ---- io_uring evidence -------------------------------------------------
# Three independent probes, all recorded even when they disagree.
EV="$PREFIX/IO-URING-EVIDENCE.txt"
{
	echo "# WiredTiger io_uring evidence, collected $(date -u +%FT%TZ)"
	echo "wt_source_ref=$REF"
	echo "wt_source_sha=$WT_SHA"
	echo "wt_version=$(LD_LIBRARY_PATH=$PREFIX/lib "$PREFIX/bin/wt" -V 2>&1 | head -1)"
	echo
	echo "## probe 1: io_uring symbols/strings in the built library"
	# NOTE: match io_uring/liburing as WORDS.  A plain `grep -i uring` is a
	# false-positive machine here -- it matches the English word "d-uring",
	# of which WiredTiger's error strings contain over a hundred.  The first
	# run of this script reported "uring strings PRESENT" for exactly that
	# reason and would have put a false parity claim in the report.
	if strings "$PREFIX"/lib*/libwiredtiger.so* 2>/dev/null | \
	    grep -Ei 'io_uring|liburing|io_uring_setup|IORING_'; then
		echo "RESULT: io_uring strings PRESENT"
	else
		echo "(none)"
		echo "RESULT: no io_uring strings in libwiredtiger.so"
	fi
	echo
	echo "## probe 1b: io_uring syscall symbols in the dynamic/undefined symbol table"
	nm -D --undefined-only "$PREFIX"/lib*/libwiredtiger.so.*.* 2>/dev/null | \
	    grep -Ei 'uring' || echo "(no uring symbols undefined -- nothing to link against)"
	echo
	echo "## probe 2: dynamic dependency on liburing"
	ldd "$PREFIX"/lib*/libwiredtiger.so* 2>/dev/null || true
	if ldd "$PREFIX"/lib*/libwiredtiger.so* 2>/dev/null | grep -q uring; then
		echo "RESULT: links liburing"
	else
		echo "RESULT: does NOT link liburing"
	fi
	echo
	echo "## probe 3: io_uring references in the WiredTiger source tree"
	(cd "$SRC" && grep -rilE 'io_uring|liburing' src/ 2>/dev/null | head -20) || true
	if (cd "$SRC" && grep -qriE 'io_uring' src/ 2>/dev/null); then
		echo "RESULT: source mentions io_uring"
	else
		echo "RESULT: no io_uring anywhere in src/"
	fi
	echo
	echo "## probe 4: which syscalls the block manager actually uses"
	(cd "$SRC" && grep -n 'pread\|pwrite\|preadv\|pwritev\|O_DIRECT' \
	    src/os_posix/os_fs.c 2>/dev/null | head -20) || true
} > "$EV" 2>&1

echo "=== io_uring evidence written to $EV ==="
cat "$EV"
