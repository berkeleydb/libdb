#!/bin/sh
# U6 regression: the meson and autoconf builds must produce INTEROPERABLE
# environments.
#
# The two build systems configure the library independently, and they drifted:
# meson omitted 13 HAVE_* probes autoconf performs (atomics, io_uring, POSIX
# aio, getrandom, the pthread re-init checks, SHM_LOCK) and force-defined two
# autoconf leaves off by default.  Several of those change struct layout, so
# __env_struct_sig() differed and an environment created by one library could
# not be attached by the other:
#
#     BDB1539 Build signature doesn't match environment
#     DB_VERSION_MISMATCH (-30969)
#
# Comparing db_config.h alone is not enough -- it would pass while the library
# still behaved differently -- so this asserts the OBSERVABLE consequence: create
# an environment with one library and open it with the other, BOTH WAYS.  It also
# diffs the HAVE_* sets, because that names which probe drifted when the attach
# fails.
#
# Usage:  sh test/db/run_meson_autoconf_parity.sh [autoconf-build-dir]
# Needs meson and ninja; SKIPs with a reason when either is absent.

set -u

HERE=$(cd "$(dirname "$0")" && pwd)
SRC=$(cd "$HERE/../.." && pwd)
ACDIR=${1:-"$SRC/build_unix"}

for t in meson ninja ; do
	command -v $t >/dev/null 2>&1 || {
		echo "run_meson_autoconf_parity.sh: SKIP no $t on this host"
		exit 0
	}
done
[ -f "$ACDIR/db_config.h" ] || {
	echo "run_meson_autoconf_parity.sh: SKIP no autoconf build at $ACDIR"
	exit 0
}

work=${TMPDIR:-/tmp}/u6parity_$$
MDIR=$work/meson
rm -f "$work"/* 2>/dev/null
mkdir -p "$work"

cleanup() { rm -f "$work"/* 2>/dev/null ; }
trap cleanup EXIT INT TERM

meson setup "$MDIR" "$SRC" >"$work/setup.log" 2>&1 || {
	echo "run_meson_autoconf_parity.sh: FAIL meson setup"
	tail -5 "$work/setup.log" | sed 's/^/    /'
	exit 1
}
ninja -C "$MDIR" >"$work/build.log" 2>&1 || {
	echo "run_meson_autoconf_parity.sh: FAIL meson build"
	grep -E 'error:|undefined reference' "$work/build.log" | head -5 | sed 's/^/    /'
	exit 1
}

rc=0

# 1. The capability sets must match.  Reported first because it names the cause.
acfg=$work/ac.txt
mcfg=$work/me.txt
grep -oE '^#define HAVE_[A-Z0-9_]+' "$ACDIR/db_config.h" | awk '{print $2}' | sort -u > "$acfg"
grep -oE '^#define HAVE_[A-Z0-9_]+' "$MDIR/dist/db_config.h" | awk '{print $2}' | sort -u > "$mcfg"
if ! cmp -s "$acfg" "$mcfg" ; then
	echo "  HAVE_* sets differ between the two builds:"
	comm -23 "$acfg" "$mcfg" | sed 's/^/    autoconf only: /'
	comm -13 "$acfg" "$mcfg" | sed 's/^/    meson only:    /'
	rc=1
fi

# 2. The observable consequence: cross-attach, both directions.
cat > "$work/xattach.c" <<'EOF'
#include <stdio.h>
#include <db.h>
int main(int argc, char **argv) {
	DB_ENV *e;
	int r;
	if ((r = db_env_create(&e, 0)) != 0) {
		printf("%s create-failed %d\n", argv[2], r);
		return (2);
	}
	e->set_errfile(e, stderr);
	r = e->open(e, argv[1],
	    DB_CREATE | DB_INIT_MPOOL | DB_INIT_TXN | DB_INIT_LOG | DB_INIT_LOCK,
	    0644);
	printf("%s rc=%d %s\n", argv[2], r, r ? db_strerror(r) : "OK");
	if (r == 0)
		(void)e->close(e, 0);
	return (r != 0);
}
EOF

URING=$(pkg-config --libs liburing 2>/dev/null)
aclib=$(ls "$ACDIR"/.libs/libdb-*.so "$ACDIR"/libdb.a 2>/dev/null | head -1)
[ -n "$aclib" ] || { echo "run_meson_autoconf_parity.sh: FAIL no autoconf library"; exit 1; }

cc -O0 -I "$ACDIR" "$work/xattach.c" "$aclib" \
    -Wl,-rpath,"$ACDIR/.libs" -lpthread $URING -o "$work/xa" 2>/dev/null || {
	echo "run_meson_autoconf_parity.sh: FAIL cannot build the autoconf probe"
	exit 1; }
cc -O0 -I "$MDIR/dist" "$work/xattach.c" "$MDIR/dist/libdb.so" \
    -Wl,-rpath,"$MDIR/dist" -lpthread $URING -o "$work/xm" 2>/dev/null || {
	echo "run_meson_autoconf_parity.sh: FAIL cannot build the meson probe"
	exit 1; }

for pair in 'xa:xm:autoconf-creates:meson-attaches' \
            'xm:xa:meson-creates:autoconf-attaches' ; do
	a=$(echo "$pair" | cut -d: -f1)
	b=$(echo "$pair" | cut -d: -f2)
	an=$(echo "$pair" | cut -d: -f3)
	bn=$(echo "$pair" | cut -d: -f4)
	d=$work/env_$a
	rm -f "$d"/* 2>/dev/null
	mkdir -p "$d"
	o1=$("$work/$a" "$d" "$an" 2>&1 | tail -1)
	echo "  $o1"
	echo "$o1" | grep -q 'rc=0' || { rc=1 ; continue ; }
	o2=$("$work/$b" "$d" "$bn" 2>&1 | grep "^$bn" | tail -1)
	echo "  $o2"
	echo "$o2" | grep -q 'rc=0' || rc=1
done

if [ "$rc" -eq 0 ] ; then
	echo "run_meson_autoconf_parity.sh: PASS"
else
	echo "run_meson_autoconf_parity.sh: FAIL"
fi
exit $rc
