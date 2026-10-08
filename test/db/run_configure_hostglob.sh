#!/bin/sh
# Guard the host-triplet globs in the generated configure.
#
# `freebsd1*' matches freebsd10 through freebsd19, not just FreeBSD 1.x. Three
# case arms in dist/configure used it, so EVERY FreeBSD from 10 onward took the
# "no shared libraries" path from a default ./configure -- verified on FreeBSD
# 14.5, where build_libtool_libs came out `no'. The fix is `freebsd1.*'.
#
# This is a TEXT check on the generated configure, deliberately. The defect is a
# glob that is wrong for hosts we do not have in CI, so a build test cannot
# cover it: on Linux both the broken and fixed forms behave identically. The
# only way to catch a regression here is to assert the pattern itself.
#
# It also guards the same class of bug for other version-numbered platforms,
# because `netbsd1*' / `openbsd1*' would be wrong the same way the moment those
# reach version 10.

set -u
HERE=$(cd "$(dirname "$0")" && pwd)
SRC=$(cd "$HERE/../.." && pwd)
CONF=$SRC/dist/configure

[ -f "$CONF" ] || {
	echo "run_configure_hostglob.sh: FAIL no generated configure at $CONF"
	exit 1
}

rc=0

# A case ARM of the form `freebsd1*)' -- leading whitespace allowed, and the
# pattern may be one alternative among several separated by |.
bad=$(grep -nE '^[[:space:]]*(.*\|[[:space:]]*)?freebsd1\*[[:space:]]*\)' "$CONF" || true)
if [ -n "$bad" ] ; then
	echo "run_configure_hostglob.sh: FAIL unanchored freebsd1* case arm(s):"
	echo "$bad" | sed 's/^/    /'
	echo ""
	echo "  freebsd1* matches freebsd10..freebsd19. Use freebsd1.* so the arm"
	echo "  covers only real FreeBSD 1.x. Fix dist/aclocal/libtool.m4 and"
	echo "  regenerate with dist/s_config -- editing configure alone is undone"
	echo "  by the next regeneration."
	rc=1
fi

for os in netbsd openbsd dragonfly ; do
	b=$(grep -nE "^[[:space:]]*(.*\|[[:space:]]*)?${os}1\*[[:space:]]*\)" "$CONF" || true)
	[ -n "$b" ] && {
		echo "run_configure_hostglob.sh: FAIL unanchored ${os}1* case arm(s):"
		echo "$b" | sed 's/^/    /'
		rc=1
	}
done

# The anchored form must actually be PRESENT, or this test would pass on a
# configure that dropped the arm entirely (or on an empty file).
n=$(grep -cE '^[[:space:]]*freebsd1\.\*[[:space:]]*\)' "$CONF" || true)
if [ "${n:-0}" -lt 1 ] ; then
	echo "run_configure_hostglob.sh: FAIL no freebsd1.* arm found at all --"
	echo "  this check cannot confirm anything about $CONF"
	rc=1
fi

# Same class of defect, same reason a build test cannot catch it: BSD make
# leaves $< empty for an explicit rule whose suffix is not declared, and
# dist/Makefile.in declares no .SUFFIXES. The .o rules survive because .o is
# built in; the .lo rules a SHARED build generates do not, and fail with
# "cc: error: no input files". On Linux, GNU make handles $< either way, so
# only a text check sees it.
MKIN=$SRC/dist/Makefile.in
if [ -f "$MKIN" ] ; then
	d=$(grep -cE '\$\(DEPFLAGS\)[[:space:]]+\$<' "$MKIN" || true)
	if [ "${d:-0}" -gt 0 ] ; then
		echo "run_configure_hostglob.sh: FAIL $d compile rule(s) in dist/Makefile.in use \$<"
		grep -nE '\$\(DEPFLAGS\)[[:space:]]+\$<' "$MKIN" | head -5 | sed 's/^/    /'
		echo "  BSD make leaves \$< EMPTY for these. Name the source explicitly;"
		echo "  see the .SUFFIXES note in dist/Makefile.in."
		rc=1
	fi
	# And the rules must exist, so a truncated file cannot pass.
	k=$(grep -cE '^[[:space:]]+\$\(CC\) \$\(CFLAGS\) \$\(DEPFLAGS\)' "$MKIN" || true)
	if [ "${k:-0}" -lt 100 ] ; then
		echo "run_configure_hostglob.sh: FAIL only ${k:-0} compile rules found in"
		echo "  dist/Makefile.in -- expected hundreds; this check is not looking at"
		echo "  what it thinks it is."
		rc=1
	fi
fi

[ "$rc" -eq 0 ] && {
	echo "  $n anchored freebsd1.* arm(s); no unanchored version globs"
	echo "  $k compile rules in dist/Makefile.in, 0 using \$<"
	echo "run_configure_hostglob.sh: PASS"
	exit 0
}
echo "run_configure_hostglob.sh: FAIL"
exit 1
