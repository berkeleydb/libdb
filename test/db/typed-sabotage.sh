#!/bin/sh
#
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# See the file LICENSE for redistribution information.
#
# test/db/typed-sabotage.sh -- teeth for the RFC 0014 tests (spec §7.1, §7.2).
#
# Each ARM re-introduces one defect (the exact revert named in §7.2), rebuilds
# the given build dir, runs the one test that must catch it, REQUIRES that test
# to FAIL, then restores the tree and rebuilds.  An arm whose test still passes
# means the TEST is wrong -- that is the vacuous-green shape this project has
# shipped eleven times -- so this exits non-zero then.
#
# Not in CI: every arm is red by design.  The implementer runs every arm before
# each commit and pastes the FAIL lines into the commit message (§9).  Follows
# test/c/flagapi-sabotage.sh.
#
# Usage:  sh test/db/typed-sabotage.sh <build-dir> [arm ...]     (default: all)
#         sh test/db/typed-sabotage.sh -l                        (list arms)
# Exit 0 only when every requested arm made its test FAIL.
#
# An arm is three functions: arm_<id>_apply (edit the tree; non-zero if the
# anchor text is gone, so a moved anchor cannot pass vacuously), the test to
# run (arm_<id>_test), and the FAIL text it must print (arm_<id>_expect, a
# grep -E pattern).  The tree is restored from a copy of every file an arm
# touched, whatever happens.

set -u
HERE=$(cd "$(dirname "$0")" && pwd)
REPO=$(cd "$HERE/../.." && pwd)
MAKE=${MAKE:-$(command -v gmake || command -v make)}
PY=${PYTHON:-python3}

# edit FILE OLD NEW -- replace exactly one occurrence of OLD (a literal) or fail.
edit() {
	"$PY" - "$REPO/$1" "$2" "$3" <<'EOF'
import sys
p, old, new = sys.argv[1], sys.argv[2], sys.argv[3]
s = open(p).read()
n = s.count(old)
if n != 1:
    sys.exit("sabotage anchor found %d times in %s (want 1): the code moved, "
             "update test/db/typed-sabotage.sh" % (n, p))
open(p, 'w').write(s.replace(old, new, 1))
EOF
}

# ---------------------------------------------------------------------------
# Arms.  ARMS lists them in run order.  Each test command runs in the build
# dir with BUILD set; TIER is set to the test's MANIFEST tier.
# ---------------------------------------------------------------------------
ARMS="footprint_canary"

# §7.2 typed_footprint (a): an unguarded symbol in an engine file must move
# the default build's .text.
arm_footprint_canary_files="src/db/db_cam.c"
arm_footprint_canary_apply() {
	printf '\nint __typed_canary(void);\nint\n__typed_canary(void)\n{\n\treturn (1);\n}\n' \
	    >> "$REPO/src/db/db_cam.c"
}
arm_footprint_canary_test="TIER=db sh $HERE/run_typed_footprint.sh"
arm_footprint_canary_expect='^VERDICT typed_footprint FAIL.* 1\(\.text'
arm_footprint_canary_rebuild=0	# the runner builds its own trees

# ---------------------------------------------------------------------------

[ "${1:-}" = -l ] && { for a in $ARMS ; do echo "$a" ; done ; exit 0 ; }
BUILD=${1:?usage: typed-sabotage.sh <build-dir> [arm ...]}
BUILD=$(cd "$BUILD" && pwd) || exit 1
shift
want=${*:-$ARMS}

save=$(mktemp -d) || exit 1
rc=0
for a in $want ; do
	eval "files=\${arm_${a}_files:-}"
	eval "tcmd=\${arm_${a}_test:-}"
	eval "expect=\${arm_${a}_expect:-}"
	eval "rebuild=\${arm_${a}_rebuild:-1}"
	[ -n "$tcmd" ] || { echo "ARM $a: unknown arm" ; rc=1 ; continue ; }

	for f in $files ; do
		mkdir -p "$save/$(dirname "$f")"
		cp "$REPO/$f" "$save/$f"
	done
	restore() {
		for f in $files ; do cp "$save/$f" "$REPO/$f" ; done
	}
	trap 'restore ; exit 1' INT TERM

	if ! "arm_${a}_apply" ; then
		echo "ARM $a: could not apply (anchor missing)"
		restore ; rc=1 ; continue
	fi
	if [ "$rebuild" = 1 ] &&
	    ! (cd "$BUILD" && $MAKE -j4) > "$BUILD/sabotage_$a.build.log" 2>&1 ; then
		echo "ARM $a: the sabotaged tree did not BUILD; proves nothing"
		echo "       see $BUILD/sabotage_$a.build.log"
		restore ; rc=1 ; continue
	fi
	out=$(cd "$BUILD" && BUILD="$BUILD" eval "$tcmd" 2>&1)
	trc=$?
	restore
	trap - INT TERM
	fl=$(echo "$out" | grep -E "$expect" | head -3)
	if [ "$trc" -ne 0 ] && [ -n "$fl" ] ; then
		echo "ARM $a: FAIL as required (rc=$trc):"
		echo "$fl" | sed 's/^/    /'
	else
		echo "ARM $a: SABOTAGE NOT DETECTED (rc=$trc) -- the test is wrong"
		echo "$out" | grep VERDICT | head -5 | sed 's/^/    /'
		rc=1
	fi
done

# Put the build back in the restored tree's state.
if [ -n "$want" ] ; then
	(cd "$BUILD" && $MAKE -j4) > "$BUILD/sabotage_restore.build.log" 2>&1 ||
	    { echo "restore rebuild FAILED: see $BUILD/sabotage_restore.build.log" ; rc=1 ; }
fi
[ -d "$save" ] && find "$save" -mindepth 1 -delete && rmdir "$save"
exit $rc
