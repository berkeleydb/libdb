#!/bin/sh
# U9 regression: DB_TXN_SERIALIZABLE must stay reachable from the Java API.
#
# Checks the accessor contract on TransactionConfig, CursorConfig and
# EnvironmentConfig -- round-trip, independence from the snapshot switch, and
# that the constant is the one the C layer expects.  The flag WORDS are built
# inside beginTransaction()/openCursor(), which need a live environment, so this
# additionally asserts that each class's flag-assembly code references
# DbConstants.DB_TXN_SERIALIZABLE at all.
#
# Skips (exit 0 with a reason) when no JDK is present: the Java bindings are
# optional and most CI runners for this tier do not build them.

set -u
HERE=$(cd "$(dirname "$0")" && pwd)
SRC=$HERE/../../lang/java/src

command -v javac >/dev/null 2>&1 || {
	echo "run_u9_serializable.sh: SKIP no javac on this host"
	exit 0
}

# The flag-assembly wiring: a grep, because the value cannot be read back
# without an environment.
#
# `grep -c ... || echo 0' is WRONG here: grep -c already prints 0 when it finds
# nothing and merely exits 1, so the `|| echo 0' appends a SECOND line and the
# arithmetic test then fails with "integer expected" and is treated as false --
# which made the sabotage arm of this very test pass. Use grep -c alone and
# ignore its exit status.
rc=0
for f in TransactionConfig CursorConfig EnvironmentConfig ; do
	n=$(grep -c "DbConstants.DB_TXN_SERIALIZABLE" \
	    "$SRC/com/sleepycat/db/$f.java" 2>/dev/null)
	[ -n "$n" ] || n=0
	if [ "$n" -lt 1 ] ; then
		echo "  $f.java does not reference DB_TXN_SERIALIZABLE"
		rc=1
	fi
done

d=${TMPDIR:-/tmp}/u9_$$
rm -f "$d"/* 2>/dev/null
mkdir -p "$d"
javac -nowarn -d "$d" $(find "$SRC" -name '*.java') 2>"$d/javac.err" || {
	echo "run_u9_serializable.sh: FAIL the Java sources do not compile"
	sed -n '1,5p' "$d/javac.err" | sed 's/^/    /'
	exit 1
}
javac -nowarn -cp "$d" -d "$d" "$HERE/U9Check.java" 2>>"$d/javac.err" || {
	echo "run_u9_serializable.sh: FAIL U9Check does not compile"
	sed -n '1,5p' "$d/javac.err" | sed 's/^/    /'
	exit 1
}

out=$(java -cp "$d" com.sleepycat.db.U9Check 2>&1)
echo "$out" | sed 's/^/  /'
v=$(echo "$out" | grep '^VERDICT u9' | head -1)
[ -n "$v" ] || { echo "run_u9_serializable.sh: FAIL no VERDICT line"; exit 1; }
echo "$v" | grep -q PASS || rc=1

if [ "$rc" -eq 0 ] ; then
	echo "run_u9_serializable.sh: PASS"
else
	echo "run_u9_serializable.sh: FAIL"
fi
exit $rc
