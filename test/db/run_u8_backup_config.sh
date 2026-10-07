#!/bin/sh
# U8 regression: the four backup tunables must actually reach the library.
#
# EnvironmentConfig.setBackupReadCount and its three siblings existed but were
# WRITE-ONLY -- they stored to a private field and nothing pushed the value into
# DB_ENV->set_backup_config, so calling them had no effect whatsoever.  Neither
# compilation nor an accessor round-trip can detect that: the field round-trips
# perfectly either way.  So this opens a REAL environment and reads each value
# back through the C layer via Environment.getConfig().
#
# Needs a JDK and the JNI library (--enable-java); SKIPs with a reason otherwise,
# because the Java bindings are optional and most runners do not build them.

set -u
HERE=$(cd "$(dirname "$0")" && pwd)
SRC=$HERE/../../lang/java/src
BUILD=${1:-"$HERE/../../build_unix"}

command -v javac >/dev/null 2>&1 || {
	echo "run_u8_backup_config.sh: SKIP no javac on this host"
	exit 0
}
jni=$(ls "$BUILD"/.libs/libdb_java-*.so "$BUILD"/.libs/libdb_java-*.dylib \
    2>/dev/null | head -1)
[ -n "$jni" ] || {
	echo "run_u8_backup_config.sh: SKIP no JNI library in $BUILD (needs --enable-java)"
	exit 0
}

d=${TMPDIR:-/tmp}/u8bc_$$
rm -f "$d"/* 2>/dev/null
mkdir -p "$d/classes" "$d/env"
trap 'rm -f "$d"/* 2>/dev/null' EXIT INT TERM

javac -nowarn -d "$d/classes" $(find "$SRC" -name '*.java') 2>"$d/err" || {
	echo "run_u8_backup_config.sh: FAIL the Java sources do not compile"
	sed -n '1,5p' "$d/err" | sed 's/^/    /'
	exit 1
}
javac -nowarn -cp "$d/classes" -d "$d/classes" "$HERE/U8Check.java" \
    2>>"$d/err" || {
	echo "run_u8_backup_config.sh: FAIL U8Check does not compile"
	sed -n '1,5p' "$d/err" | sed 's/^/    /'
	exit 1
}

out=$(java -cp "$d/classes" -Dsleepycat.db.libfile="$jni" \
    com.sleepycat.db.U8Check "$d/env" 2>&1)
echo "$out" | sed 's/^/  /'

v=$(echo "$out" | grep '^VERDICT u8' | head -1)
[ -n "$v" ] || {
	echo "run_u8_backup_config.sh: FAIL no VERDICT line (driver did not report)"
	exit 1
}
if echo "$v" | grep -q PASS ; then
	echo "run_u8_backup_config.sh: PASS"
	exit 0
fi
echo "run_u8_backup_config.sh: FAIL"
exit 1
