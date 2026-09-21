#!/bin/sh
#
# test/c/flagapi-sabotage.sh -- teeth for the flag-behaviour tier.
#
# P6, P7 and P8 are all fixed, so there is no longer an open XFAIL for the tier's
# must-fail arm to borrow teeth from: FLAGAPI_STRICT=1 now passes.  A must-fail
# arm with nothing left to fail on is exactly the vacuous-green shape this
# project has nine recorded instances of.  So the teeth come from SABOTAGE
# instead -- break one flag's mechanism in the library and require the tier to
# notice.
#
# DB_BACKUP_NO_LOGS is the subject for two reasons: its mechanism is a single
# readable guard in db_backup.c, and its test asserts an observable consequence
# (log files present or absent in the backup directory) rather than a return
# code.  Removing that guard IS defect P6, so this also pins P6 against silent
# regression.
#
# Usage:  sh test/c/flagapi-sabotage.sh <build-dir>
# Exit 0 only when the sabotaged library makes backup_nologs FAIL.

set -u

HERE=$(cd "$(dirname "$0")" && pwd)
REPO=$(cd "$HERE/../.." && pwd)
BUILD=${1:-"$REPO/build_unix"}
SRC="$REPO/src/db/db_backup.c"
LOG=${LOG:-/tmp/flagapi-sabotage.log}
ORIG=/tmp/db_backup.c.sabotage-orig

[ -f "$SRC" ] || { echo "sabotage: no $SRC"; exit 1; }
[ -d "$BUILD" ] || { echo "sabotage: no build dir $BUILD"; exit 1; }

cp "$SRC" "$ORIG"

# Always put the tree back, however we leave.
restore() {
	cp "$ORIG" "$SRC"
	make -C "$BUILD" -j"$(nproc 2>/dev/null || echo 4)" >/dev/null 2>&1 || true
}
trap restore EXIT INT TERM

# Re-introduce P6: stop consulting the flag.
"${PYTHON:-python3}" - "$SRC" <<'EOF'
import sys
p = sys.argv[1]
s = open(p).read()
old = ("\tif (!LF_ISSET(DB_BACKUP_NO_LOGS) &&\n"
       "\t    (ret = backup_read_log_dir(dbenv, target, &copy_min, flags)) != 0)\n"
       "\t\tgoto err;")
new = ("\tif ((ret = backup_read_log_dir(dbenv, target, &copy_min, flags)) != 0)\n"
       "\t\tgoto err;")
if old not in s:
    sys.exit("sabotage anchor missing -- the DB_BACKUP_NO_LOGS guard moved, "
             "update test/c/flagapi-sabotage.sh")
open(p, 'w').write(s.replace(old, new, 1))
print("sabotage applied: DB_BACKUP_NO_LOGS guard removed")
EOF
[ $? -eq 0 ] || exit 1

if ! make -C "$BUILD" -j"$(nproc 2>/dev/null || echo 4)" \
    >/tmp/flagapi-sabotage-build.log 2>&1 ; then
	echo "sabotage: the sabotaged tree did not BUILD, so this proves nothing."
	echo "see /tmp/flagapi-sabotage-build.log"
	exit 1
fi

sabotage_rc=0
( cd "$REPO/test/c" &&
  LIBDB_RESULTS_DIR="$REPO/test/.results-sabotage" TIMEOUT=${TIMEOUT:-300} \
      ./flagapi-run.sh "$BUILD" ) >"$LOG" 2>&1 || sabotage_rc=$?

if [ "$sabotage_rc" -eq 0 ] ; then
	echo "SABOTAGE NOT DETECTED: the tier PASSED against a library with the"
	echo "DB_BACKUP_NO_LOGS guard removed.  That is defect P6 reintroduced,"
	echo "and backup_nologs did not notice -- it has stopped asserting its"
	echo "observable consequence."
	exit 1
fi

# It must fail for the RIGHT reason: backup_nologs specifically, not a crash or
# an unrelated breakage elsewhere in the tier.
if ! grep -q -- "--- backup_nologs: FAIL" "$LOG" ; then
	echo "SABOTAGE FAILED FOR THE WRONG REASON: the run failed, but"
	echo "backup_nologs was not the mode that reported FAIL."
	grep -E "^--- " "$LOG" || true
	exit 1
fi

echo "OK: removing the DB_BACKUP_NO_LOGS guard made backup_nologs FAIL --"
echo "the tier detects a real regression in a flag that is currently fixed."
exit 0
