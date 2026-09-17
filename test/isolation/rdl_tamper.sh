#!/bin/sh
# rdl_tamper.sh -- TEETH CHECK for the D10 phantom gate.
#
# A passing test proves nothing unless it can fail.  This deliberately breaks the
# mechanism the gate is supposed to be watching -- the SIREAD marker a read
# descent leaves on the leaf -- and asserts that phantom_pages then FAILS under
# DB_TXN_SERIALIZABLE.
#
# The tamper is exactly the change this task was asked to evaluate: "under SI, do
# read-only descents need lock-manager lock objects at all?"  It makes __db_lget
# skip the lock for a snapshot-safe (SSI) read exactly as it already does for a
# plain SI read, i.e. it treats candidate answer (4) as if it were true for SSI
# too.  If phantom_pages still passed with this applied, the gate would be
# worthless.
#
# Builds in a SEPARATE tree (bt/) so the trusted build is never the tampered one.
set -e
W=/home/admin/rdl-wt
T=/home/admin/rdl-tamper
BT=$T/bt

rm -f /tmp/rdl_tamper_verdict.txt
mkdir -p $T
# Fresh copy of the source each run: never tamper the tree the real gate uses.
find $T -mindepth 1 -maxdepth 1 ! -name bt -exec rm -rf {} + 2>/dev/null || true
mkdir -p $T/src
cp -a $W/src $W/dist $W/test $W/util $W/lang $T/ 2>/dev/null || true

# The tamper: skip the lock for SSI reads too, not just plain SI reads.
python3 - "$T/src/db/db_meta.c" <<'PY'
import sys
p = sys.argv[1]
s = open(p).read()
old = """	if (MULTIVERSION(dbp) && mode == DB_LOCK_READ &&
	    txn != NULL && F_ISSET(txn, TXN_SNAPSHOT)) {
		if (!F_ISSET(txn, TXN_SNAPSHOT_SAFE)) {
			LOCK_INIT(*lockp);
			return (0);
		}"""
new = """	if (MULTIVERSION(dbp) && mode == DB_LOCK_READ &&
	    txn != NULL && F_ISSET(txn, TXN_SNAPSHOT)) {
		if (1 /* RDL TAMPER: SSI reads too */) {
			LOCK_INIT(*lockp);
			return (0);
		}"""
if old not in s:
    sys.exit("TAMPER_ANCHOR_NOT_FOUND")
open(p, "w").write(s.replace(old, new, 1))
print("TAMPER_APPLIED")
PY

mkdir -p $BT
cd $BT
if [ ! -f db.h ]; then
	$T/dist/configure --enable-debug --enable-diagnostic --disable-shared \
	    >cfg.log 2>&1 || { tail -20 cfg.log; exit 1; }
fi
make -j48 >build.log 2>&1 || { tail -30 build.log; exit 1; }
# The tampered object must actually be in the archive we are about to test.
if ! grep -q "RDL TAMPER" $T/src/db/db_meta.c; then
	echo "TAMPER LOST"; exit 1
fi
echo "TAMPER_BUILD_OK"

cd $T/test/isolation
LIBDB_BUILD=$BT ISO_PARTS=default ISO_LEVEL=both ISO_SSI_GATES=0 \
    ISO_TIMEOUT=600 timeout 800 ./run.sh phantom_pages g2_antidep \
    > /tmp/rdl_tamper_verdict.txt 2>&1 || true
echo "=== tampered-build verdicts ==="
grep -E "ISO_LEVEL|== |scan saw|insert |committed txns|PASS|FAIL|XFAIL|unexpected" \
    /tmp/rdl_tamper_verdict.txt
echo "=== teeth assertion ==="
# Under SERIALIZABLE the tampered build MUST fail phantom_pages.
if sed -n '/ISO_LEVEL=serializable/,$p' /tmp/rdl_tamper_verdict.txt |
   grep -q "FAIL"; then
	echo "TEETH OK: tampered build FAILS phantom_pages under DB_TXN_SERIALIZABLE"
else
	echo "TEETH MISSING: tampered build still passed -- the gate is not watching the marker"
	exit 1
fi
