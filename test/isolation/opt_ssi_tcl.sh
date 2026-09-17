#!/bin/sh
# opt_ssi_tcl.sh -- run ssi001..ssi011 explicitly and assert real verdict lines.
#
# Carried over from perf/read-descent-locks unchanged except for the tree paths:
# the SSI suite is a hard gate for any change to the read path, and RFC 0007
# phase 1 changes how a read descent reaches its leaf.
#
# The targeted list in test/MANIFEST only carries ssi001/ssi002, so a run of the
# manifest subset is NOT a run of the SSI suite.  This runs all eleven and fails
# unless it sees eleven pass verdicts -- rc=0 from tclsh proves nothing here (a
# test whose source fails prints an error and the interpreter still exits 0).
set -u
W=${W:-/tmp/optw}
B=${B:-$W/btcl}
TCLSH=${TCLSH:-tclsh}
OUT=/tmp/opt_ssi_tcl.txt
SCRIPT=/tmp/opt_ssi.tcl
TESTS="ssi001 ssi002 ssi003 ssi004 ssi005 ssi006 ssi007 ssi008 ssi009 ssi010 ssi011"

rm -f $OUT $SCRIPT
{
	echo "source $W/test/tcl/test.tcl"
	for t in $TESTS; do
		cat <<TCL
if {[catch {source $W/test/tcl/$t.tcl} res]} {
	puts "VERDICT $t fail (source: \$res)"
} elseif {[catch {eval $t} res]} {
	puts "VERDICT $t fail (\$res)"
} else {
	puts "VERDICT $t pass"
}
TCL
	done
	echo "puts \"OPT_SSI_TCL_END\""
} > $SCRIPT

cd $B
mkdir -p TESTDIR
find TESTDIR -mindepth 1 -delete 2>/dev/null || true
timeout 3000 $TCLSH $SCRIPT > $OUT 2>&1
echo "tclsh rc=$?"

echo "=== verdicts ==="
grep -E "^VERDICT|OPT_SSI_TCL_END" $OUT || true
np=$(grep -c "^VERDICT .* pass" $OUT || true)
nf=$(grep -c "^VERDICT .* fail" $OUT || true)
echo "pass=$np fail=$nf expected_total=11"
if ! grep -q OPT_SSI_TCL_END $OUT; then
	echo "FAIL: run did not reach the end marker -- it died partway"
	tail -20 $OUT
	exit 1
fi
if [ "$np" -ne 11 ] || [ "$nf" -ne 0 ]; then
	echo "FAIL: expected 11 pass / 0 fail"
	grep -E "^VERDICT .* fail" -A 3 $OUT | head -40
	exit 1
fi
echo "OPT_SSI_TCL_OK 11/11"
