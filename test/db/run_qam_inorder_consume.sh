#!/bin/sh
# Regression for P7: DB_INORDER + DB_CONSUME over a deleted record spins forever.
#
# The defect is a HANG, so rc alone cannot judge it: a timeout kill and a clean
# exit both need distinguishing from a real verdict.  The driver prints exactly
# one VERDICT line, and this script requires it.  A hang produces NO verdict,
# which is the failure signal -- not a nonzero rc.
#
# Both arms are run: inorder=0 is the control that always worked, so a
# regression in the shared consume path shows up as the control failing too.

test -n "$1" && cd "$1"
lib=$(ls libdb.a .libs/libdb-*.so 2>/dev/null | head -1)
if [ -z "$lib" ]; then echo "qam_inorder_consume.sh: FAIL no libdb library found" ; exit 1 ; fi
src=$(dirname "$0")/qam_inorder_consume.c
inc=$(dirname "$0")/../../src

cc -O2 -I. -I"$inc" "$src" $lib -Wl,-rpath,"$PWD/.libs" -lpthread \
    -o ./qam_inorder_consume 2>/dev/null || {
	echo "qam_inorder_consume.sh: FAIL compile" ; exit 1 ; }
test -x ./qam_inorder_consume || { echo "qam_inorder_consume.sh: FAIL no binary"; exit 1; }

rc=0
for arm in 1 0 ; do
	d=./QAM_INORDER_$arm
	rm -f $d/* 2>/dev/null
	mkdir -p $d
	out=$(timeout 60 ./qam_inorder_consume $d $arm 2>&1)
	kill_rc=$?
	v=$(echo "$out" | grep '^VERDICT p7' | head -1)
	if [ $kill_rc -eq 124 ] ; then
		echo "  inorder=$arm: HUNG (no verdict, killed at 60s) -- P7 regression"
		rc=1
	elif [ -z "$v" ] ; then
		echo "  inorder=$arm: no VERDICT line (rc=$kill_rc) -- driver did not report"
		rc=1
	elif echo "$v" | grep -q 'PASS' ; then
		echo "  inorder=$arm: $v"
	else
		echo "  inorder=$arm: $v"
		rc=1
	fi
done

if [ $rc -eq 0 ] ; then echo "qam_inorder_consume.sh: PASS" ; else echo "qam_inorder_consume.sh: FAIL" ; fi
exit $rc
