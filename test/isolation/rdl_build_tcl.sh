#!/bin/sh
# rdl_build_tcl.sh -- TCL-enabled build, needed to run ssi001..ssi011.
#
# The bu/ (DIAGNOSTIC) and bp/ (measurement) trees are configured without
# --enable-tcl, so test/tcl/include.tcl is present but UNSUBSTITUTED
# (tclsh_path empty) and sourcing test.tcl dies with "can't read tclsh_path".
# A run against those trees produces zero verdicts, which is not a pass.
set -e
W=/home/admin/rdl-wt
cd $W
mkdir -p btcl
cd btcl
if [ ! -f db.h ]; then
	../dist/configure --enable-tcl --with-tcl=/usr/lib/x86_64-linux-gnu \
	    --enable-debug --enable-diagnostic >cfg.log 2>&1 ||
	    { tail -30 cfg.log; exit 1; }
fi
make -j48 >build.log 2>&1 || { tail -40 build.log; exit 1; }
echo "--- include.tcl ---"
sed -n '3,5p' include.tcl
test -n "$(sed -n 's/^set tclsh_path //p' include.tcl)" ||
    { echo "FAIL tclsh_path still empty"; exit 1; }
ls -la .libs/libdb_tcl*.so 2>/dev/null | head -2 || true
echo TCL_BUILD_OK
