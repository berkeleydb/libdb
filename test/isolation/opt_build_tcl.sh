#!/bin/sh
# opt_build_tcl.sh -- TCL-enabled build, needed to run ssi001..ssi011.
#
# The bu/ (DIAGNOSTIC) and bp/ (measurement) trees are configured without
# --enable-tcl, so test/tcl/include.tcl is present but UNSUBSTITUTED
# (tclsh_path empty) and sourcing test.tcl dies with "can't read tclsh_path".
# A run against those trees produces zero verdicts, which is not a pass.
#
# --enable-test is also required, not just --enable-tcl: without it the tcl
# extension is built but the test-only commands are not registered, and test.tcl
# dies at `berkdb getconfig` with "bad command".  Same symptom class -- zero
# verdicts -- from a different missing flag.  Both flags match ci.yml's
# tcl-tests job.
set -e
W=${W:-/tmp/optw}
cd $W
mkdir -p btcl
cd btcl
if [ ! -f db.h ]; then
	../dist/configure --enable-tcl --enable-test \
	    --with-tcl=/usr/lib/tcl8.6 \
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
