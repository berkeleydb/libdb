#!/bin/sh
# rdl_build_prod.sh -- production-flavoured build for the MEASUREMENT arms.
#
# The bu/ tree is --enable-debug --enable-diagnostic: correct for the gdb/stat
# probes (DIAGNOSTIC adds the lock-order checker and extra assertions), WRONG for
# throughput numbers.  Measurement uses this tree instead.
set -e
W=/home/admin/rdl-wt
cd $W
mkdir -p bp
cd bp
if [ ! -f db.h ]; then
	../dist/configure --disable-shared --enable-stat >cfg.log 2>&1 ||
	    { tail -20 cfg.log; exit 1; }
fi
make -j48 >build.log 2>&1 || { tail -30 build.log; exit 1; }
grep -c "DIAGNOSTIC" db_config.h || true
echo "PROD_BUILD_OK"
grep -E "^#define (DIAGNOSTIC|HAVE_STATISTICS)" db_config.h || true
