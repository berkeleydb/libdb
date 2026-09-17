#!/bin/sh
# rdl_abi.sh -- ABI and region-signature proofs for perf/read-descent-locks.
#
# This branch adds no struct field to anything env_sig.c hashes and changes no
# public struct, so both proofs must come out IDENTICAL to the merge base.  That
# is the claim; this is the evidence, computed the same way on both trees rather
# than asserted.
set -e
W=/home/admin/rdl-wt
B=${B:-$W/bu}
# Merge base of this branch, checked out and built separately: /home/admin/libdb
# is the bare-ish clone whose build_unix/ has never been configured, so it has no
# db.h and cannot answer the ABI question.
BASE=${BASE:-/home/admin/rdl-base}
BASEB=${BASEB:-$BASE/bb}
OUT=/tmp/rdl_abi.txt

if [ ! -f $BASEB/db.h ]; then
	if [ ! -d $BASE ]; then
		( cd /home/admin/libdb &&
		  git worktree add $BASE \
		    $(cd $W && git merge-base HEAD master) )
	fi
	mkdir -p $BASEB
	( cd $BASEB && ../dist/configure --enable-debug --enable-diagnostic \
	    --disable-shared >cfg.log 2>&1 && make -j48 >build.log 2>&1 ) ||
	    { echo "BASE BUILD FAILED"; tail -20 $BASEB/build.log; exit 1; }
fi

: > $OUT
probe() {
	tree=$1; bld=$2; tag=$3
	src=/tmp/rdl_sizeof_$tag.c
	bin=/tmp/rdl_sizeof_$tag.bin
	rm -f $bin
	cat > $src <<'EOF'
#include <stdio.h>
#include "db.h"
int main(void) {
	printf("sizeof(DB)=%zu\n", sizeof(DB));
	printf("sizeof(DBC)=%zu\n", sizeof(DBC));
	printf("sizeof(DB_ENV)=%zu\n", sizeof(DB_ENV));
	printf("sizeof(DB_TXN)=%zu\n", sizeof(DB_TXN));
	return 0;
}
EOF
	gcc -O0 -o $bin $src -I$bld -L$bld -ldb -lpthread -luring
	test -x $bin || { echo "COMPILE FAILED $tag"; exit 1; }
	echo "--- ABI $tag ($bld) ---" >> $OUT
	$bin >> $OUT
	echo "--- env signature $tag ---" >> $OUT
	( cd $tree && sh dist/env_sig_print.sh 2>/dev/null ||
	  sh $tree/dist/env_sig_print.sh ) >> $OUT 2>&1
}

probe "$BASE" "$BASEB" base
probe "$W" "$B" branch

cat $OUT
echo "=== diff base vs branch ==="
awk '/^--- ABI base/,/^--- env signature base/' $OUT | grep sizeof > /tmp/rdl_abi_base.txt
awk '/^--- ABI branch/,/^--- env signature branch/' $OUT | grep sizeof > /tmp/rdl_abi_branch.txt
awk '/^--- env signature base/,/^--- ABI branch/' $OUT | grep -v '^---' > /tmp/rdl_sig_base.txt
awk '/^--- env signature branch/,0' $OUT | grep -v '^---' > /tmp/rdl_sig_branch.txt

fail=0
if diff -u /tmp/rdl_abi_base.txt /tmp/rdl_abi_branch.txt; then
	echo "ABI IDENTICAL"
else
	echo "ABI DIFFERS"; fail=1
fi
if diff -u /tmp/rdl_sig_base.txt /tmp/rdl_sig_branch.txt; then
	echo "ENV SIGNATURE IDENTICAL"
else
	echo "ENV SIGNATURE DIFFERS"; fail=1
fi

# Assert the documented absolute values too, not just base==branch: if both
# trees drifted together the diff would still be clean.
echo "=== absolute ABI check (expected 1744/552/2088/336) ==="
exp="sizeof(DB)=1744
sizeof(DBC)=552
sizeof(DB_ENV)=2088
sizeof(DB_TXN)=336"
if [ "$(cat /tmp/rdl_abi_branch.txt)" = "$exp" ]; then
	echo "ABI MATCHES DOCUMENTED VALUES"
else
	echo "ABI DOES NOT MATCH DOCUMENTED VALUES:"
	cat /tmp/rdl_abi_branch.txt
	fail=1
fi
[ $fail -eq 0 ] && echo RDL_ABI_OK || { echo RDL_ABI_FAIL; exit 1; }
