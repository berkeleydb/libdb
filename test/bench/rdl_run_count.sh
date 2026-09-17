#!/bin/sh
# rdl_run_count.sh -- build + run rdl_count on EC2, fresh env dir each time.
set -e
W=/home/admin/rdl-wt
B=$W/bu
S=/tmp/rdl-scratch
OUT=/tmp/rdl_count.bin
rm -f $OUT
gcc -O2 -g -o $OUT $W/test/bench/rdl_count.c -I$B -L$B -ldb -lpthread -luring
test -x $OUT || { echo "COMPILE FAILED"; exit 1; }
mkdir -p $S
find $S -mindepth 1 -delete
mkdir -p $S/env
timeout 600 $OUT -h $S/env "$@"
