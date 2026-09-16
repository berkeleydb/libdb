#!/bin/bash
# pinrm_build_bench.sh -- compile pin_bench against every arm's own build tree
# and verify with ldd that it links THAT tree's library.
set -u
for n in base bhpin rsnap lockrp mpoolp; do
  b=/home/admin/wt-$n/build_unix
  out=/home/admin/pin_bench-$n
  cc -O2 -g -pthread -I"$b" /home/admin/pin_bench.c \
     -L"$b/.libs" -Wl,-rpath,"$b/.libs" -ldb-2026.0 -o "$out" 2>&1 | tail -5
  if [ ! -x "$out" ]; then echo "CC-FAIL $n"; continue; fi
  lib=$(ldd "$out" | awk '/libdb-2026.0/{print $3}')
  case "$lib" in
    "$b/.libs/"*) echo "CC-OK $n -> $lib" ;;
    *) echo "CC-WRONGLIB $n -> $lib" ;;
  esac
done
