#!/bin/bash
# pinrm_pathlen.sh -- is the cross-run baseline level set by the LENGTH of the
# PIN_HOME path?  ASLR is off on this box (randomize_va_space=0), so the size of
# the environment block deterministically shifts the stack and every subsequent
# mmap; a 512 MB DB_PRIVATE region landing at a different offset changes its
# cache-set aliasing.  That is the only difference left between two runs that
# gave 7.9M and 4.85M for the same binary and the same arguments.
#
# Run the identical command with PIN_HOME paths of the two lengths used by the
# sweep (41 chars) and the confirm run (38 chars), alternating, plus two padded
# controls, and see whether throughput tracks the path length.
set -u
REPS=${1:-4}
OUT=${2:-/home/admin/runs/pathlen.txt}
BIN=/home/admin/pin_bench-base
mkdir -p "$(dirname "$OUT")"
# Same parent, names chosen so the full path lengths differ as the two runs did.
A=/home/admin/runs/env-base-private-batch-r1   # 41 chars, as pinrm_sweep.sh
B=/home/admin/runs/cf-base-private-batch       # 38 chars, as pinrm_confirm.sh
C=/home/admin/runs/env-base-private-batch-r1XX # 43
D=/home/admin/runs/x                           # 19
echo "# pathlen probe start $(date -Is) reps=$REPS" >>"$OUT"
for rep in $(seq 1 "$REPS"); do
  if [ $((rep % 2)) = 1 ]; then order="$A $B $C $D"; else order="$D $C $B $A"; fi
  for P in $order; do
    mkdir -p "$P"; find "$P" -mindepth 1 -delete
    PIN_PRIVATE=1 PIN_HOME=$P PIN_CACHE_MB=512 \
      PIN_TAG="len${#P}/private/rep$rep" \
      timeout 300 "$BIN" batch 200000 16 2 5 96 >>"$OUT" 2>>"$OUT.err" \
      || echo "FAIL path=$P rep=$rep exit=$?" >>"$OUT"
    find "$P" -mindepth 1 -delete
  done
done
echo "# pathlen probe done $(date -Is)" >>"$OUT"
grep -E '^RESULT|^FAIL' "$OUT" | sed 's/batch_p50.*//' | tail -20
echo PATHLEN-DONE
