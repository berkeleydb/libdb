#!/bin/bash
# pinrm_correctness.sh -- correctness gate for any arm that looks like a win.
# usage: pinrm_correctness.sh <arm>
#
# Two checks, both required by the re-measurement brief:
#   1. db_verify clean on an environment the arm's own library produced.
#   2. the batch_diff differential test -- batched reads must equal individual
#      reads, including conflict/abort behaviour under DB_TXN_SERIALIZABLE.
#
# Neither accepts rc=0 as a verdict: db_verify must print its OK line and
# batch_diff must print its PASS line plus >=4 VERDICT lines (the same
# anti-vacuity rule test/c/leak-run.sh enforces).
set -u
ARM=${1:?arm}
WT=/home/admin/wt-$ARM
B=$WT/build_unix
rc=0

echo "=== correctness arm=$ARM ($(cd "$WT" && git log --oneline -1))"

# --- 1. db_verify on an environment this arm's library created.
D=/home/admin/runs/verify-$ARM
mkdir -p "$D"; find "$D" -mindepth 1 -delete
PIN_HOME=$D PIN_CACHE_MB=512 PIN_TAG="verify-$ARM" \
  timeout 300 /home/admin/pin_bench-"$ARM" indiv 200000 16 0 2 4 \
  >"$D/bench.out" 2>&1
if ! grep -q '^RESULT ' "$D/bench.out"; then
  echo "VERDICT verify arm=$ARM: FAIL (bench produced no RESULT line)"; rc=1
else
  vout=$("$B/db_verify" -h "$D" bench.db 2>&1; echo "rc=$?")
  if echo "$vout" | grep -q 'rc=0'; then
    echo "VERDICT db_verify arm=$ARM: CLEAN (db_verify -h $D bench.db, rc=0, no error output)"
    echo "$vout" | grep -v '^rc=' | sed -n 's/^/    /p'
  else
    echo "VERDICT db_verify arm=$ARM: FAIL"; echo "$vout" | sed -n 's/^/    /p'; rc=1
  fi
fi

# --- 2. batch_diff differential (all three arms of the driver).
if [ ! -x "$B/batch_diff" ]; then
  ( cd "$B" && make batch_diff >/dev/null 2>&1 )
fi
if [ ! -x "$B/batch_diff" ]; then
  echo "VERDICT batch_diff arm=$ARM: FAIL (driver did not build)"; rc=1
else
  for bdarm in indiv batch both; do
    BD=/home/admin/runs/bd-$ARM-$bdarm
    mkdir -p "$BD"; find "$BD" -mindepth 1 -delete
    out=$BD/out.txt
    bd_rc=0
    ( cd "$BD" && BATCH_DIFF_HOME=$BD BATCH_DIFF_ARM=$bdarm \
        timeout 600 "$B/batch_diff" 4000 ) >"$out" 2>&1 || bd_rc=$?
    nv=$(grep -c '^VERDICT ' "$out" || true)
    if [ "$bd_rc" = 0 ] && grep -q '^PASS: 0 failure' "$out" && [ "$nv" -ge 4 ]
    then
      echo "VERDICT batch_diff arm=$ARM bdarm=$bdarm: PASS ($nv verdicts)"
      grep '^VERDICT\|^PASS' "$out" | sed -n 's/^/    /p'
    else
      echo "VERDICT batch_diff arm=$ARM bdarm=$bdarm: FAIL (exit=$bd_rc verdicts=$nv)"
      tail -25 "$out" | sed -n 's/^/    /p'
      rc=1
    fi
  done
fi

echo "VERDICT correctness arm=$ARM: $([ $rc = 0 ] && echo ALL-PASS || echo FAILED)"
exit $rc
