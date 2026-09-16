#!/bin/bash
# pinrm_stress.sh -- run the DB_PRIVATE reader/writer stress against an arm,
# with the R1 kill switch both ways, plus db_verify on the resulting database.
# usage: pinrm_stress.sh <arm> [secs]
#
# Why this exists: test/c/batch_diff.c opens a SHARED environment, where R1's
# own counters report attempts=0, so batch_diff passing on the bhpin build says
# nothing about R1.  R1 is gated on ENV_PRIVATE, so a DB_PRIVATE stress is the
# only correctness evidence that touches the code under test.
set -u
ARM=${1:?arm}
SECS=${2:-20}
B=/home/admin/wt-$ARM/build_unix
cc -O2 -g -pthread -I"$B" /home/admin/pin_stress.c \
   -L"$B/.libs" -Wl,-rpath,"$B/.libs" -ldb-2026.0 -o /home/admin/pin_stress-$ARM \
   2>&1 | tail -5
[ -x /home/admin/pin_stress-$ARM ] || { echo "CC-FAIL $ARM"; exit 1; }
rc=0
for arm in on off; do
  # 42-char home so the DB_PRIVATE layout mode is the fast one, held constant.
  H=/home/admin/runs/stress-$ARM-$arm-private-btree
  mkdir -p "$H"; find "$H" -mindepth 1 -delete
  if [ "$arm" = off ]; then export DB_NO_BHPIN=1; else unset DB_NO_BHPIN; fi
  export PIN_PRIVATE=1
  out=$H/out.txt
  PIN_HOME=$H timeout 600 /home/admin/pin_stress-$ARM 5000 48 16 "$SECS" \
    >"$out" 2>&1
  ec=$?
  grep -E '^VERDICT|^FAIL|PANIC' "$out" | head -5
  if [ "$ec" != 0 ]; then echo "VERDICT stress arm=$ARM bhpin=$arm: FAIL exit=$ec"; rc=1; fi
  vout=$("$B/db_verify" -h "$H" stress.db 2>&1; echo "rc=$?")
  if echo "$vout" | grep -q 'rc=0'; then
    echo "VERDICT stress-db_verify arm=$ARM bhpin=$arm: CLEAN"
  else
    echo "VERDICT stress-db_verify arm=$ARM bhpin=$arm: FAIL"
    echo "$vout" | sed -n 's/^/    /p'
    rc=1
  fi
  find "$H" -mindepth 1 -delete
done
echo "VERDICT pinrm-stress arm=$ARM: $([ $rc = 0 ] && echo ALL-PASS || echo FAILED)"
exit $rc
