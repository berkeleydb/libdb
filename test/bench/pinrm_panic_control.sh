#!/bin/bash
# pinrm_panic_control.sh -- the panic seen when perf/bhpin-r1's optimistic path
# finally FIRES (after the harness checkpoint cleans the buffer pool) must be
# attributed with a control, not by inspection.  Run the SAME driver, same
# flags, same DB_PRIVATE env, against:
#   base  (v2026.09.6)  -- if this panics too, the harness is at fault
#   bhpin (R1)          -- if only this panics, R1 is at fault
# in both a production (-O2) and a --enable-diagnostic build.
set -u
mkbuild() {  # mkbuild <name> <ref> <configure-args...>
  local n=$1; shift
  local ref=$1; shift
  local WT=/home/admin/wt-$n
  local B=$WT/build_unix
  if [ ! -d "$WT" ]; then
    ( cd /home/admin/libdb && git worktree add --detach "$WT" "$ref" ) \
      >/dev/null 2>&1 || { echo "WORKTREE-FAIL $n"; return 1; }
    ( cd "$WT" && git checkout -f -- build_windows/ 2>/dev/null; true )
  fi
  if [ ! -f "$B/libdb.a" ] && [ ! -f "$B/.libs/libdb-2026.0.so" ]; then
    mkdir -p "$B"
    ( cd "$B" && ../dist/configure "$@" LIBS=-luring CFLAGS="-O2 -g" \
        >conf.log 2>&1 && make -j96 >make.log 2>&1 ) \
      || { echo "BUILD-FAIL $n"; return 1; }
  fi
  echo "$B"
}

run_one() {  # run_one <label> <builddir> <envkind>
  local label=$1 B=$2 envk=$3
  local bin=/home/admin/pb-$label
  cc -O2 -g -pthread -I"$B" /home/admin/pin_bench.c \
     $( [ -f "$B/libdb.a" ] && echo "$B/libdb.a" \
        || echo "-L$B/.libs -Wl,-rpath,$B/.libs -ldb-2026.0" ) \
     -lpthread -ldl -luring -o "$bin" 2>/dev/null
  [ -x "$bin" ] || { echo "PANICTEST label=$label: CC-FAIL"; return 1; }
  local D=/home/admin/runs/panic-$label-$envk
  mkdir -p "$D"; find "$D" -mindepth 1 -delete
  if [ "$envk" = private ]; then export PIN_PRIVATE=1; else unset PIN_PRIVATE; fi
  local out=$D/out.txt
  PIN_HOME=$D PIN_CACHE_MB=512 PIN_TAG="panic-$label" \
    timeout 300 "$bin" indiv 200000 16 1 4 32 >"$out" 2>&1
  local ec=$?
  local np
  np=$(grep -c 'PANIC' "$out" || true)
  local nr
  nr=$(grep -c '^RESULT ' "$out" || true)
  echo "PANICTEST label=$label env=$envk exit=$ec panic_lines=$np result_lines=$nr"
  [ "$np" != 0 ] && grep 'PANIC\|BDB2031\|already unlocked' "$out" | sort -u | head -4 | sed -n 's/^/    /p'
  find "$D" -mindepth 1 -delete
  return 0
}

BP=$(mkbuild basediag v2026.09.6 --enable-diagnostic --disable-shared) || exit 1
BR=$(mkbuild bhpindiag2 rm/bhpin-r1 --enable-diagnostic --disable-shared) || exit 1
for envk in private shared; do
  run_one basediag  "$BP" "$envk"
  run_one bhpindiag "$BR" "$envk"
  run_one baseprod  /home/admin/wt-base/build_unix  "$envk"
  run_one bhpinprod /home/admin/wt-bhpin/build_unix "$envk"
done
