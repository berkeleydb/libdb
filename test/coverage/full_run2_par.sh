#!/usr/bin/env bash
# full_run2_par.sh -- PARALLEL full-suite coverage run (rep + logverify included).
# Each group runs in its OWN rundir (isolated ./TESTDIR + util wrappers), sourcing
# the harness by absolute path; gcov merges .gcda into the shared .libs/*.gcno
# (gcno carries absolute paths, so merge works from any cwd). Measurement only.
set -uo pipefail
R="$HOME/libdb"; B="$R/build_unix"; here="$R/test/coverage"
TCLBIN=tclsh8.6
IGN="mismatch,source,gcov,unused,negative,empty,inconsistent,version"
AM_CAP=${AM_CAP:-9000}; SUB_CAP=${SUB_CAP:-5400}; REP_CAP=${REP_CAP:-360}; LV_CAP=${LV_CAP:-2400}
log(){ echo "[$(date -u +%H:%M:%S)] $*"; }
cd "$B"
RESULTS=/tmp/cov2-results.txt; : > "$RESULTS"
DONE=/tmp/cov2-done; rm -f "$DONE"

# --- build the include.tcl template with ABSOLUTE source paths (.libs relative) ---
INC_TMPL=/tmp/cov2-include.tcl
sed -e "s#^set src_root .*#set src_root $R#" \
    -e "s#^set test_path .*#set test_path $R/test/tcl#" \
    -e "s#^set je_root .*#set je_root $R/../je#" \
    -e "s#^set tcl_utils .*#set tcl_utils $R/test/tcl_utils#" \
    "$B/include.tcl" > "$INC_TMPL"

# make one isolated rundir: symlinks .libs + util wrappers + include.tcl
mkrundir(){ # dir
  local rd="$1"; mkdir -p "$rd"
  ln -sfn "$B/.libs" "$rd/.libs"
  cp "$INC_TMPL" "$rd/include.tcl"
  # executable util wrappers (skip build artifacts)
  for f in "$B"/db_* "$B"/berkdb_svc "$B"/test_micro; do
    bn=$(basename "$f")
    case "$bn" in *.o|*.d|*.lo|*.gcno|*.gcda|*.c|*.h) continue;; esac
    [ -f "$f" ] && [ -x "$f" ] && ln -sfn "$f" "$rd/$bn"
  done
}

run_group(){ # name cap tcl-body
  local name="$1" cap="$2" body="$3"
  local rd="$B/rd_$name" out="/tmp/cov2-grp-$name.log"
  mkrundir "$rd"
  { echo "source $R/test/tcl/test.tcl"; echo "set testdir ./TESTDIR"; echo "$body"; } > "$rd/.run.tcl"
  ( cd "$rd"; t0=$(date +%s); timeout "$cap" "$TCLBIN" .run.tcl >"$out" 2>&1; rc=$?; t1=$(date +%s)
    fl=$(grep -c '^FAIL' "$out" 2>/dev/null || echo 0)
    log "GRP-DONE $name rc=$rc dur=$((t1-t0))s FAILlines=$fl"
    echo "$name rc=$rc dur=$((t1-t0)) faillines=$fl" >> "$RESULTS"
    find "$rd/TESTDIR" -mindepth 1 -delete 2>/dev/null || true ) &
}

am_body(){ cat <<EOF
foreach t \$test_names(test) {
  if {[catch {eval run_method $1 \$t 0 1 stdout} res]} { puts "FAIL $1/\$t: \$res" }
}
puts "GROUP_DONE_$1"
EOF
}
sub_body(){ echo "if {[catch {r $1} res]} { puts \"FAIL sub_$1: \$res\" }; puts \"GROUP_DONE_$1\""; }

log "=== launch access-method groups (parallel) ==="
for m in btree hash recno queue heap; do run_group "am_$m" "$AM_CAP" "$(am_body $m)"; done

log "=== launch subsystem groups (parallel) ==="
for sub in env archive backup fop lock log memp mutex txn sdb byte rsrc dbm ndbm hsearch sindex sec compact partition compressed; do
  run_group "sub_$sub" "$SUB_CAP" "$(sub_body $sub)"
done
run_group "recd" "$SUB_CAP" "if {[catch {run_recds all 1 0} r]} { puts \"FAIL recd: \$r\" }; puts GROUP_DONE_recd"

log "=== launch logverify (parallel) ==="
run_group "logverify" "$LV_CAP" \
  "source $R/test/tcl/logverify001.tcl; source $R/test/tcl/logverify002.tcl; if {[catch {logverify001} r]} {puts \"FAIL logverify001: \$r\"}; if {[catch {logverify002} r]} {puts \"FAIL logverify002: \$r\"}; puts GROUP_DONE_logverify"

# --- replication: rep0NN (in-process) + repmgrNN (own BDBBASEPORT), each own rundir ---
# SKIP known hangers: rep016 rep034 repmgr024 repmgr026.
REP_TESTS="rep001 rep002 rep003 rep005 rep006 rep007 rep008 rep009 rep010 rep011 rep012 rep013 rep014 rep015 rep019 rep020 rep021 rep022 rep023 rep024 rep025 rep026"
REPMGR_TESTS="repmgr009 repmgr010 repmgr011 repmgr012 repmgr013 repmgr017 repmgr018 repmgr023 repmgr025 repmgr027 repmgr030 repmgr031 repmgr032 repmgr033 repmgr034"

run_rep_one(){ # test baseport
  local t="$1" bp="$2"
  local rd="$B/rd_rep_$t" out="/tmp/cov2-rep-$t.log"
  mkrundir "$rd"
  case "$t" in rep[0-9]*) call="$t btree" ;; *) call="$t" ;; esac
  printf 'source %s/test/tcl/test.tcl\nset testdir ./TESTDIR\nsource %s/test/tcl/reputils.tcl\nif {[catch {%s} r]} { puts "FAIL %s: $r"; exit 3 }\nputs "PASS %s"\n' \
    "$R" "$R" "$call" "$t" "$t" > "$rd/.run.tcl"
  ( cd "$rd"; BDBBASEPORT="$bp" timeout "$REP_CAP" "$TCLBIN" .run.tcl >"$out" 2>&1; rc=$?
    if [ $rc -eq 124 ]; then st="HANG"; elif [ $rc -eq 0 ] && grep -q "^PASS $t" "$out"; then st="PASS"; else st="FAIL(rc=$rc)"; fi
    log "REP-DONE $t: $st"; echo "rep_$t $st" >> "$RESULTS"
    find "$rd/TESTDIR" -mindepth 1 -delete 2>/dev/null || true ) &
}

log "=== launch rep0NN in-process tests (parallel, all) ==="
for t in $REP_TESTS; do run_rep_one "$t" 30100; done
log "=== launch repmgrNN socket tests (parallel, distinct base ports) ==="
i=0
for t in $REPMGR_TESTS; do
  bp=$((31000 + i*200)); i=$((i+1)); run_rep_one "$t" "$bp"
  while [ "$(jobs -rp | wc -l)" -ge 48 ]; do sleep 2; done
done

log "=== all groups launched; waiting ==="
wait
log "=== ALL COMPLETE. gcda=$(find .libs -name '*.gcda' | wc -l) ==="

log "lcov capture from .libs"
lcov --capture --directory .libs --output-file coverage2.info --gcov-tool gcov \
  --rc geninfo_unexecuted_blocks=1 --branch-coverage --ignore-errors "$IGN" >/tmp/cov2-lcov.log 2>&1 \
  || { echo "LCOV CAPTURE FAILED"; tail -30 /tmp/cov2-lcov.log; exit 1; }
lcov --extract coverage2.info "*/src/*" --output-file cov2-src-all.info --branch-coverage --ignore-errors "$IGN" >/dev/null 2>&1
lcov --remove cov2-src-all.info "*/dbinc_auto/*" --output-file cov-src.info --branch-coverage --ignore-errors "$IGN" >/dev/null 2>&1
log "summary"; lcov --summary cov-src.info --branch-coverage --ignore-errors "$IGN" 2>&1 | tee /tmp/cov2-summary.txt
log "ranking"; python3 "$here/rank_coverage.py" cov-src.info > /tmp/cov2-ranking.txt 2>&1
log "subsystems"; python3 "$here/subsystem_breakdown.py" cov-src.info > /tmp/cov2-subsystems.txt 2>&1
cp cov-src.info /tmp/cov2-cov-src.info
head -30 /tmp/cov2-ranking.txt
echo "=== subsystems ==="; cat /tmp/cov2-subsystems.txt
echo "=== results ==="; sort "$RESULTS"
touch "$DONE"; log "DONE"
