#!/usr/bin/env bash
# full_run4.sh -- authoritative full-suite coverage driver, portable rework of
# full_run3_combined.sh.
#
# Differences from full_run3_combined.sh (which was written for a 96-way EC2 box
# with a distro tcl):
#   * repo root is derived from the script location (was hard-coded $HOME/libdb)
#   * tcl lib dir + gcov are taken from the environment / nix dev shell
#   * PHASE 1 group launches are throttled to $COV_PAR concurrent groups so the
#     run is survivable on an 8-way box (uncapped fan-out on 8 cores makes every
#     group hit its timeout instead of finishing)
#   * PHASE 2 folds in the C drivers that report #3 recorded as a MEASUREMENT
#     GAP (recd_handlers) plus the new cov_* drivers, the faultinject sweep,
#     the PBT tier, the fuzz corpus replay and the DST scenarios
#   * SKIP_PHASE1=1 / SKIP_PHASE2=1 / PHASE2_ONLY_NEW=1 let a run add .gcda to
#     an existing accumulation instead of starting over, which is how the
#     per-batch coverage deltas in FULL-COVERAGE-REPORT-4.md were measured.
#
# Measurement only -- no engine code touched.
set -uo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
R="$(cd "$here/../.." && pwd)"
B="${COV_BUILD:-$R/build_unix}"
TCLBIN="${TCLBIN:-tclsh8.6}"
command -v "$TCLBIN" >/dev/null 2>&1 || TCLBIN=tclsh
TCLLIB="${TCLLIB:-}"
IGN="mismatch,source,gcov,unused,negative,empty,inconsistent,version,corrupt,range,count"
LCOV="${LCOV:-lcov}"
GCOV="${GCOV:-gcov}"
AM_CAP=${AM_CAP:-9000}; SUB_CAP=${SUB_CAP:-5400}; REP_CAP=${REP_CAP:-420}; LV_CAP=${LV_CAP:-2400}
COV_PAR=${COV_PAR:-8}
SKIP_PHASE1=${SKIP_PHASE1:-0}
SKIP_PHASE2=${SKIP_PHASE2:-0}
PHASE2_ONLY_NEW=${PHASE2_ONLY_NEW:-0}
SKIP_BUILD=${SKIP_BUILD:-0}
TAG=${TAG:-cov4}
export CC="${CC:-gcc}"
log(){ echo "[$(date -u +%H:%M:%S)] $*"; }
mkdir -p "$B"; cd "$B"
RESULTS=/tmp/$TAG-results.txt; [ "$SKIP_PHASE1$SKIP_PHASE2" = "00" ] && : > "$RESULTS"; touch "$RESULTS"
T_START=$(date +%s)

throttle(){ while [ "$(jobs -rp | wc -l)" -ge "$COV_PAR" ]; do sleep 2; done; }

# =========================================================================
# PHASE 0: configure + build ONCE with coverage instrumentation
# =========================================================================
if [ "$SKIP_BUILD" = 0 ]; then
log "=== PHASE 0: configure + build (--coverage) ==="
find "$B" \( -name '*.gcda' -o -name '*.gcno' \) -delete 2>/dev/null || true
rm -f "$B"/coverage*.info "$B"/cov*-src*.info "$B"/cov-src.info 2>/dev/null || true
#
# --enable-dst and --enable-faultinject are BOTH added here, on top of the
# --enable-debug --enable-test that report #3 used.  Rationale:
#
#   * The DST tier (test/sim, 41 scenarios) links against the sim core, which
#     that option compiles INTO libdb -- without it `make dst_tests` fails with
#     undefined __db_sim_* references, which is why no coverage run has ever
#     measured DST.
#   * The OOM sweep (test/c/cov_oom_paths.c) needs the __os_* allocation hook
#     that --enable-faultinject compiles in.
#
# Both are documented as additive and inert until armed (dist/configure.ac:
# "when off ... bit-for-bit the stock library"), and neither changes any
# measured branch unless a test arms it, so they widen what CAN be measured
# without distorting what IS measured.  Set COV_PLAIN=1 to reproduce report
# #3's exact configure line instead.
EXTRA_CONF="--enable-dst --enable-faultinject"
[ "${COV_PLAIN:-0}" = 1 ] && EXTRA_CONF=""
CC=$CC ../dist/configure --enable-debug --enable-test $EXTRA_CONF \
  ${TCLLIB:+--with-tcl="$TCLLIB"} \
  CFLAGS="-O0 -g --coverage" LDFLAGS="--coverage" >/tmp/$TAG-configure.log 2>&1 \
  || { echo "CONFIGURE FAILED"; tail -30 /tmp/$TAG-configure.log; exit 1; }
make -j"$(nproc)" >/tmp/$TAG-build.log 2>&1 \
  || { echo "BUILD FAILED"; tail -40 /tmp/$TAG-build.log; exit 1; }
log "  .gcno files: $(find . -name '*.gcno' | wc -l)"
fi

# --- build include.tcl template with ABSOLUTE source paths (.libs relative) ---
INC_TMPL=/tmp/$TAG-include.tcl
sed -e "s#^set src_root .*#set src_root $R#" \
    -e "s#^set test_path .*#set test_path $R/test/tcl#" \
    -e "s#^set je_root .*#set je_root $R/../je#" \
    -e "s#^set tcl_utils .*#set tcl_utils $R/test/tcl_utils#" \
    "$B/include.tcl" > "$INC_TMPL"

mkrundir(){ # dir  -- isolated rundir: symlinks .libs + util wrappers + include.tcl
  local rd="$1"; mkdir -p "$rd"
  ln -sfn "$B/.libs" "$rd/.libs"
  cp "$INC_TMPL" "$rd/include.tcl"
  for f in "$B"/db_* "$B"/berkdb_svc "$B"/test_micro; do
    bn=$(basename "$f")
    case "$bn" in *.o|*.d|*.lo|*.gcno|*.gcda|*.c|*.h) continue;; esac
    [ -f "$f" ] && [ -x "$f" ] && ln -sfn "$f" "$rd/$bn"
  done
}

run_group(){ # name cap tcl-body
  local name="$1" cap="$2" body="$3"
  local rd="$B/rd_$name" out="/tmp/$TAG-grp-$name.log"
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

if [ "$SKIP_PHASE1" = 0 ]; then
# =========================================================================
# PHASE 1: full Tcl suite (access methods + subsystems + rep + logverify + recd)
# =========================================================================
log "=== PHASE 1: access-method groups ==="
for m in btree hash recno queue heap; do throttle; run_group "am_$m" "$AM_CAP" "$(am_body $m)"; done

log "=== PHASE 1: subsystem groups ==="
for sub in env archive backup fop lock log memp mutex txn sdb byte rsrc dbm ndbm hsearch sindex sec compact partition compressed; do
  throttle; run_group "sub_$sub" "$SUB_CAP" "$(sub_body $sub)"
done
throttle; run_group "recd" "$SUB_CAP" "if {[catch {run_recds all 1 0} r]} { puts \"FAIL recd: \$r\" }; puts GROUP_DONE_recd"

log "=== PHASE 1: logverify ==="
throttle; run_group "logverify" "$LV_CAP" \
  "source $R/test/tcl/logverify001.tcl; source $R/test/tcl/logverify002.tcl; if {[catch {logverify001} r]} {puts \"FAIL logverify001: \$r\"}; if {[catch {logverify002} r]} {puts \"FAIL logverify002: \$r\"}; puts GROUP_DONE_logverify"

# replication: SKIP known hangers rep016 rep034 repmgr024 repmgr026.
REP_TESTS="rep001 rep002 rep003 rep005 rep006 rep007 rep008 rep009 rep010 rep011 rep012 rep013 rep014 rep015 rep019 rep020 rep021 rep022 rep023 rep024 rep025 rep026"
REPMGR_TESTS="repmgr009 repmgr010 repmgr011 repmgr012 repmgr013 repmgr017 repmgr018 repmgr023 repmgr025 repmgr027 repmgr030 repmgr031 repmgr032 repmgr033 repmgr034"
run_rep_one(){ # test baseport
  local t="$1" bp="$2"
  local rd="$B/rd_rep_$t" out="/tmp/$TAG-rep-$t.log"
  mkrundir "$rd"
  case "$t" in rep[0-9]*) call="$t btree" ;; *) call="$t" ;; esac
  printf 'source %s/test/tcl/test.tcl\nset testdir ./TESTDIR\nsource %s/test/tcl/reputils.tcl\nif {[catch {%s} r]} { puts "FAIL %s: $r"; exit 3 }\nputs "PASS %s"\n' \
    "$R" "$R" "$call" "$t" "$t" > "$rd/.run.tcl"
  ( cd "$rd"; BDBBASEPORT="$bp" timeout "$REP_CAP" "$TCLBIN" .run.tcl >"$out" 2>&1; rc=$?
    if [ $rc -eq 124 ]; then st="HANG"; elif [ $rc -eq 0 ] && grep -q "^PASS $t" "$out"; then st="PASS"; else st="FAIL(rc=$rc)"; fi
    log "REP-DONE $t: $st"; echo "rep_$t $st" >> "$RESULTS"
    find "$rd/TESTDIR" -mindepth 1 -delete 2>/dev/null || true ) &
}
log "=== PHASE 1: rep0NN in-process tests ==="
for t in $REP_TESTS; do throttle; run_rep_one "$t" 30100; done
log "=== PHASE 1: repmgrNN socket tests (distinct base ports) ==="
i=0
for t in $REPMGR_TESTS; do
  bp=$((31000 + i*200)); i=$((i+1)); throttle; run_rep_one "$t" "$bp"
done
log "=== PHASE 1: waiting ==="
wait
log "=== PHASE 1 COMPLETE. gcda=$(find .libs -name '*.gcda' | wc -l) ==="
fi

if [ "$SKIP_PHASE2" = 0 ]; then
# =========================================================================
# PHASE 2: COV_* blocks -- curated subset + all C drivers (adds to .gcda)
# =========================================================================
run_driver(){ # name script [env...]
  local name="$1"; shift
  local scr="$1"; shift
  if [ ! -f "$scr" ]; then echo "SKIP $name (no $scr)"; echo "driver_$name SKIP" >> "$RESULTS"; return 0; fi
  if env "$@" sh "$scr" >/tmp/$TAG-$name.log 2>&1; then echo "PASS $name"; echo "driver_$name PASS" >> "$RESULTS"
  else echo "FAIL $name (rc=$?)"; echo "driver_$name FAIL" >> "$RESULTS"; tail -5 /tmp/$TAG-$name.log; fi
}

if [ "$PHASE2_ONLY_NEW" = 0 ]; then
log "=== PHASE 2: curated COV_TESTS subset (crypto/stat/mvcc/lock/compression) ==="
COV_TESTS="lock001: txn001: ssi001: ssi002: env007: lock007: \
  btree/test001 btree/test111 test143:btree \
  hash/test001 hash/test006 hash/test010 hash/test025 hash/test077 \
  queue/test001 queue/test007 queue/test025 \
  recno/test001 recno/test006 recno/test024 recno/test025 \
  heap/test001 heap/test013 heap/test024 \
  run_range_partition@test001@btree run_partition_callback@test001@btree \
  logverify001: logverify002: env020: statprint001: mvcc001: sec001: sec002:"
subsetrd="$B/rd_covsubset"; mkrundir "$subsetrd"
runtcl="$subsetrd/.cov-run.tcl"
{
  echo "source $R/test/tcl/test.tcl"
  echo 'set testdir ./TESTDIR'
  for pair in $COV_TESTS; do
    case "$pair" in
    *@*) p="${pair%%@*}"; rest="${pair#*@}"; t="${rest%%@*}"; m="${rest#*@}"
      printf 'source %s/test/tcl/%s.tcl\n' "$R" "$t"
      printf 'if {[catch {%s %s %s 0 1} res]} { puts "FAIL %s/%s/%s: $res" }\n' "$p" "$t" "$m" "$p" "$t" "$m"
      printf 'puts "PASS %s/%s/%s"\n' "$p" "$t" "$m" ;;
    */*) m="${pair%%/*}"; t="${pair#*/}"
      printf 'source %s/test/tcl/%s.tcl\n' "$R" "$t"
      printf 'if {[catch {run_method %s %s 0 1} res]} { puts "FAIL %s/%s: $res" }\n' "$m" "$t" "$m" "$t"
      printf 'puts "PASS %s/%s"\n' "$m" "$t" ;;
    *) t="${pair%%:*}"; a="${pair#*:}"
      printf 'source %s/test/tcl/%s.tcl\n' "$R" "$t"
      printf 'if {[catch {eval %s %s} res]} { puts "FAIL %s: $res" }\n' "$t" "$a" "$t"
      printf 'puts "PASS %s"\n' "$t" ;;
    esac
  done
} > "$runtcl"
( cd "$subsetrd"; timeout 4800 "$TCLBIN" .cov-run.tcl >/tmp/$TAG-subset.log 2>&1
  grep -cE '^FAIL' /tmp/$TAG-subset.log | xargs -I{} echo "covsubset faillines={}" >> "$RESULTS"
  find "$subsetrd/TESTDIR" -mindepth 1 -delete 2>/dev/null || true )
log "  subset done: $(grep -cE '^PASS' /tmp/$TAG-subset.log) pass / $(grep -cE '^FAIL' /tmp/$TAG-subset.log) fail"

log "=== PHASE 2: C drivers (XA + upgrade + os_aio + backup + recd_compact + recd_handlers) ==="
run_driver xa            "$R/test/xa/run_xa_direct.sh"
run_driver db_upgrade    "$R/test/db/run_upgrade.sh"
run_driver os_aio        "$R/test/os/run_os_aio.sh"
run_driver backup        "$R/test/backup/run_backup_direct.sh"
run_driver recd_compact  "$R/test/db/run_recd_compact.sh"
# report #3 recorded recd_handlers as a MEASUREMENT GAP -- it was never in the
# combined driver even though the code shipped.  Folded in here.
run_driver recd_handlers "$R/test/db/run_recd_handlers.sh"
log "  .gcda after C drivers: $(find .libs -name '*.gcda' | wc -l)"

# COV_DEAD_REG: deadlock detector + DB_REGISTER (driver-per-test)
log "=== PHASE 2: deadlock + DB_REGISTER drivers ==="
DR_CAP=420
dead_reg=( "dead001:dead001 {2 4}" "dead002:dead002 {2 4}" "dead003:dead003 {2 4}" \
  "dead004:dead004" "dead005:dead005 {4}" "dead006:dead006 {2 4}" "env012:env012" )
drrd="$B/rd_deadreg"; mkrundir "$drrd"; dregtcl="$drrd/.cov-deadreg.tcl"
for spec in "${dead_reg[@]}"; do
  name="${spec%%:*}"; call="${spec#*:}"
  printf 'source %s/test/tcl/test.tcl\nset testdir ./TESTDIR\nif {[catch {%s} r]} { puts "FAIL %s: $r"; exit 3 }\nputs "PASS %s"\n' \
    "$R" "$call" "$name" "$name" > "$dregtcl"
  ( cd "$drrd"; timeout "$DR_CAP" "$TCLBIN" .cov-deadreg.tcl >/tmp/$TAG-dreg-$name.log 2>&1; rc=$?
    pkill -f 'wrap.tcl' 2>/dev/null||true; pkill -f 'ddscript' 2>/dev/null||true
    pkill -f 'envscript' 2>/dev/null||true; pkill -f 'db_deadlock' 2>/dev/null||true
    if [ $rc -eq 124 ]; then echo "HANG $name"; echo "dreg_$name HANG">>"$RESULTS"
    elif [ $rc -eq 0 ] && grep -q "^PASS $name" /tmp/$TAG-dreg-$name.log; then echo "PASS $name"; echo "dreg_$name PASS">>"$RESULTS"
    else echo "FAIL $name (rc=$rc)"; echo "dreg_$name FAIL">>"$RESULTS"; fi
    find "$drrd/TESTDIR" -mindepth 1 -delete 2>/dev/null || true )
done

# COV_RECD extra curated recd tests (driver-per-test) -- adds redo/undo branches
log "=== PHASE 2: recd recovery-record handler tests ==="
RECD_CAP=420
recd_tests=( "recd002:btree:0" "recd002:hash:0" "recd002:queue:0" "recd002:recno:0" \
  "recd004:btree:" "recd005:btree:" "recd005:hash:" "recd005:queue:" "recd005:recno:" \
  "recd006:btree:" "recd006:hash:" "recd008:btree:" "recd009:btree:" "recd010:btree:" \
  "recd013:btree:" "recd013:hash:" "recd014:queueext:" "recd016:btree:" "recd017:btree:" \
  "recd018:btree:" "recd019:btree:" "recd020:btree:" "recd022:btree:" "recd023:btree:" \
  "recd024:btree:" "recd025:btree:" )
recdrd="$B/rd_recdcov"; mkrundir "$recdrd"; recdtcl="$recdrd/.cov-recd.tcl"
for spec in "${recd_tests[@]}"; do
  t="${spec%%:*}"; rest="${spec#*:}"; m="${rest%%:*}"; a="${rest#*:}"
  printf 'source %s/test/tcl/test.tcl\nset testdir ./TESTDIR\nsource %s/test/tcl/%s.tcl\nif {[catch {eval %s %s %s} r]} { puts "FAIL %s %s: $r"; exit 3 }\nputs "PASS %s %s"\n' \
    "$R" "$R" "$t" "$t" "$m" "$a" "$t" "$m" "$t" "$m" > "$recdtcl"
  ( cd "$recdrd"; timeout "$RECD_CAP" "$TCLBIN" .cov-recd.tcl >/tmp/$TAG-recd-$t-$m.log 2>&1; rc=$?
    pkill -f 'recdscript' 2>/dev/null || true
    if [ $rc -eq 124 ]; then echo "HANG $t $m"; echo "recd_${t}_$m HANG">>"$RESULTS"
    elif [ $rc -eq 0 ] && grep -q "^PASS $t $m" /tmp/$TAG-recd-$t-$m.log; then echo "PASS $t $m"; echo "recd_${t}_$m PASS">>"$RESULTS"
    else echo "FAIL $t $m (rc=$rc)"; echo "recd_${t}_$m FAIL">>"$RESULTS"; fi
    find "$recdrd/TESTDIR" -mindepth 1 -delete 2>/dev/null || true )
done
fi   # PHASE2_ONLY_NEW

# ---------------------------------------------------------------------------
# PHASE 2b: the tiers report #3 never measured -- new in run #4.
#
# Each of these is a MEASUREMENT GAP closer as much as a new test: the tier
# already existed and passed, but no coverage driver ever ran it, so what it
# covers has always been reported cold.  They are ordered cheapest-first so a
# time-boxed run still gets the high-value ones.
# ---------------------------------------------------------------------------
log "=== PHASE 2b: new coverage C drivers + never-measured tiers ==="
# Deterministic, seconds each: the API/config/query surface drivers.
run_driver cov_api_surface  "$R/test/c/run_cov_api_surface.sh"
run_driver cov_rep_api      "$R/test/c/run_cov_rep_api.sh"
# One rich log -> both the *_autop.c printers and log_verify_int.c's verifiers.
run_driver cov_logrec_print "$R/test/c/run_cov_logrec_print.sh"
# cutest: 12 existing suites incl. TestChannel (3 live repmgr sites).
run_driver cov_cutest       "$R/test/c/run_cov_cutest.sh"
# The committed fuzz corpus + crash seeds, replayed against THIS build.
run_driver cov_fuzz_corpus  "$R/test/c/run_cov_fuzz_corpus.sh"
# The 41 DST scenarios (crash/torn/ENOSPC/clock-skew/crash-in-recovery).
run_driver cov_dst          "$R/test/c/run_cov_dst.sh"
# The OOM error-path sweep.  Last: it is the longest, and it is the one that
# needs --enable-faultinject (it SKIPs cleanly without it).
run_driver cov_oom_paths    "$R/test/c/run_cov_oom_paths.sh"

log "=== PHASE 2 COMPLETE. gcda=$(find .libs -name '*.gcda' | wc -l) ==="
fi

# =========================================================================
# PHASE 3: single lcov capture from .libs, merge everything
# =========================================================================
log "=== PHASE 3: lcov capture from .libs ==="
$LCOV --capture --directory .libs --output-file coverage4.info --gcov-tool "$GCOV" \
  --rc geninfo_unexecuted_blocks=1 --branch-coverage --ignore-errors "$IGN" >/tmp/$TAG-lcov.log 2>&1 \
  || { echo "LCOV CAPTURE FAILED"; tail -30 /tmp/$TAG-lcov.log; exit 1; }
$LCOV --extract coverage4.info "*/src/*" --output-file cov4-src-all.info --branch-coverage --ignore-errors "$IGN" >/dev/null 2>&1
$LCOV --remove cov4-src-all.info "*/dbinc_auto/*" --output-file cov-src.info --branch-coverage --ignore-errors "$IGN" >/dev/null 2>&1
log "=== SUMMARY ==="; $LCOV --summary cov-src.info --branch-coverage --ignore-errors "$IGN" 2>&1 | tee /tmp/$TAG-summary.txt
log "ranking"; python3 "$here/rank_coverage.py" cov-src.info > /tmp/$TAG-ranking.txt 2>&1
log "subsystems"; python3 "$here/subsystem_breakdown.py" cov-src.info > /tmp/$TAG-subsystems.txt 2>&1
cp cov-src.info /tmp/$TAG-cov-src.info
T_END=$(date +%s)
echo "WALL_SECONDS=$((T_END-T_START))" | tee /tmp/$TAG-wall.txt
echo "=== TOP 30 COLD FILES ==="; head -31 /tmp/$TAG-ranking.txt
echo "=== SUBSYSTEMS ==="; cat /tmp/$TAG-subsystems.txt
echo "=== RESULTS ==="; sort "$RESULTS"
log "ALL DONE"
