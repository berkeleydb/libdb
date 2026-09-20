#!/bin/sh
# test/coverage/run_coverage_parallel.sh -- the SAME coverage measurement as
# run_coverage.sh, with the independent test blocks run CONCURRENTLY and their
# .gcda counts merged, instead of one after another.
#
# THE PROBLEM THIS SOLVES
#
# docs/design/testing-program-2026-09.md: "The Coverage workflow is 34 minutes
# in a single job -- the longest thing in CI and entirely serial."  Measured on
# an idle 32-vCPU box, that splits roughly as
#
#	configure + build       ~5 min   (already -j parallel)
#	tcl subset              ~6 min
#	recd handlers          ~10 min   <- the single biggest block
#	deadlock + register     ~4 min
#	cov_* C drivers         ~4 min
#	xa/upgrade/backup       ~2 min
#	lcov capture + genhtml  ~1 min
#
# see build_unix/coverage-phases.txt, which run_coverage.sh now writes.
#
# WHY THE BLOCKS CANNOT SIMPLY BE BACKGROUNDED IN ONE TREE
#
# Every Tcl-side block uses THE SAME SCRATCH DIRECTORY -- build_unix/TESTDIR --
# and three of them explicitly empty it between tests:
#
#	find TESTDIR -mindepth 1 -delete
#
# (run_coverage.sh, the replication, deadlock/register and recd loops).  Two
# blocks running concurrently in one tree would delete each other's databases
# mid-test.  That is the "tiers serialised only because they share a fixed
# scratch path" case, and it is the reason the serial script is serial.
#
# It is NOT cheaply fixable by parameterising the path: TESTDIR is baked into
# test/tcl/test.tcl and the ~135 Tcl tests that source it, the recd tests spawn
# recdscript.tcl subprocesses that compute it independently, and the C drivers
# hard-code their own home directories.  Changing that is a large, risky edit to
# the thing being measured.
#
# WHAT THIS DOES INSTEAD
#
# One BUILD, several RUN trees.  The instrumented build happens once; each
# concurrent group then runs in its own directory with its own TESTDIR and its
# own .gcda files, and the counts are merged with `lcov -a` at the end.  gcov
# arc counts are additive across runs of the same binary, which is exactly what
# lcov -a does -- so the merged result covers the union of what the groups
# covered, and nothing is traded away for the speed.
#
# COVERAGE IS NOT TRADED FOR SPEED, AND THAT IS CHECKED
#
# The merged branch total is compared against the serial baseline by the ratchet
# (test/coverage/ratchet.sh) exactly as the serial run is.  If parallelising lost
# coverage, the ratchet fails -- the speed work cannot quietly buy time with
# coverage.  Measured results are in docs/design/testing-program-improvements.md.
#
# Usage:
#	test/coverage/run_coverage_parallel.sh
#	COVP_GROUPS="tcl recd cdrivers" test/coverage/run_coverage_parallel.sh
#
# Env:
#	COVP_JOBS	build parallelism (default: all cores)
#	COVP_GROUPS	which groups to run (default: all five)
#	COVP_KEEP=1	keep the per-group run trees for inspection
#	TCL_LIB		as run_coverage.sh

set -eu

here="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
root="$(CDPATH= cd -- "$here/../.." && pwd)"
bld="$root/build_unix"

: "${COVP_JOBS:=$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 4)}"
# The five groups, chosen so the longest (recd, ~10 min) starts first and runs
# alongside everything else -- the critical path is then recd, not the sum.
# MEASURED group wall times from the first full parallel run:
#
#	deadreg    1344s   <- the critical path, on its own
#	recd        439s
#	tcl          93s
#	cdrivers     15s
#	misc          ~5s
#
# deadreg alone was longer than everything else put together, so five groups
# could not beat ~22 minutes no matter how they were scheduled.  It is therefore
# SPLIT: the eight dead/env tests are independent driver-per-test runs (each gets
# its own tclsh and empties TESTDIR afterwards), so they split cleanly into two
# halves that run concurrently.  deadreg1 takes dead001-003, which are the slow
# multi-process lock-cycle tests; deadreg2 takes the rest.
#
# recd is split for the same reason, into the four access methods' worth of
# recd002 plus everything else.
: "${COVP_GROUPS:=deadreg1 deadreg2 recd1 recd2 tcl cdrivers misc}"
: "${TCLSH:=tclsh}"
: "${TCL_LIB:=}"
if [ -z "$TCL_LIB" ]; then
  for d in /nix/store/*tcl-8.6*/lib /usr/lib/tcl8.6 /usr/lib; do
    [ -d "$d" ] && { TCL_LIB="$d"; break; }
  done
fi
export CC=gcc
GCOV="${GCOV:-gcov}"
LCOV=lcov
GENHTML=genhtml
command -v lcov >/dev/null 2>&1 || {
  echo "error: lcov not found on PATH" >&2; exit 1; }

# Same two-dialect handling as run_coverage.sh: lcov 1.x rejects
# --branch-coverage and knows only four --ignore-errors classes, and both
# failures abort AFTER the tests have run.  See run_coverage.sh for the detail.
if "$LCOV" --help 2>&1 | grep -q -- '--branch-coverage'; then
  BRCOV="--branch-coverage"
else
  BRCOV="--rc lcov_branch_coverage=1"
fi
lcov_ign_help=$("$LCOV" --help 2>&1 | sed -n 's/.*--ignore-errors[^(]*(\([^)]*\)).*/\1/p' | head -1)
IGN=""
for c in mismatch source gcov unused negative empty inconsistent version; do
  if [ -z "$lcov_ign_help" ] || printf '%s' "$lcov_ign_help" | grep -q "$c"; then
    IGN="${IGN:+$IGN,}$c"
  fi
done
: "${IGN:=gcov,source}"

T0=$(date +%s)
echo "== libdb coverage (PARALLEL) =="
echo "  repo:    $root"
echo "  groups:  $COVP_GROUPS"
echo "  build:   -j$COVP_JOBS"
echo "  lcov:    $("$LCOV" --version 2>&1 | head -1)"
echo "           branch=$BRCOV ignore=$IGN"
echo

# ---------------------------------------------------------------------------
# 1. ONE instrumented build.
# ---------------------------------------------------------------------------
echo "== configure + build (once, -j$COVP_JOBS) =="
find "$bld" \( -name '*.gcda' -o -name '*.gcno' \) -delete 2>/dev/null || true
cd "$bld"
CC=gcc ../dist/configure --enable-test --with-tcl="$TCL_LIB" \
  --enable-faultinject CFLAGS="-O0 -g --coverage" LDFLAGS="--coverage" \
  >/tmp/covp-configure.log 2>&1 \
  || { echo "configure failed:"; tail -30 /tmp/covp-configure.log; exit 1; }
make -j"$COVP_JOBS" >/tmp/covp-build.log 2>&1 \
  || { echo "build failed:"; tail -40 /tmp/covp-build.log; exit 1; }
T_BUILD=$(date +%s)
echo "  .gcno files: $(find . -name '*.gcno' | wc -l), build $((T_BUILD - T0))s"

# HOW THE GROUPS ARE SEPARATED: GCOV_PREFIX.
#
# GCOV_PREFIX makes an instrumented binary write its .gcda under a prefix
# directory, mirroring the original absolute path (GCOV_PREFIX_STRIP=0 keeps the
# whole path).  So each group accumulates its own arc counts with no change to
# the build and, crucially, no change to TESTDIR's hard-coded name.
#
# ONE WRINKLE, MEASURED RATHER THAN ASSUMED: lcov needs each .gcno GRAPH file
# next to the .gcda it describes, and the .gcno stay in the build tree where the
# compiler wrote them.  Capturing a bare prefix directory therefore yields an
# EMPTY .info -- which would merge as "this group covered nothing" and quietly
# understate the total while every command still exited 0.  That was reproduced
# on a two-branch test program before this script was trusted: capture returned
# no summary lines at all.
#
# So mirror_gcno() symlinks every .gcno into the group's prefix at the mirrored
# path before capture.  With that in place the same two-branch experiment gives
#
#	arm 1 alone   3 of 6 branches (50%)
#	arm 2 alone   3 of 6 branches (50%)
#	lcov -a both  6 of 6 branches (100%)
#	serial run    6 of 6 branches (100%)   <- identical
#
# i.e. merging per-group captures equals a single serial run, which is the claim
# this script depends on.
#
# mirror_gcno PREFIX -- link the build tree's .gcno into PREFIX's mirror.
mirror_gcno() {
  find "$bld" -name '*.gcno' | while IFS= read -r f; do
    t="$1$f"
    d=$(dirname "$t")
    mkdir -p "$d" 2>/dev/null || continue
    ln -sf "$f" "$t" 2>/dev/null || true
  done
}

# ---------------------------------------------------------------------------
# 2. The groups.  Each is a shell function run in its OWN directory, with its
#    own TESTDIR and its own GCOV_PREFIX.
# ---------------------------------------------------------------------------
TCLBIN="$TCLSH"; command -v tclsh8.6 >/dev/null 2>&1 && TCLBIN=tclsh8.6

# The same test sets the serial script uses, so the union is the same measurement.
TCL_SUBSET="lock001: txn001: ssi001: ssi002: env007: lock007: \
  btree/test001 btree/test111 test143:btree \
  hash/test001 hash/test006 hash/test010 hash/test025 hash/test077 \
  queue/test001 queue/test007 queue/test025 \
  recno/test001 recno/test006 recno/test024 recno/test025 \
  heap/test001 heap/test013 heap/test024 \
  run_range_partition@test001@btree run_partition_callback@test001@btree \
  logverify001: logverify002: env020: statprint001: mvcc001: sec001: sec002:"

# The recd set, split in two halves of comparable length.  Same tests as the
# serial script's COV_RECD list, in the same forms.
RECD1_TESTS="recd002:btree:0 recd002:hash:0 recd002:queue:0 recd002:recno:0
recd004:btree: recd005:btree: recd005:hash: recd005:queue: recd005:recno:
recd006:btree: recd006:hash: recd008:btree:"
RECD2_TESTS="recd009:btree: recd010:btree: recd013:btree: recd013:hash:
recd014:queueext: recd016:btree: recd017:btree: recd018:btree: recd019:btree:
recd020:btree: recd022:btree: recd023:btree: recd024:btree: recd025:btree:"

# The deadlock/register set, split so the critical path is halved.  dead001-003
# are the slow multi-process lock-cycle tests.
DEADREG1_TESTS="dead001:dead001 {2 4}
dead002:dead002 {2 4}
dead003:dead003 {2 4}"
DEADREG2_TESTS="dead004:dead004
dead005:dead005 {4}
dead006:dead006 {2 4}
env007:env007
env012:env012"

# group_dir GROUP -- a fresh run directory for GROUP, echoed on stdout.
#
# The Tcl harness is cwd-relative in two ways that both have to be satisfied for
# a group directory to work at all:
#
#   test/tcl/test.tcl line 7 does `source ./include.tcl`, and include.tcl is
#   GENERATED INTO build_unix by dist/s_test -- so a group running in a
#   subdirectory cannot find it.  Measured: four of five groups produced ZERO
#   .gcda and the run aborted, all with "couldn't read file ./include.tcl".
#
#   include.tcl itself then names `.libs/libdb_tcl-*.so` and `../dist/..`
#   relatively, so those have to resolve from the group directory too.
#
# Symlinks rather than copies: the .so is large, and a copy would also have to be
# kept in step with a rebuild.
group_dir() {
  d="$bld/covp-$1"
  if [ -d "$d" ]; then find "$d" -mindepth 1 -delete; else mkdir -p "$d"; fi
  mkdir -p "$d/TESTDIR" "$d/.libs"
  # include.tcl is generated with paths relative to build_unix ("../dist/..",
  # "../dist/../test/tcl"), which do not resolve one level deeper.  Symlinking it
  # gets past `source ./include.tcl` and then fails on
  # "couldn't read file ../dist/../test/tcl/testutils.tcl".  So REWRITE those
  # roots to absolute paths for the group copy; everything else is carried
  # through verbatim, including the tcllib path, which is relative to the cwd and
  # is satisfied by the .libs symlinks below.
  if [ -f "$bld/include.tcl" ]; then
    sed -e "s,^set src_root .*,set src_root $root," \
        -e "s,^set test_path .*,set test_path $root/test/tcl," \
        -e "s,^set tcl_utils .*,set tcl_utils $root/test/tcl_utils," \
        -e "s,^set je_root .*,set je_root $root/../je," \
        "$bld/include.tcl" > "$d/include.tcl"
  fi
  for so in "$bld"/.libs/libdb_tcl-*.so "$bld"/.libs/libdb-*.so; do
    [ -e "$so" ] && ln -sf "$so" "$d/.libs/$(basename "$so")"
  done
  # The Tcl tests also expect the utilities (db_dump, db_verify, ...) and
  # DB_CONFIG-adjacent paths at the cwd, as they are in build_unix.
  for u in "$bld"/db_* "$bld"/libtool; do
    [ -e "$u" ] && [ ! -d "$u" ] && ln -sf "$u" "$d/$(basename "$u")"
  done
  echo "$d"
}

# run_group GROUP -- the body of one concurrent group.  Runs in a subshell so
# cd and the exported GCOV_PREFIX cannot leak between groups.
run_group() {
  g=$1
  d=$(group_dir "$g")
  log="/tmp/covp-$g.log"
  # Each group's .gcda go under its own prefix, so concurrent groups cannot
  # interleave writes to the same file.  This is what makes the runs
  # independent WITHOUT touching TESTDIR's hard-coded name.
  GCOV_PREFIX="$d/gcda"
  mkdir -p "$GCOV_PREFIX"
  export GCOV_PREFIX
  export GCOV_PREFIX_STRIP=0
  cd "$d"
  t0=$(date +%s)

  case "$g" in
  tcl)
    # One tclsh for the whole curated subset, as the serial script does.
    rt="$d/run.tcl"
    {
      echo "source $root/test/tcl/test.tcl"
      for pair in $TCL_SUBSET; do
        case "$pair" in
        *@*) p="${pair%%@*}"; rest="${pair#*@}"; t="${rest%%@*}"; m="${rest#*@}"
             printf 'source %s/test/tcl/%s.tcl\n' "$root" "$t"
             printf 'if {[catch {%s %s %s 0 1} r]} { puts "FAIL %s/%s" }\n' \
               "$p" "$t" "$m" "$p" "$t"
             printf 'puts "PASS %s/%s"\n' "$p" "$t" ;;
        */*) m="${pair%%/*}"; t="${pair#*/}"
             printf 'source %s/test/tcl/%s.tcl\n' "$root" "$t"
             printf 'if {[catch {run_method %s %s 0 1} r]} { puts "FAIL %s/%s" }\n' \
               "$m" "$t" "$m" "$t"
             printf 'puts "PASS %s/%s"\n' "$m" "$t" ;;
        *)   t="${pair%%:*}"; a="${pair#*:}"
             printf 'source %s/test/tcl/%s.tcl\n' "$root" "$t"
             printf 'if {[catch {eval %s %s} r]} { puts "FAIL %s" }\n' "$t" "$a" "$t"
             printf 'puts "PASS %s"\n' "$t" ;;
        esac
      done
    } > "$rt"
    timeout "${COVP_TCL_TIMEOUT:-2400}" "$TCLBIN" "$rt" >"$log" 2>&1 || true
    ;;
  recd|recd1|recd2)
    # Driver-per-test: several recd tests use conflicting Tcl globals and each
    # spawns recdscript.tcl subprocesses, so they cannot share one tclsh.
    case "$g" in
    recd1) _set=$RECD1_TESTS ;;
    recd2) _set=$RECD2_TESTS ;;
    *)     _set="$RECD1_TESTS $RECD2_TESTS" ;;
    esac
    rt="$d/recd.tcl"
    : > "$log"
    printf '%s\n' "$_set" | tr ' ' '\n' | grep ':' | while read -r spec; do
      [ -n "$spec" ] || continue
      t="${spec%%:*}"; rest="${spec#*:}"; m="${rest%%:*}"; a="${rest#*:}"
      printf 'source %s/test/tcl/test.tcl\nsource %s/test/tcl/%s.tcl\nif {[catch {eval %s %s %s} r]} { puts "FAIL %s %s: $r"; exit 3 }\nputs "PASS %s %s"\n' \
        "$root" "$root" "$t" "$t" "$m" "$a" "$t" "$m" "$t" "$m" > "$rt"
      timeout "${COVP_RECD_TIMEOUT:-300}" "$TCLBIN" "$rt" >>"$log" 2>&1 || true
      pkill -f "$rt" 2>/dev/null || true
      pkill -f 'recdscript' 2>/dev/null || true
      find TESTDIR -mindepth 1 -delete 2>/dev/null || true
    done
    ;;
  deadreg|deadreg1|deadreg2)
    case "$g" in
    deadreg1) _set=$DEADREG1_TESTS ;;
    deadreg2) _set=$DEADREG2_TESTS ;;
    *)        _set="$DEADREG1_TESTS
$DEADREG2_TESTS" ;;
    esac
    rt="$d/dreg.tcl"
    : > "$log"
    printf '%s\n' "$_set" | while IFS= read -r spec; do
      [ -n "$spec" ] || continue
      nm="${spec%%:*}"; call="${spec#*:}"
      printf 'source %s/test/tcl/test.tcl\nif {[catch {%s} r]} { puts "FAIL %s: $r"; exit 3 }\nputs "PASS %s"\n' \
        "$root" "$call" "$nm" "$nm" > "$rt"
      timeout "${COVP_DREG_TIMEOUT:-300}" "$TCLBIN" "$rt" >>"$log" 2>&1 || true
      pkill -f 'wrap.tcl' 2>/dev/null || true
      pkill -f 'ddscript' 2>/dev/null || true
      pkill -f 'envscript' 2>/dev/null || true
      find TESTDIR -mindepth 1 -delete 2>/dev/null || true
    done
    ;;
  cdrivers)
    # The cov_* C drivers.  Like the misc group, these run FROM build_unix: they
    # reference their sources as "../test/c/..." and each uses its own run
    # directory under the build tree rather than the shared TESTDIR.
    #
    # cov_cutest brings up live repmgr sites; run_cov_cutest.sh now gives itself a
    # private BDBPORTRANGE per run, which is what makes it safe to run this group
    # concurrently with anything else (and was worth 3.8pp of measured branch
    # coverage variance before it was fixed).
    : > "$log"
    CFLAGS="${CFLAGS:-} --coverage"; export CFLAGS
    cd "$bld"
    for drv in cov_api_surface cov_rep_api cov_logrec_print cov_codecs \
               cov_cutest cov_fuzz_corpus cov_oom_paths; do
      COV_OOM_STRIDE="${COV_OOM_STRIDE:-4}" \
      BUILD=. sh "$root/test/c/run_$drv.sh" >>"$log" 2>&1 \
        && echo "PASS $drv" >>"$log" || echo "FAIL $drv" >>"$log"
    done
    ;;
  misc)
    # XA, on-disk upgrade, os_aio, backup, compaction-recovery, hash-unsorted.
    #
    # These run FROM build_unix, not from a group directory, because each one
    # compiles its driver from a path spelled "../test/xa/xa_direct.c" -- i.e.
    # relative to build_unix.  Measured: from a group dir they all died with
    # "cc1: fatal error: ../test/xa/xa_direct.c: No such file or directory", the
    # group produced zero .gcda, and the PRODUCED-NO-GCDA guard aborted the run.
    #
    # That is safe here, unlike for the Tcl groups: each of these drivers uses
    # its OWN home directory (XA_TESTDIR, OSAIO_TESTDIR, BACKUP_TESTDIR, ...),
    # never the shared build_unix/TESTDIR, so running in build_unix collides with
    # nothing the other groups touch.  Their .gcda still land under this group's
    # GCOV_PREFIX, which is what keeps the counts separate.
    : > "$log"
    CFLAGS="${CFLAGS:-} --coverage"; export CFLAGS
    cd "$bld"
    for sc in xa/run_xa_direct.sh db/run_upgrade.sh os/run_os_aio.sh \
             backup/run_backup_direct.sh db/run_recd_compact.sh \
             db/run_recd_handlers.sh db/run_hash_unsorted_cmp.sh; do
      BUILD=. sh "$root/test/$sc" >>"$log" 2>&1 \
        && echo "PASS $sc" >>"$log" || echo "FAIL $sc" >>"$log"
    done
    ;;
  *)
    echo "unknown group: $g" >&2
    return 1 ;;
  esac

  t1=$(date +%s)
  # The .gcno must sit beside the .gcda or the capture below is empty -- see the
  # mirror_gcno comment above; an empty capture merges as "covered nothing".
  mirror_gcno "$GCOV_PREFIX"
  n=$(find "$GCOV_PREFIX" -name '*.gcda' 2>/dev/null | wc -l)
  echo "GROUP $g wall=$((t1 - t0))s gcda=$n" >> /tmp/covp-groups.txt
  # A group that produced NO .gcda measured nothing.  That must be visible, not
  # silently merged as an empty contribution -- it is the parallel version of
  # the vacuous-green shape this project has nine recorded instances of.
  [ "$n" -gt 0 ] || echo "GROUP $g PRODUCED NO GCDA" >> /tmp/covp-groups.txt
}

# ---------------------------------------------------------------------------
# 3. Launch the groups concurrently.
# ---------------------------------------------------------------------------
: > /tmp/covp-groups.txt
echo "== running groups concurrently: $COVP_GROUPS =="
for g in $COVP_GROUPS; do
  ( run_group "$g" ) &
done
wait
T_RUN=$(date +%s)
echo "  group wall times:"
sed -n 's/^/    /p' /tmp/covp-groups.txt

if grep -q 'PRODUCED NO GCDA' /tmp/covp-groups.txt; then
  echo
  echo "error: a group produced no .gcda at all -- it measured NOTHING, and"
  echo "merging it would understate coverage while looking like a normal run."
  grep 'PRODUCED NO GCDA' /tmp/covp-groups.txt
  exit 1
fi

# ---------------------------------------------------------------------------
# 4. Capture each group, then MERGE with lcov -a.
#
# gcov arc counts are additive across separate runs of the same binary, so the
# merge of per-group captures equals what a single serial run would have
# accumulated in one set of .gcda files.  That equivalence is the claim this
# whole script rests on, and it is checked end-to-end by comparing the merged
# branch total against the serial baseline through ratchet.sh.
# ---------------------------------------------------------------------------
echo "== capture per group + merge (lcov -a) =="
add_args=""
for g in $COVP_GROUPS; do
  pfx="$bld/covp-$g/gcda"
  info="$bld/covp-$g.info"
  # The .gcda live under $pfx mirroring the absolute build path; capture from
  # the mirrored .libs, which is where the merged replication counts live (the
  # serial script's note: capturing "." drops repmgr).
  dir="$pfx$bld/.libs"
  [ -d "$dir" ] || dir="$pfx$bld"
  [ -d "$dir" ] || dir="$pfx"
  if "$LCOV" --capture --directory "$dir" --output-file "$info" \
      --gcov-tool "$GCOV" --rc geninfo_unexecuted_blocks=1 $BRCOV \
      --ignore-errors "$IGN" >>"/tmp/covp-capture.log" 2>&1; then
    lines=$(grep -c '^SF:' "$info" 2>/dev/null || echo 0)
    echo "  $g: $lines source files captured"
    [ "$lines" -gt 0 ] && add_args="$add_args -a $info"
  else
    echo "  $g: CAPTURE FAILED (see /tmp/covp-capture.log)"
  fi
done

[ -n "$add_args" ] || { echo "error: nothing captured from any group"; exit 1; }

# shellcheck disable=SC2086
"$LCOV" $add_args --output-file "$bld/coverage.info" $BRCOV \
  --ignore-errors "$IGN" >>/tmp/covp-merge.log 2>&1 \
  || { echo "lcov merge failed:"; tail -20 /tmp/covp-merge.log; exit 1; }

"$LCOV" --extract "$bld/coverage.info" "*/src/*" \
  --output-file "$bld/coverage-src.info" $BRCOV \
  --ignore-errors "$IGN" >/dev/null 2>&1

echo "== summary =="
"$LCOV" --summary "$bld/coverage-src.info" $BRCOV --ignore-errors "$IGN" 2>&1 \
  | grep -E 'source files|lines|functions|branches' \
  | tee "$bld/coverage-summary.txt"

T_END=$(date +%s)
echo
echo "== wall time =="
printf '  %-22s %6ds\n' "configure+build" "$((T_BUILD - T0))"
printf '  %-22s %6ds\n' "groups (concurrent)" "$((T_RUN - T_BUILD))"
printf '  %-22s %6ds\n' "capture+merge" "$((T_END - T_RUN))"
printf '  %-22s %6ds\n' "TOTAL" "$((T_END - T0))"

if [ "${COVP_KEEP:-0}" != 1 ]; then
  for g in $COVP_GROUPS; do
    d="$bld/covp-$g"
    [ -d "$d" ] && find "$d" -mindepth 1 -delete 2>/dev/null || true
  done
fi

echo
echo "Now gate it:  test/coverage/ratchet.sh"
