#!/usr/bin/env bash
#
# run_coverage.sh -- build libdb with gcov instrumentation, run a bounded
# representative test subset, and report line + branch coverage (SQLite-style:
# measure branch coverage, then aim new tests at the gaps).
#
# See test/coverage/README.md for how to read the report and find the
# least-covered files. Tier B3 of .agents/test-suite-maturity-plan.md.
#
# Usage:
#   test/coverage/run_coverage.sh              # subset build + report
#   COV_TESTS="lock001 txn001" test/coverage/run_coverage.sh   # custom tests
#   COV_JOBS=4 test/coverage/run_coverage.sh   # limit build parallelism
#
# Requires: gcc, gcov (matching gcc), lcov + genhtml, tclsh. In the nix dev
# shell gcov comes from gcc; lcov/genhtml are pulled via `nix run nixpkgs#lcov`
# automatically if not on PATH. On CI (ubuntu) install `lcov` from apt.
#
# Everything lands in build_unix/ (gitignored build tree); the HTML report is
# written to build_unix/coverage-html/ and a text summary to
# build_unix/coverage-summary.txt.

set -euo pipefail

# --- locate the repo (this script lives in test/coverage/) -------------------
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
root="$(cd "$here/../.." && pwd)"
bld="$root/build_unix"

# --- config ------------------------------------------------------------------
# Bounded, representative subset. Runs in a few minutes, exercises the core
# engine AND every access method plus the verify/salvage paths.
#
# Two entry forms, space-separated:
#   test:arg      -> `eval test arg` (arg blank for tests taking none).
#                    Used for the lock/txn/SSI/recovery core.
#   method/test   -> `run_method method test`, which runs the access-method
#                    test AND then verify_dir + salvage_dir on the databases
#                    it leaves behind -- so this one form covers the hash/
#                    queue/recno/heap access methods (src/hash, src/qam,
#                    src/btree/bt_recno.c, src/heap) *and* db verification
#                    (src/db/db_vrfy.c, src/hash/hash_verify.c,
#                    src/qam/qam_verify.c, src/heap/heap_verify.c) *and*
#                    salvage. Before this, only test001:btree ran, leaving
#                    hash/queue/heap/recno + all verify code at 0%.
#   proc@test@method -> `run_range_partition`/`run_partition_callback`
#                    (or any run_* proc) with (test method) -- runs an
#                    existing Tcl test under range partitioning / partition
#                    callbacks, lighting up src/db/partition.c.
#
# The access-method matrix is deliberately curated (a few high-value tests per
# method: basic put/get, cursors, dups, delete/renumber, partial) rather than
# the whole suite, to keep the run bounded while lighting up the red files.
# btree/test111 adds compaction coverage (src/btree/bt_compact.c); the
# run_range_partition/run_partition_callback entries add src/db/partition.c.
: "${COV_TESTS:=lock001: txn001: ssi001: ssi002: recd001:btree \
  btree/test001 btree/test111 \
  hash/test001 hash/test006 hash/test010 hash/test025 hash/test077 \
  queue/test001 queue/test007 queue/test025 \
  recno/test001 recno/test006 recno/test024 recno/test025 \
  heap/test001 heap/test013 heap/test024 \
  run_range_partition@test001@btree \
  run_partition_callback@test001@btree}"
: "${COV_JOBS:=$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 4)}"
: "${TCLSH:=tclsh}"
# TCL lib dir: nix store on this box, /usr/lib/tcl8.6 on ubuntu CI.
: "${TCL_LIB:=}"
if [ -z "$TCL_LIB" ]; then
  for d in /nix/store/*tcl-8.6*/lib /usr/lib/tcl8.6 /usr/lib; do
    [ -d "$d" ] && { TCL_LIB="$d"; break; }
  done
fi

# gcov MUST match the compiler that built the .gcno files. We force CC=gcc
# (BDB's configure otherwise picks `cc`, which in the nix shell is clang and
# emits LLVM-format .gcda that gcc's gcov cannot read -- the "B11 vs B52"
# version mismatch). gcov ships alongside gcc.
export CC=gcc
GCOV="${GCOV:-gcov}"

# lcov/genhtml: prefer PATH, else pull via nix (dev shell has neither on PATH).
LCOV=lcov
GENHTML=genhtml
if ! command -v lcov >/dev/null 2>&1; then
  if command -v nix >/dev/null 2>&1; then
    LCOV_BIN="$(nix build nixpkgs#lcov --no-link --print-out-paths 2>/dev/null)/bin"
    LCOV="$LCOV_BIN/lcov"
    GENHTML="$LCOV_BIN/genhtml"
  else
    echo "error: lcov not found on PATH and nix unavailable; install lcov" >&2
    exit 1
  fi
fi

# lcov error classes we tolerate: version (LLVM/gcc tag noise), source/mismatch
# (generated headers), inconsistent (gcov line/branch quirks), empty/unused.
IGN="mismatch,source,gcov,unused,negative,empty,inconsistent,version"

echo "== libdb coverage =="
echo "  repo:    $root"
echo "  CC:      $CC ($($CC -dumpversion 2>/dev/null || echo '?'))"
echo "  gcov:    $GCOV ($($GCOV --version 2>/dev/null | head -1))"
echo "  tcl lib: $TCL_LIB"
echo "  tests:   $COV_TESTS"
echo

# --- clean any prior coverage artifacts (idempotent) -------------------------
find "$bld" \( -name '*.gcda' -o -name '*.gcno' \) -delete 2>/dev/null || true
rm -f "$bld/coverage.info" "$bld/coverage-src.info" 2>/dev/null || true

# --- configure + build with instrumentation ---------------------------------
echo "== configure (--coverage) =="
cd "$bld"
CC=gcc ../dist/configure --enable-test --with-tcl="$TCL_LIB" \
  CFLAGS="-O0 -g --coverage" LDFLAGS="--coverage" >/tmp/cov-configure.log 2>&1 \
  || { echo "configure failed:"; tail -30 /tmp/cov-configure.log; exit 1; }

echo "== build (-j$COV_JOBS) =="
make -j"$COV_JOBS" >/tmp/cov-build.log 2>&1 \
  || { echo "build failed:"; tail -40 /tmp/cov-build.log; exit 1; }
echo "  .gcno files: $(find . -name '*.gcno' | wc -l)"

# --- run the test subset (produces .gcda) ------------------------------------
echo "== run tests =="
runtcl="$bld/.cov-run.tcl"
{
  echo 'source ../test/tcl/test.tcl'
  for pair in $COV_TESTS; do
    case "$pair" in
    *@*)  # proc@test@method form -> `run_proc test method` (partition runners)
      p="${pair%%@*}"; rest="${pair#*@}"; t="${rest%%@*}"; m="${rest#*@}"
      printf 'source ../test/tcl/%s.tcl\n' "$t"
      printf 'if {[catch {%s %s %s 0 1} res]} { puts "FAIL %s/%s/%s: $res"; exit 1 }\n' "$p" "$t" "$m" "$p" "$t" "$m"
      printf 'puts "PASS %s/%s/%s"\n' "$p" "$t" "$m"
      ;;
    */*)  # method/test form -> run_method (test + verify_dir + salvage_dir)
      m="${pair%%/*}"; t="${pair#*/}"
      printf 'source ../test/tcl/%s.tcl\n' "$t"
      printf 'if {[catch {run_method %s %s 0 1} res]} { puts "FAIL %s/%s: $res"; exit 1 }\n' "$m" "$t" "$m" "$t"
      printf 'puts "PASS %s/%s"\n' "$m" "$t"
      ;;
    *)    # test:arg form -> eval test arg
      t="${pair%%:*}"; a="${pair#*:}"
      printf 'source ../test/tcl/%s.tcl\n' "$t"
      printf 'if {[catch {eval %s %s} res]} { puts "FAIL %s: $res"; exit 1 }\n' "$t" "$a" "$t"
      printf 'puts "PASS %s"\n' "$t"
      ;;
    esac
  done
} > "$runtcl"
# tclsh8.6 preferred if present (nix); fall back to tclsh.
TCLBIN="$TCLSH"; command -v tclsh8.6 >/dev/null 2>&1 && TCLBIN=tclsh8.6
timeout "${COV_TIMEOUT:-2400}" "$TCLBIN" "$runtcl" 2>&1 | tee /tmp/cov-tests.log \
  | grep -E '^PASS|^FAIL' || true
rm -f "$runtcl"
if grep -q '^FAIL' /tmp/cov-tests.log; then
  echo "warning: a test FAILED; coverage still aggregated below" >&2
fi
echo "  .gcda files: $(find . -name '*.gcda' | wc -l)"

# --- optional replication tests (COV_REP=1) ----------------------------------
# Replication is the single biggest cold surface (rep/ + repmgr/ ~= 12.4k lines
# at ~0.8%). These tests are NOT in the default COV_TESTS because each one wants
# its own tclsh (they reset TESTDIR and a few election/lease tests hang), so
# they are run driver-per-test with a per-test timeout instead of one big tclsh.
# Set COV_REP=1 to include them. They use the in-process message-shuffling
# harness (rep0NN) and single-process real-socket repmgr (repmgrNN) -- no
# external orchestration. See test/coverage/REPLICATION-COVERAGE.md.
#
# NOT included (need real multi-process): the rep*script.tcl subprocess tests
# and the repmgr 100-series (need the db_repsite utility, absent from this fork)
# and a few election/lease tests that hang (rep016, repmgr024/026).
if [ "${COV_REP:-0}" = 1 ]; then
  echo "== run replication tests (COV_REP=1) =="
  : "${COV_REP_TIMEOUT:=300}"
  : "${COV_REP_TESTS:=rep001 rep002 rep003 rep005 rep006 rep007 rep008 rep009 \
 rep010 rep011 rep012 rep013 rep014 rep015 rep019 rep020 rep021 rep022 rep023 \
 rep024 rep025 rep026}"
  : "${COV_REPMGR_TESTS:=repmgr009 repmgr010 repmgr011 repmgr012 repmgr013 \
 repmgr017 repmgr018 repmgr023 repmgr025 repmgr027 repmgr030 repmgr031 \
 repmgr032 repmgr033 repmgr034}"
  reptcl="$bld/.cov-rep-one.tcl"
  for t in $COV_REP_TESTS $COV_REPMGR_TESTS; do
    case "$t" in
    rep[0-9]*) call="$t btree" ;;   # rep0NN take a method arg
    *)         call="$t" ;;          # repmgrNN use defaults
    esac
    printf 'source ../test/tcl/test.tcl\nsource ../test/tcl/reputils.tcl\nif {[catch {%s} r]} { puts "FAIL %s: $r"; exit 3 }\nputs "PASS %s"\n' \
      "$call" "$t" "$t" > "$reptcl"
    timeout "$COV_REP_TIMEOUT" "$TCLBIN" "$reptcl" >/tmp/cov-rep-$t.log 2>&1
    rc=$?
    if [ $rc -eq 124 ]; then echo "HANG $t"
    elif [ $rc -eq 0 ] && grep -q "^PASS $t" /tmp/cov-rep-$t.log; then echo "PASS $t"
    else echo "FAIL $t (rc=$rc)"; fi
    pkill -f "$reptcl" 2>/dev/null || true
    find TESTDIR -mindepth 1 -delete 2>/dev/null || true
  done
  rm -f "$reptcl"
  echo "  .gcda files after rep: $(find . -name '*.gcda' | wc -l)"
fi

# --- aggregate ---------------------------------------------------------------
echo "== capture (lcov) =="
# NOTE: capture from .libs (not .) -- libtool double-compiles, and only the
# .libs/*.gcda carry the merged replication counts; capturing "." drops repmgr.
"$LCOV" --capture --directory .libs --output-file coverage.info \
  --gcov-tool "$GCOV" --rc geninfo_unexecuted_blocks=1 --branch-coverage \
  --ignore-errors "$IGN" >/tmp/cov-lcov.log 2>&1 \
  || { echo "lcov capture failed:"; tail -20 /tmp/cov-lcov.log; exit 1; }

# Keep only the library sources under src/ (drop tcl harness, examples, system).
"$LCOV" --extract coverage.info "*/src/*" --output-file coverage-src.info \
  --branch-coverage --ignore-errors "$IGN" >/dev/null 2>&1

echo "== summary =="
"$LCOV" --summary coverage-src.info --branch-coverage --ignore-errors "$IGN" 2>&1 \
  | grep -E 'source files|lines|functions|branches' | tee coverage-summary.txt

# --- HTML report -------------------------------------------------------------
echo "== genhtml =="
rm -rf coverage-html
"$GENHTML" coverage-src.info --output-directory coverage-html --branch-coverage \
  --ignore-errors "empty,inconsistent,source,category,unmapped" >/tmp/cov-genhtml.log 2>&1 \
  && echo "  report: $bld/coverage-html/index.html" \
  || { echo "genhtml failed:"; tail -10 /tmp/cov-genhtml.log; }

# --- least-covered files (the actionable list) -------------------------------
echo
echo "== least-covered src files (>=50 lines) -- aim new tests here =="
"${PYTHON:-python3}" "$here/rank_coverage.py" coverage-src.info | head -20
echo
echo "Done. See test/coverage/README.md for how to read the report."
