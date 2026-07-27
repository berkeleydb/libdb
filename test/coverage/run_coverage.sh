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
# Bounded, representative subset: lock/txn/basic-access + SSI + one access
# method + a recovery test. Runs in a few minutes, exercises the core engine.
# Format: "test arg" pairs (arg blank for tests that take none).
: "${COV_TESTS:=lock001: txn001: test001:btree ssi001: ssi002: recd001:btree}"
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
    t="${pair%%:*}"; a="${pair#*:}"
    printf 'source ../test/tcl/%s.tcl\n' "$t"
    printf 'if {[catch {eval %s %s} res]} { puts "FAIL %s: $res"; exit 1 }\n' "$t" "$a" "$t"
    printf 'puts "PASS %s"\n' "$t"
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

# --- aggregate ---------------------------------------------------------------
echo "== capture (lcov) =="
"$LCOV" --capture --directory . --output-file coverage.info \
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
