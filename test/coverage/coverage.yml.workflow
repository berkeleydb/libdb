# Code coverage (gcov/lcov) -- advisory measurement tier.
#
# SQLite's ethic is 100% MC/DC branch coverage. We are nowhere near that, but
# Tier B3 of .agents/test-suite-maturity-plan.md says: MEASURE branch coverage,
# then aim new DST/PBT/unit tests at the uncovered branches, and ratchet the
# number upward. This workflow does the measuring.
#
# Coverage builds are slow (instrumented -O0 build + test run), so this does
# NOT run on every PR: nightly (cron) + on-demand (workflow_dispatch) only.
# Dedicated workflow -- does NOT touch ci.yml. Advisory (continue-on-error):
# it informs and warns on regressions but never gates a merge.

name: Coverage

on:
  schedule:
    # Nightly at 05:41 UTC, staggered after ci.yml (03:17) and ci-extended (04:23).
    - cron: '41 5 * * *'
  workflow_dispatch:
    inputs:
      tests:
        description: 'Space-separated "test:arg" pairs (blank = default subset)'
        type: string
        default: ''

concurrency:
  group: coverage-${{ github.ref }}
  cancel-in-progress: true

permissions:
  contents: read

jobs:
  coverage:
    name: gcov/lcov branch coverage
    runs-on: ubuntu-latest
    continue-on-error: true   # advisory tier
    timeout-minutes: 90
    steps:
      - uses: actions/checkout@v4

      - name: Install toolchain (gcc, gcov, lcov, tcl)
        run: |
          sudo apt-get update
          sudo apt-get install -y gcc lcov tcl-dev tcl python3

      - name: Build with coverage + run test subset + aggregate
        id: cov
        env:
          # gcov must match the compiler; run_coverage.sh forces CC=gcc.
          TCL_LIB: /usr/lib/tcl8.6
          COV_TESTS: ${{ github.event.inputs.tests }}
        run: |
          # Empty COV_TESTS => script uses its built-in representative subset.
          [ -z "$COV_TESTS" ] && unset COV_TESTS
          test/coverage/run_coverage.sh | tee /tmp/cov.out
          # Surface the summary as a job notice.
          summary=$(grep -E 'lines|branches|functions' build_unix/coverage-summary.txt | tr '\n' ' ')
          echo "::notice title=Coverage (src/)::$summary"
          # Extract the branch % (e.g. "12.3%") for the ratchet.
          br=$(grep -E 'branches' build_unix/coverage-summary.txt \
               | grep -oE '[0-9]+\.[0-9]+%' | head -1 | tr -d '%')
          ln=$(grep -E 'lines' build_unix/coverage-summary.txt \
               | grep -oE '[0-9]+\.[0-9]+%' | head -1 | tr -d '%')
          echo "branch=$br" >> "$GITHUB_OUTPUT"
          echo "line=$ln" >> "$GITHUB_OUTPUT"

      - name: Ratchet (advisory) -- warn if branch coverage dropped
        run: |
          base_file=test/coverage/baseline.txt
          br="${{ steps.cov.outputs.branch }}"
          ln="${{ steps.cov.outputs.line }}"
          if [ -f "$base_file" ]; then
            base_br=$(grep -E '^branch=' "$base_file" | cut -d= -f2)
            base_ln=$(grep -E '^line=' "$base_file" | cut -d= -f2)
            echo "baseline: line=${base_ln}% branch=${base_br}%  now: line=${ln}% branch=${br}%"
            # bc may be absent; use awk for the float compare.
            drop=$(awk -v a="$br" -v b="$base_br" 'BEGIN{print (a+0 < b-0.5) ? 1 : 0}')
            if [ "$drop" = "1" ]; then
              echo "::warning title=Coverage ratchet::branch coverage dropped ${base_br}% -> ${br}% (advisory)"
            else
              echo "::notice title=Coverage ratchet::branch coverage ${br}% >= baseline ${base_br}% (ok)"
            fi
          else
            echo "::notice title=Coverage ratchet::no baseline file yet; current branch=${br}% line=${ln}%"
          fi
          echo "To update the committed baseline: put 'line=${ln}' and 'branch=${br}' in $base_file"

      - name: Upload HTML report
        if: always()
        uses: actions/upload-artifact@v4
        with:
          name: coverage-html
          path: |
            build_unix/coverage-html/
            build_unix/coverage-summary.txt
          if-no-files-found: warn
