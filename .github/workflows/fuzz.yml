# Fuzzing (libFuzzer) — advisory smoke fuzz tier.
#
# Dedicated workflow (does NOT touch ci.yml). Builds the three libdb fuzz
# harnesses under test/fuzz/ with clang -fsanitize=fuzzer,address,undefined
# and runs each for a BOUNDED time as a smoke fuzz on every PR. Any crashing
# input is uploaded as an artifact for triage.
#
# continue-on-error is set so this tier is advisory: it surfaces crashes
# without gating merges while the fuzz harnesses (Tier C of the
# test-suite-maturity plan) mature toward OSS-Fuzz integration.

name: Fuzz

on:
  push:
    branches: [master]
  pull_request:
    paths:
      - 'test/fuzz/**'
      - 'src/**'
      - 'dist/**'
      - '.github/workflows/fuzz.yml'
  workflow_dispatch:
    inputs:
      seconds:
        description: 'Seconds per harness'
        default: '60'

concurrency:
  group: fuzz-${{ github.ref }}
  cancel-in-progress: true

permissions:
  contents: read

jobs:
  smoke-fuzz:
    name: libFuzzer smoke (${{ github.event.inputs.seconds || '60' }}s/harness)
    runs-on: ubuntu-latest
    continue-on-error: true   # advisory tier
    steps:
      - uses: actions/checkout@v4

      - name: Install clang + libFuzzer + build deps
        run: |
          sudo apt-get update
          sudo apt-get install -y clang llvm liburing-dev
          clang --version

      - name: Build libdb (debug, so DB_ASSERTs are live)
        run: |
          cd build_unix
          ../dist/configure --enable-debug
          make -j"$(nproc)"

      - name: Build fuzz harnesses (fuzzer,address,undefined)
        run: |
          cd test/fuzz
          CC=clang ./run.sh build

      - name: Smoke fuzz each harness (bounded)
        run: |
          cd test/fuzz
          # Advisory: report crashes but don't fail the job (continue-on-error
          # also covers this, but keep the step green so the artifact upload
          # always runs).
          CC=clang ./run.sh smoke "${{ github.event.inputs.seconds || 60 }}" || true

      - name: Replay committed regression seeds (standalone, no libFuzzer)
        run: |
          cd test/fuzz
          for h in dbfile recover api; do
            for f in crashes/${h}_*.seed; do
              [ -e "$f" ] || continue
              echo "== replay $f =="
              # Known engine bugs may abort here; advisory, so don't fail.
              ./run.sh repro "$h" "$f" || true
            done
          done

      - name: Upload crashing inputs (regression candidates)
        if: always()
        uses: actions/upload-artifact@v4
        with:
          name: fuzz-artifacts
          path: |
            test/fuzz/build/artifacts_*/**
          if-no-files-found: ignore
