# CBMC formal-verification harnesses (advisory).
#
# Runs test/cbmc/run.sh in the flake dev shell (which provides CBMC 6.9.0).
# Each harness proves memory-safety + functional properties of a self-contained
# libdb algorithmic core over ALL inputs within a bound (see test/cbmc/README.md).
#
# ADVISORY: this workflow does not gate merges.  It is expected to report the
# known __db_ret_okitem OOB bug (see README "BUG FOUND") until the one-line
# engine fix lands; run.sh treats that as an expected reproduction, not a suite
# failure, so a green run means "all verifying harnesses verified AND the known
# bug still reproduces".  If run.sh goes red, either a verifying harness
# regressed or the okitem bug's status changed -- investigate.
#
# This file is owned by the CBMC effort.  It does NOT touch ci.yml or any other
# effort's workflow.  It is staged as *.yml.workflow because pushing files under
# .github/workflows/ needs an OAuth token with the `workflow` scope; a
# maintainer enables it with:
#   cp test/cbmc/cbmc.yml.workflow .github/workflows/cbmc.yml

name: CBMC

on:
  push:
    branches: [master]
    paths:
      - 'test/cbmc/**'
      - 'src/common/db_compint.c'
      - 'src/common/db_getlong.c'
      - 'src/hash/hash_func.c'
      - 'src/db/db_ret.c'
      - 'src/lock/lock_deadlock.c'
      - 'src/dbinc/db_swap.h'
      - 'src/dbinc/db_page.h'
      - 'flake.nix'
  pull_request:
    paths:
      - 'test/cbmc/**'
      - 'src/common/db_compint.c'
      - 'src/common/db_getlong.c'
      - 'src/hash/hash_func.c'
      - 'src/db/db_ret.c'
      - 'src/lock/lock_deadlock.c'
      - 'src/dbinc/db_swap.h'
      - 'src/dbinc/db_page.h'
      - 'flake.nix'
  workflow_dispatch:

concurrency:
  group: cbmc-${{ github.ref }}
  cancel-in-progress: true

permissions:
  contents: read

jobs:
  cbmc:
    name: cbmc harnesses (advisory)
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Install nix
        uses: cachix/install-nix-action@v27
        with:
          extra_nix_config: |
            experimental-features = nix-command flakes

      - name: cbmc --version
        run: nix develop . --command bash -c 'cbmc --version'

      - name: Run CBMC harness suite
        run: nix develop . --command bash test/cbmc/run.sh
