# Coccinelle coding-convention checks + libabigail/nm ABI drift detection.
#
# TWO jobs, deliberately split by authority:
#
#   conventions  -- Coccinelle (spatch) SOURCE-LEVEL early warning.  Runs the
#                   rule_*.cocci convention checks and fails ONLY on NEW
#                   violations vs the committed baseline (dist/cocci/baseline.txt).
#                   This is a lint-style gate, NOT an ABI guarantee.
#
#   abi-diff     -- libabigail (abidiff) + nm are the AUTHORITATIVE binary-ABI
#                   check (SHARED-struct layout, public symbols).  Advisory
#                   (continue-on-error) by default; the ONE blocking sub-check is
#                   the header-regen drift gate (hand-edited generated headers).
#
# Coccinelle is complementary early warning at the source level; libabigail/nm
# is the real ABI contract.  See dist/cocci/README.md.
#
# This file is owned by the cocci/abi effort.  It does NOT touch ci.yml,
# ocr-review.yml, or .github/ocr/ (owned by other efforts).

name: Cocci + ABI

on:
  push:
    branches: [master]
  pull_request:
  workflow_dispatch:

concurrency:
  group: cocci-${{ github.ref }}
  cancel-in-progress: true

permissions:
  contents: read
  pull-requests: write

jobs:
  # ---------------------------------------------------------------------------
  # Source-level convention checks (Coccinelle).  Blocking on NEW violations.
  # ---------------------------------------------------------------------------
  conventions:
    name: cocci conventions
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Install coccinelle
        run: |
          sudo apt-get update
          sudo apt-get install -y coccinelle
          spatch --version | head -1

      - name: Generate headers (configure only; no full build needed)
        run: |
          cd build_unix
          ../dist/configure >/tmp/configure.log 2>&1 || { tail -40 /tmp/configure.log; exit 1; }
          test -f db.h && test -f db_int.h

      - name: Run convention rules
        run: |
          sh dist/cocci/run_conventions.sh > /tmp/cocci-current.txt
          echo "== current violations =="
          cut -d'|' -f1 /tmp/cocci-current.txt | sort | uniq -c || true

      - name: Compare against baseline
        id: compare
        run: |
          sort -u dist/cocci/baseline.txt > /tmp/base.txt
          sort -u /tmp/cocci-current.txt   > /tmp/cur.txt
          # NEW = in current, not in baseline.
          comm -13 /tmp/base.txt /tmp/cur.txt > /tmp/new.txt || true
          # FIXED = in baseline, not in current (informational).
          comm -23 /tmp/base.txt /tmp/cur.txt > /tmp/fixed.txt || true
          echo "new_count=$(wc -l < /tmp/new.txt)"   >> "$GITHUB_OUTPUT"
          echo "fixed_count=$(wc -l < /tmp/fixed.txt)" >> "$GITHUB_OUTPUT"
          {
            echo "### Coccinelle convention checks"
            echo
            if [ -s /tmp/new.txt ]; then
              echo "**NEW violations ($(wc -l < /tmp/new.txt)):**"
              echo '```'
              cat /tmp/new.txt
              echo '```'
            else
              echo "No new violations. :white_check_mark:"
            fi
            if [ -s /tmp/fixed.txt ]; then
              echo
              echo "<details><summary>Resolved since baseline ($(wc -l < /tmp/fixed.txt))"
              echo " -- update dist/cocci/baseline.txt to lock these in.</summary>"
              echo
              echo '```'
              cat /tmp/fixed.txt
              echo '```'
              echo "</details>"
            fi
          } > /tmp/cocci-report.md
          cat /tmp/cocci-report.md

      - name: Post convention report to PR
        if: github.event_name == 'pull_request'
        uses: actions/github-script@v9
        with:
          script: |
            const fs = require('fs');
            let body = '';
            try { body = fs.readFileSync('/tmp/cocci-report.md', 'utf8').trim(); } catch (e) {}
            if (!body) return;
            const marker = '<!-- cocci-conventions -->';
            body = marker + '\n' + body;
            const prNumber = context.payload.pull_request.number;
            const { data: comments } = await github.rest.issues.listComments({
              owner: context.repo.owner, repo: context.repo.repo,
              issue_number: prNumber, per_page: 100 });
            const mine = comments.find(c => c.user.type === 'Bot' && c.body && c.body.includes(marker));
            if (mine) {
              await github.rest.issues.updateComment({
                owner: context.repo.owner, repo: context.repo.repo,
                comment_id: mine.id, body });
            } else {
              await github.rest.issues.createComment({
                owner: context.repo.owner, repo: context.repo.repo,
                issue_number: prNumber, body });
            }

      - name: Fail on new violations
        if: steps.compare.outputs.new_count != '0'
        run: |
          echo "::error::${{ steps.compare.outputs.new_count }} new Coccinelle convention violation(s). See PR comment / baseline."
          cat /tmp/new.txt
          exit 1

  # ---------------------------------------------------------------------------
  # Binary ABI diff (libabigail + nm) -- AUTHORITATIVE.  Advisory by default;
  # only the header-regen drift sub-check is blocking.
  # ---------------------------------------------------------------------------
  abi-diff:
    name: abi drift (advisory)
    runs-on: ubuntu-latest
    if: github.event_name == 'pull_request'
    steps:
      - uses: actions/checkout@v4
        with:
          fetch-depth: 0   # need tags for git describe

      - name: Install abigail-tools + coccinelle
        run: |
          sudo apt-get update
          sudo apt-get install -y abigail-tools coccinelle binutils
          abidiff --version | head -1 || true

      # ---- Header-regen drift gate (BLOCKING) --------------------------------
      # Regenerate the auto headers from source PUBLIC comments.  A diff means
      # someone hand-edited a generated header -- a real, blocking error.
      - name: Header-regen drift gate (blocking)
        run: |
          (cd dist && sh s_include)
          if ! git diff --exit-code src/dbinc_auto/; then
            echo "::error::src/dbinc_auto/ drifted from dist/s_include output."
            echo "::error::Do not hand-edit generated headers; edit the PUBLIC comments in src/**/*.c and re-run 'cd dist && sh s_include'."
            exit 1
          fi
          echo "Generated headers are in sync. :white_check_mark:"

      # ---- Everything below is advisory ------------------------------------
      - name: Flag-bit inventory (early warning context)
        continue-on-error: true
        run: |
          sh dist/cocci/flagbits_inventory.sh > /tmp/flagbits-head.txt || true
          echo "Flag-bit #defines on PR head: $(wc -l < /tmp/flagbits-head.txt)"

      - name: Build PR head + base release, then abidiff
        id: abi
        continue-on-error: true
        run: |
          set -x
          NPROC=$(nproc)
          BASE_TAG=$(git describe --tags --match 'v*' --abbrev=0 2>/dev/null || echo "")
          echo "Base release tag: ${BASE_TAG:-<none>}"

          build_so() {  # $1 = worktree dir -> echoes path to libdb .so
            ( cd "$1/build_unix" && ../dist/configure >/tmp/cfg-$2.log 2>&1 && \
              make -j"$NPROC" >/tmp/make-$2.log 2>&1 ) || { echo "build $2 failed"; tail -30 /tmp/make-$2.log; return 1; }
            ls "$1"/build_unix/.libs/libdb-*.so | head -1
          }

          HEAD_SO=$(build_so "$PWD" head) || { echo "head build failed"; exit 0; }
          echo "head .so: $HEAD_SO"

          if [ -z "$BASE_TAG" ]; then
            echo "No base release tag; skipping abidiff." > /tmp/abi-report.md
            exit 0
          fi

          git worktree add /tmp/base "$BASE_TAG" >/dev/null 2>&1 || { echo "worktree add failed"; exit 0; }
          BASE_SO=$(build_so /tmp/base base) || { echo "base build failed; skipping abidiff" > /tmp/abi-report.md; exit 0; }
          echo "base .so: $BASE_SO"

          {
            echo "### ABI diff vs \`$BASE_TAG\` (libabigail — authoritative)"
            echo
            echo '```'
            abidiff "$BASE_SO" "$HEAD_SO" 2>&1 | head -200 || true
            echo '```'
            echo
            echo "#### Removed exported symbols (nm -D, _NNNN version suffix normalized)"
            # Normalize the DB_VERSION_UNIQUE_NAME _NNNN suffix (may or may not be
            # applied by a given build) so it does not create phantom diffs.
            norm() { nm -D --defined-only "$1" | awk '$2 ~ /[TWiw]/ {print $3}' | sed -E 's/_[0-9]{4}$//' | sort -u; }
            norm "$BASE_SO" > /tmp/base-syms.txt
            norm "$HEAD_SO" > /tmp/head-syms.txt
            REMOVED=$(comm -23 /tmp/base-syms.txt /tmp/head-syms.txt)
            if [ -n "$REMOVED" ]; then
              echo '```'
              echo "$REMOVED"
              echo '```'
            else
              echo "None."
            fi
          } > /tmp/abi-report.md
          cat /tmp/abi-report.md

      - name: Post ABI report to PR
        if: always()
        continue-on-error: true
        uses: actions/github-script@v9
        with:
          script: |
            const fs = require('fs');
            let body = '';
            try { body = fs.readFileSync('/tmp/abi-report.md', 'utf8').trim(); } catch (e) {}
            if (!body) body = '_ABI diff produced no report (build skipped or no base tag)._';
            const marker = '<!-- cocci-abi-diff -->';
            body = marker + '\n' + body +
              '\n\n---\n_Advisory: libabigail/nm is the authoritative binary-ABI check; ' +
              'Coccinelle is complementary source-level early warning. See dist/cocci/README.md._';
            const prNumber = context.payload.pull_request.number;
            const { data: comments } = await github.rest.issues.listComments({
              owner: context.repo.owner, repo: context.repo.repo,
              issue_number: prNumber, per_page: 100 });
            const mine = comments.find(c => c.user.type === 'Bot' && c.body && c.body.includes(marker));
            if (mine) {
              await github.rest.issues.updateComment({
                owner: context.repo.owner, repo: context.repo.repo,
                comment_id: mine.id, body });
            } else {
              await github.rest.issues.createComment({
                owner: context.repo.owner, repo: context.repo.repo,
                issue_number: prNumber, body });
            }
