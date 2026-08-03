#!/usr/bin/env python3
"""Spelling gate: codespell over the docs, hard-failing only on NEW typos.

The migrated docs carry ~150 legacy typos in decades-old source prose. Fixing
those is a separate content pass (and would churn no-loss-protected content),
so this gate baselines them: it runs codespell, keys each finding on
(relative-path, typo-word), and FAILS only on findings NOT in
codespell-baseline.txt. Any typo an editor introduces on a new/edited page
fails CI; the legacy backlog does not block. Shrinking the backlog (fixing a
baselined typo) is always safe -- a stale baseline entry is reported, not fatal.

Usage:  spellcheck.py            # gate (exit 1 on new typos)
        spellcheck.py --report   # list everything, never fail (advisory)
Requires: codespell on PATH.
"""
import re
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parents[1]
DOCS = REPO / "docs_src"
BASELINE = HERE / "codespell-baseline.txt"
WORDLIST = HERE / "codespell-wordlist.txt"
SKIP = "*.toml,*.tmpl,*.css,_migrate,PLAN.md"

LINE = re.compile(r"^(.+?):(\d+): ([A-Za-z_-]+) ==> (.+)$")


def run_codespell():
    cmd = ["codespell", str(DOCS), f"--skip={SKIP}"]
    if WORDLIST.exists():
        cmd.append(f"--ignore-words={WORDLIST}")
    p = subprocess.run(cmd, capture_output=True, text=True)
    findings = []          # (relpath, line, word, suggestion)
    for l in p.stdout.splitlines():
        m = LINE.match(l)
        if not m:
            continue
        path, line, word, sugg = m.groups()
        rel = str(Path(path).resolve().relative_to(REPO))
        findings.append((rel, line, word, sugg))
    return findings


def load_baseline():
    if not BASELINE.exists():
        return set()
    out = set()
    for l in BASELINE.read_text().splitlines():
        if "\t" in l:
            rel, word = l.split("\t", 1)
            out.add((rel.strip(), word.strip()))
    return out


def main():
    findings = run_codespell()
    baseline = load_baseline()
    report = "--report" in sys.argv
    seen = {(rel, word) for rel, _, word, _ in findings}
    new = [(rel, ln, w, s) for (rel, ln, w, s) in findings
           if (rel, w) not in baseline]
    stale = sorted(baseline - {(rel, w) for rel, _, w, _ in findings})

    print(f"codespell findings: {len(findings)} "
          f"({len(baseline)} baselined legacy typos)")
    if report:
        for rel, ln, w, s in findings:
            print(f"  {rel}:{ln}: {w} ==> {s}")
        print(f"\nstale baseline entries (now fixed): {len(stale)}")
        return
    if stale:
        print(f"note: {len(stale)} baselined typo(s) no longer present "
              "(trim codespell-baseline.txt):")
        for rel, w in stale[:20]:
            print(f"  {rel}\t{w}")
    if new:
        print(f"\nSPELLING GATE FAIL: {len(new)} NEW typo(s) "
              "(fix, or add to codespell-wordlist.txt if a real word):")
        for rel, ln, w, s in new:
            print(f"  {rel}:{ln}: {w} ==> {s}")
        sys.exit(1)
    print("SPELLING GATE PASS: no new typos beyond the legacy baseline.")


if __name__ == "__main__":
    main()
