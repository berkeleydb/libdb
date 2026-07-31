#!/usr/bin/env python3
"""Rank libdb source files by coverage from an lcov .info file.

Prints the least-covered files first (lowest line %, then largest), so future
DST/PBT/unit test work can aim at the biggest uncovered surfaces. Files under
50 lines are skipped as noise. This is the actionable output of Tier B3
(coverage in CI) in the test-suite maturity plan.

Usage: rank_coverage.py <coverage.info> [min_lines]
"""
import sys

def parse(path):
    files, cur = {}, None
    for ln in open(path):
        ln = ln.rstrip()
        if ln.startswith("SF:"):
            cur = ln[3:]
            files[cur] = {"la": 0, "lh": 0, "ba": 0, "bh": 0}
        elif ln.startswith("DA:") and cur:
            cnt = ln[3:].rsplit(",", 1)[1]
            files[cur]["la"] += 1
            files[cur]["lh"] += (cnt != "0")
        elif ln.startswith("BRDA:") and cur:
            taken = ln.split(",")[-1]
            files[cur]["ba"] += 1
            files[cur]["bh"] += (taken not in ("-", "0"))
    return files

def main():
    if len(sys.argv) < 2:
        sys.exit("usage: rank_coverage.py <coverage.info> [min_lines]")
    min_lines = int(sys.argv[2]) if len(sys.argv) > 2 else 50
    rows = []
    for f, d in parse(sys.argv[1]).items():
        if d["la"] < min_lines:
            continue
        lr = 100.0 * d["lh"] / d["la"] if d["la"] else 0.0
        br = 100.0 * d["bh"] / d["ba"] if d["ba"] else 0.0
        short = f.split("/src/", 1)[-1] if "/src/" in f else f.split("/")[-1]
        rows.append((lr, br, d["la"], d["lh"], d["ba"], d["bh"], short))
    rows.sort(key=lambda r: (r[0], -r[2]))  # lowest line%, then biggest file
    print(f"{'line%':>6} {'br%':>6} {'lines':>6} {'lhit':>5} {'branch':>7} {'bhit':>5}  file")
    for lr, br, la, lh, ba, bh, short in rows:
        print(f"{lr:6.1f} {br:6.1f} {la:6d} {lh:5d} {ba:7d} {bh:5d}  {short}")

if __name__ == "__main__":
    try:
        main()
    except BrokenPipeError:
        # allow `... | head` to close the pipe without a traceback
        try:
            sys.stdout.close()
        except BrokenPipeError:
            pass
