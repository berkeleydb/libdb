#!/usr/bin/env python3
"""pinrm_ab_report.py -- median/CV table for a two-arm kill-switch A/B file.

Reads the RESULT lines produced by pinrm_bhpin_ab.sh, whose tags are
r1on/<envkind>/rep<N> and r1off/<envkind>/rep<N>, and prints ON vs OFF medians
with CV and the ON/OFF ratio per (env, API path, thread count).

usage: pinrm_ab_report.py <file> [on-tag] [off-tag]
"""
import re
import statistics
import sys

PAT = re.compile(
    r"RESULT tag=(?P<arm>[^/]+)/(?P<envk>[^/]+)/rep(?P<rep>\d+) "
    r"mode=(?P<mode>\w+) thr=(?P<thr>\d+) batch=\d+ "
    r"keys_per_sec=(?P<kps>[\d.]+)"
)


def cv(v):
    if len(v) < 2 or statistics.mean(v) == 0:
        return 0.0
    return 100.0 * statistics.stdev(v) / statistics.mean(v)


def main(argv):
    fn = argv[0]
    on_tag = argv[1] if len(argv) > 1 else "r1on"
    off_tag = argv[2] if len(argv) > 2 else "r1off"
    d, fails = {}, 0
    with open(fn) as fh:
        for line in fh:
            if line.startswith("FAIL"):
                fails += 1
                print("!! " + line.strip())
            m = PAT.search(line)
            if m:
                d.setdefault((m["envk"], m["mode"], int(m["thr"]), m["arm"]),
                             []).append(float(m["kps"]))
    if not d:
        print("VERDICT ab-report: NO DATA -- 0 RESULT lines parsed (FAILED RUN)")
        return 1
    print("| env | api | thr | ON median | CV% | OFF median | CV% | ON/OFF |")
    print("|---|---|---:|---:|---:|---:|---:|---:|")
    for envk in sorted({k[0] for k in d}):
        for mode in sorted({k[1] for k in d}):
            for thr in sorted({k[2] for k in d}):
                on = d.get((envk, mode, thr, on_tag))
                off = d.get((envk, mode, thr, off_tag))
                if not on or not off:
                    continue
                mo, mf = statistics.median(on), statistics.median(off)
                print(f"| {envk} | {mode} | {thr} | {mo:,.0f} | {cv(on):.1f} | "
                      f"{mf:,.0f} | {cv(off):.1f} | {mo / mf:.3f}x |")
    n = sum(len(v) for v in d.values())
    print(f"\nVERDICT ab-report: {n} measured points, {len(d)} cells, "
          f"{fails} FAIL line(s)")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
