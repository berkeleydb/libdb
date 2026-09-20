#!/usr/bin/env python3
"""Summarise a P5 A/B TSV: median and CV per (arm, threads), plus the ratio.

    p5_report.py <tsv> [arm-column]

Reports the median rather than the mean because a single descheduled rep skews
a mean and we run few reps; CV is reported so a reader can see when a ratio is
not meaningful.  A ratio inside the noise floor measured by the base-against-
itself run is a NULL result and must be reported as one.
"""
import sys
import statistics
from collections import defaultdict


def main():
    path = sys.argv[1]
    armcol = sys.argv[2] if len(sys.argv) > 2 else None

    with open(path) as fh:
        rows = [l.rstrip("\n").split("\t") for l in fh if l.strip()]
    hdr, rows = rows[0], rows[1:]
    if armcol is None:
        armcol = "batch" if "batch" in hdr else "arm"
    ai, ti = hdr.index(armcol), hdr.index("threads")
    # throughput column: rows_per_sec for batch runs, ops_per_sec otherwise
    oi = hdr.index("rows_per_sec" if "rows_per_sec" in hdr else "ops_per_sec")
    extra = [c for c in ("rec_per_row", "mb_per_sec") if c in hdr]

    acc = defaultdict(list)
    ex = defaultdict(lambda: defaultdict(list))
    for r in rows:
        try:
            acc[(r[ai], int(r[ti]))].append(float(r[oi]))
        except (ValueError, IndexError):
            continue
        for c in extra:
            try:
                ex[(r[ai], int(r[ti]))][c].append(float(r[hdr.index(c)]))
            except (ValueError, IndexError):
                pass

    arms = sorted({k[0] for k in acc}, key=lambda s: float(s))
    threads = sorted({k[1] for k in acc})

    def cv(v):
        if len(v) < 2 or statistics.mean(v) == 0:
            return 0.0
        return statistics.stdev(v) / statistics.mean(v) * 100

    base = arms[0]
    cols = "".join(f"{('arm ' + a):>13} {'CV%':>7}" for a in arms)
    print(f"{'t':>4}{cols}" + ("".join(f"{a+'/'+base:>10}" for a in arms[1:])))
    ratios = defaultdict(list)
    for t in threads:
        line = f"{t:>4}"
        for a in arms:
            v = acc.get((a, t), [])
            line += f"{(statistics.median(v) if v else 0):>13.0f} {cv(v):>7.2f}"
        for a in arms[1:]:
            v, b = acc.get((a, t), []), acc.get((base, t), [])
            if v and b:
                rr = statistics.median(v) / statistics.median(b)
                ratios[a].append((t, rr))
                line += f"{(rr - 1) * 100:>+9.2f}%"
            else:
                line += f"{'-':>10}"
        print(line)

    worst = max((abs(r - 1) for a in arms[1:] for _, r in ratios[a]), default=0)
    if len(arms) > 1:
        print(f"\nlargest |deviation| across all cells: {worst * 100:.2f}%")
    else:
        allcv = [cv(acc[(base, t)]) for t in threads]
        print(f"\nmax CV within the single arm: {max(allcv):.2f}%")

    for c in extra:
        print(f"\n{c}:")
        for t in threads:
            cells = "".join(
                f"{(statistics.median(ex[(a,t)][c]) if ex[(a,t)][c] else 0):>13.2f}"
                for a in arms)
            print(f"{t:>4}{cells}")


main()
