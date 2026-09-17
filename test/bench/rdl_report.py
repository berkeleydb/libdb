#!/usr/bin/env python3
"""rdl_report.py -- median + CV aggregation for the rdl_bench two-regime sweep.

Reads RDLB lines from stdin or a file and prints, per (regime, iso, threads),
the median ops/sec and the coefficient of variation across reps, plus the
locks/op and partition-contention columns.  Median (not mean) because a single
descheduled rep otherwise moves the number more than any code change here does.
"""
import re
import statistics
import sys
from collections import defaultdict

PAT = re.compile(
    r"RDLB\s+(?P<regime>\S+)\s+(?P<iso>\S+)\s+thr=(?P<thr>\d+)\s+"
    r"ops/sec=\s*(?P<ops>[\d.]+)\s+locks/op=\s*(?P<locks>[\d.]+)\s+"
    r"pages/op=\s*(?P<pages>[\d.]+)\s+lockpart%=\s*(?P<lockpart>[\d.]+)\s+"
    r"conflict%=\s*(?P<conflict>[\d.]+)\s+objs%=\s*(?P<objs>[\d.]+)\s+"
    r"lockreg_w=(?P<regw>\d+)\s+errs=(?P<errs>\d+)"
)

ISO_ORDER = ["none", "plain", "rc", "uncom", "si", "ssi"]


def cv(xs):
    if len(xs) < 2:
        return 0.0
    m = statistics.mean(xs)
    return 100.0 * statistics.stdev(xs) / m if m else 0.0


def main():
    src = open(sys.argv[1]) if len(sys.argv) > 1 else sys.stdin
    rows = defaultdict(list)
    for line in src:
        m = PAT.search(line)
        if m:
            rows[(m["regime"], m["iso"], int(m["thr"]))].append(
                {k: float(m[k]) for k in
                 ("ops", "locks", "pages", "lockpart", "conflict", "objs",
                  "errs")}
            )
    if not rows:
        sys.exit("rdl_report: no RDLB lines found -- nothing to aggregate")

    regimes = sorted({k[0] for k in rows})
    threads = sorted({k[2] for k in rows})
    isos = [i for i in ISO_ORDER if any(k[1] == i for k in rows)]

    print(f"{'regime':<8} {'iso':<6} {'thr':>4} {'reps':>4} "
          f"{'median ops/s':>13} {'CV%':>6} {'locks/op':>9} {'pages/op':>9} "
          f"{'lockpart%':>10} {'errs':>6}")
    for rg in regimes:
        for iso in isos:
            for t in threads:
                r = rows.get((rg, iso, t))
                if not r:
                    continue
                ops = [x["ops"] for x in r]
                print(f"{rg:<8} {iso:<6} {t:>4} {len(r):>4} "
                      f"{statistics.median(ops):>13,.0f} {cv(ops):>6.1f} "
                      f"{statistics.median([x['locks'] for x in r]):>9.3f} "
                      f"{statistics.median([x['pages'] for x in r]):>9.3f} "
                      f"{statistics.median([x['lockpart'] for x in r]):>10.1f} "
                      f"{sum(x['errs'] for x in r):>6.0f}")
        print()

    # The headline comparison: si (0 lock objects) vs ssi (1 SIREAD marker).
    print("=== SSI read-set cost: ssi vs si, same regime and thread count ===")
    print(f"{'regime':<8} {'thr':>4} {'si ops/s':>12} {'ssi ops/s':>12} "
          f"{'ssi/si':>8}  interpretation")
    for rg in regimes:
        for t in threads:
            si = rows.get((rg, "si", t))
            ss = rows.get((rg, "ssi", t))
            if not si or not ss:
                continue
            a = statistics.median([x["ops"] for x in si])
            b = statistics.median([x["ops"] for x in ss])
            ratio = b / a if a else 0.0
            note = ("SSI marker costs %.1f%%" % (100 * (1 - ratio))
                    if ratio < 1 else "no measurable SSI cost")
            print(f"{rg:<8} {t:>4} {a:>12,.0f} {b:>12,.0f} {ratio:>8.3f}  {note}")


if __name__ == "__main__":
    main()
