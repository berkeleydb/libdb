#!/usr/bin/env python3
"""pinrm_report.py -- median/CV table from one or more pinrm sweep files.

Reads RESULT lines of the form

  RESULT tag=<arm>/<envkind>/rep<N> mode=<indiv|batch> thr=<n> batch=16 \
      keys_per_sec=<f> batch_p50_us=<f> batch_p99_us=<f> notfound=<n>

and prints, per (envkind, mode, thr), every arm's median keys/s, CV%, n, and
the ratio to the `base` arm.  `base2` is the same baseline binary run again:
its ratio to base IS the noise floor, and any arm whose deviation is inside
that band is a NULL result.

usage: pinrm_report.py sweep1.txt [sweep2.txt ...] [--md]
"""
import re
import statistics
import sys

PAT = re.compile(
    r"RESULT tag=(?P<arm>[^/]+)/(?P<envk>[^/]+)/rep(?P<rep>\d+) "
    r"mode=(?P<mode>\w+) thr=(?P<thr>\d+) batch=\d+ "
    r"keys_per_sec=(?P<kps>[\d.]+) batch_p50_us=(?P<p50>[\d.]+) "
    r"batch_p99_us=(?P<p99>[\d.]+)"
)
ARMS = ["base", "base2", "bhpin", "rsnap", "lockrp", "mpoolp"]


def cv(vals):
    if len(vals) < 2:
        return 0.0
    m = statistics.mean(vals)
    return 0.0 if m == 0 else 100.0 * statistics.stdev(vals) / m


def main(argv):
    md = "--md" in argv
    files = [a for a in argv if not a.startswith("--")]
    data, p99s, fails = {}, {}, []
    for fn in files:
        with open(fn) as fh:
            for line in fh:
                if line.startswith("FAIL"):
                    fails.append(line.strip())
                m = PAT.search(line)
                if not m:
                    continue
                k = (m["envk"], m["mode"], int(m["thr"]), m["arm"])
                data.setdefault(k, []).append(float(m["kps"]))
                p99s.setdefault(k, []).append(float(m["p99"]))
    if not data:
        print("VERDICT pinrm-report: NO DATA -- 0 RESULT lines parsed (FAILED RUN)")
        return 1
    for f in fails:
        print("!! " + f)

    envks = sorted({k[0] for k in data})
    modes = sorted({k[1] for k in data})
    thrs = sorted({k[2] for k in data})
    npoints = 0
    for envk in envks:
        for mode in modes:
            print()
            print(f"### env={envk} api={mode}")
            hdr = ("| thr | arm | median keys/s | CV% | n | vs base | p99 us |"
                   if md else
                   f"{'thr':>4} {'arm':<7} {'median':>12} {'CV%':>6} {'n':>3} "
                   f"{'vsbase':>7} {'p99us':>8}")
            print(hdr)
            if md:
                print("|---:|---|---:|---:|---:|---:|---:|")
            for thr in thrs:
                bkey = (envk, mode, thr, "base")
                bmed = statistics.median(data[bkey]) if bkey in data else None
                for arm in ARMS:
                    k = (envk, mode, thr, arm)
                    if k not in data:
                        continue
                    v = data[k]
                    npoints += len(v)
                    med = statistics.median(v)
                    ratio = med / bmed if bmed else float("nan")
                    p99 = statistics.median(p99s[k])
                    if md:
                        print(f"| {thr} | {arm} | {med:,.0f} | {cv(v):.1f} | "
                              f"{len(v)} | {ratio:.3f}x | {p99:.1f} |")
                    else:
                        print(f"{thr:>4} {arm:<7} {med:>12,.0f} {cv(v):>6.1f} "
                              f"{len(v):>3} {ratio:>6.3f}x {p99:>8.1f}")
    # Noise floor: how far base2 (same binary as base) strays from base.
    devs = []
    for (envk, mode, thr, arm), v in data.items():
        if arm != "base2":
            continue
        bkey = (envk, mode, thr, "base")
        if bkey in data:
            devs.append(abs(statistics.median(v) /
                            statistics.median(data[bkey]) - 1.0) * 100.0)
    print()
    if devs:
        print(f"VERDICT noise-floor: base2-vs-base |deviation| "
              f"max={max(devs):.1f}% median={statistics.median(devs):.1f}% "
              f"over {len(devs)} cells")
    else:
        print("VERDICT noise-floor: NOT MEASURED (no base2 arm present)")
    print(f"VERDICT pinrm-report: {npoints} measured points across "
          f"{len(files)} file(s), {len(fails)} FAIL line(s)")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
