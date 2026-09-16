#!/usr/bin/env python3
"""Summarise a false-abort sweep CSV: medians, min/max, artifact fraction.

    ./false_abort_report.py false-abort.csv [false-abort-rmw.csv ...]

Works on the output of both false_abort_sweep.sh (ring arm) and
false_abort_rmw.sh (realistic arm); tags are auto-detected.

The derived number is the ARTIFACT FRACTION.  Every "decoy" arm has an EMPTY
logical conflict graph -- no transaction reads a key any transaction writes --
so a key-granularity SSI implementation would abort ZERO transactions there and
100% of the aborts it shows are false, caused purely by keys sharing a leaf page.
Comparing the decoy arm's abort RATE (not count: the arms commit at different
throughputs) with the matching genuine arm gives the fraction of that workload's
SSI aborts that page granularity, not the conflict graph, is responsible for.
"""
import csv
import re
import statistics
import sys
from collections import defaultdict

ROWS = defaultdict(list)


def load(paths):
    for p in paths:
        with open(p) as fh:
            for r in csv.DictReader(fh):
                if not r.get("workload") or r["workload"] == "FAILED":
                    continue
                ROWS[r["tag"]].append(r)


def med(xs):
    return statistics.median(xs) if xs else float("nan")


def stats(tag):
    rs = ROWS.get(tag)
    if not rs:
        return None
    f = lambda k: [float(r[k]) for r in rs]
    tot = [c + a + d + o for c, a, d, o in
           zip(f("commit"), f("ssi_abort"), f("deadlock"), f("other"))]
    rate = [100.0 * a / t if t else 0.0 for a, t in zip(f("ssi_abort"), tot)]
    dlrate = [100.0 * d / t if t else 0.0 for d, t in zip(f("deadlock"), tot)]
    return dict(
        n=len(rs), ps=int(rs[0]["pagesize"]), valsz=int(rs[0]["valsz"]),
        rpl=med(f("recs_per_leaf")), ab=med(f("ssi_abort")),
        ab_lo=min(f("ssi_abort")), ab_hi=max(f("ssi_abort")),
        rate=med(rate), rate_lo=min(rate), rate_hi=max(rate),
        dl=med(f("deadlock")), dlrate=med(dlrate), other=med(f("other")),
        tps=med(f("txn_per_sec")), panic=max(f("panicked")),
    )


def table():
    hdr = (f"{'tag':<26} {'n':>2} {'ps':>6} {'vsz':>4} {'rpl':>7} "
           f"{'ssi_abort':>10} {'[min..max]':>17} {'ab_rate%':>8} "
           f"{'dl':>6} {'txn/s':>7}")
    print(hdr)
    print("-" * len(hdr))
    for tag in sorted(ROWS, key=lambda t: (stats(t)["ps"], t)):
        s = stats(tag)
        span = f"{s['ab_lo']:.0f}..{s['ab_hi']:.0f}"
        print(f"{tag:<26} {s['n']:>2} {s['ps']:>6} {s['valsz']:>4} "
              f"{s['rpl']:>7.2f} {s['ab']:>10.0f} {span:>17} "
              f"{s['rate']:>8.3f} {s['dl']:>6.0f} {s['tps']:>7.0f}"
              + ("  *** PANIC ***" if s["panic"] else ""))


def pairs(genuine_fmt, decoy_fmt, base_fmt=None, label=""):
    """Artifact-fraction table over every page size both arms cover."""
    pss = sorted({stats(t)["ps"] for t in ROWS})
    out = []
    for ps in pss:
        g, d = stats(genuine_fmt.format(ps=ps)), stats(decoy_fmt.format(ps=ps))
        b = stats(base_fmt.format(ps=ps)) if base_fmt else None
        if not g or not d:
            continue
        out.append((ps, g, d, b))
    if not out:
        return
    print(f"\n## Artifact fraction -- {label}\n")
    cols = (f"{'pagesize':>8} {'recs/leaf':>9} {'genuine ab%':>11} "
            f"{'decoy ab%':>10} {'artifact_frac':>13}")
    if base_fmt:
        cols += f" {'baseline ab%':>12}"
    cols += f" {'decoy dl%':>9}"
    print(cols)
    for ps, g, d, b in out:
        frac = d["rate"] / g["rate"] if g["rate"] else float("nan")
        line = (f"{ps:>8} {g['rpl']:>9.2f} {g['rate']:>11.3f} "
                f"{d['rate']:>10.3f} {frac:>13.3f}")
        if base_fmt:
            line += f" {(b['rate'] if b else float('nan')):>12.3f}"
        line += f" {d['dlrate']:>9.3f}"
        print(line)


def offsets():
    byps = defaultdict(list)
    for tag in ROWS:
        m = re.match(r"decoy-ps(\d+)-off(\d+)$", tag)
        if m:
            byps[int(m.group(1))].append((int(m.group(2)), stats(tag)))
    for ps in sorted(byps):
        pts = sorted(byps[ps])
        rpl = pts[0][1]["rpl"]
        print(f"\n## Read-offset decay, pagesize {ps} "
              f"(records-per-leaf {rpl:.2f})\n")
        print(f"  {'read_off':>8} {'off/rpl':>8} {'ab_rate%':>9} "
              f"{'ssi_abort':>10} {'[min..max]':>17} {'dl':>5} {'txn/s':>7}")
        for off, s in pts:
            span = f"{s['ab_lo']:.0f}..{s['ab_hi']:.0f}"
            print(f"  {off:>8} {off / rpl:>8.2f} {s['rate']:>9.3f} "
                  f"{s['ab']:>10.0f} {span:>17} {s['dl']:>5.0f} "
                  f"{s['tps']:>7.0f}")


def si_controls():
    tags = sorted(t for t in ROWS if "-si-" in t or t.endswith("-si"))
    if not tags:
        return
    print("\n## SI controls -- ssi_abort MUST be 0 (validity check)\n")
    bad = 0
    for t in tags:
        s = stats(t)
        flag = "" if s["ab"] == 0 else "   *** NONZERO -- INVALID ***"
        bad += s["ab"] != 0
        print(f"  {t:<26} n={s['n']} ssi_abort={s['ab']:.0f} "
              f"deadlock={s['dl']:.0f}{flag}")
    print(f"  => {len(tags) - bad}/{len(tags)} controls clean")


def main(paths):
    load(paths)
    n = sum(len(v) for v in ROWS.values())
    print(f"# {' '.join(paths)}: {n} rows, {len(ROWS)} points, "
          f"{med([len(v) for v in ROWS.values()]):.0f} reps/point\n")
    table()
    # Ring arm (false_abort_sweep.sh).
    pairs("ring-ps{ps}", "decoy-ps{ps}", label="write-skew ring, by page size")
    g, d = stats("ring-default"), stats("decoy-default")
    if g and d:
        print(f"\n  shipped default (pagesize 1024, valsz 200, recs/leaf "
              f"{g['rpl']:.2f}): genuine {g['rate']:.3f}%  decoy "
              f"{d['rate']:.3f}%  artifact_frac "
              f"{d['rate'] / g['rate'] if g['rate'] else float('nan'):.3f}")
    offsets()
    # Realistic arm (false_abort_rmw.sh).
    for dist in ("uni", "zipf"):
        pairs(dist + "-plain-ps{ps}", dist + "-decoy-ps{ps}",
              dist + "-split-ps{ps}",
              label=f"realistic read-modify-write, {dist} "
                    f"(baseline = SSI_WSPLIT row-granularity ideal)")
    si_controls()


if __name__ == "__main__":
    main(sys.argv[1:] or ["false-abort.csv"])
