#!/usr/bin/env python3
"""Summarise a false_abort_sweep.sh CSV: medians, min/max, artifact fraction.

    ./false_abort_report.py false-abort.csv

Reports, per tag: reps, measured records-per-leaf, median/min/max ssi_abort,
deadlock, throughput, and -- for each page size -- the ARTIFACT FRACTION,

    artifact_fraction = decoy_ssi_abort_rate / ring_ssi_abort_rate

where the decoy arm has an EMPTY logical conflict graph (each worker reads a key
nobody writes), so a key-granularity SSI would abort zero transactions there and
100% of its aborts are false.  Rates, not counts: the two arms commit at
different throughputs, so counts are not directly comparable.
"""
import csv
import statistics
import sys
from collections import defaultdict


def med(xs):
    return statistics.median(xs) if xs else 0.0


def main(path):
    rows = defaultdict(list)
    with open(path) as fh:
        for r in csv.DictReader(fh):
            if r.get("workload") in (None, "FAILED"):
                continue
            rows[r["tag"]].append(r)

    def num(rs, k, cast=float):
        return [cast(r[k]) for r in rs]

    def stats(tag):
        rs = rows[tag]
        if not rs:
            return None
        tot = [
            float(r["commit"]) + float(r["ssi_abort"]) + float(r["deadlock"]) + float(r["other"])
            for r in rs
        ]
        rate = [
            100.0 * float(r["ssi_abort"]) / t if t else 0.0
            for r, t in zip(rs, tot)
        ]
        return dict(
            n=len(rs),
            rpl=med(num(rs, "recs_per_leaf")),
            ps=int(rs[0]["pagesize"]),
            valsz=int(rs[0]["valsz"]),
            ab=med(num(rs, "ssi_abort")),
            ab_lo=min(num(rs, "ssi_abort")),
            ab_hi=max(num(rs, "ssi_abort")),
            rate=med(rate),
            rate_lo=min(rate),
            rate_hi=max(rate),
            dl=med(num(rs, "deadlock")),
            other=med(num(rs, "other")),
            tps=med(num(rs, "txn_per_sec")),
        )

    print(f"# {path}: {sum(len(v) for v in rows.values())} rows, {len(rows)} points\n")
    hdr = (
        f"{'tag':<24} {'n':>2} {'ps':>6} {'vsz':>4} {'rpl':>7} "
        f"{'ssi_abort(med)':>14} {'[min..max]':>19} {'ab_rate%':>8} {'dl':>7} {'txn/s':>8}"
    )
    print(hdr)
    print("-" * len(hdr))
    for tag in sorted(rows, key=lambda t: (t.split("-")[0], int(rows[t][0]["pagesize"]), t)):
        s = stats(tag)
        span = f"{s['ab_lo']:.0f}..{s['ab_hi']:.0f}"
        print(
            f"{tag:<24} {s['n']:>2} {s['ps']:>6} {s['valsz']:>4} {s['rpl']:>7.2f} "
            f"{s['ab']:>14.0f} {span:>19} "
            f"{s['rate']:>8.2f} {s['dl']:>7.0f} {s['tps']:>8.0f}"
        )

    print("\n## Artifact fraction by page size (decoy rate / ring rate)\n")
    print(
        f"{'pagesize':>8} {'rpl':>7} {'ring ab%':>9} {'decoy ab%':>10} "
        f"{'artifact_frac':>13} {'ring dl':>8} {'decoy dl':>9}"
    )
    for ps in sorted({int(r["pagesize"]) for rs in rows.values() for r in rs}):
        rt = stats(f"ring-ps{ps}")
        dc = stats(f"decoy-ps{ps}")
        if not rt or not dc:
            continue
        frac = dc["rate"] / rt["rate"] if rt["rate"] else float("nan")
        print(
            f"{ps:>8} {rt['rpl']:>7.2f} {rt['rate']:>9.2f} {dc['rate']:>10.2f} "
            f"{frac:>13.3f} {rt['dl']:>8.0f} {dc['dl']:>9.0f}"
        )
    for tag in ("default",):
        rt, dc = stats(f"ring-{tag}"), stats(f"decoy-{tag}")
        if rt and dc:
            frac = dc["rate"] / rt["rate"] if rt["rate"] else float("nan")
            print(
                f"{rt['ps']:>8} {rt['rpl']:>7.2f} {rt['rate']:>9.2f} "
                f"{dc['rate']:>10.2f} {frac:>13.3f} {rt['dl']:>8.0f} "
                f"{dc['dl']:>9.0f}   <- shipped default (valsz 200)"
            )

    print("\n## Read-offset decay (decoy only; where false sharing dies)\n")
    for ps in (4096, 32768):
        pts = []
        for tag in rows:
            if tag.startswith(f"decoy-ps{ps}-off"):
                pts.append((int(tag.rsplit("off", 1)[1]), stats(tag)))
        if not pts:
            continue
        rpl = pts[0][1]["rpl"]
        print(f"pagesize {ps} (records-per-leaf {rpl:.2f}):")
        print(f"  {'read_off':>8} {'ab_rate%':>9} {'ssi_abort':>10} {'dl':>7} {'txn/s':>8}")
        for off, s in sorted(pts):
            mark = "  <- off >= rpl" if off >= rpl and (off / 2) < rpl else ""
            print(
                f"  {off:>8} {s['rate']:>9.2f} {s['ab']:>10.0f} {s['dl']:>7.0f} "
                f"{s['tps']:>8.0f}{mark}"
            )
        print()

    print("## Realistic read-modify-write: SSI premium over SI\n")
    print(
        f"{'workload':>10} {'pagesize':>8} {'rpl':>7} {'SSI ab%':>8} {'SI ab%':>7} "
        f"{'SSI-only%':>9} {'SSI txn/s':>10} {'SI txn/s':>9}"
    )
    for wl in ("uni", "zipf"):
        for ps in (512, 1024, 4096, 16384, 32768):
            ss, si = stats(f"rmw-{wl}-ps{ps}"), stats(f"rmw-{wl}-si-ps{ps}")
            if not ss:
                continue
            sir = si["rate"] if si else float("nan")
            print(
                f"{wl:>10} {ps:>8} {ss['rpl']:>7.2f} {ss['rate']:>8.3f} "
                f"{sir:>7.3f} {ss['rate'] - sir:>9.3f} {ss['tps']:>10.0f} "
                f"{(si['tps'] if si else 0):>9.0f}"
            )

    print("\n## SI controls (ssi_abort MUST be 0 -- validity check)\n")
    for tag in sorted(t for t in rows if "-si-" in t or t.endswith("-si")):
        s = stats(tag)
        flag = "" if s["ab"] == 0 else "   *** NONZERO -- INVALID ***"
        print(f"  {tag:<24} ssi_abort={s['ab']:.0f} deadlock={s['dl']:.0f}{flag}")


if __name__ == "__main__":
    main(sys.argv[1] if len(sys.argv) > 1 else "false-abort.csv")
