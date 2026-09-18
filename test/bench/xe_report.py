#!/usr/bin/env python3
"""
xe_report.py -- aggregate the cross-engine campaign TSV into medians, CVs and
a noise floor, and refuse to call anything a difference unless it clears that
floor.

Reads the streamed TSV from xe_run.sh (one row per metric per run) and emits:

  * median and CV per (workload, arm, threads, metric)
  * the NOISE FLOOR, measured by running one arm against itself: the spread of
    the SAME configuration across reps.  Any cross-arm difference inside that
    band is a NULL RESULT and is labelled as such.
  * latency percentiles per transaction type
  * the out-of-cache evidence (cache hit rate, read amplification)

The reason this is a separate program from the runner: the runner should only
measure, so that re-analysing does not mean re-running.  The reason it computes
a noise floor at all: this project has retracted benchmark claims, and the
retraction pattern is always the same -- a ratio reported without knowing what
ratio the machine produces when nothing changes.

usage: xe_report.py results.tsv [--md] [--floor-arm ARM]
"""
import sys
import statistics
from collections import defaultdict


def load(path):
    rows = []
    with open(path) as f:
        for line in f:
            line = line.rstrip("\n")
            if not line or line.startswith("#"):
                continue
            p = line.split("\t")
            if len(p) != 9 or p[0] == "workload":
                continue
            wl, arm, engine, am, aio, threads, rep, metric, value = p
            try:
                v = float(value)
            except ValueError:
                # Non-numeric metrics (steady_state) are kept as strings.
                rows.append(dict(wl=wl, arm=arm, engine=engine, am=am,
                                 aio=aio, threads=threads, rep=rep,
                                 metric=metric, value=value, num=None))
                continue
            rows.append(dict(wl=wl, arm=arm, engine=engine, am=am, aio=aio,
                             threads=int(threads), rep=int(rep),
                             metric=metric, value=value, num=v))
    return rows


def cv(vals):
    """Coefficient of variation, percent.  Returns None for n<2."""
    if len(vals) < 2:
        return None
    m = statistics.mean(vals)
    if m == 0:
        return None
    return 100.0 * statistics.stdev(vals) / m


def agg(rows, metric):
    """(wl, arm, threads) -> list of values for one metric."""
    out = defaultdict(list)
    for r in rows:
        if r["metric"] == metric and r["num"] is not None:
            out[(r["wl"], r["arm"], r["threads"])].append(r["num"])
    return out


def noise_floor(rows, metric):
    """
    The noise floor: for each (workload, arm, threads) with >=2 reps, the CV of
    that single configuration.  The floor we report is the MAXIMUM such CV,
    because a difference must clear the worst same-config spread before it can
    be called real, not merely the average one.
    """
    per = agg(rows, metric)
    floors = {}
    for (wl, arm, t), vals in per.items():
        c = cv(vals)
        if c is not None:
            floors[(wl, arm, t)] = (c, len(vals))
    return floors


def fmt(v, prec=1):
    if v is None:
        return "n/a"
    return f"{v:.{prec}f}"


def main():
    if len(sys.argv) < 2:
        print(__doc__)
        return 1
    path = sys.argv[1]
    md = "--md" in sys.argv
    rows = load(path)
    if not rows:
        print("FAIL no data rows in " + path)
        return 1

    # ---- refuse to mix experiments -------------------------------------
    # Every row must come from a run with the same scale/pad/cache.  A results
    # directory that accumulates logs from smoke tests and campaign runs will
    # otherwise average them, and the result looks like a real effect.  Abort
    # loudly with the offending configs rather than reporting a blended number.
    cfgs = {}
    for r in rows:
        if r["metric"] in ("cfg_scale", "cfg_pad", "cfg_cache_mb"):
            cfgs.setdefault((r["wl"], r["arm"], r["threads"], r["rep"]),
                            {})[r["metric"]] = r["value"]
    distinct = {tuple(sorted(v.items())) for v in cfgs.values()}
    if len(distinct) > 1:
        print("FAIL results mix MORE THAN ONE experiment configuration:")
        for d in sorted(distinct):
            print("   ", dict(d))
        print("Refusing to aggregate: averaging runs with different scale, "
              "padding or cache size produces a number that describes no "
              "experiment.  Separate the logs by config and re-run.")
        return 1
    if distinct:
        print(f"# config (uniform across all runs): {dict(list(distinct)[0])}")

    workloads = sorted({r["wl"] for r in rows})
    print(f"# rows parsed: {len(rows)}")

    for wl in workloads:
        wrows = [r for r in rows if r["wl"] == wl]
        metric = "txn_per_sec" if wl == "c" else "queries_per_sec"
        per = agg(wrows, metric)
        if not per:
            print(f"\n## workload {wl}: NO {metric} DATA -- nothing measured")
            continue

        arms = sorted({k[1] for k in per})
        threads = sorted({k[2] for k in per})

        # ---- noise floor ------------------------------------------------
        floors = noise_floor(wrows, metric)
        floorvals = [c for (c, n) in floors.values() if n >= 2]
        maxfloor = max(floorvals) if floorvals else None
        medfloor = statistics.median(floorvals) if floorvals else None

        print(f"\n## workload {wl} -- {metric}")
        print(f"\nNOISE FLOOR (same config across reps): "
              f"median CV {fmt(medfloor)}%, MAX CV {fmt(maxfloor)}% "
              f"over {len(floorvals)} configs")
        if maxfloor is not None:
            print(f"  -> any cross-arm ratio within +/-{fmt(maxfloor)}% is a "
                  f"NULL RESULT")

        # ---- per-arm table ---------------------------------------------
        hdr = ["threads"] + arms
        print()
        if md:
            print("| " + " | ".join(hdr) + " |")
            print("|" + "|".join(["---"] * len(hdr)) + "|")
        else:
            print("  ".join(f"{h:>22}" for h in hdr))
        for t in threads:
            cells = [str(t)]
            for a in arms:
                vals = per.get((wl, a, t), [])
                if not vals:
                    cells.append("-")
                    continue
                m = statistics.median(vals)
                c = cv(vals)
                cells.append(f"{m:,.0f} (CV {fmt(c)}%, n={len(vals)})")
            if md:
                print("| " + " | ".join(cells) + " |")
            else:
                print("  ".join(f"{c:>22}" for c in cells))

        # ---- cross-arm verdicts, floor-aware ---------------------------
        print("\n### ratios vs the like-for-like baseline")
        base = "libdb-sync-btree"
        if any(k[1] == base for k in per):
            for a in arms:
                if a == base:
                    continue
                print(f"\n{a} / {base}:")
                for t in threads:
                    bv = per.get((wl, base, t), [])
                    av = per.get((wl, a, t), [])
                    if not bv or not av:
                        continue
                    bm, am_ = statistics.median(bv), statistics.median(av)
                    if bm == 0:
                        continue
                    ratio = am_ / bm
                    delta = 100.0 * (ratio - 1.0)
                    verdict = ""
                    if maxfloor is not None and abs(delta) <= maxfloor:
                        verdict = (f"  NULL RESULT (|{delta:+.1f}%| within "
                                   f"the {fmt(maxfloor)}% noise floor)")
                    print(f"  t={t:<4} {ratio:6.2f}x  ({delta:+6.1f}%)"
                          f"{verdict}")

        # ---- latency ----------------------------------------------------
        print("\n### latency percentiles (us), median across reps")
        lat = defaultdict(list)
        for r in wrows:
            if r["num"] is None:
                continue
            for pfx in ("p50_", "p99_", "p999_"):
                if r["metric"].startswith(pfx):
                    txn = r["metric"][len(pfx):]
                    lat[(r["arm"], r["threads"], txn, pfx.rstrip("_"))].append(
                        r["num"])
        txns = sorted({k[2] for k in lat})
        for a in arms:
            shown = False
            for t in threads:
                line = []
                for txn in txns:
                    p50 = lat.get((a, t, txn, "p50"), [])
                    p99 = lat.get((a, t, txn, "p99"), [])
                    p999 = lat.get((a, t, txn, "p999"), [])
                    if not p50:
                        continue
                    line.append(f"{txn} {statistics.median(p50):.0f}/"
                                f"{statistics.median(p99):.0f}/"
                                f"{statistics.median(p999):.0f}")
                if line:
                    if not shown:
                        print(f"\n  {a}  (p50/p99/p99.9)")
                        shown = True
                    print(f"    t={t:<4} " + "  ".join(line))

        # ---- out-of-cache evidence -------------------------------------
        print("\n### out-of-cache evidence")
        for a in arms:
            for t in threads:
                hr = [r["num"] for r in wrows
                      if r["arm"] == a and r["threads"] == t
                      and r["metric"] == "io_hit_rate_pct"
                      and r["num"] is not None]
                ra = [r["num"] for r in wrows
                      if r["arm"] == a and r["threads"] == t
                      and r["metric"] == "io_read_amp_pages_per_txn"
                      and r["num"] is not None]
                dg = [r["num"] for r in wrows
                      if r["arm"] == a and r["threads"] == t
                      and r["metric"] == "data_gib" and r["num"] is not None]
                if not hr:
                    continue
                print(f"  {a:24} t={t:<4} hit_rate="
                      f"{statistics.median(hr):7.3f}%  "
                      f"read_amp={statistics.median(ra) if ra else 0:7.3f} "
                      f"pages/txn  data={statistics.median(dg) if dg else 0:.1f} GiB")

        # ---- anomalies --------------------------------------------------
        stalls = [r for r in wrows if r["metric"] == "TIMEOUT_STALL"]
        noverd = [r for r in wrows if r["metric"] == "NO_VERDICT"]
        notsteady = [r for r in wrows if r["metric"] == "steady_state"
                     and r["value"] != "yes"]
        if stalls or noverd or notsteady:
            print("\n### anomalies (reported, not discarded)")
            for r in stalls:
                print(f"  STALL {r['arm']} t={r['threads']} rep={r['rep']} "
                      f"(timeout {r['value']}s) -- recorded as data: this is "
                      f"the S1 class under the feature being measured")
            for r in noverd:
                print(f"  NO_VERDICT {r['arm']} t={r['threads']} "
                      f"rep={r['rep']} -- run produced no throughput number, "
                      f"counted as failed not passed")
            ns = defaultdict(int)
            for r in notsteady:
                ns[(r["arm"], r["threads"])] += 1
            for (a, t), n in sorted(ns.items()):
                print(f"  STEADY-STATE NOT REACHED {a} t={t}: {n} run(s)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
