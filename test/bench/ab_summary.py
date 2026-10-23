#!/usr/bin/env python3
# ab_summary.py -- summarise ab_hash_conc.sh raw.tsv into median + CV per cell
# and an A/B comparison table (base vs branch), plus the noise floor.
#
# CV = stdev/mean of the per-rep ops_per_s within one (arm,type,mix,txn,thr)
# cell.  A cell is quoted only when BOTH arms have CV <= CV_MAX (default 10%).
# Usage: ab_summary.py raw.tsv [cv_max_percent]
import sys, statistics, collections

def main():
    raw = sys.argv[1]
    cv_max = float(sys.argv[2]) if len(sys.argv) > 2 else 10.0
    rows = []
    with open(raw) as f:
        hdr = f.readline().rstrip("\n").split("\t")
        for line in f:
            p = line.rstrip("\n").split("\t")
            if len(p) != len(hdr):
                continue
            rows.append(dict(zip(hdr, p)))

    # The throughput metric is 'goodput_per_s' in the corrected harness and
    # 'ops_per_s' in the original (attempts) one; pick whichever is present.
    thr_metric = "goodput_per_s" if "goodput_per_s" in hdr else "ops_per_s"
    metrics = [thr_metric, "p50_us", "p99_us", "p999_us", "max_us",
               "deadlocks", "lock_waits"]
    if "aborts_per_s" in hdr:
        metrics.append("aborts_per_s")

    # group by (arm,type,mix,txn,thr)
    cells = collections.defaultdict(lambda: collections.defaultdict(list))
    for r in rows:
        key = (r["arm"], r["type"], r["mix"], r["txn"], int(r["thr"]))
        for m in metrics:
            cells[key][m].append(float(r[m]))

    def med(key, metric):
        return statistics.median(cells[key][metric])
    def cv(key, metric=thr_metric):
        v = cells[key][metric]
        if len(v) < 2:
            return 0.0
        m = statistics.mean(v)
        return 0.0 if m == 0 else 100.0 * statistics.pstdev(v) / m

    has_aborts = "aborts_per_s" in hdr
    out = []
    out.append("## RFC 0017 change 1 -- hash_conc_bench A/B (base=origin/master, branch)\n")
    lbl = "goodput (committed ops/s)" if thr_metric == "goodput_per_s" \
        else "ops/s (attempts, incl. aborted txns)"
    out.append(f"Metric: {lbl}. Median of per-rep values; CV = stdev/mean "
               f"within a cell. Cells with CV > {cv_max:.0f}% in either arm "
               f"are marked low-confidence.\n")

    arms = set(r["arm"] for r in rows)
    combos = []
    for type_ in ("hash","btree"):
        for mix in ("insert","lookup"):
            for txn in ("txn","notxn"):
                combos.append((type_,mix,txn))

    for (type_,mix,txn) in combos:
        thrs = sorted(set(k[4] for k in cells if k[1]==type_ and k[2]==mix and k[3]==txn))
        if not thrs:
            continue
        out.append(f"\n### {type_} / {mix} / {txn}\n")
        if has_aborts:
            out.append("| thr | base good/s | branch good/s | branch/base | base CV% | branch CV% | branch p99 us | base p99 us | branch abort/s | base abort/s | conf |")
            out.append("|----:|------------:|--------------:|------------:|---------:|-----------:|--------------:|------------:|---------------:|-------------:|:----:|")
        else:
            out.append("| thr | base ops/s | branch ops/s | branch/base | base CV% | branch CV% | branch p99 us | base p99 us | branch dlk | base dlk | conf |")
            out.append("|----:|-----------:|-------------:|------------:|---------:|-----------:|--------------:|------------:|-----------:|---------:|:----:|")
        for thr in thrs:
            bk = ("base",type_,mix,txn,thr)
            rk = ("branch",type_,mix,txn,thr)
            if bk not in cells or rk not in cells:
                continue
            bops, rops = med(bk,thr_metric), med(rk,thr_metric)
            bcv, rcv = cv(bk), cv(rk)
            ratio = (rops/bops) if bops else 0
            conf = "ok" if (bcv<=cv_max and rcv<=cv_max) else "LOW"
            if has_aborts:
                out.append(f"| {thr} | {bops:,.0f} | {rops:,.0f} | {ratio:.3f} | "
                           f"{bcv:.1f} | {rcv:.1f} | {med(rk,'p99_us'):.1f} | "
                           f"{med(bk,'p99_us'):.1f} | {med(rk,'aborts_per_s'):,.0f} | "
                           f"{med(bk,'aborts_per_s'):,.0f} | {conf} |")
            else:
                out.append(f"| {thr} | {bops:,.0f} | {rops:,.0f} | {ratio:.3f} | "
                           f"{bcv:.1f} | {rcv:.1f} | {med(rk,'p99_us'):.1f} | "
                           f"{med(bk,'p99_us'):.1f} | {med(rk,'deadlocks'):.0f} | "
                           f"{med(bk,'deadlocks'):.0f} | {conf} |")

    # Noise floor: noiseA vs noiseB (same binary), if present.
    if any(k[0]=="noiseA" for k in cells):
        out.append("\n### Noise floor (base vs base, hash/insert/txn)\n")
        out.append("| thr | A ops/s | B ops/s | B/A | A CV% | B CV% |")
        out.append("|----:|--------:|--------:|----:|------:|------:|")
        thrs = sorted(set(k[4] for k in cells if k[0]=="noiseA"))
        for thr in thrs:
            ak = ("noiseA","hash","insert","txn",thr)
            bk = ("noiseB","hash","insert","txn",thr)
            if ak not in cells or bk not in cells:
                continue
            a,b = med(ak,thr_metric), med(bk,thr_metric)
            out.append(f"| {thr} | {a:,.0f} | {b:,.0f} | {(b/a if a else 0):.3f} | "
                       f"{cv(ak):.1f} | {cv(bk):.1f} |")

    print("\n".join(out))

if __name__ == "__main__":
    main()
