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

    # group by (arm,type,mix,txn,thr) -> list of ops_per_s (and latencies)
    cells = collections.defaultdict(lambda: collections.defaultdict(list))
    for r in rows:
        key = (r["arm"], r["type"], r["mix"], r["txn"], int(r["thr"]))
        for m in ("ops_per_s","p50_us","p99_us","p999_us","max_us",
                  "deadlocks","lock_waits"):
            cells[key][m].append(float(r[m]))

    def med(key, metric):
        return statistics.median(cells[key][metric])
    def cv(key, metric="ops_per_s"):
        v = cells[key][metric]
        if len(v) < 2:
            return 0.0
        m = statistics.mean(v)
        return 0.0 if m == 0 else 100.0 * statistics.pstdev(v) / m

    out = []
    out.append("## RFC 0017 change 1 -- hash_conc_bench A/B (base=origin/master, branch)\n")
    out.append(f"Median of per-rep throughput; CV = stdev/mean within a cell. "
               f"Cells with CV > {cv_max:.0f}% in either arm are marked low-confidence.\n")

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
        out.append("| thr | base ops/s | branch ops/s | branch/base | base CV% | branch CV% | branch p99 us | base p99 us | branch dlk | base dlk | conf |")
        out.append("|----:|-----------:|-------------:|------------:|---------:|-----------:|--------------:|------------:|-----------:|---------:|:----:|")
        for thr in thrs:
            bk = ("base",type_,mix,txn,thr)
            rk = ("branch",type_,mix,txn,thr)
            if bk not in cells or rk not in cells:
                continue
            bops, rops = med(bk,"ops_per_s"), med(rk,"ops_per_s")
            bcv, rcv = cv(bk), cv(rk)
            ratio = (rops/bops) if bops else 0
            conf = "ok" if (bcv<=cv_max and rcv<=cv_max) else "LOW"
            out.append(f"| {thr} | {bops:,.0f} | {rops:,.0f} | {ratio:.3f} | "
                       f"{bcv:.1f} | {rcv:.1f} | {med(rk,'p99_us'):.1f} | "
                       f"{med(bk,'p99_us'):.1f} | {med(rk,'deadlocks'):.0f} | "
                       f"{med(bk,'deadlocks'):.0f} | {conf} |")

    # Noise floor: noiseA vs noiseB (same binary).
    out.append("\n### Noise floor (base vs base, hash/insert/txn)\n")
    out.append("| thr | A ops/s | B ops/s | B/A | A CV% | B CV% |")
    out.append("|----:|--------:|--------:|----:|------:|------:|")
    thrs = sorted(set(k[4] for k in cells if k[0]=="noiseA"))
    for thr in thrs:
        ak = ("noiseA","hash","insert","txn",thr)
        bk = ("noiseB","hash","insert","txn",thr)
        if ak not in cells or bk not in cells:
            continue
        a,b = med(ak,"ops_per_s"), med(bk,"ops_per_s")
        out.append(f"| {thr} | {a:,.0f} | {b:,.0f} | {(b/a if a else 0):.3f} | "
                   f"{cv(ak):.1f} | {cv(bk):.1f} |")

    print("\n".join(out))

if __name__ == "__main__":
    main()
