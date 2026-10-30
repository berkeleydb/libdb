#!/usr/bin/env python3
# Copyright (c) 2026 libdb contributors.  All rights reserved.
# SPDX-License-Identifier: Sleepycat
# See the file LICENSE for redistribution information.
"""xrel_report.py -- summarise the cross-release benchmark TSVs.

For every cell (phase, workload, access method, thread count) print the
median over reps for each arm, its CV, and the ratios lts/bdb, master/bdb and
master/lts.  A ratio is flagged when either arm's CV exceeds 10%.

usage: xrel_report.py <out-dir> [phase ...]
"""
import csv
import os
import statistics as st
import sys
from collections import defaultdict

ARMS = ("bdb", "lts", "master")


def cv(v):
    return 100 * st.pstdev(v) / st.mean(v) if len(v) > 1 and st.mean(v) else 0.0


def rows(path):
    with open(path) as f:
        return list(csv.DictReader(f, delimiter="\t"))


def table(title, groups, keyfmt, extra=None):
    print(f"\n## {title}\n")
    hdr = "| cell | n | bdb | cv | lts | cv | master | cv | lts/bdb | master/bdb | master/lts |"
    if extra:
        hdr += " " + extra[0] + " |"
    print(hdr)
    print("|" + "---|" * (hdr.count("|") - 1))
    for key in sorted(groups):
        g = groups[key]
        if not all(a in g and g[a] for a in ARMS):
            continue
        m = {a: st.median(g[a]) for a in ARMS}
        c = {a: cv(g[a]) for a in ARMS}
        noisy = max(c.values()) > 10
        r = lambda x, y: (m[x] / m[y]) if m[y] else float("nan")
        fl = " ⚠" if noisy else ""
        line = (f"| {keyfmt(key)} | {len(g['bdb'])} | {m['bdb']:.0f} | {c['bdb']:.1f} | "
                f"{m['lts']:.0f} | {c['lts']:.1f} | {m['master']:.0f} | {c['master']:.1f} | "
                f"{r('lts','bdb'):.2f}{fl} | {r('master','bdb'):.2f}{fl} | {r('master','lts'):.2f}{fl} |")
        if extra:
            line += " " + extra[1](key) + " |"
        print(line)


def kv(out):
    p = os.path.join(out, "kv.tsv")
    if not os.path.exists(p):
        return
    g = defaultdict(lambda: defaultdict(list))
    ab = defaultdict(lambda: defaultdict(list))
    p99 = defaultdict(lambda: defaultdict(list))
    for x in rows(p):
        k = (x["am"], x["wl"], int(x["threads"]))
        g[k][x["arm"]].append(float(x["ops_per_s"]))
        ab[k][x["arm"]].append(float(x["aborts"]) / max(float(x["secs"]), 1))
        p99[k][x["arm"]].append(float(x["p99_us"]))
    def p99s(k):
        return " / ".join(f"{st.median(p99[k][a]):.0f}" for a in ARMS if p99[k][a])
    table("YCSB-style key/value, transactional (committed ops/s)", g,
          lambda k: f"{k[0]} {k[1]} t={k[2]}", ("p99 µs bdb/lts/master", p99s))


def tpcb(out):
    p = os.path.join(out, "tpcb.tsv")
    if not os.path.exists(p):
        return
    g = defaultdict(lambda: defaultdict(list))
    for x in rows(p):
        g[(x["durability"], int(x["threads"]))][x["arm"]].append(float(x["txn_per_s"]))
    table("TPC-B-like debit/credit (txn/s)", g, lambda k: f"{k[0]} t={k[1]}")


def tpcc(out):
    p = os.path.join(out, "tpcc.tsv")
    if not os.path.exists(p):
        return
    g = defaultdict(lambda: defaultdict(list))
    for x in rows(p):
        g[(int(x["threads"]),)][x["arm"]].append(float(x["tpmC_like"]))
    table("TPC-C-like (tpmC-like)", g, lambda k: f"t={k[0]}")


def micro(out):
    p = os.path.join(out, "micro.tsv")
    if not os.path.exists(p):
        return
    g = defaultdict(lambda: defaultdict(list))
    desc = {}
    for x in rows(p):
        try:
            v = float(x["ops_per_s"])
        except ValueError:
            continue
        k = (int(x["test"]),)
        g[k][x["arm"]].append(v)
        desc[k] = x["desc"][:70]
    table("Berkeley DB test_micro run.std (ops/s, single-threaded)", g,
          lambda k: f"{k[0]} {desc[k]}")


def main():
    out = sys.argv[1]
    phases = sys.argv[2:] or ["kv", "tpcb", "tpcc", "micro"]
    for ph in phases:
        {"kv": kv, "tpcb": tpcb, "tpcc": tpcc, "micro": micro}[ph](out)


if __name__ == "__main__":
    main()
