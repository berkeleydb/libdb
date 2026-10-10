# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# See the file LICENSE for redistribution information.
import statistics as st, glob, os
def load(p):
    rows=[]; hdr=None
    for line in open(p):
        if line.startswith('#'): continue
        parts=line.rstrip('\n').split('\t')
        if hdr is None: hdr=parts; continue
        rows.append(dict(zip(hdr,parts)))
    return rows
for scale in ('96','32'):
    print(f"\n=== scale {scale} shape gate (t=8 -> t=32, PASS >= -10%) ===")
    for arm in ('base','p13x','p12x','both'):
        p=f"/tmp/rescue/shape_{scale}_{arm}.tsv"
        rows=[r for r in load(p) if r['metric']=='tpmC_like']
        by={}
        for r in rows: by.setdefault(int(r['threads']),[]).append(float(r['value']))
        ts=sorted(by)
        m={t:st.median(by[t]) for t in ts}
        cv={t:(st.stdev(by[t])/st.mean(by[t])*100 if len(by[t])>1 else 0) for t in ts}
        if 8 in m and 32 in m:
            d=(m[32]/m[8]-1)*100
            v='PASS' if d>=-10 else 'FAIL'
            print(f"{arm:>6}: t8={m[8]:.0f} (n={len(by[8])},cv={cv[8]:.1f}%) t32={m[32]:.0f} (n={len(by[32])},cv={cv[32]:.1f}%)  {d:+.1f}% {v}")
            print(f"        all t: "+" ".join(f"{t}={m[t]:.0f}" for t in ts))
