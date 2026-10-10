#!/usr/bin/env python3
# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# See the file LICENSE for redistribution information.
"""F5 Linux sweep analysis: per-cell cv, per-arm mean, ON-vs-OFF delta.

ARM NAMING in f5_sweep.sh is the opposite of the intuitive reading:
  ARM=ON  -> env is F5_UNUSED=1  -> DB_LOCK_REFRESH_LOCK_MUTEX NOT set -> F5 FIX ACTIVE (new path)
  ARM=OFF -> DB_LOCK_REFRESH_LOCK_MUTEX=1                             -> OLD destroy+init path
So relabel to NEW / OLD to avoid ever reporting the wrong direction.
"""
import statistics as st
import sys

rows = []
for ln in open(sys.argv[1]):
    f = ln.split()
    if len(f) != 7 or f[0] == 'rep' or f[0].startswith('DONE'):
        continue
    rep, arm, t = int(f[0]), f[1], int(f[2])
    if f[3] == 'TIMEOUT':
        print(f"TIMEOUT cell rep={rep} arm={arm} t={t}")
        continue
    rows.append((rep, 'NEW' if arm == 'ON' else 'OLD', t,
                 float(f[3]), int(f[4]), int(f[5])))

threads = sorted({r[2] for r in rows})
print(f"reps={len({r[0] for r in rows})}  cells={len(rows)}")
print()
print(f"{'t':>3} {'arm':>4} {'n':>2} {'mean ops/s':>12} {'cv%':>7} "
      f"{'median':>12} {'min':>12} {'max':>12} {'deadlock/s':>11} {'lockwaits':>11}")
worst_cv = 0.0
summary = {}
for t in threads:
    for arm in ('NEW', 'OLD'):
        v = [r[3] for r in rows if r[2] == t and r[1] == arm]
        dl = [r[4] for r in rows if r[2] == t and r[1] == arm]
        lw = [r[5] for r in rows if r[2] == t and r[1] == arm]
        m = st.mean(v)
        cv = (st.stdev(v) / m * 100) if len(v) > 1 and m else 0.0
        worst_cv = max(worst_cv, cv)
        summary[(t, arm)] = (m, cv, len(v))
        print(f"{t:>3} {arm:>4} {len(v):>2} {m:>12,.0f} {cv:>7.2f} "
              f"{st.median(v):>12,.0f} {min(v):>12,.0f} {max(v):>12,.0f} "
              f"{st.mean(dl)/10:>11,.0f} {st.mean(lw):>11,.0f}")
print()
print(f"worst per-cell cv = {worst_cv:.2f}%  "
      f"({'UNDER 10% -> deltas quotable' if worst_cv < 10 else 'OVER 10% -> DO NOT QUOTE A DELTA'})")
print()
print(f"{'t':>3} {'NEW(fix)':>12} {'OLD(refresh)':>13} {'ratio':>7} {'delta%':>8} {'cvNEW':>6} {'cvOLD':>6}")
for t in threads:
    n, cvn, _ = summary[(t, 'NEW')]
    o, cvo, _ = summary[(t, 'OLD')]
    print(f"{t:>3} {n:>12,.0f} {o:>13,.0f} {n/o:>7.3f} {(n/o-1)*100:>+8.2f} "
          f"{cvn:>6.2f} {cvo:>6.2f}")
