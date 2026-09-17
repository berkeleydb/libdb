#!/usr/bin/env python3
"""opt_report.py -- aggregate an opt_ab.sh run into medians, CVs and a verdict.

Reads the TSV opt_ab.sh writes and prints, per thread count:

  base median + CV, opt median + CV, the ratio, the NOISE FLOOR
  (|base - base2| / base, the same-code arms measured against each other), and a
  verdict that is "WIN"/"REGRESS" only when the effect EXCEEDS the noise floor.
  Anything inside the floor prints "noise" -- a difference smaller than
  base-against-itself is not a result, and calling one a win is how a
  measurement lies.

Also prints pages/try (how many pages the optimistic descent actually read --
0 means the arm under test never ran and the row is meaningless) and the
validation rate per 1000 tries.

usage: opt_report.py results.tsv
"""
import re,sys,statistics as st
from collections import defaultdict
d=defaultdict(list); pg=defaultdict(list); inv=defaultdict(list)
for l in open(sys.argv[1]):
    m=re.search(r'tag=(\S+).*thr=(\d+).*keys_per_sec=(\d+).*opt_tries=(\d+) opt_pages=(\d+) opt_invalid=(\d+)',l)
    if not m: continue
    tag,thr=m.group(1),int(m.group(2))
    d[(tag,thr)].append(float(m.group(3)))
    if int(m.group(4)): pg[(tag,thr)].append(int(m.group(5))/int(m.group(4)))
    inv[(tag,thr)].append((int(m.group(6)),int(m.group(4))))
def cv(v):
    return 0.0 if len(v)<2 or st.median(v)==0 else 100*st.pstdev(v)/st.mean(v)
print(f"{'t':>4} {'base med':>10} {'CV%':>6} {'opt med':>10} {'CV%':>6} {'ratio':>7} {'noise':>7} {'verdict':>10}  pages/try inv/1k")
for thr in sorted({k[1] for k in d}):
    b=d.get(('base',thr),[])+d.get(('base2',thr),[])
    o=d.get(('opt',thr),[])+d.get(('opt2',thr),[])
    if not b or not o: continue
    b1,b2=d.get(('base',thr),[]),d.get(('base2',thr),[])
    noise=abs(st.median(b1)-st.median(b2))/st.median(b) if b1 and b2 else 0
    r=st.median(o)/st.median(b)
    v="WIN" if (r-1)>max(noise,0.02) else ("REGRESS" if (1-r)>max(noise,0.02) else "noise")
    p=pg.get(('opt',thr),[])+pg.get(('opt2',thr),[])
    iv=inv.get(('opt',thr),[])+inv.get(('opt2',thr),[])
    ivr=1000*sum(x[0] for x in iv)/max(1,sum(x[1] for x in iv))
    print(f"{thr:>4} {st.median(b)/1e6:>9.3f}M {cv(b):>5.1f}% {st.median(o)/1e6:>9.3f}M {cv(o):>5.1f}% {r:>7.3f} {100*noise:>6.1f}% {v:>10}  {st.median(p) if p else 0:>8.2f} {ivr:>6.3f}")
