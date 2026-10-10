# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# See the file LICENSE for redistribution information.
import statistics as st, itertools, random
def load(p):
    rows=[];hdr=None
    for line in open(p):
        if line.startswith('#'):continue
        q=line.rstrip('\n').split('\t')
        if hdr is None: hdr=q;continue
        rows.append(dict(zip(hdr,q)))
    return rows
def mw(a,b):
    """Exact two-sided Mann-Whitney U p-value via permutation of ranks.
    9v9 -> C(18,9)=48620 combos, cheap enough to enumerate exactly."""
    n1,n2=len(a),len(b)
    allv=sorted(a+b)
    # rank with ties averaged
    rk={}
    i=0
    while i<len(allv):
        j=i
        while j+1<len(allv) and allv[j+1]==allv[i]: j+=1
        r=(i+j)/2+1
        for k in range(i,j+1): rk[allv[k]]=r
        i=j+1
    R1=sum(rk[x] for x in a)
    U1=R1-n1*(n1+1)/2
    U=min(U1,n1*n2-U1)
    idx=list(range(n1+n2))
    cnt=0;tot=0
    for comb in itertools.combinations(idx,n1):
        s=set(comb)
        ga=[allv[i] for i in comb]
        Ra=sum(rk[x] for x in ga)
        Ua=Ra-n1*(n1+1)/2
        Uu=min(Ua,n1*n2-Ua)
        tot+=1
        if Uu<=U: cnt+=1
    return cnt/tot
rows=load('/tmp/rescue/p13mx.tsv')
d={}
for r in rows: d.setdefault((r['arm'],int(r['threads'])),[]).append(float(r['tpm']))
print("Exact Mann-Whitney two-sided p, 9 vs 9, tpm.  alpha=0.05")
print(f"{'t':>4} {'comparison':>22} {'median delta':>13} {'p':>9} {'verdict':>12}")
for t in (1,2,8,16,32,64):
    for a,b,lbl in (('P13','base','P13 vs base'),('P12','base','P12 vs base'),
                    ('P13P12','base','P13+P12 vs base'),('P13P12','P13','P13+P12 vs P13')):
        x,y=d[(a,t)],d[(b,t)]
        p=mw(x,y)
        delta=(st.median(x)/st.median(y)-1)*100
        v='significant' if p<0.05 else 'NOT sig'
        print(f"{t:>4} {lbl:>22} {delta:>+12.1f}% {p:>9.5f} {v:>12}")
