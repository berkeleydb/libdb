# Copyright (c) 2026 libdb contributors.  All rights reserved.
#
# SPDX-License-Identifier: Sleepycat
#
# See the file LICENSE for redistribution information.
import statistics as st
LAT=[('c_region','LOCK region lock'),('c_locker','LOCK locker-alloc'),
     ('c_part','LOCK partition'),('c_partmax','LOCK partition max-any-one'),
     ('c_objq','LOCK object-queue'),('c_confl_w','LOCK conflict, waited'),
     ('c_confl_nw','LOCK conflict, did not wait'),('c_deadlock','LOCK deadlocks'),
     ('x_region','MUTEX region lock'),('x_inuse','MUTEX in-use count'),
     ('x_maxinuse','MUTEX max in-use count')]
def load(p):
    rows=[];hdr=None
    for line in open(p):
        if line.startswith('#'):continue
        q=line.rstrip('\n').split('\t')
        if hdr is None:hdr=q;continue
        rows.append(dict(zip(hdr,q)))
    return rows
rows=load('/tmp/rescue/p13mx.tsv')
ARMS=['base','P13','P12','P13P12']
for key,lbl in LAT:
    src='db_stat -c' if key.startswith('c_') else 'db_stat -x'
    print(f"\n#### {lbl}  (`{key}`, {src}) -- per commit, median of 9")
    print("| t | base | P13 | P12 | P13+P12 |")
    print("|---:|---:|---:|---:|---:|")
    for t in (1,2,8,16,32,64):
        cells=[]
        for a in ARMS:
            sel=[r for r in rows if r['arm']==a and int(r['threads'])==t]
            v=st.median([float(r[key])/float(r['commits']) for r in sel])
            cells.append(f"{v:.5f}")
        print(f"| {t} | "+" | ".join(cells)+" |")
