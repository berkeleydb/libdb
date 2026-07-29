#!/usr/bin/env python3
# subsystem_breakdown.py <cov-src.info> -- per src/<subsys>/ line/branch/func table.
import sys, collections
def parse(path):
    subs=collections.defaultdict(lambda:{"la":0,"lh":0,"ba":0,"bh":0,"fa":0,"fh":0,"files":set()})
    cur=None;sub=None
    for ln in open(path):
        ln=ln.rstrip()
        if ln.startswith("SF:"):
            cur=ln[3:]
            if "/src/" in cur:
                sub=cur.split("/src/",1)[1].split("/")[0]
            else:
                sub="other"
            subs[sub]["files"].add(cur)
        elif ln.startswith("DA:") and sub:
            c=ln[3:].rsplit(",",1)[1]; subs[sub]["la"]+=1; subs[sub]["lh"]+=(c!="0")
        elif ln.startswith("BRDA:") and sub:
            t=ln.split(",")[-1]; subs[sub]["ba"]+=1; subs[sub]["bh"]+=(t not in("-","0"))
        elif ln.startswith("FNDA:") and sub:
            c=ln[5:].split(",",1)[0]; subs[sub]["fa"]+=1; subs[sub]["fh"]+=(c!="0")
    return subs
def main():
    subs=parse(sys.argv[1])
    rows=[]
    tot={"la":0,"lh":0,"ba":0,"bh":0,"fa":0,"fh":0}
    for s,d in subs.items():
        for k in tot: tot[k]+=d[k]
        lr=100.0*d["lh"]/d["la"] if d["la"] else 0
        br=100.0*d["bh"]/d["ba"] if d["ba"] else 0
        fr=100.0*d["fh"]/d["fa"] if d["fa"] else 0
        rows.append((lr,br,fr,d["la"],d["lh"],d["ba"],d["bh"],d["fa"],d["fh"],len(d["files"]),s))
    rows.sort(reverse=True)
    print(f"{'line%':>6} {'br%':>6} {'fn%':>6} {'lines':>6} {'lhit':>6} {'branch':>7} {'bhit':>6} {'fn':>5} {'fnhit':>5} {'files':>5}  subsystem")
    for r in rows:
        print(f"{r[0]:6.1f} {r[1]:6.1f} {r[2]:6.1f} {r[3]:6d} {r[4]:6d} {r[5]:7d} {r[6]:6d} {r[7]:5d} {r[8]:5d} {r[9]:5d}  {r[10]}")
    lr=100.0*tot["lh"]/tot["la"]; br=100.0*tot["bh"]/tot["ba"]; fr=100.0*tot["fh"]/tot["fa"]
    print(f"{lr:6.1f} {br:6.1f} {fr:6.1f} {tot['la']:6d} {tot['lh']:6d} {tot['ba']:7d} {tot['bh']:6d} {tot['fa']:5d} {tot['fh']:5d} {'':>5}  TOTAL")
if __name__=="__main__": main()
