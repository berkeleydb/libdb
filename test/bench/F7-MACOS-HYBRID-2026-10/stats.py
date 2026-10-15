import csv, statistics as st, collections, sys, random
random.seed(1)
def load(p):
    d = collections.defaultdict(list)
    for r in csv.DictReader(open(p), delimiter='\t'):
        d[(r['bench'], int(r['threads']), r['arm'])].append(float(r['ops_per_sec']))
    return d
def boot_ci(p, h, n=4000):
    # bootstrap CI on the ratio of medians
    rs = sorted(st.median(random.choices(h, k=len(h))) / st.median(random.choices(p, k=len(p))) for _ in range(n))
    return rs[int(.025*n)], rs[int(.975*n)]
for path in sys.argv[1:]:
    d = load(path)
    print(f"== {path.split('/')[2]} ==")
    print(f"  {'bench':14}{'thr':>4}{'pthread':>12}{'hybrid':>12}{'ratio':>7}{'95% CI':>15}{'cv_p':>6}{'cv_h':>6}  verdict")
    for b, t in sorted({(b, t) for b, t, _ in d}):
        p, h = d[(b, t, 'pthread')], d[(b, t, 'hybrid')]
        mp, mh = st.median(p), st.median(h)
        lo, hi = boot_ci(p, h)
        cv = lambda x: 100 * st.pstdev(x) / st.mean(x)
        v = "faster" if lo > 1.0 else ("SLOWER" if hi < 1.0 else "~")
        if max(cv(p), cv(h)) >= 10: v += " (noisy)"
        print(f"  {b:14}{t:>4}{mp:>12.0f}{mh:>12.0f}{mh/mp:>7.2f}  [{lo:.2f},{hi:.2f}]{cv(p):>6.1f}{cv(h):>6.1f}  {v}")
