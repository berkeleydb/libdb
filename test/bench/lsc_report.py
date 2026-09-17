#!/usr/bin/env python3
"""lsc_report.py -- aggregate lsc_matrix.sh RESULT lines into medians + CV.

Reads one or more matrix logs on argv (or stdin) and prints, per
(arm, threads), the median and CV of each metric across reps.  CV is
stdev/median so a tight arm and a noisy one cannot be confused.

Never invents a rep: n= is printed on every row.
"""
import re
import statistics
import sys


def median_cv(vals):
    if not vals:
        return (float('nan'), float('nan'))
    m = statistics.median(vals)
    sd = statistics.stdev(vals) if len(vals) > 1 else 0.0
    return (m, 100.0 * sd / m if m else 0.0)


def main():
    files = sys.argv[1:]
    lines = []
    for f in files:
        # Tag reps with their source file: rep numbering restarts in each log,
        # so a bare rep= would silently merge rep 1 of two logs into one rep.
        lines += ['%s|%s' % (f, ln) for ln in open(f).read().splitlines()]
    if not files:
        lines = ['-|%s' % ln for ln in sys.stdin.read().splitlines()]

    # rows[(arm, t)][metric] = [values...]
    rows = {}
    for ln in lines:
        m = re.match(r'(.*)\|RESULT rep=(\d+) arm=(\w+) t=(\d+) (.*)', ln)
        if not m:
            continue
        rep = m.group(1) + '#' + m.group(2)
        arm, t, rest = m.group(3), int(m.group(4)), m.group(5)
        key = (arm, t)
        d = rows.setdefault(key, {})
        d.setdefault('_reps', set()).add(rep)
        if rest.startswith('mode='):
            for k, v in re.findall(r'(\w+)=([0-9.]+)', rest):
                if k in ('ops_sec', 'flush_per_commit', 'maxcpf', 'commits',
                         'p99_us', 'p50_us', 'p999_us'):
                    d.setdefault(k, []).append(float(v))
        elif rest.startswith('PHASE '):
            ph = rest.split()[1].replace('_us', '')
            for k, v in re.findall(r'(p\d+)=(\d+)', rest):
                d.setdefault('%s.%s' % (ph, k), []).append(float(v))
        elif rest.startswith('LOCKS '):
            for k, v in re.findall(r'(\w+)=(-?[0-9]+)', rest):
                d.setdefault('lk.' + k, []).append(float(v))
        elif rest.startswith('LOG '):
            for k, v in re.findall(r'(__\w+)=(\d+)', rest):
                d.setdefault('log.' + k, []).append(float(v))
        elif rest.startswith('BATCH '):
            for k, v in re.findall(r'(\w+)=([0-9.]+)', rest):
                d.setdefault('b.' + k, []).append(float(v))

    metrics = ['ops_sec', 'put.p50', 'put.p99', 'put.p999', 'commit.p50',
               'commit.p99', 'begin.p99', 'lk.wait_total', 'lk.wait_pg0',
               'lk.wait_other', 'lk.held_pages', 'lk.held_pg0',
               'flush_per_commit', 'maxcpf',
               'log.__bam_split', 'log.__db_pg_alloc', 'log.__txn_regop']
    print('%-9s %4s %4s %s' % ('arm', 't', 'n', ' '.join(
        '%20s' % m for m in metrics)))
    for (arm, t) in sorted(rows, key=lambda k: (k[0], k[1])):
        d = rows[(arm, t)]
        n = len(d.get('_reps', ()))
        cells = []
        for me in metrics:
            v = d.get(me, [])
            md, cv = median_cv(v)
            cells.append('%20s' % ('%.0f(%.1f%%)' % (md, cv)
                                   if v else '-'))
        print('%-9s %4d %4d %s' % (arm, t, n, ' '.join(cells)))

    # derived: splits and allocations per 1000 puts
    print()
    print('%-9s %4s %4s %12s %12s %14s' % ('arm', 't', 'n', 'splits/1k',
                                           'allocs/1k', 'allocs/sec'))
    for (arm, t) in sorted(rows, key=lambda k: (k[0], k[1])):
        d = rows[(arm, t)]
        n = len(d.get('_reps', ()))
        sp = d.get('log.__bam_split', [])
        al = d.get('log.__db_pg_alloc', [])
        rg = d.get('log.__txn_regop', [])
        ops = d.get('ops_sec', [])
        if not rg:
            continue
        spk = [1000.0 * a / b for a, b in zip(sp, rg)] if sp else []
        alk = [1000.0 * a / b for a, b in zip(al, rg)] if al else []
        aps = ([o * a / 1000.0 for o, a in zip(ops, alk)]
               if alk and ops else [])
        print('%-9s %4d %4d %12s %12s %14s' % (
            arm, t, n,
            '%.1f' % statistics.median(spk) if spk else '-',
            '%.1f' % statistics.median(alk) if alk else '-',
            '%.0f' % statistics.median(aps) if aps else '-'))


if __name__ == '__main__':
    main()
