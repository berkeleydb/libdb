# RFC 0017 change 1: A/B results

## v2: lock the bucket, then re-map (current; supersedes everything below)

**Host:** c6id.8xlarge (32 vCPU, Intel Xeon 8375C), Debian 12, kernel 6.1. The
database and environment are on XFS on the local NVMe instance store
(`/dev/nvme1n1`, mounted noatime).

**Arms:** `master` = `origin/master` `411e58f13`; `v2` = this branch
`95b5ed4fe`. Both use the same configure (`--disable-shared CFLAGS=-O2`) and
the same `hash_conc_bench` source. The cache is 4 GB, so the whole database is
resident.

**Method:**
- Alternating runs: the first arm rotates every rep.
- 5 reps per cell, 5 s each, keyspace 400k, ffactor 4. `hash/lookup/txn` was
  re-run with 9 reps.
- Goodput counts committed ops only. Aborts are reported separately.
- Raw data: `rfc0017-v2-ab.tsv`, `rfc0017-v2-ab-lookup-txn-9rep.tsv` and
  `rfc0017-v2-noise-floor.tsv`.

**Correctness, same commit:**
- `test/db` tier on an `--enable-diagnostic` build: 43/43 pass (the lock-order
  checker is on), manifest gate OK.
- The sabotage arm (validation off) loses 13697 of 13715 committed keys, and
  `db_verify` fails.
- Region signature: `0x841f4ea8` on both arms.

### All cells, 5 reps

```
cell                     n   master   cv%       v2   cv%  v2/m  abrt/s m->v2   p99us m->v2
btree/insert/txn/1       5    24573   0.1    24555   1.7  1.00      0->0         218->218   
btree/insert/txn/2       5    23363   1.0    23122   1.3  0.99      0->0         263->264   
btree/insert/txn/4       5    45712   0.6    45507   1.7  1.00      0->0         263->263   
btree/insert/txn/8       5    70600   1.4    70403   2.6  1.00      0->0         282->281   
btree/insert/txn/16      5    91771   3.1    92280   4.4  1.01      0->0         339->339   
btree/insert/txn/32      5    85529   2.8    87039   2.6  1.02      0->0         653->646   
btree/lookup/txn/1       5   555557   0.5   559309   1.1  1.01      0->0           2->2     
btree/lookup/txn/2       5   599979   2.9   623139   2.9  1.04      0->0           4->4     
btree/lookup/txn/4       5   883318   2.0   906306   1.8  1.03      0->0           7->6     
btree/lookup/txn/8       5   705110   7.9   711780   5.8  1.01      0->0          25->25    
btree/lookup/txn/16      5   353879   4.4   360583   4.8  1.02      0->0         134->132   
btree/lookup/txn/32      5   316534   3.0   323870   7.7  1.02      0->0         363->366   
hash/insert/notxn/1      5    13494   4.8    13474   2.2  1.00      0->0         246->247   
hash/insert/notxn/2      5    20039   3.9    21181   1.4  1.06      0->0         325->287   
hash/insert/notxn/4      5    26023   4.2    29633   3.8  1.14    209->0         430->394   
hash/insert/notxn/8      5    35795   3.7    43528   5.6  1.22    628->0         545->624   
hash/insert/notxn/16     5    40205   4.9    53081   4.4  1.32   1026->33        799->1377  
hash/insert/notxn/32     5    41236   1.9    60120   5.1  1.46   2457->98       1391->3324  
hash/insert/txn/1        5    13463   3.4    13497   3.2  1.00      0->0         247->247   
hash/insert/txn/2        5    19855   3.0    21231   3.2  1.07      0->0         325->288   
hash/insert/txn/4        5    26020   3.3    29633   0.4  1.14    200->0         429->394   
hash/insert/txn/8        5    35367   6.1    43719   6.1  1.24    627->0         548->619   
hash/insert/txn/16       5    40070   4.2    52990   5.5  1.32    996->31        801->1376  
hash/insert/txn/32       5    41152   1.3    60045   3.5  1.46   2451->94       1397->3319  
hash/lookup/notxn/1      5   688250   3.0   768156   4.2  1.12      0->0           3->3     
hash/lookup/notxn/2      5   836621   2.5  1209589   1.2  1.45      0->0           3->2     
hash/lookup/notxn/4      5  1085067   4.1  1819860   3.4  1.68      0->0           6->4     
hash/lookup/notxn/8      5   674321  11.2  2495028   5.2  3.70      0->0          30->6      CV>10
hash/lookup/notxn/16     5   371758  10.9  2703501   3.4  7.27      0->0         132->13     CV>10
hash/lookup/notxn/32     5   322449  10.2  3753484   4.6 11.64      0->0         389->17     CV>10
hash/lookup/txn/1        5   503743   0.6   545746   0.7  1.08      0->0           6->5     
hash/lookup/txn/2        5   524549   3.9   620519   2.3  1.18      0->0           5->4     
hash/lookup/txn/4        5   784764   3.0   908375   2.2  1.16      0->0           7->6     
hash/lookup/txn/8        5   761727   1.4   657850   7.8  0.86      0->0          22->28    
hash/lookup/txn/16       5   361511   3.4   333669   7.2  0.92      0->0         130->143   
hash/lookup/txn/32       5   296805   6.5   302584   6.6  1.02      0->0         348->394   
```

### hash/lookup/txn, 9 reps

```
cell                     n   master   cv%       v2   cv%  v2/m  abrt/s m->v2   p99us m->v2
hash/lookup/txn/1        9   523822   3.6   575185   4.3  1.10      0->0           4->3     
hash/lookup/txn/2        9   520898   3.8   644812   2.2  1.24      0->0           5->4     
hash/lookup/txn/4        9   763068   2.1   909529   3.1  1.19      0->0           7->6     
hash/lookup/txn/8        9   762005   5.6   711398   5.5  0.93      0->0          21->25    
hash/lookup/txn/16       9   349855   5.0   364541   5.3  1.04      0->0         135->130   
hash/lookup/txn/32       9   313667   7.8   345529   6.5  1.10      0->0         341->336   
```

### Noise floor: master against master, hash/lookup/txn, 9 reps

| threads | B/A | CV% A / B |
|---:|---:|---|
| 1 | 1.00 | 2.7 / 2.9 |
| 2 | 0.99 | 3.7 / 4.2 |
| 4 | 1.01 | 2.3 / 2.5 |
| 8 | 0.96 | 7.0 / 12.6 |
| 16 | 0.96 | 5.9 / 5.6 |
| 32 | 0.92 | 4.2 / 9.1 |

Two identical builds differ by up to 8% at 8–32 threads on this host. Ratios
inside that band are noise.

### Reading

- **Hash inserts (split-heavy):** 1.14–1.46× at 4–32 threads, for both txn and
  non-txn. Master aborts 200–2450 txns/s on metapage deadlocks; v2 aborts about
  0–100. p99 is higher at 16–32 threads (1.4 ms against 0.8 ms at t=16), with
  32–46% more committed work going through. Fewer ops queue on the metapage,
  and more of them queue on bucket and page latches.
- **Hash lookups, non-transactional:** 1.12–1.68× at 1–4 threads, and
  3.7–11.6× at 8–32. Master's lookups serialise on the metapage lock object;
  v2 takes no metapage lock. Master's CV is 10–11% there; v2's is ≤5.2%.
- **Hash lookups, transactional:** 1.10–1.24× at 1–4 threads. At 8–32 threads
  the result is 0.93–1.10×, inside the noise floor. A profile at 8 threads
  shows both arms spending about 60% in `__txn_begin`/`__txn_end` on the
  transaction region mutex. The hash code is no longer the bottleneck there,
  so removing the metapage lock cannot help further.
- **B-tree control:** 0.99–1.04× in every cell, so no regression.

---

## SUPERSEDED AND INVALID: v1 runs (kept for the record)

All numbers below were measured with a 512 MB cache, but the ffactor-4 hash
database is about 1 GB. Those runs spent up to 93% of their time evicting
buffers (`__memp_alloc`), not in hash locking. They also measured v1, which
refilled its cache under a metapage lock on every operation. Do not use them.

### v1, corrected goodput A/B (2026-10-23)

The first A/B below counted every attempt as throughput, including
deadlock-aborted transactions. That inflated base, which deadlocks under
split-heavy inserts while the branch does not. `hash_conc_bench` now counts
only committed ops (goodput) and reports aborts separately. The corrected
re-run is in `rfc0017-ab-goodput.tsv`. **It is partial.** The EC2 account
expired mid-run: hash cells have 9/9 reps, the btree control 2-3 reps, and the
lookup cells were not re-run (they had no deadlocks).

| cell (goodput ops/s) | base | CV% | branch | CV% | branch/base | base aborts/s | base→branch p99 µs |
|---|---:|---:|---:|---:|---:|---:|---|
| hash insert txn t=1  | 11989 | 5.6 | 12556 | 4.1 | 1.05 | 0    | 252→251 |
| hash insert txn t=2  | 18445 | 1.5 | 17867 | 1.2 | 0.97 | 0    | 328→325 |
| hash insert txn t=4  | 23905 | 4.8 | 24439 | 2.2 | 1.02 | 182  | 451→447 |
| hash insert txn t=8  | 35306 | 4.0 | 34933 | 5.9 | 0.99 | 459  | 633→679 |
| hash insert txn t=16 | 38012 | 4.1 | 33885 | 1.0 | **0.89** | 971  | 879→**1511** |
| hash insert txn t=32 | 39948 | 1.7 | 33869 | 1.7 | **0.85** | 1941 | 1497→**3396** |
| hash insert notxn t=16 | 37348 | 6.5 | 33923 | 0.8 | **0.91** | 951 | 885→1523 |
| hash insert notxn t=32 | 39885 | 2.1 | 33520 | 0.9 | **0.84** | 1985 | 1491→3391 |
| btree insert txn t=1..32 | — | ≤3.0 | — | ≤2.5 | 0.97–1.02 | 0 | unchanged |

**Verdict: change 1 is a measured negative for split-heavy inserts.** Counting
goodput, the branch is 11–16% slower at 16–32 threads and its p99 is 1.7–2.3×
worse. The earlier "branch p99 is 2× better" came from counting fast-failing
aborted attempts and is withdrawn. At ≤8 threads hash inserts are within noise.
The B-tree control is unchanged. Likely cause, not yet proven: with the metapage
no longer fencing, inserters pile onto a splitting bucket and its page latches
instead of being refused early by a deadlock abort. Change 1 alone does not meet
the RFC's "gain without regression" bar.

---

### v1, first A/B (attempts, including aborted txns)

## RFC 0017 change 1 -- hash_conc_bench A/B (base=origin/master, branch)

Median of per-rep throughput; CV = stdev/mean within a cell. Cells with CV > 10% in either arm are marked low-confidence.


### hash / insert / txn

| thr | base ops/s | branch ops/s | branch/base | base CV% | branch CV% | branch p99 us | base p99 us | branch dlk | base dlk | conf |
|----:|-----------:|-------------:|------------:|---------:|-----------:|--------------:|------------:|-----------:|---------:|:----:|
| 1 | 10,770 | 10,484 | 0.973 | 10.4 | 13.1 | 271.0 | 268.5 | 0 | 0 | LOW |
| 2 | 15,901 | 14,427 | 0.907 | 6.1 | 11.0 | 423.2 | 345.4 | 0 | 0 | LOW |
| 4 | 21,041 | 20,952 | 0.996 | 9.3 | 14.3 | 579.5 | 566.3 | 0 | 798 | LOW |
| 8 | 27,913 | 25,475 | 0.913 | 10.5 | 14.9 | 881.2 | 747.3 | 0 | 3838 | LOW |
| 16 | 31,337 | 23,002 | 0.734 | 9.2 | 20.8 | 2281.7 | 1021.9 | 1 | 9322 | LOW |
| 32 | 37,278 | 26,209 | 0.703 | 7.8 | 16.1 | 4254.4 | 1599.5 | 4 | 16812 | LOW |

### hash / insert / notxn

| thr | base ops/s | branch ops/s | branch/base | base CV% | branch CV% | branch p99 us | base p99 us | branch dlk | base dlk | conf |
|----:|-----------:|-------------:|------------:|---------:|-----------:|--------------:|------------:|-----------:|---------:|:----:|
| 1 | 11,137 | 12,172 | 1.093 | 9.2 | 9.4 | 254.6 | 268.1 | 0 | 0 | ok |
| 2 | 16,347 | 16,500 | 1.009 | 4.2 | 8.6 | 333.3 | 356.4 | 0 | 0 | ok |
| 4 | 23,012 | 24,165 | 1.050 | 9.6 | 10.2 | 465.2 | 469.1 | 0 | 759 | LOW |
| 8 | 29,582 | 28,032 | 0.948 | 9.2 | 8.5 | 804.4 | 665.9 | 0 | 3779 | ok |
| 16 | 32,643 | 25,584 | 0.784 | 8.3 | 15.3 | 2053.4 | 941.7 | 1 | 8913 | LOW |
| 32 | 40,292 | 30,045 | 0.746 | 5.9 | 15.1 | 3719.9 | 1481.4 | 1 | 14498 | LOW |

### hash / lookup / txn

| thr | base ops/s | branch ops/s | branch/base | base CV% | branch CV% | branch p99 us | base p99 us | branch dlk | base dlk | conf |
|----:|-----------:|-------------:|------------:|---------:|-----------:|--------------:|------------:|-----------:|---------:|:----:|
| 1 | 190,197 | 180,710 | 0.950 | 2.0 | 37.9 | 97.6 | 97.6 | 0 | 0 | LOW |
| 2 | 307,961 | 284,176 | 0.923 | 3.5 | 35.7 | 119.5 | 114.7 | 0 | 0 | LOW |
| 4 | 229,182 | 245,588 | 1.072 | 7.5 | 38.7 | 408.0 | 471.6 | 0 | 0 | LOW |
| 8 | 111,681 | 111,502 | 0.998 | 4.3 | 37.5 | 2263.4 | 2307.3 | 0 | 0 | LOW |
| 16 | 57,984 | 61,973 | 1.069 | 7.0 | 37.9 | 8397.7 | 9234.4 | 0 | 0 | LOW |
| 32 | 58,741 | 59,568 | 1.014 | 6.1 | 38.2 | 17786.6 | 18693.4 | 0 | 0 | LOW |

### hash / lookup / notxn

| thr | base ops/s | branch ops/s | branch/base | base CV% | branch CV% | branch p99 us | base p99 us | branch dlk | base dlk | conf |
|----:|-----------:|-------------:|------------:|---------:|-----------:|--------------:|------------:|-----------:|---------:|:----:|
| 1 | 211,419 | 220,481 | 1.043 | 3.3 | 2.7 | 94.7 | 95.7 | 0 | 0 | ok |
| 2 | 349,950 | 376,811 | 1.077 | 4.3 | 3.8 | 112.9 | 122.4 | 0 | 0 | ok |
| 4 | 221,351 | 241,045 | 1.089 | 14.7 | 8.7 | 477.9 | 525.5 | 0 | 0 | LOW |
| 8 | 119,589 | 119,545 | 1.000 | 7.9 | 4.6 | 2196.8 | 2204.6 | 0 | 0 | ok |
| 16 | 67,023 | 66,068 | 0.986 | 8.1 | 5.5 | 8223.5 | 8117.0 | 0 | 0 | ok |
| 32 | 59,017 | 59,786 | 1.013 | 9.9 | 3.8 | 18480.6 | 18551.2 | 0 | 0 | ok |

### btree / insert / txn

| thr | base ops/s | branch ops/s | branch/base | base CV% | branch CV% | branch p99 us | base p99 us | branch dlk | base dlk | conf |
|----:|-----------:|-------------:|------------:|---------:|-----------:|--------------:|------------:|-----------:|---------:|:----:|
| 1 | 24,172 | 23,757 | 0.983 | 1.7 | 0.3 | 221.5 | 218.6 | 0 | 0 | ok |
| 2 | 23,155 | 22,956 | 0.991 | 1.6 | 1.4 | 264.9 | 263.1 | 0 | 0 | ok |
| 4 | 44,647 | 44,124 | 0.988 | 2.6 | 0.8 | 266.0 | 263.4 | 0 | 0 | ok |
| 8 | 67,139 | 66,890 | 0.996 | 2.8 | 2.4 | 286.4 | 286.6 | 0 | 0 | ok |
| 16 | 87,219 | 86,054 | 0.987 | 3.2 | 0.4 | 374.4 | 370.0 | 0 | 0 | ok |
| 32 | 82,184 | 82,111 | 0.999 | 3.2 | 4.2 | 674.9 | 675.6 | 0 | 0 | ok |

### btree / insert / notxn

| thr | base ops/s | branch ops/s | branch/base | base CV% | branch CV% | branch p99 us | base p99 us | branch dlk | base dlk | conf |
|----:|-----------:|-------------:|------------:|---------:|-----------:|--------------:|------------:|-----------:|---------:|:----:|
| 1 | 24,289 | 23,834 | 0.981 | 0.3 | 0.1 | 221.1 | 218.4 | 0 | 0 | ok |
| 2 | 23,242 | 22,713 | 0.977 | 1.0 | 0.9 | 266.9 | 262.9 | 0 | 0 | ok |
| 4 | 44,897 | 43,965 | 0.979 | 0.7 | 0.7 | 268.7 | 265.1 | 0 | 0 | ok |
| 8 | 69,385 | 66,674 | 0.961 | 0.5 | 1.6 | 286.6 | 283.5 | 0 | 0 | ok |
| 16 | 90,277 | 86,263 | 0.956 | 0.4 | 1.5 | 374.0 | 354.7 | 0 | 0 | ok |
| 32 | 85,492 | 81,380 | 0.952 | 0.4 | 1.9 | 677.9 | 653.8 | 0 | 0 | ok |

### btree / lookup / txn

| thr | base ops/s | branch ops/s | branch/base | base CV% | branch CV% | branch p99 us | base p99 us | branch dlk | base dlk | conf |
|----:|-----------:|-------------:|------------:|---------:|-----------:|--------------:|------------:|-----------:|---------:|:----:|
| 1 | 548,100 | 545,638 | 0.996 | 0.5 | 1.1 | 2.2 | 2.2 | 0 | 0 | ok |
| 2 | 598,870 | 607,754 | 1.015 | 0.7 | 2.3 | 4.0 | 4.0 | 0 | 0 | ok |
| 4 | 872,815 | 864,054 | 0.990 | 1.3 | 1.6 | 6.8 | 6.4 | 0 | 0 | ok |
| 8 | 704,902 | 687,864 | 0.976 | 3.9 | 3.6 | 26.5 | 25.5 | 0 | 0 | ok |
| 16 | 349,816 | 349,232 | 0.998 | 3.6 | 3.6 | 135.7 | 135.3 | 0 | 0 | ok |
| 32 | 304,840 | 307,755 | 1.010 | 4.1 | 3.9 | 370.6 | 374.1 | 0 | 0 | ok |

### btree / lookup / notxn

| thr | base ops/s | branch ops/s | branch/base | base CV% | branch CV% | branch p99 us | base p99 us | branch dlk | base dlk | conf |
|----:|-----------:|-------------:|------------:|---------:|-----------:|--------------:|------------:|-----------:|---------:|:----:|
| 1 | 776,072 | 780,931 | 1.006 | 1.0 | 0.2 | 1.6 | 1.6 | 0 | 0 | ok |
| 2 | 1,180,124 | 1,168,218 | 0.990 | 4.6 | 2.4 | 2.0 | 2.0 | 0 | 0 | ok |
| 4 | 1,803,878 | 1,869,438 | 1.036 | 5.3 | 4.7 | 2.7 | 3.4 | 0 | 0 | ok |
| 8 | 2,673,835 | 2,561,385 | 0.958 | 5.2 | 3.6 | 4.8 | 4.5 | 0 | 0 | ok |
| 16 | 3,521,845 | 3,494,129 | 0.992 | 1.6 | 3.3 | 7.6 | 7.5 | 0 | 0 | ok |
| 32 | 4,242,525 | 4,256,846 | 1.003 | 2.4 | 4.8 | 14.5 | 15.8 | 0 | 0 | ok |

### Noise floor (base vs base, hash/insert/txn)

| thr | A ops/s | B ops/s | B/A | A CV% | B CV% |
|----:|--------:|--------:|----:|------:|------:|
| 1 | 12,438 | 12,507 | 1.006 | 1.2 | 3.6 |
| 2 | 17,767 | 17,164 | 0.966 | 1.6 | 1.9 |
| 4 | 24,528 | 23,307 | 0.950 | 1.9 | 6.5 |
| 8 | 30,838 | 31,068 | 1.007 | 1.3 | 3.5 |
| 16 | 34,992 | 33,706 | 0.963 | 5.6 | 6.7 |
| 32 | 40,838 | 40,477 | 0.991 | 0.4 | 1.3 |
