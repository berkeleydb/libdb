# SSI false-abort rate at page granularity (2026-09)

libdb implements Cahill's Serializable Snapshot Isolation as
`DB_TXN_SERIALIZABLE`, but it tracks rw-antidependencies at **page** granularity,
not key granularity. Two transactions that touch entirely different keys which
happen to land on the same btree leaf therefore produce a rw-edge that a
row-granularity SSI implementation (PostgreSQL's, for instance) would not — and
enough such phantom edges make a transaction a *pivot* and abort it with
`DB_SNAPSHOT_CONFLICT`. That abort is sound (aborting is always safe) but not
necessary. It is a **false abort**.

Michael Cahill's condition for judging RFC 0005 (key-precise conflict edges):
measure the false-abort rate first, because it *is* the performance lever, and
until it is measured the project cannot tell a user what SSI costs them.

This is that measurement. **No engine change** — the whole result comes from
`test/bench/ssi_abort_bench` plus new environment-variable knobs, because the
lever that controls records-per-leaf is already public API (`DB->set_pagesize`).

## The answer, up front

**On a realistic read-modify-write workload, essentially the entire SSI abort
rate is a page-granularity artifact — and the abort rate itself ranges from zero
measured aborts to 11.4% of transactions depending purely on how many records fit
on a leaf.**

| pagesize | records/leaf | SSI aborts (uniform) | of which false |
|---:|---:|---:|---:|
| 512 | 4.00 | 0.000% | n/a (nothing to attribute) |
| 1 024 | 10.00 | 0.018% | ~100% |
| 4 096 | 44.94 | 0.294% | ~100% |
| 8 192 | 90.91 | 1.101% | ~100% |
| 16 384 | 183.49 | 3.880% | ~100% |
| 32 768 | 370.37 | 11.352% | ~100% |

The abort rate scales with records-per-leaf across **three orders of magnitude**
(0.018% → 11.4% over the range where it is measurable at all, ~630×; the
512-byte point rounds to zero aborts entirely) on a workload whose *logical*
conflict structure never changes. The "of which false" column is not an estimate from a model: it is
a directly measured control (below).

## What makes "false" measurable rather than asserted

The problem with measuring a false-abort rate is that a genuine workload's aborts
are genuine and artifact mixed, and nothing in the engine labels them. The design
here removes the mixing instead of trying to untangle it: **hold the logical
conflict structure at EMPTY, and vary only the physical co-location of keys.**

Three arms per point, run back to back so machine drift hits all three equally:

- **plain** — writes and reads both drawn from the whole key space. Has a genuine
  conflict graph. This is the number a user would see.
- **decoy** (`SSI_WSTRIDE=8`) — writes go *only* to keys ≡ 0 mod 8, reads *only*
  to keys ≢ 0 mod 8. **No transaction ever reads a key any transaction writes.**
  The logical conflict graph is therefore empty and a key-granularity SSI would
  abort exactly **zero** transactions. Every abort observed here is false, by
  construction. The two key classes still interleave in key order, so they share
  leaves.
- **split** (`SSI_WSPLIT=1`) — same empty logical graph, but writes come from the
  lower half of the key space and reads from the upper half, so the two classes
  are *separated in key order* and only the single boundary leaf can be shared.
  This is the **row-granularity ideal**: the baseline.

The decisive comparison is decoy vs split. Both have an empty logical conflict
graph. The *only* difference between them is whether the read keys and write keys
sit on the same leaf pages. Measured (uniform, 8 threads, 20 000 keys, 5 s):

| pagesize | records/leaf | decoy (co-located) | split (separated) |
|---:|---:|---:|---:|
| 512 | 4.00 | 0.006% | 0.000% |
| 1 024 | 10.00 | 0.017% | 0.000% |
| 4 096 | 44.94 | 0.304% | 0.000% |
| 8 192 | 90.91 | 1.071% | 0.000% |
| 16 384 | 183.49 | 3.753% | 0.000% |
| 32 768 | 370.37 | 11.367% | 0.006% |

The split arm is **0.000% at five of the six page sizes** and 0.006% at the
sixth. So the aborts in the decoy arm are not some residual property of SSI, of
MVCC bookkeeping, or of the harness — they are page sharing, and nothing else.
And because plain ≈ decoy at every page size (artifact fraction 0.94–1.04 for
uniform, 0.75–1.05 for Zipfian), the *genuine* edges in the realistic workload
contribute almost nothing next to the artifact.

Re-run independently after the sweeps completed, as a fresh 3-rep A/B at the
decisive point (pagesize 32 768, uniform, 8 threads, 5 s), on a fresh environment
per run:

| arm | reps | ssi_abort median | [min..max] | abort rate |
|---|---:|---:|---:|---:|
| decoy (co-located) | 3 | 2 218 | 2 164..2 221 | **11.266%** |
| split (separated) | 3 | 0 | 0..1 | **0.000%** |

Same binary, same empty logical conflict graph, same records-per-leaf (370.37).
The only difference is whether the read keys and the write keys share leaves.

### The mechanism, isolated

The clearest single result is the read-offset decay on the write-skew ring. Each
worker reads a key `read_off` positions away from a key nobody else writes; the
logical graph is empty at every offset, so all aborts are false. Sweeping
`read_off` across measured records-per-leaf:

pagesize 4 096, records-per-leaf 44.77:

| read_off | off/recs-per-leaf | SSI abort rate |
|---:|---:|---:|
| 1 | 0.02 | 96.89% |
| 8 | 0.18 | 96.92% |
| 16 | 0.36 | 96.27% |
| 32 | 0.71 | 92.72% |
| **64** | **1.43** | **0.000%** |
| 128 | 2.86 | 0.000% |
| 256 | 5.72 | 0.000% |
| 512 | 11.44 | 0.000% |

pagesize 32 768, records-per-leaf 356.17:

| read_off | off/recs-per-leaf | SSI abort rate |
|---:|---:|---:|
| 1 | 0.00 | 96.96% |
| 32 | 0.09 | 96.90% |
| 64 | 0.18 | 96.94% |
| 128 | 0.36 | 96.29% |
| 256 | 0.72 | 93.01% |
| **512** | **1.44** | **0.000%** |

The two curves are the *same curve* in units of records-per-leaf: flat near 97%
while the read key is closer than one leaf, a first dip at off/rpl ≈ 0.36, a
larger one at ≈ 0.72, then a hard collapse to exactly zero once the offset
exceeds one leaf. The transition sits between off/rpl 0.72 and 1.43 in both, at
page sizes 8× apart and absolute offsets 8× apart. That is not a correlation with
page size, it is the mechanism: **the false-abort rate is a function of
records-per-leaf, and it is 0 the moment the keys stop sharing a leaf.** Nothing
else in the configuration changed — same key distribution, same schedule, same
thread count, `deadlock == 0` at every point.

`records-per-leaf` is measured, not assumed: `DB->stat` (full stat, not
`DB_FAST_STAT`) gives `bt_ndata / bt_leaf_pg` per run and it is a column in the
raw CSV. At the shipped bench default (pagesize 1024, 200-byte values) it is
**3.00**, confirming the `ssi_abort_bench` header's "~3 records per leaf" and
therefore `SSI_MIN_SPREAD = 8`.

## The ring saturates — an honest limit on one arm

The write-skew ring cannot produce a meaningful artifact *fraction*, and reporting
one would be misleading. Its abort rate is **saturated**: genuine 96.91%, decoy
96.88% at pagesize 32 768, artifact fraction **1.000 at every one of the six page
sizes**. Both arms are pinned near the ceiling, so the ratio is 1.0 by saturation,
not by measurement. Thread count does not rescue it (pagesize 4096, 3 s):

| threads | ring | decoy |
|---:|---:|---:|
| 2 | 98.36% | 98.33% |
| 4 | 98.12% | 98.11% |
| 8 | 96.97% | 96.98% |
| 16 | 94.90% | 94.75% |

The ring is a worst case by construction — every worker's transaction is designed
to be in a dangerous structure, and it re-runs that structure as fast as it can
in a tight loop. Its value here is the *offset decay* above (where the collapse to
exactly 0 is unambiguous and saturation is irrelevant), not a fraction. **The
realistic read-modify-write arm is where the fraction is meaningful**, and that is
the number quoted at the top.

## Deadlocks are counted separately, and they grow too

Page granularity has a second cost the SSI counter does not capture: page-level
**write-write** conflicts, which the deadlock detector resolves as
`DB_LOCK_DEADLOCK`. These are not SSI aborts and are never counted as such here,
but a user loses the transaction either way. In the decoy arm (empty logical
graph, uniform):

| pagesize | SSI false aborts | deadlocks |
|---:|---:|---:|
| 512 | 0.006% | 0.073% |
| 4 096 | 0.304% | 0.339% |
| 32 768 | 11.367% | 1.787% |

Under Zipfian access the deadlock rate dominates outright (40.1% at pagesize
32 768 vs 12.8% SSI aborts) — and the `split` control shows the same deadlock
growth with `ssi_abort = 0`, so it is page ww conflict, independent of SSI. Any
future evaluation of RFC 0005 must keep these separate: **key-precise SSI edges
would not remove the deadlocks**, because those come from the write-lock
protocol, not from the conflict-edge bookkeeping. RFC 0005 explicitly scopes
itself to the edges and leaves the page-lock protocol untouched, which means it
addresses the smaller of the two costs on a Zipfian workload.

## What this implies for RFC 0005

Reading the numbers rather than hoping for them:

1. **The mechanism is real, large, and precisely characterised.** Up to 11.4% of
   transactions aborted with `DB_SNAPSHOT_CONFLICT` on a workload with *no*
   logical conflicts at all. It is not a theoretical concern, and RFC 0005's
   premise is confirmed, not weakened, by measurement.

2. **But the user already has the lever, and it is one line.** `DB->set_pagesize`
   moves the false-abort rate from 11.4% to *zero measured aborts*. At 512-byte
   pages the realistic workload's SSI abort rate rounds to 0.000% — low enough
   that key-precise edges would buy nothing measurable. Anyone hurting from false
   aborts today is hurting because they run large pages, and the fix costs them a
   configuration change, not a new conflict-tracking subsystem. RFC 0005's own
   "Alternatives considered" listed this as the baseline every option must beat
   with numbers; **it now has the numbers, and the bar is high.**

3. **Smaller pages are not free, and that is the real trade.** They cost tree
   height, I/O amplification and cache efficiency — which this measurement did
   *not* quantify (see caveats). The honest framing is not "just use small
   pages", it is: the false-abort tax is a known, measured, documented function
   of a knob users already control, and RFC 0005's value is precisely the
   *decoupling* of that tax from the page-size choice. Whether that decoupling is
   worth a new lock-object identity, the phantom-prevention rule RFC 0005 itself
   flags as a blocking precondition, and the region-exhaustion risk is a
   judgement about how many users need both large pages and serializable
   isolation. This measurement cannot answer that, and it would be dishonest to
   claim it does.

4. **On a Zipfian workload RFC 0005 addresses the minority cost.** Deadlocks from
   page-level ww conflict exceed SSI false aborts by ~3× there (40.1% vs 12.8% at
   32 KB pages), and key-precise edges do not touch them. A proposal that fixed only the edges would leave most
   of the measured page-granularity tax in place for skewed workloads.

**Recommendation: do not build RFC 0005 on the strength of this number alone.**
Ship the documentation of the tax and the page-size guidance first — that is
nearly free and it is what a user actually needs today. Revisit key-precise edges
only with a concrete workload that (a) requires `DB_TXN_SERIALIZABLE`, (b) cannot
use small pages for an I/O reason that is itself measured, and (c) shows the
false-abort rate to be its real bottleneck. Cahill's instruction was to measure
before building; the measurement's verdict is that the cheap lever is
unexpectedly effective, so the expensive change needs a use case, not just a
mechanism.

## Incidental find: the regression gate was crashing on this bench

Validating that the new knobs did not disturb the shipped bench turned up a
pre-existing harness bug. `run_bench.sh` took `ssi_abort_bench`'s thread count
from `$1`, but commit `b3e3f669c` ("rework ssi_abort_bench to distinguish SI from
SSI") added a `level=` field *in front of* `threads=`. Since then the harness has
emitted

```
ssi_abort_bench  hot64  level=serializable  txn_per_sec ...
```

so every thread point collapses onto one label, and `bench_cmp.py`'s
`int(r["threads"])` raises an **uncaught `ValueError`** — the gate crashes on any
run that includes this bench, and the `ssi_abort_bench/hot64/t1|t8|t32|t96` rows
quoted in `NOISE.md` and `GATE-VERIFICATION.md` cannot be produced at all. Same
shape as the `mvcc_purge_visible` skip: a documented gate case that silently
stopped running, with nothing failing to say so. Fixed here by matching
`threads=` by name; verified in both directions.

## Method

| | |
|---|---|
| commit | `a7857c847` (`test/false-abort-rate` off master), library `libdb-2026.0` |
| instance | EC2 c7i-class, 96 vCPU, Debian, shared with other agents |
| build | `dist/configure --enable-shared --disable-static LIBS=-luring`, no diagnostic |
| driver | `test/bench/ssi_abort_bench.c` + `false_abort_sweep.sh` / `false_abort_rmw.sh` |
| analysis | `false_abort_report.py` (medians, min/max, artifact fraction) |
| reps | **5 per point, all 117 points** (`n` column in the report output) |
| window | 5 s measured per point, 8 threads |
| raw data | `results/ssi-false-abort-2026-09-c7i.csv` (585 rows, 0 failures) |

**Instrument verified before use.** The deterministic two-transaction
`--selfcheck` passes at both levels on this build: `ISO_LEVEL=snapshot` commits
both transactions to the skewed state `A==B==1` (SI permits write skew) and
`ISO_LEVEL=serializable` aborts exactly one with `DB_SNAPSHOT_CONFLICT`. The
measurement rests on a tool whose behaviour was re-confirmed, not assumed.

**SI controls: 37/37 clean.** Every configuration was also run at
`ISO_LEVEL=snapshot`, where `ssi_abort` **must** be 0 — a nonzero value would mean
the counter rather than the mechanism was being measured. All 37 control points
(5 reps each) reported exactly 0. The deadlock counter was nonzero in the same
runs, which is the point: it separates page ww loss from SSI aborts.

**Fresh environment per run.** A reused or recovered environment has previously
produced a phantom `ssi_abort > 0` in this codebase. Every single run gets its own
unique `SSI_ENV` directory, and it is removed afterwards *even when the run
fails*, so no point can inherit another's state. No run in the reported data
panicked (`panicked` column, 0 throughout) and no run timed out.

**Every invocation under `timeout`.** One hung point costs one point. **585 of 585
planned runs completed; zero timeouts, zero failures, zero panics.**

### The overflow trap, found by measurement

A first attempt swept page size at the bench's default 200-byte value and got a
non-monotonic records-per-leaf curve with sudden deadlocks. Cause: a value above
roughly `pagesize/4` goes to an **overflow page**, the leaf then holds only
pointers, and records-per-leaf *jumps*:

| pagesize | valsz | inline? | records/leaf | deadlocks |
|---:|---:|---|---:|---:|
| 512 | 64 | yes | 4.00 | 0 |
| 512 | 200 | **no** | 13.98 | 2 414 |
| 1 024 | 200 | yes | 3.00 | 0 |
| 1 024 | 400 | **no** | 29.90 | 2 236 |

So the sweep pins `SSI_VALSZ=64`, which stays inline at every page size ≥ 512,
giving a monotonic records-per-leaf curve with `deadlock == 0` throughout the ring
arm. This is exactly the hazard the `ssi_abort_bench` header warned about
("an overflow item would collapse all the pointers onto one leaf page") — now
quantified rather than avoided by convention.

## Caveats and uncertainty

Stated plainly, because several of them bound the conclusion:

1. **The split baseline is a baseline, not a proof of zero.** For the uniform
   workload it is 0.000% at five of six page sizes and 0.006% at the sixth, which
   is as clean as this kind of control gets. For **Zipfian** it is not: 0.149% at
   8 KB, 0.198% at 16 KB and **1.070%** at 32 KB. That residual is not negligible
   and its origin is not established here. The likely cause is that the split
   control is not perfectly matched under skew — Zipf puts the hottest keys at low
   indices, i.e. entirely inside the write half, so the write half is far hotter
   than the read half and the shared boundary leaf is not the only asymmetry.
   Treat the Zipfian artifact fractions (0.75–1.05) as approximate; the uniform
   ones are much better conditioned.
2. **Artifact fractions slightly above 1.0** (up to 1.05) are not physical — they
   mean the decoy arm measured a marginally *higher* rate than the genuine arm,
   which is run-to-run variation, not a real excess. Read them as
   "indistinguishable from 1.0", i.e. the genuine edges contribute little enough
   to be lost in the noise. The uniform 512-byte point reports `nan` because the
   genuine arm's median abort count was 0 (0.000%) while the decoy's was 0.006%;
   at that page size both are so close to zero that the ratio is meaningless and
   the honest statement is "both ≈ 0".
3. **The ring arm's artifact fraction is uninformative** (saturation), as stated
   above. It is reported for completeness, not as evidence. Its *offset decay* is
   the load-bearing ring result.
4. **One machine, one thread count for the main matrix** (8 threads, 96 vCPU box
   shared with other agents). The thread sensitivity check above covers 2–16 for
   the ring only. Absolute throughputs on a shared box are not trustworthy;
   *ratios between arms run back to back* are what this study rests on, and that
   is why the arms are alternated rather than batched.
5. **The cost of small pages is not measured here.** The recommendation leans on
   "use smaller pages" being cheap, and this study does not establish that. Tree
   height, I/O amplification and cache-efficiency costs of 512-byte pages are the
   obvious next measurement, and without it point 2 of the RFC 0005 discussion is
   a judgement, not a result.
6. **Two synthetic workloads.** A ring and a read-modify-write over 20 000 keys,
   both in cache. Real applications have range scans, mixed transaction sizes and
   secondary indices; phantom behaviour under scans (which RFC 0005 flags as its
   blocking correctness question) is not exercised at all.
7. **`other` is 0 in every row.** No transaction failed for a reason outside
   {commit, SSI abort, deadlock}, so no abort class is being silently absorbed —
   worth stating because a nonzero `other` would undermine every percentage here.

## Reproducing

```sh
cd test/bench
make BDB=../../build_unix ssi_abort_bench      # or compile directly, see header

# instrument check first
ISO_LEVEL=snapshot     SSI_ENV=/tmp/sc1 ./ssi_abort_bench --selfcheck
ISO_LEVEL=serializable SSI_ENV=/tmp/sc2 ./ssi_abort_bench --selfcheck

BENCH=./ssi_abort_bench OUT=ring.csv ./false_abort_sweep.sh 5
BENCH=./ssi_abort_bench OUT=rmw.csv  ./false_abort_rmw.sh  5
./false_abort_report.py ring.csv rmw.csv
```

The single most informative one-liner — an empty logical conflict graph aborting
11% of transactions purely because the keys share a leaf:

```sh
SSI_ENV=/tmp/e1 SSI_CSV=1 SSI_PAGESIZE=32768 SSI_VALSZ=64 \
    SSI_WORKLOAD=rmw SSI_WSTRIDE=8 ./ssi_abort_bench 20000 5 8   # aborts
SSI_ENV=/tmp/e2 SSI_CSV=1 SSI_PAGESIZE=32768 SSI_VALSZ=64 \
    SSI_WORKLOAD=rmw SSI_WSPLIT=1 ./ssi_abort_bench 20000 5 8    # zero
```
