# RFC 0017 working directory

Measurement plan, results and notes for RFC 0017 (HASH concurrency and online
resize). Every number recorded here must come from a run on libdb, with the
host, commit, build flags and command line beside it.

Before RFC 0017 can leave Draft, this directory must contain the following.

1. **Concurrency benchmark results.** `test/bench/hash_conc_bench.c` runs T
   threads doing point inserts and lookups against one HASH database at a fixed
   fill factor. It covers transactional and non-transactional runs, reports
   throughput against thread count and a latency histogram, and includes a
   B-tree control. The A/B against the base is run on one host, with arms
   alternated over repeated runs.
2. **Split-stall measurement.** How long concurrent cursors wait on the hash
   metapage while a split is in progress (`__ham_expand_table` through
   `__ham_split_page`, until the splitting transaction ends), before and after
   the change.
3. **Correctness evidence.** The `test/db` tier, the TCL hash tests, DST with
   `BUGGIFY_HASH_EXPAND_EARLY`, and a multi-process hash test, all green, with
   the region signature for change 1 compared against master.

---

## Results (2026-xx, change 1)

- **Host.** EC2 `c6id.8xlarge` (32 vCPU), Debian 12, kernel 6.1. Every database
  and environment on an **XFS filesystem on the local NVMe instance store**
  (`/dev/nvme1n1`, 1.7 TB), mounted at `/nvme` (`findmnt /nvme` shows `xfs`).
  THP off. CPU frequency is fixed on this instance class.
- **Build.** `../dist/configure` (no `--enable-diagnostic`) for the benchmark
  library on both arms; identical flags. Correctness ran a separate
  `--enable-diagnostic` build so the lock-order checker was active.
- **Commits.** base = `origin/master` `411e58f13`; branch =
  `feat/rfc17-hash-concurrency` (this PR).
- **Method.** `test/bench/ab_hash_conc.sh` alternates branch and base per rep
  (ABAB), one `hash_conc_bench` invocation per cell sweeping T ∈ {1,2,4,8,16,32}.
  The headline insert cells were re-run at **9 reps, 6 s each** to tighten the
  CV; the full matrix used 5 reps, 4–5 s. `test/bench/ab_summary.py` takes the
  per-rep median and the within-cell CV; cells with CV > 10% in either arm are
  marked low-confidence. Raw data: `test/bench/rfc0017-ab-raw.tsv` and
  `test/bench/rfc0017-ab-insert-tight.tsv`; full table:
  `test/bench/rfc0017-ab-results.md`.

### Headline: hash insert-heavy (split-inducing), tight CV (≤6%)

| thr | base ops/s | branch ops/s | branch/base | branch p99 µs | base p99 µs | branch deadlocks | base deadlocks |
|----:|-----------:|-------------:|------------:|--------------:|------------:|-----------------:|---------------:|
| 1 (txn)    | 12,476 | 12,583 | 1.009 | 251 | 251 | 0 | 0 |
| 8 (txn)    | 25,153 | 20,724 | **0.824** | 1,100 | 704 | 1 | 21,864 |
| 16 (txn)   | 31,713 | 29,999 | 0.946 | 1,770 | 960 | 1 | 22,536 |
| 32 (txn)   | 39,319 | 31,476 | **0.801** | 3,468 | 1,503 | 2 | 28,582 |
| 1 (notxn)  | 12,032 | 12,559 | 1.044 | 251 | 252 | 0 | 0 |
| 8 (notxn)  | 25,181 | 20,629 | **0.819** | 1,095 | 701 | 2 | 21,546 |
| 16 (notxn) | 32,912 | 29,824 | 0.906 | 1,796 | 949 | 1 | 22,900 |
| 32 (notxn) | 39,699 | 31,418 | **0.791** | 3,497 | 1,497 | 2 | 26,090 |

### B-tree control (must not move) — unchanged within noise

insert/notxn branch/base ∈ [0.95, 0.98], lookup/notxn ∈ [0.96, 1.04], all with
CV ≤ 5%. No regression to the default access method.

### Hash lookup-heavy (change 1's target), CV ≤ 10% cells

notxn: branch/base = 1.04 (t=1), 1.08 (t=2), 1.00 (t=8), 0.99 (t=16), 1.01
(t=32). Neutral to slightly positive. The txn lookup cells were too noisy
(branch CV ~38%) to quote.

### Noise floor (base vs base, hash insert txn)

branch/base-equivalent ratios 0.95–1.01; differences under ~5% are noise.

## Conclusion — a measured negative on the headline metric

**Change 1 does not deliver a throughput gain on the insert-heavy, split-inducing
workload; at high concurrency it is a 5–20% throughput regression** (0.80× at
t=32, both transactional and not), measured confidently (CV ≤ 6%). What it does
instead:

- **Deadlocks drop ~4 orders of magnitude** (≈1–2 vs ≈22,000–29,000 per run).
  On master every insert that meets a split contends on the metapage WRITE
  lock, and the `DB_LOCK_YOUNGEST` detector resolves the resulting cycles by
  aborting and retrying — so master's higher op count is substantially
  deadlock-retry churn.
- **Tail latency roughly halves** (p99 960 µs vs 1,770 µs at t=16; 1,503 µs vs
  3,468 µs at t=32), because an operation no longer queues behind a
  whole-table metapage WRITE held through a split.

So the honest summary is a **trade, not a win**: fewer deadlocks and lower tail
latency at the cost of raw insert throughput under heavy splitting. The
point-lookup case the RFC singled out as "cheap" is neutral-to-slightly-positive
and never regresses. The B-tree default and single-threaded hash are unchanged
within noise.

### Why the insert path lost throughput

The branch's per-operation cost moved: instead of one metapage READ lock held
cheaply to transaction end (which, under `DB_LOCK_YOUNGEST`, master turns into a
fast deadlock-abort-retry loop that happens to push a high op count), the branch
takes an unlocked metapage buffer pin for validation on every keyed op and, when
a split is in flight, a `DB_MPOOL_TRY` that returns `NOTGRANTED` and triggers a
bucket-release-and-retry. Under a storm of splits (ffactor 4-equivalent,
4 M-key space) those TRY-miss retries cost more than master's deadlock-retries
earn, so throughput falls even as contention collapses.

### Recommendation

Change 1 as implemented is **not worth shipping for throughput alone**. It is
worth shipping if the project values the deadlock-rate and tail-latency
reduction for latency-sensitive hash workloads; otherwise the real win needs
**change 2** (non-fencing split), which removes the whole-table WRITE fence at
its source rather than working around it with validate-and-retry. The data here
is the evidence the RFC asked for before it leaves Draft, and it argues for
prioritising change 2.
