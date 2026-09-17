# B-tree lock scope under bulk insert: the convoy is `PGNO_BASE_MD`, not the leaf (2026-09)

**STATUS: IN PROGRESS.** Numbers below are real and from the box; reps and the
final verdict are still landing. Every table says how many reps it rests on.
Nothing here is projected or estimated unless the row says so.

Assignment: characterise the write-path lock-scope convoy that
`WRITE-TAIL-2026-09.md` left open — "a writer holds page locks (leaf, plus
parent/ancestors during a split) across a ~3.7 ms durable commit, serializing
every other writer needing those pages" — attributing the wait three ways
between (a) the leaf lock, (b) split-time parent/ancestor locks, and (c)
`PGNO_BASE_MD` metadata contention on page allocation. Then fix it only if the
data justifies a safe change.

## Headline: the assignment's named target (a) and (b) have ZERO waiters

The convoy is real, reproduces exactly, and is a **single-object queue on page
0, the metadata page**. Neither the leaf lock nor any split-time ancestor lock
has a single waiter on it in this workload.

Unwarmed (growing-tree) t=96, one 15 s run, five `db_stat -Co` samples 1 s
apart, 400 WAIT records across the five samples:

| lock object | WAIT records | HELD records |
|---|---:|---:|
| page 0 (`PGNO_BASE_MD`) | **400** | 5 (1 per sample) |
| every other page (leaves + internal) | **0** | 95 |

That is the whole census: 100 % of waiters are on page 0, and the 95 leaf/
internal write locks that *are* held across commits have no queue behind them.

## Environment

`c7i.24xlarge` (96 vCPU Xeon 8488C, 1 NUMA node), Debian 13, `gp3` 10k IOPS,
THP off, ASLR off, governor `performance`. Base `0578113b9`. `DB_TXN_SYNC`,
`KEYRANGE=1000000`, 15 s windows. Unwarmed arm = `PREPOP=0`, the insert ramp,
which is the bulk-insert model. Warmed arm = prepopulated steady state, carried
as the control that proves the problem was not measured around.

**Device fsync floor** — measured separately so device time is never confused
with software time. `fsync_probe`, n=300, *taken while the matrix was running,
so it is contended and inflated*: `fsync` p50 3312 µs, `fdatasync` p50 3286 µs.
An idle re-measure is pending; `WRITE-TAIL-2026-09.md` reports p50 2.77 ms on
this box class and that is the number to compare software against until the
idle probe lands.

One env directory reused by every arm at a fixed path length: the only
unconfounded form on this box (`run_bench.sh`'s DB_PRIVATE layout warning).

## Deliverable 1: characterisation

### The matrix, rep 1 of 5 (more reps landing)

`ops/s`, `put p99`, and the waiter census, per thread count and arm:

| t | arm | ops/s | put p50 | put p99 | commit p50 | commit p99 | wait total | wait pg0 | wait other | held pages | splits/1k puts |
|--:|---|------:|--------:|--------:|-----------:|-----------:|-----------:|---------:|-----------:|-----------:|---------------:|
| 1 | unwarmed | 886 | 3 µs | 17 µs | 884 µs | 2893 µs | 0 | 0 | 0 | 9 | 54.4 |
| 1 | warmed | 1064 | 4 µs | 5 µs | 881 µs | 2802 µs | 0 | 0 | 0 | 5 | 0 |
| 8 | unwarmed | 2285 | 8 µs | 3752 µs | 3660 µs | 5688 µs | 0 | 0 | 0 | 49 | 54.5 |
| 8 | warmed | 3642 | 6 µs | 8 µs | 1774 µs | 3781 µs | 0 | 0 | 0 | 38 | 0 |
| 32 | unwarmed | 4030 | 7 µs | **66120 µs** | 3780 µs | 5861 µs | 63 | **62** | 1 | 110 | 54.8 |
| 32 | warmed | 9133 | 6 µs | 10 µs | 3660 µs | 4022 µs | 0 | 0 | 0 | 155 | 0 |
| 96 | unwarmed | 3906 | 28 µs | **281634 µs** | 3722 µs | 5792 µs | 400 | **400** | 0 | 95 | 61.4 |
| 96 | warmed | 16359 | 12 µs | 303 µs | 5878 µs | 6722 µs | 1 | 0 | 1 | 443 | 0 |

`wait *` columns are summed over the five 1 s samples of that run. `splits/1k
puts` is `__bam_split` log records per 1000 `__txn_regop` records in that run's
log; the warmed arm's splits all happen during `PREPOP`, before the measured
window, hence 0 in-window.

Reading it:

- **The convoy needs concurrency.** t=1 and t=8 unwarmed have zero waiters even
  though they split at the same rate (54/1k). The queue appears at t=32 and
  saturates at t=96.
- **The convoy is entirely page 0.** 62 of 63 at t=32, 400 of 400 at t=96. The
  one non-meta waiter at t=32 was a single record on page 712, and one more at
  t=96 warmed on page 225 — noise level, not a queue.
- **`put` carries it, `commit` does not.** commit p99 is 5.7–5.9 ms against a
  3.7 ms p50 unwarmed — a 1.6x spread that barely moves from t=8 to t=96 —
  while put p99 goes 3.8 ms → 66 ms → 282 ms. This reproduces
  `WRITE-TAIL-2026-09.md`'s Finding 3 exactly.

### Attribution (a) / (b) / (c)

**(c) `PGNO_BASE_MD` metadata contention on page allocation is the entire
effect. (a) leaf and (b) split ancestors are zero.**

Mechanism, from the code: `__db_new` (`src/db/db_meta.c:134`) takes
`PGNO_BASE_MD` `DB_LOCK_WRITE` with `LCK_ALWAYS`, and releases it at
`db_meta.c:263` with `__TLPUT`. Inside a transaction `__TLPUT` is a **no-op**
(`src/dbinc/db_am.h:234` → `__db_lput`, which for a write lock in a txn keeps
it) — so the meta write lock acquired to allocate one page is held **to
commit**, across the allocating transaction's own ~3.7 ms `fsync`. Every other
writer that needs to allocate blocks on that one lock object.

`__bam_split` (`src/btree/bt_split.c:83-86`) *also* takes `PGNO_BASE_MD` write
before descending, but releases it with `__LPUT` at `bt_split.c:190` — an
unconditional put — so the split's own meta hold is short. The long hold is the
one `__db_new` leaves behind, and `__bam_split` calls `__db_new` twice
(`bt_split.c:238-239` for a root split, once at `:424` for a page split).

### Arithmetic: is one meta hold per allocation enough to explain 282 ms?

t=96 unwarmed, rep 1: 3657 `__db_pg_alloc` records against 59,482 `__txn_regop`
= **6.1 % of puts allocate a page**. At 3906 ops/s that is **240
allocations/s**, each holding page 0's write lock across a 3.72 ms commit p50 →
offered utilisation **0.89**. Little's law on the census (78 mean waiters
observed by the prior agent's samples, 400/5 = 80 in this run) gives
80 / 240 = **333 ms** expected wait, against a measured put p99 of **282 ms**
and p99.9 of **310 ms**. The queue closes.

## Deliverable 2: pending

Candidate ranking follows the attribution: (a) and (b) are refuted by
measurement — there is nothing to shorten. Only (c) is live. Work in progress:

1. Ceiling probe (deliberately unsafe, never shipped) to size the prize before
   designing anything.
2. `DB_TXN_BULK` already exists (`src/txn/txn.c:374`, `TXN_BULK` in
   `src/dbinc/db.in:967`) and does file-extension watermarking — checking
   whether it is a bulk path that simply is not being used.
3. Any change must not release a write lock before commit (2PL). The safety
   argument for whatever lands goes here.

## Durability, isolation, ABI: pending

Nothing in the library is changed as of this revision, so nothing can have
regressed — but that is asserted, not proved, and the proof lands with any
change.

## Verdict: pending

## Reproducing

```sh
# on the box, from the branch worktree
cd test/bench && sh lsc_setup.sh
SECS=15 REPS=1 OUT=/tmp/lsc-r1 sh lsc_matrix.sh r1 1 8 32 96
```

Raw data: `results/btree-lock-scope-2026-09/`.
