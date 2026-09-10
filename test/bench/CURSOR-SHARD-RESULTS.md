# Cursor-queue sharding: measurement

`DB->get` allocates and frees a transient cursor per operation, and
`__db_cursor_int` / `__dbc_close` take `dbp->mutex` to move it between the
handle's free and active queues. When many threads share one `DB` handle that
mutex serializes every `get`. This directory records what sharding those queues
is actually worth, measured rather than asserted.

## Result

Idle EC2 `c7i.24xlarge` (96 vCPU, Xeon Platinum 8488C, 1 NUMA node, THP never,
numa_balancing off), 5 reps per point, `scale_bench` `rrand`, medians in ops/s.
Raw data: `results/cursor-shard-sweep-c7i.24xlarge.tsv`.

| threads | master | sharded, as first written | sharded + thread-id fix |
|--------:|-------:|--------------------------:|------------------------:|
|       1 | 926867 |                    924332 |                  916862 |
|       8 | 556739 |            396060 (−29 %) |         2456548 (4.4×)  |
|      16 | 292833 |            167177 (−43 %) |         3294383 (11.3×) |
|      24 | 174288 |            118063 (−32 %) |         3706043 (21.3×) |
|      48 |  79234 |                    106842 |         3590889 (45.3×) |
|      96 |  51872 |                    131336 |         2028715 (39.1×) |

Coefficient of variation across reps was 1.2–12.8 %, so the effect is far outside
run-to-run noise. Single-threaded throughput is unchanged, which is the expected
shape: with one thread there is no contention to remove, and the added hash plus
per-partition mutexes cost nothing measurable.

**As first written, the change was a 29–43 % regression.** The idea is sound; the
partition selection was not.

## Why the first version made things worse

Partition selection degenerated two independent ways. Both were confirmed in the
source and then instrumented in a real library driving 480 000 threaded
`DB->get` calls on a shared `DB_THREAD` handle across 24 threads.

1. **It hashed the process id.** `DB_CURSOR_PART_PICK` used `ip->dbth_pid`, and
   `__os_id` returns `env->pid_cache` / `getpid()` (`src/os/os_pid.c`);
   `src/env/env_failchk.c` sets `dbth_pid` from `id.pid` separately from
   `dbth_tid` from `id.tid`. Every thread in a process therefore hashes to the
   same partition. Instrumented: `part[5] = 480000`, every other partition 0.

2. **On a default environment the thread-info block does not exist at all.**
   `ENV_ENTER` sets `ip = NULL` when `env->thr_hashtab == NULL`
   (`src/dbinc/db_int.in`), and `thr_hashtab` is NULL whenever `thr_max == 0`
   (`src/env/env_failchk.c`) — that is, unless the application called
   `DB_ENV->set_thread_count()`. The macro's `(ip) == NULL ? 0` arm then pins
   everything to partition 0 and the pid is never even hashed. Instrumented:
   `part[0] = 480000`, with `ip == NULL` on 480000 of 480000 calls.

So the sharded build paid for a hash and eight mutexes per handle while still
funnelling every allocation through one partition — strictly worse than the
single mutex it replaced.

**A trap for anyone benchmarking this area:** none of `scale_bench.c`,
`scale_iso.c` or `lock_bench.c` calls `set_thread_count`, so a benchmark of the
original change exercises path 2. The result is pure overhead with zero sharding,
which is indistinguishable from "sharding does not help" unless you check the
partition distribution.

## The fix

`__db_cursor_part()` asks `dbenv->thread_id()` for the real thread id and hashes
it with `DB_CURSOR_PART_HASH`, taking the **high** bits after Knuth
multiplicative mixing. The high bits matter: `db_threadid_t` is `pthread_t`,
typically a stack address, so bits below the per-thread stack stride are constant
and the original `(v * 2654435761U) >> 16 & 7` collides even after substituting
the correct field. Swapping `dbth_pid` for `dbth_tid` alone is not sufficient.
Thread types that are not simple integers are folded through `__ham_func5` first.

## Why a 45× gain is credible

A number that large invites suspicion, so it was checked against a workload that
should *not* benefit.

On `rhot` — hot-key reads, where every thread contends for the same few pages —
the fix gains only **1.18×** (188 272 → 222 216 at 24 threads), and the lock
partition becomes the bottleneck instead: `lockpart_pct` rises from about 9 % to
74 %. A change that removes the cursor-allocation mutex should help uniform
random reads enormously and hot-key reads barely, and that is the observed
asymmetry.

It also agrees with an independent earlier experiment: giving each thread its own
`DB` handle, which sidesteps `dbp->mutex` entirely, ran +49 % faster at 24
threads. Sharding recovers far more than that because it removes the serialization
without duplicating per-handle state, and the bottleneck moves to
`__memp_fget` / `__memp_fput` — exactly where that study predicted it would land.

## Correctness

On the rebased branch: 9/9 `test/db` regression runners; `test/isolation`,
`test/lockmatrix` and `test/soak` (5 workloads, 0 unexpected); TCL `lock001`,
`txn001`, `ssi001`, `ssi002`, and — the cursor-lifetime cases that sharding could
plausibly break — `test001`, `test003`, `test011` (duplicates and off-page
duplicates), `test026` (cursor delete) across btree and hash, plus `jointest`
(join cursors). All pass.

An AddressSanitizer build is clean with **zero** sanitizer findings. Three
apparent ASan failures were harness artifacts, not defects: the `test/db` runners
compile their driver without `-fsanitize=address`, so the link fails on
`__asan_report_load4`, and `run_qam_readpath_bound` reports `rc=124` purely from
ASan's 5–7× slowdown against its timing threshold (it passes with
`QAM_DOS_LIMIT_SECS=800`).

## What is not measured here

Write workloads (`wrand` is fsync-bound and not cursor-limited), multi-socket
NUMA effects (the instance has a single node), and cross-engine comparison. The
sweep used a standalone driver rather than `run_bench.sh`, so there is no
`bench_cmp.py` verdict against `baseline-c7i.24xlarge.tsv`; the committed baseline
does not include a shared-handle threaded-`get` case, which is the workload this
change targets.
