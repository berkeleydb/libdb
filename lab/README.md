# `lab/` — design prototypes

Self-contained prototypes that validate control logic before it is wired into
the engine. Not built by the library; each has its own Makefile.

## `lab/bench` — scaling probe and TPROC-* workload suite

Benchmark drivers built against an Autoconf tree:

```sh
cd build_unix && ../dist/configure && make        # build libdb first
cd ../lab/bench && make BDB=../../build_unix       # build the drivers
```

- **`scale_bench`** — micro multi-core scaling probe (`rrand`/`rhot`/`wrand`),
  drives a shared environment from N threads and reports ops/sec plus
  region-contention signals.

- **`lock_bench`** — direct lock-manager probe. Each thread allocates its own
  locker and calls `lock_get`/`lock_put` in a tight loop on `distinct`
  (per-thread, no-conflict) or `shared` (read-lock the same objects) keys,
  bypassing the access methods and buffer pool so the lock subsystem's own
  scaling is measured in isolation.

- **`tproc_c` / `tproc_b` / `tproc_h`** — HammerDB-style workloads
  (independently implemented; **not** the TPC benchmarks and not comparable to
  TPC results):
  - `tproc_c` — OLTP, the five weighted order-entry transactions.
  - `tproc_b` — debit/credit, one short write transaction (the parameterized
    successor to `examples/c/ex_tpcb.c`); isolates pure commit/log overhead.
  - `tproc_h` — analytic: long read-only scans run concurrently with point
    writers; with `-m` the scans use snapshot isolation and run unobstructed,
    without it they contend with the writers.

All three share `bdb_bench.h`, which exposes a **safety-feature toggle
framework** so the same workload can be run with full ACID guarantees or with
individual protections removed, to measure their cost:

| flag | effect |
|------|--------|
| `-X txn`  | no transactions (no `DB_INIT_TXN`) |
| `-X lock` | no lock manager (no `DB_INIT_LOCK`) |
| `-X log`  | no logging/recovery (no `DB_INIT_LOG`) |
| `-d sync\|wnosync\|nosync` | commit durability (default `nosync`) |
| `-m` | MVCC / snapshot isolation (`DB_MULTIVERSION`) |
| `-C` | Concurrent Data Store (`DB_INIT_CDB`) instead of full txns |
| `-D N` | deadlock detection: `0` (default) detects on every conflict; `N>0` runs a background detector every `N` ms and leaves the hot path free of detection |
| `-c` `-t` `-S` `-s` `-i` | cache bytes, threads, scale, seconds, init |

Example:

```sh
./tproc_c -i -S 4 -c 268435456                 # populate 4 warehouses, 256MB cache
./tproc_c -t 16 -s 30 -c 268435456             # full-ACID run, 16 threads, 30s
./tproc_c -t 16 -s 30 -X txn -X lock -X log    # same workload, no safety features
./tproc_h -i -S 1 -m && ./tproc_h -t 8 -w 4 -m -s 30   # MVCC analytic vs writers
```

## `lab/lsm` — unified adaptive LSM controller

Prototype of the two-axis adaptive controller from
[`docs/design/lsm.md`](../../docs/design/lsm.md): one rolling-window + cooldown
core driving both the **structure axis** (SINGLE ⇄ HYBRID ⇄ MULTILEVEL, aether-
style) and the **per-segment policy axis** (LEVELED ⇄ TIERED, Amethyst-style).

Time is a caller-supplied "tick" so the logic is deterministic and testable.

```sh
cd lab/lsm && make check
```

`test_adaptive.c` drives an Amethyst-style phase-shifting workload and asserts:
- a write-heavy phase spawns the structure to `MULTILEVEL`; an idle phase
  collapses it back to `SINGLE`, with no oscillation;
- a sustained load reaches `MULTILEVEL` and stays (cooldown anti-flap);
- write-hot segments converge to `TIERED`, read-hot to `LEVELED`, and balanced
  segments don't churn.

Next: replace the tick with a millisecond clock, feed real op counters from the
access-method layer, and have the policy axis emit segment-rewrite decisions to
the compactor / log cleaner (ROADMAP #9, #14).
