# Tier B2 — resource-accounting soak

Runs thousands of **sequential** transactions in **one long-lived**
environment and asserts that region resources return to a steady state instead
of growing with the transaction count.

## Why this tier exists

A slot/mutex/locker leak produces no crash, no corrupt page and no sanitizer
report. It only manifests after thousands of sequential transactions, when some
later API call returns `ENOMEM` because the region filled up. Neither the
crash/durability tier (`test/sim`) nor the memory-safety tier (`test/fuzz`) can
see that shape. Issues #137 and #138 are both of it.

This is a general, reusable soak harness parameterised by workload — not a
targeted regression test for one bug. Adding a workload is one function plus one
table row.

## Method

One environment, opened once with **default region sizes** (the point is that a
correct engine does not need a bigger region to run 2000 sequential
transactions, and a leak surfaces as `ENOMEM` precisely because the region is
finite). N transactions of the workload run sequentially, sampling the **public**
stat APIs at intervals:

| Counter | Source |
|---|---|
| `mutex_inuse` | `DB_ENV->mutex_stat` → `st_mutex_inuse` |
| `lock_lockers` | `DB_ENV->lock_stat` → `st_nlockers` |
| `lock_locks` | `DB_ENV->lock_stat` → `st_nlocks` |
| `lock_objects` | `DB_ENV->lock_stat` → `st_nobjects` |
| `txn_active` | `DB_ENV->txn_stat` → `st_nactive` |
| `txn_snapshot` | `DB_ENV->txn_stat` → `st_nsnapshot` |
| `mpool_dirty` | `DB_ENV->memp_stat` → `st_page_dirty` |

The verdict is the **least-squares slope** over the samples taken after warmup,
in counter units per 1000 transactions, against a per-counter tolerance
(documented in the `counters[]` table). Least squares rather than
last-minus-first so a single noisy endpoint cannot decide the verdict.

The first quarter of samples is warmup: lazy region allocation and cache fill
legitimately grow counters there, and the tier should not fight normal
behaviour. Only the steady state is asserted.

Tolerances are 20 units per 1000 transactions for the region counters — a
genuine per-transaction leak grows at ~1000 units per 1000 transactions, three
orders of magnitude above the tolerance, so the check is not marginal.

An `ENOMEM` / `DB_RUNRECOVERY` from any API call is recorded (with the call name
and transaction number) and fails the workload, but does **not** abort the run:
"ENOMEM at transaction 1187" is a much better report than a stack trace.

The **full growth curve is always printed**, so a CI log alone is enough to
diagnose a regression without re-running locally.

## Workloads

| Workload | Shape | Expectation on master |
|---|---|---|
| `ro_snapshot` | read-only `DB_TXN_SNAPSHOT` txns, no write | **XFAIL — reproduces #137** |
| `mvcc_retained` | snapshot txns that read *and* write, so their details are MVCC-retained then reaped | **XFAIL — reproduces #138** (see caveat) |
| `rw_plain` | ordinary read-write txns, no snapshot | PASS (flat) |
| `aborted` | snapshot txns that all abort | PASS (flat) |
| `cursor_churn` | plain txns that open, walk and close a cursor | PASS (flat) |

Observed on master (c4811dc87), 2000 transactions: `ro_snapshot` leaks
**+1000.00 mutex slots and +1000.00 lockers per 1000 transactions** — exactly
one of each per transaction, never returned. The three controls are flat
(`±0.00` to `-1.23`), which is what makes the leak signal credible rather than a
measurement artefact.

`cursor_churn` is deliberately a **plain** transaction, not a snapshot one: as a
snapshot reader it tripped the #137 locker leak and was simply a second copy of
`ro_snapshot`, telling us nothing about cursors. With a plain txn, growth there
is genuinely a cursor/lock-list accounting problem.

### Caveat on `mvcc_retained` / #138

#138 is a leak in `__txn_reap_si_details` (`src/txn/txn_region.c`), which frees
a parked transaction detail without freeing its `mvcc_mtx`. Reaching it needs
the detail to be parked on the `mvcc_txn` list *and* its SIREAD markers
garbage-collected *and* its MVCC pages evicted. The workload reads several keys
(leaving markers) and writes over a 512-key space (creating MVCC versions and
driving cache turnover) to get there. Whether a given N reaches the reap path is
timing- and cache-dependent; if this workload reports UNEXPECTED PASS, that may
mean the reap path was not reached rather than that #138 is fixed. Check the
`mutex_inuse` column of the printed curve before concluding anything, and see
the targeted test from the #137/#138 fix for a deterministic reproducer.

## Running it

```sh
# Build libdb first (once):
cd build_unix && ../dist/configure --enable-debug && make -j"$(nproc)"

cd test/soak
./run.sh                      # every workload, 2000 txns each
SOAK_N=10000 ./run.sh         # longer soak
./run.sh ro_snapshot          # one workload
./run.sh --list               # workload names, with expect-leak marked
./run.sh build                # build only
```

Environment: `CC`, `LIBDB_BUILD` (default `../../build_unix`), `SOAK_N` (default
2000), `SOAK_TIMEOUT` (default 900s), `SOAK_SAN=1` to add ASan.

## Exit status

- `0` — every workload matched its recorded expectation.
- `1` — a workload did not. Either a control leaked, **or** an expect-leak
  workload stayed flat, meaning the referenced issue got fixed and
  `expect_leak` should be cleared in the table in `test_soak_resources.c`. The
  message says which.
- `2` — harness error.

When #137/#138 land, clear `expect_leak` on the corresponding workloads; the
tier then gates the fixes against regression.

## Relation to the targeted leak tests

The #137/#138 fix adds its own targeted regression tests. This tier is the
general soak: it is parameterised by workload and lives under `test/soak/` (a
distinct directory, no file-name collisions with `test/c/`). The targeted tests
prove a specific code path frees a specific resource; this tier proves the
*aggregate* accounting is stable over a long run, which is the property that
would have caught both issues before release.
