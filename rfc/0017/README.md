# RFC 0017 working directory

Plan, design and results for RFC 0017 (HASH concurrency and online resize).

- [`DESIGN-CHANGE1.md`](DESIGN-CHANGE1.md): how change 1 works and why it is
  correct, plus the failed first implementation.
- [`../../test/bench/rfc0017-ab-results.md`](../../test/bench/rfc0017-ab-results.md):
  the A/B numbers, method and raw data.

## Change 1 status (v2, 2026-10-24)

Keyed hash gets and puts no longer take the metapage lock. Each cursor keeps
a hint of the mapping and locks the bucket that hint names. Then it maps the
key again from the live metapage, read under a buffer latch only, and retries
if the two disagree. A split now also write-locks the new bucket page.

Measured on a c6id.8xlarge with Debian and XFS on local NVMe, A/B against
`origin/master`. Runs alternated arms, 5 reps (9 for transactional lookups),
counting committed ops only.

| workload | 1 thread | 4 threads | 8 threads | 32 threads |
|---|---:|---:|---:|---:|
| hash insert, txn (split-heavy) | 1.00× | 1.14× | 1.24× | **1.46×** |
| hash lookup, non-txn | 1.12× | 1.68× | 3.70× | **11.6×** |
| hash lookup, txn | 1.10× | 1.19× | 0.93× (noise) | 1.10× |
| B-tree insert / lookup (control) | 1.00× / 1.01× | 1.00× / 1.03× | 1.00× / 1.01× | 1.02× / 1.02× |

- **Master vs master noise floor** on this host: up to 8% at 8–32 threads.
- **Transactional lookups at 8+ threads** are bound by the transaction region
  mutex in `__txn_begin`/`__txn_end` in both arms (about 60% of the profile),
  not by hash.
- **Hash insert p99 rises at 16–32 threads** (1.4 ms against 0.8 ms at 16
  threads) while goodput rises 32–46%. Master's lower p99 there comes with
  1000–2450 deadlock aborts per second; v2 has 30–100.

Correctness:
- The `test/db` tier passes 43/43 on an `--enable-diagnostic` build.
- TCL `test001`–`test060` under `-hash` pass 60/60 on an `--enable-diagnostic`
  build, one tclsh per test.
- The sabotage arm loses 13697 of 13715 committed keys.
- The region signature is unchanged.

## Not done

- **macOS:** the multi-process hash test exposed a pre-existing macOS bug, a
  pshared-condvar EINVAL PANIC that also happens on master. It is fixed
  separately in PR #226. This PR's macOS CI jobs hang until #226 is in their
  base. With #226 merged in (probe run 40055366615), the full CI workflow
  passes 50/50, including all four macOS default jobs.
- **aarch64, FreeBSD:** not measured.
- **Change 2:** design only (see the design note).
