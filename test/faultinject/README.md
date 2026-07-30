# Malloc-failure injection (SQLite-style) for libdb

SQLite's most powerful correctness technique is to **fail the Nth memory
allocation**, sweep N across a workload, and assert at *every* failure point
that the library returns a clean error, leaks nothing, holds no lock, and
leaves no corruption. libdb routes essentially all allocation through one
seam — `__os_malloc` / `__os_calloc` / `__os_realloc` / `__os_umalloc` /
`__os_urealloc` in `src/os/os_alloc.c` — so a single injection point covers
the library. This is the dynamic complement to the Coccinelle `malloc-leak` /
`mutex-unbalanced` static rules (Tier B1 of `.agents/test-suite-maturity-plan.md`).

## What's here

| File | Role |
|------|------|
| `fi_alloc.h` / `fi_alloc.c` | The injection hook + runtime knobs. Compiled into the library only under `--enable-faultinject`; absent from production. |
| `fi_sweep.c` | The driver: measure baseline allocation count M, then sweep K = 1..M failing the Kth allocation, classifying each run. |

The hook lives behind `HAVE_FAULT_INJECT`. When off (the default), the
`FI_FAIL()` calls in `os_alloc.c` expand to constant `0`, fold away at
compile time, and a stock build is **bit-for-bit unchanged and zero-overhead**.
Even in an `--enable-faultinject` build, injection is inactive until armed, so
the library behaves like stock until a sweep arms it.

## Build

From a fresh `build_unix` (autotools) in the nix dev shell:

```sh
cd build_unix
../dist/configure --enable-debug --enable-faultinject
make -j4          # builds libdb with the hook compiled in
make fi_tests     # builds ./fi_sweep
```

`--enable-faultinject` was added to `dist/aclocal/options.m4` +
`dist/configure.ac`; `dist/configure` is regenerated via `dist/s_config`.
It does **not** require `--enable-tcl` (unlike `--enable-test`).

## Run the sweep

```sh
./libtool --mode=execute ./fi_sweep            # full sweep, K = 1..M
./libtool --mode=execute ./fi_sweep 100        # cap at K = 1..100
FI_MAXK=100 ./libtool --mode=execute ./fi_sweep  # same, via env
```

The driver:

1. **Phase 1 (baseline):** run the workload with injection OFF to count the
   total allocations `M` (a transactional `DB_PRIVATE` env + a btree AND a
   hash DB, put/get, a cursor walk, a committed txn, an aborted txn, a
   checkpoint, plus a secondary index / `pget`, a join cursor, bulk
   put/get, an in-memory DB, a subdatabase, `DB->compact`, `DB->stat`, and a
   2PC prepare — **M ≈ 947**).
2. **Phase 2 (sweep):** for `K = 1..M`, run the SAME workload with "fail the
   Kth allocation." Each K runs in a **forked, watchdogged child** so a
   crash/hang/leak in one failure point can't wedge the sweep.

## Interpret the results

Each K's child is classified:

| Class | Meaning |
|-------|---------|
| **clean/tolerated (OK)** | Workload finished; the OOM was beyond this run's alloc count or was tolerated. |
| **clean error return** | The injected OOM surfaced as a non-zero error AND the env re-opened cleanly under `DB_RECOVER` afterwards. The good OOM case. |
| **CRASH** | The child died by signal (segfault/abort): a NULL-deref or use-after-free on the OOM return path. **A real bug.** |
| **HANG (lock leak?)** | The per-run watchdog fired: an operation blocked, almost always a lock held on the OOM return path (the #47 class). **A real bug.** |
| **DIRTY teardown/state** | The failure left the env un-reopenable even with recovery. **A real bug.** |

The process exits non-zero iff any run crashed, hung, or left dirty.

## Reproduce a specific failure point

```sh
# In-process single run for K, easy to attach a debugger / ASan to:
DB_FI_FAIL_AT=<K> DB_FI_VERBOSE=1 ./libtool --mode=execute ./fi_sweep --one

# Under gdb (point LD_LIBRARY_PATH at the built lib):
export LD_LIBRARY_PATH="$PWD/.libs:$LD_LIBRARY_PATH"
DB_FI_FAIL_AT=<K> gdb -ex 'run --one' -ex bt .libs/fi_sweep

# Under ASan (build a second tree with -fsanitize=address), catches
# heap-UAF / leaks a plain segfault check misses:
DB_FI_FAIL_AT=<K> ./.libs/fi_sweep --one
```

Runtime knobs (read lazily on the first allocation, so setting them before any
libdb call arms the sweep):

- `DB_FI_FAIL_AT=<K>` — fail the Kth allocation (K ≥ 1; 0/unset = inactive).
- `DB_FI_VERBOSE=1` — trace each fired failure to stderr.
- `FI_TIMEOUT_SEC=<n>` — per-run watchdog seconds (default 20).
- `FI_MAXK=<n>` — cap the sweep.

The in-process API (`fi_alloc.h`) — `__db_fi_arm(K)`, `__db_fi_disarm()`,
`__db_fi_reset()`, `__db_fi_count()`, `__db_fi_fired()` — is what the driver
uses to sweep without rebuilding.

## Measured results (agent/malloc-deepen, 2026-07-30)

The workload was broadened from the original basic flow (open env + btree +
hash, put/get/cursor/txn commit+abort, checkpoint; **M = 506**) to also drive
the warmer error paths functional tests miss:

- a **secondary index** (`associate` + get-by-secondary + `pget`),
- a **join cursor** (`DB->join`),
- **bulk put/get** (`DB_MULTIPLE_KEY` write buffer + `DB_MULTIPLE_KEY` cursor scan),
- an **in-memory DB** (NULL filename),
- a **subdatabase** open (named DB inside a container file),
- **compaction** (`DB->compact` with `DB_FREE_SPACE`),
- **`DB->stat`**,
- a **2PC prepare** (`txn->prepare` then resolve).

New workload baseline: **M = 947 allocations** (up from 506 — +441 sites,
~87% more failure points swept).

| Metric | Count |
|--------|------:|
| Failure points exercised (K = 1..947) | 947 |
| Clean error return (OOM → error, env re-openable) | 861 |
| Clean/tolerated | 81 |
| **CRASH** | **5** |
| HANG | 0 |
| DIRTY | 0 |

The 5 crashes are a **single new root cause** on the hash-abort *undo* path
(reached now that the wider workload leaves an aborted-hash txn to undo at
env teardown). The ASan one-shot pass additionally flagged **leaks** on the
`db_create` / `__db_join` OOM teardown paths (advisory). All are new,
pre-existing engine bugs surfaced by the wider sweep; per the harness policy
they are reported here, not fixed in this PR.

Note on `txn_recover`: it was intentionally left out of the workload. In a
single live process the prepared txn is still active, so a `txn_recover` +
resolve double-resolves it and panics the region (`transaction already
committed` → `DB_RUNRECOVERY`). `txn_recover` is a separate-process recovery
operation; `txn->prepare` alone reaches the new 2PC prepare/gid alloc sites.

### Reproduce the new findings

```sh
# plain-debug build (crash):
DB_FI_FAIL_AT=490 ./libtool --mode=execute ./fi_sweep --one   # SIGSEGV
# ASan build (leaks):
DB_FI_FAIL_AT=503 ./.libs/fi_sweep --one    # db_create bt_internal leak
DB_FI_FAIL_AT=641 ./.libs/fi_sweep --one    # __db_join cursor leak
```

## Historical results (agent/malloc-inject, 2026-07-27)

Original basic-workload baseline: **M = 506 allocations.** That sweep found
8 crashes across 4 root causes on OOM return paths (`db_env_create` teardown,
`__lock_getlocker_int`, `__db_pgin`, and a txn/lock double-free). Those four
were fixed separately (see PR #52); the current tree returns clean errors at
those K.

## Bugs found by the expanded sweep (report only — NOT fixed in this PR)

Per the harness policy, this PR broadens the workload and reports engine bugs;
fixes are separate focused PRs (like PR #52 for the first batch). The wider
workload surfaced **one new crash root cause** (5 K values) plus a **family of
OOM-teardown leaks** the ASan pass flags.

### New Bug A — NULL-deref in hash-abort undo cursor open (K=490, 491, 492, 493, 497)
`src/db/db_iface.c:370` (`__db_cursor`, the `MULTIVERSION(dbp)` check reads an
atomic off a partially-built handle), via
`src/hash/hash_rec.c:84` (`__ham_insdel_recover`) ←
`src/txn/txn.c:1942,2036` (`__txn_dispatch_undo`/`__txn_undo`) ←
`src/txn/txn.c:1242` (`__txn_abort`) ←
`src/txn/txn_region.c:252` (`__txn_env_refresh`) ← `__env_refresh` at env close.

The wider workload leaves an **aborted hash transaction** whose insdel log
records must be undone. When an allocation on the undo path fails (K in
490..497), `__ham_insdel_recover` still calls `__db_cursor` on a dbp whose
backing (mpf / cursor prerequisites) was never allocated, and
`__os_atomic_read(p=0xa4)` dereferences a NULL base + field offset.
Same stack for all five K.
Fix shape: propagate the undo-path alloc failure so `__ham_insdel_recover`
does not open a cursor on a half-built handle (or guard `__db_cursor` /
`REC_INTRO` when the handle's mpf is NULL).

Repro: `DB_FI_FAIL_AT=490 ./libtool --mode=execute ./fi_sweep --one`

### New Bug B — `db_create` OOM teardown leaks the access-method private struct (ASan; K=485,488,503,540,641,775,813,823,860,886,909,928,930,932,933,935,937,…)
`src/db/db_method.c:206` (`__db_create_internal` `err:` path), leaking the
struct allocated by `__db_init` → `__bam_db_create` (`src/btree/bt_method.c:47`,
152 bytes) or `__qam_db_create` (queue), and analogous `__env_alloc` /
`__lock_vec` region allocations.

`__db_create_internal`'s `err:` path frees only `dbp` and `dbp->mpf`:
```c
err:    if (dbp != NULL) {
                if (dbp->mpf != NULL)
                        (void)__memp_fclose(dbp->mpf, 0);
                __os_free(env, dbp);   /* does NOT free dbp->bt_internal */
        }
```
When an allocation *after* `__bam_db_create`/`__qam_db_create` fails (e.g.
`__memp_fcreate`), the access-method private struct hung off
`dbp->bt_internal` / `dbp->q_internal` leaks, and `*dbpp` is set to NULL so
the caller cannot free it either. A per-access-method OOM leak.
Fix shape: call the access-method's own destructor (or free
`dbp->bt_internal`/`dbp->q_internal`/`dbp->h_internal`) on the `err:` path.

Repro (ASan build): `DB_FI_FAIL_AT=503 ./.libs/fi_sweep --one`

### New Bug C — `__db_join` leaks the join cursor on OOM (ASan; K=641, and nearby)
`src/db/db_join.c:93` (`__db_join`).

`__db_join` allocates the join-cursor struct (256 bytes) and then does further
allocations while wiring up the constituent cursors. When one of those later
allocations fails, `__db_join` returns an error **without freeing the
already-allocated join cursor**, and since it never set `*dbcp` the caller
cannot free it. An OOM leak local to `__db_join`.
Fix shape: free the partially-built join cursor on `__db_join`'s error path.

Repro (ASan build): `DB_FI_FAIL_AT=641 ./.libs/fi_sweep --one`

### Note on classification

Bug A is caught by BOTH the plain-debug forking sweep (as a `CRASH`) and the
ASan one-shot pass (as `SEGV`). Bugs B and C are **leaks** that the plain
segfault check cannot see; only the ASan build flags them (`LeakSanitizer`),
so they are advisory. The four original bugs (K=2, 212…, 375…, txn/lock UAF)
are no longer reproducible on the current tree — they were fixed in PR #52.

## CI

`.github/workflows/faultinject.yml` runs the full plain-debug sweep
(K = 1..M, ~46s at M=947) as the primary signal plus a full ASan one-shot
pass (K = 1..M, ~2min), both `continue-on-error: true` (advisory) until the
bugs above are fixed. The full sweep is cheap enough to run unbounded in CI;
cap it with `FI_MAXK` locally if needed. See the workflow header for detail.

Updating the workflow requires a token with `workflow` scope, which agent
pushes do not carry, so the updated workflow is staged alongside this README
as `test/faultinject/faultinject.yml.workflow`. A maintainer syncs it into
`.github/workflows/faultinject.yml` (the only change vs. the installed file:
the ASan one-shot pass now sweeps the full K = 1..M range instead of
stopping at K = 260, so it reaches the new leaks/crashes past K = 480).
