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
   checkpoint).
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

## Measured results (this branch, 2026-07-27)

Workload baseline: **M = 506 allocations.**

| Metric | Count |
|--------|------:|
| Failure points exercised (K = 1..506) | 506 |
| Clean error return (OOM → error, env re-openable) | 467 |
| Clean/tolerated | 31 |
| **CRASH** | **8** |
| HANG | 0 |
| DIRTY | 0 |

The 467 clean-error paths are the good news: the vast majority of OOM sites
return a clean error and leave a recoverable environment (many print a `PANIC`
to stderr and return an error — that is BDB's *defined* in-region-OOM
behavior, not a crash). The 8 crashes are **real pre-existing bugs** on OOM
return paths, listed below.

## Bugs found (report only — NOT fixed in this PR)

Per the task, this PR is the harness; engine bugs are for the maintainer to
fix separately. Four distinct root causes, all NULL-deref / use-after-free on
allocation-failure paths — exactly the #47 class the Coccinelle static rules
target statically and this sweep confirms dynamically.

### Bug 1 — NULL-deref in `db_env_create` OOM teardown (K=2)
`src/rep/rep_method.c:95` (`__rep_env_destroy`), via
`src/env/env_method.c:105,123`.

In `db_env_create`, alloc #1 is the `DB_ENV`, alloc #2 is the `ENV`. When the
`ENV` alloc fails (`env_method.c:87`), `dbenv->env` is still NULL and the
`err:` path calls `__db_env_destroy(dbenv)` → `__rep_env_destroy(dbenv)`, which
does `env = dbenv->env; if (env->rep_handle != NULL)` → dereferences NULL.
Fix shape: guard `__rep_env_destroy` (and siblings) on `dbenv->env == NULL`.

Repro: `DB_FI_FAIL_AT=2 ./fi_sweep --one`

### Bug 2 — NULL-deref in `__lock_getlocker_int` shared-region OOM (K=212, 214, 220, 353, 359)
`src/lock/lock_id.c:349`.

When the free-locker list is empty and `__env_alloc` (shared-region alloc) can
never satisfy the request, the `while (__env_alloc(...) != 0) if ((nlockers >> 1) == 0) break;`
loop breaks with `nlockers` still ≥ 1 and `sh_locker` still NULL, then the
`for (i = 0; i < nlockers; i++) SH_TAILQ_INSERT_HEAD(..., sh_locker, ...)`
loop dereferences the NULL `sh_locker`. The `if (nlockers == 0)` nomem check
comes *after* the deref and never fires. Reached via `__txn_begin_int` and
`__db_open`/`__fop_file_setup`'s `__lock_id`.
Fix shape: on the `break`, set `nlockers = 0` (or bail to `__lock_nomem`)
*before* the insert loop.

Repro: `DB_FI_FAIL_AT=212 ./fi_sweep --one`

### Bug 3 — NULL-deref in `__db_pgin` with a NULL pgin cookie (K=375, 376)
`src/db/db_conv.c:76`, via `__memp_pg`/`__memp_pgread`/`__memp_fget` ←
`__ham_get_meta` ← `__ham_open`.

`__db_pgin` does `pginfo = (DB_PGINFO *)cookie->data;` with `cookie == NULL` —
the pgin cookie / DB_PGINFO allocation failed upstream (in the hash-open page
read setup) but the page-in proceeds and dereferences the NULL cookie.
Fix shape: propagate the upstream alloc failure so `__db_pgin` is never called
with a NULL cookie (or guard it).

Repro: `DB_FI_FAIL_AT=375 ./fi_sweep --one`

### Bug 4 — heap-use-after-free in txn/lock OOM cleanup (K≈213, 215, 216; ASan-only)
`src/lock/lock_id.c:507` (`__lock_freelocker_int` → `__os_atomic_read`), via
`src/txn/txn.c:1854` (`__txn_end`) ← `__txn_abort`.

When `__txn_begin_int` partially fails under OOM (locker allocated at
`txn.c:423`), the abort path's `__txn_end` frees the locker once (`txn.c:1816`)
and then calls `__lock_freelocker` again at `txn.c:1854` on the already-freed
locker, whose `__os_atomic_read` reads freed memory. A double-cleanup on the
transaction OOM path. Caught by the ASan build; the plain-debug build
tolerates the adjacent reads and instead segfaults at the nearby K in Bug 2.
Fix shape: null out / de-link the locker after the first free so `__txn_end`
does not free it twice.

Repro (ASan build): `DB_FI_FAIL_AT=213 ./.libs/fi_sweep --one`

## CI

`.github/workflows/faultinject.yml` runs the plain-debug sweep as the primary
signal plus a bounded ASan one-shot pass, both `continue-on-error: true`
(advisory) until the bugs above are fixed. See the workflow header for detail.
