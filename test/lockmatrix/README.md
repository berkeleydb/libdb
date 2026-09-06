# Tier B3 — lock-mode matrix

Exercises **every** `DB_LOCK_*` mode through the lock-list paths, under ASan, so
that adding a new lock mode cannot silently break list sizing.

## Why this tier exists

Issue #140 is a heap out-of-bounds **write** in `__lock_vec`. The root-cause
class is what matters: the SSI work added a new lock mode (`DB_LOCK_SIREAD` = 9)
without auditing pre-existing loops that enumerate lock modes exhaustively.
`__lock_vec`'s `DB_LOCK_PUT_READ` / `DB_LOCK_UPGRADE_WRITE` path sizes an
objlist from `sh_locker->nwrites`, but its release-loop skip condition tests
only `DB_LOCK_READ` and `DB_LOCK_READ_UNCOMMITTED`. `DB_LOCK_SIREAD` matches
neither those read tests nor `IS_WRITELOCK`, so an SIREAD lock is never counted
in `nwrites` yet still consumes an objlist slot — a write past the end of the
allocation. The `DB_ASSERT` that would catch it is diagnostic-only and compiled
out of release builds.

This tier is the **general matrix**, not a targeted regression test: it asserts
the invariant, so the *next* mode added is covered too.

## The invariant

> The set of locks the release loop **skips** must equal the set of locks
> counted in `nwrites`, because `nwrites` sizes the objlist allocation.

A mode that is neither recognised as a read (and released) nor counted as a
write (and allocated for) breaks it.

## What it checks

Entirely through the public `DB_ENV` lock API — `lock_id`, `lock_get`,
`lock_put`, `lock_vec`, `lock_stat`. No internal headers.

1. **`modes`** — every value in `db_lockmode_t` is acquired and released via
   both `lock_get`/`lock_put` and `lock_vec` `DB_LOCK_GET`/`DB_LOCK_PUT_ALL`. A
   mode missing from a conflict table or a mode-name switch shows up here.
   `DB_LOCK_NG` and `DB_LOCK_WAIT` are listed but not requestable; listing them
   keeps the table exhaustive, which is the point.
2. **`conflicts`** — every (held, wanted) cell. The expected verdict is *not*
   hard-coded: the engine's conflict table is the specification. What is
   asserted are the two properties a table must have regardless of policy:
   *totality* (every requestable mode gets a definite granted/`NOTGRANTED`
   answer, never an internal error and never a hang) and *symmetry of conflict*
   (if held H blocks wanted W then held W blocks wanted H — an asymmetric cell
   is how a hand-edited table acquires a hole when a mode is appended). It also
   checks `lock_stat`→`st_nmodes` against the number of `db_lockmode_t` values,
   which catches "a mode was added without widening the table" directly. The
   matrix is printed.
3. **`list`** — the lock-LIST operations that size an objlist from `nwrites`:
   `DB_LOCK_PUT_READ` and `DB_LOCK_UPGRADE_WRITE`, driven by a locker holding a
   **mix** of write locks and SIREAD locks across a sweep of `(nwrite, nsiread)`
   shapes, plus pure-write and write+read shapes as controls. The overflow is
   `(nsiread - nwrite)` DBTs, so the sweep walks `nsiread` well past `nwrite`.

Lock objects are genuine `DB_LOCK_ILOCK`s rather than opaque blobs, because
`__lock_fix_list` treats an object of exactly `sizeof(DB_LOCK_ILOCK)` as a page
lock to be coalesced by fileid — using real ILOCKs is what drives that code.

## ASan is required

The interesting failure is an out-of-bounds write inside **libdb's own**
allocation. A harness-only ASan build cannot see it; libdb itself must be
instrumented. So `run.sh` builds (once) an ASan libdb under `build_asan_gate/`,
reusing the same mechanism and directory as `test/fuzz/check-crashes.sh`, and
links against it.

## Result on current master

**Reproduces #140.** The very first mixed shape faults:

```
    PUT_READ       nwrite= 0 nsiread= 1 nread= 0 ...
==...==ERROR: AddressSanitizer: heap-buffer-overflow on address ... 
WRITE of size 8 at ... thread T0
    #0 ... in __lock_vec .../src/lock/lock.c:457:15
    #1 ... in __lock_vec_api .../src/lock/lock.c:94:9
    #2 ... in __lock_vec_pp .../src/lock/lock.c:76:2
SUMMARY: AddressSanitizer: heap-buffer-overflow .../src/lock/lock.c:457:15 in __lock_vec
```

`nwrite=0, nsiread=1` means the allocation was zero-sized (`nwrites == 0`) and
the loop wrote one DBT into it — the minimal form of the bug. All the
pure-write and write+read control shapes pass first, which is what confirms the
SIREAD mode specifically is the trigger.

Because an ASan abort kills the process, the harness prints each shape
**before** attempting it, so the last line of output always names the shape that
faulted.

**Expectations are written for the FIXED engine**: when #140 lands, the whole
matrix must run to completion and exit 0. No `expect_fail` flag is used here —
the tier asserts correct behaviour and currently the engine aborts, which is the
honest signal.

### It is detectable without ASan too

The corruption is severe enough that even an **uninstrumented** build trips
glibc's own heap consistency checks. Running the meson-built driver against a
plain `libdb.so` aborts with `double free or corruption (out)` at
`nwrite=1, nsiread=2`. That is a useful second signal — it means the bug is
reachable in a stock build, not only under a sanitizer — but ASan is still what
gives the faulting line (`src/lock/lock.c:457`), so `run.sh` defaults to it.

## Running it

```sh
cd test/lockmatrix
./run.sh                      # build ASan libdb (once) + run every section
./run.sh list                 # just the #140 sweep
./run.sh modes conflicts      # the mode and conflict-matrix sections
./run.sh build                # build only

# Against a non-ASan libdb (the OOB write then goes unnoticed, but the
# mode/conflict checks still run):
LIBDB_ASAN=0 ./run.sh
```

Under meson (plain, non-ASan libdb — still aborts, via glibc):

```sh
meson setup build && ninja -C build test/tiers/test_lock_matrix
meson test -C build --suite tiers-xfail
```

Environment: `CC` (default `clang`, needed for the ASan libdb), `LIBDB_ASAN`
(default 1), `LIBDB_BUILD` (explicit build dir, skips the ASan auto-build),
`LOCK_TIMEOUT` (default 600s).

## Exit status

- `0` — the matrix completed and every check held.
- `1` — a check failed, **or** ASan aborted (the sanitizer's own exit). The
  heap-buffer-overflow report plus the last progress line identify the shape.
- `2` — harness error.
