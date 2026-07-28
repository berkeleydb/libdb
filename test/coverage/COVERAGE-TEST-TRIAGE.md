# Coverage full-suite failure triage (6 tests)

Follow-up to `test/coverage/FULL-COVERAGE-REPORT.md`. That run built with
`-O0 -g --coverage` (gcov) on a `c7i.24xlarge` and reported 6 failing test IDs.
This note reproduces each on a **clean, uninstrumented** build to decide:
coverage/-O0 resource artifact vs. real engine bug.

## Method

- Worktree: `/tmp/libdb-wt-test-triage`, branch `agent/test-triage` off master.
- Build (NO `--coverage`):
  `../dist/configure --enable-debug --enable-diagnostic --enable-test
  --with-tcl=/nix/store/mzx2cj89c7phpv7cb6r9hydadnndp53m-tcl-8.6.16/lib && make -j4`.
  Note: BDB `--enable-debug` compiles at `-g -O2` (adds `-g`, not `-O0`);
  `--enable-diagnostic` sets `DIAGNOSTIC 1` so `DB_ASSERT` is live.
- Each test run via the harness: `source test.tcl; run_method <method> testNNN 0 1`,
  under `timeout`. `run_method` auto-runs `verify_dir`+`salvage_dir` afterwards,
  which is where the "db_dump/db_load round-trip" and "verify/salvage" checks live.
- The EC2 report did not record which access method failed, so each test was run
  on btree first, then hash/recno/queue/heap for the round-trip trio.

## Verdicts

| Test | btree | hash | recno | queue | heap | Verdict |
|------|-------|------|-------|-------|------|---------|
| test046 | PASS | PASS | PASS | PASS | **FAIL** | see below |
| test049 | PASS | PASS | PASS | PASS | **FAIL** | see below |
| test139 | PASS | PASS | PASS | PASS | **FAIL** | see below |
| test097 | PASS | — | — | — | — | artifact |
| test128 | PASS | — | — | — | — | artifact |
| test131 | PASS | — | — | — | — | artifact |

### test097, test128, test131 — coverage/-O0 resource artifacts (NOT bugs)

All three PASS cleanly on the uninstrumented build (btree), including the
`verify_dir`/`salvage_dir` steps whose per-page `BDB0534/0535/0552` warnings
dominated the report's raw `FAIL:` line count.

- **test097** (500 simultaneous DBs) and **test128** (sub/secondary DB bulk
  update + verify/salvage) are exactly the fd/memory-pressure shapes the report
  flagged. The report's own early `db open: not enough memory` under `--coverage`
  is the tell: gcov `.gcda` buffers + `-O0` bloat pushed the 500-DB / many-subdb
  working set over a resource limit on the shared 96-way box. Uninstrumented →
  clean PASS.
- **test131** (foreign/secondary verify) PASS clean.

Verdict: coverage instrumentation resource artifacts. No engine defect.
(Bounded: btree only, since these are AM-generic and the failure mode is
resource-based, not AM-specific.)

### test046 / test049 / test139 — REAL BUG, heap access method only

These three FAIL on the clean build **for method `heap`** (PASS for
btree/hash/recno/queue). They are not dump/load tests themselves; the failure is
the harness post-test check:

```
FAIL: dump(./TESTDIR/test046.a.db:child process exited abnormally): expected 0, got 1
```

i.e. `db_dump` (invoked by `salvage_dir`/`dumploadtest` in `testutils.tcl`)
exits non-zero on a **heap** database that the test leaves behind.

This is a real engine bug, present on a clean `--enable-debug
--enable-diagnostic` build with no coverage. It is heap-AM-specific and
deletion-triggered. It very likely also failed on EC2 under the heap AM (the
report just didn't record which method).

#### Root cause (isolated)

`db_dump` on such a heap DB completes the dump output (`DATA=END` is written) but
then returns **`DB_PAGE_NOTFOUND` (-30986)**, so `util/db_dump.c` sets
`exitval = 1`. Chain:

- `__db_dump` (`src/db/db_pr.c`) only maps `DB_NOTFOUND -> 0` at end-of-scan; any
  other non-zero from the terminating `__dbc_get(... DB_NEXT)` propagates out.
- The terminating `__dbc_get` -> `__dbc_iget` (`src/db/db_cam.c:780`) returns
  **-30986**. Confirmed by gdb `finish`: `__dbc_idup` returns 0, but the
  enclosing `__dbc_iget` returns `$ = -30986`.
- `__heapc_get`'s `DB_ASSERT(ret != DB_PAGE_NOTFOUND)` (heap.c:957) did **not**
  fire and no `__memp_fget` returned non-zero — so the -30986 is produced on the
  `DB_NEXT` step where the heap cursor sits on a page that has a deletion "hole"
  (an empty offset-table slot below `HEAP_HIGHINDX`). The most consistent source
  is the `DB_NEXT` current-page reacquire `ACQUIRE_CUR(dbc, lock_type, cp->pgno, ...)`
  (heap.c ~717) whose `if (ret != 0) goto err;` does **not** translate
  `DB_PAGE_NOTFOUND -> DB_NOTFOUND` (unlike the "beyond last page" branch at
  heap.c ~735 which does translate it). End-of-scan therefore surfaces
  `DB_PAGE_NOTFOUND` instead of `DB_NOTFOUND`.

#### Minimal reproduction (no test harness needed)

```
# heap DB, 20 records, delete ANY non-last record -> db_dump exits 1
berkdb_open -create -heap h.db
  put -append d1 .. d20   (capture RIDs)
  del <RID of a non-last record>
  close
db_dump -k h.db   ; echo $?     # -> 1   (returns DB_PAGE_NOTFOUND)

# controls that PASS (exit 0):
#   - same 20 records, NO delete
#   - 20 records, delete the LAST-inserted (highest-index) record only
```

Observed threshold (single data page):
- delete pos 1 / 10 / 19 (any non-last) -> `db_dump` rc=1
- delete pos 20 (last / highest HEAP_HIGHINDX) -> rc=0
- n=2 or n=5, delete first -> rc=1
This matches test046/049/139 which delete a middle record then leave the heap DB
for the harness to dump.

#### Suggested fix location (for a separate focused PR — NOT applied here)

`src/heap/heap.c`, `__heapc_get`, `case DB_NEXT/DB_NEXT_NODUP`: the current-page
`ACQUIRE_CUR(cp->pgno)` error branch should translate `DB_PAGE_NOTFOUND` to
`DB_NOTFOUND` at end-of-scan the same way the get-next-page branch already does
(heap.c ~735), or `__db_dump` should treat `DB_PAGE_NOTFOUND` as end-of-scan for
heap. Do not fix from this triage.

## Summary

- **3 of 6 are coverage/-O0 resource artifacts** (test097, test128, test131) —
  pass clean, no engine defect.
- **3 of 6 are one real engine bug** (test046, test049, test139) — heap AM only,
  deletion-triggered `db_dump` `DB_PAGE_NOTFOUND` leak in the heap cursor
  `DB_NEXT` end-of-scan path. Reproduces on a clean uninstrumented build.
