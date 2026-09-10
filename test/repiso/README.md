# Tier B4 — two-site replication isolation harness

Two **real OS processes** — a master and a client — over a **real TCP socket**,
using the **Base Replication API** (`rep_set_transport`, `rep_start`,
`rep_process_message`), modelled on `examples/c/ex_rep/base`.

## Why this tier exists

Issue #140 was a heap out-of-bounds **write** in `__lock_vec`, fixed in #145 and
gated by `test/lockmatrix/`. It had a **second** consequence which the reporter
derived from source and stated explicitly could not be observed:

> "The client isolation consequence follows from source analysis and has not
> been directly observed."

This tier is that observation. It is also this project's **first executable
replication-isolation test**: `rep/` + `repmgr/` is ~12.4k lines and the
existing suite reaches none of the multi-process apply-under-contention paths.

### The mechanism, end to end

1. On the master, `__txn_commit` asks `__lock_vec` for the transaction's
   retained write locks (`DB_LOCK_PUT_READ` with an objlist) and logs them in
   the `__txn_regop` record. Before #145 the objlist was **sized** from
   `sh_locker->nwrites` — which does not count `DB_LOCK_SIREAD`, the SSI read
   marker — while the **populate** loop wrote a descriptor for every retained
   lock of either kind. `__lock_fix_list` then serialized only the first
   `nwrites` descriptors.

2. Newly granted locks go to the **head** of the locker's `heldby` list. So a
   transaction that **writes** page A and then **reads** page B holds
   `[SIREAD(B), WRITE(A)]` in that order, and a 1-entry truncation keeps
   `SIREAD(B)` and **drops** `WRITE(A)`. The commit record therefore names the
   page the transaction did *not* modify and omits the one it did.

3. On the client, `__rep_process_txn` (`src/rep/rep_record.c:1617`) calls
   `__lock_get_list(env, locker, 0, DB_LOCK_WRITE, lock_dbt)`: apply reacquires
   every **listed** object as a write lock, and takes no other page locks — the
   apply cursor is `DBC_RECOVER`, which `__db_lget` short-circuits on a
   replication client (`src/db/db_meta.c`). An omitted page is therefore **not
   locked**.

4. So apply modifies that page while an ordinary client transaction holds a read
   lock on it, and a repeated read in that transaction returns a different
   value — a **non-repeatable read**, which its isolation level forbids.

### Why the in-process harness cannot see this

The `rep0NN` Tcl tests hand-carry replication messages between environments
inside one `tclsh` (`test/tcl/reputils.tcl`'s `replsend` / `process_msgs`). In
that harness the client's apply and the client's reader are **the same thread**,
so the reader can never be holding a lock while apply runs. The anomaly is
structurally unobservable there. See `test/coverage/REPLICATION-COVERAGE.md`.

The repmgr 100-series is genuinely multi-process but needs the `db_repsite`
utility, which is **not present in this fork**.

## The two checks

Both come out of the **same** master+client run.

### 1. `locklist` — deterministic (THE GATE)

The invariant, as a property of the log:

> Every page a replicated transaction **modified** must appear in that
> transaction's **commit lock list**.

This is the **authoritative check**. It is deterministic, needs no timing and no
race, and its verdict is a pure function of the log bytes.

`check_locklist.c` reads `db_printlog` output from the **client's own replicated
log** and, per committed transaction, compares the set of pages its
page-modification records touch against the set its commit lock list names. A
violation names both sides:

```
OMISSION txn 80000049 modified pgno 2 (fileid 0) but its commit lock list
         contains only {3} -- apply would modify pgno 2 while holding no lock on it
         (txn 80000049 modified {2}, locked {3})
```

The commit lock list only exists when the writing site is a **master**:
`src/txn/txn.c:881-887` sets `request.obj = &list_dbt` only under
`IS_REP_MASTER(env) && !IS_ZERO_LSN(td->last_lsn)`. A plain `DB_INIT_REP`
environment with no `rep_start` logs an **empty** list, so the two-site setup is
load-bearing rather than incidental. `txn.c:899` logs it as `__txn_regop`'s
`locks` DBT, which is why it is recoverable from the client's own replicated log.

On the consuming side, `__lock_get_list` (`src/lock/lock_list.c`) reads `nlocks`
via `GET_COUNT` and loops exactly that many times — an omitted object is simply
never locked, **silently, with no error return**. Nothing downstream compensates.

Parsing `db_printlog` rather than linking the engine is deliberate. The commit
lock list is serialized by `__lock_fix_list` into a private format (count,
per-object page counts, coalesced-by-fileid page runs) that only
`__lock_list_print` and `__lock_get_list` decode — and `__lock_list_print` is
what `db_printlog` already calls. The dump **is** the engine's own reading of
the list, so there is no second decoder to get wrong.

(`db_printlog` in this fork has **no `-a` flag**; the plain `-h DIR` dump is what
the checker consumes.)

The checker is **deliberately conservative**: excluding a field or a record type
can only make it *miss* an omission, never invent one. A missed omission is
caught by check 2 in the same run; an invented one would be unfalsifiable noise
that gets the tier disabled. (`next:`/`prev:` are excluded for exactly this
reason — they double as free-list pointers and appear inside `__bam_split`'s
embedded page dumps, and including them produced false omissions on a
known-good library.)

### 2. `anomaly` — behavioural (non-gating)

The client opens a transaction at **default isolation** (`DB_TXN_SNAPSHOT` is
rejected outright on a replication client, `src/txn/txn.c:236`, so this is
ordinary two-phase locking), reads `aaa_write`, signals the master, waits, then
**re-reads the same key in the same transaction**. The two reads must agree.

The read lock is held across the **entire** window, so there is no interleaving
to win — the outcome is decided by whether the modified page reached the commit
lock list, not by who gets scheduled.

**Anti-vacuous checks.** A harness that never delivered the update would report
"both reads agree" and look identical to a pass. So:

- After committing, the reader polls until the new value **is** visible. If it
  never becomes visible the verdict is `INCONCLUSIVE`, **not** `PASS`.
- The master writes a **sentinel** record to a *separate* database immediately
  after the trigger commit. Apply is sequential in the client's single message
  thread, so the sentinel cannot become visible until the trigger transaction
  has been applied. `sentinel_while_holding=1` is therefore independent
  evidence that apply was **not** blocked.

  **In practice this signal is weaker than intended, and the reason matters.**
  On the reverted library the master often *never writes* the sentinel, because
  it **crashes first** — the same truncated-list bug is a heap overflow on the
  master (see the results table). So `sentinel_while_holding=0` on a reverted
  run does **not** mean apply was blocked; it usually means the master died.
  The signal is only meaningful when the master exited cleanly. The verdict
  never depends on it: it is printed as evidence, and the `v3` visibility poll
  is what actually guards against a vacuous pass.
- The master verifies the B-tree actually split (`DB->stat`'s `bt_leaf_pg >= 2`)
  and reports `SHAPE_NOT_ESTABLISHED` if not, rather than testing a shape it
  did not build.
- `run.sh` reports a master killed by a **signal** as its own failure, so the
  master-side crash cannot hide behind the client's verdict.

## The trigger shape

A logged, top-level `DB_TXN_SNAPSHOT` transaction on a database opened
`DB_MULTIVERSION`, doing **one write then one read**, where the two keys land on
**different B-tree leaf pages**.

- `DB_MULTIVERSION` is required: it is what makes a `DB_TXN_SNAPSHOT` read take
  a `DB_LOCK_SIREAD` marker (`src/db/db_meta.c:1191`) instead of no lock at all,
  and the SIREAD marker is what truncated the list.
- **Write first, read second.** The truncation keeps the *head* of `heldby`, so
  the SIREAD must be the newer lock to displace the write lock. Reading first
  puts the write lock at the head and the list is (accidentally) still correct.
- 512-byte pages plus 64 filler keys sorting strictly between `aaa_write` and
  `zzz_read` split the leaf; `DB->stat`'s `bt_leaf_pg` verifies it (33 leaf
  pages in practice).

## Results

Measured on an idle EC2 `c7i.4xlarge` (load 0.2-1.6), `--enable-debug` build,
Ubuntu 24.04, gcc 13.

| library | `locklist` (gate) | `anomaly` | master |
|---|---|---|---|
| master (fix present) | **PASS** — 70 committed txns checked, **0** omissions, 8/8 runs | **PASS** — `v1=1 v2=1 v3=99` | clean exit |
| `IS_WRITELOCK` sizing predicate **reverted** | **FAIL** — `OMISSION txn 80000049 modified pgno 2 ... contains only {3}`, 6/6 runs | **ANOMALY** — `v1=1 v2=99`, 6/6 runs | **SIGSEGV in `__lock_vec`** |

**The client-side consequence of #140 is directly OBSERVED**, not inferred. Both
checks fire, in every one of 6 reverted runs, and both are clean in every one of
8 fixed runs — 0 false positives across 70 unrelated committed transactions.

The reverted master additionally **segfaults** inside `__lock_vec`:

```
#0  __db_tas_mutex_lock_int (env=0x5d500000001c, ...) at mut_tas.c:95
#2  __lock_vec (...) at ../src/lock/lock.c:463
#4  __txn_end (txn=..., is_commit=1) at ../src/txn/txn.c:1802
#5  __txn_commit (...) at ../src/txn/txn.c:975
```

Note the corrupted `env=0x5d500000001c`. That is the #140 **master-side** heap
out-of-bounds write, reproduced by this tier as a side effect — the overflowing
`np++` writes past the objlist into adjacent heap, and the next `__lock_vec`
call follows a smashed pointer. Under `--enable-diagnostic` the same shape
instead trips the old `DB_ASSERT` at `lock.c:489`, which is precisely why #145
replaced that diagnostic-only assert with a runtime bounds check and
`__env_panic`.

So on the reverted library the trigger shape reproduces **both** consequences of
#140 from one run: the master-side overflow and the client-side isolation
violation.

## Reproducing the teeth demonstration

```sh
cd /path/to/worktree
git show 166e27eb7 -- src/lock/lock.c > /tmp/fix140.patch
git apply -R /tmp/fix140.patch          # revert; do NOT commit
(cd build_unix && make libdb.a db_printlog)
(cd test/repiso && rm -f build/test_rep_iso && ./run.sh)   # expect FAIL both checks
git apply /tmp/fix140.patch             # restore
```

Under `--enable-diagnostic` the reverted library instead **aborts** at the old
`DB_ASSERT` in `__lock_vec` on the same shape — which is the #140 master-side
reproduction, and is why the runtime bounds check replaced that assert.

## Architecture

```
run.sh
 ├── builds test_rep_iso (driver + transport) and check_locklist
 └── per run:
     ├── fork: test_rep_iso --role=master --port=N   (listens, then rep_start MASTER)
     ├── fork: test_rep_iso --role=client --port=N   (connects, rep_start CLIENT)
     │     both: one reader thread -> rep_process_message
     ├── rendezvous via flag files in a shared dir
     │     reader_holds -> trigger_done -> client_done
     ├── collects the CLIENT's verdict (check 2)
     └── db_printlog -h client/ | check_locklist      (check 1)
```

`rep_iso_net.c` is a ~300-line cut-down of `ex_rep`'s `rep_net.c`: two sites
with fixed roles, so no machine table, no elections, no site discovery. It keeps
the ex_rep wire format (4-byte rec size, rec, 4-byte control size, control).
A harness that is mostly its own transport is a harness whose failures are its
own fault.

Rendezvous is by flag file rather than a second socket: it cannot perturb the
replication transport under test, and the **master clears all flags at startup**
so a rerun in a reused directory cannot see a previous run's `reader_holds` (a
stale one would let the master commit before the client took its lock — a pass
that observed nothing).

## Deliberate configuration choices

| choice | why |
|---|---|
| `DB_TXN_NOSYNC` | As `ex_rep` does (`rep_common.c:512`). The master writes 66 setup records; per-commit fsync cost ~30s per run. Both sites `log_flush` before shutdown. |
| **no** `txn_checkpoint` | A checkpoint on a replication master sleeps for `DB_REP_CHECKPOINT_DELAY` (**default 30s**, `src/rep/rep_method.c:54`) to let clients catch up. Measured 30.046s → 0.024s once removed. `log_flush` is all the tier needed. |
| lock timeout 500ms, **detector off** | Apply reacquires the listed write locks and waits. With the fix, apply correctly blocks on the reader — and with no timeout that correct behaviour would be a *hang*. A timeout makes it `DB_LOCK_NOTGRANTED`, which `__rep_process_rec`'s `do/while` retries. The detector is off on purpose: apply runs at `DB_LOCK_MAXPRIORITY`, so a detector resolves the conflict by killing the **reader**, destroying the observation instead of making it. |
| client opens its DBs inside a txn | `__db_open` sets `DB_AM_TXN` only when `IS_REAL_TXN(txn)`. Without it every later txn-scoped get is rejected ("Transaction specified for a non-transactional database") — which would silently make the reader non-transactional and holding no lock at all. |

## Timing and timeouts

Every number here is a **measured spread**, never a single sample. A timeout set
from one measurement failed release qualification twice in this repo.

**`RISO_SENTINEL_POLL_SECS = 10`** — how long the client keeps its read lock
while polling for the sentinel. With the fix present this is *expected* to
expire (apply is correctly blocked), so the whole budget is spent every run.

The interval it must cover is `apply_ms`: from the master signalling
`trigger_done` to the client having applied the trigger and made the sentinel
visible. Measured with the fix **reverted** (the only case where the sentinel
does arrive), 21 runs:

```
0 0 0 1 11 20 21 41 95 10269 …   -> the sub-100ms values are the real apply
                                     latency; min 0, median ~20, max 95 ms
```

10s is **>100x** the worst observed apply latency. Do **not** tighten it from a
single measurement: a too-tight budget here does not fail the tier, it silently
converts "apply was blocked" (correct) into "apply had not got there yet"
(proves nothing), which is worse than a failure.

**`REPISO_TIMEOUT = 180`** — wall clock for one master+client pair. Measured on
the idle EC2 `c7i.4xlarge`, **N=8** consecutive runs of `./run.sh locklist`:

```
11.67 11.66 11.67 11.76 11.66 11.75 11.65 11.66  s
min 11.65  median 11.665  max 11.76  spread 0.11s
```

of which ~10s is the deliberate sentinel poll. On the shared local box at load
25-37 the same runs took **51-92s** — a **7x** spread from load alone, which is
precisely why the ceiling is generous. 180s is >2x the worst loaded observation
and >15x the idle median. The failure mode being guarded is "did not finish",
not "slightly slow", so a generous ceiling costs nothing.

## Usage

```sh
cd test/repiso
./run.sh            # build + both checks
./run.sh build      # build only
./run.sh locklist   # deterministic log check only (1 run)
./run.sh anomaly    # behavioural check only
```

Environment: `CC`, `LIBDB_BUILD` (default `../../build_unix`), `REPISO_TIMEOUT`,
`REPISO_PORT` (default 39100), `REPISO_RUNS` (default 3), `REPISO_KEEP`,
`REPISO_VERBOSE`.

Requires a real `build_unix` — like `test/isolation/` and `test/soak/`, a
symlink does not satisfy it.

## CI

`.github/workflows/rep-isolation.yml`.

- **`workflow_dispatch` is the AUTHORITATIVE mode** — it runs `REPISO_RUNS=5`
  and **fails the job** on any omission (check 1) or crash. Check 2's anomaly
  verdict is reported but, being the timing-dependent one, does not by itself
  fail the authoritative run.
- Per-push / PR and the nightly schedule are **advisory**: they run and report,
  but do not fail the build.

That split is deliberate and matches the house policy in `test-tiers.yml` (B1
per-push hard gate, B2 schedule+dispatch). Real-socket multi-process tests are
timing-sensitive on shared runners, and a flaky **gate** gets disabled — at
which point it protects nothing. An advisory signal that always runs plus an
authoritative mode that is trusted is worth more than a hard gate someone
switches off.

## What remains unobservable

Stated plainly, because a harness's limits are part of its result.

- **A window narrower than one apply.** The reader holds its lock across the
  whole window by design, which is what makes the observation deterministic. A
  variant where apply and the reader genuinely interleave *within* a single page
  operation is not reachable from the application API.
- **Multi-file transactions.** `check_locklist` matches on `pgno` only: the page
  records print `fileid: N` (the dbreg id) while the lock list prints the raw
  5-byte fileid, so the two never spell a file the same way. The harness
  replicates one interesting database, so this is unambiguous here; a
  multi-database checker would have to join through the `__dbreg_register`
  records.
- **Record types not in `is_page_modify`.** Conservative by design (see above),
  so an omission in an unlisted record type would be missed by check 1 — though
  not by check 2 if it changed a value.
- **Whether other lock modes can produce the same truncation.** `DB_LOCK_IREAD`
  and `DB_LOCK_WAIT` are also non-write modes that `__lock_vec` retains. The
  general invariant is gated by `test/lockmatrix/`; this tier drives only the
  SIREAD path, because that is the one the SSI work actually creates.
- **Election / lease paths.** Fixed roles, two sites, no elections. `rep_lease.c`
  stays at 0% (see `REPLICATION-COVERAGE.md`).
