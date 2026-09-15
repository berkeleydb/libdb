# Tier B1 — isolation / anomaly checker

Runs concurrent transaction schedules under `DB_TXN_SNAPSHOT` (which in this
fork means serializable snapshot isolation) and checks the **committed** result
against some serial order of the committed transactions.

## Why this tier exists

The existing suite is strong on crash/durability (`test/sim`, 41 DST
scenarios) and on memory safety against malformed input (`test/fuzz`). Neither
can see a write skew that *commits successfully*: nothing crashes, no page is
corrupt, no sanitizer fires — the database just holds a state that no serial
execution could have produced. Issue #136 is exactly that shape, and it
shipped in v5.3.34.

## The verdict is computed, not hard-coded

Each scenario declares, per transaction, a `model` function: the transaction's
semantics as a pure function over an abstract state vector. After the schedule
runs, the harness reads the real state back out of the databases (after closing
and reopening the environment, so the verdict is about the **durable** state),
then enumerates every permutation of the transactions that actually committed
and applies their models serially. If no permutation reproduces the observed
state, the history is not serializable and the scenario fails with the schedule
printed.

The state vector has two kinds of slot:

- **record slots** — one per database record; the observed value is read back
  from the database.
- **observation slots** — what a transaction claims it *read*. These are what
  make the read-only anomaly checkable: there the stored state is perfectly
  fine and only the read-only transaction's observation has no serial
  explanation. Observation slots of a transaction that did not commit are
  ignored.

## Scenarios

| Scenario | Shape | Expectation on master |
|---|---|---|
| `write_skew_trigger` | two one-page DBs; T2's write lands while T1 is inside `commit` | PASS (`DB_SNAPSHOT_UNSAFE` to T2) — regression gate for #136 |
| `write_skew_control` | two one-page DBs; T2 writes and commits before T1 commits | PASS (`DB_SNAPSHOT_CONFLICT` to T1) |
| `write_skew_late` | two one-page DBs; T2 writes after T1's commit returned | PASS (`DB_SNAPSHOT_UNSAFE` to T2) |
| `write_skew_samebtree_control` | two records on **different pages of one** B-tree; control timing | PASS |
| `write_skew_samebtree_trigger` | same, trigger timing | PASS — regression gate for #136 |
| `g2_antidep` | G2-item: both txns scan for markers, both insert one | PASS |
| `read_only_anomaly` | Fekete's 3-txn pattern; the read-only txn's observation is checked | PASS |
| `lost_update` | both txns read the counter and write read+1 | PASS |
| `read_your_writes` | sanity: a txn must observe its own uncommitted write | PASS |

## The two SSI mechanism gates

`run.sh` also builds and runs two drivers that test the SSI *mechanisms*
rather than schedules. Both were added because the existing coverage of those
mechanisms was about resources and lifetimes, never about a missed conflict.

### `test_ssi_gc_pressure` — marker GC must not drop a marker early

`test/c/leak_si_locker.c` covers the resource side of SIREAD-marker GC (the
committed-reader footprint is a bounded sawtooth) and the marker-lifetime
hardening covers use-after-free. Neither can see the *correctness* failure of
the same machinery: if `__lock_sicleanup` frees a marker while a still-active
transaction could still form the other end of the edge it records, the
conflict is **missed** and a non-serializable schedule commits *silently* — no
crash, no corrupt page, no sanitizer report, no `ENOMEM`.

The driver runs a deterministic single-threaded write skew whose only
remaining evidence at the decisive moment is one committed reader's SIREAD
marker, interleaved with all three real GC triggers: the `txn_begin` pressure
sweep (`nsireaders > st_objects / SI_CLEANUP_TRIGGER_DIV`), forced
`txn_checkpoint` calls inside the window, and a long-lived concurrent snapshot
reader holding the oldest-reader frontier back. The assertion is one exact
bit: the final state `(A,B) == (0,0)` is reachable only if both transactions
committed.

Two anti-vacuity checks: it fails if the live-marker peak never crossed the
sweep threshold (the pressure trigger was not exercised), and at
`ISO_LEVEL=snapshot` it asserts the *opposite* — plain SI takes no SIREAD
markers, so the skew MUST appear, which is what makes a clean serializable run
meaningful. Teeth: neutering the oldest-reader predicate in
`__lock_siclean_obj` makes every iteration commit the skew.

### `test_ssi_crash_pivot` — a pivot must not survive a crash

Issue #136 fixed the *live* race in the pivot's commit window. Cahill's SSI
has no crash model — the pivot argument is about a live execution — so "can a
pivot become durable because the crash landed inside its commit window?" was
unanswered. A child drives a write skew in which T2 reaches its commit-time
pivot check holding both conflict ends while T1's write is already durable,
then dies with `SIGKILL` at one of four points inside `DB_TXN->commit`. The
parent reopens with `DB_RECOVER` and asserts the recovered state is never
`(0,0)`.

The in-commit kill points are unreachable from userspace (all inside one
library call), so they are reached by a **DIAGNOSTIC-only** hook in
`src/txn/txn.c` (`__txn_ssi_crash` / `SSI_CRASH_POINT`), inert unless
`SSI_CRASH_AT` is set *and* the transaction is named `ssi-pivot` via
`DB_TXN->set_name`. It compiles to nothing in a production build.

The observed answer is stronger than "no skew seen": the pivot never reaches
kill point 3 or 4 at all, because `DB_SNAPSHOT_CONFLICT` returns upstream of
the log write — no commit record for a pivot is ever produced. Since "the hook
never fired" is also what a broken hook looks like, the sweep re-runs every
point with the *legal* non-pivot transaction armed; those do die at 3 and 4,
and recovery correctly redoes the flushed commit (point 4) and undoes the
unflushed one (point 3). Teeth: neutering the commit-time pivot check makes
point 4 recover to `(0,0)` — the pivot's commit record was flushed and
recovery redid it.

`ISO_SSI_GATES=0` skips both gates. The crash gate is skipped with a notice if
the library was not built `--enable-diagnostic`.

`read_your_writes` exists so a *vacuously* passing checker is detectable: if
the harness ever stops driving the engine, that scenario fails.

### On the "separate defect" reported alongside #136

The #136 reporter suspected a second, independent defect: two records on
different pages of one B-tree detecting no conflict at all, *even in the
control*. That does **not** reproduce here.
`write_skew_samebtree_control` builds the shape explicitly (512-byte pages plus
filler keys sorting between `alice` and `bob`, giving 33 leaf pages with
`alice` as the minimum key and `bob` as the maximum, verified via
`DB->stat`→`bt_leaf_pg`) and the control correctly returns
`DB_SNAPSHOT_CONFLICT`. Only the trigger timing commits both. On this
construction the different-pages case has the **same** root cause as #136
proper (the commit-window race), not an extra page-granularity hole. The
reporter did not publish their same-btree variant, so their shape may differ;
the control is kept as a live PASS expectation precisely so a real
page-granularity regression would surface here.

## How the #136 interleaving is reached — no engine hook

Landing T2's write while T1 is **inside** `DB_TXN->commit` is done entirely from
the application side: `pthread_barrier` for the ordered phases, plus an atomic
flag that T1 sets immediately before entering `commit` and T2 spins on. This is
the reporter's own technique. **No engine change, no `HAVE_DST` site, zero
production overhead.** A test-only yield point in the commit path was
considered and not needed.

That window is genuinely racy — T1's commit can finish before T2's put reaches
the conflict check, degenerating into the benign "late" schedule. So the racy
scenarios run multiple attempts (40 by default) and the rule is asymmetric on
purpose: **one** violation in any attempt is a reproduction, while a pass
requires **every** attempt to be clean. A serializability violation is a real
counterexample; a single clean run of a racy schedule proves nothing.

In practice both #136 shapes violated on the first attempt before the fix; both
now report `DB_SNAPSHOT_UNSAFE` to T2 in every attempt.

## Running it

```sh
# Build libdb first (once):
cd build_unix && ../dist/configure --enable-debug && make -j"$(nproc)"

# All scenarios:
cd test/isolation && ./run.sh

# One scenario, with the btree-shape diagnostics:
ISO_VERBOSE=1 ./run.sh write_skew_samebtree_control

./run.sh --list        # scenario names, with expect-fail marked
./run.sh build         # build only
```

Environment: `CC`, `LIBDB_BUILD` (default `../../build_unix`), `ISO_TIMEOUT`
(default 300s), `ISO_SAN=1` to add ASan, `ISO_SSI_GATES=0` to skip the two SSI
mechanism gates.

The SSI gates take their own knobs: `SSI_GC_ITER` / `SSI_GC_FILLER` /
`SSI_GC_VERBOSE` for the GC gate, and `SSI_PIVOT_SEEDS` / `SSI_PIVOT_POINTS` /
`SSI_PIVOT_VERBOSE` for the crash gate.

## Exit status

- `0` — every scenario matched its recorded expectation.
- `1` — a scenario did not. Either a new serializability violation, **or** an
  expect-fail scenario that stopped violating, meaning the referenced issue got
  fixed and `expect_fail` should be cleared in the table in
  `test_iso_anomaly.c`. The message says which.
- `2` — harness error.

#136 is fixed (`TXN_DTL_SICHECKED`, see `rfc/0003`), so `expect_fail` is clear
on every scenario and this tier is a plain regression gate: any violation is a
new bug.
