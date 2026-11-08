# `lock_vec` atomicity re-audit (A4, 2026-11)

> Review item A4 (REVIEW-2026-11.md §1.5; `A/bostic.md` action item 2):
> re-audit every `__lock_vec` caller for the atomicity Berkeley DB's own design
> note flags as lost once the lock region stopped being a single mutex, now the
> 64-stripe locker table and `lk_partitions` object partitions are the default.

## 1. The hazard, from the source

Bostic's in-tree design note states the FAQ plainly
(`sources/historical/lock-Design.txt`, around line 292, Sleepycat bug #553):

> **Q: What are the implications of doing fine grain locking?**
> A: Since we no longer globally lock the entire region, `lock_vec` will no
> longer be atomic.  We still execute the items in a `lock_vec` in order, so
> things like lock-coupling still work, but you can't make any guarantees about
> atomicity.

So the note gives two guarantees and withdraws a third:

* **Kept:** the elements of one vector execute **in program order**.
* **Kept:** each single element is atomic (its own object/partition mutex is
  held across that one operation).
* **Withdrawn:** the *whole vector* is no longer one atomic step. Between
  element *i* finishing and element *i+1* starting, no lock-region mutex is
  held, so another locker can observe the intermediate state and act on it.

## 2. That hazard is live on master (not hypothetical)

`__lock_vec` (src/lock/lock.c:485) opens with `LOCK_SYSTEM_LOCK(lt, region)` and
closes with `LOCK_SYSTEM_UNLOCK`. But those macros (src/dbinc/lock.h:461-470)
only take a mutex when there is a **single** object partition:

```
#define LOCK_SYSTEM_LOCK(lt, reg) do {            \
    if ((reg)->part_t_size == 1)                  \
        MUTEX_LOCK((lt)->env, (reg)->mtx_region); \
} while (0)
```

The default partition count is set in `__lock_env_create`
(src/lock/lock_method.c:42):

```
dbenv->lk_partitions = cpu > 1 ? 10 * cpu : 1;
```

So on **any multi-CPU machine** (every server and laptop the fork targets),
`part_t_size > 1`, `LOCK_SYSTEM_LOCK` is a **no-op**, and the only mutexes
`__lock_vec` holds are the **per-object partition** mutexes that
`__lock_get_internal` / `__lock_put_internal` / `OBJECT_LOCK` take and release
**inside each element**. A vector of N elements therefore acquires and releases
up to N different partition mutexes, one at a time, holding none of them across
the element boundary.

A single-partition environment (`set_lk_partitions(1)`, or a single-CPU box) is
the only configuration where the old whole-region atomicity still holds, because
there `LOCK_SYSTEM_LOCK` really is the region mutex and it wraps the whole loop.

**Conclusion of §2:** the audit must assume the *non-atomic* vector on the
default build and ask, for each caller, whether correctness depends on
whole-vector atomicity that it no longer gets.

## 3. Every `__lock_vec` caller

Found with `grep -rn '__lock_vec\b' src` at master `e1fe5c2c4`.
`nlist` is the number of elements in the vector handed to one call.

| # | Caller | What the vector does | `nlist` | Relies on whole-vector atomicity? | Holds today? |
|---|--------|----------------------|---------|-----------------------------------|--------------|
| 1 | `__db_close` db/db.c:1131 | `PUT_ALL` (release handle lock on close) | 1 | No — single element | Yes (single element is atomic) |
| 2 | `__db_cursor_int`/trade db/db_am.c:425 | `TRADE` (hand the handle lock to the txn locker) | 1 | No — single element | Yes |
| 3 | `__cdsgroup_commit` db/db_cds.c:76 | `PUT_ALL` (CDS group commit releases handle locks) | 1 | No — single element | Yes |
| 4 | `__db_lock_dump` db/db_meta.c:1134 | `DUMP` (DIAGNOSTIC debug print) | 1 | No — single element, debug only | Yes |
| 5 | `__db_lget` db/db_meta.c:1306 (LCK_COUPLE / LCK_DOWNGRADE) | `[GET new]`+`[PUT old]`, or `[GET WWRITE]`+`[GET new]`+`[PUT old]` — **lock coupling** | 2–3 | **Order-only** — see §4 | **Yes** — order guarantee is what lock-coupling needs, and order is kept |
| 6 | `__db_lput` db/db_meta.c:1442 (LCK_DOWNGRADE) | `[GET WWRITE]`+`[PUT old write]` — downgrade a write lock to dirty-read | 2 | **Order-only** — see §4 | **Yes** — same reasoning as #5 |
| 7 | `__db_dispatch`/recovery db/db_rec.c:2528 | `PUT_ALL` (release locks after a recovered sub-op) | 1 | No — single element; also recovery is single-threaded | Yes |
| 8 | `__fop_lock_handle` fileops/fop_util.c:144 | `[PUT old elock]`+`[GET handle lock]` — release the open-protocol lock, take the handle lock | 2 | **No** — the PUT is a *deliberate* release; see §5 | **Yes** — intended release-then-acquire, no atomicity wanted |
| 9 | `__lock_failchk` lock/lock_failchk.c:155 | `PUT_READ` (reap a dead non-txn locker's read locks) | 1 | No — single element | Yes |
| 10 | `__rep_process_txn` rep/rep_record.c:1655 | `PUT_ALL` (release the replication-apply locker's locks) | 1 | No — single element | Yes |
| 11 | `__txn_commit` txn/txn.c:1029 | `PUT_READ` (release read locks at commit, build the write-lock list for rep) | 1 | No — single element; see §6 | Yes |
| 12 | `__txn_abort` txn/txn.c:1397 | `UPGRADE_WRITE` (promote WWRITE→WRITE before undo) | 1 | No — single element | Yes |
| 13 | `__txn_prepare` txn/txn.c:1579 | `PUT_READ` (release read locks at prepare) | 1 | No — single element | Yes |
| 14 | `__txn_end` txn/txn.c:1956 | `PUT_ALL` (commit/abort) or `INHERIT` (child→parent) | 1 | No — single element; see §6 | Yes |
| 15 | `DO_TRADE` macro txn/txn_util.c:239 | `TRADE` (txn-event lock trade to parent/handle locker) | 1 | No — single element | Yes |
| 16 | `__lock_vec_pp` lock/lock.c:84 (application API) | **arbitrary** — whatever the application passes | any | **Possibly** — application-defined; see §7 | Contract was never whole-vector-atomic; see §7 |

### Internal multi-element ops that are themselves atomic within one element

`PUT_ALL`, `PUT_READ`, `UPGRADE_WRITE` and `INHERIT` each *look* like they touch
many locks, but each is a **single `list[i]` element**. Inside that one element
`__lock_vec` walks the locker's `heldby` list and, for every lock, takes that
lock's object partition (`OBJECT_LOCK_NDX`), mutates, and releases
(src/lock/lock.c:590-640). So these are **not** multi-element vectors — they are
one element that iterates. Their guarantee is unchanged by partitioning: within
one element, each lock's own partition mutex still covers its own mutation. They
never needed whole-region atomicity across *different* locks, because the result
("this locker now holds no read locks") is correct regardless of the order in
which each lock's partition mutex is taken: a locker's own lock set is private to
that locker (the comment at lock.c:600 — "any threads sharing this locker must
not be in the API at the same time").

## 4. The only vectors with >1 element that matter: lock coupling (#5, #6)

`__db_lget` with `action == LCK_COUPLE` builds `[GET new page lock, PUT old page
lock]`; `LCK_DOWNGRADE` builds `[GET WWRITE on this page, PUT old WRITE]`; the
three-element form prepends a `GET WWRITE`. This is classic **lock coupling**:
hold the lock on the page you are leaving until you hold the lock on the page you
are entering, so the tree position is never unprotected.

**Does it rely on whole-vector atomicity?** No — it relies only on **order**,
which the design note explicitly keeps. The GET element runs first and completes
(its partition mutex taken and released, the lock now granted to this locker)
*before* the PUT element releases the old lock. At no instant is the locker
without a lock on the path: it holds the old lock until element 1 has already
granted the new one. A concurrent locker that observes the gap between the two
elements sees the locker holding **both** locks, never neither — which is the
safe direction. Lock coupling is exactly the case the note names as still
correct, and it is still correct here.

(The reverse — release the old lock, *then* try to get the new one — would be
unsafe without atomicity, because the gap exposes an unprotected position. That
pattern does not occur in lock coupling; it occurs in `__fop_lock_handle` (#8),
where it is deliberate — see §5.)

## 5. `__fop_lock_handle` PUT+GET (#8): a deliberate release, not a lost atom

`reqs[0] = PUT elock; reqs[1] = GET handle lock`. This is release-then-acquire,
the order §4 warns about. But here it is **intended**: `elock` is the
file-registry / directory lock held during the open/create/rename protocol, and
the handle lock is on the file's metadata page — a *different* object. Dropping
`elock` is the whole point (it lets another opener make progress); the open
protocol is written to tolerate another process acting in that window (it
re-reads metadata and revalidates, e.g. the `CLOSE_HANDLE` + retry at
fop_util.c:462-470). The caller never assumes the PUT and GET are one atomic
step; it only uses one `__lock_vec` call so that, under the old single-partition
build, a blocking GET would not hold `elock` while it waits. Non-atomicity here
changes nothing the open protocol did not already handle. **No atomicity
dependency.**

## 6. The txn commit/abort/end single-element ops (#11, #12, #14)

These were examined with extra care because they touch the SSI read/write
antidependency machinery (issues #136–#140), where cross-subsystem ordering
matters. Each is a single `__lock_vec` element, so there is no *inter-element*
gap to worry about. The cross-subsystem serialization they do need — against
`__txn_commit`'s pivot check and `__memp_si_rwconflict` (issue #136) — is
**already handled inside `__lock_get_internal`**, which was audited for exactly
this partition hazard. When `part_t_size != 1` (i.e. `LOCK_SYSTEM_LOCK` is a
no-op), `__lock_get_internal` takes the txn-region latch itself
(`si_txn_lock = TXN_ON(env) && (region->part_t_size != 1 || txn_split)`,
src/lock/lock.c:1068), precisely because it can no longer rely on the aliased
region mutex being held by `LOCK_SYSTEM_LOCK`. That is the audit A4 asks for,
already done for the one place that needed it, with a DIAGNOSTIC assertion
(lock.c:1069-1081) that fires if a future change re-aliases or drops the hold.
**No further atomicity dependency in the txn callers.**

## 7. The application API `__lock_vec_pp` (#16): the only residual exposure

`DB_ENV->lock_vec` lets an application submit an arbitrary multi-element vector.
An application *could* write code that assumes the whole vector is one atomic
step against other lockers — e.g. `[PUT A, GET B]` expecting no third party to
grab A in the gap. On the default (multi-partition) build it does **not** get
that.

This is **not a regression the fork introduced in behavior**: the documented
contract of `DB_ENV->lock_vec` has never promised whole-vector atomicity against
other lockers. The reference documentation describes `lock_vec` as performing
the operations *"in the order they are specified"* and returning the element at
which a failure occurred — an **ordering** contract, the same one the design
note keeps. No Berkeley DB manual text promises region-wide atomicity of a
vector. The fork changed the *default* from single-partition (where the old
region mutex incidentally made a vector atomic) to multi-partition (where it is
not), so an application that **relied on the incidental atomicity** of the old
default would now be surprised.

**Recommendation (documentation, not code):** state the ordering-not-atomicity
contract explicitly in the `DB_ENV->lock_vec` reference and in a note under
`set_lk_partitions`, so an application author cannot mistake the single-partition
behavior for a guarantee. This matches review item A2's style (document a
sharp edge rather than paper over it) and needs no engine change. The test in
§8 pins the behavior so the doc cannot drift from reality.

## 8. Verdict

**No internal caller of `__lock_vec` relies on whole-vector atomicity that it
fails to get on the default multi-partition build.**

* 11 of the 16 call sites submit a **single-element** vector, where the question
  does not arise (one element is atomic under its own partition mutex).
* The two genuine multi-element internal vectors (#5, #6) are **lock coupling**,
  which the design note names as still correct because it needs only element
  order — and order is preserved.
* The one release-then-acquire internal vector (#8) is a **deliberate** release
  whose window the open protocol already tolerates.
* The SSI-sensitive txn ops (#6, #11, #12, #14) get their cross-subsystem
  ordering from `__lock_get_internal`'s own partition-aware latch
  (`si_txn_lock`), already audited and asserted.
* The only residual exposure is the **application API** (#16): an app that
  assumed the old single-partition incidental atomicity. The fix is a
  **documentation** clarification of the long-standing ordering-only contract,
  not an engine change.

Because no caller needs a fix, A4 produces a **regression test that pins the
contract** (§9) rather than a code fix. The test makes the "ordering, not
atomicity" behavior a checkable property: it demonstrates (a) lock coupling's
order guarantee holds (the coupling locker is never lock-less), and (b) a
multi-element application vector is observably non-atomic on the default build —
so the documented contract matches reality, and a future change that silently
restored or further weakened whole-vector behavior would move the test.

No RFC is required: nothing changes the lock region. (RFC number 0024 was
reserved for A4 in case a fix touched the region; it is **not used** and is left
available.)

## 9. The pinning test

`test/db/run_lockvec_atomicity.sh` drives `test/db/lockvec_atomicity.c`
(both committed with this audit). It uses two threads, a `pthread_barrier_t`,
and one CONFIG_TEST-only hook `DB_TEST_LOCKVEC_ELEM_GAP` added in `__lock_vec`
(src/lock/lock.c) between elements. The hook yields ~50 ms between vector
elements *only* when the env's `test_abort` field is set to the new
`DB_TEST_LOCKVEC_GAP` slot (src/dbinc/db_int.in), so it is a no-op in every
non-test build and in every other test. It is a **test hook, not a format /
ABI / region change** — the same mechanism (`#if CONFIG_TEST` + `test_abort`
slot + `__os_yield`) the tree already uses for `DB_TEST_SUBLOCKS` and the repmgr
and replication test points — so **no RFC is required**.

The determinism comes from making the inter-element window large (the hook
yield) so a second thread can reliably observe it, rather than racing a
microsecond gap.

**Scenario 1 — the vector is not atomic across elements (the documented
contract).** The worker sets `test_abort = DB_TEST_LOCKVEC_GAP` and submits the
**non-blocking** application vector `[PUT A, GET C]` (C is a free object, so
`GET C` never blocks — this isolates the *inter-element* window, not the
mutex-drop-on-wait window of §2). The hook yields after `PUT A`. A probe thread
(a different locker) acquires A with `DB_LOCK_NOWAIT` and records whether the
worker's `lock_vec` had returned yet. On the engine the probe gets A **while
the worker is still inside `lock_vec`** (`A FREE during vector`) — element 0
released A and element 1 has not completed, and no region mutex spans the two
(partitioned default). PASS.

**Scenario 2 — lock-coupling order holds (the kept guarantee).** The worker
submits the `[GET C, PUT A]` couple `__db_lget` uses. The probe confirms A is
**held for the whole vector** (`A HELD during vector`): the coupling locker gets
the new lock before dropping the old, so it is never lock-less. PASS.

**Teeth (FAIL-before / PASS-after on the detection itself).** The runner also
builds a sabotage arm with `-DLOCKVEC_NO_HOOK`, which keeps everything identical
but does **not** arm the hook. Without the yield the real inter-element window
is microseconds, so scenario 1's probe can no longer catch it: it observes
`A HELD during vector` — the same scenario flips from FREE to HELD purely
because the hook stopped firing. The runner asserts that flip. This proves the
normal arm's `A FREE` is a **genuine detection of the inter-element window, not
timing luck**, and that the test would catch a regression that made `__lock_vec`
hold a lock across all elements (true whole-vector atomicity): such a change
would make even the hooked normal arm observe `A HELD`, and the normal arm would
then FAIL. Verified robust across `-O0/-O1/-O2` and 5 repeated runs.

**Why not force atomicity with `set_lk_partitions(1)`?** An earlier version of
this test tried to force the vector atomic by dropping to one partition. It does
not work, and the reason is the §2 finding: a vector element that *blocks* calls
`LOCK_SYSTEM_UNLOCK` before it waits (src/lock/lock.c:1665), so even at one
partition the region mutex is dropped mid-vector. Single-partition atomicity
holds only for vectors whose elements never block — so the honest sabotage is
"remove the hook," which isolates exactly the observability the test depends on.
