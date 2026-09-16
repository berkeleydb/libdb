# SSI marker-GC margin (issue T1)

`test/isolation/test_ssi_gc_pressure` had a passing window narrow enough that
`SSI_GC_FILLER=300` was needed to keep the isolation tier green. This is the
measurement of why, the mechanism fix, and the margin achieved.

**Outcome: the mechanism was made tolerant.** The gate now passes from
`SSI_GC_FILLER=400` to `50000` — the highest value tested — against a measured
baseline cliff at **525→530**. That is **≥95× headroom**, not 2×. No
`SSI_GC_FILLER` special-casing is needed and none was added.

All measurements on the dedicated box `c7i.4xlarge` (16 vCPU), branch
`fix/ssi-gc-margin`, `--enable-debug --enable-diagnostic`, base
`29f616142`. Both arms built in separate worktrees (`/home/admin/t1-base` at
detached `29f616142`, `/home/admin/t1-wt` at the branch) with separate build
dirs and separate scratch dirs, so no arm can contaminate the other.

---

## 1. Two premises in the issue text are wrong

Both were checked against source and against a running engine before anything
was changed, because the fix depends on which one is true.

### 1.1 `BDB4525` is the txn region, not the lock region

`src/txn/txn.c:516`:

```c
	/* Allocate a new transaction detail structure. */
	if ((ret = __env_alloc(&mgr->reginfo, sizeof(TXN_DETAIL), &td)) != 0) {
		__db_errx(env, DB_STR("4525",
		    "Unable to allocate memory for transaction detail"));
```

`mgr->reginfo` is the **transaction** manager's region. The lock region's own
ENOMEM messages are different strings (`"No space for lock object storage"` in
`__lock_getobj`). So the resource that runs out is `TXN_DETAIL` slots in the txn
region, whose size is fixed at open by `__txn_region_size` from `tx_init`
(default `DEF_MAX_TXNS` = 100, `src/dbinc/txn.h:40`), and `HAVE_MMAP_EXTEND` is
`#undef` on this platform so it cannot grow.

This matters directly: the GC trigger is keyed to the **lock** region
(`st_objects / SI_CLEANUP_TRIGGER_DIV`), so **retuning the divisor cannot move
this cliff**. The trigger already fires on essentially every `txn_begin` in the
failing runs (measured in §3) — firing it more often changes nothing, because
the problem is not trigger frequency.

### 1.2 The "~300–310 markers, <7% margin" window is not what fails

Measured on this box, at the test's own default `SSI_GC_ITER=120`, master passes
at `SSI_GC_FILLER` = 220, 300, 310, 320, 400, 450, 480, 500, 520 — every one, 3
reps each. The cliff is much further out (§2).

The "300–310" figure came from the *other* failure mode. The anti-vacuity check
was

```c
	if (peak_locks <= gc_threshold) {
		printf("FAIL: peak live locks %lu never reached the"
		    " txn_begin sweep threshold %lu -- raise SSI_GC_FILLER; ...
```

i.e. "raise the filler, you did not apply *enough* pressure". That is the
opposite of ENOMEM. On a box whose region sizing gives a larger `st_objects`
(hence a larger threshold) this check fails for filler values that are too
*small* — `test/bench/BATCHED-READS-RESULTS.md:402` records exactly that, "peak
live locks 554 never reached the sweep threshold 600", and confirms it was
byte-identical on master. So the narrow window was two failure modes closing in
from both sides: too little pressure below, ENOMEM above. Both are fixed here,
and the lower one turned out to be a defect in the *check*, not in the engine
(§5).

---

## 2. The cliff curve, with reps

`SSI_GC_ITER=1` for the sweep. Justified, and it is not a shortcut: the ENOMEM
lands inside **iteration 0** (§3 shows the population reaching the limit within
the first window), so one iteration is the whole phenomenon. Long runs are in
§6. Verdict is scraped from the driver's own `PASS:`/`FAIL:`/`BDB4525` line,
never from `rc=0` alone; a run with no verdict line is recorded `NOVERDICT`.

### 2.1 Baseline (master, `29f616142`) — 5 reps per point

| `SSI_GC_FILLER` | markers (hwm) | pass | fail | outcome |
|---:|---:|---:|---:|---|
| 400 | 805 | 5 | 0 | PASS |
| 480 | 965 | 5 | 0 | PASS |
| 500 | 1005 | 5 | 0 | PASS |
| 510 | 1025 | 5 | 0 | PASS |
| 515 | 1035 | 5 | 0 | PASS |
| **520** | **1045** | **5** | **0** | **PASS — last passing point** |
| **525** | **1055** | **5** | **0** | **PASS** |
| **530** | — | **0** | **5** | **`BDB4525`** |
| 540 | — | 0 | 5 | `BDB4525` |
| 560 | — | 0 | 5 | `BDB4525` |
| 600 | — | 0 | 5 | `BDB4525` |
| 800 | — | 0 | 5 | `BDB4525` |

The cliff is **sharp and perfectly reproducible**: 5/5 pass at 525, 5/5 fail at
530. Not a flaky boundary — a hard resource wall.

Marker population tracks filler exactly as `2 * filler + 5` (two filler batches
per iteration plus the schedule's own working set), so the cliff in *markers* is
between 1055 and 1065 — consistent with ~1050 usable `TXN_DETAIL` slots in a
`tx_init=100` txn region. That is the arithmetic confirming §1.1.

Also note what the table does **not** show: no dependence on iteration count.
Master at filler=800 dies in iteration 0 whether asked for 1 iteration or 120.

### 2.2 Fixed — 3 reps per point

| `SSI_GC_FILLER` | markers (hwm) | sampled peak | `st_nlockers` | pass | fail |
|---:|---:|---:|---:|---:|---:|
| 400 | 318 | 102 | 10 | 3 | 0 |
| 520 | 296 | — | 9 | 3 | 0 |
| 530 | 299 | — | 10 | 3 | 0 |
| 800 | 322 | 100 | 10 | 3 | 0 |
| 2000 | 323 | — | 10 | 3 | 0 |
| 5000 | 324 | — | 10 | 3 | 0 |
| 20000 | 324 | 59 | 10 | 3 | 0 |
| **50000** | **324** | — | **10** | **3** | **0** |

**No cliff was found.** 24/24 runs pass. `SSI_GC_FILLER=50000` is 125× the
`SSI_GC_FILLER=300` the tier used to need and 95× the baseline's last passing
value.

The second column is the point. The marker high-water mark **asymptotes at 324
and stops**, while the offered load rises 125-fold. `st_nlockers` — committed
readers retaining their locker until their last marker is reclaimed — collapses
from **1046 to 10**, a 100× reduction, and is likewise flat. The population is
now bounded by something that is not the transaction count.

Cross-check at the tier level, `test/isolation/run.sh` default sweep (§7): the
same 36/36 pass across 220→20000 at **both** `lk_partitions=default` (160) and
`lk_partitions=1`, so the margin is not specific to one partition count.

---

## 3. Root cause: (ii) GC fires but reclaims nothing

Classified against the four candidates in the brief. The distinguishing
measurement is a purpose-built probe (`t1_probe.c`, `t1_probe2.c`) that mirrors
the test's env and schedule exactly but dumps `DB_TXN_STAT` and `DB_LOCK_STAT`
at every phase boundary instead of asserting.

### 3.1 Not (iv) a leak, and not (i) trigger frequency

With **no long-lived transaction open**, 20 000 serializable read txns, no
checkpoints (`t1_probe`, `T1_HOLDER=0`):

```
ntxn   nsnapshot maxnsnap  nlocks maxnlocks nlockers
 1000     98       108       92     311       92
 5000     58       108       52     315       52
10000      8       108        2     315        2
15000     59       108       53     315       53
20000      9       108        3     315        3
```

A textbook bounded sawtooth. `maxnlocks` reaches 315 by txn 2000 and never moves
again. GC works perfectly. So there is no leak in the marker machinery itself,
and the trigger fires often enough.

### 3.2 The trigger *is* firing, constantly

In the failing configuration the live marker count is 800+ against a sweep
threshold of 100. The `txn_begin` predicate
(`nsireaders > st_objects / SI_CLEANUP_TRIGGER_DIV`) is therefore true on
essentially every single `txn_begin`, so `__lock_sicleanup` runs on essentially
every transaction — and the population still climbs monotonically to ENOMEM.
**The sweep runs and reclaims zero.** That excludes (i) definitively: no divisor
makes "run more often" fix "each run frees nothing".

### 3.3 The mechanism, isolated

`t1_probe2` runs the test's exact schedule at filler=800. Master:

```
iter phase           nsnapshot nactive nlocks maxnlocks nlockers
 0   iter-start          9        1       3      14        4
 0   pre-sched         106        1     100     277      101
 0   post-reads-ckp      6        3       5     277        6
 0   t1-committed        7        2       5     278        6
 0   window-1          807        2     805     811      806     <-- climbing
 0   window-1-ckp      807        2     805     811      806     <-- sweep freed 0
# ENOMEM-POINT iter=0 filler=250 txn_begin: Cannot allocate memory (12)
```

Two things to read off. First, `window-1` → `window-1-ckp`: a **forced
checkpoint**, which calls `__lock_sicleanup` directly, moves the population
`807 → 807`. Zero reclaimed. Second, `pre-sched` (106) → `post-reads-ckp` (6):
in the *same* run, a checkpoint at a moment when no long-lived reader is open
reclaims almost everything. Same code, same sweep, opposite result. The
difference is not the sweep — it is what the sweep is allowed to free.

The gate is in `__lock_siclean_obj`:

```c
		if (IS_MAX_LSN(LOCK_COMMITLSN(env, lp))) {
			if (LOG_COMPARE(&LOCK_READLSN(env, lp), old_lsnp) > 0)
				continue;
		} else if (LOG_COMPARE(&LOCK_COMMITLSN(env, lp), old_lsnp) > 0)
			continue;
```

`old_lsnp` is `__txn_oldest_reader`, which is the minimum `read_lsn` over
`region->active_txn`. **One** long-lived transaction pins that minimum at its own
`read_lsn` for as long as it lives. Every reader that starts afterwards has a
strictly newer `read_lsn`, so `LOG_COMPARE(READLSN, old_lsn) > 0` holds for
**every** marker and the gate keeps all of them. The test holds exactly such a
transaction open across the decisive window, by design — that is GC pressure (c)
in its own header comment.

Each retained marker also pins a `TXN_DETAIL` through `si_ref`
(`__txn_reap_si_details` will not free a detail while `si_ref != 0`), so the
retained marker count *is* a retained-`TXN_DETAIL` count. The markers live in the
lock region; the details they pin live in the txn region. The trigger watches the
former and the wall is in the latter.

**So the answer is (ii), with a component of (iii).** (ii) because the markers
being held are provably dead for conflict-detection purposes — this is the
recorded gap **G2**, the SSI-marker-GC visibility gate, and this is a concrete
instance of it. (iii) in the weaker sense that the demand really does exceed the
region: the demand is `O(committed readers)` when the advertised bound is
`O(lock objects)`.

It is **not** (iv). The distinguishing test from the #137 precedent — plot peak
against transaction count over a long run — is in §6, and after the fix the peak
is flat at 324 across a 100× range of transaction counts.

---

## 4. The fix: coalesce interchangeable markers

`src/lock/lock.c`, one new static predicate plus a survivor election in the
existing sweep. No new struct field anywhere — the observable is **derived at
read time** from state the detail already carries, as the brief requires.

### 4.1 The safety argument

The only consumer of a SIREAD marker is the rw-antidependency branch of
`__lock_get_internal` (`src/lock/lock.c:1116`). A marker is *interchangeable*
with the other markers on its object when its contribution to that branch is a
constant — independent of who acquires, and of anything that happens later.
Three conditions give exactly that:

1. **`status == TXN_COMMITTED`** ⇒ `TXN_SI_PAST_CHECK(td)` is unconditionally
   true. That removes the one path in the branch whose outcome depends on the
   reader's future ("defer the edge to the reader's own pivot check").
   `TXN_ABORTED` is deliberately **not** accepted: an aborted reader is not
   past-check and drives a different path.
2. **`visible_lsn == MAX_LSN`** ⇒ the reader never wrote, so it has no
   serialization point of its own. The branch's guard
   `LOG_COMPARE(COMMITLSN, acquirer->read_lsn) > 0` is then true for **every**
   acquirer, because `MAX_LSN` exceeds every `read_lsn` — the edge is always
   recorded, never skipped. `visible_lsn` is initialised to `MAX_LSN`
   (`txn.c:543`) and set to a real LSN only by `__txn_regop_log` on a commit that
   wrote (`txn.c:986`) or by `__txn_end` when pages were dirtied
   (`txn.c:1858`), so this is exactly "committed and read-only".
3. **`TXN_DTL_WCONF` clear.** With `WCONF` set, and given 1, the branch returns
   `DB_SNAPSHOT_UNSAFE` instead of recording an edge — a different outcome.

With all three, the branch's entire effect is *"set the **acquirer's** `WCONF`
unless it already has `RCONF` (then `DB_SNAPSHOT_UNSAFE`), and set this reader's
`RCONF`"*. That is idempotent in the acquirer, and a committed reader's own
`RCONF` is never read again. So N such markers on one object do exactly what one
does, and **keeping one loses no conflict**: every schedule the N markers would
have aborted, the survivor still aborts.

Any marker failing any of the three is never coalesced and goes through the
existing LSN gate unchanged. In particular the marker the test's schedule depends
on keeps byte-identical semantics.

### 4.2 What changed

```c
static int
__lock_si_coalescible(env, lp)
{
	sh_locker = LOCK_HOLDER(env, lp);
	if (sh_locker->td_off == INVALID_ROFF)
		return (0);
	td = LOCKER_TD(env, sh_locker);
	return (td->status == TXN_COMMITTED && IS_MAX_LSN(td->visible_lsn) &&
	    !F_ISSET(td, TXN_DTL_WCONF));
}
```

and in `__lock_siclean_obj`, a survivor election followed by one extra reclaim
condition in the existing loop:

```c
	keep = NULL;
	SH_TAILQ_FOREACH(lp, &obj->sireaders, links, __db_lock)
		if (__lock_si_coalescible(env, lp) && (keep == NULL ||
		    LOG_COMPARE(&LOCK_READLSN(env, lp),
		    &LOCK_READLSN(env, keep)) > 0))
			keep = lp;
	...
		if (lp != keep && __lock_si_coalescible(env, lp))
			goto reclaim;
```

The survivor is the newest `read_lsn` — the one the unmodified LSN gate would
have retained longest, so the sweep converges on the same marker it always would
have. The survivor still passes through the LSN gate on later sweeps, so a
genuinely obsolete population still collapses to nothing rather than leaving one
marker per object forever. The reclaim path itself is untouched: `goto reclaim`
enters the existing code, including the `si_ref`/`td_off` accounting whose
comments document the UAF hazard.

This bounds the marker population by the number of lock **objects** instead of
the number of committed readers — which is the ceiling
`SI_CLEANUP_TRIGGER_DIV` already advertises to operators through
`DB_ENV->lock_stat_print`. The documented bound was simply not true in the
long-lived-reader case; now it is.

### 4.3 Options weighed and rejected

- **Adaptive trigger / lower divisor** — excluded by measurement (§3.2): the
  sweep already runs on nearly every `txn_begin` and frees nothing.
- **Backpressure on a near-full region** — would convert `BDB4525` into a stall
  or an abort. Does not address dead markers being retained, and degrades a
  workload that is not actually over-subscribed.
- **Smaller per-marker footprint** — the binding resource is `TXN_DETAIL` in the
  txn region, not the marker; and shrinking either struct changes
  `__env_struct_sig()` and breaks environment attach.

---

## 5. The test's own defect: asserting on a sampled peak

Fixing the engine made the *anti-vacuity check* fail, and the reason is the
second half of T1.

The check compared `peak_locks` — the largest value the **harness** happened to
observe across three `lock_stat` calls per iteration — against the exact sweep
threshold. With GC working properly the sweep collapses the population between
two samples, so the sampled peak lands *below* the threshold and the test fails:

```
FAIL: peak live locks 47 never reached the txn_begin sweep threshold 100
```

That check was measuring *"was GC slow enough that I caught it in the act"*, not
*"was pressure applied"*. It penalises the mechanism for working, and it is why
the passing window was narrow and why `SSI_GC_FILLER` had to be hand-picked.
This is a genuine test defect, independent of the engine change.

Fix: assert on `st_maxnlocks`, which the **engine** bumps on every single grant
(`src/lock/lock.c:1350`) and therefore cannot miss a peak that the sweep has
already reclaimed. Same run, same build: engine hwm **320**, sampled peak **47**,
against a threshold of 100 — the sampled peak was a 6.8× underestimate. The
sampled peak is still printed for contrast; nothing asserts on it.

This is the only test change. No verdict name changed (§8).

---

## 6. Bounded, not a leak: peak vs transaction count

The #137 precedent (peak 302 @ 8k txns vs 303 @ 50k) says: plot the peak against
transaction count over a long run before calling anything a leak. Done, on the
fixed build, running the full failing schedule (`t1_probe2`, filler=400,
long-lived reader held across every window):

| iterations | filler txns | window-1 population | engine hwm | final `nsnapshot` |
|---:|---:|---:|---:|---:|
| 40 | ~32 000 | 19–24 | **324** | 11 |
| 200 | ~160 000 | 19–22 | **324** | 11 |
| 1 000 | ~800 000 | 20–24 | **324** | 11 |
| 4 000 | ~3 200 000 | 19–25 | **324** | 11 |

**Flat.** 100× the transaction count, identical high-water mark, identical
terminal state. The in-window population oscillates in a narrow band (19–25) —
the sawtooth — and never trends. Compare master's `807 → ENOMEM` inside
iteration 0 of the same schedule. Bounded sawtooth, not a leak, and the bound is
independent of transaction count.

---

## 7. SSI correctness is not weakened

### 7.1 The gate itself, both levels, both partition counts

`test/isolation/run.sh` on the fixed build (default `ISO_PARTS="default 1"`,
`ISO_LEVEL=both`):

```
RC=0
verdict lines: 42
manifest gate: OK

RESULT isolation ssi_gc_pressure@snapshot-default    pass
RESULT isolation ssi_gc_pressure@serializable-default pass
RESULT isolation ssi_gc_pressure@snapshot-1          pass
RESULT isolation ssi_gc_pressure@serializable-1      pass
```

All 42 isolation verdicts pass — every anomaly scenario
(`write_skew_trigger`, `write_skew_late`, `g2_antidep`, `read_only_anomaly`,
`lost_update`, `read_your_writes`, …) at `snapshot` and `serializable` × 160 and
1 partitions. `ssi_crash_pivot` passes at both partition counts (the build is
`--enable-diagnostic`, so it is not skipped).

### 7.2 The anti-vacuity control

The brief is explicit that the snapshot pass is the control and must show
conflicts **not** being detected. It does:

```
PASS (control): plain DB_TXN_SNAPSHOT committed the write skew in 120 of 120
iterations, as snapshot isolation permits -- the schedule has teeth
```

**120 of 120.** The schedule really is a write skew, and the serializable pass
(0 of 120) is therefore not vacuous. Both directions measured on the same build.

This also disposes of the trap: SSI arms only on `DB_TXN_SERIALIZABLE`
(`txn.c:319-322`). The driver sets `iso_level` from `ISO_LEVEL` and the two runs
give opposite results, which is only possible if the serializable run really did
arm rw-antidependency tracking.

### 7.3 Tamper control: the test still catches an unsafe GC

An anti-vacuity control proves the *schedule* has teeth. It does not prove the
*new predicate's* conditions are load-bearing. So a tamper build
(`-DT1_TEETH_UNSAFE`, branch `tmp/t1-teeth`, **not** merged) progressively
weakened `__lock_si_coalescible`:

| tamper | result |
|---|---|
| drop condition 2 (`visible_lsn == MAX_LSN`) | still PASS |
| also elect no survivor (`keep = NULL`) | still PASS |
| also drop condition 3 (`TXN_DTL_WCONF`) | **FAIL: 40 of 40 write skews** |

```
WRITE SKEW COMMITTED at iteration 0: A=0 and B=0, which no serial order of
{T1,T2} can produce
    iterations=40  both-committed=40  neither-committed=0  write-skews=40
FAIL: 40 of 40 iterations committed a write skew under DB_TXN_SERIALIZABLE --
a SIREAD marker was dropped while it was still needed, so the
rw-antidependency was MISSED
```

40/40, not 1/40 — the gate is emphatic, not marginal. And the progression is
informative in its own right: the decisive marker is protected by conditions 2
**and** 3 *and* the survivor election, three independent guards, and only
removing all of them loses the conflict. That is why the first two tampers still
passed. Reported rather than hidden, because a reviewer should know the first
tamper attempt was too weak to be a control.

### 7.4 `ssi001`–`ssi011`

TCL suite on the fixed build (`--enable-test --with-tcl=/usr/lib/tcl8.6`, per
`.github/workflows/ci.yml:274`):

```
T1RESULT ssi001 PASS      T1RESULT ssi007 PASS
T1RESULT ssi002 PASS      T1RESULT ssi008 PASS
T1RESULT ssi003 PASS      T1RESULT ssi009 PASS
T1RESULT ssi004 PASS      T1RESULT ssi010 PASS
T1RESULT ssi005 PASS      T1RESULT ssi011 PASS
T1RESULT ssi006 PASS
T1DONE rc=0
```

11/11. `ssi` is in `$subs` in `test/tcl/testparams.tcl:17` and
`test_names(ssi)` lists exactly `ssi001`–`ssi011` (line 85), so the tier is not
silently skipping. Note `ssi009` is the multi-process locker-teardown test that
`S5` in `KNOWN-ISSUES.md` records as failing at `lk_partitions=1`; it passes here
at the default partition count, which is the configuration these runs used.

---

## 8. Manifest, ABI and environment-signature proofs

### 8.1 Manifest

```
== test-execution manifest gate ==
verdict lines: 42
manifest gate: OK
```

`test/check_manifest.sh --tier isolation`: **0 MISSING, 0 UNDECLARED**, 42
verdicts against the 42 declared isolation entries. No verdict name changed —
the engine change is invisible to naming, and the test change touched only which
counter the anti-vacuity check reads — so `test/MANIFEST` needed no edit. The
`SCENARIO@LEVEL-PARTS` scheme is intact.

### 8.2 Public ABI

Compiled against each arm's own generated `db.h`:

```
t1-base  DB=1744 DBC=552 DB_ENV=2088 DB_TXN=336
t1-wt    DB=1744 DBC=552 DB_ENV=2088 DB_TXN=336
```

Byte-identical, and equal to the required `1744/552/2088/336`.

### 8.3 Environment signature

`dist/env_sig_print.sh` on both arms, each pointed at its own tree:

```
t1-base  0xeae0caa0
t1-wt    0xeae0caa0
```

Identical, as it must be: no struct that `src/env/env_sig.c` hashes was touched.
`__lock_si_coalescible` derives its answer from `status`, `visible_lsn` and
`flags`, all of which `TXN_DETAIL` already carries — the brief's "prefer deriving
an observable at read time over storing a new field", followed literally. No
compile-time size guard fired (`struct __db_mpool`, `struct __db_txnmgr`), which
is consistent.

One methodological note, because it nearly produced a false alarm and matches a
recorded prior instance of this same trap: run naively, the script reported
`0xdaf24890` for the main checkout and `0x89d6fd80` for the branch worktree. The
difference was entirely a **stale `build_unix/db_config.h`** in the main
checkout, not a code change. The value is only meaningful when both arms are
measured with the same `BUILD_DIR` provenance — a fresh worktree at the base
commit gave `0x89d6fd80`, matching the branch exactly. **An env-signature
comparison whose two sides used different `db_config.h` files has not measured
anything.**

---

## 9. Was `SSI_GC_FILLER=300` special-casing deleted?

There was none to delete. The value lives only as the driver's compiled-in
default (`nfiller = 220`) and an env override; `test/isolation/run.sh` never sets
`SSI_GC_FILLER`, so the tier already ran at the default. What made the tier green
on other boxes was operators setting it by hand — recorded in
`test/bench/BATCHED-READS-RESULTS.md:456` as a follow-up ("needs `SSI_GC_FILLER`
raised for this box's region sizing, or the threshold derived from `st_objects`
at runtime").

**That follow-up is now closed by §5**: the threshold is derived from
`st_objects` at runtime and compared against an engine-maintained high-water
mark, so the check no longer depends on box-specific region sizing. The default
`nfiller = 220` is left alone — with a 95× margin above it and the anti-vacuity
check now measuring the right thing, there is no value in changing it, and
changing it would invalidate the comparison to every previously recorded run.

---

## 10. Operator-visible signal

Both signals that would have caught this already exist, from the release's
7 operator health signals. Verified against the failing run rather than assumed:

- `DB_ENV->lock_stat_print`, "SSI committed-reader SIREAD markers live", reported
  against `st_objects / SI_CLEANUP_TRIGGER_DIV` (`src/lock/lock_stat.c:459`). In
  the failing configuration this reads 807 against a ceiling of 100 — **807%**.
- `DB_ENV->txn_stat_print`, "Snapshot txn details retained (MVCC/SSI)", i.e.
  `st_nsnapshot` against `st_maxtxns` (`src/txn/txn_stat.c:286`) — the direct
  count of the resource that actually runs out.

Programmatically: `DB_LOCK_STAT.st_nlockers` (1046 → 10 here) and
`DB_TXN_STAT.st_nsnapshot`. No new signal was added, because the existing pair
already reads far over 100% before ENOMEM; the gap was never observability.

---

## 11. Reproducing

```sh
# arms in separate worktrees, separate build dirs
git worktree add --detach /tmp/t1-base 29f616142
git worktree add /tmp/t1-wt -b fix/ssi-gc-margin origin/fix/ssi-gc-margin
for w in /tmp/t1-base /tmp/t1-wt; do
  mkdir -p $w/build_unix && (cd $w/build_unix &&
    ../dist/configure --enable-debug --enable-diagnostic && make -j16)
done

# the cliff (one iteration is enough: master dies inside iteration 0)
cc -g -O1 -w -I$B -I$W/test/isolation $W/test/isolation/test_ssi_gc_pressure.c \
    $B/libdb.a $LIBS -ldl -lpthread -o gcp
for f in 400 480 500 510 515 520 525 530 540 560 600 800; do
  for r in 1 2 3 4 5; do
    ISO_LEVEL=serializable SSI_GC_FILLER=$f SSI_GC_ITER=1 ./gcp; done; done

# the tier, both levels x both partition counts, plus the manifest gate
(cd $W/test/isolation && LIBDB_BUILD=$W/build_unix ./run.sh)
$W/test/check_manifest.sh --tier isolation

# ssi001-011
(cd $W/build_tcl && ../dist/configure --enable-debug --enable-diagnostic \
    --enable-test --with-tcl=/usr/lib/tcl8.6 && make -j16)

# proofs
for w in /tmp/t1-base /tmp/t1-wt; do sh $w/dist/env_sig_print.sh $w; done
```

Raw data: `CLIFF.csv` (baseline, 60 runs), `CLIFF_FIX.csv` (fixed, 24 runs),
`iso-fix2.log` (tier), `tcl-fix.log` (ssi001–011), `longrun.log` (§6),
`t1_probe.c` / `t1_probe2.c` (the diagnostic probes).

---

## 12. Summary

| | before | after |
|---|---|---|
| cliff (`SSI_GC_FILLER`) | **525** → 530 fails | **none found at 50000** |
| headroom over tier default (220) | 2.4× | **≥227×** |
| headroom over baseline cliff | — | **≥95×** |
| marker hwm at filler=20000 | ENOMEM | **324, flat** |
| `st_nlockers` at filler≥800 | 1046 | **10** |
| peak vs txn count (100× range) | ENOMEM in iter 0 | **flat at 324** |
| isolation tier | — | **42/42, gate OK** |
| `ssi001`–`ssi011` | — | **11/11** |
| anti-vacuity control | — | **120/120 skews under SI** |
| tamper control | — | **40/40 skews → FAIL** |
| ABI / env signature | — | **unchanged** |

Root cause **(ii)**: GC fired constantly and reclaimed nothing, because one
long-lived transaction pins `__txn_oldest_reader` and the per-marker LSN gate
then retains every committed reader's marker — an instance of recorded gap
**G2**. Fixed by coalescing provably interchangeable markers, which makes the
already-documented `O(lock objects)` bound true instead of aspirational. T1's
second half — the tuning sensitivity — was a defect in the test's anti-vacuity
check, which asserted on a sampled peak that *shrinks as GC improves*; it now
asserts on the engine's own high-water mark.

`T1` can be closed. Recommend leaving **G2** open with this instance recorded:
this fix removes the specific unbounded case, but the general visibility gate —
"is this marker still needed by any live transaction?" — is still answered by an
`old_lsn` comparison that one long-lived reader can pin indefinitely for markers
outside the coalescible class.
