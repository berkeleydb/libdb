# Read-descent lock-manager cost, and whether SI readers need lock objects

Branch: `perf/read-descent-locks`. Box: `c7i.24xlarge`, 96 vCPU, dedicated.
Merge base: `0578113b9`.

**Verdict: no safe change found, because there is nothing left to remove.**
The premise the investigation started from is false, and the one lock that a
read descent does take is load-bearing for serializability. Both halves of that
are measured, not argued.

---

## 1. The premise was wrong: it is one lock per read, not one per level

The task states that a 3-level point read costs `3 x __lock_get` +
`3 x __lock_put`. It does not. Measured with `test/bench/rdl_count.c`
(`st_nrequests` delta / N reads, `--enable-diagnostic` build), sweeping page
size to move tree depth:

| tree levels | 2 | 3 | 4 |
|---|---|---|---|
| **locks / read** | **1.000** | **1.000** | **1.000** |
| pages / read | 1.00 | 2.00 | 3.00 |

Locks per read are **depth-independent**. Pages per read grow with depth, which
is the pin cost RFC 0007 is attacking separately — that half of the premise is
right; the lock half is not.

### Why: interior pages are latch-coupled, never locked, on a read descent

The descent loop acquires a lock only at the gate `src/btree/bt_search.c:942`:

```c
if ((getlock || level - 1 == LEAFLEVEL) &&
    (ret = __db_lget(dbc, LCK_COUPLE_ALWAYS, pg, lock_mode, wait, &lock)) != 0)
```

`getlock` is computed once at `bt_search.c:563-565`:

```c
getlock = F_ISSET(cp, C_RECNUM) ||
   (lock_mode == DB_LOCK_WRITE && (stack || LF_ISSET(SR_NEXT | SR_DEL)));
```

For a plain btree read descent `C_RECNUM` is clear and `lock_mode` is
`DB_LOCK_READ` (set at `bt_search.c:249`), so `getlock == 0` and the gate
reduces to `level - 1 == LEAFLEVEL`: **the leaf, and only the leaf.**

The root is also not locked on a read descent. `BAM_GET_ROOT`
(`src/dbinc/btree.h:439-444`) calls `__db_lget` only when

```c
STD_LOCKING(dbc) && ((lock_mode) == DB_LOCK_WRITE || F_ISSET(dbc, DBC_DOWNREV)
    || dbc->dbtype == DB_RECNO || F_ISSET(__cp, C_RECNUM))
```

none of which hold for a btree `DB_LOCK_READ` descent.

So the optimization the task proposes as the smallest candidate change — "skip
lock acquisition for internal (non-leaf) pages on a read-only descent" —
**already exists, and has since Berkeley DB.** There is no per-level lock to
remove.

### Mechanical confirmation, per call, with the page identified

`test/bench/rdl_where.c` + `test/bench/rdl_run_where.sh` break on `__db_lget`
and `__lock_get_internal` across **exactly one** point read of a 4-level tree
(295 interior pages, 5000 leaves), and print the `__bam_search` frame at each
hit. Counting calls, not just stat requests, distinguishes "never called for
interior pages" from "called and short-circuited":

| mode | `__db_lget` calls | `__lock_get_internal` calls | frame at the call |
|---|---|---|---|
| plain txn | 1 | 1 (`mode=1` = `DB_LOCK_READ`) | `level=2 pg=3270` |
| `DB_TXN_SNAPSHOT` (SI) | 1 | **0** | `level=2 pg=3270` |
| `DB_TXN_SERIALIZABLE` (SSI) | 1 | 1 (`mode=9` = `DB_LOCK_SIREAD`) | `level=2 pg=3270` |

`level=2` is the proof that the locked page is the leaf: the gate is
`level - 1 == LEAFLEVEL` and `LEAFLEVEL` is 1, so a lock taken at `level=2` is a
lock on a level-1 page. Interior pages at levels 3 and 4 produce no call at all.
Lock modes are `db.in:319` (`DB_LOCK_READ=1`) and `db.in:327`
(`DB_LOCK_SIREAD=9`).

---

## 2. The design question, answered per isolation level

> When SI/MVCC is active, do read-only descents need lock-manager lock objects
> per level at all?

**Per level: no, and they never did — see §1.** The real question is what the
one leaf lock does, and the answer differs by isolation level. All of it is
decided in `__db_lget` (`src/db/db_meta.c:1148-1194`) before any lock is
requested.

### `DB_TXN_READ_UNCOMMITTED` — degraded to a non-conflicting mode, still taken

`db_meta.c:1244-1245`:

```c
if (F_ISSET(dbc, DBC_READ_UNCOMMITTED) && mode == DB_LOCK_READ)
	mode = DB_LOCK_READ_UNCOMMITTED;
```

The lock object is still allocated (measured: `locks/read = 1.000`), but in a
mode that conflicts with nothing a reader cares about, and `db_meta.c:1266`
couples it so it is released at the next acquisition rather than held to commit.
Its remaining function is **(3) deadlock-detection and lock-ordering
bookkeeping** — the locker's held-by list is what the detector walks — plus
serialization against a concurrent structural modification, since a page
write-locker still conflicts with it. It is *not* doing isolation work.

### Plain (no flags) and `DB_READ_COMMITTED` — real two-phase read lock

A `DB_LOCK_READ` on the leaf, held to commit (plain) or coupled
(`DB_READ_COMMITTED`, via the `action = LCK_COUPLE` branch at
`db_meta.c:1263-1265`). This is answer **(1)**: it serializes the reader against
structural modification and against writers of the same page. Removing it
without another mechanism would let a reader observe a page mid-split. This is
exactly the guarantee RFC 0007's optimistic validation is designed to replace,
so if that work lands, *this* lock — not a per-level one — is the one it makes
removable, and only for non-serializable readers (see below).

### `DB_TXN_SNAPSHOT` (plain SI) — **already zero lock objects**

`db_meta.c:1184-1189`:

```c
if (MULTIVERSION(dbp) && mode == DB_LOCK_READ &&
    txn != NULL && F_ISSET(txn, TXN_SNAPSHOT)) {
	if (!F_ISSET(txn, TXN_SNAPSHOT_SAFE)) {
		LOCK_INIT(*lockp);
		return (0);
	}
```

A plain SI reader takes **no lock object at any level, including the leaf.**
Measured: `locks/read = 0.000`, and the gdb probe shows `__db_lget` called once
but `__lock_get_internal` **zero** times. Isolation comes entirely from the MVCC
version chain in mpool; structural safety comes from the buffer pin/latch, not
from the lock manager.

So candidate answer **(4)** — "vestigial for SI reads specifically, can be
skipped" — is **true and already harvested.** There is no remaining SI read-lock
cost to attack. Note this requires `DB_MULTIVERSION` on the handle:
`MULTIVERSION(dbp)` is `atomic_read(&dbp->mpf->mfp->multiversion)`
(`src/dbinc/mp.h:695`), so a `DB_TXN_SNAPSHOT` transaction reading a
*non*-multiversion database falls through and takes the ordinary read lock
(measured: `nonMV-snapshot`, `locks/read = 1.000`).

### `DB_TXN_SERIALIZABLE` (SSI) — the one lock, and it is the read set

`db_meta.c:1190-1191` turns the same acquisition into a marker:

```c
	lkflags |= DB_LOCK_SNAPSHOT_SAFE;
	mode = DB_LOCK_SIREAD;
```

Granting it links the marker onto the object's `sireaders` list
(`src/lock/lock.c:1506`), and a later `DB_LOCK_WRITE` acquisition on that object
walks that list to form the `R --rw--> W` antidependency edge
(`src/lock/lock.c:1188-1194`, loop at `1216`). This is answer **(2)**, and it is
the load-bearing one: **no lock object means no marker means no edge means no
conflict detection.** SSI arms only on `DB_TXN_SERIALIZABLE`
(`src/txn/txn.c:319-322`), so a probe written against plain `DB_TXN_SNAPSHOT`
passes vacuously — see §4, where that trap is handled explicitly.

This is invariant **D10** (`docs/design/global-invariants.md`): phantom
prevention is *emergent* from page granularity. The marker is per **page**, so a
scan unavoidably marks every leaf it touches, and an insert must write-lock the
leaf it lands on. Refining or removing the read set silently loses phantom
prevention — the TidesDB bug (`docs/design/tidesdb-comparison.md`).

MVCC adds a second, independent edge-recording path for the same reader:
`__memp_si_rwconflict` (`src/mp/mp_fget.c:113-170`, called from `mp_fget.c:369`)
records `R --rw--> W` for newer versions the reader skipped on the chain. It is
*not* a substitute for the marker: it fires when the reader **skips a version**,
i.e. when the writer already wrote. The marker covers the opposite and harder
direction — the writer arriving **after** the read, which is exactly the phantom
case, and which is why removing the marker breaks phantom prevention while
leaving `__memp_si_rwconflict` intact. §4's tamper test demonstrates this
concretely: with markers suppressed and `__memp_si_rwconflict` untouched, the
phantom commits.

### Summary table

| Isolation level | Lock objects per read | Level(s) locked | What the lock is doing |
|---|---|---|---|
| `DB_TXN_READ_UNCOMMITTED` | 1 (`DB_LOCK_READ_UNCOMMITTED`) | leaf | (3) deadlock/ordering bookkeeping; no isolation |
| plain, `DB_READ_COMMITTED` | 1 (`DB_LOCK_READ`) | leaf | (1) serialize vs structural modification + writers |
| `DB_TXN_SNAPSHOT` + `DB_MULTIVERSION` | **0** | none | (4) nothing — already skipped |
| `DB_TXN_SNAPSHOT`, non-MV handle | 1 (`DB_LOCK_READ`) | leaf | (1), as plain |
| `DB_TXN_SERIALIZABLE` + `DB_MULTIVERSION` | 1 (`DB_LOCK_SIREAD`) | leaf | **(2) the SSI read set / D10 phantom prevention** |

Answers (1), (2), (3) and (4) are all true — of different isolation levels. None
of them is true *per level*.

---

## 3. Two-regime measurement

Both regimes were measured; each result is labelled. **Read §3.3 before
quoting any throughput number here** — the reps are incomplete.

### 3.1 The harness confounder that had to be removed first

`test/bench/scale_bench.c` shares **one** `DB` handle across all threads, so
every `DB->get` allocates a transient cursor on that handle under `dbp->mutex`.
At t=96 that single mutex dominates both regimes and hides the lock manager
entirely:

| regime (scale_bench, t=96) | `__db_tas_mutex_lock_int` | lock-mgr self-time | pin self-time |
|---|---|---|---|
| uniform (`rrand`) | 85.0% | 0.65% | 0.96% |
| hot key (`rhot`) | 76.7% | 1.32% | 0.15% |

Reported as a caution: **an 80%-mutex reading from `scale_bench` is not
evidence about the lock manager.** The earlier "80% of all CPU in
`__db_tas_mutex_lock_int`, split 40.6/38.3 between `__lock_get_internal` and
`__lock_put_nolock`" figure quoted in the task is a *different* measurement
(shared-environment hot-key with per-handle threads); with `scale_bench`'s
shared handle the same symbol is mostly the cursor mutex. `test/bench/rdl_bench.c`
therefore follows `scale_iso` and gives each thread its **own** handle on the
same file.

### 3.2 What the regimes show (rdl_bench, per-thread handles)

Single rep, 3 s, 20k keys, t=1 and t=8 — all 12 arms:

| regime | iso | t=1 ops/s | t=8 ops/s | locks/op | lockpart% @ t=8 |
|---|---|---|---|---|---|
| uniform | none | 963,564 | 2,997,502 | 1.000 | 0.1 |
| uniform | plain | 161,751 | 124,307 | 1.000 | 0.0 |
| uniform | rc | 161,690 | 126,550 | 1.000 | 0.0 |
| uniform | uncom | 963,207 | 3,235,875 | 1.000 | 0.1 |
| uniform | **si** | 1,035,008 | 1,823,439 | **0.000** | 0.0 |
| uniform | **ssi** | 897,463 | 2,070,709 | **1.000** | 0.1 |
| hot | none | 1,286,312 | **622,757** | 1.000 | **54.0** |
| hot | plain | 169,422 | 127,561 | 1.000 | 3.1 |
| hot | rc | 169,203 | 118,261 | 1.000 | 0.6 |
| hot | uncom | 1,282,822 | **818,579** | 1.000 | **49.5** |
| hot | **si** | 1,269,658 | 1,568,341 | **0.000** | **0.0** |
| hot | **ssi** | 1,163,306 | 1,712,474 | **1.000** | **25.4** |

The regime dependence the task predicted is confirmed, and it is a **partition**
effect, not a per-level one:

- **Uniform keys**: `lockpart%` is ~0.1 at every level. Lock objects hash to
  different partitions; the lock manager is not a bottleneck. Consistent with
  the ~2.8% figure.
- **Hot key**: `lockpart%` jumps to 49-54% for the levels that take a lock, and
  throughput *inverts with thread count* (hot `none`: 1.29M at t=1 →
  0.62M at t=8). One hot key is one lock object, one hash index, and therefore
  **one** partition mutex — `LOCK_PART(reg, ndx) = ndx % (reg)->part_t_size`
  (`src/dbinc/lock.h:222`) — regardless of `npart=960`.
- **`si` is immune** (`lockpart% = 0.0` in the hot regime, and it is the only
  arm that scales up there) precisely because it takes no lock object.
- **`ssi` re-imports the contention** (`lockpart% = 25.4` hot) because the
  `DB_LOCK_SIREAD` marker is a real lock object on that one hot leaf.

So the cost that remains on the read path is **the SSI read set on skewed keys**,
and its mechanism is single-partition serialization on the hot object, not
per-level acquisition.

### 3.3 Measurement gap — stated plainly

The 5-rep × t={1,8,32,96} sweep was **launched and ~4.2 reps complete** when the
box became unreachable (all three t3 instances lost simultaneously; local AWS
credentials expired, so no diagnosis was possible). The results file was on the
instance and had not been copied back — my error, it should have been streamed.

Consequently the table in §3.2 is **single-rep at t=1 and t=8**, and does **not**
meet the stated standard (≥5 reps, median + CV, noise floor base-vs-itself,
t={1,8,32,96}). Two t=96 tail lines survived in transcript
(`hot none t=96: 177,951 ops/s, lockpart 50.3%`;
`hot ssi t=96: 342,925 ops/s, lockpart 57.7%`), consistent with the t=8 shape.

What this gap does and does not affect:

- It does **not** affect the §1 or §2 conclusions. Those rest on lock **counts**
  and per-call gdb traces, which are exact integers reproduced at three tree
  depths, not on throughput deltas.
- It does **not** affect §4's correctness proofs, which completed.
- It **does** mean the *magnitudes* in §3.2 are provisional. The regime
  *direction* (hot-key `lockpart%` 50%+ vs uniform 0.1%, `si` immune, `ssi` not)
  is a ~500× separation and is not a noise artifact, but the exact ops/s figures
  should be re-measured.

`test/bench/rdl_run_bench.sh` re-runs the full sweep in one command
(`REPS=5 SECS=5 NKEYS=100000 THREADS='1 8 32 96'`) and
`test/bench/rdl_report.py` produces the median+CV table; it alternates arms
within each rep and reuses one environment directory, so the DB_PRIVATE
env-path-length artifact (`test/bench/run_bench.sh` header) does not apply —
these are shared-environment runs regardless.

No change was made to the engine, so there is no A/B to report and no noise
floor is required to interpret a null: the null is structural, not measured.

---

## 4. Correctness: SSI and D10, with anti-vacuity and tamper controls

No engine code changed on this branch, so nothing here can regress. These runs
exist to establish that the mechanism §2 identifies as load-bearing **is**
load-bearing — the evidence that any future attempt must clear.

### 4.1 `g2_antidep` does not test D10 — it passes vacuously

The existing `g2_antidep` scenario looks like the phantom test but is not one.
Its two markers (`marker.t1`, `marker.t2`) sort adjacently into the **same** leaf
of a small tree, so T2's insert collides with T1's uncommitted **page write
lock** and is refused with `DB_LOCK_DEADLOCK`. That is ordinary two-phase
locking, not the SSI read set. Measured on stock master, all partition counts:

```
snapshot:     T2 insert -> DB_LOCK_DEADLOCK; committed 1/2; PASS
serializable: T2 insert -> DB_LOCK_DEADLOCK; committed 1/2; PASS
```

**It passes identically at both isolation levels**, so it cannot distinguish a
working read set from an absent one. This is the exact vacuity trap the task
warned about, present in the existing suite.

### 4.2 New gate: `phantom_pages` (scan-then-insert, inserts on different leaves)

Added to `test/isolation/test_iso_anomaly.c` with `si_anomaly=1`. It splits the
tree with 200 filler keys and has T1 insert the **minimum** key and T2 the
**maximum**, so the two inserts land on **different** leaves and cannot block
each other on a page lock. The only thing that can stop both committing is the
SIREAD marker each scan left on the other's target page.

The predicate is self-checked (insert / count / delete / recount) before any
verdict depends on it. That guard earned itself immediately: the first version
matched the prefix `"mark_"` while the keys are `"a_mark_t1"` / `"z_mark_t2"`, so
it counted 0 markers in every state and produced a **false FAIL** under SSI.

Result on stock master, `ISO_LEVEL=both`:

```
=== ISO_LEVEL=snapshot (plain SI: anomalies expected) ===
  T1 scan saw 0 markers; insert a_mark_t1 (min key) -> success; commit -> success
  T2 scan saw 0 markers; insert z_mark_t2 (max key) -> success; commit -> success
  observed: db[2]   committed txns = 2/2, serial order found = NO
  XFAIL (SI anomaly under DB_TXN_SNAPSHOT)          <-- ANTI-VACUITY CONTROL

=== ISO_LEVEL=serializable (SSI: anomalies prevented) ===
  T1 scan saw 0 markers; insert a_mark_t1 (min key) -> success; commit -> success
  T2 scan saw 0 markers; insert z_mark_t2 (max key) -> DB_SNAPSHOT_UNSAFE; abort
  observed: db[1]   committed txns = 1/2, serial order found = yes (T1)
  PASS
```

The `DB_TXN_SNAPSHOT` pass shows the conflict **not** being detected (both
commit, no serial order exists) — so the serializable pass is not vacuous. This
is the required control, and it is what `g2_antidep` lacks.

### 4.3 Teeth: the gate fails when the marker is removed

`test/isolation/rdl_tamper.sh` builds a **separate** tree with `__db_lget`
patched so an SSI read skips the lock exactly as a plain SI read already does —
i.e. it implements candidate answer (4) as if it applied to SSI — and asserts
the gate then fails:

```
TAMPER_APPLIED / TAMPER_BUILD_OK
serializable, tampered:
  phantom_pages: T2 insert -> success; commit -> success
                 committed 2/2, serial order found = NO
                 FAIL: this history is NOT serializable
  g2_antidep:    T2 insert -> DB_LOCK_DEADLOCK; PASS      <-- still passes!
TEETH OK: tampered build FAILS phantom_pages under DB_TXN_SERIALIZABLE
```

Two things are proved at once: `phantom_pages` **catches** the loss of the read
set, and `g2_antidep` **does not** (it still passes on a build with phantom
prevention destroyed). It also confirms §2's claim that `__memp_si_rwconflict`
cannot cover for the marker: it is untouched by the tamper, and the phantom
commits anyway.

This is the concrete answer to "if you remove read locks you may silently
destroy phantom prevention": **you do, and here is the run that shows it.**

### 4.4 Full isolation tier, both levels, both partition counts

`test/isolation/run.sh` with `ISO_LEVEL=both`, `ISO_PARTS="default 1"`:
**9 scenarios, 0 unexpected outcomes** in all four level×partition combinations.
Controls firing as designed:

- SI arm: 5 `XFAIL` (anomalies visible under `DB_TXN_SNAPSHOT`), including
  `phantom_pages`.
- SSI arm: those same 5 `PASS` (prevented).
- `ssi_gc_pressure` SI control: *"plain DB_TXN_SNAPSHOT committed the write skew
  in 120 of 120 iterations, as snapshot isolation permits — the schedule has
  teeth."*
- `ssi_crash_pivot`: *"serializable after recovery at every kill point (6 crashes
  inside the PIVOT's commit, 12 inside the legal txn's commit, 9 runs saw the
  engine refuse the pivot, 0 recovered states required a committed pivot)."*

One pre-existing, unrelated failure was observed at `parts=default`:

```
ssi_gc_pressure: FAIL: live-lock high-water mark 528 never reached the
txn_begin sweep threshold 600 -- raise SSI_GC_FILLER; the pressure trigger
was not exercised
```

This is the gate's own **anti-vacuity** check reporting too *little* GC pressure
on a 96-core box (the threshold scales with allocated lock objects). It is a
tuning issue in the test's pressure knob, present on the merge base, not a
correctness failure, and not caused by this branch (which changes no engine
code). It passes at `parts=1`. Left alone deliberately — out of scope here, and
the same shape as a previously recorded finding about that check.

### 4.5 `ssi001`–`ssi011`

The targeted list in `test/MANIFEST` carries only `ssi001` and `ssi002`, so a
manifest-subset run is **not** a run of the SSI suite.
`test/isolation/rdl_ssi_tcl.sh` runs all eleven and asserts eleven verdict lines
plus an end marker, because `tclsh` exits 0 when a test fails to source:

```
VERDICT ssi001 pass ... VERDICT ssi011 pass
RDL_SSI_TCL_END
pass=11 fail=0 expected_total=11
RDL_SSI_TCL_OK 11/11
```

That assertion earned itself twice, both times converting a would-be silent
green into a named failure: the measurement build lacks `--enable-tcl` (empty
`tclsh_path`, 0 verdicts) and `--enable-tcl` alone is insufficient — without
`--enable-test`, `test.tcl` dies at `berkdb getconfig`. Both flags are required
and now recorded in `test/isolation/rdl_build_tcl.sh`.

---

## 5. MANIFEST, ABI and environment-signature proofs

### MANIFEST

`phantom_pages` adds 4 verdict names (`@snapshot-default`, `@snapshot-1`,
`@serializable-default`, `@serializable-1`), added to `test/MANIFEST` in the same
commit as the scenario. The isolation tier goes from 42 entries (40 required + 2
`optional`, the `ssi_crash_pivot` pair) to 46 (44 required + the same 2
optional). No existing verdict name changed, so no other tier or gate moves.

### ABI and env signature

`test/bench/rdl_abi.sh` computes both on the merge base and on this branch, the
same way, and diffs them. No struct field was added to anything
`src/env/env_sig.c` hashes, so both must be identical:

```
--- ABI base (0578113b9) ---        --- ABI branch ---
sizeof(DB)=1744                     sizeof(DB)=1744
sizeof(DBC)=552                     sizeof(DBC)=552
sizeof(DB_ENV)=2088                 sizeof(DB_ENV)=2088
sizeof(DB_TXN)=336                  sizeof(DB_TXN)=336
--- env signature base ---          --- env signature branch ---
0xb86f77f0                          0xb86f77f0

ABI IDENTICAL
ENV SIGNATURE IDENTICAL
ABI MATCHES DOCUMENTED VALUES (1744/552/2088/336)
RDL_ABI_OK
```

Both the base-vs-branch diff *and* the documented absolute values are asserted —
a diff alone would stay clean if both trees drifted together.

---

## 6. Conclusion and recommendation

**No safe change found, and no change is available.** Specifically:

1. There is no per-level read lock. A read descent takes exactly one lock, on
   the leaf, at any tree depth. The interior-page skip the task proposed already
   exists (`bt_search.c:942`, `btree.h:439`).
2. Plain SI readers already take **zero** lock objects
   (`db_meta.c:1184-1189`). Candidate answer (4) is true and already harvested.
3. The single remaining lock under `DB_TXN_SERIALIZABLE` is the
   `DB_LOCK_SIREAD` marker, which **is** the SSI read set and **is** D10's
   phantom prevention. Removing it is candidate answer (2), and §4.3 demonstrates
   the resulting non-serializable commit on a real build. It cannot be removed
   without first adding range/next-key predicate locking.
4. The total addressable per-read lock cost is therefore `1 get + 1 put`, not
   `3 + 3`. The prior `perf/lock-readpath` NULL result is fully explained: it was
   attacking a cost three times smaller than assumed and already minimal.

### Where the real remaining lock-manager cost is

Not per-level acquisition — **single-partition serialization on a hot object.**
`LOCK_PART(reg, ndx) = ndx % (reg)->part_t_size` (`src/dbinc/lock.h:222`) maps
one hot page to one partition mutex no matter how many partitions exist
(measured: `lockpart%` 49-54% at t=8, `npart=960`, throughput inverting with
thread count). Any future work on read-path lock cost should target that, and it
is a **partitioning** problem, not a descent problem. It affects the levels that
take a lock (plain, RC, uncommitted, SSI) and provably not plain SI. Because SSI
is the level that both needs the object and suffers the contention, the
tractable direction is reducing marker *contention* (e.g. partitioning keyed on
something other than the object index alone), never marker *absence*.

### Reproduce

| Artifact | Command |
|---|---|
| Lock/page counts per level | `sh test/bench/rdl_run_count.sh -k 20000 -n 2000 -p 512` |
| Which page, per call (gdb) | `sh test/bench/rdl_run_where.sh {plain,snapshot,serializable}` |
| Two-regime sweep | `REPS=5 SECS=5 NKEYS=100000 THREADS='1 8 32 96' sh test/bench/rdl_run_bench.sh \| python3 test/bench/rdl_report.py` |
| Isolation tier, both levels | `cd test/isolation && ISO_LEVEL=both ./run.sh` |
| D10 teeth (tamper) | `sh test/isolation/rdl_tamper.sh` |
| `ssi001`–`ssi011` | `sh test/isolation/rdl_build_tcl.sh && sh test/isolation/rdl_ssi_tcl.sh` |
| ABI + env signature | `sh test/bench/rdl_abi.sh` |

Build trees used: `bu/` (`--enable-debug --enable-diagnostic`) for the counting
and gdb probes and the isolation tier; `bp/` (`--enable-stat`, no DIAGNOSTIC) for
throughput; `btcl/` (`--enable-tcl --enable-test`) for the TCL suite;
`bt/` (tampered, separate tree) for the teeth check.
