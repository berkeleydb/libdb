# perf/lock-readpath — lock-manager read-path fast grant: MEASURED RESULT

Branch: `perf/lock-readpath` (off `master` 86074b6b6)
Commits: `a91909856` (STEP 1 change), `6c3612573` (diagnostic-caught correctness fix)
Measured on: EC2 c7i.24xlarge, 96 vCPU, Debian 12 (kernel 6.1), THP off.

## VERDICT — no reliable throughput win (documented negative result)

STEP 1 (eliminate the O(N) holder-list walk on the compatible-read fast path
via an O(1) per-object holder mode-count summary) is **correct and safe** —
every correctness gate passes, all preserved invariants hold, public ABI is
unchanged — **but it does not deliver a reliable throughput win** on the target
`rhot` workload, and is dead-neutral on `rrand`. Under the project's ironclad
rule (keep only a measured win; three prior perf attempts were correctly
discarded), **this must NOT be merged as a perf win.** It is documented here
exactly like `perf/bhpin-r1` (R1) and `perf/rsnap-multilevel`.

STEP 2 was scoped and found **not applicable**: the main-btree plain-read
descent already takes only the single required leaf-page read lock — it does
NOT lock internal pages (latch-coupling only). There is no redundant
internal-page read lock to remove, so STEP 2's premise does not hold; removing
the one leaf read lock would break degree-3 isolation. Not attempted further.

## The change (STEP 1)

Files / functions / fields:

- `src/dbinc/lock.h`
  - `#define DB_LOCK_NMODES 10` (max lock modes; the RIW conflict table is the
    widest, `DB_LOCK_SIREAD == 9`).
  - `DB_LOCKOBJ` gains `u_int32_t nheld[DB_LOCK_NMODES]` — per-mode count of the
    granted locks currently on this object's `holders` list. (SIREAD markers
    live on the separate `sireaders` list and are intentionally NOT counted;
    they never participate in holders conflicts.)
  - Macros `LOCK_OBJ_HELD_ADD(obj,m)` / `LOCK_OBJ_HELD_DEL(obj,m)` (the DEL has
    a diagnostic `DB_ASSERT(nheld>0)` underflow guard).
- `src/lock/lock.c`
  - `__lock_get_internal`: an O(1) read-lock fast path inserted before the
    existing O(N) holder walk. For a plain lock get (not UPGRADE / SWITCH /
    WAIT / CHECK, not SSI snapshot-safe) with **no waiters** and **no held mode
    conflicting** with the request (tested against the same `CONFLICTS` matrix,
    O(nmodes)=10), grant immediately; self-hold refcount is served from the
    locker's short `heldby` list. Anything the summary can't prove falls back to
    the unchanged O(N) walk.
  - `nheld[]` maintained under the object partition mutex at every holders
    mutation: GRANT insert, `__lock_promote` (waiter->holder), the UPGRADE
    in-place mode change, the DB_LSTAT_PENDING removals, `__lock_put_internal`
    removal, `__lock_inherit_locks` merge removal, `__lock_change` moves, and
    `__lock_downgrade` in-place mode change (this last was the one the
    diagnostic build caught — commit `6c3612573`).
  - `__lock_getobj` zeroes `nheld` on object (re)initialization.
- `src/dbinc/db.in`, `build_windows/db.h`, `build_android/db.h`:
  `DB_REGION_MAGIC` 0x120897 -> 0x120898 (region layout changed; see ABI proof).

## Correctness argument per preserved invariant

- **Writer-starvation** (do not grant a reader ahead of a conflicting waiter):
  the fast path fires ONLY when `SH_TAILQ_FIRST(&sh_obj->waiters) == NULL`.
  Any waiter present -> fall back to the full walk, which runs the existing
  waiters scan. An empty waiters list trivially satisfies the rule.
- **Self-hold / upgrade exception**: the fast path only fires when NO held mode
  conflicts with the request (so granting is safe whether or not this locker
  already holds it). When it does grant and the locker already holds a same-mode
  HELD lock, it refcounts that lock (found via the locker's `heldby` list),
  identical to the walk's self-hold refcount branch. Any UPGRADE flag, or a
  conflicting self-held mode (e.g. holds WRITE, wants READ -> WRITE conflicts
  with READ in the matrix -> summary reports conflict), forces the full walk.
- **Dirty readers (grant_dirty placement)**: `DB_LOCK_READ_UNCOMMITTED`
  placement logic lives only in the walk + wait-decision block. The fast path
  never sets grant_dirty; when it grants there is no waiting/placement to do,
  and any case needing dirty-reader queue placement (which requires a
  conflicting holder, i.e. a WRITE present -> summary conflict) falls back.
- **Deadlock-detector consistency**: the detector reads `holders` and `waiters`
  (lock_deadlock.c). Those lists are untouched — `nheld` is a redundant summary
  maintained in lockstep, never a replacement. The detector still sees the exact
  same lists.
- **SSI sireaders consistency**: SIREAD markers are on the `sireaders` list, not
  `holders`; `nheld` counts holders only. The fast path is disabled for
  `safe_si` acquires (DB_LOCK_SNAPSHOT_SAFE), so the rw-antidependency recording
  walk over `sireaders` is never bypassed. Object reclamation still requires
  holders AND waiters AND sireaders all empty (unchanged).

## sizeof — public ABI UNCHANGED (+ region bump)

Measured base (master) vs patched, same libdb-2026.0:

    PUBLIC (identical):  DB=1744  DBC=552  DB_ENV=2088  DB_LOCK=24  DB_LOCKREQ=48
    INTERNAL region:     DB_LOCKOBJ 136 -> 176 (+40 = nheld[10])
                         DB_LOCKER  176 -> 176 (unchanged)
                         DB_LOCKREGION 1000 -> 1000 (unchanged)

`DB_LOCKOBJ` is a region-internal struct, not public ABI. Its growth is a
region-format change, covered by bumping `DB_REGION_MAGIC` (0x120897->0x120898);
the region version check (env_region.c) already rejects a layout from a
different MAJOR.MINOR (2026.0). No public struct changed size, so no public ABI
break. `db_version()` on the patched build: `libdb 2026.09.2 (September 10,
2026)`, soname libdb-2026.0.

## Tests RUN (real result lines)

Builds: shared (`~/patched`, -O2 -fno-omit-frame-pointer), diagnostic
(`~/build_unix`, --enable-diagnostic, DB_ASSERT live), ASan+UBSan
(`~/build_asan`, clang), ASan-only (`~/build_asanonly`), TSan (`~/build_tsan`).
All built libdb-2026.0 clean.

1. **test/lockmatrix (ASan libdb) — the exact subsystem changed**
   - `modes` + `conflicts` + `list` sections: `0 check(s) failed`, exit 0.
   - full conflict matrix printed and every (held,wanted) cell correct
     (`10 modes wide; db_lockmode_t has 10 values ... 0 check(s) failed`).

2. **test/isolation (diagnostic libdb.a) — SSI serializability**
   - `9 scenario(s) run, 0 unexpected outcome(s)`, exit 0.
   - write_skew / write_skew_samebtree / g2_antidep / read_only_anomaly /
     lost_update all detect the anomaly (DB_SNAPSHOT_UNSAFE / DB_LOCK_DEADLOCK
     abort). Write-skew IS still detected.
   - **Deadlock detection confirmed**: g2_antidep and lost_update abort one txn
     with `DB_LOCK_DEADLOCK` (a real deadlock, one victim aborted).

3. **test/isolation under ASan** (ASan-only libdb):
   `9 scenario(s) run, 0 unexpected outcome(s)`, exit 0, memory-clean.
   (UBSan strict mode surfaces two PRE-EXISTING findings in the DB-open path —
   `log_put.c:1989` zero-length memcpy nonnull-attr and `bt_open.c:94` — both
   confirmed identical on master 86074b6b6 by building a master ASan+UBSan lib;
   neither is in src/lock nor touched by this change.)

4. **SSI abort-count parity** (ssi_abort_bench, base vs patched, 3 reps):
   - hot=8: commit/deadlock/abort_rate statistically identical (e.g. t=32
     base commit~3950 deadlock~67k abort_rate 94.5% == patched).
   - hot=512 (where SSI aborts dominate): ssi_abort t=8 base 1811-1952 /
     patched 1951-2001; t=16 base 3285-3470 / patched 3260-3465; t=32 base
     1538-1636 / patched 1566-1704 — identical within noise. SSI unchanged.

5. **Concurrent reader/writer stress + db_verify** (stress_rw.c):
   - Diagnostic (nheld asserts live), 5000 keys, 20s, 48 readers + 16 writers:
     `reads ok=1670510 writes ok=494497 value_mismatch=0`, `db_verify: CLEAN`,
     no assert fired.
   - Diagnostic HOT (4 keys, 64 readers + 8 writers — max fast-path + conflict
     churn): `reads ok=1132905 writes ok=131986 value_mismatch=0`,
     `db_verify: CLEAN`.
   - ASan, 2000 keys, 32 readers + 16 writers:
     `reads ok=1018318 writes ok=428283 value_mismatch=0`, `db_verify: CLEAN`.

6. **TSan** (the gate a lock-mgr change is for): stress 512 keys, 24 readers +
   12 writers, `db_verify: CLEAN`. **Zero data races involving `nheld`** (the
   new field). Total race reports 2200; master (86074b6b6) built with the same
   TSan flags reports 2056 at the SAME top sites (db_iface.c:728, txn.c:698/975,
   bt_cursor.c:2804, lock.c:802, db_meta.c:1279) — BDB's shared-region
   test-and-set mutexes are invisible to TSan, so every mutex-guarded shared
   access looks racy on BOTH branches. No NEW race site attributable to the
   change; nheld is written and read under the object partition mutex.

## Before/after perf profile (rhot t=64, perf --call-graph fp -F 997)

    base    : __db_tas_mutex_lock_int ~80%  (40.6% __lock_get_internal +
                                             38.3% __lock_put_nolock)
              contention signal: lockpart% ~64-74%
    patched : __db_tas_mutex_lock_int ~90%  (47% get + 42% put)
              contention signal: lockpart% ~28-56%  <-- partition-mutex
                                                        HOLD-time contention
                                                        DID drop
              but total mutex spin did not improve throughput.

Interpretation: the change DOES cut the partition-mutex hold-time contention
(lockpart% 64-74% -> 28-56%), confirming the O(N) walk was real hold time.
But throughput does not rise, because on a single hot key each read still
acquires + releases the ONE object-partition mutex twice (get in __bam_search,
put in __bamc_close), and the mutex-ACQUIRE contention (test-and-set cache-line
ping-pong across 64-96 cores) plus the unchanged put-side critical section
dominate. A shorter critical section makes threads re-acquire faster, so the
spin doesn't shrink. This is the fundamental "hot key -> one mutex" wall the
diagnosis itself named ("partitioning CANNOT help a hot key").

## A/B scaling tables (patched / base ops-per-sec)

Same `sb.c` driver (SHARED env, 8GB cache, DB_TXN_NOSYNC), fresh per-variant
env dir (differing DB_REGION_MAGIC forbids sharing region files), A/B ALTERNATE
per rep.

### Full sweep, 1M keys, 4s/point, 8 reps (median, min..max)

rhot:
    thr   base median (min..max)      patched median (min..max)   ratio
      1   973911  (943945..979118)    955100  (951897..958956)    0.98
      8   666266  (641512..724390)    777321  (651366..881279)    1.17
     16   358437  (283678..462414)    378474  (326229..464768)    1.06
     32   164686  (149410..188659)    175823  (155282..243671)    1.07
     64   172991  (139453..178805)    147748  ( 96560..175594)    0.85
     96   188592  (163993..196381)    152098  ( 78857..177630)    0.81
  per-rep patched/base at t=96: 1.02 0.41 0.80 0.70 0.86 0.45 0.91 0.99
  per-rep patched/base at t=64: 1.14 0.55 0.82 0.76 0.93 0.60 0.98 1.11

rrand (not lock-partition bound; positive scaling):
    thr   base median                 patched median              ratio
      1   554749                      549110                      0.99
      8   1700940                     1714086                     1.01
     16   2203744                     2214960                     1.01
     32   2311131                     2363068                     1.02
     64   2552514                     2585179                     1.01
     96   2786246                     2789576                     1.00

### Isolated per-threadcount, 1M keys, 5s, 12 reps (no in-process sweep warmup)

    t=64: base median 164748 (75609..175485)  patched median 173237 (132410..184329)
          ratio 1.05 (mean 1.13); paired sorted: 0.79 0.93 1.00 1.00 1.04 1.04
          1.05 1.05 1.06 1.25 2.35 2.40  (the 2.3x are base-collapse reps)
    t=96: base median 188344 (66602..192660)  patched median 171114 (84439..186103)
          ratio 0.91 (mean 0.94); paired sorted: 0.44 0.73 0.87 0.88 0.90 0.90
          0.95 0.98 0.98 1.01 1.12 2.78

### Reading the tables

- rrand: dead neutral (0.99-1.02) across the sweep — expected, rrand is not
  hot-key-partition bound.
- rhot t=1/16/32: neutral, medians straddle 1.0.
- rhot t=8: leans positive (median 1.17) but per-rep 0.92-1.32 — noisy.
- rhot t=64/96: the two measurement methods DISAGREE (full-sweep medians
  0.85/0.81 negative; isolated 12-rep 1.05/0.91 mixed), and both are dominated
  by base's severe run-to-run collapse (reps as low as 66-75K vs 175-193K).
  The per-rep ratios span 0.41..2.78. This is exactly the documented rhot noise
  band on this box ("0.54-1.25x, no direction"). No claim of a win or a clean
  regression is supportable at t=64/96.

## Conclusion

STEP 1 correctly and safely eliminates the O(N)/O(N^2) holder-walk hold time
(lockpart% measurably drops), but that hold time is NOT what caps hot-key
throughput — the single-object partition-mutex acquire contention and the
mandatory get+put lock pair are, and those are fundamental to a single hot key.
STEP 2's redundant-internal-lock premise does not hold for the read descent.
Net: **no reliable throughput win.** Kept on `perf/lock-readpath`, off master,
NOT merged as perf — a valid documented negative result, consistent with the
R1 and rsnap outcomes. The one durable takeaway: the hot-key lock-manager wall
is the mutex ACQUIRE + the two-lock-per-read structure, not the holder-walk
hold time; relieving it needs a structurally different read-lock scheme
(e.g. lock elision / a striped or lock-free reader count on the object itself),
which is a larger design change than this branch attempts.
