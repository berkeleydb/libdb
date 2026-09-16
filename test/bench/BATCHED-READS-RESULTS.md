# Batched point reads (db_get_multiple) — measured results

**Branch:** `perf/batched-reads` (pushed, `origin/perf/batched-reads`)
**Base:** `origin/master` = `a7857c847`
**Box:** EC2, 96 vCPU, 185 GB RAM, Linux 6.1, all measurements on the box only
**Verdict:** **WIN — 2.47× throughput and 4× better p99 at 96 threads, growing with thread count, with measured isolation equivalence and no ABI/region-signature change.**

---

## 1. Which sub-lever, and why — the profile decided it

The brief offered two sub-levers. **(A) batched/vectored point reads** was chosen
on measurement, and **(B) async read-path I/O was not pursued**; the reason is at
the end of this section.

### 1.1 The throughput shape (baseline, master build)

`scale_bench rrand`, 200 000-key B-tree, 512 MB cache (fully in cache), 5 s/point:

| threads | ops/s | locker-latch wait % |
|--------:|------:|--------------------:|
| 1 | 775 036 | 0.0 |
| 8 | 2 450 263 | 84.6 |
| 16 | 3 159 277 | 93.8 |
| 32 | **3 479 783** (peak) | 80.6 |
| 64 | 3 079 968 | 96.0 |
| 96 | 2 293 406 | 88.9 |

Peaks at 32 threads, then **negatively scales** — the shape this repo has
documented for years. `lockpart%`, `mpoolhash%`, `objs%` and all three region
waits are ~0; the only loud signal is the locker/mutex wait.

### 1.2 Where the cycles actually go (the decisive measurement)

`perf record -F 97 --call-graph dwarf,2048`, same workload at 96 threads
(56 746 samples). Flat profile is useless on its own — 85.3 % self time in one
symbol — so the callers are what matter:

```
85.32%  __db_tas_mutex_lock_int
        |
        |--42.95%--__db_cursor_int  <- __db_cursor <- __db_get <- __db_get_pp
        |
         --42.09%--__dbc_close                   <- __db_get <- __db_get_pp
```

Cumulative (children) for comparison — the *real work* is a rounding error:

```
47.07%  __db_cursor_int      (0.85% self)
46.49%  __dbc_close          (0.67% self)
 5.24%  __dbc_iget
 4.82%  __bamc_search
 4.79%  __bam_search
 1.92%  __memp_fget          (0.56% self)
 0.69%  __bam_cmp            (self)
```

**Reading:** the dominant per-point-read cost on this box is neither the B-tree
descent nor the buffer-header pin — it is the **cursor lifecycle**. `DB->get`
allocates a transient cursor and frees it *per operation*, and both ends take a
cursor-queue partition mutex, which goes to the kernel futex under contention.
`__bam_cmp`, the actual key comparison, is 0.69 %.

This matches the brief's own framing: four parked branches
(`perf/mpool-pin`, `perf/rsnap-ml`, `perf/bhpin-r1`, `perf/lock-readpath`)
already refuted the buffer-header pin and lock-hold-time hypotheses. The
untested hypothesis was API-entry and cursor-lifecycle overhead per call. It is
now measured, and it is 85 % of the profile.

It is also **per-CALL, not per-key** — which is exactly what amortization
attacks. That is why sub-lever (A) was chosen.

### 1.3 Why (B) async read I/O was not pursued

(B) can only help a workload that *misses* cache, and the brief says so. But
the gap this task is aimed at — cross-engine workload C, 6.8× @t8 → 58.2× @t128
— is an **in-cache** read-only workload, and the profile above shows why it is
slow: 85 % futex on a cursor mutex, with `__memp_fget` at 1.92 % cumulative and
**zero** device I/O. No amount of read-ahead moves a curve whose cost is a
userspace latch. Pursuing (B) would have produced a correct measurement of zero
against the workload that actually exhibits the gap, so the effort went where
the profile pointed. (B) remains a legitimate but *separate* lever for an
out-of-cache workload; this branch does not touch it and makes no claim about it.

---

## 2. What was implemented

A single additive entry point, `src/db/db_iface.c`:

```c
int db_get_multiple(DB *, DB_TXN *,
    DBT *keys, DBT *datas, int *rets, u_int32_t nkeys, u_int32_t flags);
```

Fetches N scattered keys per API crossing. Each key still gets:

* its own B-tree descent (`DB_SET` search per key — no key-order assumption,
  keys may be scattered, absent, or repeated);
* its own page read lock, or its own SIREAD marker under
  `DB_TXN_SERIALIZABLE`;
* its own return code in `rets[i]` (`0`, `DB_NOTFOUND`, `DB_BUFFER_SMALL`, …).

What is amortized is only per-call overhead: one `ENV_ENTER`/`ENV_LEAVE`, one
replication-block check, one `__db_check_txn`, one master-lease check, and above
all **one cursor allocate/free pair instead of N**.

**Why per-key equivalence holds.** `__bamc_search` begins with `DISCARD_CUR`,
which releases the previous key's page (`__memp_fput`) and, through `__TLPUT`,
its read lock when there is no transaction — precisely the state that closing
and reopening the cursor produced. The batch therefore does not *hold* anything
across keys that the individual path released.

Flags are deliberately restrictive: `DB_READ_COMMITTED`,
`DB_READ_UNCOMMITTED`, `DB_RMW`, `DB_GET_BOTH`, `DB_IGNORE_LEASE`. `DB_MULTIPLE`,
`DB_CONSUME*`, `DB_SET_RECNO` and the positioning flags are rejected — they
change position semantics or return bulk buffers, and supporting them here would
not be equivalent to the per-key path.

Error behavior mirrors a caller looping over `DB->get` and breaking on error:
the loop stops at the first non-`0`/non-`DB_NOTFOUND` code and returns it.

### Deliberate non-decisions

* **Not a `DB` method.** Adding a method pointer to `struct __db` would change
  `sizeof(DB)` (public ABI) *and* `__env_struct_sig()`, which hashes `struct __db`
  at `src/env/env_sig.c:112`. A changed signature makes `env_region.c:265` refuse
  to attach existing environments, and **libabigail cannot see that**. So it is a
  free function, exported the same way `db_create` is (an `EXTERN:` comment →
  `dist/s_include` → `ext_def.in` + `ext_prot.in`).
* **No new public flag.** `dist/api_flags` auto-assigns bit values, so adding one
  risks renumbering existing flags. The A/B baseline arm is a plain loop of
  `DB->get`, which is what the batch has to beat anyway.

---

## 3. A/B measurement

`test/bench/batch_bench.c` — same binary does both arms; the arm is `argv[1]`.
Both arms fetch **16 scattered random keys per iteration** from a 200 000-key
in-cache B-tree, so they perform identical logical work and differ only in how
many API crossings and cursor allocate/free pairs it costs. Throughput is
reported in **keys/s** so the arms are directly comparable.

* **5 reps** per point, **arms alternate order per rep** (odd reps `indiv` then
  `batch`, even reps reversed) so neither arm is systematically favored by box
  warmth.
* 6 s measured window, thread sweep 1/8/16/32/64/96, fresh environment per arm
  per rep.
* 60 data points total.
* `ldd`-verified to link the tree under test (`-rpath` to the branch build).

`arm=indiv` = N × `DB->get`. `arm=batch` = one `db_get_multiple`.

| thr | indiv median | min | max | batch median | min | max | ratio | p99 indiv (µs) | p99 batch (µs) | p99 gain |
|----:|-------------:|----:|----:|-------------:|----:|----:|------:|----------:|----------:|---------:|
| 1 | 776 849 | 768 738 | 783 239 | 913 332 | 900 799 | 917 932 | **1.18×** | 32.8 | 16.4 | 2.00× |
| 8 | 2 489 792 | 2 391 948 | 2 845 972 | 3 304 732 | 3 180 779 | 3 521 523 | **1.33×** | 65.5 | 32.8 | 2.00× |
| 16 | 3 183 123 | 2 597 655 | 3 508 365 | 4 081 112 | 3 625 335 | 4 118 518 | **1.28×** | 65.5 | 65.5 | 1.00× |
| 32 | 3 633 022 | 3 216 932 | 3 674 747 | 4 280 547 | 4 012 950 | 4 355 822 | **1.18×** | 131.1 | 131.1 | 1.00× |
| 64 | 3 028 557 | 2 948 391 | 3 062 187 | 4 646 601 | 4 610 739 | 4 663 311 | **1.53×** | 524.3 | 262.1 | 2.00× |
| 96 | 2 208 874 | 2 080 584 | 2 309 851 | 5 449 062 | 5 374 443 | 5 559 732 | **2.47×** | 1048.6 | 262.1 | **4.00×** |

**The important result is not the ratio, it is the shape.** The individual path
reproduces the documented curve: peak at 32 threads, then decline to 2.21 M at
96. The batched path **keeps climbing all the way to 96 threads** (5.45 M) — it
does not peak inside the sweep. The 2.47× at 96 threads is therefore a lower
bound on the box's remaining headroom, not a plateau.

Spread is tight where it matters: at 64 and 96 threads the batch arm's min/max
band is ±1 % (4.61–4.66 M, 5.37–5.56 M), i.e. the win is far outside noise. The
individual arm is noisier at 16 threads (2.60–3.51 M), which is itself the
contention showing up as variance — the same effect the cross-engine report
noted (libdb CV 24–62 % vs WiredTiger 1–6 %).

p99 per-batch latency improves at every thread count where it changes at all,
and by **4×** at 96 threads (1048.6 µs → 262.1 µs). Latency histogram buckets
are powers of two, so the p99 column is quantized — the ratios are honest but
the absolute values are bucket edges.

### The batch-size caveat, stated plainly

The gain is per *call*, so it scales with batch size, and the numbers above are
for batch = 16. A caller who can only batch 2 keys gets roughly half the
amortization; a caller who cannot batch at all gets nothing. This is an
**opt-in API win for callers with N keys in hand**, not a transparent speedup of
existing `DB->get` traffic. Nothing in this branch changes `DB->get`.

---

## 4. Before/after profile — which cost was removed

Same workload, same 96 threads, same `perf -F 97 --call-graph dwarf,2048`.

| symbol | before (indiv) | after (batch) |
|---|---:|---:|
| `__db_tas_mutex_lock_int` (self) | **85.32 %** | **0.70 %** |
| `__db_cursor_int` (cumulative) | 47.07 % | — (not in profile above threshold) |
| `__dbc_close` (cumulative) | 46.49 % | — |
| `__memp_fget` (self / cum) | 0.56 % / 1.92 % | **28.35 % / 49.86 %** |
| `__os_atomic_read` (self) | 1.12 % | **35.18 %** |
| `__bam_cmp` (self) | 0.69 % | **6.01 %** |
| `__bam_search` (cumulative) | 4.79 % | **91.58 %** |

The cursor-lifecycle mutex is **gone** — 85.32 % → 0.70 %, a 122× reduction —
and it is not replaced by another lock: `__db_tas_mutex_readlock_int` is 2.41 %
and `__db_tas_mutex_unlock` 2.49 %. What fills the profile instead is *actual
work*: the buffer-pool fetch, the pin refcount atomics, the key comparison, the
descent. That is the correct after-picture for this change, and it also says
where the next lever is: with the cursor mutex removed, `__memp_fget` plus
`__os_atomic_read`/`__os_atomic_dec` (the page pin) is now ~63 % of self time —
the buffer-header pin the four parked branches attacked, which was previously
*hidden behind* the cursor mutex.

---

## 5. Correctness: differential equivalence, with teeth

`test/c/batch_diff.c`, wired into both `LEAK_TESTS` (`dist/Makefile.in`) and
`test/c/leak-run.sh` — deliberately both, because this repo's recurring
vacuous-green failure is a driver listed in one and missing from the other
(`mvcc_purge_visible`, `aio_concurrent_sync`).

Anti-vacuity is enforced at two levels: the driver refuses to print `PASS`
unless it produced all four `VERDICT` lines, and the `leak-run.sh` wrapper does
not accept `rc=0` — it greps for the `PASS` line and counts `VERDICT` lines.

All results below are from the branch build on the box, three arms
(`BATCH_DIFF_ARM=indiv|batch|both`).

### Phase 1 — values, not-founds, error codes

128 keys per run: present, absent, first, last, and deliberate repeats inside
one batch. Compared per key on return code, size, and value bytes.

```
VERDICT phase1-values: 128 keys compared, 0 mismatches
VERDICT phase1-buffer-small: indiv=-30999 batch=-30999 size indiv=64 batch=64
```

`-30999` is `DB_BUFFER_SMALL`: an undersized `DB_DBT_USERMEM` buffer yields the
same code *and* the same required size (64) in both arms.

### Phase 2 — read set (fresh-process probe)

**This is the check that two wrong versions got wrong, and the failure mode is
worth recording** because it produced a false accusation against the code.

Lock objects are shared and reused. Whichever arm touches a key range *first*
creates its objects; any later arm measures ~0 on that range — indistinguishable
from a skipped SIREAD read set. Giving each arm its own range does not fix it
either: different ranges sit at different depths and share leaves differently.
Both in-process versions reported "batch delta 10 < indiv delta 32 — isolation
weakened". **Swapping the arms (`BATCH_DIFF_ARM=indiv` vs `=batch`) showed the
32/10 split followed the RANGE, not the arm** — so the report was a measurement
artifact, and the in-process phase is now labelled `CONFOUNDED` and asserts only
non-vacuity.

The real comparison runs the probe once per (arm, range) in a **fresh process
against a fresh environment**, so every run is a first-toucher, and compares
arms *within* a range where geometry is identical by construction:

```
READSET arm=indiv base=0   stride=58 delta=32
READSET arm=batch base=0   stride=58 delta=32
READSET arm=indiv base=997 stride=58 delta=32
READSET arm=batch base=997 stride=58 delta=32
```

**Identical on both ranges (32 = 32).** The batch records the same read set as N
individual gets. A batch that skipped a marker would show a strictly smaller
delta; the individual arm's delta is > 0, so the probe is not vacuous.

### Phase 3 — isolation equivalence (the crossed write skew)

The schedule is the one `ssi_abort_bench` proved actually reaches the SSI pivot
rather than being resolved as a lock conflict:

```
T1 reads A     (via the arm under test)
T2 reads B     (always DB->get — only T1's read is the variable)
T1 writes B
T2 writes A
```

Both transactions read what the other is about to write. Under
`DB_TXN_SERIALIZABLE` the skew must be prevented; under plain `DB_TXN_SNAPSHOT`
it must be *allowed* — that is the anti-vacuity control.

```
arm=indiv: SERIALIZABLE skew prevented indiv=10/10-armed batch=0/0-armed
           (lock-resolved indiv=0 batch=0) ; snapshot control skew allowed indiv=10
arm=batch: SERIALIZABLE skew prevented indiv=0/0-armed batch=10/10-armed
           (lock-resolved indiv=0 batch=0) ; snapshot control skew allowed batch=10
arm=both:  SERIALIZABLE skew prevented indiv=10/10-armed batch=10/10-armed
           (lock-resolved indiv=0 batch=0) ; snapshot control skew allowed indiv=10 batch=10
PASS: 0 failure(s), 4 verdict(s)     [all three arms]
```

**Equivalent: 10/10 vs 10/10 prevented, control 10/10 allowed in both arms.** The
control is what makes this a real result — the same schedule at plain snapshot
commits the skew every time, so the SERIALIZABLE refusals are attributable to
SSI and not to an accident of the schedule.

Three separate harness bugs had to be fixed to get a trustworthy answer here,
each of which had produced a *wrong* verdict first, and each of which was
identified by a control rather than by inspection:

1. **No deadlock detector.** The crossed writes are a genuine lock cycle; with
   no detector the pair blocked forever. The first run hung in **both arms** —
   which is how it was known not to be a batch bug. Fixed with
   `set_lk_detect(DB_LOCK_MINWRITE)` plus 10 s lock / 20 s txn timeouts, so any
   residual cycle *reports* instead of hanging. (A hung gate produces no verdict,
   which is worse than a red one — and this is the same trap that cost this repo
   a 17-hour run.)
2. **Under-counting SSI refusals.** Only `DB_SNAPSHOT_CONFLICT` was counted;
   `ssi_abort_bench:144` also counts `DB_SNAPSHOT_UNSAFE`. That undercount is
   what produced "indiv 3/10, batch 8/10" — a false asymmetry.
3. **Keys past the end of the database.** Pair bases were fixed multiples
   (0/1000/2000/3000) of the stride, putting the control arm at key ~249 000 in a
   2 000-key DB. Both transactions then *inserted* brand-new adjacent keys into
   the same empty region — a real `ww` page conflict, not a write skew — and
   deadlocked. Caught under ASan by attaching gdb: `__bam_split` →
   `__db_lget(DB_LOCK_WRITE)` at `iso_flag=4`, key 83025. Bases are now
   quarter-of-keyspace offsets sized from `nkeys`.

Iterations resolved by the deadlock detector are now excluded from *both*
numerator and denominator (reported separately as `lock-resolved`), because they
never exercised SSI at all — excluding them is what makes the phase a statement
about isolation rather than about lock scheduling. In the runs above,
`lock-resolved = 0`, so nothing was excused.

---

## 6. ABI and region-signature proofs

Both builds from the same configure line on the same box; signature printed by
calling `__env_struct_sig()` from each build's own headers and library.

```
--- base (master a7857c847) ---
env_struct_sig=0xb86f77f0
sizeof_DB=1744 sizeof_DBC=552 sizeof_DB_ENV=2088 sizeof_DB_TXN=336

--- br (perf/batched-reads) ---
env_struct_sig=0xb86f77f0
sizeof_DB=1744 sizeof_DBC=552 sizeof_DB_ENV=2088 sizeof_DB_TXN=336
```

**`__env_struct_sig()` identical (`0xb86f77f0`), all four public sizes
unchanged.** This is the gate `libabigail` cannot see and which
`env_region.c:265` enforces by refusing to attach an existing environment. It
holds trivially here because **no struct was touched at all** — the change is one
new free function plus per-call stack state, exactly as the brief prefers. Note
`0xb86f77f0` is also the pre-`mtx_aio` signature recorded in this repo's history,
which is a useful cross-check that the number is the real one.

The generated headers were produced by `cd dist && sh s_include` (not
hand-edited), which the `cocci.yml` workflow checks; the diff is two lines:

```
+#define db_get_multiple db_get_multiple@DB_VERSION_UNIQUE_NAME@   (ext_def.in)
+int db_get_multiple __P((DB *, DB_TXN *, DBT *, DBT *, int *, u_int32_t, u_int32_t));   (ext_prot.in)
```

The prototype's visibility was verified, not assumed: the first EC2 build
compiled `batch_diff` against a stale `db.h` and produced
`implicit declaration of function 'db_get_multiple'`. That would have linked and
run correctly on x86-64 by luck while proving nothing about the header, so all
final runs assert `NO_IMPLICIT_DECL`.

---

## 7. Gates

| gate | result |
|---|---|
| `test/db` (9 drivers) | **9/9 PASS** |
| `test/isolation`, `ISO_LEVEL=both` | **9/9 scenarios, 0 unexpected outcomes** |
| `batch_diff` differential, arms `indiv`/`batch`/`both` | **PASS, 0 failures, 4 verdicts each** |
| read-set probe (fresh process, 2 arms × 2 ranges) | **PASS, deltas identical 32/32** |
| ASan + UBSan (`-fsanitize=address,undefined`, `detect_leaks=1`) | **clean** — no ASan error, no leak, all three arms PASS |
| `db_verify` | **clean** on every environment produced (`VERIFY_OK`) |
| `__env_struct_sig()` equality | **PASS** (0xb86f77f0 = 0xb86f77f0) |
| public ABI sizes | **PASS** (1744/552/2088/336 unchanged) |

### Two pre-existing items, each confirmed by a control

Neither is caused by this branch, and each was checked rather than assumed:

1. **UBSan `src/log/log_put.c:1989`** — "null pointer passed as argument 2,
   which is declared to never be null", reached from `__fop_create` →
   `__db_open`. **Control:** the *identical* report, same stack, is produced by
   the existing driver `test/c/health_stats.c` (line 229) and by
   `leak_si_locker`, neither of which calls `db_get_multiple`. It fires during
   plain `DB->open` file creation. Pre-existing; out of scope here, but real and
   worth a separate fix (a zero-length `memcpy` from a NULL source).

2. **`test_ssi_gc_pressure` "peak live locks 554 never reached the sweep
   threshold 600"** — a *tuning* failure telling the operator to raise
   `SSI_GC_FILLER`, not a correctness failure. **Control:** byte-identical on the
   **master** build — same `peak-live-locks=554`, same `st_nlockers=446`, same
   message. Pre-existing on this box's region sizing.

`test_ssi_crash_pivot` reports `SKIP` because the library is not built
`--enable-diagnostic` (the in-commit crash points are compiled out) — that is the
test's own designed behavior, not a silent pass.

**ssi001–011 (TCL) were not run.** The TCL harness needs `libdb_tcl`/`tclsh`
wiring that is not configured in these build dirs (`run_upgrade.sh` likewise
reports "libdb_tcl or tclsh not found, skipping current/old DB pass" and still
passes its other checks). Rather than claim a green I did not observe, this is
reported as **not run**. The isolation tier at both `ISO_LEVEL`s plus the
purpose-built differential test cover the isolation-equivalence question this
change actually raises; a reviewer wanting ssi001–011 should run them in a
TCL-configured build.

---

## 8. Verdict

**WIN.**

* The profile identified a cost no prior attempt had tested — cursor lifecycle,
  85.3 % of cycles at 96 threads — and the change removed it: **85.32 % → 0.70 %**,
  with the after-profile dominated by real work.
* Throughput **2.47×** and p99 **4×** better at 96 threads, 5 reps, alternating
  arms, ±1 % spread in the high-thread cells. The batched path **still scales at
  96 threads** where the individual path has been declining since 32.
* Isolation equivalence is **measured, not argued**: identical SIREAD read-set
  deltas from a fresh-process probe, and 10/10 vs 10/10 write skews prevented
  with a control proving the schedule was armed.
* No struct touched, so `__env_struct_sig()` and all four public sizes are
  provably unchanged; the new API is additive and exported the same way
  `db_create` is.

**Honest scope of the claim.** This is an opt-in win for callers that have N keys
in hand, proportional to batch size; it does not speed up existing single-key
`DB->get` traffic, and it does not close the cross-engine gap by itself — the
cross-engine C workload is one key per call. What it does establish is that a
large majority of libdb's per-point-read cost at high thread count is per-call
overhead rather than per-key work, which reframes the read-path problem: the next
lever is making the cursor lifecycle cheap for *unbatched* `DB->get` too (a
per-thread transient-cursor cache, on the evidence of this profile), and after
that the buffer-header pin, which this change has now exposed as the ~63 %
successor bottleneck.

### Follow-ups this work identified (not done here)

1. `DB->get` itself still pays the full cursor allocate/free; the same profile
   argues for a per-thread cached transient cursor, which would benefit every
   existing caller with no API change.
2. UBSan null-argument report at `log_put.c:1989` — pre-existing, reproduced from
   two unrelated existing drivers, deserves its own small fix.
3. `test_ssi_gc_pressure` needs `SSI_GC_FILLER` raised for this box's region
   sizing, or the threshold derived from `st_objects` at runtime; it currently
   fails identically on master.

---

## Appendix — reproducing on the box

```sh
# build (branch and master, separate dirs)
$W/dist/configure --enable-o_direct LIBS=-luring     # pkg-config present but pass -luring
make -j96

# A/B (5 reps, arms alternate per rep, 6 thread counts)
cc -O2 -g -pthread -I$B test/bench/batch_bench.c \
   -L$B/.libs -Wl,-rpath,$B/.libs -ldb-2026.0 -o batch_bench
BATCH_HOME=<empty dir> ./batch_bench indiv 200000 16 6 1 8 16 32 64 96
BATCH_HOME=<empty dir> ./batch_bench batch 200000 16 6 1 8 16 32 64 96

# differential correctness (all three arms) + read-set probe
BATCH_DIFF_HOME=<dir> BATCH_DIFF_ARM=both ./batch_diff 4000
BATCH_DIFF_HOME=<dir> ./batch_diff readset indiv 0
BATCH_DIFF_HOME=<dir> ./batch_diff readset batch 0

# signature / ABI proof, from EACH build's own headers
printf '#include <stdio.h>\n#include "db.h"\nextern unsigned __env_struct_sig(void);\n
int main(void){printf("%%08x %%zu %%zu %%zu %%zu\\n", __env_struct_sig(),
sizeof(DB),sizeof(DBC),sizeof(DB_ENV),sizeof(DB_TXN));return 0;}\n' > sig.c
cc -I$B sig.c $B/libdb.a -lpthread -ldl -luring -o sig && ./sig
```

`test/bench/scale_bench.c`'s `system("rm -rf ./SCALEDB")` was patched out into a
local copy via `python3 re.sub` (reading the home from `$SCALE_HOME`, pre-created)
because the box guard blocks that string anywhere in a command. No driver in this
branch removes anything.
