# RFC 0007: Optimistic read-path page validation (removing the shared pin refcount)

- **Status:** Phase 1 implemented and measured (see Decision)
- **Type:** Prospective
- **Author:** libdb maintainers
- **Date:** 2026-09-17
- **Tracking:** ROADMAP read-path performance; supersedes the four parked pin branches
- **Prototype:** `rfc/0007/` (planned)

---

## Summary

libdb's point-read path is dominated not by tree descent but by the bookkeeping
around touching a page: on a 96-thread run, `__memp_fget` plus its pin atomics are
**71.9% of self time**, against **0.69%** for the actual key comparison
(`__bam_cmp`). The cost is `atomic_inc(&bhp->ref)` / `atomic_dec` — a
read-modify-write on a cacheline every other core also wants, executed once per
page per level of every descent.

This RFC proposes replacing the shared refcount *on the read path only* with
**optimistic validation**: a reader reads the page without writing any shared
state, then validates that the frame did not change underneath it. This is the
LeanStore/OLC design (Leis et al.), adapted to libdb's constraint that LeanStore
does not have — **multi-process shared buffer pools**.

The key observation making this cheap for libdb: **the per-thread pin list already
exists.** `struct __db_thread_info` carries `dbth_pincount`, `dbth_pinlist` and
`dbth_pinarray[PINMAX]`, in shared memory, and `__memp_fget` already writes a pin
record on every fetch (`mp_fget.c:1252`). Today that record is consumed only by
`failchk` for crash cleanup, while the *authoritative* pin remains the contended
atomic. libdb is already doing the work twice; the expensive copy is the redundant
one.

## Motivation

Measured, not assumed. From `test/bench/PIN-REMEASURE-2026-09.md`, at 96 threads
after `db_get_multiple()` removed the cursor-lifecycle mutex:

| symbol | self time (batched path) |
|---|---:|
| `__os_atomic_read` | 34.31% |
| `__memp_fget` | 28.11% |
| `__os_atomic_dec` | 8.21% |
| **pin subtotal** | **71.89%** |
| `__bam_cmp` (real work) | 0.69% |

Before that mutex was removed the pin was only 2.4% of self time — it was hidden
behind a bigger wall. This is why the four parked branches (`perf/mpool-pin`,
`perf/bhpin-r1`, `perf/rsnap-ml`, `perf/lock-readpath`) all measured
neutral-or-regression: they were measured behind the cursor mutex, and they
attacked `bhp->ref` *as a refcount* — trying to make the counter cheaper. The
re-measurement (`perf/pin-remeasure-2026096`) confirmed the premise and still
rescued none of them.

The leverage is not a cheaper refcount. It is **not having a shared-cacheline
refcount on the read path at all.**

**Corroborated since this RFC was drafted.** A separate investigation
(`docs/design/read-descent-locks.md`, merged) measured the *other* per-page read
cost — the lock manager — and found it already minimal: a read descent takes
**one** lock, on the leaf, **depth-independent** (1.000 locks/read at 2, 3 and 4
levels), because interior pages are latch-coupled through buffer pins and never
locked. Plain SI readers with MVCC take **zero** lock objects
(`db_meta.c:1184-1189`). So the lock manager has nothing left to give on the read
path, and the buffer pin is the *only* remaining per-page read cost of any size.
That strengthens this RFC's case and also bounds it: pins are taken `levels-1`
times per descent (`rsnap` skips the root), so report throughput per **page
touched**, not per read.

Competitive context: against TidesDB (LSM) libdb is at **near-parity on warm point
reads (1.14x)** and **3.71x behind on single-threaded writes**
(`test/bench/TIDESDB-2026-09.md`). The write gap is structural — an LSM memtable
insert versus a B-tree in-place page update — and no tuning closes it. Reads are
where libdb is competitive, so reads are where per-operation cost should be
attacked.

## What LeanStore actually does

Read from source (`~/src/leanstore` @ `90fcf18`), not from the paper:

1. **There is no refcount.** `BufferFrame::Header` (`BufferFrame.hpp`) has no `ref`
   field at all. It has `HybridLatch latch` carrying the comment
   *"ATTENTION: NEVER DECREMENT"*.
2. **The latch is a version counter.** `HybridLatch` is
   `alignas(64) { atomic<u64> version; shared_mutex mutex; }` with
   `LATCH_EXCLUSIVE_BIT = 1`. An exclusive release does `version += 1` and stores.
3. **The optimistic read path performs no store.** `Guard::toOptimisticSpin()` /
   `toOptimisticOrJump()` (`Latch.hpp:106-127`) only *load* the version; if the
   exclusive bit is set they spin or abort. Nothing is written, so no cacheline is
   dirtied and no other core is invalidated.
4. **Safety comes from validation, not from pinning.** `Guard::recheck()`
   (`Latch.hpp:84`) compares the version again and aborts the operation if it moved.
5. **Eviction is safe because it goes exclusive.** `PageProviderThread.cpp`
   `evict_bf` takes the frame exclusive (`c_guard.guard.toExclusive()`), which bumps
   the version, so every concurrent optimistic reader's `recheck()` fails and
   restarts. There is a two-phase COOL-then-evict staging with the parent swip
   cooled first.
6. **Abort is `longjmp`.** `JumpMU.hpp` is a `setjmp`/`longjmp` framework with a
   manual destructor stack (`jumpmu_registerDestructor`, and the warning
   *"DO NOT DO ANYTHING BETWEEN setjmp and if"*).
7. **The buffer pool is `MAP_PRIVATE | MAP_ANONYMOUS`** (`BufferManager.cpp:41`) —
   **single-process.** LeanStore never has to reason about a peer process dying
   while holding an optimistic read.

Points 6 and 7 are the two things libdb cannot copy directly, and they shape the
design below.

## North-star check

This is the hard gate, and two items need real answers.

- **Embedded / no-server:** unaffected.
- **ACID, crash recovery:** unaffected *if* validation failure is a retry rather
  than an error return. A restarted read is not an anomaly; it is the same read
  performed again against a stable frame.
- **All access methods:** the mechanism lives in mpool, below the access methods,
  so B-tree/Hash/Queue/Recno/Heap all benefit. However the *retry* must be driven
  from a caller that can safely restart, which initially limits adoption to
  read-only descents (see Design, phase 1).
- **Multi-process correctness — THE hard part.** LeanStore is single-process.
  libdb's buffer pool lives in a shared region attached by unrelated processes, any
  of which can be `SIGKILL`ed mid-read. An optimistic reader that dies holding no
  shared state is *strictly better* than one that dies holding a refcount (which is
  exactly the leak `failchk` exists to repair). So optimistic reads **improve** the
  multi-process failure story. The risk moves to eviction: an evictor must be sure
  no live reader is mid-validation, and it cannot ask a dead process. Resolved by
  keeping the existing per-thread pin list authoritative for *eviction eligibility*
  (below), which `failchk` already knows how to clean.
- **On-disk / log format:** unchanged. No page content changes.
- **Region layout / ABI — MUST BE ADDRESSED.** `src/env/env_sig.c:80` hashes
  `struct __bh` (`__ADD(__bh)`). **Adding a version field to `BH` changes
  `__env_struct_sig()`, and `env_region.c` then refuses to attach every existing
  environment with `BDB1539 / DB_VERSION_MISMATCH`.** `abidiff` cannot see this,
  because `BH` is not public ABI — this is precisely the class of break that nearly
  shipped in v2026.09.5. Options:
  1. **Reuse existing bits.** `BH.flags` is `u_int16_t`; `mtx_buf` is a
     `db_mutex_t`. A version counter could live in currently-unused space, if any
     exists, at no size change. *Requires audit; preferred if feasible.*
  2. **Accept the signature change**, gated behind a major-version bump, with the
     `region-sig` CI gate updated in the same commit and release notes stating that
     existing environments must be recovered/recreated. Honest but expensive.
  3. **Derive the version from the mutex.** libdb's `db_mutex_t` already has
     internal state that changes on exclusive acquisition. If a monotonic
     generation can be read from it, no new field is needed. *Requires audit.*

  **Option 1 or 3 is a precondition for this RFC.** If neither is feasible, this
  becomes a major-version proposal and should be re-scoped.

## Design

### Phase 0 — resolve the version-storage question — **ANSWERED: YES**

Measured, not argued. `sizeof(BH) = 96` on LP64, with this layout:

| field | offset | size |
|---|---:|---:|
| `mtx_buf` | 0 | 8 |
| `ref` | 8 | 4 |
| `flags` | 12 | 2 |
| `wired` | 14 | 1 |
| *(hole)* | **15** | **1** |
| `priority` | 16 | 4 |
| `hq` | 24 | 16 |

There is a **1-byte hole at offset 15**, between `wired` and the 4-byte-aligned
`priority`. Adding `u_int8_t gen;` there keeps `sizeof(BH) == 96` — verified by
compiling with the field present — and, decisively, **`__env_struct_sig()` is
byte-identical: `0xdaf24890` before, with the field, and after reverting.**

So existing environments still attach, and this is a point-release change rather
than a major version. `BH.flags` additionally has 7 unused bits (only 0x001-0x100
are defined) if more room is ever needed.

Caveat carried forward into risk 3: a `u_int8_t` generation wraps every 256
exclusive acquisitions, which is *far* too small to rely on alone. The design must
therefore pair the generation with a field that cannot ABA over the same window
(e.g. validating `pgno` and `mf_offset` together with `gen`), or claim two bytes
(one from the hole, one from the spare `flags` bits) and state the wrap argument
explicitly. **This is now the top open question, replacing the storage question.**

### Phase 1 — optimistic read-only descents

Narrowest useful slice, mirroring the guard that already exists at
`bt_search.c:517` for read-only descents (`SR_READ` with no write/stack/parent
flags, non-OPD, B-tree, no `C_RECNUM`, `multiversion == 0`).

1. `__memp_fget` gains a `DB_MPOOL_OPTIMISTIC` mode: locate the frame, read the
   generation, **do not** `atomic_inc(&bhp->ref)`, **do not** take `bhp->mtx_buf`,
   return the page pointer plus the observed generation.
2. The caller reads what it needs from the page — key comparison, child pgno — and
   then calls `__memp_fvalidate(bhp, gen)`. If the generation moved, the read is
   discarded and the operation retries with the current pinning path.
3. The per-thread pin record (`dbth_pinarray`) **is still written**, and becomes
   the authoritative signal for eviction eligibility. It is a thread-local store,
   uncontended, already on the code path today, and already understood by
   `failchk`.
4. Eviction (`__memp_alloc`) must, before reusing a frame, (a) bump the frame
   generation under exclusive `mtx_buf`, and (b) confirm no live thread's pin list
   references it. (b) is a scan over `DB_THREAD_INFO` records; it is off the hot
   path and bounded by thread count.

**Retry mechanism — libdb does NOT get `longjmp`.** LeanStore's `jumpmu` is not
portable to a C library that must be safe as a callee in arbitrary applications and
compiled as C89/C++. Instead: validation failure returns a distinguished code
(`DB_MPOOL_RETRY`, internal) and the *single* call site in `__bam_search`'s
read-only path loops. One retry site, explicitly bounded (e.g. 3 attempts, then
fall back to the pinning path permanently for that descent). This is a smaller and
more auditable change than a non-local jump, and it is why phase 1 is scoped to one
caller.

### Phase 2 — extend to Hash/Recno read paths, and to internal-node traversal

Only after phase 1 is measured. Each additional caller is a new retry site and must
be justified by its own measurement.

### Explicitly out of scope

- Write paths. A writer must pin; nothing here changes that.
- MVCC/snapshot reads (`multiversion != 0`). The chain walk mutates state; the
  existing guard already excludes these.
- Replacing `bhp->ref` entirely. It remains the write-path pin. This RFC removes it
  from the *read* path only.

## Alternatives considered

- **Make the refcount cheaper** (sharded counters, per-core counters, biased
  locking). This is what the four parked branches attempted, all measured
  neutral-or-regression. The problem is the shared cacheline, not the arithmetic.
- **Hazard pointers proper** (per-thread published pointer, scanned by the
  reclaimer). This is very close to what the existing `dbth_pinarray` already is,
  and the design above is best understood as *finishing* libdb's existing hazard
  list by making it authoritative. A from-scratch hazard implementation would
  duplicate it.
- **RCU / epoch reclamation.** Requires quiescent states, which a library with
  application-owned threads cannot force.
- **Do nothing and pursue the write path instead.** Rejected on the measurement:
  the write deficit versus an LSM is structural (3.71x), while reads are at parity
  and have a 71.9% software overhead that is removable.

## Risks & open questions

1. **(Blocking) Can a generation be stored without changing `sizeof(struct __bh)`?**
   See Phase 0. This decides whether the RFC is a point release or a major version.
2. **Is the eviction pin-list scan cheap enough?** It is off the hot path, but it is
   O(threads) per eviction and eviction is not rare under memory pressure. Needs
   measurement, not argument.
3. **ABA on the generation.** A `u16` generation can wrap. Width and wrap
   behaviour must be stated; a wrapped generation that compares equal is a silent
   correctness bug of exactly the kind this project keeps finding.
4. **A dead process's pin list.** `failchk` already reclaims these; the new
   dependency is that eviction must treat a stale pin from a dead process as
   *not* blocking, or a crashed reader wedges the pool. This is a liveness bug
   waiting to happen and needs an explicit test.
5. **Memory ordering.** The generation read, the page read, and the re-read must be
   correctly fenced. libdb's `atomic.h` already has the primitives; the ordering
   argument must be written down, not assumed, and stated per-architecture
   (x86-64 TSO is not sufficient justification for the ARM builds libdb ships).
6. **Torn reads of page contents.** An optimistic reader may read a page *while* a
   writer modifies it, seeing an inconsistent intermediate state, and only discover
   it at `recheck()`. Everything done with that data before validation must be
   side-effect-free and must not fault. Copying a child pgno is safe; following a
   corrupt offset into the page is not. This bounds what phase 1 may do before
   validating.

## Prototype / evidence

Required before any merge:

- Phase 0 answer with `env_sig_print.sh` proof.
- A/B on a dedicated EC2 box (never a shared host — a shared-host run at load 25
  produced CVs of 40–92% and inverted an ordering earlier in this project),
  >= 5 reps, arms alternating within each rep, at t = {1, 8, 32, 96}, reporting
  median and CV, with the noise floor measured base-against-itself.
- Profile before and after: the pin's share of self time must fall, and the
  claim "reads perform no shared writes" must be shown with `perf c2c` or
  equivalent cacheline evidence, not inferred from source.
- Correctness: `db_verify` clean, full `test/isolation` at both ISO levels,
  `ssi001`–`ssi011`, ASan and TSan clean, and a **retry-path teeth test** proving
  the validation actually fires (a build where the generation never changes must
  FAIL the test — otherwise the optimistic path is vacuously "correct" because it
  never retried; this project has eight recorded vacuous-green instances,
  including one in a performance measurement).
- Multi-process: the `failchk` interaction from risk 4, with a killed reader.

## Decision

- **Decision:** **Phase 1 IMPLEMENTED and MEASURED** on
  `perf/optimistic-reads`; **accepted for the batched read path, and for the
  per-key path to 32 threads. NOT recommended as a default yet** because the
  per-key path regresses at 96 threads for a reason outside this RFC (below).
  Phase 0 was already resolved; the two blockers named above are now answered:
  the generation-width/ABA question and the phase-1 measurement
  (`test/bench/OPTIMISTIC-READS-2026-09.md`).

- **Rationale:**

  *The premise held.* The pin atomics leave the profile entirely: at t=96
  `__os_atomic_read` falls from 20.8% of self time to below a 1% cutoff,
  `__os_atomic_dec` from 5.4% to below cutoff, `__memp_fget` from 12.4% to below
  cutoff. Pin operations per read fall from `levels-1` to exactly 1 (only the
  leaf is pinned and locked); measured `opt_pages/opt_tries` is 2.0.

  *Throughput, dedicated 96-vCPU box, 4-5 reps, arms alternating within each rep,
  noise floor (base vs base2) under 0.5% at t>=8:*

  | path | t=1 | t=8 | t=32 | t=96 |
  |---|---:|---:|---:|---:|
  | per-key `DB->get` | 1.09x | 1.26x | **1.71x** | **0.77x** |
  | batched `db_get_multiple` (32) | 1.12x | 1.64x | 1.84x | **2.05x** |

  At t=32 the per-key path does 1.71x the reads while performing **0.57x** the
  pinned page-touches, and on the batched path (t=32) hardware counters show
  **0.78x** the Read-For-Ownership requests per read -- 22% less cross-core write
  traffic per unit of work.  (`perf c2c` is unusable on this instance: its PMU
  does not expose the PEBS memory events, and it produces no data at all.)

  *The t=96 per-key regression is measured, not speculated, and is not this
  change.* `perf --call-graph dwarf`: 82% of time in `__db_tas_mutex_lock_int`,
  of which 41% is under `__dbc_close` and 40% under `__db_cursor_int` -- the
  cursor-lifecycle mutex, reached from `__db_get`. A faster descent arrives
  there more often per second, converting a page-pin bottleneck into a
  cursor-lifecycle one. The identical code on the batched API, which does not pay
  per-key cursor open/close, is 2.03x at the same thread count. This is the same
  wall `PIN-REMEASURE-2026-09.md` hit from the other side (the pin was only 2.4%
  of self time until that mutex left the batched path).

  *Risk 3 (ABA) -- answered.* 7-bit counter + 1 in-flux bit in the single spare
  byte, with validation comparing the full tuple `(gen, pgno, mf_offset)` and
  `gen` never reset on frame reuse. Taking a second byte from `BH.flags` was
  rejected: the flags word takes a non-atomic read-modify-write under a merely
  SHARED latch (`__memp_pgwrite` clears `BH_DIRTY` that way), so a generation
  living there could be lost to a racing `F_CLR`. **Stated residual:** the frame
  must be exclusively acquired a nonzero multiple of 128 times *and* be hosting
  the same `(pgno, mf_offset)` at validation, all inside one reader's
  pre-validation window of a few hundred instructions.

  *Risk 4 (dead process's pin) -- answered, with teeth.* `__memp_bh_pinned`
  disregards a pin whose owner fails `is_alive`. The first version of that test
  was VACUOUS (a cursor also holds `bhp->ref`, which eviction already respects,
  so both arms passed at 160 stranded frames); the shipped test drives
  `__memp_bh_pinned` directly and flips only the owner's liveness. A build with
  the `is_alive` check removed fails it.

  *Risk 5 (memory ordering) -- answered per architecture.*
  `__os_atomic_thread_fence()` is seq-cst on every backend, so the argument is
  not an x86-TSO argument: ARM64 gets `dmb ish`, PowerPC `sync`, RISC-V
  `fence rw,rw`. The subtle edge is store-load: the reader publishes its pin
  record *before* re-reading the generation, which is what makes the eviction
  handshake (bump under exclusive latch, then scan pin lists) airtight.

  *Risk 6 (torn reads) -- bounded.* `__bam_opt_child` is the only code touching an
  unvalidated page: reads confined to the frame, `HOFFSET`/index-array/`BINTERNAL`
  bounds-checked before any dereference, no page pointer followed (an overflow
  key bails), nothing written. `bt_compare` is called only with a DBT inside the
  frame, so torn bytes can produce a wrong answer that validation discards, but
  not a fault.

  *Risk 2 (eviction pin scan) -- acceptable.* `__memp_bh_pinned` is O(threads),
  off the hot path, and did not appear in any profile.

  *Two things source review missed and measurement caught*, recorded because they
  are the reason this RFC's evidence bar exists:
  1. The "pin-free" path performed **shared writes** -- `++c_mp->put_counter`
     (one word per region, dirtied by every reader on every core) and a
     per-MPOOLFILE stat counter. ThreadSanitizer found the first. Removed.
  2. The first design **preempted** the existing `rsnap` root snapshot instead of
     composing with it, so the A/B was measuring "optimistic descent vs root
     snapshot" -- and the snapshot was winning (0.87x at t=8, 0.78x at t=32; raw
     data kept in `test/bench/opt_ab_v1_preempt.tsv`). Now the snapshot supplies
     the root's child and the optimistic walk covers the remaining interior
     levels; `pages_per_read` fell 0.88 -> 0.16, i.e. the root fetch is gone
     rather than merely cheaper.

- **Conditions / follow-ups:**
  1. **Do not enable by default until the cursor-lifecycle mutex is addressed.**
     It is now the t=96 wall on the per-key path and is outside this RFC.
     `DB_NO_OPTREAD` makes the path inert.
  2. **Next target: the bucket mutex.** `__memp_fget_opt` still read-locks
     `hp->mtx_hash` to walk the hash chain safely, trading one atomic on the
     *frame* for one on the *bucket*. Favourable (the bucket line is contended by
     a fraction of the readers of a given page) and confirmed by the profile, but
     it is why the win is 1.7x rather than what "71.9% is the pin" would suggest.
  3. **Phase 2 callers** (Hash/Recno, internal-node traversal) each need their own
     retry site and their own measurement. None added here.
  4. **TSan cannot gate this code.** Baseline 315 races vs 310 on the branch;
     libdb's shared-region mutexes are invisible to it. Useful diagnostically
     (it found the `put_counter` write) but not as a pass/fail gate.
  5. Precondition worth documenting for users: the optimistic path requires
     `DB_ENV->set_thread_count` (the pin list lives in the thread region, which is
     allocated only when `thr_max != 0`). Without it the path correctly refuses to
     engage -- silently. It bailed on 100% of descents before this was found.
