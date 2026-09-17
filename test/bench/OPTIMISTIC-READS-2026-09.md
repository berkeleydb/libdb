# Optimistic read-path page validation — RFC 0007 phase 1, measured

- **Date:** 2026-09-17
- **Branch:** `perf/optimistic-reads` (base: `master` @ `e358fb611`)
- **RFC:** `rfc/0007-optimistic-read-validation.md`
- **Hardware:** EC2, 96 vCPU, 185 GB, Linux 6.1.0-53-cloud-amd64, THP off, ASLR off
- **Verdict:** **Win on the batched read path at every thread count and on the
  per-key path to t=32; an honest regression at t=96 on the per-key path, whose
  cause is measured and is not this change.** Details in §6 and §11.

---

## 1. What was built

`BH.gen`, a one-byte frame generation in the padding hole `struct __bh` already
had, plus a pin-free page fetch and a bounded retry at one call site.

| piece | file | what it does |
|---|---|---|
| `BH.gen` | `src/dbinc/mp.h` | 1-byte seqlock: bit 0 "in flux", bits 1-7 episode counter |
| `BH_SET/CLR_EXCLUSIVE` | `src/dbinc/mp.h` | bump+mark and unmark, replacing every `F_SET(bhp, BH_EXCLUSIVE)` |
| `__memp_fget_opt` | `src/mp/mp_fget.c` | find frame, sample `(gen, pgno, mf_offset)`, return the page with **no `atomic_inc(&bhp->ref)` and no `bhp->mtx_buf`** |
| `__memp_fget_opt_valid` / `_release` | `src/mp/mp_fget.c` | re-check the sample; drop the pin record |
| `__memp_bh_pinned` | `src/mp/mp_alloc.c` | eviction's "is any LIVE thread's pin list on this frame" |
| `__bam_opt_child` | `src/btree/bt_search.c` | pick a child from an unvalidated page, within a stated safety bound |
| `__bam_opt_descend` | `src/btree/bt_search.c` | pin-free interior walk with optimistic latch coupling |
| retry site | `src/btree/bt_search.c` | ONE site, 3 attempts, then the pinning path permanently |

The per-thread pin record (`dbth_pinarray`) is still written and becomes
authoritative for eviction eligibility, exactly as the RFC specifies.

## 2. Storage: env signature and ABI proofs

`env_sig.c:80` hashes `struct __bh`. A change there makes `env_region.c` refuse
to attach **every existing environment** (BDB1539 / `DB_VERSION_MISMATCH`) while
`abidiff` stays green, because `BH` is not public ABI.

```
$ dist/env_sig_print.sh .          # master
0xdaf24890
$ dist/env_sig_print.sh .          # perf/optimistic-reads
0xdaf24890
```

Byte-identical. Same value measured on the EC2 box for both trees
(`0xb86f77f0` there — a different build configuration, but equal between arms,
which is the only comparison that means anything).

Layout, from the compiler (`test/bench/bh_layout.c`):

```
sizeof(BH) = 96          (96 on master)
  mtx_buf  off=  0 size=8
  ref      off=  8 size=4
  flags    off= 12 size=2
  wired    off= 14 size=1
  gen      off= 15 size=1   <-- was a 1-byte hole
  priority off= 16 size=4
  ...
public: DB=1744 DBC=552 DB_ENV=2088 DB_TXN=336
```

`sizeof(BH)` unchanged at 96; public ABI unchanged at 1744/552/2088/336.

## 3. The ABA decision, and its exact bound

**Decision: 7 bits of counter + 1 in-flux bit in the single spare byte, with
validation comparing the FULL tuple `(gen, pgno, mf_offset)`.**

Rejected the alternative (steal a second byte from `BH.flags`, which has 7 spare
bits) because it buys 8 more bits of counter at the cost of putting the
generation inside the flags word — and the flags word is subject to a
**non-atomic read-modify-write under a merely SHARED latch**: `__memp_pgwrite`
clears `BH_DIRTY` that way. A generation that can be lost to a racing
`F_CLR` is worse than a short one. This is the same reasoning that made `wired`
a dedicated byte rather than a flag bit.

Why the tuple closes the gap: 7 bits wraps after **128** exclusive
acquisitions. For a stale sample to compare equal, the frame must be
re-acquired exactly a multiple of 128 times **and** end up holding the *same
page of the same file* — because `pgno` and `mf_offset` are compared too, and
`gen` is never reset on frame reuse (a reset could hand a reader a value from
the frame's previous life).

**Stated residual, exactly:** an optimistic reader loses correctness only if,
inside its pre-validation window, the frame is exclusively acquired a nonzero
multiple of 128 times and is, at validation, hosting the same `(pgno,
mf_offset)` it hosted at sampling. The window is a few hundred instructions (one
binary search over one page, no I/O, no locks); 128 exclusive acquisitions
require 128 evictions/dirties of one frame in that window, and the eviction path
alone takes the frame's `mtx_buf` and a bucket mutex each time. Not zero, but
bounded and stated rather than hand-waved.

The in-flux bit is what makes the remaining case fail-closed: a reader that
samples an odd generation gives up immediately rather than reading bytes a
writer is modifying.

## 4. Memory ordering, per architecture

```
writer: [store gen odd] FENCE [store page bytes] ... FENCE [store gen even]
reader: [load gen,pgno,mf] FENCE [load page bytes] FENCE [reload gen,pgno,mf]
```

The fence is `__os_atomic_thread_fence()` (`src/os/os_atomic.c`), which is a
**sequentially-consistent** fence on every backend libdb ships:

| backend | emits |
|---|---|
| C11 `<stdatomic.h>` / GCC-Clang `__atomic` (tier 0/1) | `__atomic_thread_fence(__ATOMIC_SEQ_CST)` |
| legacy `__sync` (tier 2) | `__sync_synchronize()` |
| Windows | `MemoryBarrier()` |
| Solaris | `membar_enter()` |

So the argument is **not** an x86-TSO argument. On x86-64 the store-store and
load-load orderings would come free and only the store-load edge needs the
fence; on **ARM64 the fence emits `dmb ish`**, on PowerPC `sync`, on RISC-V
`fence rw,rw` — which is exactly what makes the plain (non-atomic) loads of the
page bytes safe to order against the generation load. libdb ships ARM builds,
so this is load-bearing, not decoration.

One ordering edge is easy to miss and is called out at the code: the reader
publishes its **pin record before** re-reading the generation, with a fence
between (`mp_fget.c`). That is a store-load edge — the one x86 also needs — and
it is what makes the eviction handshake work: `__memp_alloc` bumps the
generation under the exclusive latch (with a fence) and *then* scans the pin
lists, so a reader either published before the bump and is seen by the scan, or
published after and fails validation. Neither can slip through.

## 5. The pre-validation safety bound (RFC risk 6)

`__bam_opt_child` is the only code that touches an unvalidated page. It may:

- read only within `[h, h + dbp->pgsize)`;
- dereference nothing derived from page contents without bounds-checking it
  first — `HOFFSET` and the index array are validated against the page size
  before any `inp[]` access, and every `BINTERNAL` is checked against the page
  end both at its header and after reading its length;
- **not follow a page pointer.** An overflow key (`B_TYPE != B_KEYDATA`) would
  require fetching another page, so it bails;
- write nothing, anywhere;
- call the user's `bt_compare` only with a DBT whose `(data, size)` lies inside
  the frame. libdb already requires `bt_compare` to be a pure function of its
  two DBTs, so torn bytes can yield a *wrong answer* — which validation then
  discards — but not a fault.

Any violation returns `DB_MPOOL_RETRY`: on a torn page that is a race to retry,
on a genuinely corrupt page the ordinary pinning path produces the real error
with the page latched. Copying a child `pgno` is safe; following an unvalidated
in-page offset is not, and is not done.

**Optimistic latch coupling** is the other half of correctness, and is the part
that is easy to get wrong. The pinning descent holds the parent's latch across
the child fetch. With no latch:

```
sample P; child = search(P); validate P     -> child really was P's child
sample C; read C;            validate C     -> C's bytes were stable
re-validate P (still held)                  -> P unchanged across all of it
```

The third line is load-bearing, and is why the parent's sample outlives the
child's acquisition: it produces a single instant at which the whole hop was
true. Without it, P could split after being validated and before C was read,
and the descent would land in a subtree that no longer covers the key — a wrong
answer, not merely a slow one.

## 6. Measurement

Methodology, all of it enforced by `test/bench/opt_ab.sh`:

- **one binary, one env-home directory, one runtime switch** (`DB_NO_OPTREAD`).
  Two builds or two directories would be two memory layouts, and DB_PRIVATE
  throughput on this class of box is bimodal in the *length* of the env-home
  path (1.65x step, ~0% spread within a mode). The tidy form is the one that
  manufactures a win.
- **arms alternate within each rep**: `base, opt, base2, opt2`.
- **noise floor inline**: `base` vs `base2` is the same code measured against
  itself. `test/bench/opt_report.py` prints WIN/REGRESS only when the effect
  exceeds that floor.
- every rep printed and appended as it completes.
- every RESULT line carries `opt_tries/opt_pages/opt_invalid`, so an arm that
  never executed the code under test cannot be published as a neutral result.

### 6.1 Per-key reads (`DB->get`), 2M keys, 4 GB cache, 10 s windows

```
   t   base med    CV%    opt med    CV%   ratio   noise    verdict  pages/try
   1     0.441M   1.9%     0.480M   2.2%   1.089    0.0%        WIN      2.00
   8     1.824M   2.4%     2.297M   1.9%   1.259    0.2%        WIN      2.00
  32     2.311M   1.4%     3.959M   1.8%   1.713    0.4%        WIN      1.87
  96     2.801M   0.6%     2.148M   13.6%  0.767    0.1%    REGRESS     2.00
```

(keys/s; 4 reps x 2 arms per tag. Noise floor under 0.5% at t>=8.)

### 6.2 Per *page touched*

The brief is right that the pin count per read is `levels-1`, not `levels`.
Measured `opt_pages / opt_tries` is **2.0** on this tree, and `rsnap` supplies
the root's child without a fetch, so a read touches the root snapshot (no pin),
2 interior pages optimistically (no pin), and 1 leaf (pinned + locked).

| t | base pages/s (3 pins/read) | opt pages/s (1 pin + 2 pin-free) | pinned pages/s base | pinned pages/s opt |
|---|---:|---:|---:|---:|
| 1 | 1.32M | 1.44M | 1.32M | 0.48M |
| 8 | 5.47M | 6.89M | 5.47M | 2.30M |
| 32 | 6.93M | 11.88M | 6.93M | 3.96M |
| 96 | 8.40M | 6.44M | 8.40M | 2.15M |

At t=32 the change does 1.71x the reads while performing **0.57x** the pin
operations — 3.96M pinned page-touches/s against 6.93M.

### 6.3 Batched reads (`db_get_multiple`, 32 keys/call), t=96 spot check

```
base  3.578M / 3.470M keys/s
opt   7.283M / 6.987M keys/s      ratio 2.03x
```

Same code, same box, same env home, minutes apart. Full sweep in section 6.4.

### 6.4 Batched reads (`db_get_multiple`, 32 keys/call), full sweep

```
   t   base med    CV%    opt med    CV%   ratio   noise    verdict  pages/try
   1     0.495M   2.5%     0.554M   2.6%   1.120    2.1%        WIN      2.00
   8     1.914M   2.6%     3.130M   3.4%   1.635    0.5%        WIN      1.98
  32     2.573M   1.0%     4.740M   3.1%   1.842    0.8%        WIN      1.45
  96     3.479M   1.0%     7.116M   1.5%   2.046    0.0%        WIN      1.78
```

WIN at every thread count, noise floor at or under 2.1%. Raw data in
`test/bench/opt_ab_batch.tsv`.

## 7. Why t=96 regresses on the per-key path (measured, not guessed)

`perf record --call-graph dwarf`, t=96, per-key arm:

```
base:  31.4% __db_tas_mutex_lock_int   20.8% __os_atomic_read
       12.4% __memp_fget                7.5% __memp_fput
        5.4% __os_atomic_dec            4.6% __bam_cmp

opt:   80.8% __db_tas_mutex_lock_int    7.6% __db_tas_mutex_unlock
        1.5% __bam_search
```

The pin atomics **disappear** from the profile: `__os_atomic_read` 20.8% -> below
the 1% cutoff, `__os_atomic_dec` 5.4% -> below cutoff, `__memp_fget` 12.4% ->
below cutoff. That is the RFC's premise confirmed.

What replaced them, resolved by call graph:

```
opt, __db_tas_mutex_lock_int callers:
   41.1%  __dbc_close  <- __db_get
   40.5%  __db_cursor_int <- __db_get
```

The bottleneck at t=96 is the **cursor-lifecycle mutex**, not anything this
branch touches. A faster descent simply arrives there more often per second, so
the change converts a page-pin bottleneck into a cursor-lifecycle one and loses
throughput at the point where the latter saturates. This is the same wall
`PIN-REMEASURE-2026-09.md` documented from the other side: the pin was only 2.4%
of self time *until* the cursor mutex was removed from the batched path. On the
batched path, which does not pay per-key cursor open/close, the identical code is
2.03x at t=96 (§6.3).

So: reported as a regression on that path at that thread count, with the cause
named. Not attributed to noise (the floor is 0.1%), and not explained away.

## 8. No shared writes -- hardware evidence, and one that source review missed

The first version of this branch **did** perform shared writes on the
"pin-free" path, and the source read as though it did not:

- `++c_mp->put_counter` -- one word per cache region, so every reader on every
  core dirtied the same line. ThreadSanitizer flagged it.
- `STAT_INC_VERB(... st_cache_hit ...)` -- one word per MPOOLFILE.

Both removed; hit attribution moved to the process-local `opt_pages` counter.
This is exactly why RFC 0007 requires cacheline evidence rather than a source
argument, and the requirement earned itself on this branch.

Remaining writes on the optimistic path, exhaustively: this thread's own
`PIN_LIST` slot and `ip->dbth_pincount`, both inside this thread's
`DB_THREAD_INFO`; plus the shared *read* lock on the bucket mutex (section 9).

### 8.1 `perf c2c` does not work here -- measured, not assumed

```
$ perf c2c record -o /tmp/t.data -- sleep 0.2
failed: memory events not supported
```

The instance's PMU does not expose the PEBS memory events `perf c2c` needs, and
it produces **no data at all** -- a first run of `opt_c2c.sh` left two 68-byte
files saying `failed to open ... .c2c.data`. Reporting "c2c clean" from that
would have been evidence-free.

### 8.2 The counter that does work: RFO per read

`l2_rqsts.all_rfo` / `l2_rqsts.rfo_miss`. A Read-For-Ownership is issued when a
core needs a line in a **writable** state, and it misses L2 exactly when another
core owns it. That is the cross-core write sharing this RFC is about, counted in
hardware.

Method: run the same binary for a 4 s and a 12 s window and difference them, so
the load phase (identical in both) cancels exactly and what remains is 8 s of
pure read traffic. Normalized per read, because the faster arm does more work
per second and raw counts would flatter it. (A first attempt reported ~700 RFO
misses for a 32-thread 8-second run -- the benchmark had failed to open its
environment and `perf stat` dutifully counted 0.7 ms of nothing. Implausibly
small counters are a harness failure, not a result.)

**Batched path (t=32), where no per-key cursor mutex is in the way:**

| arm | keys/s | RFO per read | stores per read |
|---|---:|---:|---:|
| base | 3.533M | 13.65 | 876.3 |
| opt | 6.034M | **10.67** | 818.7 |
| ratio | 1.71x | **0.781** | 0.934 |

**22% fewer cross-core write-ownership requests per read**, and 6.6% fewer
retired stores per read, while doing 1.71x the work. That is the claim, in
hardware.

**Per-key path (t=32) -- the opposite sign, and worth stating:**

| arm | keys/s | RFO per read | stores per read |
|---|---:|---:|---:|
| base | 3.303M | 33.35 | 1228.6 |
| opt | 3.882M | **42.46** | 1150.3 |
| ratio | 1.18x | **1.273** | 0.936 |

RFO per read goes **up** 27% on the per-key path, and this is section 7's finding
seen through a different instrument. Note first that *stores* per read fall
(0.936x) on both paths -- the change genuinely removes writes. What rises is RFO
**traffic**, and libdb's mutex is a test-and-set: every spin iteration is a
locked RMW, i.e. an RFO. A descent that reaches the cursor-lifecycle mutex
sooner spins on it more, so the pin's RFOs are replaced by more mutex RFOs.
Remove that mutex from the path (the batched API) and the number inverts to 0.78x
on otherwise identical code.

So the honest form of the no-shared-write claim is: **the optimistic read path
itself performs no shared writes, and where the surrounding code does not
reintroduce one, cross-core write traffic per unit of work falls 22%.**

## 9. What is left on the table: the bucket mutex

`__memp_fget_opt` still takes `hp->mtx_hash` **read-locked** to walk the hash
chain, because walking an unlocked `SH_TAILQ` risks a wild pointer rather than a
stale read. A shared latch acquire is itself an atomic RMW on a shared line, so
the optimistic path trades one atomic on the *frame* for one on the *bucket*.
That trade is favourable here — the bucket line is contended by a fraction of
readers where the frame line is contended by all readers of that page — and the
profile confirms the frame atomics disappear. But it is the obvious next
target, and it is why the win is 1.7x rather than the 3.5x a naive reading of
"71.9% of self time is the pin" would predict.

## 10. Correctness gates

### Before/after profile at t=32 (per-key path, arms alternating)

```
base:  28.7% __os_atomic_read   18.4% __memp_fget   11.1% __memp_fput
        9.3% __os_atomic_dec     7.1% mutex_unlock   6.4% mutex_lock_int
opt:   30.7% mutex_lock_int     15.6% mutex_unlock  12.1% __bam_search
        8.7% __os_atomic_read    3.5% __db_cursor_int 3.3% __dbc_close
        2.3% __memp_fget         2.1% __memp_fput
```

`__memp_fget` 18.4% -> 2.3%, `__memp_fput` 11.1% -> 2.1%, `__os_atomic_read`
28.7% -> 8.7%. The pin's share falls by roughly 8x; what grows is mutex time,
which section 7 resolves to the cursor-lifecycle mutex.

| gate | result |
|---|---|
| teeth: optimistic ON | `VERDICT opt-fires-teeth ran=37397 fired=99` — path runs, validation fires, 0 wrong answers |
| teeth: `DB_NO_OPTREAD=1` | `VERDICT opt-fires-control inert` — tries == 0 |
| teeth: **sabotaged build** (no gen bump) | **FAIL 3/3** — twice "validation NEVER FIRED", once a real wrong answer (`mismatch=1`) |
| dead-process pin liveness | `VERDICT opt-deadpin ... 5/5 assertions`; sabotaged (is_alive removed) FAILS assertion 3 |
| `db_verify` | clean; 300k keys, half deleted, every read correct |
| `test/isolation`, both ISO levels, parts={default,1} | 9 scenarios x 2 levels x 2 parts, **0 unexpected outcomes** |
| `ssi001`–`ssi011` | **11/11 pass** (`OPT_SSI_TCL_OK 11/11`) |
| ASan + UBSan | **0 AddressSanitizer errors**. 60 UBSan reports vs 54 on the same binary with the path disabled; the 6 new ones are all libdb's own `SSZA`/`P_OVERHEAD` null-pointer-offsetof idiom in `bt_search.c`, the same class as the 54 pre-existing |
| TSan | **not usable as a gate**: baseline 315 races, branch 310. libdb's shared-region mutexes are invisible to TSan. Used diagnostically, and it found the `put_counter` shared write (§8) |
| `test/check_manifest.sh` | `manifest gate: OK`, 42 verdict lines |

### Retry rate

`opt_invalid / opt_tries`, i.e. how often validation fired:

| workload | rate |
|---|---|
| read-only, 4 GB cache (the A/B) | 0 per 1000 tries |
| readers + writers splitting/merging, 8 MB cache | 0.40 – 0.61 per 1000 tries |
| same, ASan build (slower, wider windows) | 0.50 per 1000 tries |

Zero on a pure read workload is the correct and expected value — nothing moves
the generation. That is precisely why the teeth test runs writers and a tiny
cache, and why a build that cannot fire is proven to fail (§10, row 3).

## 11. Verdict

**Phase 1 works, is safe by a stated argument, and is worth having on the
batched read path and on the per-key path to 32 threads.** The mechanism does
what the RFC predicted: the pin atomics leave the profile entirely.

- Batched reads: **1.12x / 1.64x / 1.84x / 2.05x** at t=1/8/32/96 -- a win at
  every thread count, noise floor at or under 2.1%.
- Per-key reads: **1.09x / 1.26x / 1.71x** at t=1/8/32.
- Per-key reads at t=96: **0.77x**, because the change moves the bottleneck onto
  the pre-existing cursor-lifecycle mutex. Cause measured (§7), not speculated.
- Pin operations per read fall from `levels-1` to 1; at t=32 the branch does
  1.71x the work with 0.57x the pinned page-touches.

Recommended follow-ups, in order of expected value: (a) the cursor-lifecycle
mutex, which is now the t=96 wall on the per-key path and is *not* part of this
RFC; (b) the bucket mutex in `__memp_fget_opt` (§9); (c) phase 2 callers, each
with its own measurement.

Not recommended: shipping this enabled by default before (a), because the t=96
per-key regression is real for applications that use `DB->get` per key at very
high thread counts. `DB_NO_OPTREAD` exists and makes the path inert.

## 12. Reproducing

```sh
# proofs
dist/env_sig_print.sh .                    # expect 0xdaf24890, same as master
cc -w -DDB_BH_HAS_GEN -I build_unix -I src -I . -o /tmp/bh test/bench/bh_layout.c && /tmp/bh

# teeth (three arms, including the sabotaged build that MUST fail)
test/bench/opt_teeth.sh -b build_unix

# dead-process pin liveness
cc -O2 -w -I build_unix -I src -I . -o /tmp/dp test/bench/opt_deadpin.c build_unix/libdb.a -lpthread -luring
/tmp/dp /tmp/somewhere

# A/B  (per-key, then batched)
test/bench/opt_ab.sh -b build_unix -r 5 -s 10 -n "1 8 32 96" -k 2000000 -o out.tsv
test/bench/opt_ab.sh -b build_unix -r 5 -s 10 -n "1 8 32 96" -k 2000000 -m batch -B 32 -o outb.tsv
test/bench/opt_report.py out.tsv

# profiles + cacheline evidence
test/bench/opt_c2c.sh -b build_unix -t 32

# gates
test/isolation/run.sh
test/isolation/opt_build_tcl.sh && test/isolation/opt_ssi_tcl.sh
test/check_manifest.sh
```
