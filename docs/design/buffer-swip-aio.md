# Scalable buffer access: tagged swip, optimistic descent, and async I/O

Status: design / plan of record. Companion to
[`scaling-findings.md`](scaling-findings.md). Code anchors verified against
`master` and recorded in `research/mpool-btree.md` and `research/os-aio.md`.

## 1. Problem

Measured (24-core Xeon, `lab/bench/scale_bench`): in-cache random reads peak at
~8 threads and then *negatively scale*. `perf` self-time is dominated by
`futex` + `__atomic_inc/dec`. The cause is not a lock we can shard: every
B-tree descent re-pins the root and upper internal pages, and each pin performs
**atomic read-modify-writes on shared words** for a page touched by every thread
on every operation:

1. the bucket latch share-count (`hp->mtx_hash`, already a shared latch),
2. the buffer pin count (`bhp->ref`, `mp_fget.c:296`),
3. the buffer latch share-count (`bhp->mtx_buf`, `mp_fget.c:318`),

plus a fourth write on the way out: the per-access LRU bump
(`bhp->priority = c_mp->lru_priority`, `mp_fput.c:215`).

A controlled A/B already proved that **cache-line isolation of those fields does
nothing** (±0.6%): the cost is *true* sharing of the words themselves, not false
sharing of neighbours.

### The hardware rule that governs >300-core NUMA

A **read** of a shared cache line is nearly free — the line is replicated in
Shared state in every socket simultaneously. A **write** to a shared line
invalidates every replica and forces a cross-socket read-for-ownership. At
hundreds of cores across many sockets, any per-operation write to a shared line
is the scaling wall. Therefore:

> **The read path must perform zero stores to shared memory.**

Sharded/per-core pin counts only relocate one of four writes and still store on
every read; they cannot reach this bar. Only **optimistic, version-validated
reads** (readers write nothing) do. That is what LeanStore's Optimistic Lock
Coupling achieves and is the design we adopt.

## 2. Principles

1. **Write-free reads of hot pages.** Reads of resident root/internal pages
   take no latch share-count, bump no pin count, and write no LRU field. They
   validate a version instead.
2. **Exploit the structure instead of generalizing.** The hot pages are the
   B-tree root and upper internal nodes: a tiny fraction of all pages, almost
   never evicted, almost never modified. We make exactly those pages cheap and
   leave leaves on the existing path. This sidesteps the hardest LeanStore
   prerequisite (general epoch reclamation) — see §5.
3. **One substrate for everything.** A single tagged child pointer (the *swip*)
   carries replacement state (hot/cool/cold), residency, and async-I/O state
   (in-flight). Replacement, scan resistance, prefetch, and the descent fast
   path all read the same word. Build it once.
4. **Measure each stage** on the 24-core box before/after; do not enable a
   default-on change without the full TCL regression.

## 3. The swip (tagged `roff_t`)

A *swip* is a child reference stored by a parent internal frame. BDB buffer
frames are aligned, so the low bits of a region offset (`roff_t`) are free
(3 with 8-byte alignment, 6 with cache-line alignment). We tag them exactly as
LeanStore tags its pointer:

| state | low bits | payload (high bits) | descent action |
|---|---|---|---|
| `COLD` | `00` | — (child is the on-disk `pgno` in the page image) | normal `__memp_fget` path; optionally issue prefetch |
| `HOT` | `01` | `roff_t` of the buffer frame | follow directly, read under LSN validation — **pure read** |
| `COOL` | `10` | `roff_t` of the buffer frame | follow; reheat to `HOT` (a rare write, only at the hot/cold boundary) |
| `IN_FLIGHT` | `11` | `roff_t` of the reserved frame | async read outstanding — wait on the in-transit latch |

Two BDB-specific constraints shape where the swip lives:

1. **Multi-process shared region.** A raw swizzled pointer is valid in only one
   address space. The swip is therefore a **`roff_t`**, resolved with
   `R_ADDR(region, off)` (base + offset). This works for both the >300-core
   single-process-multithreaded target and BDB's classic multi-process mode.
   Raw-pointer swizzling stays available later as a single-process-only fast
   path (Stage 3), not now.
2. **Pages are persisted images.** A btree internal page stores child *page
   numbers* on disk (`BINTERNAL.pgno`, `bt_search.c:476`), and that image lives
   in the buffer. We must not overwrite a child slot with a swip or we would
   persist a transient pointer. So the swip lives in an **in-memory shadow
   vector** attached to the internal frame (parallel to the page's child slots),
   allocated lazily at wire time near `mp_region.c:289`, not in the page image.

Reads of a `HOT` swip are pure loads (Shared-state, replicated per socket).
Writes happen only on rare transitions (swizzle/unswizzle, hot↔cool, I/O). The
root and upper internals stay `HOT`, so they are never written on the hot path.

## 4. Replacement = the hot/cool/cold state machine (Stage 0)

Today every `__memp_fput` writes `bhp->priority` (`mp_fput.c:215`), bumps a
global counter (`mp_fput.c:269-271`), and a periodic `__memp_reset_lru`
(`mp_fput.c:279-331`) sweeps the whole cache. That is a shared write per access
and an O(cache) sweep. Replace it with a clock / second-chance scheme whose
state is the swip's 2 bits (plus a per-frame referenced bit in `flags`):

- **Read of a HOT page**: at most set a "referenced" bit, and only with a plain
  store *if currently clear* (read-first, so the steady-state hot read is a pure
  read). No global counter, no per-access priority write.
- **Eviction** (`__memp_alloc`, clock hand `c_mp->last_checked` at
  `mp_alloc.c:182`): the hand demotes `HOT`→`COOL` (clears referenced / moves to
  a cooling FIFO) and evicts `COOL` pages whose referenced bit is still clear.
  Touching a `COOL` page reheats it to `HOT` (second chance).
- **Scan resistance** (the property you want): bulk-scanned pages enter `COOL`
  and cycle out via the cooling hand without displacing the `HOT` working set —
  the swip state *is* the scan-resistance mechanism, as in LeanStore. The hot
  working set (root/upper internals) stays `HOT` and is never churned.

This removes write #4 from the read path and deletes the global-counter +
`__memp_reset_lru` machinery. It is independent of the descent/AIO work and is
the foundation the swip sits on, so it ships first.

Invariant preserved: eviction still requires `BH_REFCOUNT == 0` and the buffer
latch exclusive (`mp_alloc.c:485/489/498`). We only change *which* victim is
chosen and *how* recency is tracked, never the safety handshake.

## 5. Optimistic descent + `BH_WIRED` (Stage 1)

`__bam_search` descends parent→child, latch-coupling: it fetches the child
(`bt_search.c:806`), then releases the parent (`bt_search.c:810`). Internal vs
leaf is `TYPE(h) == P_LBTREE` (`bt_search.c:348`; `P_IBTREE=3`, `P_LBTREE=5`).

**`BH_WIRED` (`0x200`, first free flag bit, `mp.h:552`).** When the btree
fetches a `P_IBTREE` page, mark its buffer wired; the evictor skips wired
buffers (`mp_alloc.c:320` and `:485`). Because internal nodes are <1% of pages
and always hot, the memory cost is negligible and they would never be evicted
anyway. **Wiring converts "safely reclaim arbitrary pages" (general epoch/RCU)
into "these specific pages are never freed"** — so an optimistic reader of a
wired internal page can never dereference freed memory. This is the trick that
avoids boiling the ocean.

**LSN-validated optimistic read.** At the child-pointer read (`bt_search.c:476`)
of a wired internal parent:
1. snapshot `LSN(h)` (page LSN, byte 0 of the page, updated under the page's
   exclusive latch on every structural change),
2. read/binary-search the node *without* the pin/latch,
3. re-read `LSN(h)`; if unchanged, the read was consistent — follow the child
   swip; if it moved (a rare split/merge), fall back to the existing
   pinned+latched path and retry.

For a wired parent we skip the child `__memp_fget` pin and the parent
`__memp_fput` (`bt_search.c:810`). The child swip (the `roff_t` of the resident
child frame) is cached in the parent's shadow vector at `:476`; a `HOT` swip is
followed with no hash lookup and no bucket latch — removing writes #1, #2, #3 on
the internal levels. Leaves keep the normal pin+latch (numerous, evictable, not
a single hot line).

**Gating (from the survey).**
- Only on `mfp->multiversion == 0` (mirror `__bam_get_root` at `:188/226`); the
  MVCC version-chain visibility walk (`mp_fget.c:264`) must not be bypassed.
- `BH_WIRED` mutually exclusive with `BH_FROZEN`/thaw.
- Binary search over the node must tolerate transiently inconsistent bytes
  (it is validated by the post-read LSN re-check; never act on a result that
  fails revalidation).
- Swips are invalidated on `__bam_split` / `__bam_pinsert`, keyed off the parent
  LSN change the optimistic reader already checks.

## 6. Async I/O, prefetch, trickle (Stage 2)

All data-page I/O funnels through one synchronous choke point: `__os_io`
(`os/os_rw.c:20`, `pread`/`pwrite`). Read-in is `__memp_pgread`
(`mp_bh.c:298`); writeback is `__memp_pgwrite` (`mp_bh.c:469`) via
`__memp_bhwrite`, reached from eviction (`mp_alloc.c:509`), checkpoint/sync
(`mp_sync.c:562`), and trickle (`mp_trickle.c`). There is no prefetch/readahead
today (no `fadvise`/`madvise` anywhere).

**`os_aio` interface** (new, in the `os/` layer; sketch in `research/os-aio.md`
and `src/dbinc/os_aio.h`): per-ENV context with `submit_read`, `submit_write`,
`reap`/poll, `cancel`; completion cookie `{BH*, DB_MPOOL_HASH*, MPOOLFILE*}` so
the completion handler runs the existing `pgread`/`pgwrite` tail. Backends:

- Linux **io_uring** (prep/submit, reap CQEs; pairs with `O_DIRECT`),
- BSD/macOS **kqueue + aio** (`aiocb`, `EVFILT_AIO`),
- Windows **IOCP** (requires reopening files `FILE_FLAG_OVERLAPPED` —
  `os_windows/os_open.c` does not today),
- POSIX **aio** fallback; `j_aio_*` test hooks mirroring `j_pread`/`j_pwrite`.

**Prefetch** transitions `COLD → IN_FLIGHT` and submits an async read at the
miss site (`mp_fget.c:806-865`, where the buffer is allocated and marked
`BH_TRASH`); the `IN_FLIGHT` swip lets concurrent accessors find the reserved
frame instead of re-issuing. A cursor/scan hint drives readahead.

**Trickle writeback** is the safest first async target: background flush of
cool/dirty pages so foreground eviction rarely stalls on a write. Split
`__memp_pgwrite` into **prep** (WAL flush + `pgout`, keep `mp_bh.c:347-405`
verbatim) → **submit** (replace `__os_io` at `:469` with `os_aio_submit_write`)
→ **finish** (move the `BH_DIRTY` clear + `hash_page_dirty` decrement,
`mp_bh.c:480-507`, into the completion handler).

**WAL rule** (must precede every async write): the per-page `__log_flush` keyed
on `mfp->lsn_off` (`mp_bh.c:347-354`) and the batch flush (`mp_sync.c:464-473`)
stay in the *prep* step, before submission. Non-negotiable.

**Per-process constraint** (critical, from the survey): AIO contexts are
per-process but the mpool region is shared, so a page marked in-flight by one
process cannot be reaped by another. Resolution: **only the process holding
`bhp->mtx_buf` exclusive submits and reaps**, releasing the latch in its own
completion handler. Other processes use the existing in-transit wait — block on
`bhp->mtx_buf`, then re-check `BH_TRASH` (`mp_fget.c:309-329`). Crash with an op
in flight reuses the existing `mtx_buf` recovery on `DB_RECOVER`. This means the
high-core single-process target gets full async; multi-process degrades
gracefully to today's behaviour.

## 7. Stages, exit criteria, ordering

The stages are a dependency chain; each is a separate branch/PR, measured on the
24-core box.

| stage | branch | delivers | exit criteria |
|---|---|---|---|
| **0** | `perf/swip-stage0-cooling` | clock/cool replacement; delete per-access `priority` write + global counter + `__memp_reset_lru`; referenced bit in `flags` | TCL regression green; eviction quality not worse on a cache-pressure workload; `wrand`/`rrand` no regression; **scan-resistance** shown (a scan doesn't evict the hot set) |
| **1** | `perf/swip-stage1-descent` | `BH_WIRED`; shadow swip vector (`roff_t` + 2 bits); LSN-validated optimistic descent of wired internals | TCL regression green; **read ceiling lifts** on `rrand`/`snap` at 8–24 threads vs master; correctness under concurrent split (stress test) |
| **2** | `perf/swip-stage2-aio` | `os_aio` interface + 1 backend (io_uring first); async trickle writeback; prefetch hint | TCL green; trickle keeps dirty-eviction stalls down under write load; prefetch improves cold-scan latency; multi-process falls back correctly |
| **3** | (later, if needed) | general pointer swizzling + epoch reclamation + raw-pointer single-process fast path | only if profiling shows the wired-internals scope leaves gains on the table — **measured: it does not** (see Stage 1c in Status below), so not planned |

### Status (what shipped vs. deferred)

Stage 1 shipped in a deliberately narrowed form, and two pieces of the original
design were intentionally **not** built:

- **Stage 0 (cooling)** — shipped (PR #21).
- **Stage 1 (descent)** — shipped as PR #22, but as *option B: root snapshot
  only*. Rather than the full **shadow swip vector** (tagged `roff_t` child
  pointers cached on every internal node, walked lock-free up the whole
  internal path), it caches a private copy of just the **single root page** per
  handle and does the LSN-validated lock-free read only for the
  root→first-child step. The root is the one page every descent touches, so
  this captured most of the available win cheaply: measured **+12–21%** on
  cached reads at 4–24 threads.
- **Stage 1c (built, measured, dropped)** — extending optimistic descent to
  *deeper* internal pages. **Built and measured; it is a wash and was not
  landed.** The original plan above feared Stage 1c required *general epoch
  reclamation*, but in fact the existing `BH_WIRED` mechanism is sufficient: a
  wired internal page is non-evictable, so a lock-free reader can hold a raw
  frame pointer to it and validate with an LSN re-check (`__db_free` clears the
  wired bit before reuse, and frees are logged so the LSN changes). The
  implementation (`__memp_fget_wired` + a latch-free `__bam_search_opt`
  descent, ~350 lines) was correct — it passed the full TCL regression
  including recovery and multi-process, plus a value-verified concurrent
  reader/writer stress with a clean `db_verify` under maximal split/merge
  churn. But the A/B on the 12-physical-core box showed **no throughput change**
  (master vs Stage 1c both ~500K ops/s at 8t, both declining past 12t), and the
  cycles profile was unchanged. The reason: on a realistic tree the height is
  ~3 (root → one internal level → leaves). Stage 1b already removed the root
  pin; Stage 1c removes the *single* internal-level pin (≈1% of cycles). The
  dominant read-path cost is the **leaf** `__memp_fget` pin, the **per-key leaf
  `__db_lget` page read-lock**, and **cursor allocation churn**
  (`__db_cursor_int`/`__bamc_refresh`) — none of which Stage 1c touches.
  Internal-page latching is simply a negligible cost on shallow trees. Stage 1c
  would only matter on very deep trees (very large keys / many levels), which
  is not the common case, so it is dropped (the branch is kept only as a
  documented negative result, like the cursor-shard experiment).
- **The 2-bit tagged swip itself** (the unifying hot/cool/cold + residency +
  in-flight substrate, §2.3) was **not** realized: each shipped stage used its
  own ad-hoc mechanism (a `wired` byte, a root-page copy) instead of the single
  tagged word. With Stage 1c dropped, the unified substrate has no remaining
  consumer and is not planned.

Net: the cheap, high-value piece (root snapshot, Stage 1b) shipped and is
validated (+12–21% on cached reads). Stage 1c (deeper internals) was built and
measured rather than assumed — and proved to be a wash, so it is dropped. The
next read-path bottleneck is now identified by measurement: the per-key leaf
page read-lock (`__db_lget`), the leaf buffer pin, and cursor-object churn —
not the internal-page descent. Re-evaluating any of this at very high core
counts still wants a true many-physical-core / multi-socket NUMA box, since the
12-physical-core box ceilings around 8–12 threads regardless of software.

## 8. Correctness invariants (the parts that must not be wrong)

1. Hot pages stay `HOT`, so their swips are read-only on the descent; cooling
   churns only the boundary.
2. Every structural modification bumps the page LSN under the page's exclusive
   latch *before* the change is visible; the optimistic reader's LSN re-check
   brackets every read of node contents and never acts on an unvalidated result.
3. Eviction's `refcount == 0` + exclusive-`mtx_buf` handshake is unchanged.
4. `BH_WIRED` excludes `BH_FROZEN`; optimistic fast path gated on
   `mfp->multiversion == 0`.
5. Async writes obey the WAL rule in the prep step; only the latch-holding
   process reaps its own AIO; swips are never persisted to the page image.

## 9. Measurement plan

Re-run `lab/bench/scale_bench {rrand,sepdb,snap} 200000 3 1 2 4 8 12 16 24` plus
a cache-pressure workload and a write/trickle workload, on the 24-core Linux box
(`meh`) — the laptop is too noisy (asymmetric cores) to resolve these deltas.
Each stage records before/after medians and a `perf` self-time delta in
`scaling-findings.md`. Stage 1's headline metric: does the 8-thread ceiling lift
and the `futex`/atomic self-time collapse.

## 10. Open questions

- Shadow swip vector lifetime vs `__bam` page reorganization — invalidate vs
  rebuild on split/merge (lean toward invalidate-on-LSN-change).
- Whether to wire only levels ≥1 (internals) or also a configurable number of
  upper levels under memory accounting.
- io_uring + `O_DIRECT` alignment interaction with BDB page sizes and the
  existing `DB_OSO_DIRECT` path.
- Interaction of trickle/group-commit (#3 in the roadmap) — the async writer and
  the WAL group-commit should share the log-flush coordination.

## 11. Reference implementations (sqlxtc, noxu) and refinements

Two sibling projects implement LeanStore/Umbra-style cooling buffer managers;
studying them validates this design and sharpens three points.

**sqlxtc `bufmgr.c`** (libxtc `examples/06_sqlxtc`): frame states
`FREE/HOT/COOL/LOADED/WRITING`; the eviction state lives in the parent **swip**
and transitions are owned by whoever wins a CAS (loser retries) — exactly the
tagged-swip model here. Its `evict_one` clock sweep is the key:

- **Probationary admission + COOL-first eviction.** New pages are admitted
  `COOL`. The sweep reclaims an already-`COOL` frame and **only cools a `HOT`
  frame when a full sweep finds no `COOL` victim** (`force_cool`). Because a
  scan keeps supplying `COOL` pages, *the hot set is never cooled to make room
  for a scan* — robust scan resistance for a scan of any length. A `COOL` page
  carries a CLOCK `ref` bit (set on access, cleared by the sweep) so a
  re-touched cool page survives one sweep and is promoted back to `HOT`.
- **Single-word pin/evict gate.** `pin >= 0` ⇒ a fixer may `CAS pin→pin+1`;
  the evictor reserves an unpinned frame with `CAS 0→-1` (`-1` = EVICTING).
  Acquiring a pin and reserving for eviction race on **one** atomic word, with
  no separate "pin++ then re-check" window.
- **prefer-clean foreground + background trickle.** Foreground eviction
  reclaims a clean victim and leaves dirty `COOL` pages for the trickler; it
  flushes a dirty page inline only as a last-resort progress guarantee.

**noxu `noxu-evictor`** (Berkeley-DB-JE lineage): a per-operation **`CacheMode`**
(`Default/Unchanged/EvictLn/EvictBin/KeepHot/MakeEvictable`) drives **two
independent tracking sets** — `primary` and `scan_resistant` — and the evictor
drains *scan → primary → dirty* with per-phase quotas. Pluggable LRU/Clock/ARC/
CAR/LIRS; it notes LRU pollutes on scans while ARC/CAR/LIRS resist inherently.

### What this changes here

1. **Scan resistance is probationary admission + COOL-first, not a single
   counter.** Stage 0's frequency-climb CLOCK ages *every* buffer the hand
   passes, so a long scan still decays the hot set (measured: only ~19% fewer
   hot-set page-ins than LRU). The fix is to **admit new/scan-read pages cool
   and never cool a HOT buffer while a COOL victim exists** — then a scan of any
   length leaves the hot set untouched. This is implementable in BDB's existing
   bucket-scan evictor *without* the full swip (call it **Stage 0.5**): split
   the warmth range into COOL/HOT bands, admit reads at the top of COOL, promote
   to HOT only on re-reference, and only decrement HOT-band warmth when a full
   sweep finds no COOL victim (BDB's existing `aggressive` escalation is the
   hook). Robust scan resistance, no optimistic-descent risk.
2. **Adopt the single-word pin/evict reservation** on `bhp->ref`
   (`-1` = EVICTING) to replace `__memp_alloc`'s TRYLOCK-`mtx_buf`-then-recheck-
   `ref==1` dance, removing that race window.
3. **prefer-clean eviction + trickle** is the Stage 2 writeback design; confirm
   foreground eviction never blocks on a device write while a clean COOL victim
   exists.

### BDB constraints (why we adopt principles, not code)

Both references are greenfield, single-address-space. BDB's mpool is
multi-process (swips must be `roff_t`, resolved via `R_ADDR`), pages are
persisted disk images (the swip lives in an in-memory shadow vector, never in
the page), and the on-disk format + public API are fixed. So we take the
*mechanisms* — probationary COOL admission, COOL-first eviction, swip-encoded
state, single-word pin/evict, prefer-clean+trickle — within those constraints.
