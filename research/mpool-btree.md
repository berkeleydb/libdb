# MPOOL / BTREE implementation-grounding report

Read-only survey of `master` for a buffer-manager rework (Stage 0: cooling
state replacing the per-access priority write; Stage 1: `BH_WIRED` +
per-parent child swip + LSN-validated optimistic descent of wired internal
pages). Every line number below was read from the working tree at survey time.

> **Two premises in the request do not exist in this tree — flag before relying on them:**
> 1. `MPOOL_HOTFIELDS_ISOLATED` — **no such guard, macro, or `#ifdef` anywhere**
>    (`grep` across the whole repo returns nothing). `struct __bh` in
>    `src/dbinc/mp.h` is a plain, unguarded, unpadded `/* SHARED */` struct.
>    There is no cache-line isolation of the hot fields (`ref`, `flags`,
>    `priority`) today; they share a line with `mtx_buf`, `hq`, `pgno`.
> 2. `__bam_iget` — **does not exist**. The btree descent entry points are
>    `__bam_get_root` (root fetch) and `__bam_search` (root→leaf walk) in
>    `src/btree/bt_search.c`. The report uses those.

---

## 1. MPOOL pin / unpin

### 1a. Pin — `__memp_fget`, `src/mp/mp_fget.c:103`

The resident-page fast path:

**Bucket latch (read lock on `hp->mtx_hash`)** is taken inside the
`MP_GET_BUCKET` macro at `src/mp/mp_fget.c:245`:

```c
245:	MP_GET_BUCKET(env, mfp, *pgnoaddr, &infop, hp, bucket, ret);
246:	if (ret != 0)
247:		return (ret);
248:	c_mp = infop->primary;
```

The macro body (`src/dbinc/mp.h`, `MP_GET_BUCKET`) computes the bucket and does
`MUTEX_READLOCK(env, (hp)->mtx_hash)` for the single-region case, else calls
`__memp_get_bucket`. After this, `h_locked = 1` (set at line 257).

**Chain walk reading `bhp->pgno` / `bhp->mf_offset`**, `src/mp/mp_fget.c:256`:

```c
256:	st_hsearch = 0;
257:	h_locked = 1;
258:	SH_TAILQ_FOREACH(bhp, &hp->hash_bucket, hq, __bh) {
259:		++st_hsearch;
260:		if (bhp->pgno != *pgnoaddr || bhp->mf_offset != mf_offset)
261:			continue;
```

(For snapshot reads the MVCC version chain is walked backward at lines
264–278 via `SH_CHAIN_PREV(bhp, vc, __bh)` — see hazards §6.)

**`atomic_inc(&bhp->ref)` (the pin)**, `src/mp/mp_fget.c:288`:

```c
288:		if (BH_REFCOUNT(bhp) == UINT16_MAX) {     /* overflow guard */
...
296:		atomic_inc(env, &bhp->ref);
297:		b_incr = 1;
```

**Drop bucket latch, then take the buffer latch.** Hash mutex is released
*before* the buffer mutex (lock-ordering: hash → buf), `src/mp/mp_fget.c:304`:

```c
304:		MUTEX_UNLOCK(env, hp->mtx_hash);
305:		h_locked = 0;
306:		if (dirty || extending || makecopy || F_ISSET(bhp, BH_FROZEN)) {
307: xlatch:		if (LF_ISSET(DB_MPOOL_TRY)) {
308:				if ((ret =
309:				    MUTEX_TRYLOCK(env, bhp->mtx_buf)) != 0)
310:					goto err;
311:			} else
312:				MUTEX_LOCK(env, bhp->mtx_buf);
313:			F_SET(bhp, BH_EXCLUSIVE);
314:		} else if (LF_ISSET(DB_MPOOL_TRY)) {
315:			if ((ret = MUTEX_TRY_READLOCK(env, bhp->mtx_buf)) != 0)
316:				goto err;
317:		} else
318:			MUTEX_READLOCK(env, bhp->mtx_buf);
```

The clean read fast path is line 318 (`MUTEX_READLOCK(env, bhp->mtx_buf)`);
`b_lock = 1` at line 333. Note: without `HAVE_SHARED_LATCHES`,
`F_SET(bhp, BH_EXCLUSIVE)` is unconditional at line 330.

The pin is recorded in the thread's `PIN_LIST` at lines 866–905 (`lp->b_ref =
R_OFFSET(infop, bhp)`), and the page pointer returned at line 933
(`*(void **)addrp = bhp->buf;`).

### 1b. Unpin — `__memp_fput`, `src/mp/mp_fput.c:65`

**Multi-reference early-out** (`atomic_dec` returning >1 keeps priority
untouched), `src/mp/mp_fput.c:183`:

```c
183:	DB_ASSERT(env, atomic_read(&bhp->ref) != 0);
184:	if (atomic_dec(env, &bhp->ref) > 1 || (atomic_read(&bhp->ref) == 1 &&
185:	    !F_ISSET(bhp, BH_DIRTY))) {
186:		if (F_ISSET(bhp, BH_EXCLUSIVE))
187:			F_CLR(bhp, BH_EXCLUSIVE);
188:		MUTEX_UNLOCK(env, bhp->mtx_buf);
189:		return (0);
190:	}
```

**The LRU bump (the per-access `bhp->priority` write — Stage 0's target)**,
`src/mp/mp_fput.c:206`:

```c
206:	if (priority == DB_PRIORITY_VERY_LOW ||
207:	    mfp->priority == MPOOL_PRI_VERY_LOW)
208:		bhp->priority = 0;
209:	else {
...
215:		bhp->priority = c_mp->lru_priority;
...                         /* per-priority adjust */
249:				bhp->priority += adjust;
...
253:	}
```

**Global clock advance + wraparound reset**, `src/mp/mp_fput.c:269`:

```c
269:	if (++c_mp->lru_priority >= MPOOL_LRU_REDZONE &&
270:	    (t_ret = __memp_reset_lru(env, infop)) != 0 && ret == 0)
271:		ret = t_ret;
```

`__memp_reset_lru` (`src/mp/mp_fput.c:279`) bulk-decrements every buffer's
priority when the counter saturates (lines 300, 325). This O(cache) sweep is
exactly what a clock hand makes unnecessary.

---

## 2. Replacement / eviction — `__memp_alloc`, `src/mp/mp_alloc.c:30`

### Victim selection
`__memp_alloc` first tries `__env_alloc` from free memory (`alloc:` label,
line ~95). On failure it falls to `search:` (line 149) and scans buckets.

**High-priority threshold** (buffers fresher than `lru_priority -
pages/10` are skipped until aggressive), `src/mp/mp_alloc.c:155`:

```c
155:	cache_reduction = c_mp->pages / 10;
156:	high_priority = aggressive ? MPOOL_LRU_MAX :
157:	    c_mp->lru_priority - cache_reduction;
158:	lru_generation = c_mp->lru_generation;
```

**Bucket scan with wraparound** via `c_mp->last_checked`,
`src/mp/mp_alloc.c:182`:

```c
182:		hp = &dbht[c_mp->last_checked++];
183:		if (hp >= hp_end) {
184:			c_mp->last_checked = 0;
185:			hp = &dbht[c_mp->last_checked++];
186:		}
```

After `MPOOL_ALLOC_SEARCH_LIMIT` (500) buckets, or a full pass, `aggressive`
is raised and `high_priority = MPOOL_LRU_MAX` (lines 261, 297) so every buffer
becomes eligible.

**Per-bucket lowest-priority pick** (`bhp->priority` is the LRU key),
`src/mp/mp_alloc.c:303`:

```c
303: retry_search:	bhp = NULL;
304:		bucket_priority = high_priority;
...
320:			if (SH_CHAIN_SINGLETON(current_bhp, vc)) {
321:				if (BH_REFCOUNT(current_bhp) != 0)
322:					continue;            /* skip pinned */
323:				buffers++;
324:				if (bucket_priority > current_bhp->priority) {
325:					bucket_priority = current_bhp->priority;
...
328:					bhp = current_bhp;
329:					atomic_inc(env, &bhp->ref);
330:				}
331:				continue;
332:			}
```

Two buckets are compared and the lower-priority winner kept
(`priority = bhp->priority;` at line 409; swap logic 420–447).

### `BH_REFCOUNT==0` check and exclusive `mtx_buf`
Eviction will not touch a pinned buffer. After dropping the hash mutex
(`src/mp/mp_alloc.c:481`):

```c
484:		/* Don't bother trying to latch a busy buffer. */
485:		if (BH_REFCOUNT(bhp) > 1)
486:			goto next_hb;
...
489:		if ((ret = MUTEX_TRYLOCK(env, bhp->mtx_buf)) != 0) {  /* exclusive */
...
493:		F_SET(bhp, BH_EXCLUSIVE);
494:		b_lock = 1;
497:		/* Someone may have grabbed it while we got the lock. */
498:		if (BH_REFCOUNT(bhp) != 1)
499:			goto next_hb;
```

So the invariant is: an evictable buffer must have `ref == 1` (the evictor's
own transient inc) and be exclusively latched via non-blocking `MUTEX_TRYLOCK`.
Dirty victims are written first (`F_ISSET(bhp, BH_DIRTY)` →
`__memp_bhwrite`, lines 507–525); MVCC mid-chain victims are frozen
(`SH_CHAIN_HASPREV` → `__memp_bh_freeze`, lines 540–566). A same-size clean
victim is reused in place (`p = bhp; goto found;`, lines 660–666).

### How a clock / second-chance / hot–cool state machine slots in
- **State storage:** reuse the 32-bit `bhp->priority` (`src/dbinc/mp.h:555`) or
  steal bits from `bhp->flags` (`u_int16_t`, `src/dbinc/mp.h:553`, only 9 bits
  used through `BH_THAWED 0x100`). A 2-bit HOT/WARM/COOL/COLD field fits in
  `flags`.
- **Reference bit set on access:** replace the `bhp->priority = c_mp->lru_priority`
  write at `src/mp/mp_fput.c:215` (and the `DB_PRIORITY_*` adjust block 209–253)
  with a single `F_SET(bhp, BH_HOT)` / set-reference-bit — cheaper, and it
  removes the `++c_mp->lru_priority` global counter contention (line 269) and
  the whole `__memp_reset_lru` sweep (lines 279–331).
- **Evictor consults the state bit / advances the cool hand:** the victim test
  at `src/mp/mp_alloc.c:320–332`. Instead of "lowest priority wins," do
  second-chance: if `BH_HOT` set, clear it (demote one level) and `continue`;
  only a COLD, `BH_REFCOUNT==0` buffer becomes `bhp`. `c_mp->last_checked`
  (line 182) already *is* a per-region clock hand — reuse it as the cooling
  hand. The `aggressive`/`high_priority` escalation (155–157, 261) maps to
  "force-demote on the second sweep."

---

## 3. BH struct + flags — `src/dbinc/mp.h`

`struct __bh` is `/* SHARED */` (cross-process), `src/dbinc/mp.h:539`:

```c
539: struct __bh { /* SHARED */
540:	db_mutex_t	mtx_buf;	/* Shared/Exclusive mutex */
541:	db_atomic_t	ref;		/* Reference count. */
542:#define	BH_REFCOUNT(bhp)	atomic_read(&(bhp)->ref)
544:#define	BH_CALLPGIN	0x001
545:#define	BH_DIRTY	0x002
546:#define	BH_DIRTY_CREATE	0x004
547:#define	BH_DISCARD	0x008
548:#define	BH_EXCLUSIVE	0x010
549:#define	BH_FREED	0x020
550:#define	BH_FROZEN	0x040
551:#define	BH_TRASH	0x080
552:#define	BH_THAWED	0x100
553:	u_int16_t	flags;
554:
555:	u_int32_t	priority;	/* Priority. */
556:	SH_TAILQ_ENTRY	hq;		/* MPOOL hash bucket queue. */
557:
558:	db_pgno_t	pgno;
559:	roff_t		mf_offset;
560:	u_int32_t	bucket;
561:	int		region;
562:
563:	roff_t		td_off;		/* MVCC: creating TXN_DETAIL offset. */
564:	SH_CHAIN_ENTRY	vc;		/* MVCC: version chain. */
...
571:	DB_ALIGN8	u_int8_t buf[1];	/* Variable length data. */
572: };
```

- **No `MPOOL_HOTFIELDS_ISOLATED` guard exists** (confirmed by grep). If the
  rework needs cache-line isolation of `ref`/`flags`/`priority` to avoid false
  sharing of the new reference/cool bit, that padding does **not** exist yet and
  must be added here.
- **`BH_WIRED` (new, non-evictable) flag:** next free bit is `0x200`
  (`#define BH_WIRED 0x200`) — `flags` is `u_int16_t`, room through `0x8000`.
- **Where the evictor checks `BH_WIRED`:** add `if (F_ISSET(current_bhp,
  BH_WIRED)) continue;` in the victim loop at `src/mp/mp_alloc.c:320`
  (singleton branch) and at the MVCC-chain branch ~line 350, alongside the
  existing `BH_REFCOUNT != 0` skip. Belt-and-suspenders: also short-circuit at
  `src/mp/mp_alloc.c:485` before the `MUTEX_TRYLOCK`.
- **Where wired internal pages are marked on fetch:** in `__memp_fget` after
  the buffer is pinned and latched, just before the return at
  `src/mp/mp_fget.c:933`. But the *caller* knows page type, not mpool — so the
  natural mark point is the btree layer once `TYPE(h) == P_IBTREE` is known
  (see §4). A new mpool entry point (e.g. `__memp_wire(bhp)` setting `BH_WIRED`
  under `bhp->mtx_buf`) called from `__bam_get_root`/`__bam_search` after the
  internal page fget is the clean seam.

---

## 4. BTREE descent — `src/btree/bt_search.c`

Entry points (note: **`__bam_iget` does not exist**):
- `__bam_get_root`, `src/btree/bt_search.c:59` — fetches the root via the
  `BAM_GET_ROOT` macro at line 114, asserts `TYPE(h)` is one of
  `P_IBTREE/P_IRECNO/P_LBTREE/P_LRECNO/P_LDUP` at lines 123–124.
- `__bam_search`, `src/btree/bt_search.c:253` — the root→leaf descent loop.

### The descent loop — `src/btree/bt_search.c:347`
```c
347:	for (;;) {
348:		if (TYPE(h) == P_LBTREE)        /* leaf vs internal distinction */
349:			adjust = P_INDX;
350:		else { ... adjust = O_INDX; }
...
        /* binary search on h ... then for an internal page: */
476:			pg = GET_BINTERNAL(dbp, h, indx)->pgno;   /* child pgno from parent */
477:			level = LEVEL(h);
...
601:				parent_h = h;     /* normal search: latch-couple parent→child */
602:				goto lock_next;
...
803: skip_lock:	stack = set_stack;
804:		}
805:		/* Get the child page. */
806:		if ((ret = __memp_fget(mpf, &pg,
807:		     dbc->thread_info, dbc->txn, get_mode, &h)) != 0)
808:			goto err;
809:		/* Release the parent. */
810:		if (parent_h != NULL && (ret = __memp_fput(mpf,
811:		    dbc->thread_info, parent_h, dbc->priority)) != 0)
812:			goto err;
813:		parent_h = NULL;
814:	}
```

So per level: read child pgno from parent (`GET_BINTERNAL(...)->pgno`, line
476) → fetch+pin+latch child (`__memp_fget`, lines 806–807) → unpin parent
(`__memp_fput`, lines 810–811). Latch-coupling: the parent stays pinned
(`parent_h = h`, line 601) across the child fetch.

### Internal vs leaf, and the page LSN
- Page type: `P_IBTREE = 3` (`src/dbinc/db_page.h:39`), `P_LBTREE = 5`
  (`src/dbinc/db_page.h:41`); read via `TYPE(p)` macro (`db_page.h:317`),
  `LEVEL(p)` (`db_page.h:316`), `LEAFLEVEL == 1` (`db_page.h`).
- LSN lives at the head of every `PAGE`: `DB_LSN lsn;` is bytes 00–07
  (`src/dbinc/db_page.h:260`), accessed via `LSN(p)` (`db_page.h:310`). It is
  stamped under exclusive page latch + log write by the access-method redo/undo
  paths (e.g. `__db_*_log` / `__memp_dirty` callers), **not** by `__memp_fget`.
  For a reader, `LSN(h)` is stable as long as the page is latched.
- Child entry record: `BINTERNAL` (`src/dbinc/db_page.h:784–791`), `pgno` at
  offset 04–07; fetched by `GET_BINTERNAL` (`db_page.h:795`).

### Stage 1 insertion point — optimistic LSN-validated descent of a wired internal page
The exact seam is the top of the descent loop body, **between** reading the
child pointer and the `__memp_fget` of the child:

- Replace the sequence at `src/btree/bt_search.c:476` (`pg =
  GET_BINTERNAL(...)->pgno`) → `806` (`__memp_fget(child)`) → `810`
  (`__memp_fput(parent)`) with: if the current `h` is a wired internal page
  (`F_ISSET(PAGE_TO_BH(h), BH_WIRED)`), read `pg` and snapshot `lsn = LSN(h)`
  **without** holding the buffer latch, do the binary search against the
  unlatched (but wired, hence non-evictable, non-relocating) frame, then
  re-validate `LOG_COMPARE(&lsn, &LSN(h)) == 0` before trusting `pg`. On
  mismatch, fall back to the latched path (current lines 806–811). Because
  `BH_WIRED` guarantees the frame is not reused by `__memp_alloc` (§2/§3), the
  pointer stays valid; the LSN check guards against in-place content mutation
  (split/merge) of the internal page.
- The parent `__memp_fput` at lines 810–811 is skipped for wired parents (no
  pin was taken), which is the whole point — descent of the upper tree touches
  no `mtx_hash`, no `mtx_buf`, no `atomic_inc`.

### Where a per-parent child-swip cache attaches
`GET_BINTERNAL(dbp, h, indx)->pgno` (line 476) is a `db_pgno_t` (logical page
number) that must be re-hashed and re-looked-up each descent. A swip cache
turns it into a direct `roff_t` to the child `BH` with low state bits
(resolved/unresolved). Two candidate homes:
- **Shadow vector keyed by internal frame** (preferred, see §5): an array of
  `roff_t` swips parallel to the `BINTERNAL` slots of each wired internal page,
  consulted at line 476 before falling back to `__memp_fget`. Low 2–3 bits of
  the `roff_t` encode state (0 = unresolved → do the fget and fill the swip;
  1 = resolved offset; 2 = invalidated by split). Alignment of `BH`
  allocations (`MVCC_BHALIGN`, and `__env_alloc` granularity) leaves low bits
  free.
- It must be invalidated wherever the child set changes — `__bam_split` /
  `__bam_pinsert` and page-free paths — keyed off the parent page LSN bump.

---

## 5. Where the region stores per-buffer metadata — `src/mp/mp_region.c`

Per-region layout is built in `__memp_init` (`src/mp/mp_region.c:196`):

```c
286:	if ((ret = __env_alloc(infop,
287:	    htab_buckets * sizeof(DB_MPOOL_HASH), &htab)) != 0)
288:		goto mem_err;
289:	mp->htab = R_OFFSET(infop, htab);
...
339:	mp->htab_buckets = htab_buckets;
```

The cache region (`MPOOL`, `src/dbinc/mp.h:struct __mpool`) holds offsets, not
arrays: `roff_t htab` (hash buckets), `roff_t regids`, `roff_t ftab`, plus the
`free_frozen` / `alloc_frozen` lists. Individual `BH`s are *not* a contiguous
array — each is `__env_alloc`'d on demand inside `__memp_alloc`
(`src/mp/mp_alloc.c`, `alloc:` path, `c_mp->pages++`), so there is no existing
dense per-buffer index to hang a parallel vector off.

**For a per-internal-frame shadow swip vector:** allocate it in `__memp_init`
right after the htab allocation (after `src/mp/mp_region.c:289`), as another
`__env_alloc(infop, ...)` whose offset is stored in a new `roff_t` field on
`struct __mpool`. Because swips are *per wired internal frame* (not per cache
slot), the cleaner design is to allocate the swip vector lazily when a frame is
wired (alongside `BH_WIRED` marking, §3), sized to that page's `NUM_ENT`, and
store its `roff_t` either in a new `BH` field or in a small region-side hash
keyed by `bhp` offset. Both the htab and any new vector must be reachable by
`R_ADDR(infop, off)` from every attached process (cross-process — see §6).

---

## Concrete insertion points

### Stage 0 — cooling state replaces the per-access priority write
| What | File:line |
|------|-----------|
| Remove/replace the LRU priority write | `src/mp/mp_fput.c:215` (`bhp->priority = c_mp->lru_priority`) and the adjust block `209–253` → set a reference/HOT bit instead |
| Remove the global clock increment + redzone reset | `src/mp/mp_fput.c:269–271` (and delete the `__memp_reset_lru` sweep `279–331`) |
| State bits storage | `src/dbinc/mp.h:553` (`flags`, free bits ≥ `0x200`) or repurpose `priority` `mp.h:555` |
| Evictor second-chance / cooling check | `src/mp/mp_alloc.c:320–332` (singleton) and `~345–360` (MVCC chain); reuse `c_mp->last_checked` `mp_alloc.c:182` as the cool hand |
| Keep refcount==0 + TRYLOCK invariant unchanged | `src/mp/mp_alloc.c:485, 489, 498` |

### Stage 1 — `BH_WIRED` + swip + LSN-validated descent
| What | File:line |
|------|-----------|
| Define `BH_WIRED 0x200` | `src/dbinc/mp.h:552` (after `BH_THAWED`) |
| Evictor skips wired buffers | `src/mp/mp_alloc.c:320` (and `485`) — `if (F_ISSET(.., BH_WIRED)) continue;` |
| Mark internal page wired after fetch | `src/btree/bt_search.c` after the child `__memp_fget` at `806–807`, gated on `TYPE(h)==P_IBTREE`; via a new `__memp_wire()` touching `src/mp/mp_fget.c:933` region |
| Optimistic read seam | `src/btree/bt_search.c:476` (read `pg`+`LSN(h)`) … revalidate before/instead of `__memp_fget` at `806`; skip parent `__memp_fput` `810–811` for wired parents |
| Swip cache read/fill | at `src/btree/bt_search.c:476` (`GET_BINTERNAL(...)->pgno`) |
| Swip vector allocation | `src/mp/mp_region.c:289` (region) + new `roff_t` on `struct __mpool` in `src/dbinc/mp.h` |
| Swip invalidation | btree split/insert (`__bam_split`, `__bam_pinsert`) keyed off parent `LSN` |

---

## Correctness hazards

1. **Cross-process shared region.** `struct __bh`, `struct __mpool`,
   `DB_MPOOL_HASH` are all `/* SHARED */` and live in a region mapped by
   multiple processes at *different* virtual addresses — every reference is a
   `roff_t` + `R_ADDR`/`R_OFFSET`, never a pointer. A swip cache **must store
   `roff_t`, not `BH *`**, and any new state bits must be written atomically
   w.r.t. other processes (the existing code only relies on 32-bit reads/writes
   being atomic — see the `priority`/`lru_priority` "we don't lock, garbage is
   tolerable" comments at `mp_fput.c:210–214` and `mfp->priority` note in
   `mp.h`). A HOT/cool bit with relaxed semantics is fine; a swip that gates
   correctness is not — it needs the LSN revalidation as its correctness anchor.

2. **Optimistic descent vs latching.** Skipping `bhp->mtx_buf` on a wired
   internal page means giving up the latch that today guarantees a stable
   `LSN(h)` and stable `BINTERNAL` bytes during the binary search
   (`bt_search.c:347–476`). `BH_WIRED` only guarantees the *frame* is not
   evicted/reused; it does **not** stop an in-place split/merge from rewriting
   the page under you. The `LOG_COMPARE(&snapshot_lsn, &LSN(h))` recheck after
   the search is mandatory, and the binary-search code must tolerate transiently
   inconsistent bytes (no out-of-bounds `indx`) before the recheck rejects them.

3. **MVCC / version-chain interaction at `read_lsnp`.** `__memp_fget` resolves
   snapshot reads by walking the `vc` chain backward
   (`src/mp/mp_fget.c:264–278`, `BH_VISIBLE`/`SH_CHAIN_PREV`) to the version
   visible at `td->read_lsn` (set at `mp_fget.c:179`). A swip/LSN fast path that
   bypasses `__memp_fget` bypasses this visibility resolution — so it is only
   sound for **internal btree pages, which are not multi-versioned** under
   normal operation. Guard the optimistic path with
   `atomic_read(&mfp->multiversion) == 0` (as `__bam_get_root` already does at
   `bt_search.c:188, 226`) **or** restrict it to `P_IBTREE` frames that are
   provably singletons (`SH_CHAIN_SINGLETON(bhp, vc)`).

4. **Freeze / thaw.** Mid-chain MVCC victims are frozen
   (`__memp_bh_freeze`, `mp_alloc.c:545`) and re-materialized
   (`__memp_bh_thaw`); a frozen buffer's data is gone. `BH_WIRED` must be
   mutually exclusive with `BH_FROZEN` — the evictor's freeze branch
   (`mp_alloc.c:540–566`) and the fget thaw branches (`mp_fget.c:307, 360+`)
   must treat wired as "never freeze," else a swip could point at a frozen
   header whose `buf` is invalid. Wiring should also force `SH_CHAIN_SINGLETON`.

5. **`last_pgno` / extend races and `DB_MPOOL_NEW`.** The optimistic path reads
   a child `pgno` that a concurrent allocation could be creating
   (`mp_fget.c` `newpg:`/`extending`); the LSN recheck on the *parent* covers
   structural change, but the swip-fill must take the normal latched fget for
   any not-yet-resident child.

6. **`priority` is read locklessly during stats and reset.** Repurposing
   `priority` for cool-state must keep `__memp_reset_lru`'s assumptions or
   remove that function entirely; leaving both the old sweep (`mp_fput.c:279`)
   and a new state machine running would double-account.

---

## Summary (10 lines)

1. Pin fast path: `__memp_fget` (`mp_fget.c:103`) read-locks `hp->mtx_hash` (`:245`), walks the chain (`:258`), `atomic_inc(&bhp->ref)` (`:296`), drops hash lock (`:304`), `MUTEX_READLOCK(bhp->mtx_buf)` (`:318`).
2. Unpin: `__memp_fput` (`mp_fput.c:65`) `atomic_dec(&bhp->ref)` (`:184`) and the per-access LRU write `bhp->priority = c_mp->lru_priority` (`:215`) + global `++c_mp->lru_priority` (`:269`) → `__memp_reset_lru` sweep (`:279`).
3. Eviction: `__memp_alloc` (`mp_alloc.c:30`) clock-scans `last_checked` buckets (`:182`), picks lowest `priority` singleton (`:320–332`), requires `BH_REFCOUNT==1` (`:485,498`) + `MUTEX_TRYLOCK(bhp->mtx_buf)` exclusive (`:489`), writes dirty (`:507`), freezes MVCC mid-chain (`:545`).
4. `struct __bh` (`mp.h:539`) is plain/unguarded — **`MPOOL_HOTFIELDS_ISOLATED` does not exist**; `flags` (`:553`) has free bits, define `BH_WIRED 0x200` after `BH_THAWED` (`:552`).
5. Descent: **`__bam_iget` does not exist**; use `__bam_get_root` (`bt_search.c:59`) + `__bam_search` loop (`:347`); child pgno from `GET_BINTERNAL(...)->pgno` (`:476`), child `__memp_fget` (`:806`), parent `__memp_fput` (`:810`), latch-couple via `parent_h=h` (`:601`).
6. Page type/LSN: `P_IBTREE=3`/`P_LBTREE=5` (`db_page.h:39/41`), `TYPE()`/`LEVEL()` (`:316/317`), `LSN()` at PAGE byte 0 (`:260/310`); LSN stamped under exclusive latch by the AM, stable while latched.
7. Stage 0: replace `mp_fput.c:215` priority write with a HOT/reference bit, delete `:269` counter + `:279` sweep, make the evictor at `mp_alloc.c:320` do second-chance using `last_checked` (`:182`) as the cool hand.
8. Stage 1: mark `P_IBTREE` frames `BH_WIRED` after fget (seam near `bt_search.c:806`), skip them in the evictor (`mp_alloc.c:320,485`); at `bt_search.c:476` snapshot `LSN(h)`, search unlatched, revalidate `LOG_COMPARE(LSN(h))` before trusting `pg`; attach a `roff_t`-with-state swip vector allocated near `mp_region.c:289`.
9. Hazards: shared region forces `roff_t` swips not pointers; optimistic reads need the LSN recheck because `BH_WIRED` stops eviction but not in-place splits; MVCC `read_lsnp` visibility walk (`mp_fget.c:264`) must be bypassed only for non-multiversion singleton internal pages; `BH_WIRED` must exclude `BH_FROZEN`/thaw.
10. Region: `BH`s are `__env_alloc`'d on demand (no dense array); per-region htab built in `__memp_init` (`mp_region.c:286–289`) — add the swip vector there or lazily at wire time, with a new `roff_t` on `struct __mpool`.
