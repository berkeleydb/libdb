# M2 design note — porting SIREAD lock GC to 5.3.x partitioned lock regions

Scope: how Cahill's 2008 SIREAD-lock bookkeeping (built on BDB 4.6.21's single
global lock table) maps onto 5.3.x, whose lock region is **partitioned**. This
is the one area most likely to be subtly wrong, so we agree on it before coding.

## What the prototype assumed (4.6.21)

- One global object hash table; GC walked `for i in object_t_size { for obj in obj_tab[i] }`.
- `OBJECT_LOCK_NDX(lt, ndx)` — 2 args; one region mutex effectively serialized object access.
- It added its own `__txn_oldest_reader`.
- `__lock_siclean_obj` could call `__lock_freelocker` **while holding the object mutex**.

## What actually exists in 5.3.x (verified)

- `obj_tab` is still a flat array of `object_t_size` hash buckets. Partitioning
  is a *locking* concern: `LOCK_PART(reg, ndx) = ndx % part_t_size` maps a
  bucket to a partition mutex.
- `OBJECT_LOCK_NDX(lt, reg, ndx)` / `OBJECT_UNLOCK(lt, reg, ndx)` — **3 args**;
  they lock/unlock only the partition owning `ndx`.
- `LOCK_SYSTEM_LOCK(lt, reg)` locks the single region mutex **only when
  `part_t_size == 1`**; with multiple partitions it is a no-op and callers must
  take partition mutexes individually.
- `__txn_oldest_reader(ENV *, DB_LSN *)` **already exists** (`txn_region.c`,
  used for MVCC buffer freezing). It takes `TXN_SYSTEM_LOCK` internally and
  scans `region->active_txn` for the smallest `read_lsn`. **Reuse it; do not
  reimplement.**
- Locker freeing is `__lock_freelocker_int(lt, region, sh_locker, reallyfree)`
  (the public `__lock_freelocker(lt, sh_locker)` wraps it) and runs under
  `LOCK_LOCKERS` (`mtx_lockers`).
- Internal API is `ENV *env`, not `DB_ENV *dbenv`.

## The critical decision: lock ordering

Master's nesting is **system → partition**, and locker frees happen separately
under `mtx_lockers`. The prototype freed a locker *while holding the object
partition mutex* (partition → lockers). Reproducing that introduces a
partition→`mtx_lockers` ordering that master does not otherwise use — a
lock-order-inversion / deadlock risk.

**Decision:** do **not** free lockers while holding a partition mutex. Instead,
within `__lock_siclean_obj`, only (a) detach the SIREAD lock and (b) decrement
`sh_locker->nlocks`; collect any locker that reaches `nlocks == 0 && FREED`
onto a small local victim list. After the partition mutex is released, free the
victims under `LOCK_LOCKERS`. This preserves master's existing order
(partition released before lockers) and keeps GC partition-local.

## GC algorithm (5.3.x form)

`__txn_oldest_reader` is computed **once up front**, before taking any
partition mutex (it takes `TXN_SYSTEM_LOCK`, so taking it under a partition
mutex would itself be a new ordering). Then:

```
__lock_sicleanup(env):
    if (__txn_oldest_reader(env, &old_lsn)) return ret;   # no partition held
    for ndx in 0 .. object_t_size-1:
        OBJECT_LOCK_NDX(lt, region, ndx)
        __lock_siclean_obj(env, &obj_tab[ndx]'s objects, &old_lsn, &victims)
        OBJECT_UNLOCK(lt, region, ndx)
    free victims under LOCK_LOCKERS
```

`__lock_siclean_obj` keeps a SIREAD lock if its owner is still `TXN_RUNNING`,
or if its read/commit LSNs are still newer than `old_lsn` (the live-snapshot
window); otherwise it unlinks the lock from `obj->sireaders` via
`__lock_put_internal(... DB_LOCK_FREE | DB_LOCK_DOALL)` and adjusts counts.

## Counting / stats

SIREAD locks occupy real `struct __db_lock`s, so they must be reflected in
`part_array[part].part_stat.st_nlocks` on alloc/free exactly like normal locks
(the prototype's single-region counter becomes the per-partition counter).

## Acquisition path (gated)

`DB_LOCK_SNAPSHOT_SAFE` is read into a local `safe_si` and cleared at the top of
`__lock_get_internal`. Only when `safe_si` is set do we (1) place SIREAD locks on
`obj->sireaders` and (2), on a `DB_LOCK_WRITE` acquire, scan `obj->sireaders` to
record rw-antidependencies (M3/M4). With `safe_si` unset the path is byte-for-byte
the existing behavior — SSI is strictly opt-in.

## Open items to confirm during coding

- Whether any `__lock_put_internal` path needs a `DB_LOCK_FREE` variant that
  skips holder/waiter promotion for SIREAD locks (prototype used `DB_LOCK_FREE`).
- Recovery/`__lock_getlocker` must initialize `td_off` for every locker that can
  take SIREAD locks (M2 part 2 wires this in `__lock_getlocker`).
