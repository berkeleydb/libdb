# MVCC / cache-resize coverage (mp_mvcc.c, mp_resize.c)

Two mpool files sat near-cold because the functional test suite exercises
multiversion databases but never applies the *cache pressure* (freeze/thaw)
or the *post-open cache growth* (region resize) that these files implement.

`test/tcl/mvcc001.tcl` (COV group `mvcc`, wired into `run_coverage.sh`) closes
both gaps.

## What mvcc001 covers

- **mvcc001.a -- mp_mvcc.c freeze/thaw** (`__memp_bh_freeze`,
  `__memp_bh_thaw`, `__pgno_cmp`). A tiny (512K, 512-byte-page) multiversion
  cache, two long-lived `txn -snapshot` readers pinning the original page
  versions, and a writer churning every page in small committed transactions.
  The old versions cannot stay resident, so they are spilled to a
  `__db.freezer.*` file (freeze) and read back when the readers touch them
  (thaw). Asserts the readers still see their original snapshot and the
  mpool `Buffers frozen` / `Buffers thawed` counters are non-zero (observed:
  ~7600 frozen, ~2900 thawed). Lifts **mp_mvcc.c 0% -> ~70%**.

- **mvcc001.b -- mp_resize.c region growth** (`__memp_resize`,
  `__memp_add_region`, `__memp_add_bucket`, `__memp_merge_buckets`,
  `__memp_map_regions`, `__memp_get_cache_max`). A small 2-region cache opened
  with a larger `cache_max`, then grown in steps with `resize_cache`
  (`DB_ENV->set_cachesize` post-open). Each grow splits the last region's hash
  buckets and copies the buffers into the newly-attached region; the test
  re-reads all 800 records after every step to prove the data survives the
  re-hash, and asserts an over-max resize fails cleanly (the `EINVAL` branch).
  Lifts **mp_resize.c 0% -> ~58%**.

## REAL BUG found: cache shrink crashes (SIGSEGV)

While developing mvcc001.b, shrinking the cache (resize_cache to *fewer*
regions) reliably **crashes** with SIGSEGV. mvcc001 therefore only grows the
cache; the shrink path is left uncovered on purpose.

### Reproduction

Multi-region cache, grow, then shrink:

```tcl
set e [berkdb_env -create -txn -cache_max {0 16777216} \
    -cachesize {0 1048576 2} -home $testdir]
set db [berkdb_open -create -auto_commit -env $e -btree -pagesize 512 t.db]
for {set i 0} {$i < 800} {incr i} { $db put key$i [string repeat D 200] }
$e resize_cache {0 4194304}   ;# grow to 8 regions -- OK
$e resize_cache {0 2097152}   ;# shrink to 4 regions -- SIGSEGV
```

### Stack

```
#0  __os_detach          src/os/os_map.c:317   munmap(infop->addr, rp->max)
#1  __env_sys_detach     src/env/env_region.c:1348
#2  __env_region_detach  src/env/env_region.c:1217
#3  __memp_remove_region src/mp/mp_resize.c:459
#4  __memp_resize        src/mp/mp_resize.c:547
#5  __memp_set_cachesize src/mp/mp_method.c:176   (resize_cache path)
```

### Root cause (off-by-one region index)

`__memp_remove_region()` picks the region to detach with:

```c
infop = &dbmp->reginfo[mp->nreg];      /* src/mp/mp_resize.c:451 */
...
ret = __env_region_detach(env, infop, 1);
if (ret == 0)
    mp->nreg--;                        /* decrement AFTER detach, line 461 */
```

Cache regions are stored in `reginfo[0 .. nreg-1]`, so the last valid region
is index `nreg-1`. But `__memp_remove_region` indexes `reginfo[nreg]` -- one
past the last live region -- and hands that uninitialized/stale `REGINFO`
(garbage `addr`/`rp`) to `__env_region_detach`, so `munmap()` faults.

Contrast the (correct) add path, `__memp_add_region()`:

```c
infop = &dbmp->reginfo[mp->nreg];      /* src/mp/mp_resize.c:377 */
...
regids[mp->nreg++] = infop->id;        /* increment AFTER, line 391 */
```

There the increment is *after* use, so index `nreg` is exactly the new region.
The remove path copied the same `reginfo[mp->nreg]` expression but decrements
afterward, making it off-by-one. The fix would be to detach
`&dbmp->reginfo[mp->nreg - 1]` (and free that index's per-bucket mutexes in the
`ENV_PRIVATE` block above). **Not fixed here** -- coverage work only documents
engine bugs, it does not change engine code.

This matches the historical state: even the full test run
(`test/coverage/full-run-2/`) shows `FNDA:0` for `__memp_remove_region`,
`__memp_remove_bucket`, and `__memp_merge_buckets` -- the shrink path has never
been exercised, so the crash has never been hit before.
