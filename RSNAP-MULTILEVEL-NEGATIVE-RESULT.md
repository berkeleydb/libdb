# perf/rsnap-ml — multi-level wired snapshot: MEASURED NEGATIVE RESULT

Status: **do not merge.** Kept as a documented negative result (like
`perf/mpool-pin`). The single-level root snapshot already in master is the
right stopping point; the next lever is R1 (latch-free/optimistic BH pin,
`docs/design/` design from the Plan agent).

## What this branch tried

Extend the shipped root-snapshot fast path (`__bam_rsnap_*`, which skips the
*root* page pin on leaf-level reads) to also skip the **upper-internal** page
pins, by caching wired copies of internal pages per handle (`BTREE.bt_isnap[3]`)
and validating each against its wired frame's live LSN with a plain load — a
natural extension of the root wiring. Per-handle, process-local; ABI unchanged
(`sizeof(DB)/DBC/DB_ENV` verified identical to master).

## Why it was measured, not merged

Correctness was clean (1.9M concurrent reads, 0 wrong-child, db_verify + ASan
clean). But the fork's rule is measure-before-merge, and the scaling A/B on a
96-vCPU EC2 c7i.24xlarge (Debian 12, 3M-key height-4 btree, 8 GB cache,
in-cache uniform-random reads) was decisive:

| threads | root-rsnap only (DB_NO_ISNAP=1) | multi-level ON | ratio |
|--------:|--------------------------------:|---------------:|------:|
| 1 | 373 K | 119 K | 0.32× |
| 8 | 1.5 M | 175–202 K | **0.12–0.14×** |
| 32 | 2.33 M | 178 K | 0.08× |

Multi-level ON is a **7–13× regression**, reproducible across reps.
`DB_NO_ISNAP=1` (root-rsnap only) matches/beats the `DB_NO_RSNAP=1` baseline —
so the root path is fine; the *multi-level layer* is the regression.

## Root cause — a fundamental design flaw, not a bug

The root snapshot works because there is exactly **one** root page: cache it
once, reuse it forever. There is **no small stable hot working set at the
internal levels below the root** for a uniform-random workload. A height-4 tree
has thousands of L1 internal pages; a 3-slot per-handle cache (`BAM_ISNAP_MAX`)
thrashes — nearly every random descent hits an L1 page not in the cache, so it
pays `__os_malloc` + full-page `memcpy` + `__memp_wire` (and evicts a prior
slot) *per descent*. That per-descent cost dwarfs the one buffer-header pin it
saves. Growing the cache doesn't fix it: to cover the L1 fan-out you'd cache a
large fraction of the internal tier per handle, which is just a second buffer
pool. The premise — "cache the hot internal pages like the root" — is false for
random reads; below the root there is no hot page, only a hot *tier*.

(A bug was found and fixed on the way — commit 57edb3e9: the store was gated on
`snap_ok` not `__bam_isnap_enabled()`, so `DB_NO_ISNAP` didn't actually disable
it. Fixing that let the OFF path match baseline and isolated the regression to
the layer itself. The fix is correct but the layer is still a loss.)

## Conclusion

The buffer-header pin on internal pages is real contention (the cross-engine
data stands), but per-handle snapshot copying is the wrong tool below the root.
The right tool is a contention-free pin on the *shared* buffer header itself —
**R1**, the optimistic/seqlock-validated wired read-hit path in `__memp_fget`,
which removes the pin's hot cache-line RMW without copying pages per handle.
Proceed to R1.
